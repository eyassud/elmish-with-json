module App

open Elmish
open Elmish.React
open Fable.SimpleHttp
open Feliz

type State =
  { StoreInfo: Deferred<Result<string, string>> }

type Msg =
  | LoadStoreInfo of AsyncOperationEvent<Result<string, string>>

let init() =
  { StoreInfo = HasNotStartedYet }, Cmd.ofMsg (LoadStoreInfo Started)

let update (msg: Msg) (state: State) =
  match msg with
  | LoadStoreInfo Started ->
      let nextState = { state with StoreInfo = InProgress }
      let loadProducts =
        async {
          // simulate delay
          do! Async.Sleep 1500
          let! (statusCode, products) = Http.get "/store.json"
          if statusCode = 200
          then return LoadStoreInfo (Finished (Ok products))
          else return LoadStoreInfo (Finished (Error "Could not load the products"))
        }

      nextState, Cmd.fromAsync loadProducts

  | LoadStoreInfo (Finished result) ->
      let nextState = { state with StoreInfo = Resolved result }
      nextState, Cmd.none

let render (state: State) (dispatch: Msg -> unit) =
  match state.StoreInfo with
  | HasNotStartedYet -> Html.none
  | InProgress -> Html.h1 "Loading..."
  | Resolved (Error errorMsg) ->
      Html.h1 [
        prop.style [ style.color.red ]
        prop.text errorMsg
      ]

  | Resolved (Ok products) ->
      Html.pre products

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run