module App

open Elmish
open Elmish.React
open Fable.SimpleHttp
open Feliz

type State =
  { Products: Deferred<Result<string, string>> }

type Msg =
  | LoadProducts of AsyncOperationEvent<Result<string, string>>

let init() =
  { Products = HasNotStartedYet }, Cmd.ofMsg (LoadProducts Started)

let update (msg: Msg) (state: State) =
  match msg with
  | LoadProducts Started ->
      let nextState = { state with Products = InProgress }
      let loadProducts =
        async {
          do! Async.Sleep 1500
          let! (statusCode, products) = Http.get "/products.json"
          if statusCode = 200
          then return LoadProducts (Finished (Ok products))
          else return LoadProducts (Finished (Error "Could not load the product's information"))
        }

      nextState, Cmd.fromAsync loadProducts

  | LoadProducts (Finished result) ->
      let nextState = { state with Products = Resolved result }
      nextState, Cmd.none

let render (state: State) (dispatch: Msg -> unit) =
  match state.Products with
  | HasNotStartedYet -> Html.none
  | InProgress -> Html.h1 "Loading..."
  | Resolved (Error errorMsg) -> Html.h1 errorMsg
  | Resolved (Ok products) -> Html.pre products

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run