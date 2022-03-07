module Main exposing (main)

import Json.Decode as Decode exposing (Value)
import Todos.Models exposing (Todo, TodoEditView(..))

import Browser exposing (Document)
import Html exposing (..)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid


main : Program Value Model Msg
main = 
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type Msg = NoOp | TodosMsg Todos.Models.Msg

type alias Model = { todos : List Todo, todoEditView : TodoEditView}

init : flags -> ( Model, Cmd Msg )
init fs =
    let model = Model [] None
        cmds = Cmd.batch[ Cmd.map TodosMsg Todos.Models.fetchAll ]
    in ( model, cmds )

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> ( model, Cmd.none )
        TodosMsg subMsg -> 
            let
                ( newTodoEditView, newTodos, cmd ) = Todos.Models.update subMsg model.todoEditView model.todos
                newModel = { model | todoEditView = newTodoEditView, todos = newTodos }
            in ( newModel, Cmd.map TodosMsg cmd )

view : Model -> Document Msg
view model =
    { title = "Туду"
    , body =
        [ Grid.container []
            [ CDN.stylesheet,
                Html.map TodosMsg <| Todos.Models.viewEdit model.todoEditView
            , br [] []
            , Html.map TodosMsg <| Todos.Models.viewList model.todos
            ]
        ]
    }