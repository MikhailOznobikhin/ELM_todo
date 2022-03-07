module Todos.Models exposing (Todo, TodoEditView(..),Msg(..),update,fetchAll,viewEdit,viewList)

import Http
import Json.Decode
import Json.Encode
import String
import Task
import Utils
--edit
import Html exposing (..)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
--list
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
--bootstrap
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Table as Table

type alias Todo = { id : Int, title : String, completed : Bool}

type TodoEditView = None | New String| Editing Todo

--msg
type Msg
    = NoOp
    | FetchAllDone (Result Http.Error (List Todo))
    | CreateDone (Result Http.Error Todo)
    | PatchDone (Result Http.Error Todo)
    | DeleteDone (Result Http.Error Todo)
    | ShowEditView TodoEditView
    | ChangeTitle String
    | CreateOrPatch
    | Complete Todo
    | Revert Todo
    | Patch Todo
    | Delete Todo
    | DeleteCompleted

--commands
singleUrl : Int -> String
singleUrl id = String.join "/" [ "http://localhost:4000/todos", String.fromInt id ]


todosDecoder : Json.Decode.Decoder (List Todo)
todosDecoder = Json.Decode.list todoDecoder

todoDecoder : Json.Decode.Decoder Todo
todoDecoder =
    Json.Decode.map3 Todo
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "completed" Json.Decode.bool)


todoEncoder : String -> Bool -> Json.Encode.Value
todoEncoder title completed =
    let encodings = [ ( "title", Json.Encode.string title ), ( "completed", Json.Encode.bool completed )]
    in  encodings |> Json.Encode.object

fetchAll : Cmd Msg
fetchAll =
    let request = Http.get "http://localhost:4000/todos" todosDecoder
    in Http.send FetchAllDone request

create : String -> Cmd Msg
create title =
    let task = Utils.postJson todoDecoder "http://localhost:4000/todos" <| todoEncoder title False
    in Task.attempt CreateDone task

patch : Todo -> Cmd Msg
patch { id, title, completed } =
    let task = Utils.patchJson todoDecoder (singleUrl id) <|todoEncoder title completed
    in Task.attempt PatchDone task

delete : Todo -> Cmd Msg
delete todo = Task.attempt DeleteDone <| Utils.delete todo (singleUrl todo.id)

--update
update : Msg -> TodoEditView -> List Todo -> ( TodoEditView, List Todo, Cmd Msg )
update msg ev todos =
    case msg of
        NoOp -> ( ev, todos, Cmd.none )
        ShowEditView nev -> ( nev, todos, Cmd.none )
        ChangeTitle title ->
            let nev = case ev of
                    None -> ev
                    New _ -> New title
                    Editing todo -> Editing { todo | title = title }
            in ( nev, todos, Cmd.none )
        
        FetchAllDone res -> case res of
            Result.Ok newTodos -> ( ev, newTodos, Cmd.none )
            Result.Err _ -> ( ev, todos, Cmd.none )

        CreateDone res -> case res of
                Result.Ok todo ->( ev, Utils.mergeById todos todo, Cmd.none )
                Result.Err _ -> ( ev, todos, Cmd.none )

        PatchDone res -> case res of
                Result.Ok newTodo -> ( ev, Utils.mergeById todos newTodo, Cmd.none )
                Result.Err _ -> ( ev, todos, Cmd.none )

        DeleteDone res -> case res of
                Result.Ok todo -> ( ev, Utils.removeById todos todo, Cmd.none )
                Result.Err _ -> ( ev, todos, Cmd.none )

        CreateOrPatch ->
            let cmd = case ev of
                    None -> Cmd.none
                    New title -> create title
                    Editing todo -> patch todo
            in ( None, todos, cmd )
        
        Complete todo -> 
            let newTodo = { todo | completed = True }
                newTodos = todos
            in ( ev, newTodos, patch newTodo )

        Revert todo ->
            let newTodo = { todo | completed = False }
            in ( ev, todos, patch newTodo )
        
        Patch todo -> ( ev, todos, patch todo )

        Delete todo -> ( ev, todos, delete todo )

        DeleteCompleted ->
            let cmds = todos |> List.filter .completed |> List.map delete
            in ( ev, todos, Cmd.batch cmds )

--edit
viewEdit : TodoEditView -> Html Msg
viewEdit ev =
    div []
        [ case ev of
            None -> Button.button[ Button.primary, Button.large, Button.block, Button.attrs [onClick <| ShowEditView <| New ""  ]][text "Новая задача"]
            New title -> div [][editingInputs title]
            Editing { title } -> div [] [ h2 [] [ text <| "Редактируемое: " ++ title ], editingInputs title]
        ]

editingInputs : String -> Html Msg
editingInputs title =
    Grid.row
        [ Row.topSm ]
        [ Grid.col
            [ Col.xs2 ]
            [ Button.button[ Button.dark, Button.large,Button.attrs [ onClick <| ShowEditView None  ]][text "Отмена" ] ]
        , Grid.col
            [ Col.xs7 ]
            [ Input.text[ Input.large,Input.value title, Input.onInput ChangeTitle, Input.placeholder "Введите название"]]
        , Grid.col
            [ Col.xs2 ]
            [ Button.button[ Button.success,Button.large, Button.attrs [ onClick CreateOrPatch ]][text "Сохранить"] ]
        ]
        

--list
viewList : List Todo -> Html Msg
viewList todos =
    Table.table{ options = [ Table.striped ]
        , thead = Table.thead []
            [ Table.tr []
                [ Table.th [] [ text "Название" ]
                , Table.th [] [ text "Готово" ]
                , Table.th [] [ 
                        Grid.row
                            [ Row.bottomXs ]
                            [ Grid.col
                                [ Col.xs6 ]
                                [ text "Действия" ]
                            , Grid.col
                                [ Col.xs6 ]
                                [ delCompl]
                            ]
                    ]
                ]
            ]
        , tbody = Table.tbody [] (List.map todoRow todos)
        }
    

todoRow : Todo -> Table.Row Msg
todoRow t =
    let
        { id, title, completed } = t
        ( completedText, buttonText, buttonMsg ) =
            if completed then ( "Да", "Невыполненно", Revert )
            else ( "Нет", "Выполненно", Complete )
    in
    Table.tr []
        [ Table.td [] [ text title ]
        , Table.td [] [ text completedText ]
        , Table.td [] [ 
            Grid.row
                [ Row.centerMd ]
                [ Grid.col
                    [ Col.xs4 ]
                    [ Button.button[ Button.success, Button.attrs [ onClick <| buttonMsg t ]][text buttonText] ]
                , Grid.col
                    [ Col.xs4 ]
                    [ Button.button[ Button.info, Button.attrs [ onClick <| ShowEditView <| Editing t  ]][text "Редактирвоать"]]
                , Grid.col
                    [ Col.xs4 ]
                    [  Button.button[ Button.warning, Button.attrs [ onClick <| Delete t ]][text "Удалить"]]
                ]
            ]
        ]


delCompl : Html Msg
delCompl = Button.button
        [ Button.danger
        , Button.attrs [ onClick DeleteCompleted ]
        ][text "Удалить выполненные"]