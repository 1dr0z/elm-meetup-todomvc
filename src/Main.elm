module Main exposing (main, removeAt)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode


type alias Flags =
    ()


type alias Task =
    { text : String
    , isComplete : Bool
    }


type alias Model =
    { tasks : List Task
    , inputText : String
    , numTasks : Int
    , selectedFilter : Filter
    }


type Filter
    = All
    | Active
    | Completed


emptyModel : Model
emptyModel =
    { tasks =
        [ { text = "Install Elm (Dynamically)", isComplete = False }
        , { text = "Install Elm (Dynamically)", isComplete = False }
        ]
    , inputText = ""
    , numTasks = 2
    , selectedFilter = All
    }


type alias KeyCode =
    Int


type alias Index =
    Int


type Msg
    = ToggleCompletion Index
    | AddTask String
    | Submit KeyCode
    | UpdateInputText String
    | RemoveTask Index
    | SelectFilter Filter
    | ClearCompleted


main : Program Flags Model Msg
main =
    Browser.document
        { init = \_ -> ( emptyModel, Cmd.none )
        , view = \model -> { title = "Elm TodoMVC Generated", body = [ view model ] }
        , update = \msg model -> update msg model
        , subscriptions = \_ -> Sub.none
        }


onKeyUp : (KeyCode -> a) -> Attribute a
onKeyUp toMsg =
    on "keyup" (Decode.map toMsg keyCode)


view : Model -> Html Msg
view model =
    Html.div [ class "todomvc-wrapper", style "visibility" "hidden" ]
        [ section [ class "todoapp" ]
            [ header [ class "header" ]
                [ h1 [] [ text "todo" ]
                , input
                    [ class "new-todo"
                    , value model.inputText
                    , placeholder "What needs to be done?"
                    , autofocus True
                    , name "newTodo"
                    , type_ "text"
                    , onInput UpdateInputText
                    , onKeyUp Submit
                    ]
                    []
                ]
            , section [ class "main", style "visibility" "visible" ]
                [ input [ class "toggle-all", type_ "checkbox", name "toggle" ] []
                , label [ for "toggle-all" ] [ text "Mark all as complete" ]
                , ul [ class "todo-list" ]
                    (model.tasks
                        |> List.filter (matchesFilter model.selectedFilter)
                        |> List.indexedMap buildTask
                    )
                ]
            , footer [ class "footer" ]
                [ span [ class "todo-count" ]
                    [ strong [] [ text (String.fromInt model.numTasks) ]
                    , text " item left"
                    ]
                , ul [ class "filters" ]
                    [ li [] [ a [ onClick (SelectFilter All), selectedClass model All ] [ text "All" ] ]
                    , li [] [ a [ onClick (SelectFilter Active), selectedClass model Active ] [ text "Active" ] ]
                    , li [] [ a [ onClick (SelectFilter Completed), selectedClass model Completed ] [ text "Completed" ] ]
                    ]
                , button [ onClick ClearCompleted, class "clear-completed" ]
                    [ let
                        count =
                            model.tasks
                                |> List.filter (matchesFilter Completed)
                                |> List.length
                      in
                      text ("Clear completed (" ++ String.fromInt count ++ ")")
                    ]
                ]
            ]
        , footer [ class "info" ]
            [ p [] [ text "Double-click to edit a todo" ]
            , p [] [ text "Part of", a [ href "http://todomvc.com" ] [ text "TodoMVC" ] ]
            ]
        ]


matchesFilter : Filter -> Task -> Bool
matchesFilter filter task =
    case filter of
        All ->
            True

        Active ->
            not task.isComplete

        Completed ->
            task.isComplete


selectedClass : Model -> Filter -> Attribute Msg
selectedClass { selectedFilter } desired =
    if selectedFilter == desired then
        class "selected"

    else
        class ""


buildTask : Index -> Task -> Html Msg
buildTask index task =
    li [ class (completedClass task.isComplete) ]
        [ div [ class "view" ]
            [ input [ class "toggle", type_ "checkbox", onCheck (\_ -> ToggleCompletion index) ] []
            , label [] [ text task.text ]
            , button [ class "destroy", Html.Events.onClick (RemoveTask index) ] []
            ]
        , input [ class "edit", name "title", id "todo-0" ] []
        ]


completedClass : Bool -> String
completedClass isCompleted =
    if isCompleted then
        "completed"

    else
        ""


enterKey : Int
enterKey =
    13


addTask : String -> Model -> Model
addTask body model =
    { model
        | tasks = { text = body, isComplete = False } :: model.tasks
    }


clearInput : Model -> Model
clearInput model =
    { model | inputText = "" }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        ( newModel, cmd ) =
            case message of
                ToggleCompletion tidx ->
                    ( { model | tasks = List.indexedMap (completeTask tidx) model.tasks }
                    , Cmd.none
                    )

                AddTask body ->
                    ( { model | tasks = { text = body, isComplete = False } :: model.tasks }
                    , Cmd.none
                    )

                Submit keyCode ->
                    if keyCode == enterKey then
                        ( model
                            |> addTask model.inputText
                            |> clearInput
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                UpdateInputText text ->
                    ( { model | inputText = text }
                    , Cmd.none
                    )

                RemoveTask idx ->
                    ( { model | tasks = removeAt idx model.tasks }
                    , Cmd.none
                    )

                SelectFilter filter ->
                    ( { model | selectedFilter = filter }
                    , Cmd.none
                    )

                ClearCompleted ->
                    ( { model | tasks = List.filter (matchesFilter Active) model.tasks }
                    , Cmd.none
                    )

        numTasks =
            newModel.tasks
                |> List.filter (.isComplete >> (==) False)
                |> List.length
    in
    ( { newModel | numTasks = numTasks }
    , cmd
    )


removeAt : Index -> List a -> List a
removeAt idx list =
    case list of
        [] ->
            []

        head :: tail ->
            if List.length (head :: tail) < idx then
                head :: tail

            else if idx < 0 then
                head :: tail

            else if idx == 0 then
                tail

            else
                head :: removeAt (idx - 1) tail


completeTask : Int -> Int -> Task -> Task
completeTask tidx index task =
    if index == tidx then
        { task | isComplete = not task.isComplete }

    else
        task
