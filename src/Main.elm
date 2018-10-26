module Main exposing (main)

import Browser
import Browser.Events exposing (..)
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
    }


emptyModel : Model
emptyModel =
    { tasks =
        [ { text = "Install Elm (Dynamically)", isComplete = False }
        , { text = "Install Elm (Dynamically)", isComplete = False }
        ]
    , inputText = ""
    , numTasks = 2
    }


type Msg
    = ToggleCompletion Int
    | AddTask String
    | Submit Int
    | UpdateInputText String


main : Program Flags Model Msg
main =
    Browser.document
        { init = \model -> ( emptyModel, Cmd.none )
        , view = \model -> { title = "Elm TodoMVC Generated", body = [ view model ] }
        , update = \msg model -> update msg model
        , subscriptions = \_ -> Sub.none
        }


onKeyUp : (Int -> a) -> Attribute a
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
                    , type_ "Text"
                    , onInput UpdateInputText
                    , onKeyUp Submit
                    ]
                    []
                ]
            , section [ class "main", style "visibility" "visible" ]
                [ input [ class "toggle-all", type_ "checkbox", name "toggle" ] []
                , label [ for "toggle-all" ] [ text "Mark all as complete" ]
                , ul [ class "todo-list" ]
                    (List.indexedMap buildTask model.tasks)
                ]
            , footer [ class "footer" ]
                [ span [ class "todo-count" ]
                    [ strong [] [ text (String.fromInt model.numTasks) ]
                    , text " item left"
                    ]
                , ul [ class "filters" ]
                    [ li [] [ a [ href "#/", class "selected" ] [ text "All" ] ]
                    , li [] [ a [ href "#/active" ] [ text "Active" ] ]
                    , li [] [ a [ href "#/completed" ] [ text "Completed" ] ]
                    ]
                , button [ class "clear-completed" ] [ text "Clear completed (1)" ]
                ]
            ]
        , footer [ class "info" ]
            [ p [] [ text "Double-click to edit a todo" ]
            , p [] [ text "Part of", a [ href "http://todomvc.com" ] [ text "TodoMVC" ] ]
            ]
        ]


buildTask : Int -> Task -> Html Msg
buildTask index task =
    li [ class (completedClass task.isComplete) ]
        [ div [ class "view" ]
            [ input [ class "toggle", type_ "checkbox", onCheck (\_ -> ToggleCompletion index) ] []
            , label [] [ text task.text ]
            , button [ class "destroy" ] []
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

        numTasks =
            newModel.tasks
                |> List.filter (.isComplete >> (==) False)
                |> List.length
    in
    ( { newModel | numTasks = numTasks }
    , cmd
    )


completeTask : Int -> Int -> Task -> Task
completeTask tidx index task =
    if index == tidx then
        { task | isComplete = not task.isComplete }

    else
        task
