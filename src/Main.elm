module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type alias Flags =
    ()

type alias Task = 
    { text : String
    , isComplete : Bool
    , id : Int
    }

type alias Model =
    { tasks : List Task
    }

emptyModel : Model 
emptyModel = 
    { tasks = 
        [ { text = "Install Elm (Dynamically)", isComplete = False, id = 1}
        , { text = "Install Elm (Dynamically)", isComplete = False, id = 2}
        ]
    }

type Msg
    = ToggleCompletion Task Bool 

main : Program Flags Model Msg
main =
    Browser.document
        { init = \model -> ( emptyModel , Cmd.none )
        , view = \model -> { title = "Elm TodoMVC Generated", body = [view model] }
        , update = \msg model -> update msg model
        , subscriptions = \_ -> Sub.none
        }


view : Model -> Html Msg
view model = 
    Html.div [ class "todomvc-wrapper" , style "visibility" "hidden" ] 
        [ section [ class "todoapp"] 
            [ header [ class "header"] 
                [ h1 [] [text "todo"]
                , input 
                    [class "new-todo"
                    , placeholder "What needs to be done?"
                    , autofocus True
                    , name "newTodo"
                    , type_ "Text"
                    ] 
                    []
                ]
            , section [class "main" , style "visibility" "visible" ] 
                [ input [class "toggle-all", type_ "checkbox", name "toggle"][]
                , label [ for "toggle-all"][text "Mark all as complete"]
                , ul [ class "todo-list"] 
                    (List.map buildTask model.tasks)
                ]
            , footer [ class "footer"] 
                [ span [class "todo-count"]
                    [ strong[][text "1"]
                    , text " item left"
                    ]
                , ul [ class "filters"]
                    [ li [][ a [href "#/", class "selected"] [text "All"]  ]
                    , li [][ a [href "#/active" ] [text "Active"]  ]
                    , li [][ a [href "#/completed"] [text "Completed"]  ]
                    ]
                , button [class "clear-completed"][ text "Clear completed (1)"]
                ]
            ] 
        , footer [ class "info" ] 
            [ p [] [ text "Double-click to edit a todo"]
            , p [] [ text "Part of" , a [href "http://todomvc.com"] [text "TodoMVC"] ]
            ]
        ]

buildTask : Task -> Html Msg
buildTask task = 
    li [ class (completedClass task.isComplete)]
        [ div [class "view"]
            [ input [class "toggle", type_ "checkbox", onCheck (ToggleCompletion task)][]
            , label [][text task.text]
            , button [class "destroy"][]
            ]
        , input [class "edit", name "title", id "todo-0" ][]
        ]

completedClass : Bool -> String
completedClass isCompleted = 
    if isCompleted then 
        "completed"
    else 
        ""

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    case message of 
        ToggleCompletion tsk isComplete -> 
            ({model| tasks =  List.map (completeTask tsk isComplete) model.tasks}
            , Cmd.none)

completeTask : Task -> Bool -> Task -> Task 
completeTask tsk1 complete tsk2 =
    if tsk1 == tsk2 then 
        {tsk2 | isComplete = complete}
    else 
        tsk2