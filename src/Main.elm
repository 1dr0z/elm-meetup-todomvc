module Main exposing (main)

import Browser


type alias Flags =
    ()


type alias Model =
    ()


type Msg
    = NoOp


main : Program Flags Model Msg
main =
    Browser.document
        { init = \model -> ( (), Cmd.none )
        , view = \model -> { title = "Elm TodoMVC", body = [] }
        , update = \msg model -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
