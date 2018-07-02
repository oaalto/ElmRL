module Main exposing (..)

import Html
import Html.Styled exposing (..)
import Display.Model as Display exposing (..)
import Display.View as Display exposing (view)
import Array exposing (Array)
import Css.Colors exposing (white)


---- MODEL ----


type alias Model =
    { displayModel : Display.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            initialModel 80 25

        tiles =
            Array.repeat (20 * 5) (Display.Tile white '.')

        layer =
            Display.Layer (Display.Rectangle (Display.Point 10 10) (Display.Point 30 15)) tiles

        newModel =
            { model | layers = Array.push layer model.layers }
    in
        ( Model newModel, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    Display.view model.displayModel



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view >> toUnstyled
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
