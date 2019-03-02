module Chapter03.GridWorld exposing (main)

import Array exposing (Array)
import Browser
import Collage exposing (..)
import Collage.Layout exposing (at, base)
import Collage.Render as Render
import Collage.Text as Text
import Color
import Html exposing (Html, button, div, h1, hr, img, p, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import List.Extra
import Matrix exposing (Matrix)
import Round
import Task



-- Algorithm


type alias State =
    ( Int, Int )


type Action
    = North
    | South
    | West
    | East


actions =
    [ North, South, West, East ]


worldSize =
    5


positionA =
    ( 0, 1 )


positionAPrime =
    ( 4, 1 )


positionB =
    ( 0, 3 )


positionBPrime =
    ( 2, 3 )


discount =
    0.9


step : Matrix Float -> State -> Action -> ( State, Float )
step grid state action =
    if state == positionA then
        ( positionAPrime, 10.0 )

    else if state == positionB then
        ( positionBPrime, 5.0 )

    else
        let
            ( x, y ) =
                state

            nextState =
                case action of
                    North ->
                        ( x, y - 1 )

                    South ->
                        ( x, y + 1 )

                    West ->
                        ( x - 1, y )

                    East ->
                        ( x + 1, y )

            ( nextX, nextY ) =
                nextState
        in
        if nextX < 0 || nextX >= worldSize || nextY < 0 || nextY >= worldSize then
            ( state, -1.0 )

        else
            ( nextState, 0.0 )


evaluate : Matrix Float -> (List Float -> Float) -> Bool -> Matrix Float
evaluate grid policy oneStep =
    let
        newGrid =
            Matrix.indexedMap
                (\i j value ->
                    List.foldl
                        (\action stateValues ->
                            let
                                ( ( x, y ), reward ) =
                                    step grid ( i, j ) action

                                newValue =
                                    reward + discount * (Matrix.get x y grid |> Maybe.withDefault 0.0)
                            in
                            newValue :: stateValues
                        )
                        []
                        actions
                        |> policy
                )
                grid
    in
    if oneStep || goodEnough 1.0e-4 grid newGrid then
        newGrid

    else
        evaluate newGrid policy oneStep


goodEnough : Float -> Matrix Float -> Matrix Float -> Bool
goodEnough delta m1 m2 =
    List.Extra.zip (Matrix.toList m1) (Matrix.toList m2)
        |> List.foldl
            (\( a1, a2 ) sum ->
                sum + abs (a1 - a2)
            )
            0.0
        |> (\sum -> sum < delta)


randomPolicy : List Float -> Float
randomPolicy =
    List.sum >> (\x -> x * 0.25)


optimalPolicy : List Float -> Float
optimalPolicy =
    List.maximum >> Maybe.withDefault 0


gridWorld : Matrix Float -> Html msg
gridWorld grid =
    drawMatrix grid 100.0
        |> Render.svg


drawMatrix : Matrix Float -> Float -> Collage msg
drawMatrix matrix boxSize =
    matrix
        |> Matrix.toList
        |> List.foldl
            (\value ( index, group ) ->
                let
                    ( nrows, ncols ) =
                        Matrix.size matrix

                    i =
                        index // ncols

                    j =
                        remainderBy ncols index

                    text =
                        Text.fromString (Round.round 3 value)
                            |> Text.size Text.small
                            |> rendered

                    collage =
                        square boxSize
                            |> outlined (solid thin (uniform Color.black))
                            |> at base text
                            |> shift ( boxSize * toFloat j, boxSize * (0.0 - toFloat i) )
                in
                ( index + 1, collage :: group )
            )
            ( 0, [] )
        |> Tuple.second
        |> group



---- MODEL ----


type alias Model =
    { grid : Matrix Float
    , gridOptimal : Matrix Float
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { grid = Matrix.repeat worldSize worldSize 0.0
      , gridOptimal = Matrix.repeat worldSize worldSize 0.0
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Reset
    | Step
    | Evaluate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( { model
                | grid = Matrix.repeat worldSize worldSize 0.0
                , gridOptimal = Matrix.repeat worldSize worldSize 0.0
              }
            , Cmd.none
            )

        Step ->
            ( { model
                | grid = evaluate model.grid randomPolicy True
                , gridOptimal = evaluate model.gridOptimal optimalPolicy True
              }
            , Cmd.none
            )

        Evaluate ->
            ( { model
                | grid = evaluate model.grid randomPolicy False
                , gridOptimal = evaluate model.gridOptimal optimalPolicy False
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ showButtons model
        , hr [] []
        , gridWorld model.grid
        , text "Figure 3.2 - GridWorld with random policy"
        , hr [] []
        , gridWorld model.gridOptimal
        , text "Figure 3.5 - GridWorld with optimal policy"
        ]


showButtons : Model -> Html Msg
showButtons model =
    div [] <|
        [ button [ onClick Reset ] [ text "Reset" ]
        , button [ onClick Step ] [ text "Step" ]
        , button [ onClick Evaluate ] [ text "Evaluate" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
