module Chapter04.GridWorld exposing (main)

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
    = Up
    | Down
    | Right
    | Left


actions =
    [ Up, Down, Right, Left ]


worldSize =
    4


discount =
    1.0


isTerminal : State -> Bool
isTerminal state =
    let
        ( x, y ) =
            state
    in
    (x == 0 && y == 0) || (x == (worldSize - 1) && y == (worldSize - 1))


step : Matrix Float -> State -> Action -> ( State, Float )
step grid state action =
    let
        ( x, y ) =
            state

        nextState =
            case action of
                Up ->
                    ( x, y - 1 )

                Down ->
                    ( x, y + 1 )

                Left ->
                    ( x - 1, y )

                Right ->
                    ( x + 1, y )

        ( nextX, nextY ) =
            nextState
    in
    if nextX < 0 || nextX >= worldSize || nextY < 0 || nextY >= worldSize then
        ( state, -1.0 )

    else
        ( nextState, -1.0 )


evaluate : Matrix Float -> (List Float -> Float) -> Bool -> Matrix Float
evaluate grid policy oneStep =
    let
        newGrid =
            Matrix.indexedMap
                (\i j value ->
                    if isTerminal ( i, j ) then
                        value

                    else
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


drawText text =
    Text.fromString text
        |> Text.size Text.small
        |> rendered


drawMatrix : Matrix Float -> Float -> Collage msg
drawMatrix matrix matrixUnitSize =
    matrix
        |> Matrix.indexedMap
            (\i j value ->
                square matrixUnitSize
                    |> outlined (solid thin (uniform Color.black))
                    |> at base (drawText <| Round.round 3 value)
                    |> shift ( matrixUnitSize * toFloat j, matrixUnitSize * (0.0 - toFloat i) )
            )
        |> Matrix.toList
        |> group



---- MODEL ----


type alias Model =
    { grid : Matrix Float
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { grid = Matrix.repeat worldSize worldSize 0.0
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
              }
            , Cmd.none
            )

        Step ->
            ( { model
                | grid = evaluate model.grid randomPolicy True
              }
            , Cmd.none
            )

        Evaluate ->
            ( { model
                | grid = evaluate model.grid randomPolicy False
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
        , text "Figure 4.1 - Convergence of iterative policy evaluation on a small gridworld"
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
