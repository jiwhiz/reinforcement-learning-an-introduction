module Chapter04.CarRental exposing (main)

import Array exposing (Array)
import Browser
import Collage exposing (..)
import Collage.Layout exposing (at, base, spacer, vertical)
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


{-| maximum # of cars in each location
-}
maxCars =
    20


{-| maximum # of cars to move during night
-}
maxMoveOfCars =
    5


{-| expectation for rental requests in first location
-}
rentalRequestFirstLocation =
    3


{-| expectation for rental requests in second location
-}
rentalRequestSecondLocation =
    4


{-| expectation for # of cars returned in first location
-}
returnsFirstLocation =
    3


{-| expectation for # of cars returned in second location
-}
returnsSecondLocation =
    2


discount =
    0.9


rentalCredit =
    10.0


moveCarCost =
    2.0


{-| all possible actions
-}
actions =
    List.range (0 - maxMoveOfCars) (1 + maxMoveOfCars)


{-| An up bound for poisson distribution
If n is greater than this value, then the probability of getting n is truncated to 0
-}
poissonUpperBound =
    11


type alias State =
    ( Int, Int )


factorial : Int -> Int
factorial n =
    if n <= 1 then
        1

    else
        n * factorial (n - 1)


poisson : Int -> Int -> Float
poisson n lambda =
    (e ^ toFloat (0 - lambda)) * toFloat (lambda ^ n) / (toFloat <| factorial n)


expectedReturn : Matrix Float -> State -> Int -> Float
expectedReturn stateValues state action =
    let
        rentalRequests =
            Matrix.repeat poissonUpperBound poissonUpperBound 0.0
    in
    Matrix.indexedMap
        (\rentalRequest1 rentalRequest2 value ->
            let
                numOfCars1 =
                    min (Tuple.first state - action) maxCars

                numOfCars2 =
                    min (Tuple.second state + action) maxCars

                actualRental1 =
                    min numOfCars1 rentalRequest1

                actualRental2 =
                    min numOfCars2 rentalRequest2

                reward =
                    toFloat (actualRental1 + actualRental2) * rentalCredit

                prob =
                    poisson rentalRequest1 rentalRequestFirstLocation
                        * poisson rentalRequest2 rentalRequestSecondLocation

                actualNumOfCars1 =
                    min (numOfCars1 - actualRental1 + returnsFirstLocation) maxCars

                actualNumOfCars2 =
                    min (numOfCars2 - actualRental2 + returnsSecondLocation) maxCars
            in
            prob
                * (reward
                    + discount
                    * (stateValues
                        |> Matrix.get actualNumOfCars1 actualNumOfCars2
                        |> Maybe.withDefault 0.0
                      )
                  )
        )
        rentalRequests
        |> Matrix.toList
        |> List.sum
        |> (\r -> r - moveCarCost * abs (toFloat action))


evaluateState : Matrix Float -> Matrix Int -> Matrix Float
evaluateState previousStateValues policy =
    let
        newStateValues =
            Matrix.indexedMap
                (\i j value ->
                    expectedReturn previousStateValues ( i, j ) (policy |> Matrix.get i j |> Maybe.withDefault 0)
                )
                previousStateValues
    in
    if goodEnough 1.0e-4 previousStateValues newStateValues then
        newStateValues

    else
        evaluateState newStateValues policy


goodEnough : Float -> Matrix Float -> Matrix Float -> Bool
goodEnough delta m1 m2 =
    List.Extra.zip (Matrix.toList m1) (Matrix.toList m2)
        |> List.foldl
            (\( a1, a2 ) sum ->
                sum + abs (a1 - a2)
            )
            0.0
        |> (\sum -> sum < delta)


improvePolicy : Matrix Float -> Matrix Int -> Matrix Int
improvePolicy stateValues policy =
    Matrix.indexedMap
        (\i j value ->
            List.foldl
                (\action ( maxReturn, optimalAction ) ->
                    let
                        return =
                            if (action >= 0 && i >= action) || (action < 0 && j >= abs action) then
                                expectedReturn stateValues ( i, j ) action

                            else
                                -1.0e19
                    in
                    if return > maxReturn then
                        ( return, action )

                    else
                        ( maxReturn, optimalAction )
                )
                ( -1.0e19, 0 )
                actions
                |> Tuple.second
        )
        policy



-- Drawing


matrixUnitSize =
    60.0


drawText text =
    Text.fromString text
        |> Text.size Text.small
        |> rendered


drawAxis : (Int -> Collage msg) -> Collage msg
drawAxis f =
    List.range 0 maxCars
        |> List.foldl
            (\n collages ->
                f n :: collages
            )
            []
        |> group


drawTitle : String -> Collage msg
drawTitle title =
    rectangle (matrixUnitSize * toFloat maxCars) matrixUnitSize
        |> outlined invisible
        |> at base (title |> Text.fromString |> Text.size Text.huge |> rendered)
        |> shift ( 0.5 * matrixUnitSize * toFloat maxCars, 0 )


drawMatrix : Matrix String -> Collage msg
drawMatrix matrix =
    Matrix.indexedMap
        (\i j value ->
            square matrixUnitSize
                |> outlined (solid thin (uniform Color.black))
                |> at base (drawText value)
                |> shift ( matrixUnitSize * toFloat j, matrixUnitSize * toFloat i )
        )
        matrix
        |> Matrix.toList
        |> group


ticks =
    drawAxis
        (\n ->
            segment ( 0, 0 ) ( 0, matrixUnitSize / 4.0 )
                |> traced (solid thin (uniform Color.black))
                |> shift ( matrixUnitSize * toFloat n, 0.0 )
        )


numbers =
    drawAxis
        (\n ->
            drawText (String.fromInt n)
                |> shift ( matrixUnitSize * toFloat n, 0.0 )
        )


xAxis =
    [ ticks, spacer 0 8, numbers, drawTitle "# cars at second location" ]
        |> vertical
        |> shift ( 0, 15.0 - matrixUnitSize )


yAxis =
    [ drawTitle "# cars at first location", numbers, spacer 0 8, ticks ]
        |> vertical
        |> rotate (degrees 90)
        |> shift ( -matrixUnitSize - 36, 0.0 )


drawPolicy : Matrix Int -> Html msg
drawPolicy policy =
    let
        policyCollage =
            policy
                |> Matrix.map (\v -> String.fromInt v)
                |> drawMatrix
    in
    [ policyCollage, xAxis, yAxis ]
        |> group
        |> Render.svg


drawStateValue : Matrix Float -> Html msg
drawStateValue stateValues =
    let
        valueCollage =
            stateValues
                |> Matrix.map (\v -> Round.round 2 v)
                |> drawMatrix
    in
    [ valueCollage, xAxis, yAxis ]
        |> group
        |> Render.svg



---- MODEL ----


type alias Model =
    { policy : Matrix Int
    , stateValues : Matrix Float
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { stateValues = Matrix.repeat (maxCars + 1) (maxCars + 1) 0.0
      , policy = Matrix.repeat (maxCars + 1) (maxCars + 1) 0
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
            init ()

        Step ->
            ( { model
                | stateValues = evaluateState model.stateValues model.policy
                , policy = improvePolicy model.stateValues model.policy
              }
            , Cmd.none
            )

        Evaluate ->
            ( model
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ showButtons model
        , hr [] []
        , drawPolicy model.policy
        , text "Policy"
        , hr [] []
        , drawStateValue model.stateValues
        , text "Value State"
        ]


showButtons : Model -> Html Msg
showButtons model =
    div [] <|
        [ button [ onClick Reset ] [ text "Reset" ]
        , button [ onClick Step ] [ text "Step" ]
        -- , button [ onClick Evaluate ] [ text "Evaluate" ]
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
