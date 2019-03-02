module Chapter04.GamblersProblem exposing (main)

import Array exposing (Array)
import Array.Extra
import Browser
import Html exposing (Html, div, h1, img, p, text)
import Html.Attributes exposing (class, src)
import LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Axis.Line as AxisLine
import LineChart.Axis.Range as Range
import LineChart.Axis.Tick as Tick
import LineChart.Axis.Ticks as Ticks
import LineChart.Axis.Title as Title
import LineChart.Axis.Values as Values
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk exposing (..)
import LineChart.Legends as Legends
import LineChart.Line as Line



-- Algorithm


goal =
    100


evaluate : Float -> Array Float -> Array Float
evaluate headProbability stateValues =
    let
        nextStateValues =
            List.range 1 (goal - 1)
                |> List.foldl
                    (\state newStateValues ->
                        List.range 0 (min state (goal - state))
                            |> List.foldl
                                (\action returns ->
                                    let
                                        return =
                                            headProbability
                                                * (Array.get (state + action) stateValues |> Maybe.withDefault 0.0)
                                                + (1 - headProbability)
                                                * (Array.get (state - action) stateValues |> Maybe.withDefault 0.0)
                                    in
                                    return :: returns
                                )
                                []
                            |> List.maximum
                            |> Maybe.withDefault 0.0
                            |> (\maxReturn -> maxReturn :: newStateValues)
                    )
                    []
                |> List.append [ 1.0 ]
                |> List.reverse
                |> List.append [ 0.0 ]
                |> Array.fromList
    in
    if goodEnough 1.0e-9 nextStateValues stateValues then
        nextStateValues

    else
        evaluate headProbability nextStateValues


goodEnough : Float -> Array Float -> Array Float -> Bool
goodEnough delta a1 a2 =
    Array.Extra.map2
        (\a b -> abs (a - b))
        a1
        a2
        |> Array.foldl (+) 0.0
        |> (\sum -> sum < delta)


valueFunction : Float -> List Point
valueFunction headProbability =
    evaluate headProbability (Array.repeat (goal + 1) 0.0)
        |> Array.toList
        |> List.indexedMap (\index a -> Point (toFloat index) a)


policy : Float -> List Point
policy headProbability =
    let
        stateValues =
            evaluate headProbability (Array.repeat (goal + 1) 0.0)
    in
    List.range 1 goal
        |> List.map
            (\state ->
                List.range 1 (min state (goal - state))
                    |> List.foldl
                        (\action ( optimalAction, maxReturn ) ->
                            let
                                return =
                                    headProbability
                                        * (Array.get (state + action) stateValues |> Maybe.withDefault 0.0)
                                        + (1 - headProbability)
                                        * (Array.get (state - action) stateValues |> Maybe.withDefault 0.0)

                                roundedReturn =
                                    round (return * 100000.0)
                            in
                            if roundedReturn > maxReturn then
                                ( action, roundedReturn )

                            else
                                ( optimalAction, maxReturn )
                        )
                        ( 0, -10000 )
                    |> Tuple.first
            )
        |> List.append [ 0 ]
        |> List.indexedMap (\index a -> Point (toFloat index) (toFloat a))



---- MODEL ----


type alias Model =
    { intSeed : Int
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { intSeed = 0 }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ valueChart
        , policyChart <| LineChart.line Colors.blue Dots.none "Ph=0.4" (policy 0.4)
        , policyChart <| LineChart.line Colors.redLight Dots.none "Ph=0.25" (policy 0.25)
        , policyChart <| LineChart.line Colors.green Dots.none "Ph=0.55" (policy 0.55)
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


{-| A point in the Cartesian plane
given by x and y coordinates
-}
type alias Point =
    { x : Float
    , y : Float
    }


policyChart : LineChart.Series Point -> Html msg
policyChart data =
    LineChart.viewCustom
        { y =
            Axis.default 500 "Policy" .y
        , x =
            Axis.default 1000 "Capital" .x
        , container = Container.styled "line-chart-1" [ ( "font-family", "monospace" ) ]
        , interpolation = Interpolation.stepped
        , intersection = Intersection.default
        , legends = Legends.grouped .max .max -10 0
        , events = Events.default
        , junk = Junk.default
        , grid = Grid.default
        , area = Area.default
        , line = Line.default
        , dots = Dots.default
        }
        [ data ]


valueChart : Html.Html msg
valueChart =
    LineChart.viewCustom
        { y =
            customYAxis
        , x =
            customXAxis
        , container = Container.styled "line-chart-1" [ ( "font-family", "monospace" ) ]
        , interpolation = Interpolation.default
        , intersection = Intersection.default
        , legends = Legends.grouped .max .max -10 0
        , events = Events.default
        , junk = Junk.default
        , grid = Grid.default
        , area = Area.default
        , line = Line.default
        , dots = Dots.default
        }
        [ LineChart.line Colors.blue Dots.none "Ph=0.4" <| valueFunction 0.4
        , LineChart.line Colors.redLight Dots.none "Ph=0.25" <| valueFunction 0.25
        , LineChart.line Colors.green Dots.none "Ph=0.55" <| valueFunction 0.55
        ]


customXAxis : Axis.Config Point msg
customXAxis =
    Axis.custom
        { title = Title.default "Capital"
        , variable = Just << .x
        , pixels = 1100
        , range =
            Range.padded 20 20
        , axisLine =
            AxisLine.default
        , ticks =
            Ticks.int 20
        }


customYAxis : Axis.Config Point msg
customYAxis =
    Axis.custom
        { title = Title.atAxisMax 80 10 "Value estimate"
        , variable = Just << .y
        , pixels = 600
        , range =
            Range.padded 20 20
        , axisLine =
            AxisLine.default
        , ticks =
            Ticks.float 10
        }
