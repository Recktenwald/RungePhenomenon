module Runge exposing (Model, main)

import Array exposing (Array)
import Browser
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import LineChart as LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Axis.Line as AxisLine
import LineChart.Axis.Range as Range
import LineChart.Axis.Tick as Tick
import LineChart.Axis.Ticks as Ticks
import LineChart.Axis.Title as Title
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Coordinate as Coordinate
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import List
import Svg


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Msg
    = Increment
    | Decrement
    | ChangeNodes



-- MODEL


type Nodetype
    = Uniform
    | Chebyshev


other : Nodetype -> Nodetype
other nt =
    case nt of
        Uniform ->
            Chebyshev

        Chebyshev ->
            Uniform


showNodeType : Nodetype -> String
showNodeType nt =
    case nt of
        Uniform ->
            "Equidistributed interpolation nodes"

        Chebyshev ->
            "Chebyshev nodes"


type alias Model =
    { graph : List Float
    , xs : List Float
    , nodetype : Nodetype
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        n =
            60

        m =
            3
    in
    ( Model
        (List.map toFloat (List.range -n n)
            |> List.map (\x -> x / n)
        )
        (equispaced -1 1 m)
        Uniform
    , Cmd.none
    )


equispaced : Float -> Float -> Int -> List Float
equispaced a b n =
    let
        c =
            b - a
    in
    List.map toFloat (List.range 1 n)
        |> List.map (\x -> x / toFloat (n + 1))
        |> List.map ((*) c)
        |> List.map ((+) a)



-- This function should be improved to return chebyshev nodes on an interval [a,b]


chebyshevs : Float -> Float -> Int -> List Float
chebyshevs _ _ n =
    List.range 1 n
        |> List.map (\x -> cos ((2.0 * toFloat x - 1.0) / (2.0 * toFloat n) * pi))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( nodeFunction, otherNodeFunction ) =
            case model.nodetype of
                Uniform ->
                    ( equispaced, chebyshevs )

                Chebyshev ->
                    ( chebyshevs, equispaced )
    in
    case msg of
        Increment ->
            ( { model | xs = nodeFunction -1 1 (List.length model.xs + 1) }, Cmd.none )

        Decrement ->
            ( { model | xs = nodeFunction -1 1 (List.length model.xs - 1) }, Cmd.none )

        ChangeNodes ->
            ( { model | xs = otherNodeFunction -1 1 (List.length model.xs), nodetype = other model.nodetype }, Cmd.none )


rungeFun : Float -> Float
rungeFun x =
    1 / (1 + 25 * x * x)


plot : (Float -> Float) -> List Float -> List Point
plot f xs =
    let
        values =
            List.map f xs
    in
    List.map2 Tuple.pair xs values


type alias Point =
    ( Float, Float )


absDiffFun : (Float -> Float) -> (Float -> Float) -> Float -> Float
absDiffFun f g x =
    abs (f x - g x)


interpolynomEval : List Float -> (Float -> Float) -> Float -> Float
interpolynomEval nodes f x =
    neville (Array.fromList nodes) (Array.fromList (List.map f nodes)) 0 (List.length nodes - 1) x


neville : Array Float -> Array Float -> Int -> Int -> Float -> Float
neville xs ys i j x =
    if i == j then
        case Array.get i ys of
            Nothing ->
                0

            Just yi ->
                yi

    else
        case ( Array.get i xs, Array.get j xs ) of
            ( Nothing, _ ) ->
                0

            ( _, Nothing ) ->
                0

            ( Just xi, Just xj ) ->
                ((x - xj) * neville xs ys i (j - 1) x - (x - xi) * neville xs ys (i + 1) j x) / (xi - xj)


view : Model -> Html Msg
view model =
    div []
        [ LineChart.viewCustom chartConfig
            ([ LineChart.line Color.black Dots.none "Runge Function" (plot rungeFun model.graph)
             , LineChart.line Color.blue Dots.none "Interpolation" (plot (interpolynomEval model.xs rungeFun) model.graph)

             --, LineChart.line Color.red Dots.square "" (plot rungeFun [ 0 ])
             --, LineChart.line Color.red Dots.none "Difference" (plot (absDiffFun sin rungeFun) model.graph)
             ]
                ++ List.map (\x -> LineChart.line Color.red Dots.square "" (plot rungeFun [ x ])) model.xs
            )
        , div []
            [ button [ onClick Increment ] [ text "+" ]
            , div [] [ text (String.fromInt (List.length model.xs)) ]
            , button [ onClick Decrement ] [ text "-" ]
            ]
        , button [ onClick ChangeNodes ] [ text "Change Node Type" ]
        , text (showNodeType model.nodetype)
        ]



--chartConfig : Blub


yAxisConfig =
    Axis.custom
        { title = Title.default "y"
        , variable = Just << Tuple.second
        , pixels = 400
        , range = Range.window -1 1.5
        , axisLine = AxisLine.default
        , ticks = ticksConfig
        }


ticksConfig : Ticks.Config msg
ticksConfig =
    -- Ticks.int 7
    -- Ticks.time 7
    -- Ticks.float 7
    Ticks.floatCustom 7 someCustomTick


someCustomTick : Float -> Tick.Config msg
someCustomTick number =
    Tick.custom
        { position = number
        , color = Colors.black
        , width = 2
        , length = 2
        , grid = True
        , direction = Tick.negative
        , label = Just (Junk.label Colors.black (String.fromFloat number))
        }


viewLegends : Coordinate.System -> List (Legends.Legend msg) -> Svg.Svg msg
viewLegends system legends =
    Svg.g
        [ Junk.transform
            [ Junk.move system system.x.min system.y.min, Junk.offset 20 20 ]
        ]
        (List.indexedMap viewLegend (List.filter legendsNonEmpty legends))


legendsNonEmpty : Legends.Legend msg -> Bool
legendsNonEmpty l =
    not (String.isEmpty l.label)


viewLegend : Int -> Legends.Legend msg -> Svg.Svg msg
viewLegend index { sample, label } =
    Svg.g
        [ Junk.transform [ Junk.offset (toFloat index * 200) 20 ] ]
        [ sample, viewLabel label ]


viewLabel : String -> Svg.Svg msg
viewLabel label =
    Svg.g
        [ Junk.transform [ Junk.offset 40 4 ] ]
        [ Junk.label Colors.black label ]


chartConfig =
    { y = yAxisConfig
    , x = Axis.default 700 "x" Tuple.first
    , container = Container.default "line-chart-1"
    , interpolation = Interpolation.default
    , intersection = Intersection.default
    , legends = Legends.groupedCustom 25 viewLegends
    , events = Events.default
    , junk = Junk.default
    , grid = Grid.default
    , area = Area.default
    , line = Line.default
    , dots = Dots.default
    }



{--
      Tuple.first
        Tuple.second
        [ LineChart.line Color.black Dots.none "Runge Function" (plot rungeFun model)
        , LineChart.line Color.blue Dots.none "Sinus" (plot sin model)
        ]
      --}


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
