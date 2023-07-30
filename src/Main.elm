module Main exposing (main)

import Browser
import Browser.Events as E
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Array exposing (Array)
-- import Grid

main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

init : () -> (Model, Cmd Msg)
init () =
  ( { time = 0.0
    , map =
        initGrid
        gridWidth
        gridHeight
        (+)
    }
  , Cmd.none
  )

type Msg =
  TimeDelta Float

type alias Model =
  { map : Grid Int
  , time : Float
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TimeDelta delta ->
      ( if model.time + delta > 1.0 / frameRate then
          { map = simulate model.map
          , time = 0.0 }
        else
          { model | time = model.time + delta }
      , Cmd.none
      )

subscriptions : Model -> Sub Msg
subscriptions _ =
  E.onAnimationFrameDelta TimeDelta

simulate : Grid Int -> Grid Int
simulate grid =
  indexedGridMap
    ( \_ _ v ->
        v + 1
        |> modBy maxValue
    )
    grid

frameRate
  = 0.001

gridWidth
  = 20

gridHeight
  = 20

maxValue
  = gridWidth + gridHeight * 2

cellSize = 10

imgWidth
  = gridWidth * cellSize * 2

imgHeight
  = gridHeight * cellSize * 2

view { map, time } =
  svg
    [ width ( String.fromFloat imgWidth )
    , height ( String.fromFloat imgHeight )
    , viewBox "0 0 400 400"
    ]
    ( gridFoldl
        (::)
        []
        ( indexedGridMap
            ( \gX gY val ->
              rect
                [ x ( String.fromInt ( gX * cellSize ) )
                , y ( String.fromInt ( gY * cellSize ) )
                , width ( String.fromInt cellSize )
                , height ( String.fromInt cellSize )
                , fillOpacity
                    ( String.fromFloat
                      ( (toFloat val) / (toFloat maxValue) )
                    )
                , fill "green"
                ]
                []
            )
            map
        )
    )

type alias Grid a = Array ( Array (a, a), Array (a, a) )

-- Width and height are doubled because we must have an even number of blocks.
initGrid : Int -> Int -> (Int -> Int -> a) -> Grid a
initGrid width height f =
  Array.initialize
    width
    ( \x ->
      ( Array.initialize
        height
        ( \y ->
          ( f (x * 2) (y * 2)
          , f ((x * 2) + 1) (y * 2)
          )
        )
      , Array.initialize
        height
        ( \y ->
          ( f (x * 2) ((y * 2) + 1)
          , f ((x * 2) + 1) ((y * 2) + 1)
          )
        )
      )
    )

indexedGridMap : (Int -> Int -> a -> b) -> Grid a -> Grid b
indexedGridMap f =
  Array.indexedMap
    ( \x rows ->
      ( Array.indexedMap
          ( \y values ->
            ( f (x*2) (y*2) (Tuple.first values )
            , f ((x*2)+1) (y*2) ( Tuple.second values )
            )
          )
          ( Tuple.first rows)
      , Array.indexedMap
          ( \y values ->
            ( f (x*2) ((y*2)+1) ( Tuple.first values )
            , f ((x*2)+1) ((y*2)+1) ( Tuple.second values )
            )
          )
          ( Tuple.second rows)
      )
    )

gridFoldl : (a -> b -> b) -> b -> Grid a -> b
gridFoldl f acc =
  Array.foldl
    ( \(r1, r2) a1 ->
      let
          a2 = Array.foldl
                 ( \(v1, v2) a3 ->
                    f v2 ( f v1 a3 )
                 )
                 a1
                 r1
      in
        Array.foldl
          ( \(v3, v4) a4 ->
            f v4 ( f v3 a4 )
          )
          a2
          r2
    )
    acc


--    , text_
--      [ x ( String.fromFloat (imgWidth / 2) )
--      , y ( String.fromFloat (imgHeight / 2) )
--      , fill "green"
--      ]
--      [ text "Locust has Landed" ]
