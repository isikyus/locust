module Main exposing (main)

import Browser
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Grid

main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }

init =
  Grid.initialize
    gridWidth
    gridHeight
    (+)

type Msg = Update

update msg model =
  case msg of
    Update ->
      model

gridWidth
  = 40

gridHeight
  = 40

maxValue
  = gridWidth + gridHeight

cellSize = 10

imgWidth
  = gridWidth * cellSize

imgHeight
  = gridHeight * cellSize

view model =
  svg
    [ width ( String.fromFloat imgWidth )
    , height ( String.fromFloat imgHeight )
    , viewBox "0 0 400 400"
    ]
    ( Grid.foldl
        (::)
        []
        ( Grid.indexedMap
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
            model
        )
    )
--    , text_
--      [ x ( String.fromFloat (imgWidth / 2) )
--      , y ( String.fromFloat (imgHeight / 2) )
--      , fill "green"
--      ]
--      [ text "Locust has Landed" ]
