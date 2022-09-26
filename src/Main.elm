module Main exposing (main)

import Browser
import Svg exposing (..)
import Svg.Attributes exposing (..)

main =
  Browser.sandbox { init = 0, update = update, view = view }

type Msg = Update

update msg model =
  case msg of
    Update ->
      model

imgWidth
  = 400

imgHeight
  = 400

view model =
  svg
    [ width ( String.fromFloat imgWidth )
    , height ( String.fromFloat imgHeight )
    , viewBox "0 0 400 400"
    ]
    [ text_
      [ x ( String.fromFloat (imgWidth / 2) )
      , y ( String.fromFloat (imgHeight / 2) )
      , fill "green"
      ]
      [ text "Locust has Landed" ]
    ]
