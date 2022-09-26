module Main exposing (main)

import Browser
import Html exposing (Html, div, text)

main =
  Browser.sandbox { init = 0, update = update, view = view }

type Msg = Update

update msg model =
  case msg of
    Update ->
      model

view model =
  div []
    [ text "Locust has Landed" ]
