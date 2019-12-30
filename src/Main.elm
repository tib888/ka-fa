module Main exposing (..)

-- elso fraktalrajzolom
--
-- Dependencies:
--   elm install elm/svg
--   elm install elm/time

import Browser
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
--import Color exposing (..)
import Task
import Time

-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0)
  , Cmd.batch
      [ Task.perform AdjustTimeZone Time.here
      , Task.perform Tick Time.now
      ]
  )

-- UPDATE

type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model | time = newTime }
      , Cmd.none
      )

    AdjustTimeZone newZone ->
      ( { model | zone = newZone }
      , Cmd.none
      )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 50 Tick

-- VIEW

view : Model -> Html Msg
view model =
  let
    second = 0.01 * toFloat (Time.posixToMillis model.time)
    x = sin (second * 6 * pi / 180)
  in
    svg
    [ viewBox "0 0 600 600"
    , width "600"
    , height "600"
    ] (viewFractal 8 100.0 0.9 300.0 600.0 -90.0 50.0 (6.0 * x))    

viewFractal : Int -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> List(Svg msg)
viewFractal level len scale start_x start_y start_ang branch_ang u_ang =
  let
    a = start_ang * pi / 180.0 
    x = start_x + len * cos a
    y = start_y + len * sin a
    xx = (x + start_x) * 0.5
    yy = (y + start_y) * 0.5
  in
  if level < 1 then [] else
  List.concat [ 
    [
        line
        [ x1 (String.fromFloat start_x)
        , y1 (String.fromFloat start_y)
        , x2 (String.fromFloat x)
        , y2 (String.fromFloat y)
        , stroke (if level > 2 then "saddlebrown" else "green") 
        --, stroke (colorToHex (Color.rgb (255 - level * 16) (level * 32) 0)) -- #8B4513 -> 008000
        , strokeWidth (String.fromInt level)
        --, strokeLinecap "round"
        ]
        []
    ]
    , viewFractal (level - 1) (len * 0.55 * scale) scale xx yy (start_ang + branch_ang + u_ang) branch_ang u_ang
    , viewFractal (level - 1) (len * 0.55 * scale) scale xx yy (start_ang - branch_ang + u_ang) branch_ang u_ang
    , viewFractal (level - 1) (len * scale) scale x y (start_ang + u_ang) branch_ang u_ang
    ]

    