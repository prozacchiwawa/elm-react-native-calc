port module Main exposing (..)

import Debug exposing (log)
import NativeUi exposing (string, style)
import NativeUi.NativeApp exposing (program)
import NativeUi.Elements exposing (text, view)
import NativeUi.Handlers exposing (onPress)
import NativeUi.Style exposing (flexDirection, padding, width, height, flex, alignItems)
import Json.Encode
import Json.Decode
import String
import Lazy.List as LL exposing (LazyList)

import Calc exposing (..)

type Action
    = NoOp
    | Calc Calc.Action

type alias Model =
    { calc : Calc.Model }

update action model =
    case action of 
      Calc ca ->
        let (mod,eff) = Calc.update ca model.calc in
        { model | calc = mod } ! [ Cmd.map Calc eff ]
      _ -> model ! []

calcView model =
    view 
       [style [flex 1, alignItems "center", flexDirection "row"]]
       [NativeUi.map Calc (Calc.calcView model.calc)]

main =
  program
    { init = ({ calc = Calc.init }, Cmd.none)
    , update = update
    , view = calcView
    , subscriptions = \_ -> Sub.none
    , renderPort = render
    , eventPort = event identity
    }

-- React Native
port render : Json.Encode.Value -> Cmd msg
port event : ((String, Json.Decode.Value) -> msg) -> Sub msg
