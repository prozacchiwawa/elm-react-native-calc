port module Calc exposing (..)

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

import Stack exposing (..)

type Action
  = NoOp
  | Add
  | Sub
  | Mul
  | Div
  | Bad String
  | Val Float
  | Input Char
  | Clear
  | Again
  | Close

type alias Model =
    { stack : Stack Float
    , input : String
    }

button props children =
    NativeUi.node "Button" props children

stackText model =
  case pop model.stack of
    (Just _, _) -> model.stack |> stackIterator |> LL.map toString |> LL.toList |> String.join " "
    _ -> "Nothing"

update action model =
  let doBin action =
    case log "Action" action of
      Add -> (+)
      Sub -> (-)
      Mul -> (*)
      Div -> (/)
      _ -> (\a b -> a)
  in
  let updatedModel =
    let doBinary =
      let (aa, m1) = pop model.stack in
      let (bb, m2) = pop m1 in
        case (aa,bb) of
          (Just a, Just b) ->
            { model | stack = push (doBin action a b) m2 }
          _ -> model
    in
    case action of
      NoOp -> model
      Add -> doBinary
      Sub -> doBinary
      Mul -> doBinary
      Div -> doBinary
      Val v -> { model | stack = push v model.stack, input = "" }
      Bad s -> { model | input = s }
      Clear -> { model | input = "" }
      Input ch -> { model | input = String.append model.input (String.fromChar ch) }
      Again -> { model | stack = [], input = "" }
      Close -> model
  in
  (updatedModel,
   case (action, pop updatedModel.stack) of
     (Close, (Just a, st)) -> Cmd.batch [output (toString a), end 0]
     (Close, (Nothing, st)) -> end 1
     (_, (_, st)) -> updatedModel |> stackText |> output
  )

translateInputString str =
  case str of
    "+" -> Add
    "-" -> Sub
    "*" -> Mul
    "/" -> Div
    _ -> case String.toFloat str of
      Ok v -> Val v
      Err _ -> NoOp

calcView model =
    let buttonRowStyle = style [flexDirection "row"] in
    let buttonStyle = style [width 40, height 35] in
    let onPressVal =
        case String.toFloat model.input of
            Ok v -> onPress (Val v)
            Err e -> onPress (Bad e)
    in
    view [style [flex 1, alignItems "center", flexDirection "row"]] [
        view [style [flex 1, alignItems "center", flexDirection "column"]] [
            text [] [
                string "Input: "
            ,   string model.input
            ]
        ,   text [] [
                string "Calculator Stack: "
            ,   model |> stackText |> string
            ]
        ,   view [buttonRowStyle] [
                button [onPress Add, buttonStyle] [string "+"]
            ,   button [onPress Sub, buttonStyle] [string "-"]
            ,   button [onPress Mul, buttonStyle] [string "*"]
            ]
        , view [buttonRowStyle] [
                button [onPress Div, buttonStyle] [string "/"]
            ,   button [onPress Clear, buttonStyle] [string "CLR"]
            ,   button [onPress Again, buttonStyle] [string "NEW"]
            ]
        ,   view [buttonRowStyle] [
                button [onPress (Input '1'), buttonStyle] [string "1"]
            ,   button [onPress (Input '2'), buttonStyle] [string "2"]
            ,   button [onPress (Input '3'), buttonStyle] [string "3"]
            ]
        ,   view [buttonRowStyle] [
                button [onPress (Input '4'), buttonStyle] [string "4"]
            ,   button [onPress (Input '5'), buttonStyle] [string "5"]
            ,   button [onPress (Input '6'), buttonStyle] [string "6"]
            ]
        ,   view [buttonRowStyle] [
                button [onPress (Input '7'), buttonStyle] [string "7"]
            ,   button [onPress (Input '8'), buttonStyle] [string "8"]
            ,   button [onPress (Input '9'), buttonStyle] [string "9"]
            ]
        ,   view [buttonRowStyle] [
                button [onPress (Input '-'), buttonStyle] [string "-"]
            ,   button [onPress (Input '0'), buttonStyle] [string "0"]
            ,   button [onPress (Input '.'), buttonStyle] [string "."]
            ]
        ,   button [style [width 120, height 35], onPressVal] [
                string "Enter"
            ]
        ]
    ]

main =
  program
    { init = ({ stack = [], input = "" }, Cmd.none)
    , update = update
    , view = calcView
    , subscriptions = \_ -> Sub.batch [input translateInputString, close (\_ -> Close)]
    , renderPort = render
    , eventPort = event identity
    }

port input : (String -> msg) -> Sub msg
port output : String -> Cmd msg
port close : ({} -> msg) -> Sub msg
port end : Int -> Cmd msg

-- React Native
port render : Json.Encode.Value -> Cmd msg
port event : ((String, Json.Decode.Value) -> msg) -> Sub msg
