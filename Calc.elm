port module Calc exposing (..)

import NativeUi exposing (string)
import NativeUi.NativeApp exposing (program)
import NativeUi.Elements exposing (text)
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
  | Val Float
  | Close

type alias Model = Stack Float

stackText model =
  case pop model of
    (Just _, _) -> model |> stackIterator |> LL.map toString |> LL.toList |> String.join " "
    _ -> "Nothing"

update action model = 
  let doBin action =
    case action of
      Add -> (+)
      Sub -> (-)
      Mul -> (*)
      Div -> (/)
      _ -> (\a b -> a)
  in
  let updatedModel =
    let doBinary =
      let (aa, m1) = pop model in
      let (bb, m2) = pop m1 in
        case (aa,bb) of
          (Just a, Just b) ->
            push (doBin action a b) m2
          _ -> model
    in
    case action of
      NoOp -> model
      Add -> doBinary
      Sub -> doBinary
      Mul -> doBinary
      Div -> doBinary
      Val v -> push v model
      Close -> model
  in
  (updatedModel,
   case (action, pop updatedModel) of
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

main =
  program
    { init = ([], Cmd.none)
    , update = update
    , view = \model -> text [] [model |> stackText |> string]
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
