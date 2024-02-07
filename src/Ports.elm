port module Ports exposing (storeMap)

import Json.Encode exposing (Value)


port storeMap : Value -> Cmd a
