module Responsive exposing (..)


type alias Dimensions =
    { width : Int, height : Int }


type ClientType
    = Unknown
    | Desktop
    | Mobile


determineClientType : Dimensions -> ClientType
determineClientType dimensions =
    if dimensions.width < 1000 then
        Mobile

    else
        Desktop
