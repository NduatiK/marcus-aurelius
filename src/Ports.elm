port module Ports exposing (..)


type alias ScreenData =
    { scrollTop : Float
    , pageHeight : Int
    , viewportHeight : Int
    , viewportWidth : Int
    }


port scrollTop : Int -> Cmd msg


port scrollOrResize : (ScreenData -> msg) -> Sub msg


percFloat : Int -> ScreenData -> Float
percFloat limit data =
    (data.scrollTop * toFloat limit) / toFloat (data.pageHeight - data.viewportHeight)


perc : Int -> ScreenData -> String
perc limit data =
    data
        |> percFloat limit
        |> round
        |> String.fromInt
