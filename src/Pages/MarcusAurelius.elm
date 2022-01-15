module Pages.MarcusAurelius exposing (Model, Msg, page)

import Angle exposing (Angle)
import Array
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Browser
import Browser.Dom
import Browser.Events
import Camera3d
import Color exposing (Color)
import Direction3d
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import FeatherIcons
import Gen.Params.MarcusAurelius exposing (Params)
import Html exposing (Html)
import Html.Attributes exposing (class, id, style)
import Html.Events
import Http
import Illuminance exposing (Illuminance)
import Json.Decode
import Length exposing (Meters)
import Luminance
import LuminousFlux exposing (LuminousFlux)
import Obj.Decode exposing (Decoder, ObjCoordinates)
import Page
import Pixels exposing (Pixels)
import Point3d exposing (Point3d, origin)
import Ports exposing (..)
import Process
import Quantity exposing (Quantity)
import Request
import Scene3d
import Scene3d.Light as Light
import Scene3d.Material exposing (Texture)
import Scene3d.Mesh exposing (Shadow, Textured, Uniform)
import Shared
import SketchPlane3d
import Svg
import Svg.Attributes as SvgA
import Task
import Time
import TriangularMesh exposing (TriangularMesh)
import View exposing (View)
import Viewpoint3d
import WebGL.Texture


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { meshes :
        { bust : Maybe (Uniform ObjCoordinates)
        , halo : Maybe (Uniform ObjCoordinates)
        }
    , azimuth : Angle
    , elevation : Angle
    , zoom : Float
    , percent : Float
    , screenData : Maybe ScreenData
    }


rotation =
    60


init : ( Model, Cmd Msg )
init =
    ( { meshes = { bust = Nothing, halo = Nothing }
      , azimuth = Angle.degrees -rotation
      , elevation = Angle.degrees 16
      , zoom = 1.85
      , percent = 0
      , screenData = Nothing
      }
    , Cmd.batch
        [ Http.get
            { url = "circle.obj.txt" -- .txt is required to work with `elm reactor`
            , expect = Http.expectString GotHalo
            }
        , Http.get
            { url = "marcus-aurelius.obj.txt" -- .txt is required to work with `elm reactor`
            , expect = Http.expectString GotBust
            }
        , Browser.Dom.setViewport 0 0
            |> Task.andThen (\_ -> Browser.Dom.getViewport)
            |> Task.perform
                (\info ->
                    OnResize
                        { scrollTop = info.viewport.y
                        , pageHeight = round info.scene.height
                        , viewportHeight = round info.viewport.height
                        , viewportWidth = round info.viewport.width
                        }
                )
        ]
    )


updateViewport : Cmd Msg
updateViewport =
    Browser.Dom.setViewport 0 0
        |> Task.andThen (\_ -> Browser.Dom.getViewportOf "app")
        |> Task.attempt
            (\res ->
                res
                    |> Result.map
                        (\info ->
                            OnResize
                                { scrollTop = info.viewport.y
                                , pageHeight = round info.scene.height
                                , viewportHeight = round info.viewport.height
                                , viewportWidth = round info.viewport.width
                                }
                        )
                    |> Result.toMaybe
                    |> Maybe.withDefault NoOp
            )



--         { scene :
--     { width : Float
--     , height : Float
--     }
-- , viewport :
--     { x : Float
--     , y : Float
--     , width : Float
--     , height : Float
--     }
-- UPDATE


type Msg
    = GotBust (Result Http.Error String)
    | GotHalo (Result Http.Error String)
    | LoadedMeshes (Result Http.Error (Uniform ObjCoordinates))
    | Scrolled
    | MouseWheel Float
    | ScrollTop Int
    | GotNewHeight Int
    | OnResize ScreenData
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        meshes =
            model.meshes

        decodeMesh str =
            str
                |> Result.toMaybe
                |> Maybe.andThen
                    (\res ->
                        Obj.Decode.decodeString Length.meters meshWithBoundingBoxDecoder res
                            |> Result.toMaybe
                    )
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotBust result ->
            ( { model
                | meshes = { meshes | bust = decodeMesh result }
              }
            , Cmd.none
            )

        GotHalo result ->
            ( { model
                | meshes = { meshes | halo = decodeMesh result }
              }
            , Cmd.none
            )

        LoadedMeshes result ->
            ( model
            , Cmd.none
            )

        MouseWheel deltaY ->
            let
                screenData =
                    model.screenData
                        |> Maybe.map
                            (\data ->
                                { data
                                    | scrollTop =
                                        (data.scrollTop + deltaY)
                                            |> clamp 0 (toFloat (data.pageHeight - data.viewportHeight))
                                }
                            )
            in
            ( { model
                | screenData = screenData
                , percent = calcPercent screenData
              }
            , Cmd.none
            )

        Scrolled ->
            ( model, updateViewport )

        ScrollTop position ->
            ( model, Cmd.none )

        OnResize data ->
            ( { model
                | screenData = Just data
                , percent =
                    if model.screenData == Nothing then
                        0

                    else
                        calcPercent (Just data)
              }
            , Cmd.none
            )

        GotNewHeight vHeight ->
            ( { model
                | screenData =
                    model.screenData
                        |> Maybe.map
                            (\sd ->
                                { sd
                                    | viewportHeight = vHeight
                                    , pageHeight = round (3 * toFloat vHeight * 1.5)
                                }
                            )
              }
            , Cmd.none
            )


calcPercent screenData =
    screenData
        |> Maybe.map
            (\sd ->
                percFloat 1 sd
                    |> clamp 0 1
            )
        |> Maybe.withDefault 0


calculateAzimuth percent azimuth =
    let
        stage1 internalPercent =
            Angle.inDegrees azimuth + rotation * internalPercent * internalPercent + -20 * internalPercent * internalPercent

        stage2 internalPercent =
            stage1 1 - rotation * internalPercent * internalPercent - 5.5 * internalPercent
    in
    if percent < 0.45 then
        -- transition to profile
        stage1 (percent / 0.45)

    else
        -- transition to head
        let
            percent_ =
                (clamp 0 0.9 percent - 0.45) / (1 - 0.45)
        in
        stage2 percent_


calculateElevation percent elevation =
    let
        stage1 internalPercent =
            Angle.inDegrees elevation - Angle.inDegrees elevation * internalPercent

        stage2 internalPercent =
            stage1 1 - Angle.inDegrees elevation * internalPercent * internalPercent
    in
    if percent < 0.45 then
        -- transition to profile
        stage1 (percent / 0.45)
            |> Angle.degrees

    else
        -- transition to head
        let
            percent_ =
                (percent - 0.45) / (1 - 0.45)
        in
        stage2 percent_
            |> Angle.degrees


meshWithBoundingBoxDecoder : Decoder (Uniform ObjCoordinates)
meshWithBoundingBoxDecoder =
    Obj.Decode.oneOf
        [ Obj.Decode.map Scene3d.Mesh.indexedFaces Obj.Decode.faces
        , Obj.Decode.map Scene3d.Mesh.indexedFacets Obj.Decode.triangles
        , Obj.Decode.map Scene3d.Mesh.indexedFacets Obj.Decode.triangles
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize (\_ h -> GotNewHeight h)
        ]



-- VIEW


view : Model -> View Msg
view model =
    let
        loaded =
            case ( model.meshes.bust, model.meshes.halo ) of
                ( Just _, Just _ ) ->
                    True

                _ ->
                    False
    in
    { title = "Homepage"
    , body =
        List.singleton <|
            Html.div
                [ style "height" "100vh"
                , style "margin" "0"
                , style "padding" "0"
                , style "width" "100vw"
                , style "height" "100vh"
                , style "overflow" "auto"
                , id "app"
                , Html.Events.on "scroll" (Json.Decode.succeed Scrolled)
                ]
            <|
                List.singleton <|
                    layout
                        [ Font.color white
                        , Font.family
                            [ Font.typeface "Libre Baskerville"
                            , Font.serif
                            ]
                        , Font.letterSpacing 0.1
                        , Font.regular
                        , style "height"
                            (if loaded then
                                "350vh"

                             else
                                "100vh"
                            )
                            |> htmlAttribute
                        , behindContent (background model)

                        -- , inFront (debugInfo model)
                        ]
                    <|
                        if loaded then
                            column
                                [ height fill
                                , width fill

                                -- , Html.Events.on "wheel"
                                --     (Json.Decode.map
                                --         (\deltaY -> MouseWheel deltaY)
                                --         (Json.Decode.field "deltaY" Json.Decode.float)
                                --     )
                                --     |> htmlAttribute
                                ]
                                [ section1 model
                                , section2
                                , section3
                                ]

                        else
                            none
    }


debugInfo model =
    text
        (case model.screenData of
            Nothing ->
                "0"

            Just screenData ->
                "{\n"
                    ++ ("percent: " ++ String.fromFloat model.percent)
                    ++ ",\n"
                    ++ ("percent2: " ++ String.fromFloat ((model.percent - 0.45) / (1 - 0.45)))
                    ++ ",\n"
                    ++ ("scrollTop: " ++ String.fromFloat screenData.scrollTop)
                    ++ ",\n"
                    ++ ("elevation: " ++ String.fromFloat (Angle.inDegrees (calculateElevation model.percent model.elevation)))
                    ++ ",\n"
                    ++ ("azimuth: " ++ String.fromFloat (calculateAzimuth model.percent model.azimuth))
                    ++ ",\n"
                    ++ ("height: " ++ String.fromInt screenData.viewportHeight)
                    ++ ",\n"
                    ++ "}"
        )


section1 model =
    column
        [ style "max-height" "70vh" |> htmlAttribute
        , style "padding-top" "20vh" |> htmlAttribute
        , style "padding-bottom" "20vh" |> htmlAttribute
        , width fill
        , height fill
        , padding 20
        , inFront
            (row
                [ centerX
                , alignBottom
                , width shrink
                , spacing 20
                , Font.family
                    [ Font.typeface "Libre Baskerville"
                    , Font.serif
                    ]
                , below
                    (FeatherIcons.chevronDown
                        |> FeatherIcons.toHtml
                            [ style "margin-top" "20vh"
                            ]
                        |> html
                        |> el
                            [ centerX
                            , Font.color
                                (whiteWithA
                                    ((model.percent / 0.3)
                                        |> clamp 0 1
                                        |> (\a -> (1 - a) ^ 3)
                                    )
                                )
                            ]
                    )
                ]
                [ textColumn
                    [ Font.size 48
                    , centerX
                    , alignBottom
                    , width shrink
                    ]
                    [ paragraph [ width shrink, Font.alignRight ] [ text "Marcus" ]
                    , paragraph [ width shrink, Font.alignRight ] [ text "Aurelius" ]
                    , paragraph
                        [ width shrink
                        , Font.alignRight
                        , Font.size 36
                        , Font.italic
                        , paddingXY 0 8
                        ]
                        [ text "Philosopher / "
                        , el [ width shrink, Font.alignRight ] (text "King")
                        ]
                    ]
                , el
                    [ height fill
                    , width (px 2)
                    , Background.color (gray 0.8)
                    ]
                    none
                , textColumn
                    [ width (px 300)
                    , Font.size 13
                    , spacing 10
                    , Font.color (gray 0.2)

                    -- , Font.family
                    --     [ Font.typeface "Inter"
                    --     , Font.sansSerif
                    --     ]
                    ]
                    [ paragraph [] [ text "Roman emperor from 161 to 180 and Stoic philosopher." ]
                    , paragraph [] [ text "Last of the Five Good Emperors." ]

                    -- , paragraph [] [ text "Last emperor of the Pax Romana." ]
                    , paragraph [] [ text "Roman consul in 140, 145, and 161." ]
                    ]
                ]
            )
        ]
        []


section2 =
    column
        [ style "max-height" "120vh" |> htmlAttribute
        , style "margin-top" "30vh" |> htmlAttribute
        , style "margin-bottom" "70vh" |> htmlAttribute
        , style "padding-left" "10%" |> htmlAttribute
        , width fill
        , height fill
        , inFront
            (column
                [ alignBottom
                , width shrink
                , spacing 20
                , style "padding-left" "5%" |> htmlAttribute
                , Font.size 13
                , Font.color (gray 0.3)
                ]
                [ textColumn
                    [ alignBottom
                    , width shrink
                    , spacing 10
                    ]
                    [ paragraph
                        [ width shrink
                        , Font.size 48
                        , Font.color white
                        , Font.light
                        ]
                        [ text "Full name" ]
                    , paragraph [ width shrink ] [ text "Marcus Aurelius Antoninus Augustus" ]
                    ]
                , textColumn
                    [ alignBottom
                    , width shrink
                    , spacing 10
                    ]
                    [ paragraph
                        [ width shrink
                        , Font.size 48
                        , Font.color white
                        , Font.light
                        ]
                        [ text "Born" ]
                    , paragraph [ width shrink ] [ text "April 26, 121 AD, Rome, Italy" ]
                    ]
                , textColumn
                    [ alignBottom
                    , width shrink
                    , spacing 10
                    ]
                    [ paragraph
                        [ width shrink
                        , Font.size 48
                        , Font.color white
                        , Font.light
                        ]
                        [ text "Spouse" ]
                    , paragraph [ width shrink ] [ text "Faustina the Younger" ]
                    , paragraph [ width (px 350) ] [ text "Daughter of Roman Emperor Antoninus Pius and Roman Empress Faustina the Elder." ]
                    ]
                ]
            )
        ]
        []


section3 =
    column
        [ width fill
        , style "padding-left" "10%" |> htmlAttribute
        , height fill
        , spaceEvenly
        ]
        [ textColumn
            [ width shrink
            , spacing 40
            ]
            [ paragraph
                [ width shrink
                , Font.size 48
                , Font.color white
                , Font.light
                ]
                [ text "Quotes" ]
            , textColumn [ spacing 8 ]
                [ paragraph [ width shrink ] [ text "You have power over your mind- not outside events." ]
                , paragraph [ width shrink ] [ text "Realize this, and you will find strength." ]
                ]
            , textColumn [ spacing 8 ]
                [ paragraph [ width shrink ] [ text "Everything we hear is an opinion, not a fact. " ]
                , paragraph [ width shrink ] [ text "Everything we see is a perspective, not the truth." ]
                ]
            , textColumn [ spacing 8 ]
                [ paragraph [ width shrink ] [ text "Waste no more time arguing about what a good man should be." ]
                , paragraph [ width shrink, Font.italic ] [ text "Be one." ]
                ]
            , textColumn [ spacing 8 ]
                [ paragraph [ width shrink ] [ text "Never let the future disturb you. You will meet it, if you have to," ]
                , paragraph [ width shrink ] [ text "with the same weapons of reason which today arm you against the present." ]
                ]
            ]
        ]


background model =
    let
        vHeight =
            model.screenData
                |> Maybe.map .viewportHeight
                |> Maybe.withDefault 640

        azimuth =
            calculateAzimuth model.percent model.azimuth

        zoom =
            if model.percent < 1 then
                model.zoom * (1 + model.percent * 0.01)

            else
                model.zoom * (1 + model.percent * 0.01 + (model.percent - 1) * 0.01)

        elevation =
            calculateElevation model.percent model.elevation

        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint =
                            Point3d.meters 0
                                0
                                (if model.percent < 0.45 then
                                    0.2

                                 else
                                    0.2 + ((model.percent - 0.45) / (1 - 0.45)) * 0.1
                                )
                        , azimuth = Angle.degrees azimuth
                        , elevation = elevation
                        , distance = Length.meters (16 - zoom * 8)
                        }
                , verticalFieldOfView = Angle.degrees 30
                }

        pointLight1 =
            Light.point (Light.castsShadows True)
                { position = Point3d.meters 0.3 -0.2 0.1
                , intensity = LuminousFlux.lumens 1200000
                , chromaticity = Light.sunlight
                }

        pointLight2 =
            Light.point (Light.castsShadows True)
                { position = Point3d.meters -0.3 -0.2 0.1
                , intensity = LuminousFlux.lumens 100000
                , chromaticity = Light.sunlight
                }

        pointLight3 =
            Light.point (Light.castsShadows True)
                { position = Point3d.meters 0 0.4 0.1
                , intensity = LuminousFlux.lumens 100000
                , chromaticity = Light.sunlight
                }

        diffuse =
            Light.overhead
                { upDirection = Direction3d.positiveZ
                , chromaticity = Light.skylight
                , intensity = Illuminance.lux 20000
                }

        lights =
            Scene3d.fourLights pointLight1
                pointLight2
                pointLight3
                diffuse

        renderScene entities =
            Scene3d.custom
                { lights = lights
                , camera = camera
                , clipDepth = Length.meters 0.1
                , exposure = Scene3d.exposureValue 13
                , toneMapping = Scene3d.noToneMapping
                , whiteBalance = Light.daylight
                , antialiasing = Scene3d.multisampling
                , dimensions =
                    ( Pixels.int
                        (((640 + toFloat vHeight) / 2)
                            |> round
                            |> clamp 640 vHeight
                        )
                    , Pixels.int vHeight
                    )

                -- , background = Scene3d.backgroundColor Color.lightGray
                , background = Scene3d.backgroundColor Color.black
                , entities = entities
                }
    in
    case ( model.meshes.bust, model.meshes.halo ) of
        ( Just bust, Just halo ) ->
            html <|
                Html.div
                    [ Html.Attributes.style "width" "100%"
                    , Html.Attributes.style "height" "100vh"
                    , Html.Attributes.style "top" "0"
                    , Html.Attributes.style "right" "0"
                    , Html.Attributes.style "position" "fixed"
                    , Html.Attributes.style "overflow" "hidden"
                    , Html.Attributes.style "background-color" "black"
                    ]
                    [ Html.div
                        [ Html.Attributes.style "display" "flex"
                        , Html.Attributes.style "justify-content" "end"
                        ]
                        [ renderScene
                            [ Scene3d.mesh
                                -- (Scene3d.Material.matte Color.black)
                                (Scene3d.Material.pbr
                                    { baseColor = Color.black
                                    , roughness = 0.6
                                    , metallic = 0.9
                                    }
                                )
                                bust
                            , Scene3d.mesh
                                -- (Scene3d.Material.matte Color.black)
                                (Scene3d.Material.emissive
                                    Light.sunlight
                                    (Luminance.nits 200000)
                                )
                                halo
                                |> Scene3d.rotateAround Axis3d.x (Angle.degrees 90)
                                |> Scene3d.scaleAbout origin 0.008
                                |> Scene3d.translateIn Direction3d.negativeX (Length.meters 0.02)
                                |> Scene3d.translateIn Direction3d.positiveY (Length.meters 0.5)
                                |> Scene3d.translateIn Direction3d.positiveZ (Length.meters 0.35)
                            ]
                        ]
                    ]

        _ ->
            el [ centerX, centerY ] (text "Loading…")



--ICONS


icon icon_ size_ attr =
    icon_
        |> FeatherIcons.withSize size_
        |> FeatherIcons.toHtml []
        |> html
        |> el attr



-- COLORS


white =
    rgb 1 1 1


whiteWithA a =
    rgba 1 1 1 a


orangeWithA a =
    rgba255 252 102 59 a


orange =
    orangeWithA 1


toStr color =
    let
        c =
            toRgb color
    in
    "rgba("
        ++ String.fromInt (round (c.red * 255))
        ++ ","
        ++ String.fromInt (round (c.green * 255))
        ++ ","
        ++ String.fromInt (round (c.blue * 255))
        ++ ","
        ++ String.fromFloat c.alpha
        ++ ")"


gray intensity =
    rgb (1 - intensity) (1 - intensity) (1 - intensity)


darkness =
    rgb255 134 125 122


dropShadow o =
    style "filter"
        ("drop-shadow("
            ++ String.fromInt (Tuple.first o.offset)
            ++ "px "
            ++ String.fromInt (Tuple.second o.offset)
            ++ "px "
            ++ String.fromInt o.blur
            ++ "px "
            ++ toStr o.color
            ++ ")"
        )
        |> htmlAttribute
