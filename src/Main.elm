port module Main exposing (..)

import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Html.Events
import Json.Decode


---- MODEL ----


type alias Model =
    { dragging : Bool
    , files : List File
    , dataUri : String
    }


type File
    = Image FileInfo
    | Unknown String FileInfo


type alias FileInfo =
    { name : String
    , size : Int
    , dataUri : String
    }


init : ( Model, Cmd Msg )
init =
    ( { dragging = False
      , files = []
      , dataUri = ""
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Clear
    | Drop Json.Decode.Value
    | Dragging Bool
    | DataUri (Result String File)
    | DisplayDataUri String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clear ->
            init

        Drop files ->
            ( { model | dragging = False }, drop files )

        Dragging value ->
            ( { model | dragging = value }, Cmd.none )

        DataUri (Ok file) ->
            ( { model
                | files = file :: model.files
              }
            , Cmd.none
            )

        DataUri (Err err) ->
            --( model, Cmd.none )
            Debug.crash err

        DisplayDataUri base64 ->
            ( { model | dataUri = base64 }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "File-drop example in elm" ]
        , Html.div
            [ Html.Attributes.class "wrapper" ]
            [ dropArea model.dragging
            , if List.isEmpty model.files then
                Html.text ""
              else
                files model.files
            , if String.isEmpty model.dataUri then
                text ""
              else
                Html.div
                    [ Html.Attributes.class "modal" ]
                    [ Html.button
                        [ Html.Events.onClick <| DisplayDataUri "" ]
                        [ Html.text "Close" ]
                    , Html.textarea
                        [ Html.Attributes.class "content" ]
                        [ text model.dataUri ]
                    ]
            ]
        ]


dropArea : Bool -> Html Msg
dropArea dragging =
    Html.div
        [ Html.Attributes.class "drop-area"
        , Html.Attributes.classList [ ( "drag-over", dragging ) ]
        , on "dragover" <|
            Json.Decode.succeed (Dragging True)
        , on "dragleave" <|
            Json.Decode.succeed (Dragging False)
        , on "drop" <|
            Json.Decode.map Drop dropEventDecoder
        ]
        [ Html.input
            [ Html.Attributes.type_ "file"
            , Html.Attributes.id "drop-file"
            , Html.Events.on "change" <|
                Json.Decode.map Drop dropEventDecoder
            ]
            []
        , Html.label
            [ Html.Attributes.for "drop-file" ]
            [ text "Drop files or click here" ]
        ]


on : String -> Json.Decode.Decoder msg -> Html.Attribute msg
on str =
    Html.Events.onWithOptions str
        { stopPropagation = False, preventDefault = True }


files : List File -> Html Msg
files files =
    Html.div
        [ Html.Attributes.class "files" ]
        [ Html.div
            [ Html.Attributes.class "header" ]
            [ Html.h2 [] [ Html.text "Files" ]
            , Html.button
                [ Html.Events.onClick Clear ]
                [ Html.text "clear" ]
            ]
        , Html.ul [] <|
            List.indexedMap viewFile files
        ]


viewFile : Int -> File -> Html Msg
viewFile index file =
    case file of
        Image { name, size, dataUri } ->
            Html.li []
                [ Html.div [ Html.Attributes.class "preview" ]
                    [ Html.img [ Html.Attributes.src dataUri ] [] ]
                , Html.div [ Html.Attributes.class "info" ]
                    [ Html.p []
                        [ Html.text <| name ++ " (" ++ toString size ++ "Byte)"
                        ]
                    , data dataUri
                    ]
                ]

        Unknown type_ { name, size, dataUri } ->
            Html.li
                [ Html.Attributes.class "unknown" ]
                [ Html.p []
                    [ Html.text <|
                        String.join " "
                            [ name
                            , "(" ++ toString size ++ "Byte)"
                            , type_
                            ]
                    ]
                , data dataUri
                ]


data : String -> Html Msg
data dataUri =
    if String.length dataUri < 12000 then
        Html.textarea [] [ text dataUri ]
    else
        Html.button
            [ Html.Events.onClick <| DisplayDataUri dataUri ]
            [ Html.text "Display dataURI" ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dataUri (DataUri << dataUriDecoder)
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }



----- DECODER ----


dropEventDecoder : Json.Decode.Decoder Json.Decode.Value
dropEventDecoder =
    Json.Decode.at [ "dataTransfer", "files" ] Json.Decode.value


dataUriDecoder : Json.Decode.Value -> Result String File
dataUriDecoder =
    Json.Decode.decodeValue <|
        Json.Decode.map4 toFile
            (Json.Decode.field "type" Json.Decode.string)
            (Json.Decode.field "name" Json.Decode.string)
            (Json.Decode.field "size" Json.Decode.int)
            (Json.Decode.field "dataUri" Json.Decode.string)


toFile : String -> String -> Int -> String -> File
toFile type_ name size dataUri =
    if String.startsWith "image" type_ then
        Image <| FileInfo name size dataUri
    else
        Unknown type_ <| FileInfo name size dataUri



---- PORTS ----


port drop : Json.Decode.Value -> Cmd msg


port dataUri : (Json.Decode.Value -> msg) -> Sub msg
