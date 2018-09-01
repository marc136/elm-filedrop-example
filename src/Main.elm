port module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Html.Events
import Json.Decode as Json



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
    | Drop Json.Value
    | Dragging Bool
    | DataUri (Result Json.Error File)
    | DisplayDataUri String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clear ->
            init

        Drop files_ ->
            ( { model | dragging = False }, drop files_ )

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
            Debug.todo (Json.errorToString err)

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
            Json.succeed (Dragging True)
        , on "dragleave" <|
            Json.succeed (Dragging False)
        , on "drop" <|
            Json.map Drop dropEventDecoder
        ]
        [ Html.input
            [ Html.Attributes.type_ "file"
            , Html.Attributes.id "drop-file"
            , Html.Events.on "change" <|
                Json.map Drop dropEventDecoder
            ]
            []
        , Html.label
            [ Html.Attributes.for "drop-file" ]
            [ text "Drop files or click here" ]
        ]


on : String -> Json.Decoder msg -> Html.Attribute msg
on str decoder =
    Html.Events.custom str (Json.map custom decoder)


custom : msg -> { message : msg, stopPropagation : Bool, preventDefault : Bool }
custom msg =
    { message = msg
    , stopPropagation = False
    , preventDefault = True
    }


files : List File -> Html Msg
files files_ =
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
            List.indexedMap viewFile files_
        ]


viewFile : Int -> File -> Html Msg
viewFile index file =
    case file of
        Image { name, size, dataUri } ->
            Html.li []
                [ Html.div [ Html.Attributes.class "preview" ]
                    [ Html.img [ Html.Attributes.src dataUri ] [] ]
                , Html.div [ Html.Attributes.class "info" ]
                    [ Html.p [] [ Html.text <| name ++ " " ++ byte size ]
                    , data dataUri
                    ]
                ]

        Unknown type_ { name, size, dataUri } ->
            Html.li
                [ Html.Attributes.class "unknown" ]
                [ Html.p []
                    [ Html.text <|
                        String.join " " [ name, byte size, type_ ]
                    ]
                , data dataUri
                ]


byte : Int -> String
byte size =
    "(" ++ String.fromInt size ++ "Byte)"


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
        [ getDataUri (DataUri << dataUriDecoder)
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }



----- DECODER ----


dropEventDecoder : Json.Decoder Json.Value
dropEventDecoder =
    Json.at [ "dataTransfer", "files" ] Json.value


dataUriDecoder : Json.Value -> Result Json.Error File
dataUriDecoder =
    Json.decodeValue <|
        Json.map4 toFile
            (Json.field "type" Json.string)
            (Json.field "name" Json.string)
            (Json.field "size" Json.int)
            (Json.field "dataUri" Json.string)


toFile : String -> String -> Int -> String -> File
toFile type_ name size uri =
    if String.startsWith "image" type_ then
        Image <| FileInfo name size uri

    else
        Unknown type_ <| FileInfo name size uri



---- PORTS ----


port drop : Json.Value -> Cmd msg


port getDataUri : (Json.Value -> msg) -> Sub msg
