module Main exposing (..)

import GraphicSVG exposing (..)
import WebSocket
import Dict


-- Definitions


type Message
    = Tick Float GetKeyState
    | Click
    | WSReceiveMessage String


type MyColour
    = Red
    | Purple
    | Blue


type alias Model =
    { time : Float
    , colour : MyColour
    , score : Int
    , username : Maybe String
    , scores : Dict.Dict String Int
    }


init : Model
init =
    { time = 0
    , colour =
        Red
    , score =
        0
    , username = Nothing
    , scores = Dict.empty
    }



-- View


myShapes model =
    [ text ("Hello " ++ toString model.score)
        |> filled purple
    , roundedRect 20 30 5
        |> filled (myColour model.colour)
        |> move
            ( 100 * sin model.time
            , 50 * sin (2 * model.time)
            )
        |> makeTransparent 0.5
        |> notifyTap Click
    ]


myColour : MyColour -> Color
myColour c =
    case c of
        Red ->
            rgb 255 0 0

        Purple ->
            rgb 200 0 200

        Blue ->
            rgb 0 0 255


getScoreLabel : String -> String -> Int -> String
getScoreLabel currentUser user score =
    if currentUser == user then
        user ++ " (you!): " ++ toString score
    else
        user ++ ": " ++ toString score


printHighScore : String -> Int -> ( String, Int ) -> Shape a
printHighScore currentUser index ( user, score ) =
    getScoreLabel currentUser user score
        |> text
        |> size 14
        |> filled black
        |> move ( 0, -20 * (toFloat index) )


viewHighScores : Model -> List (Shape a)
viewHighScores model =
    case ( model.username, model.scores ) of
        ( Just username, scores ) ->
            scores
                |> Dict.toList
                |> List.indexedMap (printHighScore username)

        _ ->
            [ text "No scores yet." |> size 14 |> filled black
            ]


view model =
    collage 500 500 <|
        myShapes model
            ++ [ viewHighScores model |> group |> move ( 0, -50 )
               ]



-- Update


change : MyColour -> MyColour
change old =
    case old of
        Red ->
            Purple

        Purple ->
            Blue

        Blue ->
            Red


assignScore : String -> Result String Int -> Model -> Model
assignScore username resultScore model =
    case ( username, resultScore, model ) of
        ( username, Ok score, model ) ->
            { model
                | scores = Dict.insert username score model.scores
            }

        _ ->
            model


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case ( msg, model.username ) of
        ( Tick t _, _ ) ->
            ( { model | time = t }, Cmd.none )

        ( Click, Just username ) ->
            ( { model
                | colour = change model.colour
                , score = model.score + 1
              }
            , WebSocket.send "ws://localhost:8080/" username
            )

        ( WSReceiveMessage input, _ ) ->
            case String.split " " input of
                "AssignUsername" :: username :: [] ->
                    ( { model
                        | username = Just username
                      }
                    , Cmd.none
                    )

                "AssignScore" :: username :: score :: [] ->
                    ( assignScore username (String.toInt score) model
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- Program


subscriptions : Model -> Sub Message
subscriptions model =
    WebSocket.listen "ws://localhost:8080/" WSReceiveMessage


main =
    cmdApp Tick
        { init = ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
