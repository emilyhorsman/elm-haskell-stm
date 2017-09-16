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


type alias Points =
    Int


type alias Timestamp =
    Int


type Score
    = Score Points Timestamp


type alias Model =
    { time : Float
    , colour : MyColour
    , score : Int
    , username : Maybe String
    , scores : Dict.Dict String Score
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


printHighScore : String -> Int -> ( String, Score ) -> Shape a
printHighScore currentUser index ( user, Score score _ ) =
    getScoreLabel currentUser user score
        |> text
        |> size 14
        |> filled black
        |> move ( 0, -20 * (toFloat index) )


descendingCompareScore : (Score -> Int) -> ( String, Score ) -> ( String, Score ) -> Order
descendingCompareScore lens ( _, a ) ( _, b ) =
    case compare (lens a) (lens b) of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


viewHighScores : Model -> List (Shape a)
viewHighScores model =
    case ( model.username, model.scores ) of
        ( Just username, scores ) ->
            -- Take the thirty most recent scores and then show the highest
            -- first! This way people playing at the same time can compete
            -- against each other, instead of a really high but old score.
            -- In the latter scenario, the chances of seeing your score on the
            -- board would become low.
            scores
                |> Dict.toList
                |> List.sortWith (descendingCompareScore (\(Score _ time) -> time))
                |> List.take 30
                |> List.sortWith (descendingCompareScore (\(Score points _) -> points))
                |> List.take 10
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


assignScore : String -> Result String Points -> Result String Timestamp -> Model -> Model
assignScore username resultScore resultTime model =
    case ( username, resultScore, resultTime, model ) of
        ( username, Ok score, Ok time, model ) ->
            { model
                | scores = Dict.insert username (Score score time) model.scores
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

                "AssignScore" :: username :: score :: time :: [] ->
                    ( assignScore username (String.toInt score) (String.toInt time) model
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
