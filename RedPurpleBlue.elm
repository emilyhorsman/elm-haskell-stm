module Main exposing (..)

import GraphicSVG exposing (..)


-- Definitions


type Message
    = Tick Float GetKeyState
    | Click


type MyColour
    = Red
    | Purple
    | Blue


type alias Model =
    { time : Float
    , colour : MyColour
    , score : Int
    }


init : Model
init =
    { time = 0
    , colour =
        Red
    , score =
        0
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


view model =
    collage 500 500 (myShapes model)



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


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        Tick t _ ->
            ( { model | time = t }, Cmd.none )

        Click ->
            ( { model
                | colour = change model.colour
                , score = model.score + 1
              }
            , Cmd.none
            )



-- Program


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.none


main =
    cmdApp Tick
        { init = ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
