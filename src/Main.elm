module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)



-- MODEL


type alias Model =
    { x : Int
    , y : Int
    , direction : Direction
    }


type Direction
    = North
    | East
    | South
    | West


init : Model
init =
    { x = 0, y = 0, direction = North }



-- UPDATE


type Msg
    = MoveForward
    | RotateLeft
    | RotateRight


update : Msg -> Model -> Model
update msg model =
    case msg of
        MoveForward ->
            moveForward model

        RotateLeft ->
            { model | direction = rotateLeft model.direction }

        RotateRight ->
            { model | direction = rotateRight model.direction }


moveForward : Model -> Model
moveForward model =
    case model.direction of
        North ->
            if model.y > 0 then
                { model | y = model.y - 1 }

            else
                model

        South ->
            if model.y < 4 then
                { model | y = model.y + 1 }

            else
                model

        East ->
            if model.x < 4 then
                { model | x = model.x + 1 }

            else
                model

        West ->
            if model.x > 0 then
                { model | x = model.x - 1 }

            else
                model


rotateLeft : Direction -> Direction
rotateLeft direction =
    case direction of
        North ->
            West

        West ->
            South

        South ->
            East

        East ->
            North


rotateRight : Direction -> Direction
rotateRight direction =
    case direction of
        North ->
            East

        East ->
            South

        South ->
            West

        West ->
            North



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container", style "text-align" "center" ]
        [ div [] [ text ("Position: (" ++ String.fromInt model.x ++ ", " ++ String.fromInt model.y ++ ")") ]
        , div [] [ text ("Facing: " ++ directionToString model.direction) ]
        , gridView model
        , div [ class "controls" ]
            [ button [ onClick MoveForward, class "button" ] [ text "Move Forward" ]
            , button [ onClick RotateLeft, class "button" ] [ text "Rotate Left" ]
            , button [ onClick RotateRight, class "button" ] [ text "Rotate Right" ]
            ]
        ]



-- GRID VIEW


gridView : Model -> Html Msg
gridView model =
    div [ class "grid" ]
        (List.concatMap (gridRow model) (List.range 0 4))


gridRow : Model -> Int -> List (Html Msg)
gridRow model row =
    List.map (gridCell model row) (List.range 0 4)


gridCell : Model -> Int -> Int -> Html Msg
gridCell model row col =
    let
        isRobotHere =
            row == model.y && col == model.x

        cellStyle =
            if isRobotHere then
                [ style "background-color" "#333", style "color" "white" ]

            else
                [ style "background-color" "#fff", style "color" "#000" ]
    in
    div ([ class "grid-cell", style "border" "1px solid #ccc", style "width" "50px", style "height" "50px", style "display" "inline-block", style "text-align" "center", style "vertical-align" "middle", style "line-height" "50px" ] ++ cellStyle)
        [ if isRobotHere then
            text (directionToString model.direction)

          else
            text ""
        ]



-- HELPER FUNCTIONS


directionToString : Direction -> String
directionToString direction =
    case direction of
        North ->
            "N"

        East ->
            "E"

        South ->
            "S"

        West ->
            "W"



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }
