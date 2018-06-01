module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Keyboard
import Time exposing (..)
import Element exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Text
import Char
import Random exposing (..)
import Tuple


segmentDim =
    15


foodRadius =
    7.5


( width, height ) =
    ( 600, 600 )


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- define the Model


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Position =
    ( Float, Float )


pos : Float -> Float -> Position
pos x y =
    ( x, y )


type alias Snake =
    { head : Position
    , tail : List Position
    , direction : Direction
    }


type alias Food =
    Maybe Position


type alias Score =
    Int


type Model
    = NotStarted
    | Started Snake Food Score



-- define Msg that can trigger updates to Model


type Msg
    = Tick Time
    | KeyPress Keyboard.KeyCode
    | Spawn ( Float, Position )


randPos =
    Random.pair (Random.float 0 1) (Random.float 0 1)


generator : Random.Generator ( Float, Position )
generator =
    Random.pair (Random.float 0 1) randPos



-- define initial State of the App


initSnake : Snake
initSnake =
    let
        head =
            ( 0, 0 )

        tail =
            List.range 1 8
                |> List.map (\n -> pos (toFloat (-n * segmentDim)) 0)
    in
        { head = head, tail = tail, direction = Right }


init : ( Model, Cmd Msg )
init =
    ( NotStarted, Cmd.none )



-- define subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotStarted ->
            Keyboard.presses KeyPress

        Started _ _ _ ->
            Sub.batch
                [ Keyboard.presses KeyPress
                , Time.every (Time.inMilliseconds 50) Tick
                ]



--rendering!


view : Model -> Html Msg
view model =
    let
        bg =
            rect (toFloat width) (toFloat height)
                |> filled black

        content =
            case model of
                NotStarted ->
                    [ txt "press SPACE to start" ]

                Started snake food score ->
                    let
                        head =
                            rect segmentDim segmentDim
                                |> filled white
                                |> move snake.head

                        tail =
                            snake.tail
                                |> List.map
                                    (\pos ->
                                        rect segmentDim segmentDim
                                            |> filled yellow
                                            |> move pos
                                    )

                        scoreLbl =
                            txt (toString score)
                    in
                        case food of
                            Nothing ->
                                scoreLbl :: head :: tail

                            Just pos ->
                                (circle foodRadius
                                    |> filled red
                                    |> move pos
                                )
                                    :: scoreLbl
                                    :: head
                                    :: tail
    in
        collage width height (bg :: content)
            |> Element.toHtml



-- state transitions (the fun stuff)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        NotStarted ->
            case msg of
                KeyPress 32 ->
                    ( Started initSnake Nothing 0, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Started snake food score ->
            case msg of
                KeyPress keycode ->
                    let
                        newDir =
                            getNewDirection keycode snake.direction

                        newSnake =
                            { snake | direction = newDir }
                    in
                        ( Started newSnake food score, Cmd.none )

                Spawn ( chance, ( randX, randY ) ) ->
                    if chance <= 0.1 then
                        let
                            newFood =
                                spawnFood randX randY
                        in
                            ( Started snake newFood score, Cmd.none )
                    else
                        ( model, Cmd.none )

                Tick _ ->
                    let
                        newHead =
                            getNewSegment snake.head snake.direction

                        ateFood =
                            case food of
                                Just pos ->
                                    isOverlap newHead pos

                                Nothing ->
                                    False

                        newTail =
                            if ateFood then
                                snake.head :: snake.tail
                            else
                                snake.head :: (List.take (List.length snake.tail - 1) snake.tail)

                        newSnake =
                            { snake | head = newHead, tail = newTail }

                        ( newFood, newScore ) =
                            if ateFood then
                                ( Nothing, score + 1 )
                            else
                                ( food, score )

                        gameOver =
                            isGameOver newHead newTail
                    in
                        if gameOver then
                            ( NotStarted, Cmd.none )
                        else if newFood == Nothing then
                            ( Started newSnake newFood newScore, Random.generate Spawn generator )
                        else
                            ( Started newSnake newFood newScore, Cmd.none )


txt : String -> Form
txt msg =
    msg
        |> Text.fromString
        |> Text.color white
        |> Text.monospace
        |> Element.centered
        |> Collage.toForm


getNewDirection : Char.KeyCode -> Direction -> Direction
getNewDirection keyCode currentDir =
    let
        ( changeableDirs, newDir ) =
            case Char.fromCode keyCode of
                'a' ->
                    ( [ Up, Down ], Left )

                'w' ->
                    ( [ Left, Right ], Up )

                'd' ->
                    ( [ Up, Down ], Right )

                's' ->
                    ( [ Left, Right ], Down )

                _ ->
                    ( [], currentDir )
    in
        if List.any ((==) currentDir) changeableDirs then
            newDir
        else
            currentDir


getNewSegment : Position -> Direction -> Position
getNewSegment ( x, y ) direction =
    case direction of
        Up ->
            pos x (y + segmentDim)

        Down ->
            pos x (y - segmentDim)

        Left ->
            pos (x - segmentDim) y

        Right ->
            pos (x + segmentDim) y


isGameOver : Position -> List Position -> Bool
isGameOver newHead newTail =
    -- the snake ate itself
    List.any ((==) newHead) newTail
        --hit right
        || Tuple.first newHead
        > (width / 2)
        --hit bottom
        || Tuple.second newHead
        > (height / 2)
        --hit left
        || Tuple.first newHead
        < (-width / 2)
        --hit top
        || Tuple.second newHead
        < (-height / 2)


spawnFood : Float -> Float -> Food
spawnFood randX randY =
    let
        x =
            randX * width - width / 2

        y =
            randY * height - height / 2
    in
        pos x y |> Just


isOverlap : Position -> Position -> Bool
isOverlap ( snakeX, snakeY ) ( foodX, foodY ) =
    let
        ( deltaX, deltaY ) =
            ( foodX - snakeX, foodY - snakeY )

        distance =
            sqrt (deltaX ^ 2 + deltaY ^ 2)
    in
        distance <= (foodRadius * 2)
