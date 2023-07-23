{-
Digital Clock widget by using GraphicSVG in ELM
Created By : Shahram Bashokian
-}

module DigitalClock exposing (..)

import Browser
import Html exposing(Html,div)
import Html.Attributes exposing (style)
import Task
import Time
import GraphicSVG exposing (Shape)
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import GraphicSVG.App exposing (..)
import GraphicSVG.Widget as Widget
import Svg exposing (svg)


-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
  , clock : Widget.Model
  }


init : () -> (Model, Cmd Msg)
init _ =
    let
        (wstate0, wcmd0) = Widget.init 200 50 "clock"
    in
        ( Model Time.utc (Time.millisToPosix 0) wstate0
        , Task.perform AdjustTimeZone Time.here
        )



-- UPDATE

type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model | time = newTime }
      , Cmd.none
      )

    AdjustTimeZone newZone ->
      ( { model | zone = newZone }
      , Cmd.none
      )



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick



-- VIEW

view : Model -> Html Msg
view model =
  let
    hour   =  (Time.toHour   model.zone model.time)
    minute =  (Time.toMinute model.zone model.time)
    second =  (Time.toSecond model.zone model.time)

    hourTxt   = String.fromInt (Time.toHour   model.zone model.time)
    minuteTxt = String.fromInt (Time.toMinute model.zone model.time)
    secondTxt = String.fromInt (Time.toSecond model.zone model.time)

    scaleSize =0.7

    hL = hour // 10
    hR = remainderBy 10 hour
    mL = minute // 10
    mR = remainderBy 10 minute

    sL = second // 10

    sR = remainderBy 10 second

    hourLeft = numberToShape hL scaleSize

    hourRight = numberToShape hR scaleSize

    minuteLeft = numberToShape mL scaleSize
    minuteRight = numberToShape mR scaleSize

    secondLeft = numberToShape sL scaleSize
    secondRight = numberToShape sR scaleSize

    in
      div []
        [ div [style "width" "100%"]
            [
                Widget.view model.clock
                [
                    --text (hourTxt ++ ":" ++ minuteTxt ++ ":" ++ secondTxt)
                    --    |> fixedwidth
                    --    |> size 10
                    --    |> filled black
                    --    |> move (-5,11),
                      hourLeft |> move (-17.5 * scaleSize, 0)
                    , hourRight |> move (-7.5 * scaleSize, 0)
                    , dots scaleSize |> move (0, -6.5 * scaleSize)
                    , minuteLeft |> move (7.5 * scaleSize, 0)
                    , minuteRight |> move (17.5 * scaleSize, 0)
                    , dots scaleSize |> move (25 * scaleSize, -6.5 * scaleSize)
                    , secondLeft |> move (32.5 * scaleSize, 0)
                    , secondRight |> move (42.5 * scaleSize, 0)
                ]
            ]
        ]

-- Shapes

numberToShape : Int -> Float -> Shape Msg
numberToShape n scaleSize =
    let scaledShape = scale scaleSize
    in
    case n of
        0 ->
            zero scaleSize
        1 ->
            one scaleSize 
        2 ->
            two scaleSize 
        3 ->
            three scaleSize 
        4 ->
            four scaleSize
        5 ->
            five scaleSize 
        6 ->
            six scaleSize 
        7 ->
            seven scaleSize 
        8 ->
            eight scaleSize 
        9 ->
            nine scaleSize 
        _ ->
            zero scaleSize 


line : Float -> Shape Msg
line scaleSize=
    group
        [ rect (5*scaleSize) (2*scaleSize) |> filled (rgb 235 52 88)
        , polygon [(0, 0), (0, -2*scaleSize), (1*scaleSize, -1*scaleSize)] |> filled (rgb 235 52 88) |> move (2.5*scaleSize, 1*scaleSize)
        , polygon [(0, 0), (0, -2*scaleSize), (1*scaleSize, -1*scaleSize)] |> filled (rgb 235 52 88) |> move (2.5*scaleSize, 1*scaleSize) |> mirrorX
        ]
      
lineOff :Float -> Shape Msg
lineOff scaleSize = group
      [
        rect (5*scaleSize) (2*scaleSize)
          |> filled (rgb 245 233 237)
        , polygon [(0,0),(0,-2*scaleSize),(1*scaleSize,-1*scaleSize)]
          |> filled (rgb 245 233 237)
          |> move(2.5*scaleSize,1*scaleSize)
        , polygon [(0,0),(0,-2*scaleSize),(1*scaleSize,-1*scaleSize)]
          |> filled (rgb 245 233 237)
          |> move(2.5*scaleSize,1*scaleSize)
          |> mirrorX
      ]      
      
dots :Float -> Shape Msg
dots scaleSize = group
      [
        square (2 * scaleSize)
          |> filled (rgb 235 52 88)
        , square (2 * scaleSize)
          |> filled (rgb 235 52 88)
          |> move(0,-4 * scaleSize)
      ] 
      
base : Float -> Shape Msg      
base scaleSize= group
    [
    -- top top
      lineOff scaleSize
    -- top left
    , lineOff scaleSize
        |> rotate(degrees 90)
        |> move(-3.6, -3.85)
    -- top right
    , lineOff scaleSize
        |> rotate(degrees 90)
        |> move(3.6, -3.85)  
    -- top middle        
    , lineOff scaleSize
        |> move(0, -7.7)
    -- bottom right        
    , lineOff scaleSize
        |> rotate(degrees 90)
        |> move(3.6, -11.55)
    -- bottom left        
    , lineOff scaleSize
        |> rotate(degrees 90)
        |> move(-3.6, -11.55) 
    -- bottom        
    , lineOff scaleSize
        |> move(0, -15.4)
   ]  



zero : Float -> Shape Msg
zero scaleSize =
    let moveX = -3.6 * scaleSize
        moveY = -3.85 * scaleSize
    in
    group
        [ line scaleSize-- top top
        , line scaleSize-- top left
            |> rotate (degrees 90)
            |> move (moveX, moveY)
        , line scaleSize-- top right
            |> rotate (degrees 90)
            |> move (3.6 * scaleSize, moveY)
        , lineOff scaleSize-- top middle
            |> move (0, -7.7 * scaleSize)
        , line scaleSize-- bottom right
            |> rotate (degrees 90)
            |> move (3.6 * scaleSize, -11.55 * scaleSize)
        , line scaleSize-- bottom left
            |> rotate (degrees 90)
            |> move (moveX, -11.55 * scaleSize)
        , line scaleSize-- bottom
            |> move (0, -15.4 * scaleSize)
        ]
    
     
one : Float -> Shape Msg
one scaleSize =
    let moveX = -3.6 * scaleSize
    in
    group
        [ lineOff scaleSize-- top top
        , lineOff scaleSize-- top left
            |> rotate (degrees 90)
            |> move (moveX, -3.85 * scaleSize)
        , line scaleSize-- top right
            |> rotate (degrees 90)
            |> move (3.6 * scaleSize, -3.85 * scaleSize)
        , lineOff scaleSize-- top middle
            |> move (0, -7.7 * scaleSize)
        , line scaleSize-- bottom right
            |> rotate (degrees 90)
            |> move (3.6 * scaleSize, -11.55 * scaleSize)
        , lineOff scaleSize-- bottom left
            |> rotate (degrees 90)
            |> move (moveX, -11.55 * scaleSize)
        , lineOff scaleSize-- bottom
            |> move (0, -15.4 * scaleSize)
        ]
    
two : Float -> Shape Msg
two scaleSize =
    let moveX = -3.6 * scaleSize
        moveY = -3.85 * scaleSize
    in
    group
        [ line scaleSize-- top top
        , lineOff scaleSize-- top left
            |> rotate (degrees 90)
            |> move (moveX, moveY)
        , line scaleSize-- top right
            |> rotate (degrees 90)
            |> move (3.6 * scaleSize, moveY)
        , line scaleSize-- top middle
            |> move (0, -7.7 * scaleSize)
        , lineOff scaleSize-- bottom right
            |> rotate (degrees 90)
            |> move (3.6 * scaleSize, -11.55 * scaleSize)
        , line scaleSize-- bottom left
            |> rotate (degrees 90)
            |> move (moveX, -11.55 * scaleSize)
        , line scaleSize-- bottom
            |> move (0, -15.4 * scaleSize)
        ]

   
three : Float -> Shape Msg
three scaleSize =
    let moveX = -3.6 * scaleSize
        moveY = -3.85 * scaleSize
    in
    group
        [ line scaleSize-- top top
        , lineOff scaleSize-- top left
            |> rotate (degrees 90)
            |> move (moveX, moveY)
        , line scaleSize-- top right
            |> rotate (degrees 90)
            |> move (3.6 * scaleSize, moveY)
        , line scaleSize-- top middle
            |> move (0, -7.7 * scaleSize)
        , line scaleSize-- bottom right
            |> rotate (degrees 90)
            |> move (3.6 * scaleSize, -11.55 * scaleSize)
        , lineOff scaleSize-- bottom left
            |> rotate (degrees 90)
            |> move (moveX, -11.55 * scaleSize)
        , line scaleSize-- bottom
            |> move (0, -15.4 * scaleSize)
        ]
   
   
four : Float -> Shape Msg
four scaleSize =
    let moveX = -3.6 * scaleSize
        moveY = -3.85 * scaleSize
    in
    group
        [ lineOff scaleSize -- top top
        , line scaleSize-- top left
            |> rotate (degrees 90)
            |> move (moveX, moveY)
        , line scaleSize-- top right
            |> rotate (degrees 90)
            |> move (3.6 * scaleSize, moveY)
        , line scaleSize-- top middle
            |> move (0, -7.7 * scaleSize)
        , line scaleSize-- bottom right
            |> rotate (degrees 90)
            |> move (3.6 * scaleSize, -11.55 * scaleSize)
        , lineOff scaleSize-- bottom left
            |> rotate (degrees 90)
            |> move (moveX, -11.55 * scaleSize)
        , lineOff scaleSize-- bottom
            |> move (0, -15.4 * scaleSize)
        ]
  
five : Float -> Shape Msg
five scaleSize =
    let moveX = 3.6 * scaleSize
        moveY = -3.85 * scaleSize
    in
    group
        [ line scaleSize-- top top
        , line scaleSize-- top left
            |> rotate (degrees 90)
            |> move (-moveX, moveY)
        , lineOff scaleSize-- top right
            |> rotate (degrees 90)
            |> move (moveX, moveY)
        , line scaleSize-- top middle
            |> move (0, -7.7 * scaleSize)
        , line scaleSize-- bottom right
            |> rotate (degrees 90)
            |> move (moveX, -11.55 * scaleSize)
        , lineOff scaleSize-- bottom left
            |> rotate (degrees 90)
            |> move (-moveX, -11.55 * scaleSize)
        , line scaleSize-- bottom
            |> move (0, -15.4 * scaleSize)
        ]
    
    
six : Float -> Shape Msg
six scaleSize =
    let moveX = 3.6 * scaleSize
        moveY = -3.85 * scaleSize
    in
    group
        [ line scaleSize-- top top
        , line scaleSize-- top left
            |> rotate (degrees 90)
            |> move (-moveX, moveY)
        , lineOff scaleSize-- top right
            |> rotate (degrees 90)
            |> move (moveX, moveY)
        , line scaleSize-- top middle
            |> move (0, -7.7 * scaleSize)
        , line scaleSize-- bottom right
            |> rotate (degrees 90)
            |> move (moveX, -11.55 * scaleSize)
        , line scaleSize-- bottom left
            |> rotate (degrees 90)
            |> move (-moveX, -11.55 * scaleSize)
        , line scaleSize-- bottom
            |> move (0, -15.4 * scaleSize)
        ]
     
        
seven : Float -> Shape Msg
seven scaleSize =
    let moveX = 3.6 * scaleSize
        moveY = -3.85 * scaleSize
    in
    group
        [ line scaleSize-- top top
        , lineOff scaleSize-- top left
            |> rotate (degrees 90)
            |> move (-moveX, moveY)
        , line scaleSize-- top right
            |> rotate (degrees 90)
            |> move (moveX, moveY)
        , lineOff scaleSize -- top middle
            |> move (0, -7.7 * scaleSize)
        , line scaleSize-- bottom right
            |> rotate (degrees 90)
            |> move (moveX, -11.55 * scaleSize)
        , lineOff scaleSize-- bottom left
            |> rotate (degrees 90)
            |> move (-moveX, -11.55 * scaleSize)
        , lineOff scaleSize -- bottom
            |> move (0, -15.4 * scaleSize)
        ]


eight : Float -> Shape Msg
eight scaleSize =
    let moveX = -3.6 * scaleSize
        moveY = -3.85 * scaleSize
    in
    group
        [ line scaleSize-- top top
        , line scaleSize-- top left
            |> rotate (degrees 90)
            |> move (moveX, moveY)
        , line scaleSize-- top right
            |> rotate (degrees 90)
            |> move (3.6 * scaleSize, moveY)
        , line scaleSize-- top middle
            |> move (0, -7.7 * scaleSize)
        , line scaleSize-- bottom right
            |> rotate (degrees 90)
            |> move (3.6 * scaleSize, -11.55 * scaleSize)
        , line scaleSize-- bottom left
            |> rotate (degrees 90)
            |> move (moveX, -11.55 * scaleSize)
        , line scaleSize-- bottom
            |> move (0, -15.4 * scaleSize)
        ]
   
nine : Float -> Shape Msg
nine scaleSize =
    let moveX = -3.6 * scaleSize
        moveY = -3.85 * scaleSize
    in
    group
        [ line scaleSize -- top top
        , line scaleSize-- top left
            |> rotate (degrees 90)
            |> move (moveX, moveY)
        , line scaleSize-- top right
            |> rotate (degrees 90)
            |> move (3.6 * scaleSize, moveY)
        , line scaleSize-- top middle
            |> move (0, -7.7 * scaleSize)
        , line scaleSize-- bottom right
            |> rotate (degrees 90)
            |> move (3.6 * scaleSize, -11.55 * scaleSize)
        , lineOff scaleSize-- bottom left
            |> rotate (degrees 90)
            |> move (moveX, -11.55 * scaleSize)
        , line scaleSize-- bottom
            |> move (0, -15.4 * scaleSize)
        ]

        
    