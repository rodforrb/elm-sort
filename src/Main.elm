module Main exposing (main)

import Array exposing (..)
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List

main =
    gameApp Tick
        { model = init
        , title = "Elm Sort"
        , view = view
        , update = update
        }

init = 
    { size = 5
    , minSize = 5
    , maxSize = 40
    , notify = NotifyTap
    , currentButton = None
    }


view model = 
    collage 600 400 <|
        [ graphPaperCustom 10 1 (rgb 255 137 5) |> makeTransparent 0.25 -- axes and selected coordinate ticks
        , group
            [ rect 1000 0.5 |> filled brown
            , rect 0.5 1000 |> filled brown
            ]
            |> move ( -120, 70 )

        --, moveGraphicsX |> move ( 180, 220 )
        --, moveGraphicsY |> move ( 60, 50 )
        , group
            [ rect 10 200 |> filled blue |> move (50, 50)
        --, triangle 10 |> filled (rgb 100 0 100) |> notifyTap SizePlus |> move ( 0, -5 ) |> notifyMouseDown (ButtonDown SizePlus) |> notifyMouseUp (ButtonDown None)
        --, triangle 10 |> filled (rgb 100 0 100) |> rotate (degrees 180) |> notifyTap SizeMinus |> move ( -110, -5 ) |> notifyMouseDown (ButtonDown SizeMinus) |> notifyMouseUp (ButtonDown None)
            ]
        , group
            [ text ("More" ++ String.fromFloat model.uDilation) |> fixedwidth |> size 10 |> filled black |> move (-180, -10)
            , text ("Fewer" ++ String.fromFloat model.uScale) |> fixedwidth |> size 10 |> filled black |> move (-180, -50)
            --, text (String.fromInt model.size) |> fixedwidth |> size 10 |> filled black |> move (-210, -90)
            ]
        ]



type Msg m
    = Tick Float GetKeyState
    | SizeIncr
    | SizeDecr
    | MouseUp
    | ButtonDown ButtonDir
    | Notif Notifications

type Notifications
    = NotifyTap
    | NotifyTapAt
    | NotifyEnter
    | NotifyEnterAt
    | NotifyLeave
    | NotifyLeaveAt
    | NotifyMouseMoveAt
    | NotifyMouseDown
    | NotifyMouseDownAt
    | NotifyMouseUp
    | NotifyMouseUpAt
    | NotifyTouchStart
    | NotifyTouchStartAt
    | NotifyTouchEnd
    | NotifyTouchEndAt
    | NotifyTouchMoveAt

type ButtonDir
    = SizeMinus
    | SizePlus
    | Next
    | None
 
update msg model =
    case msg of
        Tick t _ -> 
            let
                -- we need to know how much time past since the last clock tick
                timeChange = t - model.time
            in
                { model
                    | time = t
                    , size = model.size
                }

        SizeDecr ->
            { model
                | size =
                    if model.size > model.minSize then
                        model.size - 1
                
                    else
                        model.size
            }

        SizeIncr ->
            { model
                | size =
                    if model.size < model.maxSize then
                        model.size + 1
                
                    else
                        model.size
            
            }
    
        ButtonDown dir ->
            { model | currentButton = dir }

        MouseUp ->
            { model | currentButton = None }
