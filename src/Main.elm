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
    { size = 20
    , minSize = 5
    , maxSize = 40
    , time = 0
    , currentButton = None
    , adjLeft = -180
    , adjRight = 160
    , notify = NotifyTap
    , array = [3, 8, 1, 5, 2, 0, 4, 7, 2, 5, 3, 8, 6, 1, 0, 6, 3, 8, 5, 3] -- random unsorted list
    }


view model = 
    collage 600 400 <|
        [ graphPaperCustom 10 1 (rgb 255 137 5) |> makeTransparent 0.1
        , group
            [
        --, triangle 10 |> filled (rgb 100 0 100) |> notifyTap SizePlus |> move ( 0, -5 ) |> notifyMouseDown (ButtonDown SizePlus) |> notifyMouseUp (ButtonDown None)
        --, triangle 10 |> filled (rgb 100 0 100) |> rotate (degrees 180) |> notifyTap SizeMinus |> move ( -110, -5 ) |> notifyMouseDown (ButtonDown SizeMinus) |> notifyMouseUp (ButtonDown None)
            ]
        , group
            [ text ("More") |> fixedwidth |> size 10 |> filled black |> move (model.adjRight, -50)
            , text ("Fewer") |> fixedwidth |> size 10 |> filled black |> move (model.adjLeft, -50)
            , text (String.fromFloat model.size) |> fixedwidth |> size 10 |> filled black
                |> move ((model.adjLeft + (model.adjRight-model.adjLeft)*(model.size/(model.maxSize-model.minSize))) , -40)
            ]
        , drawBars model.array 
        ]

drawBars nums =
    group (bars 0 nums [])

-- n = counter
-- list accumulates rectangles (bars)
-- nums contains array of unsorted numbers
bars n nums list =
    case nums of
        h::t ->
            (rect 10 (10+20*h) |> filled blue |> move ((-160+15*n), -20+10*h)) :: (bars (n+1) t list)
        [] -> []        


type Msg m
    = Tick Float GetKeyState
    | SizeIncr
    | SizeDecr
    | MouseUp
    | ButtonDown ButtonDir
    | Notif Notifications

type ButtonDir
    = SizeMinus
    | SizePlus
    | Next
    | None

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
 
update msg model =
    case msg of
        Tick t _ -> 
            let
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

        Notif notif ->
            { model | notify = notif }
        
        -- Next ->
        --     { model
        --         | array =
                    
        --         }

-- mergeSort list =
--     case list of
--         [] -> []
--         [_] -> list
--         h::t ->
--             (merge
--                 (mergeSort (partition (\x -> x < ((length list)/2))))
--             )

merge list1 list2 =
    case list1 of
        [] -> list2
        h1::t1 ->
            case list2 of
                [] -> []
                h2::t2 ->
                    if h1 < h2 then
                        h1 :: merge t2 list2
                    else    
                        h2 :: merge list1 t2