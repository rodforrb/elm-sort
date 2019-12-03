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
    , array = array20 -- random unsorted list
    , moved = array20 -- array of moved values, 0 otherwise, populated by draw function
    -- , array = [6, 5, 4, 2, 8, 1, 3, 7]
    , steps = 3 -- start at the first actual merge
    }

array20 = [3, 8, 1, 5, 2, 0, 4, 7, 2, 5, 3, 8, 6, 1, 0, 6, 3, 8, 5, 3] -- random unsorted list

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
        , drawBars model.array green
        , drawBars model.moved blue
        , rect 60 40 |> filled green |> move (180, -100) |> notifyTap Step
        , group
            [ text "Merge Sort" |> fixedwidth |> size 10 |> filled black |> move (model.adjLeft, -100)
            , text "Insertion Sort" |> fixedwidth |> size 10 |> filled black |> move (model.adjLeft, -120)
            , text "Quick Sort" |> fixedwidth |> size 10 |> filled black |> move (model.adjLeft, -140)
            ]
        ]

-- helper function to draw the bars given an array of numbers
-- calls recursive looping function with starting values
-- returns a group of bars
drawBars nums colour =
    group (bars 0 nums [] colour)

-- n = counter
-- list accumulates rectangles (bars)
-- nums contains array of unsorted numbers
bars n nums list colour =
    case nums of
        h::t ->
            (rect 10 (10+20*h) |> filled colour |> move ((-160+15*n), -20+10*h)) :: (bars (n+1) t list colour)
        [] -> []     
  

-- messages send signals to the update function
type Msg m
    = Tick Float GetKeyState
    | Step
    | SizeIncr
    | SizeDecr
    | MouseUp
    | ButtonDown ButtonDir
    | Notif Notifications

-- buttons on the screen
type ButtonDir
    = SizeMinus
    | SizePlus
    | Next
    | None

-- possible notifactions for interactions
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
 
-- given a message and model, return updated model

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
        
        Step ->
            { model
                | array =
                    stepMergeSort model.array model.steps
                , moved =
                    drawMergeSort model.array model.steps
                , steps = model.steps+1
            }

-- recursive merge sort given a list
mergeSort list =
    case list of
        [] -> []
        [_] -> list
        h::t ->
            (merge
                (mergeSort (List.take ((List.length list)//2) list))
                (mergeSort (List.drop ((List.length list)//2) list))
            )

-- merge sort helper for merging lists back together
merge list1 list2 =
    case list1 of
        [] -> list2
        h1::t1 ->
            case list2 of
                [] -> list1
                h2::t2 ->
                    if h1 < h2 then
                        h1 :: merge t1 list2
                    else    
                        h2 :: merge list1 t2


-- recursive merge sort for given number of steps
stepMergeSort list n =
    case list of
        [] -> []
        [_] -> list
        h::t ->
            (stepMerge
                (stepMergeSort (List.take ((List.length list)//2) list) n)
                (stepMergeSort (List.drop ((List.length list)//2) list) (n+1-(List.length list)//2))
                (n-(List.length list))
            )

-- step limited merge sort helper for merging lists back together
stepMerge list1 list2 n =
    if n <= 0 then 
        list1 ++ list2
    else
        case list1 of
            [] -> list2
            h1::t1 ->
                case list2 of
                    [] -> list1
                    h2::t2 ->
                        -- insert next element and continue to sort the set of numbers
                        if h1 < h2 then
                            h1 :: merge t1 list2
                        else    
                            h2 :: merge list1 t2

-- recursive merge sort for given number of steps
drawMergeSort list n =
    case list of
        [] -> []
        [_] -> list
        h::t ->
            (drawMerge
                (drawMergeSort (List.take ((List.length list)//2) list) n)
                (drawMergeSort (List.drop ((List.length list)//2) list) (n+1-(List.length list)//2))
                (n-(List.length list))
            )

-- step limited merge sort helper for merging lists back together
drawMerge list1 list2 n =
    -- don't highlight elements we aren't at yet
    if n <= 0 then
        list1 ++ list2
    -- hide colour for active merge so the highlight colour is seen
    else if n == 1 then 
        hide (list1 ++ list2)
    -- -- don't highlight elements we are past
    -- else if n > 1 then
    --     hide (list1 ++ list2)
    else
        case list1 of
            [] -> list2
            h1::t1 ->
                case list2 of
                    [] -> list1
                    h2::t2 ->
                        -- insert next element and continue to sort the set of numbers
                        if h1 < h2 then
                            h1 :: merge t1 list2
                        else    
                            h2 :: merge list1 t2

-- -10 sets bars to be unseen
hide list =
    case list of
        [] -> []
        h::t ->
            -10 :: hide t