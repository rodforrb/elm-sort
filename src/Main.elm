module Main exposing (main)

import Array exposing (..)
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List
import Random

main =
    gameApp Tick
        { model = init
        , title = "Elm Sort"
        , view = view
        , update = update
        }

init = 
    { size = 32
    , minSize = 4
    , maxSize = 32
    , time = 0
    , currentButton = None
    , adjLeft = -220
    , adjRight = 160
    , notify = NotifyTap
    , array = array32 -- random unsorted list
    , moved = array32 -- array of moved values, 0 otherwise, populated by draw function
    , steps = 2
    , sortType = MergeSort
    }

-- 'random array' which avoids problems of pure programming language
array32 = [ 10, 8, 2, 1, 5, 2, 0, 4, 7, 5
          , 2, 5, 3, 8, 6, 1, 10, 0, 6, 7
          , 3, 8, 5, 3, 10, 4, 8, 2, 0, 6
          , 10, 7
          ]

getArray n =
    let
        m = (round n)
    in
        List.take m array32

view model = 
    collage 600 400 <|
        [ graphPaperCustom 10 1 (rgb 255 137 5) |> makeTransparent 0.1
        , group
            [ text ("More") |> fixedwidth |> size 10 |> filled black |> move ((model.adjRight+50), -50) |> notifyTap (SizeIncr)
            , text ("Fewer") |> fixedwidth |> size 10 |> filled black |> move (model.adjLeft, -50) |> notifyTap (SizeDecr)
            , text (String.fromFloat model.size) |> fixedwidth |> size 10 |> filled black
                |> move ((model.adjLeft + (model.adjRight-model.adjLeft)*(model.size/(model.maxSize-model.minSize))) , -38)
            ]
        , drawBars model.array green -- full array, bottom layer
        , drawBars model.moved blue -- same array with 'active' elements removed. top layer/normal colour
        , rect 60 40 |> filled green |> move (model.adjRight+50, -90) |> notifyTap Step
        , text "Pick an Algorithm:" |> fixedwidth |> size 14 |> filled black |> move (model.adjLeft, -80)
        , group
            [ text "Merge Sort" |> fixedwidth |> size 10 |> filled black |> move (model.adjLeft+20, -100) |> notifyTap (SetSort MergeSort)
            , text "Insertion Sort" |> fixedwidth |> size 10 |> filled black |> move (model.adjLeft+20, -120) |> notifyTap (SetSort InsertionSort)
            , text "Quick Sort" |> fixedwidth |> size 10 |> filled black |> move (model.adjLeft+20, -140) |> notifyTap (SetSort QuickSort)
            ]
        , group
            [  let y =
                    case model.sortType of
                        MergeSort -> -97
                        InsertionSort -> -117
                        QuickSort -> -137
                in
                    triangle 7 |> filled orange |> move (model.adjLeft+5, y)
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
            (rect 10 (10+20*h) |> filled colour |> move ((-220+15*n), -20+10*h)) :: (bars (n+1) t list colour)
        [] -> []     


-- messages send signals to the update function
type Msg m
    = Tick Float GetKeyState
    | Step
    | SizeIncr
    | SizeDecr
    | MouseUp
    | SetSort Sort
    | ButtonDown ButtonDir
    | Notif Notifications

-- buttons on the screen
type ButtonDir
    = SizeMinus
    | SizePlus
    | Next
    | None

type Sort
    = MergeSort
    | InsertionSort
    | QuickSort

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
            let decr = if model.size > model.minSize then (model.size-1) else model.size
            in
            { model
                | size =
                    decr
                , array =
                    (getArray decr)
                , moved =
                    (getArray decr)
            }

        SizeIncr ->
            let incr = if model.size < model.maxSize then (model.size+1) else model.size
            in
            { model
                | size =
                    incr
                , array =
                    (getArray incr)
                , moved =
                    (getArray incr)
            }
    
        ButtonDown dir ->
            { model | currentButton = dir }

        MouseUp ->
            { model | currentButton = None }

        Notif notif ->
            { model | notify = notif }
        
        SetSort s ->
            { model
                | sortType = s
                , array = getArray model.size
                , moved = getArray model.size
                , steps = init.steps
            }
        
        Step ->
            { model
                -- full array
                | array =
                    case model.sortType of
                        MergeSort -> stepMergeSort model.array (model.steps+1)
                        InsertionSort -> 
                            -- only updates every second time
                            insertionSort [] (getArray model.size) ((model.steps-1)//2) 
                        QuickSort -> model.array
                -- same array with 'active' elements zeroed out
                , moved =
                    case model.sortType of
                        MergeSort -> drawMergeSort model.array (model.steps+1)
                        InsertionSort ->
                        
                            if (remainderBy 2 model.steps) == 0 then
                                drawInsertionSort1 [] (getArray model.size) ((model.steps+1)//2)
                            else 
                                drawInsertionSort2 [] (getArray model.size) ((model.steps)//2)

                        QuickSort -> model.array
                -- increment step counter 
                , steps = model.steps+1
            }


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

-- stepMergeSort for highlighting active merging segments
-- incidentally this is achieved by removing the active segment
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

-- step limited merge sort helper
-- if active (newest) merge, empties its contents so it is not drawn (showing highlight behind it)
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

-- insertion sort for given number of steps
insertionSort sorted unsorted n =
    case unsorted of
        [] -> sorted
        h::t ->
            insertionSort (insert sorted h (n)) t (n-1)

-- fake insertion sort reverses the list and passes it to the insert function
-- this is because list deconstruction cam only take the front element off the list
insert list num n =
    List.reverse (insert2 (List.reverse list) num n)

insert2 list num n =
    if n <= 0 then num :: list else
    case list of
        [] -> [num]
        h::t ->
            if num < h then
                h :: (insert2 t num (n))
            else
                num :: list


-- insertion sort for given number of steps
-- zeroes out active elements
drawInsertionSort1 sorted unsorted n =
    case unsorted of
        [] -> sorted
        h::t ->
            if n == 1 then
                -- stick the list back together
                sorted ++ (-10 :: t)
            else
                drawInsertionSort1 (drawInsert sorted h (n)) t (n-1)


-- insertion sort for given number of steps
-- zeroes out active elements
drawInsertionSort2 sorted unsorted n =
    case unsorted of
        [] -> sorted
        h::t ->
            drawInsertionSort2 (drawInsert sorted h (n)) t (n-1)


-- fake insertion sort reverses the list and passes it to the insert function
-- this is because list deconstruction cam only take the front element off the list
drawInsert list num n =
    List.reverse (drawInsert2 (List.reverse list) num n)

drawInsert2 list num n =
    if n <= 0 then num :: list else
    case list of
        [] -> 
            if n == 1 then [-10] else [num]
        h::t ->
            if num < h then
                h :: (drawInsert2 t num (n))
            else
                if n == 1 then
                    -10 :: list
                else
                    num :: list

-- returns same size list but without bars
-- -10 sets bars to be unseen
-- don't use this before sorting
hide list =
    case list of
        [] -> []
        h::t ->
            -10 :: hide t
        


