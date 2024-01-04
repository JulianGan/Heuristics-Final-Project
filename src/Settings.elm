module Settings exposing (..)

{-| This module handles everything on the Settings screen.

TODO: You will need to modify this file to add / remove settings for your game.

Adding/removing a setting is a 5-step process.
(I know it seems like a lot, but it is necessary so Elm can make static
guarantees at compile time about your Settings).

I've outlined the five steps below under SETTING DEFINITIONS.

-}

import Common exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import SettingsComponents exposing (..)
import Random


type alias Settings =
    { playmode : Playmode
    , bagcount : Bagcount
    , cheats : Cheats
    , difficulty : Difficulty
    , bag1 : List Int
    , bag2 : List Int
    , bag3 : List Int
    , bag4 : List Int
    , bag5 : List Int
    , bag6 : List Int
    , bag7 : List Int
    , bag8 : List Int
    , curr : Bag
    , temp : Int
    , ebag1 : List Int
    , ebag2 : List Int
    , ebag3 : List Int
    , ebag4 : List Int
    , seed : Int
    , tempbag1 : List Int
    , tempbag2 : List Int
    , tempbag3 : List Int
    , tempbag4 : List Int
    , computer : Computer
    }

default : Settings
default =
    { playmode = PVP
    , bagcount = Bagof4
    , cheats = Cheatsoff
    , difficulty = Easy
    , bag1 = []
    , bag2 = []
    , bag3 = []
    , bag4 = []
    , bag5 = []
    , bag6 = []
    , bag7 = []
    , bag8 = []
    , curr = Bag1
    , temp = 0
    , ebag1 = []
    , ebag2 = []
    , ebag3 = []
    , ebag4 = []
    , seed = 0
    , tempbag1 = []
    , tempbag2 = []
    , tempbag3 = []
    , tempbag4 = []
    , computer = Beginner
    }

type Computer
 = Beginner
 | Advanced

type Bag
    = Bag1
    | Bag2
    | Bag3
    | Bag4
    | Bag5
    | Bag6
    | Bag7
    | Bag8

type Difficulty
    = Easy
    | Hard

type Cheats
    = Cheatson
    | Cheatsoff

type Bagcount
    = Bagof4
    | Bagof6
    | Bagof8

type Playmode
    = PVP
    | PVE

type Msg
    = Setbagcount Bagcount
    | Setplaymode Playmode
    | Setcheats Cheats
    | Setdifficulty Difficulty
    | Add Int
    | Change Int
    | Reset
    | Next
    | SetRandomSeed Int
    | Setcomputer Computer




update : Msg -> Settings -> Settings
update msg settings =
    case msg of
        Setbagcount count -> { settings | bagcount = count, curr = Bag1, bag5 = if count == Bagof4 then [] else settings.bag5, bag6 = if count == Bagof4 then [] else settings.bag6, bag7 = if count == Bagof4 || count == Bagof6 then [] else settings.bag7, bag8 = if count == Bagof4 || count == Bagof6 then [] else settings.bag8 }
        Setplaymode p -> { settings | playmode = p , curr = Bag1 }
        Setcheats c -> { settings | cheats = c }
        Setdifficulty d -> { settings | difficulty = d }
        Add v ->
            case settings.curr of
                Bag1 -> { settings | bag1 = if List.length settings.bag1 < 5 then v::settings.bag1 else settings.bag1 }
                Bag2 -> { settings | bag2 = if List.length settings.bag2 < 5 then v::settings.bag2 else settings.bag2 }
                Bag3 -> { settings | bag3 = if List.length settings.bag3 < 5 then v::settings.bag3 else settings.bag3 }
                Bag4 -> { settings | bag4 = if List.length settings.bag4 < 5 then v::settings.bag4 else settings.bag4 }
                Bag5 -> { settings | bag5 = if List.length settings.bag5 < 5 then v::settings.bag5 else settings.bag5 }
                Bag6 -> { settings | bag6 = if List.length settings.bag6 < 5 then v::settings.bag6 else settings.bag6 }
                Bag7 -> { settings | bag7 = if List.length settings.bag7 < 5 then v::settings.bag7 else settings.bag7 }
                Bag8 -> { settings | bag8 = if List.length settings.bag8 < 5 then v::settings.bag8 else settings.bag8 }
        Change f -> { settings | temp = f }
        Reset -> { settings | bag1 = [], bag2 = [], bag3 = [], bag4 = [], bag5 = [], bag6 = [], bag7 = [], bag8 = []}
        Next -> {
            settings |
            curr = 
            case settings.playmode of
                PVP ->
                    case settings.curr of
                        Bag1 -> Bag2
                        Bag2 -> Bag3
                        Bag3 -> Bag4
                        Bag4 -> if settings.bagcount == Bagof4 then Bag1 else Bag5
                        Bag5 -> Bag6
                        Bag6 -> if settings.bagcount == Bagof6 then Bag1 else Bag7
                        Bag7 -> Bag8
                        Bag8 -> Bag1
                PVE ->
                    case settings.curr of
                        Bag1 -> Bag2
                        Bag2 -> if settings.bagcount == Bagof4 then Bag1 else Bag3
                        Bag3 -> if settings.bagcount == Bagof6 then Bag1 else Bag4
                        Bag4 -> Bag1
                        Bag5 -> Bag1
                        Bag6 -> Bag1
                        Bag7 -> Bag1
                        Bag8 -> Bag1
            }
        SetRandomSeed v -> 
            { 
              settings | 
              seed = v
            , ebag1 =
                let
                    (value, newseed) = Random.step (Random.int 2 5) (Random.initialSeed v)
                    (ls, s) = Random.step (Random.list value (Random.int 0 30)) newseed
                in
                    ls
            , ebag2 =
                let
                    (value, newseed) = Random.step (Random.int 2 5) (Random.initialSeed v)
                    (ls, newseed2) = Random.step (Random.list value (Random.int 0 30)) newseed
                    (value2, newseed3) = Random.step (Random.int 2 5) newseed2
                    (ls2, newseed4) = Random.step (Random.list value2 (Random.int 0 30)) newseed3
                in
                    ls2
            , ebag3 =
                let
                    (value, newseed) = Random.step (Random.int 2 5) (Random.initialSeed v)
                    (ls, newseed2) = Random.step (Random.list value (Random.int 0 30)) newseed
                    (value2, newseed3) = Random.step (Random.int 2 5) newseed2
                    (ls2, newseed4) = Random.step (Random.list value2 (Random.int 0 30)) newseed3
                    (value3, newseed5) = Random.step (Random.int 2 5) newseed4
                    (ls3, newseed6) = Random.step (Random.list value3 (Random.int 0 30)) newseed5
                in
                    ls3
            , ebag4 =
                let
                    (value, newseed) = Random.step (Random.int 2 5) (Random.initialSeed v)
                    (ls, newseed2) = Random.step (Random.list value (Random.int 0 30)) newseed
                    (value2, newseed3) = Random.step (Random.int 2 5) newseed2
                    (ls2, newseed4) = Random.step (Random.list value2 (Random.int 0 30)) newseed3
                    (value3, newseed5) = Random.step (Random.int 2 5) newseed4
                    (ls3, newseed6) = Random.step (Random.list value3 (Random.int 0 30)) newseed5
                    (value4, newseed7) = Random.step (Random.int 2 5) newseed6
                    (ls4, newseed8) = Random.step (Random.list value4 (Random.int 0 30)) newseed7
                in
                    ls4
            }
        Setcomputer c -> { settings | computer = c}

                


pickers : Settings -> List (SettingPickerItem Msg)
pickers settings =
    [ pickChoiceButtons
    {
      label = ""
    , onSelect = Setplaymode
    , current = settings.playmode
    , options = [("human vs. human", PVP), ("human vs. computer", PVE)]
    }
    , pickChoiceButtons 
    {
      label = "Bag Count"
    , onSelect = Setbagcount
    , current = settings.bagcount
    , options = [("4", Bagof4), ("6", Bagof6), ("8", Bagof8)]
    }
    ]

pickers2 : Settings -> List (SettingPickerItem Msg)
pickers2 settings =
    case settings.playmode of
        PVP ->
            [
            inputIntRange
            {
            label = "value"
            , value = settings.temp
            , min = 0
            , max = 30
            , onChange = Change
            }
            , pickChoiceButtons 
            {
            label = "Difficulty"
            , onSelect = Setdifficulty
            , current = settings.difficulty
            , options = [("Normal", Easy), ("Hard", Hard)]
            }
            , pickChoiceButtons 
            {
            label = ""
            , onSelect = Setcheats
            , current = settings.cheats
            , options = [("Don't Peek", Cheatsoff), ("Peek Into Bags", Cheatson)]
            }
            ]
        PVE ->
            [
            inputIntRange
            {
            label = "value"
            , value = settings.temp
            , min = 0
            , max = 30
            , onChange = Change
            }
            , pickChoiceButtons 
            {
            label = "Difficulty"
            , onSelect = Setdifficulty
            , current = settings.difficulty
            , options = [("Normal", Easy), ("Hard", Hard)]
            }
            , pickChoiceButtons
            {
            label = ""
            , onSelect = Setcomputer
            , current = settings.computer
            , options = [("Beginner Computer", Beginner), ("Advanced Computer", Advanced)]
            }
            , pickChoiceButtons 
            {
            label = ""
            , onSelect = Setcheats
            , current = settings.cheats
            , options = [("Don't Peek", Cheatsoff), ("Peek Into Bags", Cheatson)]
            }
            ]

getcurr : Settings -> String
getcurr s = 
    case s.curr of
        Bag1 -> "bag 1"
        Bag2 -> "bag 2"
        Bag3 -> "bag 3"
        Bag4 -> "bag 4"
        Bag5 -> "bag 5"
        Bag6 -> "bag 6"
        Bag7 -> "bag 7"
        Bag8 -> "bag 8"


view : Settings -> Html Msg
view settings =
    case settings.playmode of
        PVP ->
            case settings.bagcount of
                Bagof4 ->
                    div [ id "settings" ]
                        [ viewPicker [] (pickers settings)
                        , div [] ((text "Bag 1 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag1)
                        , div [] ((text "Bag 2 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag2)
                        , div [] ((text "Bag 3 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag3)
                        , div [] ((text "Bag 4 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag4)
                        , div [] [text ("Adding values for " ++ (getcurr settings) ++ " ") ,button [onClick Reset] [text "Reset"], button [onClick Next] [text "Next Bag"],button [onClick (Add settings.temp)] [text "Add"]]
                        , viewPicker [] (pickers2 settings)
                        ]
                Bagof6 ->
                    div [ id "settings" ]
                        [ viewPicker [] (pickers settings)
                        , div [] ((text "Bag 1 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag1)
                        , div [] ((text "Bag 2 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag2)
                        , div [] ((text "Bag 3 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag3)
                        , div [] ((text "Bag 4 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag4)
                        , div [] ((text "Bag 5 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag5)
                        , div [] ((text "Bag 6 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag6)
                        , div [] [text ("Adding values for " ++ (getcurr settings) ++ " ") ,button [onClick Reset] [text "Reset"], button [onClick Next] [text "Next Bag"],button [onClick (Add settings.temp)] [text "Add"]]
                        , viewPicker [] (pickers2 settings)
                        ]
                Bagof8 ->
                    div [ id "settings" ]
                        [ viewPicker [] (pickers settings)
                        , div [] ((text "Bag 1 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag1)
                        , div [] ((text "Bag 2 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag2)
                        , div [] ((text "Bag 3 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag3)
                        , div [] ((text "Bag 4 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag4)
                        , div [] ((text "Bag 5 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag5)
                        , div [] ((text "Bag 6 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag6)
                        , div [] ((text "Bag 7 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag7)
                        , div [] ((text "Bag 8 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag8)
                        , div [] [text ("Adding values for " ++ (getcurr settings) ++ " ") ,button [onClick Reset] [text "Reset"], button [onClick Next] [text "Next Bag"],button [onClick (Add settings.temp)] [text "Add"]]
                        , viewPicker [] (pickers2 settings)
                        ]
        PVE ->
            case settings.bagcount of
                Bagof4 ->
                    div [ id "settings" ]
                        [ viewPicker [] (pickers settings)
                        , div [] ((text "Bag 1 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag1)
                        , div [] ((text "Bag 2 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag2)
                        , div [] ((text "Bag 3 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.ebag1)
                        , div [] ((text "Bag 4 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.ebag2)
                        , div [] [text ("Adding values for " ++ (getcurr settings) ++ " ") ,button [onClick Reset] [text "Reset"], button [onClick Next] [text "Next Bag"],button [onClick (Add settings.temp)] [text "Add"]]
                        , viewPicker [] (pickers2 settings)
                        ]
                Bagof6 ->
                    div [ id "settings" ]
                        [ viewPicker [] (pickers settings)
                        , div [] ((text "Bag 1 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag1)
                        , div [] ((text "Bag 2 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag2)
                        , div [] ((text "Bag 3 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag3)
                        , div [] ((text "Bag 4 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.ebag1)
                        , div [] ((text "Bag 5 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.ebag2)
                        , div [] ((text "Bag 6 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.ebag3)
                        , div [] [text ("Adding values for " ++ (getcurr settings) ++ " ") ,button [onClick Reset] [text "Reset"], button [onClick Next] [text "Next Bag"],button [onClick (Add settings.temp)] [text "Add"]]
                        , viewPicker [] (pickers2 settings)
                        ]
                Bagof8 ->
                    div [ id "settings" ]
                        [ viewPicker [] (pickers settings)
                        , div [] ((text "Bag 1 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag1)
                        , div [] ((text "Bag 2 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag2)
                        , div [] ((text "Bag 3 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag3)
                        , div [] ((text "Bag 4 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.bag4)
                        , div [] ((text "Bag 5 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.ebag1)
                        , div [] ((text "Bag 6 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.ebag2)
                        , div [] ((text "Bag 7 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.ebag3)
                        , div [] ((text "Bag 8 -> ") :: List.map (\item -> text (String.fromInt item ++ " - ")) settings.ebag4)
                        , div [] [text ("Adding values for " ++ (getcurr settings) ++ " ") ,button [onClick Reset] [text "Reset"], button [onClick Next] [text "Next Bag"],button [onClick (Add settings.temp)] [text "Add"]]
                        , viewPicker [] (pickers2 settings)
                        ]