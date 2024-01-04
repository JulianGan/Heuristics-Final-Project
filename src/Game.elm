module Game exposing (..)


import Common exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)
import Settings exposing (..)
import Random
import List.Extra


type alias Game =
    { settings : Settings
    , playmode : Playmode
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
    , ebag1 : List Int
    , ebag2 : List Int
    , ebag3 : List Int
    , ebag4 : List Int
    , ebag5 : List Int
    , ebag6 : List Int 
    , ebag7 : List Int
    , ebag8 : List Int
    , seed : Int
    , valuep1 : Int
    , valuep2 : Int
    , turn : Turn
    , gameover : Bool
    , lastmove : String
    , lastlastmove : String
    , p1finished : Bool
    , p2finished : Bool
    , testnumber : Int
    , computer : Computer
    }

{-| Create the initial game data given the settings.
-}
init : Settings -> ( Game, Cmd Msg )
init settings =
    let
        initialGame =
            {
              settings = settings
            , playmode = settings.playmode
            , bagcount = settings.bagcount
            , cheats = settings.cheats
            , difficulty = settings.difficulty
            , bag1 = settings.bag1
            , bag2 = settings.bag2
            , bag3 = settings.bag3
            , bag4 = settings.bag4
            , bag5 = settings.bag5
            , bag6 = settings.bag6
            , bag7 = settings.bag7
            , bag8 = settings.bag8
            , ebag1 = settings.bag1
            , ebag2 = settings.bag2
            , ebag3 = 
                case settings.bagcount of
                    Bagof4 -> settings.ebag1
                    Bagof6 -> settings.bag3
                    Bagof8 -> settings.bag3
            , ebag4 =
                case settings.bagcount of
                    Bagof4 -> settings.ebag2
                    Bagof6 -> settings.ebag1
                    Bagof8 -> settings.bag4
            , ebag5 =
                case settings.bagcount of
                    Bagof4 -> []
                    Bagof6 -> settings.ebag2
                    Bagof8 -> settings.ebag1
            , ebag6 =
                case settings.bagcount of
                    Bagof4 -> []
                    Bagof6 -> settings.ebag3
                    Bagof8 -> settings.ebag2
            , ebag7 =
                case settings.bagcount of
                    Bagof4 -> []
                    Bagof6 -> []
                    Bagof8 -> settings.ebag3
            , ebag8 =
                case settings.bagcount of
                    Bagof4 -> []
                    Bagof6 -> []
                    Bagof8 -> settings.ebag4
            , seed = settings.seed
            , valuep1 = 0
            , valuep2 = 0
            , turn = Player1
            , gameover = False
            , lastmove = ""
            , lastlastmove = ""
            , p1finished = False
            , p2finished = False
            , testnumber = 1
            , computer = settings.computer
            }
    in
    ( initialGame, Cmd.none )

type Turn
    = Player1
    | Player2

type Move
    = Pickbag Bag
    | Endmyturn
    | EPickbag Bag
    | EEndmyturn

switchturn : Game -> Turn
switchturn game =
    case game.turn of
        Player1 -> if game.p2finished then Player1 else Player2
        Player2 -> if game.p1finished then Player2 else Player1

getnewlist : Game -> Bag -> (List Int, Int, Int)
getnewlist game bag = 
    let
        mybag = 
            case bag of
                Bag1 -> game.bag1
                Bag2 -> game.bag2
                Bag3 -> game.bag3
                Bag4 -> game.bag4
                Bag5 -> game.bag5
                Bag6 -> game.bag6
                Bag7 -> game.bag7
                Bag8 -> game.bag8
        
        generator = 
            Random.int 0 ((length mybag) - 1)

        (value, dud) =
            Random.step generator (Random.initialSeed game.seed)

        (newseed, dud2) =
            Random.step (Random.int 0 9999999) dud
        
        addscore = Maybe.withDefault 0 (List.Extra.getAt value mybag)
        newbag = List.Extra.removeAt value mybag
    in
        (newbag, newseed, addscore)

egetnewlist : Game -> Bag -> (List Int, Int, Int)
egetnewlist game bag = 
    let
        mybag =
            case bag of
                Bag1 -> game.ebag1
                Bag2 -> game.ebag2
                Bag3 -> game.ebag3
                Bag4 -> game.ebag4
                Bag5 -> game.ebag5
                Bag6 -> game.ebag6
                Bag7 -> game.ebag7
                Bag8 -> game.ebag8
        
        generator = 
            Random.int 0 ((length mybag) - 1)

        (value, dud) =
            Random.step generator (Random.initialSeed game.seed)

        (newseed, dud2) =
            Random.step (Random.int 0 9999999) dud
        
        addscore = Maybe.withDefault 0 (List.Extra.getAt value mybag)
        newbag = List.Extra.removeAt value mybag
    in
        (newbag, newseed, addscore)

getmyscore : Game -> Int -- helper
getmyscore game = 
    case game.turn of
        Player1 -> game.valuep1
        Player2 -> game.valuep2

isgameover : Game -> Int -> Bool
isgameover game addscore = 
    if game.p1finished && game.p2finished then True
    else
        case game.playmode of
            PVP ->
                case game.difficulty of
                    Easy ->
                        case game.bagcount of
                            Bagof4 ->
                                (length game.bag1) + (length game.bag2) + (length game.bag3) + (length game.bag4) == 1
                            Bagof6 ->
                                (length game.bag1) + (length game.bag2) + (length game.bag3) + (length game.bag4) + (length game.bag5) + (length game.bag6) == 1
                            Bagof8 ->
                                (length game.bag1) + (length game.bag2) + (length game.bag3) + (length game.bag4) + (length game.bag5) + (length game.bag6) + (length game.bag7) + (length game.bag8) == 1
                    Hard ->
                        case game.bagcount of
                            Bagof4 ->
                                ((length game.bag1) + (length game.bag2) + (length game.bag3) + (length game.bag4) == 1) || ((getmyscore game) + addscore) > 100
                            Bagof6 ->
                                ((length game.bag1) + (length game.bag2) + (length game.bag3) + (length game.bag4) + (length game.bag5) + (length game.bag6) == 1) || ((getmyscore game) + addscore) > 100
                            Bagof8 ->
                                ((length game.bag1) + (length game.bag2) + (length game.bag3) + (length game.bag4) + (length game.bag5) + (length game.bag6) + (length game.bag7) + (length game.bag8) == 1) || ((getmyscore game) + addscore) > 100
            PVE ->
                case game.difficulty of
                    Easy ->
                        case game.bagcount of
                            Bagof4 ->
                                (length game.ebag1) + (length game.ebag2) + (length game.ebag3) + (length game.ebag4) == 1
                            Bagof6 ->
                                (length game.ebag1) + (length game.ebag2) + (length game.ebag3) + (length game.ebag4) + (length game.ebag5) + (length game.ebag6) == 1
                            Bagof8 ->
                                (length game.ebag1) + (length game.ebag2) + (length game.ebag3) + (length game.ebag4) + (length game.ebag5) + (length game.ebag6) + (length game.ebag7) + (length game.ebag8) == 1
                    Hard ->
                        case game.bagcount of
                            Bagof4 ->
                                ((length game.ebag1) + (length game.ebag2) + (length game.ebag3) + (length game.ebag4) == 1) || ((getmyscore game) + addscore) > 100
                            Bagof6 ->
                                ((length game.ebag1) + (length game.ebag2) + (length game.ebag3) + (length game.ebag4) + (length game.ebag5) + (length game.ebag6) == 1) || ((getmyscore game) + addscore) > 100
                            Bagof8 ->
                                ((length game.ebag1) + (length game.ebag2) + (length game.ebag3) + (length game.ebag4) + (length game.ebag5) + (length game.ebag6) + (length game.ebag7) + (length game.ebag8) == 1) || ((getmyscore game) + addscore) > 100

endturnf : Game -> Game
endturnf game = 
    case game.turn of   
        Player1 ->
            {
              game |
              gameover = if game.p2finished then True else False
            , p1finished = True
            , lastmove = if game.playmode == PVP then "Alice ends turn" else "You end turn"
            , turn = Player2
            }
        Player2 ->
            {
              game |
              gameover = if game.p1finished then True else False
            , p2finished = True
            , lastmove = if game.playmode == PVP then "Bob ends turn" else "Computer ends turn"
            , turn = Player1
            }

pveendturn : Game -> Game
pveendturn game =
    let
        temp = 
            { game |
            turn = Player2
            , lastlastmove = ""
            , gameover = isgameover game 0
            , p1finished = True
            }
        newgame = randomg temp
    in
        if not newgame.gameover
        then pveendturn newgame
        else newgame

getvalidint : (List Bool) -> Game -> Int 
getvalidint ls game = 
    let  
        (index, dud) = 
            Random.step (Random.int 0 ((length ls) - 1)) (Random.initialSeed game.seed)

        (newseed, dud2) = 
            Random.step (Random.int 0 9999999) dud

        newgame = 
            {
              game |
              seed = newseed
            }

    in
        if (Maybe.withDefault False (List.Extra.getAt index ls))
        then index
        else getvalidint ls newgame

expectedvalueunbound : Game -> Game
expectedvalueunbound game = 
    let
        newgame = 
            if game.gameover
            then game
            else
                let
                    
                    ls = 
                        [ List.map (\item -> toFloat item) game.ebag1
                        , List.map (\item -> toFloat item) game.ebag2
                        , List.map (\item -> toFloat item) game.ebag3
                        , List.map (\item -> toFloat item) game.ebag4
                        , List.map (\item -> toFloat item) game.ebag5
                        , List.map (\item -> toFloat item) game.ebag6
                        , List.map (\item -> toFloat item) game.ebag7
                        , List.map (\item -> toFloat item) game.ebag8
                        ]
                    sums = List.map (\item -> (sum item) / ((toFloat (length item)) + 0.001)) ls
                    max = Maybe.withDefault 0 (maximum sums)
                    index = Maybe.withDefault 0 (List.Extra.elemIndex max sums)
                    


                    {-- 
                    needs index -> which list to choose from
                    everything else works
                    --}

                    mybag : Bag
                    mybag = 
                        if index == 0 then Bag1
                        else if index == 1 then Bag2
                        else if index == 2 then Bag3
                        else if index == 3 then Bag4
                        else if index == 4 then Bag5
                        else if index == 5 then Bag6
                        else if index == 6 then Bag7
                        else if index == 7 then Bag8
                        else Bag1
                    (newbag, newseed, addscore) = egetnewlist game mybag

                in
                    { game |
                      ebag1 = if mybag == Bag1 then newbag else game.ebag1
                    , ebag2 = if mybag == Bag2 then newbag else game.ebag2
                    , ebag3 = if mybag == Bag3 then newbag else game.ebag3
                    , ebag4 = if mybag == Bag4 then newbag else game.ebag4
                    , ebag5 = if mybag == Bag5 then newbag else game.ebag5
                    , ebag6 = if mybag == Bag6 then newbag else game.ebag6
                    , ebag7 = if mybag == Bag7 then newbag else game.ebag7
                    , ebag8 = if mybag == Bag8 then newbag else game.ebag8
                    , valuep2 = game.valuep2 + addscore
                    , seed = newseed
                    , gameover = isgameover game addscore
                    , lastmove = 
                        case mybag of
                            Bag1 -> "Computer chose bag 1 and recieved " ++ (String.fromInt addscore) ++ " points"
                            Bag2 -> "Computer chose bag 2 and recieved " ++ (String.fromInt addscore) ++ " points"
                            Bag3 -> "Computer chose bag 3 and recieved " ++ (String.fromInt addscore) ++ " points"
                            Bag4 -> "Computer chose bag 4 and recieved " ++ (String.fromInt addscore) ++ " points"
                            Bag5 -> "Computer chose bag 5 and recieved " ++ (String.fromInt addscore) ++ " points"
                            Bag6 -> "Computer chose bag 6 and recieved " ++ (String.fromInt addscore) ++ " points"
                            Bag7 -> "Computer chose bag 7 and recieved " ++ (String.fromInt addscore) ++ " points"
                            Bag8 -> "Computer chose bag 8 and recieved " ++ (String.fromInt addscore) ++ " points"
                    , turn = Player1
                    }
    in
        newgame

randommoveunbound : Game -> Game
randommoveunbound game = 
    let
        newgame = 
            if game.gameover
            then game
            else
                let
                    {--
                    ls = [game.ebag1, game.ebag2, game.ebag3, game.ebag4, game.ebag5, game.ebag6, game.ebag7, game.ebag8]
                    sums = List.map (\item -> sum item) ls
                    max = Maybe.withDefault 0 (maximum sums)
                    index = Maybe.withDefault 0 (List.Extra.elemIndex max sums)
                    --}

                    ls = [game.ebag1, game.ebag2, game.ebag3, game.ebag4, game.ebag5, game.ebag6, game.ebag7, game.ebag8]
                    list_of_bools = List.map (\item -> (length item) > 0) ls
                    index = getvalidint list_of_bools game

                    {-- 
                    needs index -> which list to choose from
                    everything else works
                    --}

                    mybag : Bag
                    mybag = 
                        if index == 0 then Bag1
                        else if index == 1 then Bag2
                        else if index == 2 then Bag3
                        else if index == 3 then Bag4
                        else if index == 4 then Bag5
                        else if index == 5 then Bag6
                        else if index == 6 then Bag7
                        else if index == 7 then Bag8
                        else Bag1
                    (newbag, newseed, addscore) = egetnewlist game mybag

                in
                    { game |
                      ebag1 = if mybag == Bag1 then newbag else game.ebag1
                    , ebag2 = if mybag == Bag2 then newbag else game.ebag2
                    , ebag3 = if mybag == Bag3 then newbag else game.ebag3
                    , ebag4 = if mybag == Bag4 then newbag else game.ebag4
                    , ebag5 = if mybag == Bag5 then newbag else game.ebag5
                    , ebag6 = if mybag == Bag6 then newbag else game.ebag6
                    , ebag7 = if mybag == Bag7 then newbag else game.ebag7
                    , ebag8 = if mybag == Bag8 then newbag else game.ebag8
                    , valuep2 = game.valuep2 + addscore
                    , seed = newseed
                    , gameover = isgameover game addscore
                    , lastmove = 
                        case mybag of
                            Bag1 -> "Computer chose bag 1 and recieved " ++ (String.fromInt addscore) ++ " points"
                            Bag2 -> "Computer chose bag 2 and recieved " ++ (String.fromInt addscore) ++ " points"
                            Bag3 -> "Computer chose bag 3 and recieved " ++ (String.fromInt addscore) ++ " points"
                            Bag4 -> "Computer chose bag 4 and recieved " ++ (String.fromInt addscore) ++ " points"
                            Bag5 -> "Computer chose bag 5 and recieved " ++ (String.fromInt addscore) ++ " points"
                            Bag6 -> "Computer chose bag 6 and recieved " ++ (String.fromInt addscore) ++ " points"
                            Bag7 -> "Computer chose bag 7 and recieved " ++ (String.fromInt addscore) ++ " points"
                            Bag8 -> "Computer chose bag 8 and recieved " ++ (String.fromInt addscore) ++ " points"
                    , turn = Player1
                    }
    in
        newgame

expectsunder100 : List Float -> Int -> Bool
expectsunder100 item value =
    (sum item) / (toFloat (length item)) + (toFloat value) < 100

randommovebound : Game -> Game
randommovebound game = 
    let
        newgame = 
            if game.gameover
            then game
            else
                let
                    {--
                    ls = [game.ebag1, game.ebag2, game.ebag3, game.ebag4, game.ebag5, game.ebag6, game.ebag7, game.ebag8]
                    sums = List.map (\item -> sum item) ls
                    max = Maybe.withDefault 0 (maximum sums)
                    index = Maybe.withDefault 0 (List.Extra.elemIndex max sums)
                    --}

                    ls = [game.ebag1, game.ebag2, game.ebag3, game.ebag4, game.ebag5, game.ebag6, game.ebag7, game.ebag8]

                    -- length item > 0 and expected value + myscore < 100
                    -- (sum item) / ((toFloat (length item)) + 0.001) + (toFloat (game.valuep2)) < 100
                    list_of_bools = List.map (\item -> (length item) > 0 && (expectsunder100 (List.map (\i -> toFloat i) item) game.valuep2)) ls

                    computer_end_turn = not (member True list_of_bools)

                    index = 
                        if computer_end_turn
                        then 0
                        else getvalidint list_of_bools game


                    {-- 
                    needs index -> which list to choose from
                    everything else works
                    --}

                    mybag : Bag
                    mybag = 
                        if index == 0 then Bag1
                        else if index == 1 then Bag2
                        else if index == 2 then Bag3
                        else if index == 3 then Bag4
                        else if index == 4 then Bag5
                        else if index == 5 then Bag6
                        else if index == 6 then Bag7
                        else if index == 7 then Bag8
                        else Bag1
                    (newbagtemp, newseed, addscore) = egetnewlist game mybag

                    newbag = 
                        if computer_end_turn 
                        then
                            case mybag of
                                Bag1 -> game.ebag1
                                Bag2 -> game.ebag2
                                Bag3 -> game.ebag3
                                Bag4 -> game.ebag4
                                Bag5 -> game.ebag5
                                Bag6 -> game.ebag6
                                Bag7 -> game.ebag7
                                Bag8 -> game.ebag8
                        else newbagtemp

                in
                    { game |
                      ebag1 = if mybag == Bag1 then newbag else game.ebag1
                    , ebag2 = if mybag == Bag2 then newbag else game.ebag2
                    , ebag3 = if mybag == Bag3 then newbag else game.ebag3
                    , ebag4 = if mybag == Bag4 then newbag else game.ebag4
                    , ebag5 = if mybag == Bag5 then newbag else game.ebag5
                    , ebag6 = if mybag == Bag6 then newbag else game.ebag6
                    , ebag7 = if mybag == Bag7 then newbag else game.ebag7
                    , ebag8 = if mybag == Bag8 then newbag else game.ebag8
                    , valuep2 = if computer_end_turn then game.valuep2 else game.valuep2 + addscore
                    , seed = newseed
                    , gameover = if computer_end_turn then isgameover game 0 else isgameover game addscore
                    , lastmove = 
                        if computer_end_turn then "Computer ends its turn"
                        else
                            case mybag of
                                Bag1 -> "Computer chose bag 1 and recieved " ++ (String.fromInt addscore) ++ " points"
                                Bag2 -> "Computer chose bag 2 and recieved " ++ (String.fromInt addscore) ++ " points"
                                Bag3 -> "Computer chose bag 3 and recieved " ++ (String.fromInt addscore) ++ " points"
                                Bag4 -> "Computer chose bag 4 and recieved " ++ (String.fromInt addscore) ++ " points"
                                Bag5 -> "Computer chose bag 5 and recieved " ++ (String.fromInt addscore) ++ " points"
                                Bag6 -> "Computer chose bag 6 and recieved " ++ (String.fromInt addscore) ++ " points"
                                Bag7 -> "Computer chose bag 7 and recieved " ++ (String.fromInt addscore) ++ " points"
                                Bag8 -> "Computer chose bag 8 and recieved " ++ (String.fromInt addscore) ++ " points"
                    , turn = Player1
                    , p2finished = computer_end_turn
                    }
    in
        newgame

expectedvaluebound : Game -> Game
expectedvaluebound game =
    let
        newgame = 
            if game.gameover
            then game
            else
                let
                    {--
                    ls = [game.ebag1, game.ebag2, game.ebag3, game.ebag4, game.ebag5, game.ebag6, game.ebag7, game.ebag8]
                    sums = List.map (\item -> sum item) ls
                    max = Maybe.withDefault 0 (maximum sums)
                    index = Maybe.withDefault 0 (List.Extra.elemIndex max sums)
                    --}

                    ls = 
                        [ List.map (\item -> toFloat item) game.ebag1
                        , List.map (\item -> toFloat item) game.ebag2
                        , List.map (\item -> toFloat item) game.ebag3
                        , List.map (\item -> toFloat item) game.ebag4
                        , List.map (\item -> toFloat item) game.ebag5
                        , List.map (\item -> toFloat item) game.ebag6
                        , List.map (\item -> toFloat item) game.ebag7
                        , List.map (\item -> toFloat item) game.ebag8
                        ]

                    sums = List.map (\item -> (sum item) / ((toFloat (length item)) + 0.001)) ls

                    lsfiltered = List.map (\item -> (if item + (toFloat game.valuep2) > 100 then 0 else item)) sums
                    max = Maybe.withDefault 0 (maximum lsfiltered)
                    index = Maybe.withDefault 0 (List.Extra.elemIndex max lsfiltered)

                    computer_end_turn = all (\item -> item == 0) lsfiltered



                    {-- 
                    needs index -> which list to choose from
                    everything else works
                    --}

                    mybag : Bag
                    mybag = 
                        if index == 0 then Bag1
                        else if index == 1 then Bag2
                        else if index == 2 then Bag3
                        else if index == 3 then Bag4
                        else if index == 4 then Bag5
                        else if index == 5 then Bag6
                        else if index == 6 then Bag7
                        else if index == 7 then Bag8
                        else Bag1
                    (newbagtemp, newseed, addscore) = egetnewlist game mybag

                    newbag = 
                        if computer_end_turn 
                        then
                            case mybag of
                                Bag1 -> game.ebag1
                                Bag2 -> game.ebag2
                                Bag3 -> game.ebag3
                                Bag4 -> game.ebag4
                                Bag5 -> game.ebag5
                                Bag6 -> game.ebag6
                                Bag7 -> game.ebag7
                                Bag8 -> game.ebag8
                        else newbagtemp

                in
                    { game |
                      ebag1 = if mybag == Bag1 then newbag else game.ebag1
                    , ebag2 = if mybag == Bag2 then newbag else game.ebag2
                    , ebag3 = if mybag == Bag3 then newbag else game.ebag3
                    , ebag4 = if mybag == Bag4 then newbag else game.ebag4
                    , ebag5 = if mybag == Bag5 then newbag else game.ebag5
                    , ebag6 = if mybag == Bag6 then newbag else game.ebag6
                    , ebag7 = if mybag == Bag7 then newbag else game.ebag7
                    , ebag8 = if mybag == Bag8 then newbag else game.ebag8
                    , valuep2 = if computer_end_turn then game.valuep2 else game.valuep2 + addscore
                    , seed = newseed
                    , gameover = if computer_end_turn then isgameover game 0 else isgameover game addscore
                    , lastmove = 
                        if computer_end_turn then "Computer ends its turn"
                        else
                            case mybag of
                                Bag1 -> "Computer chose bag 1 and recieved " ++ (String.fromInt addscore) ++ " points"
                                Bag2 -> "Computer chose bag 2 and recieved " ++ (String.fromInt addscore) ++ " points"
                                Bag3 -> "Computer chose bag 3 and recieved " ++ (String.fromInt addscore) ++ " points"
                                Bag4 -> "Computer chose bag 4 and recieved " ++ (String.fromInt addscore) ++ " points"
                                Bag5 -> "Computer chose bag 5 and recieved " ++ (String.fromInt addscore) ++ " points"
                                Bag6 -> "Computer chose bag 6 and recieved " ++ (String.fromInt addscore) ++ " points"
                                Bag7 -> "Computer chose bag 7 and recieved " ++ (String.fromInt addscore) ++ " points"
                                Bag8 -> "Computer chose bag 8 and recieved " ++ (String.fromInt addscore) ++ " points"
                    , turn = Player1
                    , p2finished = computer_end_turn
                    }
    in
        newgame

randomg : Game -> Game
randomg game = 
    if game.p2finished then game
    else
        case game.difficulty of
            Easy ->
                case game.computer of
                    Beginner ->
                        randommoveunbound game
                    Advanced ->
                        expectedvalueunbound game
            Hard ->
                case game.computer of
                    Beginner ->
                        randommovebound game
                    Advanced ->
                        expectedvaluebound game

g : Game -> Game -- computer moves (all cases)
g game = 
{--

need to simulate turns for easy or hard
need to simulate hard mode on turn end

--}
    let
        newgame = 
            if game.gameover
            then game
            else
                let
                    ls = [game.ebag1, game.ebag2, game.ebag3, game.ebag4, game.ebag5, game.ebag6, game.ebag7, game.ebag8]
                    sums = List.map (\item -> sum item) ls
                    max = Maybe.withDefault 0 (maximum sums)
                    index = Maybe.withDefault 0 (List.Extra.elemIndex max sums)
                    mybag : Bag
                    mybag = 
                        if index == 0 then Bag1
                        else if index == 1 then Bag2
                        else if index == 2 then Bag3
                        else if index == 3 then Bag4
                        else if index == 4 then Bag5
                        else if index == 5 then Bag6
                        else if index == 6 then Bag7
                        else if index == 7 then Bag8
                        else Bag1
                    (newbagtemp, newseed, addscore) = egetnewlist game mybag

                    computer_end_turn : Bool
                    computer_end_turn = 
                        if game.difficulty == Hard
                        then
                            let
                                decision = 
                                    game.valuep2 + addscore > 100
                            in
                                decision
                        else
                            False
                    
                    newbag = 
                        if computer_end_turn 
                        then
                            case mybag of
                                Bag1 -> game.ebag1
                                Bag2 -> game.ebag2
                                Bag3 -> game.ebag3
                                Bag4 -> game.ebag4
                                Bag5 -> game.ebag5
                                Bag6 -> game.ebag6
                                Bag7 -> game.ebag7
                                Bag8 -> game.ebag8
                        else newbagtemp

                in
                    { game |
                      ebag1 = if mybag == Bag1 then newbag else game.ebag1
                    , ebag2 = if mybag == Bag2 then newbag else game.ebag2
                    , ebag3 = if mybag == Bag3 then newbag else game.ebag3
                    , ebag4 = if mybag == Bag4 then newbag else game.ebag4
                    , ebag5 = if mybag == Bag5 then newbag else game.ebag5
                    , ebag6 = if mybag == Bag6 then newbag else game.ebag6
                    , ebag7 = if mybag == Bag7 then newbag else game.ebag7
                    , ebag8 = if mybag == Bag8 then newbag else game.ebag8
                    , valuep2 = if not computer_end_turn then game.valuep2 + addscore else game.valuep2
                    , seed = newseed
                    , gameover = if not computer_end_turn then isgameover game addscore else True
                    , lastmove = 
                        case mybag of
                            Bag1 -> "Computer chose bag 1 and recieved " ++ (String.fromInt addscore) ++ " points"
                            Bag2 -> "Computer chose bag 2 and recieved " ++ (String.fromInt addscore) ++ " points"
                            Bag3 -> "Computer chose bag 3 and recieved " ++ (String.fromInt addscore) ++ " points"
                            Bag4 -> "Computer chose bag 4 and recieved " ++ (String.fromInt addscore) ++ " points"
                            Bag5 -> "Computer chose bag 5 and recieved " ++ (String.fromInt addscore) ++ " points"
                            Bag6 -> "Computer chose bag 6 and recieved " ++ (String.fromInt addscore) ++ " points"
                            Bag7 -> "Computer chose bag 7 and recieved " ++ (String.fromInt addscore) ++ " points"
                            Bag8 -> "Computer chose bag 8 and recieved " ++ (String.fromInt addscore) ++ " points"
                    , turn = Player1
                    }
    in
        newgame

applyMove : Move -> Game -> Game
applyMove move game =
    case move of
        Pickbag bag ->
            case bag of
                Bag1 ->
                    if length game.bag1 == 0
                    then game
                    else
                        let
                            (newbag, newseed, addscore) = getnewlist game bag
                        in
                            { game |
                              bag1 = newbag
                            , seed = newseed
                            , turn = switchturn game
                            , valuep1 = if game.turn == Player1 then game.valuep1 + addscore else game.valuep1
                            , valuep2 = if game.turn == Player2 then game.valuep2 + addscore else game.valuep2
                            , gameover = isgameover game addscore
                            , lastmove = 
                                if game.turn == Player1 
                                then "Alice picks bag 1 and recieved " ++ (String.fromInt addscore) ++ " points"
                                else "Bob picks bag 1 and recieved " ++ (String.fromInt addscore) ++ " points"
                            }
                Bag2 ->
                    if length game.bag2 == 0
                    then game
                    else
                        let
                            (newbag, newseed, addscore) = getnewlist game bag
                        in
                            { game |
                              bag2 = newbag
                            , seed = newseed
                            , turn = switchturn game
                            , valuep1 = if game.turn == Player1 then game.valuep1 + addscore else game.valuep1
                            , valuep2 = if game.turn == Player2 then game.valuep2 + addscore else game.valuep2
                            , gameover = isgameover game addscore
                            , lastmove = 
                                if game.turn == Player1 
                                then "Alice picks bag 1 and recieved " ++ (String.fromInt addscore) ++ " points"
                                else "Bob picks bag 1 and recieved " ++ (String.fromInt addscore) ++ " points"
                            }
                Bag3 ->
                    if length game.bag3 == 0
                    then game
                    else
                        let
                            (newbag, newseed, addscore) = getnewlist game bag
                        in
                            { game |
                              bag3 = newbag
                            , seed = newseed
                            , turn = switchturn game
                            , valuep1 = if game.turn == Player1 then game.valuep1 + addscore else game.valuep1
                            , valuep2 = if game.turn == Player2 then game.valuep2 + addscore else game.valuep2
                            , gameover = isgameover game addscore
                            , lastmove = 
                                if game.turn == Player1 
                                then "Alice picks bag 1 and recieved " ++ (String.fromInt addscore) ++ " points"
                                else "Bob picks bag 1 and recieved " ++ (String.fromInt addscore) ++ " points"
                            }
                Bag4 ->
                    if length game.bag4 == 0
                    then game
                    else
                        let
                            (newbag, newseed, addscore) = getnewlist game bag
                        in
                            { game |
                              bag4 = newbag
                            , seed = newseed
                            , turn = switchturn game
                            , valuep1 = if game.turn == Player1 then game.valuep1 + addscore else game.valuep1
                            , valuep2 = if game.turn == Player2 then game.valuep2 + addscore else game.valuep2
                            , gameover = isgameover game addscore
                            , lastmove = 
                                if game.turn == Player1 
                                then "Alice picks bag 1 and recieved " ++ (String.fromInt addscore) ++ " points"
                                else "Bob picks bag 1 and recieved " ++ (String.fromInt addscore) ++ " points"
                            }
                Bag5 -> 
                    if length game.bag5 == 0
                    then game
                    else
                        let
                            (newbag, newseed, addscore) = getnewlist game bag
                        in
                            { game |
                              bag5 = newbag
                            , seed = newseed
                            , turn = switchturn game
                            , valuep1 = if game.turn == Player1 then game.valuep1 + addscore else game.valuep1
                            , valuep2 = if game.turn == Player2 then game.valuep2 + addscore else game.valuep2
                            , gameover = isgameover game addscore
                            , lastmove = 
                                if game.turn == Player1 
                                then "Alice picks bag 1 and recieved " ++ (String.fromInt addscore) ++ " points"
                                else "Bob picks bag 1 and recieved " ++ (String.fromInt addscore) ++ " points"
                            }
                Bag6 ->
                    if length game.bag6 == 0
                    then game
                    else
                        let
                            (newbag, newseed, addscore) = getnewlist game bag
                        in
                            { game |
                              bag6 = newbag
                            , seed = newseed
                            , turn = switchturn game
                            , valuep1 = if game.turn == Player1 then game.valuep1 + addscore else game.valuep1
                            , valuep2 = if game.turn == Player2 then game.valuep2 + addscore else game.valuep2
                            , gameover = isgameover game addscore
                            , lastmove = 
                                if game.turn == Player1 
                                then "Alice picks bag 1 and recieved " ++ (String.fromInt addscore) ++ " points"
                                else "Bob picks bag 1 and recieved " ++ (String.fromInt addscore) ++ " points"
                            }
                Bag7 ->
                    if length game.bag7 == 0
                    then game
                    else
                        let
                            (newbag, newseed, addscore) = getnewlist game bag
                        in
                            { game |
                              bag7 = newbag
                            , seed = newseed
                            , turn = switchturn game
                            , valuep1 = if game.turn == Player1 then game.valuep1 + addscore else game.valuep1
                            , valuep2 = if game.turn == Player2 then game.valuep2 + addscore else game.valuep2
                            , gameover = isgameover game addscore
                            , lastmove = 
                                if game.turn == Player1 
                                then "Alice picks bag 1 and recieved " ++ (String.fromInt addscore) ++ " points"
                                else "Bob picks bag 1 and recieved " ++ (String.fromInt addscore) ++ " points"
                            }
                Bag8 ->
                    if length game.bag8 == 0
                    then game
                    else
                        let
                            (newbag, newseed, addscore) = getnewlist game bag
                        in
                            { game |
                              bag8 = newbag
                            , seed = newseed
                            , turn = switchturn game
                            , valuep1 = if game.turn == Player1 then game.valuep1 + addscore else game.valuep1
                            , valuep2 = if game.turn == Player2 then game.valuep2 + addscore else game.valuep2
                            , gameover = isgameover game addscore
                            , lastmove = 
                                if game.turn == Player1 
                                then "Alice picks bag 1 and recieved " ++ (String.fromInt addscore) ++ " points"
                                else "Bob picks bag 1 and recieved " ++ (String.fromInt addscore) ++ " points"
                            }
        Endmyturn ->
            endturnf game
        EPickbag bag ->
            case bag of
                Bag1 -> 
                    if length game.ebag1 == 0
                    then game
                    else
                        let
                            (newbag, newseed, addscore) = egetnewlist game bag
                            newgame = 
                                { game |
                                  ebag1 = newbag
                                , seed = newseed
                                , valuep1 = game.valuep1 + addscore
                                , gameover = isgameover game addscore
                                , lastlastmove = "You chose bag 1 and recieved " ++ (String.fromInt addscore) ++ " points"
                                , turn = if game.p2finished then Player1 else Player2 -- cause isgameover calls
                                }
                            
                            newgame2 = 
                                if newgame.gameover
                                then newgame
                                else randomg newgame
                            
                        in
                            newgame2
                Bag2 ->
                    if length game.ebag2 == 0
                    then game
                    else
                        let
                            (newbag, newseed, addscore) = egetnewlist game bag
                            newgame = 
                                { game |
                                  ebag2 = newbag
                                , seed = newseed
                                , valuep1 = game.valuep1 + addscore
                                , gameover = isgameover game addscore
                                , lastlastmove = "You chose bag 2 and recieved " ++ (String.fromInt addscore) ++ " points"
                                , turn = if game.p2finished then Player1 else Player2 -- cause isgameover calls
                                }
                            
                            newgame2 = 
                                if newgame.gameover
                                then newgame
                                else randomg newgame
                            
                        in
                            newgame2
                Bag3 ->
                    if length game.ebag3 == 0
                    then game
                    else
                        let
                            (newbag, newseed, addscore) = egetnewlist game bag
                            newgame = 
                                { game |
                                  ebag3 = newbag
                                , seed = newseed
                                , valuep1 = game.valuep1 + addscore
                                , gameover = isgameover game addscore
                                , lastlastmove = "You chose bag 3 and recieved " ++ (String.fromInt addscore) ++ " points"
                                , turn = if game.p2finished then Player1 else Player2 -- cause isgameover calls
                                }
                            
                            newgame2 = 
                                if newgame.gameover
                                then newgame
                                else randomg newgame
                            
                        in
                            newgame2
                Bag4 -> 
                    if length game.ebag4 == 0
                    then game
                    else
                        let
                            (newbag, newseed, addscore) = egetnewlist game bag
                            newgame = 
                                { game |
                                  ebag4 = newbag
                                , seed = newseed
                                , valuep1 = game.valuep1 + addscore
                                , gameover = isgameover game addscore
                                , lastlastmove = "You chose bag 4 and recieved " ++ (String.fromInt addscore) ++ " points"
                                , turn = if game.p2finished then Player1 else Player2 -- cause isgameover calls
                                }
                            
                            newgame2 = 
                                if newgame.gameover
                                then newgame
                                else randomg newgame
                            
                        in
                            newgame2
                Bag5 -> 
                    if length game.ebag5 == 0
                    then game
                    else
                        let
                            (newbag, newseed, addscore) = egetnewlist game bag
                            newgame = 
                                { game |
                                  ebag5 = newbag
                                , seed = newseed
                                , valuep1 = game.valuep1 + addscore
                                , gameover = isgameover game addscore
                                , lastlastmove = "You chose bag 5 and recieved " ++ (String.fromInt addscore) ++ " points"
                                , turn = if game.p2finished then Player1 else Player2 -- cause isgameover calls
                                }
                            
                            newgame2 = 
                                if newgame.gameover
                                then newgame
                                else randomg newgame
                            
                        in
                            newgame2
                Bag6 -> 
                    if length game.ebag6 == 0
                    then game
                    else
                        let
                            (newbag, newseed, addscore) = egetnewlist game bag
                            newgame = 
                                { game |
                                  ebag6 = newbag
                                , seed = newseed
                                , valuep1 = game.valuep1 + addscore
                                , gameover = isgameover game addscore
                                , lastlastmove = "You chose bag 6 and recieved " ++ (String.fromInt addscore) ++ " points"
                                , turn = if game.p2finished then Player1 else Player2 -- cause isgameover calls
                                }
                            
                            newgame2 = 
                                if newgame.gameover
                                then newgame
                                else randomg newgame
                            
                        in
                            newgame2
                Bag7 -> 
                    if length game.ebag7 == 0
                    then game
                    else
                        let
                            (newbag, newseed, addscore) = egetnewlist game bag
                            newgame = 
                                { game |
                                  ebag7 = newbag
                                , seed = newseed
                                , valuep1 = game.valuep1 + addscore
                                , gameover = isgameover game addscore
                                , lastlastmove = "You chose bag 7 and recieved " ++ (String.fromInt addscore) ++ " points"
                                , turn = if game.p2finished then Player1 else Player2 -- cause isgameover calls
                                }
                            
                            newgame2 = 
                                if newgame.gameover
                                then newgame
                                else randomg newgame
                            
                        in
                            newgame2
                Bag8 -> 
                    if length game.ebag8 == 0
                    then game
                    else
                        let
                            (newbag, newseed, addscore) = egetnewlist game bag
                            newgame = 
                                { game |
                                  ebag8 = newbag
                                , seed = newseed
                                , valuep1 = game.valuep1 + addscore
                                , gameover = isgameover game addscore
                                , lastlastmove = "You chose bag 8 and recieved " ++ (String.fromInt addscore) ++ " points"
                                , turn = if game.p2finished then Player1 else Player2 -- cause isgameover calls
                                }
                            
                            newgame2 = 
                                if newgame.gameover
                                then newgame
                                else randomg newgame
                            
                        in
                            newgame2
        EEndmyturn ->
            pveendturn game

--------------------------------------------------------------------------------

type Msg
    = Choose1
    | Choose2
    | Choose3
    | Choose4
    | Choose5
    | Choose6
    | Choose7
    | Choose8
    | Endturn
    | EChoose1
    | EChoose2
    | EChoose3
    | EChoose4
    | EChoose5
    | EChoose6
    | EChoose7
    | EChoose8
    | EEndturn

withCmd : Cmd Msg -> Game -> ( Game, Cmd Msg )
withCmd cmd game =
    ( game, cmd )


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        Choose1 ->
            game
                |> applyMove (Pickbag Bag1)
                |> withCmd Cmd.none

        Choose2 ->
            game
                |> applyMove (Pickbag Bag2)
                |> withCmd Cmd.none
        
        Choose3 ->
            game
                |> applyMove (Pickbag Bag3)
                |> withCmd Cmd.none

        Choose4 ->
            game
                |> applyMove (Pickbag Bag4)
                |> withCmd Cmd.none

        Choose5 ->
            game
                |> applyMove (Pickbag Bag5)
                |> withCmd Cmd.none
        
        Choose6 ->
            game
                |> applyMove (Pickbag Bag6)
                |> withCmd Cmd.none
        
        Choose7 ->
            game
                |> applyMove (Pickbag Bag7)
                |> withCmd Cmd.none
        
        Choose8 ->
            game
                |> applyMove (Pickbag Bag8)
                |> withCmd Cmd.none
        
        Endturn ->
            game
                |> applyMove Endmyturn
                |> withCmd Cmd.none

        EChoose1 ->
            game
                |> applyMove (EPickbag Bag1)
                |> withCmd Cmd.none

        EChoose2 ->
            game
                |> applyMove (EPickbag Bag2)
                |> withCmd Cmd.none
        
        EChoose3 ->
            game
                |> applyMove (EPickbag Bag3)
                |> withCmd Cmd.none

        EChoose4 ->
            game
                |> applyMove (EPickbag Bag4)
                |> withCmd Cmd.none

        EChoose5 ->
            game
                |> applyMove (EPickbag Bag5)
                |> withCmd Cmd.none
        
        EChoose6 ->
            game
                |> applyMove (EPickbag Bag6)
                |> withCmd Cmd.none
        
        EChoose7 ->
            game
                |> applyMove (EPickbag Bag7)
                |> withCmd Cmd.none
        
        EChoose8 ->
            game
                |> applyMove (EPickbag Bag8)
                |> withCmd Cmd.none

        EEndturn ->
            game
                |> applyMove EEndmyturn
                |> withCmd Cmd.none

--------------------------------------------------------------------------------
-- GAME VIEW FUNCTION
--------------------------------------------------------------------------------

getbaglabel : Game -> Bag -> List (Html Msg)
getbaglabel game bag = 
    case game.playmode of
        PVP ->
            case bag of
                Bag1 -> if length game.bag1 == 0 then [text "empty bag"] else
                    case game.cheats of 
                        Cheatson -> ((text "Bag 1 : ") :: (List.map (\item -> text (String.fromInt item ++ " ")) game.bag1))
                        Cheatsoff -> [text "Bag 1"]
                Bag2 -> if length game.bag2 == 0 then [text "empty bag"] else
                    case game.cheats of 
                        Cheatson -> ((text "Bag 2 : ") :: (List.map (\item -> text (String.fromInt item ++ " ")) game.bag2))
                        Cheatsoff -> [text "Bag 2"]
                Bag3 -> if length game.bag3 == 0 then [text "empty bag"] else
                    case game.cheats of 
                        Cheatson -> ((text "Bag 3 : ") :: (List.map (\item -> text (String.fromInt item ++ " ")) game.bag3))
                        Cheatsoff -> [text "Bag 3"]
                Bag4 -> if length game.bag4 == 0 then [text "empty bag"] else
                    case game.cheats of 
                        Cheatson -> ((text "Bag 4 : ") :: (List.map (\item -> text (String.fromInt item ++ " ")) game.bag4))
                        Cheatsoff -> [text "Bag 4"]
                Bag5 -> if length game.bag5 == 0 then [text "empty bag"] else
                    case game.cheats of 
                        Cheatson -> ((text "Bag 5 : ") :: (List.map (\item -> text (String.fromInt item ++ " ")) game.bag5))
                        Cheatsoff -> [text "Bag 5"]
                Bag6 -> if length game.bag6 == 0 then [text "empty bag"] else
                    case game.cheats of 
                        Cheatson -> ((text "Bag 6 : ") :: (List.map (\item -> text (String.fromInt item ++ " ")) game.bag6))
                        Cheatsoff -> [text "Bag 6"]
                Bag7 -> if length game.bag7 == 0 then [text "empty bag"] else
                    case game.cheats of 
                        Cheatson -> ((text "Bag 7 : ") :: (List.map (\item -> text (String.fromInt item ++ " ")) game.bag7))
                        Cheatsoff -> [text "Bag 7"]
                Bag8 -> if length game.bag8 == 0 then [text "empty bag"] else
                    case game.cheats of 
                        Cheatson -> ((text "Bag 8 : ") :: (List.map (\item -> text (String.fromInt item ++ " ")) game.bag8))
                        Cheatsoff -> [text "Bag 8"]
        PVE ->
            case bag of
                Bag1 -> if length game.ebag1 == 0 then [text "empty bag"] else
                    case game.cheats of 
                        Cheatson -> ((text "Bag 1 : ") :: (List.map (\item -> text (String.fromInt item ++ " ")) game.ebag1))
                        Cheatsoff -> [text "Bag 1"]
                Bag2 -> if length game.ebag2 == 0 then [text "empty bag"] else
                    case game.cheats of 
                        Cheatson -> ((text "Bag 2 : ") :: (List.map (\item -> text (String.fromInt item ++ " ")) game.ebag2))
                        Cheatsoff -> [text "Bag 2"]
                Bag3 -> if length game.ebag3 == 0 then [text "empty bag"] else
                    case game.cheats of 
                        Cheatson -> ((text "Bag 3 : ") :: (List.map (\item -> text (String.fromInt item ++ " ")) game.ebag3))
                        Cheatsoff -> [text "Bag 3"]
                Bag4 -> if length game.ebag4 == 0 then [text "empty bag"] else
                    case game.cheats of 
                        Cheatson -> ((text "Bag 4 : ") :: (List.map (\item -> text (String.fromInt item ++ " ")) game.ebag4))
                        Cheatsoff -> [text "Bag 4"]
                Bag5 -> if length game.ebag5 == 0 then [text "empty bag"] else
                    case game.cheats of 
                        Cheatson -> ((text "Bag 5 : ") :: (List.map (\item -> text (String.fromInt item ++ " ")) game.ebag5))
                        Cheatsoff -> [text "Bag 5"]
                Bag6 -> if length game.ebag6 == 0 then [text "empty bag"] else
                    case game.cheats of 
                        Cheatson -> ((text "Bag 6 : ") :: (List.map (\item -> text (String.fromInt item ++ " ")) game.ebag6))
                        Cheatsoff -> [text "Bag 6"]
                Bag7 -> if length game.ebag7 == 0 then [text "empty bag"] else
                    case game.cheats of 
                        Cheatson -> ((text "Bag 7 : ") :: (List.map (\item -> text (String.fromInt item ++ " ")) game.ebag7))
                        Cheatsoff -> [text "Bag 7"]
                Bag8 -> if length game.ebag8 == 0 then [text "empty bag"] else
                    case game.cheats of 
                        Cheatson -> ((text "Bag 8 : ") :: (List.map (\item -> text (String.fromInt item ++ " ")) game.ebag8))
                        Cheatsoff -> [text "Bag 8"]

gameoverscreen : Game -> Html Msg
gameoverscreen game = 
    case game.playmode of
        PVP -> 
            case game.difficulty of
                Easy ->
                    if game.valuep1 == game.valuep2
                    then div [] [div [id "end-declare"] [text "Draw"], div [id "end-scores"] [text ("Both players ended with " ++ (String.fromInt game.valuep1) ++ " points")]]
                    else if game.valuep1 > game.valuep2
                    then div [] [div [id "end-declare"] [text "Alice Wins"], div [id "end-scores"] [text ("Alice wins with " ++ (String.fromInt game.valuep1) ++ " points over Bob's " ++ (String.fromInt game.valuep2))]]
                    else div [] [div [id "end-declare"] [text "Bob Wins"], div [id "end-scores"] [text ("Bob wins with " ++ (String.fromInt game.valuep2) ++ " points over Alice's " ++ (String.fromInt game.valuep1))]]
                Hard ->
                    if game.valuep1 > 100
                    then div [] [div [id "end-declare"] [text "Bob Wins"], div [id "end-scores"] [text "Alice exceeded the target of 100"]]
                    else if game.valuep2 > 100
                    then div [] [div [id "end-declare"] [text "Alice Wins"], div [id "end-scores"] [text "Bob exceeded the target of 100"]]
                    else if game.valuep1 == game.valuep2
                    then div [] [div [id "end-declare"] [text "Draw"], div [id "end-scores"] [text ("Both players ended with " ++ (String.fromInt game.valuep1) ++ " points")]]
                    else if game.valuep1 > game.valuep2
                    then div [] [div [id "end-declare"] [text "Alice Wins"], div [id "end-scores"] [text ("Alice wins with " ++ (String.fromInt game.valuep1) ++ " points over Bob's " ++ (String.fromInt game.valuep2))]]
                    else div [] [div [id "end-declare"] [text "Bob Wins"], div [id "end-scores"] [text ("Bob wins with " ++ (String.fromInt game.valuep2) ++ " points over Alice's " ++ (String.fromInt game.valuep1))]]
        PVE -> 
            case game.difficulty of
                Easy ->
                    if game.valuep1 == game.valuep2
                    then div [] [div [id "end-declare"] [text "Draw"], div [id "end-scores"] [text ("Both players ended with " ++ (String.fromInt game.valuep1) ++ " points")]]
                    else if game.valuep1 > game.valuep2
                    then div [] [div [id "end-declare"] [text "You Win"], div [id "end-scores"] [text ("You win with " ++ (String.fromInt game.valuep1) ++ " points over Computer's " ++ (String.fromInt game.valuep2))]]
                    else div [] [div [id "end-declare"] [text "Computer Wins"], div [id "end-scores"] [text ("Computer wins with " ++ (String.fromInt game.valuep2) ++ " points over your " ++ (String.fromInt game.valuep1))]]
                Hard ->
                    if game.valuep1 > 100
                    then div [] [div [id "end-declare"] [text "Computer Wins"], div [id "end-scores"] [text "You exceeded the target of 100"]]
                    else if game.valuep2 > 100
                    then div [] [div [id "end-declare"] [text "You Win"], div [id "end-scores"] [text "Computer exceeded the target of 100"]]
                    else if game.valuep1 == game.valuep2
                    then div [] [div [id "end-declare"] [text "Draw"], div [id "end-scores"] [text ("Both players ended with " ++ (String.fromInt game.valuep1) ++ " points")]]
                    else if game.valuep1 > game.valuep2
                    then div [] [div [id "end-declare"] [text "You Win"], div [id "end-scores"] [text ("You win with " ++ (String.fromInt game.valuep1) ++ " points over Computer's " ++ (String.fromInt game.valuep2))]]
                    else div [] [div [id "end-declare"] [text "Computer Wins"], div [id "end-scores"] [text ("Computer wins with " ++ (String.fromInt game.valuep2) ++ " points over your " ++ (String.fromInt game.valuep1))]]

turntoname : Game -> String
turntoname game =
    case game.turn of
        Player1 -> 
            case game.playmode of
                PVP -> "Alice's turn"
                PVE -> "Your turn"
        Player2 -> 
            case game.playmode of
                PVP -> "Bob's turn"
                PVE -> "Computer's turn"

gamelastmoves : Game -> Html Msg
gamelastmoves game =
    if game.lastlastmove == "" && game.lastmove == ""
    then div [] [text "..."]
    else if game.lastlastmove == ""
    then div [] [
      div [] [text "..."]
    , div [] [text game.lastmove]
    ]
    else div [] [
      div [] [text "..."]
    , div [] [text game.lastlastmove]
    , div [] [text game.lastmove]
    ]

view : Game -> Html Msg
view game =
    if game.gameover then gameoverscreen game
    else
    case game.playmode of
        PVP ->
            case game.difficulty of
                Easy ->
                    case game.bagcount of
                        Bagof4 ->
                            div [ id "game-screen-container" ]
                                [ div [id "score-value-display"] 
                                    [strong [id "score-value-left"] [text ("Alice : " ++ (String.fromInt game.valuep1))]
                                    , strong [id "score-value-right"] [text ("Bob : " ++ (String.fromInt game.valuep2))]
                                    ]
                                , h1 [id "play-turn"] [text (turntoname game)]
                                , div [id "counter-buttons"]
                                    [ 
                                      button [ onClick Choose1] (getbaglabel game Bag1)
                                    , button [ onClick Choose2 ] (getbaglabel game Bag2)
                                    ]
                                , div [id "counter-buttons"]
                                    [
                                      button [ onClick Choose3 ] (getbaglabel game Bag3)
                                    , button [ onClick Choose4 ] (getbaglabel game Bag4) 
                                    ]
                                , gamelastmoves game
                                ]
                        Bagof6 ->
                            div [ id "game-screen-container" ]
                                [ div [id "score-value-display"] 
                                    [strong [id "score-value-left"] [text ("Alice : " ++ (String.fromInt game.valuep1))]
                                    , strong [id "score-value-right"] [text ("Bob : " ++ (String.fromInt game.valuep2))]
                                    ]
                                , h1 [id "play-turn"] [text (turntoname game)]
                                , div [id "counter-buttons"]
                                    [ 
                                        button [ onClick Choose1 ] (getbaglabel game Bag1)
                                    , button [ onClick Choose2 ] (getbaglabel game Bag2)
                                    , button [ onClick Choose3 ] (getbaglabel game Bag3)
                                    ]
                                , div [id "counter-buttons"]
                                    [ 
                                        button [ onClick Choose4 ] (getbaglabel game Bag4)
                                    , button [ onClick Choose5 ] (getbaglabel game Bag5)
                                    , button [ onClick Choose6 ] (getbaglabel game Bag6)
                                    ]
                                , div [] [text game.lastmove]
                                ]
                        Bagof8 ->
                            div [ id "game-screen-container" ]
                                [ div [id "score-value-display"] 
                                    [strong [id "score-value-left"] [text ("Alice : " ++ (String.fromInt game.valuep1))]
                                    , strong [id "score-value-right"] [text ("Bob : " ++ (String.fromInt game.valuep2))]
                                    ]
                                , h1 [id "play-turn"] [text (turntoname game)]
                                , div [id "counter-buttons"]
                                    [ 
                                        button [ onClick Choose1 ] (getbaglabel game Bag1)
                                    , button [ onClick Choose2 ] (getbaglabel game Bag2)
                                    , button [ onClick Choose3 ] (getbaglabel game Bag3)
                                    , button [ onClick Choose4 ] (getbaglabel game Bag4)
                                    ]
                                , div [id "counter-buttons"]
                                    [ 
                                        button [ onClick Choose5 ] (getbaglabel game Bag5)
                                    , button [ onClick Choose6 ] (getbaglabel game Bag6)
                                    , button [ onClick Choose7 ] (getbaglabel game Bag7)
                                    , button [ onClick Choose8 ] (getbaglabel game Bag8)
                                    ]
                                , div [] [text game.lastmove]
                                ]
                Hard ->
                    case game.bagcount of
                        Bagof4 ->
                            div [ id "game-screen-container" ]
                                [ div [id "score-value-display"] 
                                    [strong [id "score-value-left"] [text ("Alice : " ++ (String.fromInt game.valuep1))]
                                    , strong [id "score-value-right"] [text ("Bob : " ++ (String.fromInt game.valuep2))]
                                    ]
                                , h1 [id "play-turn"] [text (turntoname game)]
                                , div [id "counter-buttons"]
                                    [ 
                                        button [ onClick Choose1 ] (getbaglabel game Bag1)
                                    , button [ onClick Choose2 ] (getbaglabel game Bag2)
                                    ]
                                , div [id "counter-buttons"]
                                    [
                                        button [ onClick Choose3 ] (getbaglabel game Bag3)
                                    , button [ onClick Choose4 ] (getbaglabel game Bag4)
                                    ]
                                , div [id "score-value-right"] [button [onClick Endturn] [text "End Turn"]]
                                , gamelastmoves game
                                ]
                        Bagof6 ->
                            div [ id "game-screen-container" ]
                                [ div [id "score-value-display"] 
                                    [strong [id "score-value-left"] [text ("Alice : " ++ (String.fromInt game.valuep1))]
                                    , strong [id "score-value-right"] [text ("Bob : " ++ (String.fromInt game.valuep2))]
                                    ]
                                , h1 [id "play-turn"] [text (turntoname game)]
                                , div [id "counter-buttons"]
                                    [ 
                                        button [ onClick Choose1 ] (getbaglabel game Bag1)
                                    , button [ onClick Choose2 ] (getbaglabel game Bag2)
                                    , button [ onClick Choose3 ] (getbaglabel game Bag3)
                                    ]
                                , div [id "counter-buttons"]
                                    [ 
                                        button [ onClick Choose4 ] (getbaglabel game Bag4)
                                    , button [ onClick Choose5 ] (getbaglabel game Bag5)
                                    , button [ onClick Choose6 ] (getbaglabel game Bag6)
                                    ]
                                , div [id "score-value-right"] [button [onClick Endturn] [text "End Turn"]]
                                , div [] [text game.lastmove]
                                ]
                        Bagof8 ->
                            div [ id "game-screen-container" ]
                                [ div [id "score-value-display"] 
                                    [strong [id "score-value-left"] [text ("Alice : " ++ (String.fromInt game.valuep1))]
                                    , strong [id "score-value-right"] [text ("Bob : " ++ (String.fromInt game.valuep2))]
                                    ]
                                , h1 [id "play-turn"] [text (turntoname game)]
                                , div [id "counter-buttons"]
                                    [ 
                                        button [ onClick Choose1 ] (getbaglabel game Bag1)
                                    , button [ onClick Choose2 ] (getbaglabel game Bag2)
                                    , button [ onClick Choose3 ] (getbaglabel game Bag3)
                                    , button [ onClick Choose4 ] (getbaglabel game Bag4)
                                    ]
                                , div [id "counter-buttons"]
                                    [ 
                                        button [ onClick Choose5 ] (getbaglabel game Bag5)
                                    , button [ onClick Choose6 ] (getbaglabel game Bag6)
                                    , button [ onClick Choose7 ] (getbaglabel game Bag7)
                                    , button [ onClick Choose8 ] (getbaglabel game Bag8)
                                    ]
                                , div [id "score-value-right"] [button [onClick Endturn] [text "End Turn"]]
                                , div [] [text game.lastmove]
                                ]
        PVE ->
            case game.difficulty of
                Easy ->
                    case game.bagcount of
                        Bagof4 ->
                            div [ id "game-screen-container" ]
                                [ div [id "score-value-display"] 
                                    [strong [id "score-value-left"] [text ("You : " ++ (String.fromInt game.valuep1))]
                                    , strong [id "score-value-right"] [text ("Computer : " ++ (String.fromInt game.valuep2))]
                                    ]
                                , h1 [id "play-turn"] [text (turntoname game)]
                                , div [id "counter-buttons"]
                                    [ 
                                        button [ onClick EChoose1 ] (getbaglabel game Bag1)
                                    , button [ onClick EChoose2 ] (getbaglabel game Bag2)
                                    ]
                                , div [id "counter-buttons"]
                                    [
                                        button [ onClick EChoose3 ] (getbaglabel game Bag3)
                                    , button [ onClick EChoose4 ] (getbaglabel game Bag4)
                                    ]
                                , gamelastmoves game
                                ]
                        Bagof6 ->
                            div [ id "game-screen-container" ]
                                [ div [id "score-value-display"] 
                                    [strong [id "score-value-left"] [text ("You : " ++ (String.fromInt game.valuep1))]
                                    , strong [id "score-value-right"] [text ("Computer : " ++ (String.fromInt game.valuep2))]
                                    ]
                                , h1 [id "play-turn"] [text (turntoname game)]
                                , div [id "counter-buttons"]
                                    [ 
                                        button [ onClick EChoose1 ] (getbaglabel game Bag1)
                                    , button [ onClick EChoose2 ] (getbaglabel game Bag2)
                                    , button [ onClick EChoose3 ] (getbaglabel game Bag3)
                                    ]
                                , div [id "counter-buttons"]
                                    [ 
                                        button [ onClick EChoose4 ] (getbaglabel game Bag4)
                                    , button [ onClick EChoose5 ] (getbaglabel game Bag5)
                                    , button [ onClick EChoose6 ] (getbaglabel game Bag6)
                                    ]
                                , div [] [text game.lastmove]
                                ]
                        Bagof8 ->
                            div [ id "game-screen-container" ]
                                [ div [id "score-value-display"] 
                                    [strong [id "score-value-left"] [text ("You : " ++ (String.fromInt game.valuep1))]
                                    , strong [id "score-value-right"] [text ("Computer : " ++ (String.fromInt game.valuep2))]
                                    ]
                                , h1 [id "play-turn"] [text (turntoname game)]
                                , div [id "counter-buttons"]
                                    [ 
                                        button [ onClick EChoose1 ] (getbaglabel game Bag1)
                                    , button [ onClick EChoose2 ] (getbaglabel game Bag2)
                                    , button [ onClick EChoose3 ] (getbaglabel game Bag3)
                                    , button [ onClick EChoose4 ] (getbaglabel game Bag4)
                                    ]
                                , div [id "counter-buttons"]
                                    [ 
                                        button [ onClick EChoose5 ] (getbaglabel game Bag5)
                                    , button [ onClick EChoose6 ] (getbaglabel game Bag6)
                                    , button [ onClick EChoose7 ] (getbaglabel game Bag7)
                                    , button [ onClick EChoose8 ] (getbaglabel game Bag8)
                                    ]
                                , div [] [text game.lastmove]
                                ]
                Hard ->
                    case game.bagcount of
                        Bagof4 ->
                            div [ id "game-screen-container" ]
                                [ div [id "score-value-display"] 
                                    [strong [id "score-value-left"] [text ("You : " ++ (String.fromInt game.valuep1))]
                                    , strong [id "score-value-right"] [text ("Computer : " ++ (String.fromInt game.valuep2))]
                                    ]
                                , h1 [id "play-turn"] [text (turntoname game)]
                                , div [id "counter-buttons"]
                                    [ 
                                        button [ onClick EChoose1 ] (getbaglabel game Bag1)
                                    , button [ onClick EChoose2 ] (getbaglabel game Bag2)
                                    ]
                                , div [id "counter-buttons"]
                                    [
                                        button [ onClick EChoose3 ] (getbaglabel game Bag3)
                                    , button [ onClick EChoose4 ] (getbaglabel game Bag4)
                                    ]
                                , div [id "score-value-right"] [button [onClick EEndturn] [text "End Turn"]]
                                , gamelastmoves game
                                ]
                        Bagof6 ->
                            div [ id "game-screen-container" ]
                                [ div [id "score-value-display"] 
                                    [strong [id "score-value-left"] [text ("You : " ++ (String.fromInt game.valuep1))]
                                    , strong [id "score-value-right"] [text ("Computer : " ++ (String.fromInt game.valuep2))]
                                    ]
                                , h1 [id "play-turn"] [text (turntoname game)]
                                , div [id "counter-buttons"]
                                    [ 
                                        button [ onClick EChoose1 ] (getbaglabel game Bag1)
                                    , button [ onClick EChoose2 ] (getbaglabel game Bag2)
                                    , button [ onClick EChoose3 ] (getbaglabel game Bag3)
                                    ]
                                , div [id "counter-buttons"]
                                    [ 
                                        button [ onClick EChoose4 ] (getbaglabel game Bag4)
                                    , button [ onClick EChoose5 ] (getbaglabel game Bag5)
                                    , button [ onClick EChoose6 ] (getbaglabel game Bag6)
                                    ]
                                , div [id "score-value-right"] [button [onClick EEndturn] [text "End Turn"]]
                                , div [] [text game.lastmove]
                                ]
                        Bagof8 ->
                            div [ id "game-screen-container" ]
                                [ div [id "score-value-display"] 
                                    [strong [id "score-value-left"] [text ("You : " ++ (String.fromInt game.valuep1))]
                                    , strong [id "score-value-right"] [text ("Computer : " ++ (String.fromInt game.valuep2))]
                                    ]
                                , h1 [id "play-turn"] [text (turntoname game)]
                                , div [id "counter-buttons"]
                                    [ 
                                        button [ onClick EChoose1 ] (getbaglabel game Bag1)
                                    , button [ onClick EChoose2 ] (getbaglabel game Bag2)
                                    , button [ onClick EChoose3 ] (getbaglabel game Bag3)
                                    , button [ onClick EChoose4 ] (getbaglabel game Bag4)
                                    ]
                                , div [id "counter-buttons"]
                                    [ 
                                        button [ onClick EChoose5 ] (getbaglabel game Bag5)
                                    , button [ onClick EChoose6 ] (getbaglabel game Bag6)
                                    , button [ onClick EChoose7 ] (getbaglabel game Bag7)
                                    , button [ onClick EChoose8 ] (getbaglabel game Bag8)
                                    ]
                                , div [id "score-value-right"] [button [onClick EEndturn] [text "End Turn"]]
                                , div [] [text game.lastmove]
                                ]