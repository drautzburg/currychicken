import System.Random
import Data.List
import Data.List.Split
import Data.Function (on)
import Debug.Trace
------------------------------------------------------------
-- Cards and Announcements
------------------------------------------------------------
data Suit = Karo | Herz | Pik | Kreuz deriving (Eq, Show)
data Value = Neun | Zehn | B | D | K | As deriving (Eq, Show)
type Card = (Suit, Value)
------------------------------------------------------------

allSuits = [Karo, Herz, Pik, Kreuz] -- ordered by power
allValues = [Neun , K, Zehn, As , B , D]

-- a deck has each value twice for every suit
deck =  half ++ half
  where half = [(s,v)| s<-allSuits, v<-allValues]

-- What is trump
isTrump :: Card -> Bool
isTrump (s,v) = s == Karo || v == B || v==D || (s,v) == (Herz,Zehn)

showCards :: [Card] -> String
showCards cards = concatMap show cards

-- return the Cards of a given suit from a list of Cards
ofSuit :: Suit -> [Card] -> [Card]
ofSuit suit cards = filter (\(s,v) -> s == suit && (not $ isTrump (s,v))) cards 

data Announcement = Contra | Re | Hochzeit | Pass deriving (Eq,Show)

------------------------------------------------------------
-- Computing winners of a round
------------------------------------------------------------

-- All "beats" function answer whether the second card beats the first

beatsTrump c1 c2 = i2 > i1
  where
    Just i1 = elemIndex c1 cards
    Just i2 = elemIndex c2 cards
    queens = [(s,D) | s <- allSuits]
    jacks = [(s,B) | s <- allSuits]
    diamonds = [(Karo, v) | v <- [Neun, K, Zehn, As]]
    -- a list of trump cards ordered by power, weakest fist
    cards = diamonds ++ jacks ++ queens ++ [(Herz, Zehn)]

beatsNotrump (s1,v1) (s2,v2)
        -- the second card can only beat it has the same suit
        | s1 == s2 = beatsValue v1 v2
        | otherwise = False
        where
            beatsValue v1 v2 = i2 > i1
            Just i1 = elemIndex v1 values
            Just i2 = elemIndex v2 values 
            values = [Neun, K, Zehn, As]


-- Answer if the second card beats the first
beats :: Card -> Card -> Bool
beats c1 c2
  | isTrump c1 && isTrump c2 = beatsTrump c1 c2
  -- one of the cards is not a trump
  | isTrump c2 = True
  | isTrump c1 = False
  -- none of the cards is a trump
  | otherwise = beatsNotrump c1 c2



------------------------------------------------------------
-- Players and Hands
------------------------------------------------------------
type Player = Int
------------------------------------------------------------

-- Cards of each Player - a list containing 4 list of Cards
type Hand = [Card]

-- deal produces (four) lists of 12 Cards from a random seed
type Seed = Int
deal :: Seed -> [Hand]
deal seed = chunksOf 12 sortedCards
  where
    -- assign each card in the deck a random number and sort by it
    position = randomRs (1::Int,1000) (mkStdGen seed)
    -- after sorting (position, card) take the "second" to obtain the card only
    sortedCards = map snd $ sortBy (compare `on` fst) (zip position deck)


------------------------------------------------------------
-- Events 
------------------------------------------------------------
data Move = Play Card | Announce Announcement | Comment String deriving (Eq,Show)
type Event = (Player, Move)
------------------------------------------------------------

-- test whether an event is a Card
isCard :: Event -> Bool
isCard (_, Play _) = True
isCard _ = False

-- test whether an event is an Annoucement and not Pass
isAnnouncement :: Event -> Bool
isAnnouncement (_, Announce a) = a /= Pass
isAnnouncement _ = False

-- get just the Cards-events from past events
cardEvents events =  filter isCard events

------------------------------------------------------------
-- Game
------------------------------------------------------------
-- The game status is fully described by the following attributes
data Game = Game {
            starter :: Player, 
            hands   :: [Hand], 
            events  :: [Event]
        } deriving (Eq, Show)
------------------------------------------------------------

-- return True when the game is over
gameOver :: Game -> Bool
gameOver game = (length $ cardEvents $ events game) == 48

-- append some events to a game
appendEvts :: Game -> [Event] -> Game
appendEvts game evts = game {events = (events game) ++ evts}

------------------------------------------------------------
-- Game - Players and Rounds
------------------------------------------------------------

-- return events of game grouped in rounds
rounds :: Game -> [[Event]]
rounds game = map (map snd) $ groupBy (\x y -> round x == round y) (rounds' evts (-1))
  where
      evts = events game
      round (n,e) = n `div`4
      rounds' [] r = []
      rounds' (e:es) r
              | isCard e = (r+1,e): rounds' es (r+1)
              | otherwise = (r,e): rounds' es (r)

-- a round is complete where n*4 cards are played (n = 1,2...)
roundComplete :: Game -> Bool
roundComplete game = (nCardEvts > 0) && (nCardEvts `mod` 4 == 0)
        where
            nCardEvts = length $ cardEvents $ events game

-- Return the card events of the last round
lastRoundEvts :: Game -> [Event]
lastRoundEvts game = cardEvents $ last $ rounds game


-- get the player to play the next card in turn
nextPlayerOnTable :: Game -> Player
nextPlayerOnTable game 
        | cardEvts == [] = starter game
        | otherwise = ((+ 1) $  fst $ last cardEvts) `mod` 4
        where
            cardEvts = cardEvents $ events game

-- Answer if the next player is in 1st, 2nd, 3rd or 4th position
playerPosition :: Game -> Int
playerPosition game 
        | rsg == [] = 0
        | otherwise = length $ cardEvents $ last rsg
        where
            rsg = rounds game

------------------------------------------------------------
-- Game - Cards
------------------------------------------------------------

-- Get the cards played by a given player
cardsPlayedBy :: Game -> Player -> [Card]
cardsPlayedBy game p = playedCards
        where
            playedCards = map getCard $ filter byHim $ cardEvents $ events game
            byHim (px, _) = px == p
            getCard (_,(Play c)) = c
            
-- Get the cards remaining in a player's hand
currentHand:: Game -> Player -> [Card]
currentHand game p = foldr delete (hands game !! p) playedCards
        where
            playedCards = cardsPlayedBy game p

-- Return the first card played in the current round
firstRoundCard :: Game -> Card
firstRoundCard game = card
        where
            (_,Play card) = head $ lastRoundEvts game
        

-- From a list of Card Events return the winning Event
winningEvent :: [Event] -> Event
winningEvent evts = foldl best (head evts) (tail evts)
        where
            best :: Event -> Event -> Event
            best (p1, Play c1) (p2, Play c2)
                    | beats c1 c2 = (p2, Play c2)
                    | otherwise = (p1, Play c1)


------------------------------------------------------------
-- Playing
------------------------------------------------------------

-- decide what move to make (play a card, say Contra ...) dummy
decideCard :: Game -> Player -> Move
decideCard game p 
        | playerPosition game == 0 = Play (head hisCards)
        | hisSuitCards == []       = Play (head hisCards)
        | otherwise                = Play (head hisSuitCards)
        where
            hisCards = currentHand game p
            suit = fst $ firstRoundCard game
            hisSuitCards = ofSuit suit hisCards



-- keep on playing until the end
continueGame :: Game -> Game
continueGame game
        | gameOver game      = game
        | roundComplete game = cont [(wp, Comment "--wins--"), (wp, decideCard game wp)]
        | otherwise          = cont [(np, decideCard game np)]
        where
            evts = events game
            -- winning player or next player
            wp = fst $ winningEvent $ lastRoundEvts game 
            np = nextPlayerOnTable game 
            -- append events to game and contiune
            cont  = continueGame . (appendEvts game )

-- deal, set the firstPlayer and play the game
startGame :: Int -> Player -> Game
startGame seed firstPlayer = continueGame game
  where
    game = Game {starter = firstPlayer, hands = deal seed, events=[]}

            
showEvents :: Game -> IO()
showEvents game = out $ events game
        where
            out [] = return ()
            out evts = do
                putStrLn (show $ head evts)
                out (tail evts)


------------------------------------------------------------
-- Test data
------------------------------------------------------------

testGame = startGame 1 0
testDeck = hands testGame !! 0
