module Solitaire where

  import System.Random
  import Data.List
  import Data.List.Split

  --data structures
  data Suit = Spades | Hearts | Diamonds | Clubs deriving (Enum, Show, Eq, Ord)
  data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten 
             | Jack | Queen | King deriving (Enum, Show, Eq, Ord)
  type Card = (Suit, Pip)
  type Deck = [Card] 
  data EOBoard = EOBoard Foundations Columns Reserves deriving (Show, Eq)
  type Foundations = [Deck]
  type Columns = [Deck]
  type Reserves = [Deck]
 
  --returns a list of cards
  pack :: Deck 
  pack = [(x,y) | x <- [Spades .. Clubs], y <- [Ace .. King]]

  --returns a successor of a given card
  sCard :: Card -> Maybe Card 
  sCard (s,p) 
    |p == King = Nothing
    |otherwise = Just (s, succ p)

  --returns a predecessor of a given card
  pCard :: Card -> Maybe Card
  pCard (s,p)
    |p == Ace = Nothing
    |otherwise = Just (s, pred p)

  --returns true if given card is ace
  isAce :: Card -> Bool
  isAce (s,p) = (p == Ace)

  --returns true if given card is king
  isKing :: Card -> Bool
  isKing (s,p) = (p == King)
    
  shuffledDeck :: Int -> EOBoard
  shuffledDeck seed = EOBoard foundations columns reserve
    where shuffle = map snd (sort (zip (take 52 (randoms (mkStdGen seed) :: [Int])) pack))
          foundations = [[],[],[],[]]
          columns = chunksOf 6 (take 48 shuffle)
          reserve = chunksOf 1 (drop 48 shuffle) ++ [[],[],[],[]]

  --gets the last card of the first Deck from a given [Deck]
  getCard :: [Card] -> Maybe Card
  getCard deck
    |null deck = Nothing
    |otherwise = Just (last deck)
 
  --returns the result of a maybe function without the word Just
  resMaybe :: (Maybe a) -> a
  resMaybe (Just x) = x

  --returns True if maybe function returned something and False 
  --if the function returned Nothing
  isJust :: (Maybe a) -> Bool
  isJust (Just a) = True
  isJust Nothing = False

  --addCardToF will only be given the card that was already checked and can be added
  --takes foundations deck and a card to be added and returns foundations with that card added
  addCardToF :: [Deck] -> Card -> [Deck]
  addCardToF deck card
    |isAce card && (null h) = ([card]:t)
    |isAce card && not(null h) = (h:addCardToF t card)
    |resMaybe(pCard card) == resMaybe(getCard h) = ([card]:t)
    |otherwise = (h:addCardToF t card)
    where (h:t) = deck

  --addCardToC will only be given the card that was already checked and can be added
  --takes columns deck and a card to be added and returns columns with that card added
  addCardToC :: [Deck] -> Card -> [Deck]
  addCardToC deck card
    |isKing card && null h = ([card]:t)
    |isKing card && not(null h) = (h:addCardToC t card)
    |not(null h) && (resMaybe(sCard card) == resMaybe(getCard h)) = ((h++[card]):t)
    |otherwise = (h:addCardToC t card)
    where (h:t) = deck

  --addCardToR will only be given the card that was already checked and can be added
  --takes reserves deck and a card to be added and returns reserves with that card added
  addCardToR :: [Deck] -> Card -> [Deck]
  addCardToR deck card
    |null h = ([card]:t)
    |otherwise = (h:addCardToR t card)
    where (h:t) = deck
 
  --deleteCard will only be given the card that was already checked and can be added
  --deletes the given card from columns or reserve
  deleteCard :: [Deck] -> Card -> [Deck]
  deleteCard deck card 
    |isJust c && (resMaybe c == card) = ((init h):t) 
    |not (isJust c) || (resMaybe c /= card) = (h:deleteCard t card)
    where c = getCard h
          (h:t) = deck

  --given a deck will return the first found ace from that deck
  searchForAce :: [Deck] -> Maybe Card
  searchForAce deck
    |null deck = Nothing --no aces in the given deck
    |isJust card && isAce(resMaybe card) = Just (resMaybe card)
    |not (isJust card) || not (isAce(resMaybe card)) = searchForAce t
    where (h:t) = deck
          card = getCard h

  --given reserves deck will return the first found king from that deck
  searchForReserveKing :: [Deck] -> Maybe Card
  searchForReserveKing deck
    |null deck = Nothing
    |isJust card && isKing(resMaybe card) = Just (resMaybe card)
    |not (isJust card) || not (isKing(resMaybe card)) = searchForReserveKing t
    where (h:t) = deck
          card = getCard h
  
  --given columns deck will return the first found king from that deck
  searchForColumnKing :: [Deck] -> Maybe Card
  searchForColumnKing deck
    |null deck = Nothing
    |length h /= 1 && isJust card && isKing(resMaybe card) = Just (resMaybe card)
    |otherwise = searchForColumnKing t
    where (h:t) = deck
          card = getCard h

  --given a card and a deck returns the card's successor if it exists in the given deck
  searchForCardSucc :: Card -> [Deck] -> Maybe Card
  searchForCardSucc card deck
    |not (isJust succ_card) || null deck = Nothing --given card has no successors
    |isJust succ_card && isJust comp_card && (resMaybe succ_card==resMaybe comp_card) = Just (resMaybe succ_card)
    |otherwise = searchForCardSucc card t
    where succ_card = sCard card
          comp_card = getCard h
          (h:t) = deck

  --given two decks will search for successors of the first deck's last cards
  searchForSuccs :: [Deck] -> [Deck] -> Maybe Card
  searchForSuccs deck1 deck2
    |null deck1 = Nothing --first deck's last cards have no successors
    |isJust card && isJust suc_card = Just (resMaybe suc_card)
    |otherwise = searchForSuccs t deck2
    where (h:t) = deck1
          card = getCard h
          suc_card = searchForCardSucc (resMaybe card) deck2

  --given a card and a deck returns the card's predecessor if it exists in the given deck
  searchForCardPred :: Card -> [Deck] -> Maybe Card
  searchForCardPred card deck
    |not (isJust pred_card) || null deck = Nothing --given card has no predecessors
    |isJust pred_card && isJust comp_card && (resMaybe pred_card==resMaybe comp_card) = Just (resMaybe pred_card)
    |otherwise = searchForCardPred card t
    where pred_card = pCard card
          comp_card = getCard h
          (h:t) = deck
  
  --given a shuffled board will return it after making all possible moves to foundations
  toFoundations:: EOBoard -> EOBoard
  toFoundations (EOBoard f c r) 
    |isJust ace_c = toFoundations (EOBoard (addCardToF f (resMaybe ace_c)) (deleteCard c (resMaybe ace_c)) r)
    |isJust ace_r = toFoundations (EOBoard (addCardToF f (resMaybe ace_r)) c (deleteCard r (resMaybe ace_r)))
    |isJust succ_c = toFoundations (EOBoard (addCardToF f (resMaybe succ_c)) (deleteCard c (resMaybe succ_c)) r)
    |isJust succ_r = toFoundations (EOBoard (addCardToF f (resMaybe succ_r)) c (deleteCard r (resMaybe succ_r)))
    |otherwise = (EOBoard f c r) 
    where ace_c = searchForAce c
          ace_r = searchForAce r
          succ_c = searchForSuccs f c
          succ_r = searchForSuccs f r

  --returns true if there exists at least one empty space in the given deck
  emptySpace :: [Deck] -> Bool
  emptySpace deck = not(null(filter (\n -> null n) deck))

  --function for searching column to column moves
  columnToColumn :: [Deck] -> [Deck] -> EOBoard -> [EOBoard]
  columnToColumn deck1 deck2 (EOBoard f c r)
    |null deck1 = []
    |isJust card && isJust pred_card = newBoard : (columnToColumn t deck2 (EOBoard f c r)) --if there is a card in the column and if it has a predecessor that can be moved
    |otherwise = columnToColumn t deck2 (EOBoard f c r)
    where (h:t) = deck1
          card = getCard h
          pred_card = searchForCardPred (resMaybe card) deck2
          newBoard = (EOBoard f (addCardToC (deleteCard c (resMaybe pred_card)) (resMaybe pred_card)) r)    

  --function for searching reserve to column moves
  reserveToColumn :: [Deck] -> [Deck] -> EOBoard -> [EOBoard]
  reserveToColumn deck1 deck2 (EOBoard f c r)
    |null deck1 = []
    |isJust card && isJust pred_card = newBoard : (reserveToColumn t deck2 (EOBoard f c r)) --if there is a card in the column and if it has a predecessor that can be moved
    |otherwise = reserveToColumn t deck2 (EOBoard f c r)
    where (h:t) = deck1
          card = getCard h
          pred_card = searchForCardPred (resMaybe card) deck2
          newBoard = (EOBoard f (addCardToC c (resMaybe pred_card)) (deleteCard r (resMaybe pred_card)))

  --function for searching column King to column moves
  columnKingToColumn :: [Deck] -> EOBoard -> [EOBoard]
  columnKingToColumn deck (EOBoard f c r)
    |isJust king && emptySpace c = newBoard : (columnKingToColumn t (EOBoard f c r)) --if king exists in columns and there is at least one empty space in columns
    |otherwise = []
    where (h:t) = deck
          king = searchForColumnKing deck
          newBoard = (EOBoard f (addCardToC (deleteCard c (resMaybe king)) (resMaybe king)) r)

  --function for searching reserve King to column moves
  reserveKingToColumn :: EOBoard -> [EOBoard]
  reserveKingToColumn (EOBoard f c r)
    |isJust king && emptySpace c = (newBoard:(reserveKingToColumn newBoard)) ----if king exists in reserves and there is at least one empty space in columns
    |otherwise = []
    where king = searchForReserveKing r
          newBoard = (EOBoard f (addCardToC c (resMaybe king)) (deleteCard r (resMaybe king)))
 
  --function for searching column to reserve moves
  columnToReserve :: [Deck] -> EOBoard -> [EOBoard]
  columnToReserve deck (EOBoard f c r)
    |null deck = []
    |isJust card && not(isKing(resMaybe card)) && emptySpace r && not test = newBoard : (columnToReserve t (EOBoard f c r)) --if there is a card in the column and it is not king
    |otherwise = columnToReserve t (EOBoard f c r)                                                                          --and there is an empty space in reserves and sequence of 
    where (h:t) = deck                                                                                                      --cards is longer than 1
          card = getCard h                                                                                                  
          cardSeq = findSequence h
          test = length cardSeq > 1
          newBoard = (EOBoard f (deleteCard c (resMaybe card)) (addCardToR r (resMaybe card)))

  --function for searching column sequence to reserve moves
  columnSeqToReserve :: [Deck] -> EOBoard -> [EOBoard]
  columnSeqToReserve deck (EOBoard f c r)
    |null deck = []
    |not(null h) && length cardSeq > 1 && length cardSeq <= reserveEmptySpaces && not test = move : columnSeqToReserve t (EOBoard f c r) --if columns head is not null and sequence of cards is 
    |otherwise = columnSeqToReserve t (EOBoard f c r)                                                                                    --longer than 1 and length of sequence of cards is shorter
    where (h:t) = deck                                                                                                                   --than the length of empty spaces in reserve and if King is not
          cardSeq = findSequence h                                                                                                       --the first card of the sequence and the column
          reserveEmptySpaces = length (filter (\n -> null n) r)
          newColumns = deleteSequence cardSeq c
          newReserve = moveSequence cardSeq r
          test = isKing (head h) && isKing (last cardSeq) && head h == last cardSeq
          move = EOBoard f newColumns newReserve

  --finds a sequence of cards if it exists in the given deck
  findSequence :: [Card] -> [Card]
  findSequence column
    |length column == 1 = [card1]
    |isJust(sCard card1) && resMaybe(sCard card1) == card2 = ([card1]) ++ findSequence (init column)
    |otherwise = [card1] 
    where card1 = last column
          card2 = last (init column)

  --moves a sequence of cards to the given reserve
  moveSequence :: [Card] -> [Deck] -> [Deck]
  moveSequence (h:t) deck
    |null t  = newReserve
    |otherwise = moveSequence t newReserve
    where newReserve = addCardToR deck h

  --deletes a sequence of cards from the given columns deck
  deleteSequence :: [Card] -> [Deck] -> [Deck]
  deleteSequence cards deck
    |null (tail cards) = newColumns
    |otherwise = deleteSequence (tail cards) newColumns
    where newColumns = deleteCard deck (head cards)

  --finds all possible moves from the given board
  findMoves :: EOBoard -> [EOBoard]
  findMoves (EOBoard f c r) = moves
    where moves = (columnKingToColumn c (EOBoard f c r))++(reserveKingToColumn (EOBoard f c r))
                ++(columnToColumn c c (EOBoard f c r))++(reserveToColumn c r (EOBoard f c r))
                ++(columnSeqToReserve c (EOBoard f c r)) ++(columnToReserve c (EOBoard f c r)) 

  --chooses one move from a list of moves and then performs toFoundations
  chooseMove :: EOBoard -> Maybe EOBoard
  chooseMove board 
    |not(null kingMoves) = Just (toFoundations(bestMove kingMoves))
    |not(null cToCMoves) = Just (toFoundations(bestMove cToCMoves))
    |not(null rToCMoves) = Just (toFoundations(bestMove rToCMoves))
    |not(null cToRMoves) = Just (toFoundations(bestMove cToRMoves))
    |otherwise = Nothing
    where autoPlayedBoard = toFoundations board
          allMoves = findMoves autoPlayedBoard
          kingMoves = filter (\n -> (typeOfMove autoPlayedBoard n)=="KingToC") allMoves
          cToCMoves = filter (\n -> (typeOfMove autoPlayedBoard n)=="CToC") allMoves
          rToCMoves = filter (\n -> (typeOfMove autoPlayedBoard n)=="RToC") allMoves
          cToRMoves = filter (\n -> (typeOfMove autoPlayedBoard n)=="CToR") allMoves
  
  --returns the type of move    
  typeOfMove :: EOBoard -> EOBoard -> String
  typeOfMove (EOBoard f1 c1 r1) (EOBoard f2 c2 r2)
    |length(filter (\n -> null n) c1)>length(filter (\n -> null n) c2) = "KingToC" --if first columns have more empty spaces than second it's king move
    |(foldr (+) 0 (map length r1))==(foldr (+) 0 (map length r2)) = "CToC" --if first reserves have the same number of cards as second it's columns to columns move
    |(foldr (+) 0 (map length r1))>(foldr (+) 0 (map length r2)) = "RToC" --if first reserves have more card than the second it's reserves to columns move
    |(foldr (+) 0 (map length c1))>(foldr (+) 0 (map length c2)) = "CToR" --if first columns have more cards than second it's columns to reserve move
    |otherwise = "AnotherMove" 

  --finds the best move
  bestMove :: [EOBoard] -> EOBoard
  bestMove moves 
    |moves/=afterToFoundations = head (moves \\ afterToFoundations) --selects the move after which a card can be moved to foundations
    |not(null(concat afterColumnToColumn)) = moves !! (resMaybe index1) --selects the move after which a card can be moved from columns to columns
    |not(null(concat afterReserveToColumn)) = moves !! (resMaybe index2) --selects the move after which a card can be moved from reserve to columns
    |otherwise = head moves
    where (h:t) = moves
          afterToFoundations = map (\n -> toFoundations n) moves --does toFoundations to every given move
          afterColumnToColumn = map (\(EOBoard f c r) -> columnToColumn c c (EOBoard f c r)) moves --does columnToColumn to every given move
          index1 = maximum (map length afterColumnToColumn) `elemIndex` (map length afterColumnToColumn) --finds the index of the move which would open the most columnToColumn moves 
          afterReserveToColumn = map (\(EOBoard f c r) -> reserveToColumn c r (EOBoard f c r)) moves ----does reserveToColumn to every given move
          index2 = maximum (map length afterReserveToColumn) `elemIndex` (map length afterReserveToColumn) --finds the index of the move which would open the most reserveToColumn moves 

  --displays the score of one game
  eOGame :: EOBoard -> Int
  eOGame board
    |isWin = score
    |isJust move = eOGame (resMaybe move)
    |otherwise = score
    where (EOBoard f c r) = board
          isWin = ((length (filter (\n -> null n) c)==8)&&(length (filter (\n -> null n) r)==8)) --checks if columns and reserve are empty
          score = (52 - (foldr (+) 0 (map length r)) - (foldr (+) 0 (map length c)))
          move = chooseMove board

  --displays the number of wins and the average score over 100 games
  eOExpt :: Int -> (Int,Float)
  eOExpt initialSeed = (wins,averageScore)
    where seeds = map (\n -> abs n) (take 100 (randoms (mkStdGen initialSeed) :: [Int])) --generates 100 random positive numbers
          boards = map (\n -> shuffledDeck n) seeds --creates a board with every number from 'seeds'
          scores = map (\n -> eOGame n) boards --plays 100 games
          averageScore = fromIntegral (foldr (+) 0 scores) / fromIntegral 100
          wins = length (filter (==52) scores)