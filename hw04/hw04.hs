import SedmaDatatypes

--David Herel
--Implementation of sedma game - hw04


--main function
replay :: Cards -> Maybe Winner
replay cards = handleGame cards

--funciton that handles it
--if not valid return nothing
--if valid lets play game
handleGame :: Cards -> Maybe Winner
handleGame cards | checkValidity cards == 0 = Nothing
            --     | otherwise = Nothing
                 | otherwise = playGame cards AC 0 0 0 0

--checkValidity
--if it is valid return 1
--if not return 0
checkValidity :: Cards -> Int
checkValidity cards | (length cards) == 32 = 1
                    | otherwise = 0

--Takes 4 cards from all cards and put them to play 1 round
--when no cards left -> add 10 points to last winner and evaluate results by points
-- cards->if AC is on turn -> AC points -> BD points -> Ac atleast 1 card and not ten or ace -> BD atleast 1 card same as AC 
playGame :: Cards -> Team -> Int -> Int -> Int -> Int -> Maybe Winner
playGame [] turn acPoints bdPoints acNormCard bdNormCard | (turn == AC) = chooseWinner (acPoints+10) bdPoints acNormCard bdNormCard
                                                         | (turn == BD) = chooseWinner acPoints (bdPoints+10) acNormCard bdNormCard
playGame cards turn acPoints bdPoints acNormCard bdNormCard = playGame restCards winTurn acP bdP acNormC bdNormC 
            where (fourCards, restCards) = splitAt 4 cards
                  (winTurn, acP, bdP, acNormC, bdNormC) = playRound fourCards restCards (findFirst cards) turn acPoints bdPoints acNormCard bdNormCard turn 0 0 
                    
                  
findFirst :: Cards -> Rank
findFirst ((Card suit rank):xs) = rank

changeTurn :: Team -> Team
changeTurn turn | turn == AC = BD
                | turn == BD = AC


--playRound with 4 cards
--takes one and look if it is first one, or R10, RA or R7, if R10 OR RA gives 10 points
playRound :: Cards -> Cards -> Rank -> Team -> Int -> Int -> Int -> Int -> Team -> Int -> Int -> (Team, Int, Int, Int, Int) 
playRound [] restCards currRank turn acPoints bdPoints acNormCard bdNormCard winTurn points normCard | winTurn == AC = (winTurn, (acPoints + points), bdPoints, (acNormCard+4), bdNormCard)
                                                                                                     | winTurn == BD = (winTurn, acPoints, (bdPoints+ points), acNormCard, (4 + bdNormCard))
playRound ((Card suit rank):xs) restCards currRank turn acPoints bdPoints acNormCard bdNormCard winTurn points normCard 
                                                                                                              | (rank == currRank && rank == R10)= playRound xs restCards currRank (changeTurn turn) acPoints bdPoints acNormCard bdNormCard turn (points+10) (normCard+1)
                                                                                                              | (rank == currRank && rank == RA) = playRound xs restCards currRank (changeTurn turn) acPoints bdPoints acNormCard bdNormCard turn (points+10) (normCard+1)
                                                                                                              | rank == currRank = playRound xs restCards currRank (changeTurn turn) acPoints bdPoints acNormCard bdNormCard turn points (normCard+1)
                                                                                                              | rank == R10 = playRound xs restCards currRank (changeTurn turn) acPoints bdPoints acNormCard bdNormCard winTurn (points+10) (normCard+1)
                                                                                                              | rank == RA = playRound xs restCards currRank (changeTurn turn) acPoints bdPoints acNormCard bdNormCard winTurn (points+10) (normCard+1)
                                                                                                              | rank == R7 = playRound xs restCards currRank (changeTurn turn) acPoints bdPoints acNormCard bdNormCard turn points (normCard+1)
                                                                                                              | otherwise = playRound xs restCards currRank (changeTurn turn) acPoints bdPoints acNormCard bdNormCard winTurn points (normCard+1)

--decide who is winner
chooseWinner :: Int -> Int -> Int -> Int -> Maybe Winner
chooseWinner acPoints bdPoints acNormCard bdNormCard | ((acPoints > bdPoints) && bdNormCard == 0 && bdPoints == 0) = Just (AC, Three)
                                                     | ((bdPoints > acPoints) && acNormCard == 0 && acPoints == 0) = Just (BD, Three)
                                                     | (acPoints > bdPoints && bdPoints == 0 && bdNormCard > 0) = Just (AC, Two)
                                                     | (bdPoints > acPoints && acPoints == 0 && acNormCard > 0) = Just (BD, Two)
                                                     | (acPoints > bdPoints) = Just (AC, One)
                                                     | (bdPoints > acPoints) = Just (BD, One)  
                                                     -- | otherwise = Nothing    
                                                     
instance Eq Rank
    where (==) R7 R7 = True
          (==) R8 R8 = True
          (==) R9 R9 = True
          (==) R10 R10 = True
          (==) RJ RJ = True
          (==) RQ RQ = True
          (==) RK RK = True
          (==) RA RA = True
          (==) _ _ = False                                                                                          
