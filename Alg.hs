module Alg where

  import DataStructures
  import Debug.Trace
  import Data.List (sortBy)
  import Data.Function (on)
  
  
  --funkcja zwracająca położenie wilka
  wolfPos :: State -> Point
  wolfPos (x:xs) = x
  
  --funkcja zwracająca położenie owiec
  sheepsPos :: State -> [Point]
  sheepsPos (x:xs) = xs
  
  -- operator porównania dla punktów
  instance Eq Point where
                  (Point x1 y1) == (Point x2 y2) = (x1 == x2) && (y1 == y2) 
  
  -- czy można się przesunąć na daną pozycję
  isMovePossible :: State -> Point -> Bool
  isMovePossible state pos = if x >= 0 && y >= 0 && x <=7 && y <=7 
                then not (elem pos state)
                else False
                where 
                  x = xPoint pos
                  y = yPoint pos
                  
  -- na podstawie aktualnego stanu gry zwraca listę pozycji, 
  -- w których może znaleźć się wilk w następnym ruchu
  possibleWolfMoves :: State -> [Point]
  possibleWolfMoves state = filter (isMovePossible state) [(Point x y) | x <- [xWolf + 1, xWolf - 1], y <- [yWolf + 1, yWolf - 1]]
                    where 
                      wPos = wolfPos state
                      xWolf = xPoint wPos
                      yWolf = yPoint wPos					
                  
  -- na podstawie aktualnego stanu wraca listę możliwych stanów po ruchu wilka
  possibleWolfStates :: State -> [State]
  possibleWolfStates state = [x:sPos | x <- possibleWolfMoves state]	
                    where sPos = sheepsPos state
                  
    
  -- na podstawie aktualnego stanu gry zwraca listę pozycji, 
  -- w których mogą znaleźć się owce w następnym ruchu	
  --possibleSheepsStatesWrapper :: State -> [State]
  --possibleSheepsStatesWrapper state = possibleSheepsStates (sheepsPos state) state
    
  -- na podstawie pozycji owiec aktualnego stanu gry zwraca listę pozycji, 
  -- w których mogą znaleźć się owce w następnym ruchu						
  possibleSheepsMoves :: [Point] -> State -> [State]	
  possibleSheepsMoves [] state = []						
  possibleSheepsMoves (s:ss) state = filter (isMovePossible state) [(Point x (ySheep + 1)) | x <- [xSheep - 1, xSheep + 1]] : possibleSheepsMoves ss state
                    where 
                      wPos = wolfPos state
                      xSheep = xPoint s
                      ySheep = yPoint s	
                      
  -- zwraca punkty do których może się ruszyc owca o podanym numerze
  possibleOneSheepMoves :: Int -> State -> [Point]
  possibleOneSheepMoves sheep state = (possibleSheepsMoves (sheepsPos state) state) !! sheep
                      
  -- na podstawie aktualnego stanu zwraca listę możliwych stanów po ruchu owiec
  --possibleSheepsStates :: [Point] -> State -> [State]
  --possibleSheepsStates points state = [wPos:a | a <- possibleSheepsMoves points state]	
  --									where 
  --										wPos = wolfPos state
      
  -- na podstawie aktualnego stanu gry zwraca możliwe stany gry po ruchavh owiec															
  sheepsStates :: State -> [State]
  sheepsStates (s:ss) = [[s]++[x]++[ss!!1] ++ [ss!!2] ++ [ss!!3] | x <- ((possibleSheepsMoves (s:ss) ss) !! 1)]
              ++ [[s]++[ss!!0]++[x] ++ [ss!!2] ++ [ss!!3] | x <- ((possibleSheepsMoves (s:ss) ss) !! 2)]								
              ++ [[s]++[ss!!0]++[ss!!1]++[x]++[ss!!3] | x <- ((possibleSheepsMoves (s:ss) ss) !! 3)]
              ++ [[s]++[ss!!0]++[ss!!1]++[ss!!2]++[x] | x <- ((possibleSheepsMoves (s:ss) ss) !! 4)]	
    
  --sheepsStates :: State -> [State]
  --sheepsStates (s:ss) = [[s]++[x]++[ss!!1] | x <- ((possibleSheepsMoves (s:ss) ss) !! 1)]
  --						++ [[s]++[ss!!0]++[x] | x <- ((possibleSheepsMoves (s:ss) ss) !! 2)]								
              
                      
            
  --czy wilk wygrywa przy danym stanie gry
  wolfWins :: State -> Bool
  wolfWins state = yWolf == 0 
            where
              yWolf = yPoint (wolfPos state)
            
  -- czy owce wygrywają przy daym stanie gry					
  sheepsWin :: State -> Bool
  sheepsWin state = not (wolfWins state) && (countWolfMoves state) == 0
          
  -- generuje drzewo stanów gry dla zadanej głębokości, z zacznaczeniem czyja to runda	
  generateGameTree :: Int -> Bool -> State -> GameTree
  generateGameTree 0 turn state = GameTree state [] (valueOfNode state) 
  generateGameTree depth isWolfTurn state | isWolfTurn = GameTree state (map (generateGameTree (depth-1) False) (possibleWolfStates state)) (valueOfNode state)
                      | otherwise = GameTree state (map (generateGameTree (depth-1) True) (sheepsStates state)) (valueOfNode state)
                  
  test :: Int -> State -> [GameTree]
  test depth state = map (generateGameTree (depth-1) True) (possibleWolfStates state)													
                  
  -- korzystając z algorytmu min-max wybiera optymalny ruch dla wilka
  minmax :: Bool -> GameTree -> (Int, State)
  minmax _ (GameTree state [] eval) = (eval, state)
  minmax True (GameTree state tree eval) = getMax (map (minmax False) tree) 
  minmax isMaxTurn (GameTree state tree eval) | isMaxTurn = (fst (getMax (map (minmax False) tree)), state) 	
                        | otherwise = (fst (getMin (map (minmax True) tree)), state)
    
    
  getMax :: (Num t, Ord t) => [(t, State)] -> (t, State)
  getMax tuples = sortBy (flip compare `on` fst) tuples !! 0
      
  getMin :: (Num t, Ord t) => [(t, State)] -> (t, State)
  getMin tuples = sortBy (compare `on` fst) tuples !! 0	
  
  -- mięsko całego algorytmu - zwraca najlepszy ruch i flagi wygranych dla graczy
  getBestMove :: State -> FinalState
  getBestMove state 
					| isMoreSheepPassed state = FinalState (snd (minmax True (generateGameTree 1 True state))) (wolfWins state) (sheepsWin state)
					| otherwise = FinalState (snd (minmax True (generateGameTree 3 True state))) (wolfWins state) (sheepsWin state)
    -- getBestMove state | (distanceWolfToEnd state) == 1 = FinalState ([x | x <- possibleWolfStates state, wolfWins x] !! 0) True False
    --       | isMoreSheepPassed state = FinalState (snd (minmax True (generateGameTree 1 True state))) (wolfWins state) (sheepsWin state)
    --       | otherwise = FinalState (snd (minmax True (generateGameTree 3 True state))) (wolfWins state) (sheepsWin state)
  -- funkcja heurystyczna do oceny poszczególnych stanów gry
  -- WAGI DO DOPASOWANIA EKSPERYMANTALNEGO
  valueOfNode ws =  750 * (distanceWolfToEnd ws) + 500 * (fromEnum(isMoreSheepPassed ws)) + 100 * (countWolfMoves ws) + 10000 * (fromEnum(wolfWins ws)) - 10000 * (fromEnum(sheepsWin ws))
  -- valueOfNode1111 ws = 1 * (distanceWolfToEnd ws) + 1 * (distanceWolfToSheep ws) + 1 * (fromEnum(isMoreSheepPassed ws)) + 1 * (countWolfMoves ws) + 1 * (fromEnum(wolfWins ws)) - 1 * (fromEnum(sheepsWin ws))
                
                  
  -- odległość wilka od końca plnaszy (od wygranej)								
  distanceWolfToEnd :: State -> Int								
  distanceWolfToEnd (w:ss) = 7 - (yPoint w)		
  
  --liczy sumę odlegosci, ale mozna przerobic na srednią jak potrzeba albo cos innego
  --Chebyshev distance - the best for cheess board
  distanceWolfToSheep :: State -> Int
  distanceWolfToSheep ws = func ws
      where func (w:s:ss)  = max (abs(yPoint s - yPoint w)) (abs(xPoint s - xPoint w)) + func (w:ss)
            func [w]  = 0
        
  countSheepPassedDebug :: Num t => State -> t						
  countSheepPassedDebug ws = func ws
      where func (w:s:ss) | (yPoint w <= yPoint s ) = 1 + (func (w:ss))
                          | otherwise = func (w:ss)
            func [w]  =  0
  
  countSheepPassed :: Num t => State -> t						
  countSheepPassed ws = func ws
      where func (w:s:ss) | (yPoint w <= yPoint s) = 1 + (func (w:ss))
                          | otherwise = func (w:ss)
            func [w]  = 0
  
  -- jeśli minął już 3 owce, to o ile się nie cofnie to już nie przegra
  isMoreSheepPassed :: State -> Bool
  isMoreSheepPassed ws | countSheepPassed ws >= 3 = True
                       | otherwise = False
  
  -- ile ruchów może wykonać wilk w danym momencie
  countWolfMoves :: State -> Int
  countWolfMoves state = length (possibleWolfMoves state)
          
          
  --funckja do ruszania owca/wilkiem
  movepPointToNPosition :: (Eq t1, Num t1) => t1 -> t -> [t] -> [t]
  movepPointToNPosition n nPoint (x:xs) | n == 0 = nPoint:xs
                                        | otherwise = x:movepPointToNPosition (n-1) nPoint xs
  
  --funckja do bezpiecznego ruszania owcą/wilkiem
  -- n - numer pozycji od 0 do 4, nPoint - nowy punkt z nowymi współrzędnymi, state- plansza przed ruchem, return - nowa tablica gry z wykonanym ruchem
  movepPointToNPositionSafe :: (Num t, Eq t) => t -> Point -> State -> [Point]
  movepPointToNPositionSafe n nPoint state | isMovePossible state nPoint ==True = movepPointToNPosition n nPoint state
                                           | otherwise = state
                        
              
                  
                  
  -- moje testy							
  p1 = Point 1 3
  p2 = Point 0 0
  p3 = Point 4 0 
  p4 = Point 2 2
  p5 = Point 6 0 
  p6 = Point 3 3
  s = [p1, p2, p6, p4, p3]
  
