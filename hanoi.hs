-- Implementation of the Towers of Hanoi with visualisation

type Tower = [Int]

type Towers = (Tower, Tower, Tower)

towerGen :: Int -> Towers
towerGen n = ([1 .. n], [], [])

-- moves head of a tower of index `from` to the tower of index `to`
move :: Int -> Int -> Towers -> Towers
move from to current =
  let currList = (\(a, b, c) -> [a, b, c]) current

      src = currList !! from
      dst = currList !! to

      dstOut = head src : dst
      srcOut = tail src

      decideOut i
        | from == i = srcOut
        | to == i = dstOut
        | otherwise = currList !! i
   in (decideOut 0, decideOut 1, decideOut 2)

-- hanoi implementation
hanoi' :: Int -> Int -> Int -> Towers -> [Towers]
hanoi' from to depth current
  | depth == 1 = [move from to current]
  | otherwise =
    let store = head [i | i <- [0 .. 2], i /= from, i /= to]

        movesFromStore = hanoi' from store (depth -1) current
        moveFromTo = move from to (last movesFromStore)
        movesStoreTo = hanoi' store to (depth -1) moveFromTo
     in movesFromStore ++ moveFromTo : movesStoreTo

-- hanoi wrapper
hanoi :: Int -> [Towers]
hanoi n = let towers = towerGen n in towers : hanoi' 0 2 n towers

drawTowers :: Towers -> String
drawTowers towers@(a, b, c) =
  let height = maximum [length a, length b, length c]

      drawDisc :: Tower -> Int -> String
      drawDisc a i =
        let n = i - (height - length a)
            width
              | null a = 1
              | otherwise = maximum a + 1
            space = replicate (width - discLen) ' '
            discLen
              | n < 0 = 0
              | n >= length a = 0
              | otherwise = a !! n
            disc = replicate discLen '▆'
         in space ++ disc ++ "▒" ++ disc ++ space

      drawLine :: Int -> String
      drawLine n = drawDisc a n ++ drawDisc b n ++ drawDisc c n ++ "\n"
   in concatMap drawLine [0 .. height -1] ++ "\n"

main = putStr $ concatMap drawTowers $ hanoi 5