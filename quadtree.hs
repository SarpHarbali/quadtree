data QuadTree = AllBlack | AllWhite | Node QuadTree QuadTree QuadTree QuadTree -- recursive definition
 deriving (Eq, Show)

allBlack :: Int -> QuadTree
allBlack _ = AllBlack -- returns the same regardless of the value of int

allWhite :: Int -> QuadTree
allWhite _ = AllWhite

clockwise :: QuadTree -> QuadTree -> QuadTree -> QuadTree -> QuadTree
clockwise a b c d = Node a b c d

anticlockwise :: QuadTree -> QuadTree -> QuadTree -> QuadTree -> QuadTree
anticlockwise a b c d = Node a d c b -- opposite way


left :: Maybe QuadTree -> (Int, Int) -- counting the left neighbors of adjacent quadtree according to color, returns tuple of ints
left Nothing = (0, 0) -- nothing means no black or white neighbors
left (Just AllBlack) = (1, 0) -- first index is black
left (Just AllWhite) = (0, 1) -- second index is white
left (Just (Node a b c d)) = -- recursive case 
  let (blackCountB, whiteCountB) = left (Just b) -- since these are left neighbors from adjacent tree, we need to get both the upper and the lower
      (blackCountC, whiteCountC) = left (Just c) -- this is bottom right of left neighbour
  in (blackCountB + blackCountC, whiteCountB + whiteCountC) -- sum up the colour counts


right :: Maybe QuadTree -> (Int, Int)
right Nothing = (0, 0)
right (Just AllBlack) = (1, 0)
right (Just AllWhite) = (0, 1)
right (Just (Node a b c d)) = 
  let (blackCountA, whiteCountA) = right (Just a) -- top left of right neighbour
      (blackCountD, whiteCountD) = right (Just d) -- bottom left of right neighbour
  in (blackCountA + blackCountD, whiteCountA + whiteCountD)

top :: Maybe QuadTree -> (Int, Int)
top Nothing = (0, 0)
top (Just AllBlack) = (1, 0)
top (Just AllWhite) = (0, 1)
top (Just (Node a b c d)) = 
  let (blackCountC, whiteCountC) = top (Just c) -- bottom right of top neighbour
      (blackCountD, whiteCountD) = top (Just d) -- bottom right of top neighbour
  in (blackCountC + blackCountD, whiteCountC + whiteCountD)

bottom :: Maybe QuadTree -> (Int, Int)
bottom Nothing = (0, 0)
bottom (Just AllBlack) = (1, 0)
bottom (Just AllWhite) = (0, 1)
bottom (Just (Node a b c d)) = 
  let (blackCountA, whiteCountA) = bottom (Just a) -- top left of bottom neighbour
      (blackCountB, whiteCountB) = bottom (Just b) -- top right of bottom neighbour
  in (blackCountA + blackCountB, whiteCountA + whiteCountB)

nw :: Maybe QuadTree -> Maybe QuadTree -- getting the quadrants
nw Nothing = Nothing -- case for nothing
nw (Just (Node a b c d)) = Just a -- a is the nw (top left) node
nw (Just a) = Just a -- if single node is inputted return itself

ne :: Maybe QuadTree -> Maybe QuadTree
ne Nothing = Nothing
ne (Just (Node a b c d)) = Just b -- b is the top right (ne)
ne (Just a) = Just a

sw :: Maybe QuadTree -> Maybe QuadTree
sw Nothing = Nothing
sw (Just (Node a b c d)) = Just d -- d is the bottom left (sw)
sw (Just a) = Just a

se :: Maybe QuadTree -> Maybe QuadTree
se Nothing = Nothing
se (Just (Node a b c d)) = Just c -- c is the bottom right
se (Just a) = Just a




totalColors :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -- takes in 4 tuples of ints and returns a tuple of ints
totalColors (bl, wl) (br, wr) (bt, wt) (bb, wb) = (bl+br+bt+bb, wl+wr+wt+wb) -- counts the total white and black neighbors of subtrees



find :: Maybe QuadTree -> Maybe QuadTree -> Maybe QuadTree -> Maybe QuadTree -> QuadTree -> QuadTree -- inputs are maybes since it can be a neighbor or a nothing
find leftNeighbor rightNeighbor topNeighbor bottomNeighbor tree = case tree of
  AllBlack -> if whiteCount > blackCount then AllWhite else AllBlack -- base case for counting
  AllWhite -> if blackCount > whiteCount then AllBlack else AllWhite
  Node a b c d -> Node (find (ne leftNeighbor) (Just b) (sw topNeighbor) (Just d) a) -- recursive call depending on the position of the quadtree
                       (find (Just a) (nw rightNeighbor) (se topNeighbor) (Just c) b)
                       (find (Just d) (sw rightNeighbor) (Just b) (ne bottomNeighbor) c)
                       (find (se leftNeighbor) (Just c) (Just a) (nw bottomNeighbor) d)
  where
    (blackCount, whiteCount) = totalColors (left leftNeighbor) (right rightNeighbor) (top topNeighbor) (bottom bottomNeighbor) -- step case for counting


blur :: QuadTree -> QuadTree -- it takes in a quadtree and returns a quadtree
blur AllWhite= AllWhite -- base case
blur AllBlack = AllBlack -- base case
blur (Node a b c d) = 
  Node (find Nothing (Just b) Nothing (Just d) a) (find (Just a) Nothing Nothing (Just c) b)
    (find (Just d) Nothing (Just b) Nothing c) (find Nothing (Just c) (Just a) Nothing d) -- input the outer neighbors of each subtree

