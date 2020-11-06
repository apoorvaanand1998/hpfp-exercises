data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | otherwise = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 5 Leaf) 3 (Node Leaf 6 Leaf)

mapOkay =
  if mapTree (+2) testTree' == mapExpected
  then print "yep okayyyy!"
  else error "dude wtf!"

preOrder :: BinaryTree a -> [a]
preOrder Leaf = []
preOrder (Node left x right) = [x] ++ preOrder left ++ preOrder right
-- can also be x : (preOrder left ++ preOrder right)
-- left it as is for consistency
inOrder :: BinaryTree a -> [a]
inOrder Leaf = []
inOrder (Node left x right) = preOrder left ++ [x] ++ preOrder right

postOrder :: BinaryTree a -> [a]
postOrder Leaf = []
postOrder (Node left x right) = postOrder left ++ postOrder right ++ [x]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b (Node left a right) = f a (foldTree f (foldTree f b left) right)
-- not exactly sure about this, but the exercise says write _"a"_ catamorphism
-- so I guess this counts? At least it type checks :)
