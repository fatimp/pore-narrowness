{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}

module Data.BKTree (
  Metric(..),
  BKTree,
  Data.BKTree.empty,
  singleton,
  insert,
  fromList,
  elemsDistance,
  delete
  ) where
import qualified Control.Lens as L
import qualified Data.List as Ls

class Eq a => Metric a where
  distance :: a -> a -> Int

data BKTree a = Empty | Node { elt :: !a, vis :: !Bool, ch :: ![BKTree a] } deriving Eq

visibility :: L.Lens' (BKTree a) Bool
visibility f x = (\s -> Node (elt x) s (ch x)) <$> (f $ vis x)

children :: L.Lens' (BKTree a) [BKTree a]
children f x = (\s -> Node (elt x) (vis x) s) <$> (f $ ch x)

empty :: BKTree a
empty = Empty

singleton :: Metric a => a -> BKTree a
singleton x = Node x True []

item :: Eq a => a -> L.Lens' [a] a
item e f x = cons <$> f e where
  cons s = s:Ls.delete e x

-- TODO: Ignores visibility
insert :: Metric a => a -> BKTree a -> BKTree a
insert x Empty = singleton x
insert x tree = go id tree where
  go lens node = let dist = distance x $ elt node in
    if dist == 0 then tree else
      let p child = distance (elt child) (elt node) == dist
          childrenLens = lens . children in
        case Ls.find p (ch node) of
          Nothing    -> L.over childrenLens ((singleton x):) tree
          Just node' -> go (childrenLens . item node') node'

fromList :: Metric a => [a] -> BKTree a
fromList = Ls.foldl' (flip insert) Empty

elemsDistance :: Metric a => Int -> a -> BKTree a -> [a]
elemsDistance _ _ Empty = []
elemsDistance dist e tree = go [] tree where
  go acc node = let v = elt node
                    d = distance v e
                    acc' = if (vis node) && (d <= dist) then v `seq` v:acc else acc
                    go' acc'' child = if abs (d - distance v (elt child)) <= dist then
                      go acc'' child else acc''
                    in Ls.foldl' go' acc' (ch node)

delete :: Metric a => a -> BKTree a -> BKTree a
delete _ Empty = Empty
delete e tree = go id tree where
  go lens node = let v = elt node
                     d = distance v e in
                   if d == 0 then L.set (lens . visibility) False tree else
                     let p child = d == (distance v $ elt child) in
                       case Ls.find p (ch node) of
                         Nothing -> tree
                         Just child -> go (lens . children . item child) child
