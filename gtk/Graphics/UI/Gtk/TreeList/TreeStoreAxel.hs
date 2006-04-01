-- -*-haskell-*-
--  GIMP Toolkit (GTK) CustomStore TreeModel
--
--  Author : Duncan Coutts, Axel Simon
--
--  Created: 11 Feburary 2006
--
--  Version $Revision: 1.1 $ from $Date: 2005/12/08 18:12:43 $
--
--  Copyright (C) 2005 Duncan Coutts, Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Standard model to store hierarchical data.
--
module Graphics.UI.Gtk.TreeList.TreeStoreAxel (
  TreeStore,
  treeStoreNew,
  treeStoreInsert,
  treeStoreRemove,
  treeStoreChange,
  treeStoreChangeM,
  treeStoreGet
  ) where

import Data.Bits
import Data.Word (Word)
import Data.Maybe ( fromMaybe, isJust, fromJust )
import Data.Tree
import Control.Monad ( liftM, when )
import Control.Exception (assert)
import Data.IORef
import Data.Tree
import System.Glib.FFI ( CInt )
import Graphics.UI.Gtk.TreeList.Types
import Graphics.UI.Gtk.TreeList.TreeModel
import Graphics.UI.Gtk.Types (GObjectClass, TreeModelClass)
import Graphics.UI.Gtk.TreeList.TreePath (TreePath)
import Graphics.UI.Gtk.TreeList.CustomStore
import Graphics.UI.Gtk.TreeList.TreeIter

--------------------------------------------
-- internal model data types
--

-- | A store for hierarchical data.
--
newtype TreeStore a = TreeStore (CustomTreeModel (IORef (Store a)))

instance GObjectClass (TreeStore a)
instance TreeModelClass (TreeStore a)


-- | Maximum number of nodes on each level.
--
-- * These numbers determine how many bits in a 'TreeIter' are devoted to
--   each level. Hence, these numbers reflect log2 of the maximum number
--   of nodes at a level, rounded up.
--
type Depth = [Int]

data Store a = Store {
  depth :: Depth,
  content :: Cache a
}

instance TypedTreeModelClass TreeStore where
  treeModelGetRow (TreeStore store) iter =
    readIORef (customStoreGetPrivate store) >>=
      \Store { depth = d, content = cache } ->
    case checkSuccess d iter cache of
      (True, ((_, (Node { rootLabel = val }:_)):_)) -> return val
      _ -> error "TreeStore.getRow: iter does not refer to a valid entry"

-- | Create a new list store.
--
-- * The given rose tree determines the initial content and may be the empty
--   list. Each 'Tree' in the forest corresponds to one top-level node.
--
treeStoreNew :: Forest a -> IO (TreeStore a)
treeStoreNew forest = do
  storeRef <- newIORef Store {
      depth = calcForestDepth forest,
      content = storeToCache forest
    }
  let withStore f = readIORef storeRef >>= return . f
      withStoreUpdateCache f = do
        store <- readIORef storeRef
        let (result, cache') = f store
        writeIORef storeRef store { content = cache' }
        return result

  liftM TreeStore $ customStoreNew storeRef CustomStore {
    customStoreGetFlags = return [],

    customStoreGetIter = \path -> withStore $
      \Store { depth = d } -> fromPath d path,

    customStoreGetPath = \iter -> withStore $
      \Store { depth = d } -> toPath d iter,

    customStoreIterNext = \iter -> withStoreUpdateCache $
      \Store { depth = d, content = cache } -> iterNext d iter cache,

    customStoreIterChildren = \mIter -> withStoreUpdateCache $
      \Store { depth = d, content = cache } ->
      let iter = fromMaybe invalidIter mIter
       in iterNthChild d 0 iter cache,

    customStoreIterHasChild = \iter -> withStoreUpdateCache $
      \Store { depth = d, content = cache } ->
       let (mIter, cache') = iterNthChild d 0 iter cache
        in (isJust mIter, cache'),

    customStoreIterNChildren = \mIter -> withStoreUpdateCache $
      \Store { depth = d, content = cache } ->
      let iter = fromMaybe invalidIter mIter
       in iterNChildren d iter cache,

    customStoreIterNthChild = \mIter idx  -> withStoreUpdateCache $
      \Store { depth = d, content = cache } ->
      let iter = fromMaybe invalidIter mIter
       in iterNthChild d idx iter cache,

    customStoreIterParent = \iter -> withStore $
      \Store { depth = d } -> iterParent d iter,

    customStoreRefNode = \_ -> return (),
    customStoreUnrefNode = \_ -> return ()
   }

--------------------------------------------
-- low level bit-twiddling utility functions
--

-- TODO: figure out how these things work when Word is 64 bits

bitsNeeded :: Word -> Int
bitsNeeded n = bitsNeeded' 0 n
  where bitsNeeded' b 0 = b
        bitsNeeded' b n = bitsNeeded' (b+1) (n `shiftR` 1)

getBitSlice :: TreeIter -> Int -> Int -> Word
getBitSlice (TreeIter _ a b c) off count =
      getBitSliceWord a  off     count
  .|. getBitSliceWord b (off-32) count
  .|. getBitSliceWord c (off-64) count

  where getBitSliceWord :: Word -> Int -> Int -> Word
        getBitSliceWord word off count =
          word `shiftR` off .&. (1 `shiftL` count - 1)

setBitSlice :: TreeIter -> Int -> Int -> Word -> TreeIter
setBitSlice (TreeIter stamp a b c) off count value =
  assert (value < 1 `shiftL` count) $
  TreeIter stamp
           (setBitSliceWord a  off     count value)
           (setBitSliceWord b (off-32) count value)
           (setBitSliceWord c (off-64) count value)

  where setBitSliceWord :: Word -> Int -> Int -> Word -> Word
        setBitSliceWord word off count value =
          let mask = (1 `shiftL` count - 1) `shiftL` off
           in (word .&. complement mask) .|. (value `shiftL` off)


iterPrefixEqual :: TreeIter -> TreeIter -> Int -> Bool
iterPrefixEqual (TreeIter _ a1 b1 c1) (TreeIter _ a2 b2 c2) pos
  | pos>64 = let mask = 1 `shiftL` (pos-64) - 1 in
	     a1==a2 && b1==b2 && (c1 .&. mask) == (c2 .&. mask)
  | pos>32 = let mask = 1 `shiftL` (pos-32) - 1 in
	     a1==a2 && (b1 .&. mask) == (b2 .&. mask)
  | otherwise = let mask = 1 `shiftL` pos - 1 in
		(a1 .&. mask) == (a2 .&. mask)

-- | The invalid tree iterator.
--
invalidIter :: TreeIter
invalidIter = TreeIter 0 0 0 0

showIterBits (TreeIter _ a b c) = [showBits a, showBits b, showBits c]

showBits :: Bits a => a -> String
showBits a = [ if testBit a i then '1' else '0' | i <- [0..bitSize a - 1] ]

-- | Calculate the maximum number of nodes on a per-level basis.
--
calcForestDepth :: Forest a -> Depth
calcForestDepth f = map bitsNeeded $
		    takeWhile (/=0) $
		    foldr calcTreeDepth (repeat 0) f
  where
  calcTreeDepth Node { subForest = f } (d:ds) =
      (d+1): zipWith max ds (foldr calcTreeDepth (repeat 0) f)


-- | Convert an iterator into a path.
--
toPath :: Depth -> TreeIter -> TreePath
toPath d iter = gP 0 d
  where
  gP pos [] = []
  gP pos (d:ds) = let idx = getBitSlice iter pos d in
	          if idx==0 then [] else fromIntegral (idx-1) : gP (pos+d) ds

-- | Try to convert a path into a 'TreeIter'.
--
fromPath :: Depth -> TreePath -> Maybe TreeIter
fromPath = fP 0 invalidIter
  where
  fP pos ti _ [] = Just ti -- the remaining bits are zero anyway
  fP pos ti [] _ = Nothing
  fP pos ti (d:ds) (p:ps) = let idx = fromIntegral (p+1) :: Word in
    if idx >= bit d then Nothing else
    fP (pos+d) (setBitSlice ti pos d idx) ds ps


-- | The 'Cache' type synonym is only used iternally. What it represents
--   the stack during a (fictional) lookup operations.
--   The topmost frame is the node
--   for which this lookup was started and the innermost frame (the last
--   element of the list) contains the root of the tree.
--
type Cache a = [(TreeIter, Forest a)] 


-- | Create a traversal structure that allows a pre-order traversal in linear
--   time.
--
-- * The returned structure points at the root of the first level which doesn't
--   really exist, but serves to indicate that it is before the very first
--   node.
--
storeToCache :: Forest a -> Cache a
storeToCache [] = []
storeToCache forest = [(invalidIter, [Node root forest])]
  where
  root = error "TreeStore.storeToCache: accessed non-exitent root of tree"

-- | Advance the traversal structure to the given 'TreeIter'.
--
advanceCache :: Depth -> TreeIter -> Cache a -> Cache a
advanceCache depth goal [] = []
advanceCache depth goal cache@((rootIter,_):_) = 
  moveToSameLevel 0 depth
  where
  moveToSameLevel pos [] = cache
  moveToSameLevel pos (d:ds) =
    let
      goalIdx = getBitSlice goal pos d
      curIdx = getBitSlice rootIter pos d
      isNonZero pos d (ti,_) = getBitSlice ti pos d/=0
    in
    if goalIdx==curIdx then moveToSameLevel (pos+d) ds else
    if goalIdx==0 then dropWhile (isNonZero pos d) cache else
    if curIdx==0 then moveToChild pos (d:ds) cache else
    if goalIdx<curIdx then
      moveToChild pos (d:ds) (dropWhile (isNonZero pos d) cache)
    else let
      -- advance the current iterator to coincide with the goal iterator
      -- at this level
      moveWithinLevel pos d ((ti,forest):parents) = let
	  diff = fromIntegral (goalIdx-curIdx)
	  (dropped, remain) = splitAt diff forest
	  advance = length dropped
	  ti' = setBitSlice ti pos d (curIdx+fromIntegral advance)
	in
	if advance==diff then moveToChild (pos+d) ds ((ti',remain):parents)
	else (ti',remain):parents -- node not found
    in moveWithinLevel pos d $ case ds of
        [] -> cache
	(d':_) -> dropWhile (isNonZero (pos+d) d') cache

  -- Descend into the topmost forest to find the goal iterator. The position
  -- and the remainding depths specify the index in the cache that is zero.
  -- All indices in front of pos coincide with that of the goal iterator.
  moveToChild :: Int -> Depth -> Cache a -> Cache a
  moveToChild pos [] cache = cache -- we can't set more than the leaf
  moveToChild pos (d:ds) cache@((ti,forest):parents)
    | getBitSlice goal pos d == 0 = cache
    | otherwise = case forest of
      [] -> cache -- impossible request
      Node { subForest = children }:_ ->
        let
	  childIdx :: Int
	  childIdx = fromIntegral (getBitSlice goal pos d)-1
	  (dropped, remain) = splitAt childIdx children
	  advanced = length dropped
	  ti' = setBitSlice ti pos d (fromIntegral advanced+1)
	in if advanced<childIdx then ((ti',remain):cache) else 
	   moveToChild (pos+d) ds ((ti',remain):cache)

-- | Advance to the given iterator and return weather this was successful.
--    
checkSuccess :: Depth -> TreeIter -> Cache a -> (Bool, Cache a)
checkSuccess depth iter cache = (cmp cur iter && not (null sibs), cache')
  where
  cmp (TreeIter _ a1 b1 c1) (TreeIter _ a2 b2 c2) =
      a1==a2 && b1==b2 && c2==c2
  cache'@((cur,sibs):_) = advanceCache depth iter cache

-- | Get the leaf index of this iterator.
--
-- * Due to the way we construct the 'TreeIter's, we can check which the last
--   level of an iterator is: The bit sequence of level n is zero if n is
--   greater or equal to the level that the iterator refers to. The returned
--   triple is (pos, leaf, zero) such that pos..pos+leaf denotes the leaf
--   index and pos+leaf..pos+leaf+zero denotes the bit field that is zero.
--
getTreeIterLeaf :: Depth -> TreeIter -> (Int, Int, Int)
getTreeIterLeaf ds ti = topLevel ds
  where
  topLevel [] = (0,0,0)
  topLevel (d:ds) | getBitSlice ti 0 d==0 = (0,0,0)
		  | otherwise = gTIL 0 d ds
  gTIL pos dCur (dNext:ds)
    | getBitSlice ti (pos+dCur) dNext==0 = (pos,dCur,dNext)
    | otherwise = gTIL (pos+dCur) dNext ds
  gTIL pos d [] = (pos, d, 0)

-- | Move an iterator forwards on the same level.
--
iterNext :: Depth -> TreeIter -> Cache a -> (Maybe TreeIter, Cache a)
iterNext depth iter cache = let
    (pos,leaf,child) = getTreeIterLeaf depth iter
    curIdx = getBitSlice iter pos leaf
    nextIdx = curIdx+1
    nextIter = setBitSlice iter pos leaf nextIdx
  in
  if nextIdx==bit leaf then (Nothing, cache) else
  case checkSuccess depth nextIter cache of
    (True, cache) -> (Just nextIter, cache)
    (False, cache) -> (Nothing, cache)

-- | Move down to the child of the given iterator.
--
iterNthChild :: Depth -> Int -> TreeIter -> Cache a  ->
		(Maybe TreeIter, Cache a)
iterNthChild depth childIdx_ iter cache = let
    (pos,leaf,child) = getTreeIterLeaf depth iter
    childIdx = fromIntegral childIdx_+1 :: Word
    nextIter = setBitSlice iter (pos+leaf) child childIdx
  in
  if childIdx>=bit child then (Nothing, cache) else
  case checkSuccess depth nextIter cache of
    (True, cache) -> (Just nextIter, cache)
    (False, cache) -> (Nothing, cache)

-- | Descend to the first child.
--
iterNChildren :: Depth -> TreeIter -> Cache a -> (Int, Cache a)
iterNChildren depth iter cache = case checkSuccess depth iter cache of
  (True, cache@((_,Node { subForest = forest}:_):_)) -> (length forest, cache)
  (_, cache) -> (0, cache)


-- | Ascend to parent.
--
iterParent :: Depth -> TreeIter -> Maybe TreeIter
iterParent depth iter = let
    (pos,leaf,child) = getTreeIterLeaf depth iter
  in if leaf==0 then Nothing else
     if getBitSlice iter pos leaf==0 then Nothing else
     Just (setBitSlice iter pos leaf 0)

-- | Insert nodes into the store.
--
-- * The given list of nodes is inserted into given parent at @pos@.
--   If the parent existed, the function returns @Just path@ where @path@
--   is the position of the newly inserted elements. If @pos@ is negative
--   or greater or equal to the number of children of the node at @path@,
--   the new nodes are appended to the list.
--
treeStoreInsert :: TreeStore a -> TreePath -> Int -> Forest a ->
		   IO (Maybe TreePath)
treeStoreInsert (TreeStore store) path pos nodes = do
  (paths, toggle) <- atomicModifyIORef (customStoreGetPrivate store) $
    \store@Store { depth = d, content = cache } ->
    case insertIntoForest (snd (last cache)) nodes path pos of
      Nothing -> (store, ([], False))
      Just (newForest, idx, toggle) ->
	let depth = calcForestDepth newForest
        in (Store { depth = depth,
		    content = storeToCache newForest },
	    (map (\idx -> path++[idx]) [idx..idx+length nodes-1], toggle))
  if null paths then return Nothing else do
    Store { depth = depth } <- readIORef (customStoreGetPrivate store)
    mapM_ (\path -> let Just iter = fromPath depth path in
	   treeModelRowInserted store path iter) paths
    let Just iter = fromPath depth path
    when toggle $ treeModelRowHasChildToggled store path iter
    return (Just (head paths))

-- | Insert nodes into a forest.
--
-- * If the parent was found, returns the new tree, the child number
--   and a flag denoting if these new nodes were the first children
--   of the parent.
--
insertIntoForest :: Forest a -> Forest a -> TreePath -> Int ->
		    Maybe (Forest a, Int, Bool)
insertIntoForest forest nodes [] pos
  | pos<0 = Just (forest++nodes, length forest, null forest)
  | otherwise = Just (prev++nodes++next, length prev, null forest)
    where (prev, next) = splitAt pos forest
insertIntoForest forest nodes (p:ps) pos = case splitAt p forest of
  (prev, []) -> Nothing
  (prev, Node { rootLabel = val,
		subForest = for}:next) ->
    case insertIntoForest for nodes ps pos of
      Nothing -> Nothing
      Just (for, pos, toggle) -> Just (prev++Node { rootLabel = val,
						    subForest = for }:next,
				       pos, toggle)
					       
-- | Remove a node from the store.
--
-- * The node denoted by the path is removed, along with all its children.
--   The function returns @True@ if the given node was found.
--
treeStoreRemove :: TreeStore a -> TreePath -> IO Bool
treeStoreRemove (TreeStore store) path = do
  (found, toggle) <- atomicModifyIORef (customStoreGetPrivate store) $
    \store@Store { depth = d, content = cache } ->
    if null cache then (store, (False, False)) else
    case deleteFromForest (snd (last cache)) path of
      Nothing -> (store, (False, False))
      Just (newForest, toggle) ->
        (Store { depth = d,
		 content = storeToCache newForest }, (True, toggle))
  when found $ do
    when (toggle && not (null path)) $ do
      Store { depth = depth } <- readIORef (customStoreGetPrivate store)
      let parent = init path
	  Just iter = fromPath depth parent
      treeModelRowHasChildToggled store parent iter
    treeModelRowDeleted store path
  return found

-- | Remove a node from a rose tree.
--
-- * Returns the new tree if the node was found. The returned flag is
--   @True@ if deleting the node left the parent without any children.
--
deleteFromForest :: Forest a -> TreePath -> Maybe (Forest a, Bool)
deleteFromForest forest [] = Just ([], False)
deleteFromForest forest (p:ps) =
  case splitAt p forest of
    (prev, kill@Node { rootLabel = val,
		       subForest = for}:next) ->
      if null ps then Just (prev++next, null prev && null next) else
      case deleteFromForest for ps of
        Nothing -> Nothing
	Just (for,toggle) -> Just (prev++Node {rootLabel = val,
					       subForest = for }:next, toggle)
    (prev, []) -> Nothing



-- | Change a node in the store.
--
-- * Returns @True@ if the node was found. For a monadic version, see
--   'treeStoreChangeM'.
--
treeStoreChange :: TreeStore a -> (a -> a) -> TreePath -> IO Bool
treeStoreChange store func path = treeStoreChangeM store (return . func) path


-- | Change a node in the store.
--
-- * Returns @True@ if the node was found. For a purely functional version, see
--   'treeStoreChange'.
--
treeStoreChangeM :: TreeStore a -> (a -> IO a) -> TreePath -> IO Bool
treeStoreChangeM (TreeStore model) act path = do
  store@Store { depth = d, content = cache } <- 
      readIORef (customStoreGetPrivate model)
  (store'@Store { depth = d, content = cache }, found) <- do
    mRes <- changeForest (snd (last cache)) act path
    return $ case mRes of
      Nothing -> (store, False)
      Just newForest -> (Store { depth = d,
				 content = storeToCache newForest }, True)
  writeIORef (customStoreGetPrivate model) store'
  let Just iter = fromPath d path
  when found $ treeModelRowChanged model path iter
  return found

-- | Change a node in the forest.
--
-- * Returns @True@ if the given node was found.
--
changeForest :: Forest a -> (a -> IO a) -> TreePath -> IO (Maybe (Forest a))
changeForest forest act [] = return Nothing
changeForest forest act (p:ps) = case splitAt p forest of
  (prev, []) -> return Nothing
  (prev, Node { rootLabel = val,
		subForest = for}:next) ->
    if null ps then do
      val' <- act val
      return (Just (prev++Node { rootLabel = val',
				 subForest = for }:next))
    else do
      mFor <- changeForest for act ps
      case mFor of
        Nothing -> return Nothing
	Just for -> return $ Just (prev++Node { rootLabel = val,
						subForest = for }:next)

-- | Extract the current data from the model.
--
treeStoreGet :: TreeStore a -> IO (Forest a)
treeStoreGet (TreeStore store) = do
  Store { content = cache } <- readIORef (customStoreGetPrivate store)
  return (if null cache then [] else (snd (last cache)))
