---
title: Growing a Merkle Tree
author: Ben Kirwin
---

# Background

## Ethereum

> Ethereum is a platform and a programming language that makes it possible for
> any developer to build and publish next-generation distributed applications.
>
> Ether, Ethereum's cryptofuel, powers the applications on the decentralized
> network.

http://ethereum.org

## ethereum-haskell

> An independent, unaffiliated, and incomplete reimplementation of Ethereum in
> Haskell.

https://github.com/bkirwi/ethereum-haskell

http://ben.kirw.in

# Mappings and Tries

## Setup

```haskell
import Data.ByteString
import Ethereum.Word4

type Key = [Word4] -- hexadecimal digits
type Value = ByteString

empty = ByteString.empty
```

## Association List

``` {.dot}
digraph pairs {
  node [width=0]
  node [shape=rectangle]
  CAFE
  C0D
  C0DE
  F00D
  node [shape=oval]
  CAFE -> "124AFE"
  C0D -> "A374C8"
  C0DE -> "04D21E"
  F00D -> "F4D71E"
}
```

## Association List

```haskell
type AssocList = [(Key, ByteString)]
```
. . .

```haskell
lookup :: Key -> AssocList -> Value
lookup key = fromMaybe empty . lookup key
```

. . .

```haskell
update :: Key -> Value -> AssocList -> AssocList
update key value list = (key, value) : list -- grows forever!

```

. . .

```haskell
update key empty list -- == ???
```

## Trie

``` {.dot rankdir=LR}
digraph trie {
  node [width=0]
  "<root>" [shape=rectangle]
  node [shape=none]
  C
  A
  F
  E
  O [label="0"]
  D
  E1 [label="E"]
  F1 [label="F"]
  O1 [label="0"]
  O2 [label="0"]
  D1 [label=D]
  node [shape=oval]
  "<root>" -> C -> A -> F -> E -> "124AFE"
  C -> O -> D -> "A374C8"
  D -> E1 -> "04D21E"
  "<root>" -> F1 -> O1 -> O2 -> D1 -> "F4D71E"
}
```

## Trie

```haskell
data Trie
  = Branch [Trie] ByteString
  | Empty

lookup :: Trie -> Key -> Value
lookup Empty _ = ""
lookup (Branch _ value) [] = value
lookup (Branch children _) (char : rest) =
  let child = genericIndex children char
  in lookup child rest
```

## Patricia Trie

``` {.dot rankdir=LR}
digraph patricia {
  node [width=0]
  "<root>" [shape=rectangle]
  node [shape=none]
  C
  AFE
  OD [label="0D"]
  E1 [label="E"]
  F00D
  node [shape=oval]
  "<root>" -> C -> AFE -> "124AFE"
  C -> OD -> "A374C8"
  OD -> E1 -> "04D21E"
  "<root>" -> F00D -> "F4D71E"
}
```

## Patricia Trie

```haskell
data Node
  = Branch [Patricia] ByteString
  | Shortcut [Word4] (Either ByteString Patricia)
  | Empty

lookup :: Node -> Key -> ByteString
lookup (Shortcut prefix result) key = 
  case (stripPrefix prefix key, result) of
    (Just [], Right value) -> value
    (Just remaining, Left ref) -> lookup ref suffix
    _ -> BS.empty
-- other cases stay the same
```

# Merkle Trees

## Intuition

> [...] a hash tree or Merkle tree is a tree in which every non-leaf node is
> labelled with the hash of the labels of its children nodes. Hash trees are
> useful because they allow efficient and secure verification of the contents of
> large data structures. 

http://en.wikipedia.org/wiki/Merkle_tree

## Merkle Tree

```haskell
newtype Hash = Hash ByteString

data Tree
  = Branch [Hash] ByteString
  | Shortcut [Word4] (Either ByteString Hash)
  | Empty

lookup :: Hash -> Key -> Value
lookup hash key = undefined -- ??? 
```

## Monadic State

```haskell
class Monad m => DB m where
  getDB :: Hash -> m Node
  putDB :: Hash -> Node -> m ()
```

. . .

```haskell
instance DB (State (Hash -> Node)) where
  getDB hash = do
    state <- get
    return $ state hash
  putDB hash node = do
    state <- get
    set $ \hash' ->
      if (hash' == hash) then node
      else state hash'
```

## Lookup

```haskell
lookup :: DB db => Hash -> Key -> db ByteString
lookup root key = getDB root >>= getVal
  where
    getVal Empty = return BS.empty
    getVal (Shortcut prefix result) = 
      case (stripPrefix prefix key, result) of
        (Just [], Right value) -> return value
        (Just remaining, Left ref) -> lookup ref suffix
        _ -> return BS.empty
    getVal (Branch children val) = case key of
      [] -> return val
      (char:rest) -> 
        let ref = children !! fromIntegral char
        in lookup ref rest
```

## Payoff

> - Association List
> - `Data.Map`
> - LevelDB
> - Network RPC
> - Custom error handling

# Generalizing

## Multi-Param Class

```haskell
class Monad (m k v) => DB m k v where
  getDB :: k -> m k v v
  putDB :: k -> v -> m k v ()
```

> - `MultiParamTypeClasses`
> - `FlexibleContexts`
> - `ConstraintKinds`
> - ...

## Language Note

> I advocate a direct programming style in Haskell. Advanced type system
> features have their place, but plain old functions go a long, long way.
> Functions are the masters of reuse: when you use an advanced feature, you need
> a yet more advanced feature to abstract over it [...] but all you need to
> abstract over a function is another function.

https://lukepalmer.wordpress.com/2010/01/24/haskell-antipattern-existential-typeclass/

## Free Monad

```haskell
data Free f a 
  = Pure a
  | Free (f (Free f a))

instance Functor f => Monad (Free f) where
  return = Pure
  Pure a >>= f = f a
  Free m >>= f = Free ((>>= f) <$> m)
```

## Implementation

```haskell
data DBOps k v next
  = Put k v next
  | Get k (v -> next)
  deriving (Functor)
```

. . .

```haskell
newtype DB k v a = DB (Free (KV k v) a)
  deriving (Functor, Applicative, Monad)

getDB :: k -> v -> DB k v ()
getDB k v = DB $ liftF $ Put k v ()

putDB :: k -> DB k v v
putDB k = DB $ liftF $ Get k id
```

## Lookup

```haskell
lookup :: DB db => Hash -> Key -> db ByteString
lookup root key = getDB root >>= getVal
  where
    getVal Empty = return BS.empty
    getVal (Shortcut prefix result) = 
      case (stripPrefix prefix key, result) of
        (Just [], Right value) -> return value
        (Just remaining, Left ref) -> lookup ref suffix
        _ -> return BS.empty
    getVal (Branch children val) = case key of
      [] -> return val
      (char:rest) -> 
        let ref = children !! fromIntegral char
        in lookup ref rest
```

## Interpreter

```
runDB :: Monad m           
    => (k -> v -> m ()) -- put
    -> (k -> m v) -- get
    -> DB k v a
    -> m a
runDB put get (DB ops) = go ops
  where
    go (Pure a) = return a 
    go (Free (Put k v next)) = put k v >> go next
    go (Free (Get k handler)) = get k >>= go . handler
```

## More Freedom 

- Why Free Monads Matter

    http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html

- Typed Tagless Final Interpreters

    http://okmij.org/ftp/tagless-final/course/lecture.pdf

## Leftovers

``` {.dot}
digraph merkle {
  node [width=0]
  node [shape=rectangle]
  H0
  FUUUUUUUUUUUUUUUUUUUu
  H1
  H2
  node [shape=none]
  "<root>" -> H0
  H1 -> cup -> C
}
```
