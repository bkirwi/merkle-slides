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

<http://ethereum.org>

## ethereum-haskell

> An independent, unaffiliated, and incomplete reimplementation of Ethereum in
> Haskell.

<https://github.com/bkirwi/ethereum-haskell>

<http://ben.kirw.in>

# Mappings and Tries

## Setup

```haskell
import Data.ByteString
import Ethereum.Word4

type Key = [Word4] -- hexadecimal digits
type Value = ByteString

defaultValue = ByteString.empty
```

## Key / Value Pairs

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
type AssocList = [(Key, Value)]
```
. . .

```haskell
lookup :: Key -> AssocList -> Value
lookup key = 
  fromMaybe defaultValue . Prelude.lookup key
```

. . .

```haskell
update :: Key -> Value -> AssocList -> AssocList
update key value list = (key, value) : list
```

. . .

```haskell
-- TODO: handle duplicates, default values
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
  = Branch [Trie] Value -- 16-element list
  | Empty
```

. . .

```haskell
lookup :: Trie -> Key -> Value
lookup Empty _ = defaultValue
lookup (Branch _ value) [] = value
lookup (Branch children _) (nibble : rest) =
  let child = genericIndex children nibble
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
data Trie
  = Branch [Trie] Value
  | Shortcut [Word4] (Either Trie Value)
  | Empty

lookup :: Trie -> Key -> Value
lookup (Shortcut prefix result) key = 
  case (stripPrefix prefix key, result) of
    (Just [], Right value) -> value
    (Just suffix, Left child) -> lookup child suffix
    _ -> defaultValue
lookup _ _ = ...
```

## Normalization

. . .

```haskell
Shortcut prefix (Right defaultValue) --> Empty
```

. . .

```haskell
Shortcut prefix (Left Empty) --> Empty
```

. . .

```haskell
Branch (replicate 16 Empty) value -->
  Shortcut [] (Right value)
```

. . .

Easy to mess up, but easy to QuickCheck!

# Merkle Tree

## Intuition

> [...] a hash tree or Merkle tree is a tree in which every non-leaf node is
> labelled with the hash of the labels of its children nodes. Hash trees are
> useful because they allow efficient and secure verification of the contents of
> large data structures. 

<http://en.wikipedia.org/wiki/Merkle_tree>

## Merkle Patricia Trie

```haskell
newtype Hash = Hash ByteString

data Trie
  = Branch [Hash] Value
  | Shortcut [Word4] (Either Value Hash)
  | Empty
```

. . .

```
lookup :: Hash -> Key -> Value
lookup hash key = ??? 
```

## Interleaved IO

```haskell
lookup :: Hash -> Key -> IO Value
lookup root key = retrieveNode root >>= getVal
  where
    getVal Empty = return defaultValue
    getVal (Shortcut prefix result) = 
      case (stripPrefix prefix key, result) of
        (Just [], Right value) -> return value
        (Just suffix, Left ref) -> lookup ref suffix
        _ -> return defaultValue
    getVal (Branch _ _) = ...

retrieveNode :: Hash -> IO Node
```

## Monadic State

```haskell
class Monad m => DB m where
  getDB :: Hash -> m Trie
  putDB :: Hash -> Trie -> m ()
```

. . .

```haskell
instance DB (State (Hash -> Trie)) where
  getDB hash = do
    stateFn <- get
    return $ stateFn hash
  putDB hash node = do
    stateFn <- get
    set $ \hash' ->
      if (hash' == hash) then node
      else stateFn hash'
```

## Lookup

```haskell
lookup :: DB db => Hash -> Key -> db Value
lookup root key = getDB root >>= getVal
  where getVal = ...
```

## Expressiveness

> - Association List
> - `Data.Map`
> - LevelDB (IO)
> - Network RPC (IO / Continuations)
> - Custom error handling
> - Multiple instances with `newtype`

# Generalizing

## Multi-Param Class

```haskell
class Monad m => DB m where
  getDB :: Hash -> m Trie
  putDB :: Hash -> Trie -> m ()

-- becomes...

class Monad (m k v) => DB m k v where
  getDB :: k -> m k v v
  putDB :: k -> v -> m k v ()
```

## Language Extensions

> - `MultiParamTypeClasses`
> - `FlexibleContexts`
> - `ConstraintKinds`
> - ...

## Keeping it Simple

> I advocate a direct programming style in Haskell. Advanced type system
> features have their place, but plain old functions go a long, long way.
> Functions are the masters of reuse: when you use an advanced feature, you need
> a yet more advanced feature to abstract over it [...] but all you need to
> abstract over a function is another function.

<https://lukepalmer.wordpress.com/2010/01/24/haskell-antipattern-existential-typeclass/>

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
newtype DB k v a = DB (Free (DBOps k v) a)
  deriving (Functor, Applicative, Monad)
```

. . .

```haskell
getDB :: k -> DB k v v
getDB k = DB $ liftF $ Get k id

putDB :: k -> v -> DB k v ()
putDB k v = DB $ liftF $ Put k v ()
```

## Lookup

```haskell
lookup :: DB db => Hash -> Key -> db Value
lookup root key = getDB root >>= getVal
  where
    getVal Empty = return defaultValue
    getVal (Shortcut prefix result) = 
      case (stripPrefix prefix key, result) of
        (Just [], Right value) -> return value
        (Just suffix, Left ref) -> lookup ref suffix
        _ -> return defaultValue
    getVal (Branch children val) = case key of
      [] -> return val
      (nibble:rest) -> 
        let ref = genericIndex children nibble
        in lookup ref rest
```

## Interpreter

```
runDB :: Monad m           
    => (k -> v -> m ()) -- put
    -> (k -> m v)       -- get
    -> DB k v a
    -> m a
runDB put get (DB ops) = go ops
  where
    go (Pure a) = return a 
    go (Free (Put k v next)) = 
        put k v >> go next
    go (Free (Get k handler)) = 
        get k >>= go . handler
```

## Payoff

> - Fewer extensions
> - Multiple interepreters for same context (ie. `IO`)
> - Very late binding

## More Freedom 

- Why Free Monads Matter

    <http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html>

- Typed Tagless Final Interpreters

    <http://okmij.org/ftp/tagless-final/course/lecture.pdf>

# Bonus Slides

## Optional Merkle?

```haskell
data Trie ref
  = Empty
  | Branch [ref] Value
  | ...

class Monad m => IsRef m r | r -> m where
  getRef :: r -> m Trie
  putRef :: Trie -> m r
```

## Optional Merkle?

```haskell
instance IsRef Identity Trie where
  getRef trie = return trie
  putRef trie = return trie

instance IsRef (DB Hash Trie) Hash where
  getRef hash = getDB hash
  putRef trie = 
    let hash = getHash trie 
    in putDB hash trie >> return hash
```

## Pandoc

```haskell
graphViz :: Block -> IO Block
graphViz (CodeBlock (_, classes, _) text) 
    | "dot" `elem` classes = stripStyle <$> ...
graphViz x = return x

stripStyle :: String -> String
stripStyle = renderTags . map noStyle . parseTags
  where noStyle = ...

main :: IO ()
main = toJSONFilter graphViz
```
