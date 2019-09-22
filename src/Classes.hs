module Classes where

import Types

-- Categories
class GCategory (p :: Hom o)
  where
  gid :: p a a
  gcompose :: p b c -> p a b -> p a c

(<<<) :: GCategory p => p b c -> p a b -> p a c
(<<<) = gcompose

-- Monoidal tensors
class GCategory p => GBifunctor p t
  where
  gbimap :: p a b -> p c d -> p (t a c) (t b d)

class GBifunctor p t => Tensoid p t
  where
  assoc :: Iso p (a `t` (b `t` c)) ((a `t` b) `t` c)

class Tensoid p t => Tensor p t i | t -> i
  where
  lunit :: Iso p (i `t` a) a
  runit :: Iso p (a `t` i) a

class Tensoid p t => SymmetricTensoid p t
  where
  swap :: p (t a b) (t b a)

type SymmetricTensor p t i = (SymmetricTensoid p t, Tensor p t i)

-- Terminal object
class GCategory p => Terminal p i
  where
  discard :: p x i

type Initial p i = Terminal (Op p) i

-- Semicartesian monoidal
type Semicartesian p t i = (Terminal p i, Tensor p t i)

type Semicocartesian p t i = Semicartesian (Op p) t i

right :: Semicartesian p t i => p (t x y) y
right = fwd lunit <<< gbimap discard gid

left :: Semicartesian p t i => p (t x y) x
left = fwd runit <<< gbimap gid discard

-- Full cartesian monoidal
class Semicartesian p t i => Cartesian p t i
  where
  duplicate :: p x (t x x)

type Cocartesian p t i = Cartesian (Op p) t i
