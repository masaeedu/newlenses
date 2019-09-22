{-# LANGUAGE TypeApplications #-}
module Optics where

import Data.Void

import Types
import Classes
import Instances

-- General things
flark :: GCategory p => Optic p m '(x `m` a, x `m` b) '(a, b)
flark = Optic gid gid

quimble :: SymmetricTensoid p m => Optic p m '(a `m` x, b `m` x) '(a, b)
quimble = Optic swap swap

view :: Semicartesian p m i => Optic p m '(s, t) '(a, b) -> s `p` a
view (Optic f _) = right <<< f

put :: Semicartesian p m i => Optic p m '(s, t) '(a, b) -> (s `m` b) `p` t
put (Optic f g) = g <<< gbimap (left <<< f) gid

-- Specific things
type Lens s t a b = Optic (->) (,) '(s, t) '(a, b)
type Prism s t a b = Optic (Op (->)) Either '(s, t) '(a, b)

extract :: Lens s t a b -> s -> a
extract = view

update :: Lens s t a b -> (s, b) -> t
update = put

build :: Prism s t a b -> a -> s
build = runOp <<< view

match :: Prism s t a b -> t -> Either s b
match = runOp <<< put

_1 :: Lens (a, x) (b, x) a b
_1 = quimble

_2 :: Lens (x, a) (x, b) a b
_2 = flark

_Left :: Prism (Either a x) (Either b x) a b
_Left = quimble

_Right :: Prism (Either x a) (Either x b) a b
_Right = flark
