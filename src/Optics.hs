module Optics where

import Types
import Classes

_2 :: GCategory p => Optic p m '(x `m` a, x `m` b) '(a, b)
_2 = Optic gid gid

_1 :: SymmetricTensoid p m => Optic p m '(a `m` x, b `m` x) '(a, b)
_1 = Optic swap swap

view :: Semicartesian p m i => Optic p m '(s, t) '(a, b) -> s `p` a
view (Optic f _) = right <<< f

-- over :: Cartesian p m i => Optic p m '(s, t) '(a, b) -> a

-- type Lens  s t a b = forall p r. (forall m. SymmetricTensor p (,)    ()   => Optic p m '(s, t) '(a, b) -> r) -> r
-- type Prism s t a b = forall p r. (forall m. SymmetricTensor p Either Void => Optic p m '(s, t) '(a, b) -> r) -> r

-- _1 :: Lens (a, x) (b, x) a b
-- _1 = Optic swap swap

-- _2 :: Lens (x, a) (x, b) a b
-- _3 :: Lens (x, y, a) (x, y, b) a b
-- _3 = Optic (\v@(_, _, a) -> (v, a)) (\((x, y, _), b) -> (x, y, b))
