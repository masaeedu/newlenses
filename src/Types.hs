module Types where

type Hom o = o -> o -> *

type family Fst ab
  where
  Fst '(a, b) = a

type family Snd ab
  where
  Snd '(a, b) = b

data Iso p a b = Iso { fwd :: p a b, bwd :: p b a }

newtype Op p a b = Op { runOp :: p b a }

data Optic (p :: Hom c) (m :: c -> c -> c) st ab
  where
  Optic :: Fst st `p` (x `m` Fst ab) -> (x `m` Snd ab) `p` Snd st -> Optic p m st ab
