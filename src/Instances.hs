module Instances where

import Data.Void
import Data.Bifunctor

import Classes
import Types

-- (->)
instance GCategory (->)
  where
  gid = id
  gcompose = (.)

-- Op
instance GCategory p => GCategory (Op p)
  where
  gid = Op gid
  gcompose (Op pcb) (Op pba) = Op (pba <<< pcb)

instance GBifunctor p t => GBifunctor (Op p) t
  where
  gbimap (Op pba) (Op pdc) = Op $ gbimap pba pdc

instance Tensoid p t => Tensoid (Op p) t
  where
  assoc = let (Iso fwd bwd) = assoc in Iso (Op bwd) (Op fwd)

instance Tensor p t i => Tensor (Op p) t i
  where
  lunit = let (Iso fwd bwd) = lunit in Iso (Op bwd) (Op fwd)
  runit = let (Iso fwd bwd) = runit in Iso (Op bwd) (Op fwd)

instance SymmetricTensoid p t => SymmetricTensoid (Op p) t
  where
  swap = Op swap

-- (,)
instance GBifunctor (->) (,)
  where
  gbimap = bimap

instance Tensoid (->) (,)
  where
  assoc = Iso fwd bwd
    where
    fwd (x, (y, z)) = ((x, y), z)
    bwd ((x, y), z) = (x, (y, z))

instance Tensor (->) (,) ()
  where
  lunit = Iso fwd bwd
    where
    fwd (_, x) = x
    bwd x = ((), x)

  runit = Iso fwd bwd
    where
    fwd (x, _) = x
    bwd x = (x, ())

instance SymmetricTensoid (->) (,)
  where
  swap (x, y) = (y, x)

instance Terminal (->) ()
  where
  discard = const ()

instance Cartesian (->) (,) ()
  where
  duplicate x = (x, x)

-- Either
instance GBifunctor (->) Either
  where
  gbimap = bimap

instance Tensoid (->) Either
  where
  assoc = Iso fwd bwd
    where
    fwd (Left a)          = Left $ Left $ a
    fwd (Right (Left b))  = Left $ Right $ b
    fwd (Right (Right c)) = Right c

    bwd (Left (Left a))  = Left a
    bwd (Left (Right b)) = Right $ Left $ b
    bwd (Right c)        = Right $ Right $ c

instance Tensor (->) Either Void
  where
  lunit = Iso fwd bwd
    where
    fwd = either absurd id
    bwd = Right

  runit = Iso fwd bwd
    where
    fwd = either id absurd
    bwd = Left

instance SymmetricTensoid (->) Either
  where
  swap (Left x) = Right x
  swap (Right x) = Left x

instance Terminal (Op (->)) Void
  where
  discard = Op absurd

instance Cartesian (Op (->)) Either Void
  where
  duplicate = Op $ either id id

-- Optic
instance Tensor p m i => GCategory (Optic p m)
  where
  gid = Optic (bwd lunit) (fwd lunit)
  gcompose (Optic f1 g1) (Optic f2 g2) = Optic f g
    where
    f = fwd assoc <<< gbimap gid f1 <<< f2
    g = g2 <<< gbimap gid g1 <<< bwd assoc
