// :: // :: const hom = o => o -> o -> *

// :: // :: forall o. hom o -> *
// :: const gcategory = p => {
// ::   const id = forall a. p a a
// ::   const compose = forall a b c. p b c -> p a b -> p a c
// ::
// ::   return { id, compose }
// :: }

// :: // :: forall o. hom o -> hom o
// :: const iso = p => a => b => ({ fwd: p a b, bwd: p b a })
