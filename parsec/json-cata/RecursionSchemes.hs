{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module RecursionSchemes where

import Control.Arrow hiding (left, right)

newtype Term f = In { out :: f (Term f) }
-- These instances are pretty sinful, but we'll use them for now
-- rather than complicating things with Eq1 and Show1.
deriving instance (Eq (f (Term f))) => Eq (Term f)
deriving instance (Show (f (Term f))) => Show (Term f)

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Term f -> a
cata f = out >>> fmap (cata f) >>> f