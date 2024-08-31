module PlayHaskellTypes.Statistics.Types where


-- | Strict 'Maybe'.
data Maybe' a = Nothing' | Just' !a
  deriving (Show)

instance Semigroup a => Semigroup (Maybe' a) where
  Nothing' <> x = x
  x <> Nothing' = x
  Just' x <> Just' y = Just' (x <> y)

instance Semigroup a => Monoid (Maybe' a) where
  mempty = Nothing'

-- | Strict pair.
data Pair' a b = Pair' !a !b
  deriving (Show)
