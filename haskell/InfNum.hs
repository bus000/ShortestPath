{- |
 - Module      :  InfNum
 - Description :  Represent a number that can be infinity and minus infinity.
 - Copyright   :  (c) Magnus Stavngaard
 - License     :  <license>
 -
 - Maintainer  :  magnus@stavngaard.dk
 - Stability   :  experimental
 - Portability :  portable
 -
 - Used to represent numbers that can not only have the normal values but can
 - also be either positive infinity or negative infinity.
 -}
module InfNum (InfNum(..), getValue)
    where

-- | Represents a number that can be infinity or a normal number. Infinity
-- behaves as one would expect. Infinities are not affected on addition and
-- subtraction of normal numbers. If an infinity is multiplied by a positive
-- value it is the same infinity and if multiplied by a negative value is
-- negated. Infinities compare equal to each other and compare greater or lower
-- than any number depending on its sign.
data InfNum a
    = NegInf   -- ^ Represents negative infinity.
    | Number a -- ^ Represents a number of type a.
    | PosInf   -- ^ Represents a positive infinity.
    deriving (Show)

instance Eq a => Eq (InfNum a) where
    NegInf == NegInf     = True
    NegInf == PosInf     = False
    PosInf == NegInf     = False
    PosInf == PosInf     = True
    Number x == Number y = x == y
    _ == _               = False

instance Ord a => Ord (InfNum a) where
    compare NegInf NegInf         = EQ
    compare PosInf PosInf         = EQ
    compare NegInf _              = LT
    compare PosInf _              = GT
    compare _ PosInf              = LT
    compare _ NegInf              = GT
    compare (Number x) (Number y) = compare x y

instance (Num a, Eq a) => Num (InfNum a) where
    negate NegInf           = PosInf
    negate PosInf           = NegInf
    negate (Number x)       = Number (-x)

    NegInf     + NegInf     = NegInf
    NegInf     + PosInf     = 0
    PosInf     + NegInf     = 0
    PosInf     + PosInf     = PosInf
    NegInf     + Number x   = NegInf
    Number x   + NegInf     = NegInf
    PosInf     + Number x   = PosInf
    Number x   + PosInf     = PosInf
    (Number x) + (Number y) = Number (x + y)

    NegInf     * NegInf     = PosInf
    NegInf     * PosInf     = NegInf
    PosInf     * NegInf     = NegInf
    PosInf     * PosInf     = PosInf
    NegInf     * (Number x) =
        if (signum x) == 0
            then Number (x-x)
            else if (signum x) == 1 then NegInf else PosInf
    (Number x) * NegInf     =
        if (signum x) == 0
            then Number (x-x)
            else if (signum x) == 1 then NegInf else PosInf
    PosInf * (Number x)     =
        if (signum x) == 0
            then Number (x-x)
            else if (signum x) == 1 then PosInf else NegInf
    (Number x) * PosInf     =
        if (signum x) == 0
            then Number (x-x)
            else if (signum x) == 1 then PosInf else NegInf
    (Number x) * (Number y) = Number (x * y)

    fromInteger int         = Number (fromInteger int)

    abs NegInf              = PosInf
    abs PosInf              = PosInf
    abs (Number x)          = Number (abs x)

    signum NegInf           = Number (-1)
    signum PosInf           = Number 1
    signum (Number x)       = Number (signum x)

-- | Get the numerical value of the number. If the argument is an infinity an
-- error is raised.
getValue ::
    InfNum a -- ^ InfNum to extract value from.
    -> a     -- ^ The value extracted.
getValue PosInf = error "Can't get value of infinity"
getValue NegInf = error "Can't get value of infinity"
getValue (Number x) = x
