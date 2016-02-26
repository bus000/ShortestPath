module InfNum (InfNum(..))
    where

data InfNum a = NegInf | Number a | PosInf
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

    NegInf   + NegInf       = NegInf
    NegInf   + PosInf       = 0
    PosInf   + NegInf       = 0
    PosInf   + PosInf       = PosInf
    NegInf   + Number x     = NegInf
    Number x + NegInf       = NegInf
    PosInf   + Number x     = PosInf
    Number x + PosInf       = PosInf
    (Number x) + (Number y) = Number (x + y)

    NegInf   * NegInf   = PosInf
    NegInf   * PosInf   = NegInf
    PosInf   * NegInf   = NegInf
    PosInf   * PosInf   = PosInf
    NegInf * (Number x) =
        if (signum x) == 0
            then Number (x-x)
            else if (signum x) == 1 then NegInf else PosInf
    (Number x) * NegInf =
        if (signum x) == 0
            then Number (x-x)
            else if (signum x) == 1 then NegInf else PosInf
    PosInf * (Number x) =
        if (signum x) == 0
            then Number (x-x)
            else if (signum x) == 1 then PosInf else NegInf
    (Number x) * PosInf =
        if (signum x) == 0
            then Number (x-x)
            else if (signum x) == 1 then PosInf else NegInf
    (Number x) * (Number y) = Number (x * y)

    fromInteger int     = Number (fromInteger int)

    abs NegInf          = PosInf
    abs PosInf          = PosInf
    abs (Number x)        = Number (abs x)

    signum NegInf       = Number (-1)
    signum PosInf       = Number 1
    signum (Number x)     = Number (signum x)
