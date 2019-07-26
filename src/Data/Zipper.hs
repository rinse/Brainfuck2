module Data.Zipper
    ( Zipper (..)
    , left
    , right
    ) where


data Zipper a = Zipper [a] a [a]

left, right :: Zipper a -> Zipper a
left (Zipper ls c (r:rs)) = Zipper (c:ls) r rs
right (Zipper (l:ls) c (rs)) = Zipper ls l (c:rs)

