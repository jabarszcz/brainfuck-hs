module Program where

data Op = Incr | Decr
        | MoveLeft | MoveRight
        | Input | Output
        | Loop [Op] deriving (Show, Eq)

type Program = [Op]
