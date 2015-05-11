data CannotShow = CannotShow

--will not compile
--data CannotDeriveShow = CannotDeriveShow CannotShow
--                        deriving (Show)

data OK = OK

instance Show OK where
  show _ = "OK"

data ThisWorks = ThisWorks OK
                 deriving (Show)
