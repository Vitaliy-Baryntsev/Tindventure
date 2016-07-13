module People where

--   ( getPerson
--   , janka
--   ) where
import Data.Maybe

janka = "5483f9a24af7264d1a54e543569b0acca3b8f29b5f30063c"
jenny = "5483f9a24af7264d1a54e54354f80cf78b0dfb960cfd3e47"
zach = "5483f9a24af7264d1a54e54355e101232ada82ab473d2e9f"
jason = "5483f9a24af7264d1a54e54354c3374f3c95ecac40ac307f"

getPerson :: String -> String
getPerson name = fromMaybe "" $ lookup name people

people = [ ("Emma",       "53e2d3a4e36a6cdc0a31a0235483f9a24af7264d1a54e543" )
         , ("Biggot",     "5373b3b438b0d9d21b0021af5483f9a24af7264d1a54e543" )
         , ("Oliver",     "53bb5821c616e59f175b640b5483f9a24af7264d1a54e543" )
         , ("Patrick",    "52228641cb600020760000205483f9a24af7264d1a54e543" )
         , ("Kendall",    "5483f9a24af7264d1a54e54354a62588db1fd4b1446d6d5c" )
         , ("Pamela",     "5486166f530bdab6650f87b4" )  --this girl...
         , ("Abby",       "50e6622b24f407991b0000cf5483f9a24af7264d1a54e543" )
         , ("Irfan",      "5483f9a24af7264d1a54e54354b87d3fa966c2fa0b51c193" )
         , ("Matt",       "54770d2ebf0f4d55084923115483f9a24af7264d1a54e543" )
         , ("Brian",      "542433dc544991fa12ea322f5483f9a24af7264d1a54e543" )
         , ("Donovan",    "53f25ed5fc7e799a4927c8b65483f9a24af7264d1a54e543" )
         , ("Anthony",    "53929f304439c54b61b6599c5483f9a24af7264d1a54e543" )
         , ("Logan",      "54274997d704002351778e565483f9a24af7264d1a54e543" )
         , ("Megan-old",  "5483f9a24af7264d1a54e54354b0845c4bc082c930f6256c" )
         , ("Irene",      "5403da51e3b5766556dce3035483f9a24af7264d1a54e543" )
         , ("Dea",        "5469f7dc7188034d1e34f1ee5483f9a24af7264d1a54e543" )
         , ("Ren",        "5483f9a24af7264d1a54e5435596e43fea96da7b6445ac9c" )
         , ("Susan",      "52a68f90d37bc253340007185483f9a24af7264d1a54e543" )
         , ("Megan-IU",   "539e6c84b59d6613621136d25483f9a24af7264d1a54e543" )
         , ("Sarah",      "5483f9a24af7264d1a54e54355c6bc046d9ddf1574e439d8" )
         , ("Madison",    "5483f9a24af7264d1a54e543566f9df9075fcfe304697828" )
            -- teegan's friend
         , ("Paige",      "5483f9a24af7264d1a54e543567a177bafc130ab4dfb0d70" )
         , ("Dana",       "53d4a8dafca370e004a317de5483f9a24af7264d1a54e543" )
         ]

