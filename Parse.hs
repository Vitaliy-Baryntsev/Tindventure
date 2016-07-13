module Parse
  ( Update(..)
  , Match(..)
  , Message(..)
  , Person(..)
  , Gender(..)
  , getNewMatches
  , getReceivedMessages
  ) where

import Control.Monad
import Data.Maybe

import Text.JSON

myId :: String
myId = "5483f9a24af7264d1a54e543"

getNewMatches :: Update -> [Match]
getNewMatches = filter (isJust . isNew) . matches

getReceivedMessages :: Update -> [Message]
getReceivedMessages = filter notMe . join . map messages . matches
  where notMe msg = myId /= from msg

data Update = Update
  { matches :: [Match]
  } deriving(Show)

data Match = Match
  { _id :: String
  , messages :: [Message]
  , isNew :: Maybe Person
  } deriving(Show)

data Message = Message
  { match_id :: String
  , to :: String
  , from :: String
  , message :: String
  } deriving(Show)

data Person = Person
  { person_id :: String
  , bio :: String
  , gender :: Gender
  , name :: String
  } deriving(Show)

data Gender = Male | Female
  deriving(Show, Eq)

toGender :: Int -> Gender
toGender 0 = Male
toGender 1 = Female

look :: (JSON a) => JSObject JSValue -> String -> Result a
look = flip valFromObj
infix 8 `look`

instance JSON Update where
  showJSON _ = JSNull
  readJSON (JSObject jso) = Update `liftM`
    (jso `look` "matches")

instance JSON Match where
  showJSON _ = JSNull
  readJSON (JSObject jso) = Match `liftM`
    (jso `look` "_id") `ap`
    (jso `look` "messages") `ap`
    case jso `look` "person" of
         Error _ -> Ok Nothing
         Ok per -> Ok $ Just per

instance JSON Message where
  showJSON _ = JSNull
  readJSON (JSObject jso) = Message `liftM`
    (jso `look` "match_id") `ap`
    (jso `look` "to") `ap`
    (jso `look` "from") `ap`
    (jso `look` "message")

instance JSON Person where
  showJSON _ = JSNull
  readJSON (JSObject jso) = Person `liftM`
    (jso `look` "_id") `ap`
    (jso `look` "bio") `ap`
    (liftM toGender $ jso `look` "gender") `ap`
    (jso `look` "name")

