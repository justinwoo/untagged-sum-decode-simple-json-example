module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Either (Either)
import Data.Foreign (ForeignError)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (NonEmptyList)
import Data.StrMap (StrMap)
import Global.Unsafe (unsafeStringify)
import Simple.JSON (class ReadForeign, read, readJSON)

type NumberValueRecord =
  { a :: Int
  , b :: Int
  }

type StringValueRecord =
  { a :: Int
  , b :: String
  }

data PossibleValues
  = NumberValue NumberValueRecord
  | StringValue StringValueRecord
instance readForeignPossibleValues :: ReadForeign PossibleValues where
  readImpl f = NumberValue <$> read f <|> StringValue <$> read f

-- some stuff for generic Show of PossibleValues
derive instance genericPossibleValues :: Generic PossibleValues _
instance showPossibleValues :: Show PossibleValues where
  show = genericShow

testJson1 :: String
testJson1 = """
{
  "a": 1,
  "b": 123
}
"""

testJson2 :: String
testJson2 = """
{
  "a": 1,
  "b": "asdf"
}
"""

testJson3 :: String
testJson3 = """
{
  "A1":
    {
      "a": 1,
      "b": 123
    },
  "B1":
    {
      "a": 1,
      "b": "asdf"
    }
}
"""

decodeJSON :: String -> Either (NonEmptyList ForeignError) PossibleValues -- usually you provide this type through usage or scoped type variables
decodeJSON = readJSON

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow $ decodeJSON testJson1
  logShow $ decodeJSON testJson2
  logShow $ (readJSON testJson3 :: Either (NonEmptyList ForeignError) (StrMap PossibleValues)) -- context provided like so
  log <<< unsafeStringify $ (readJSON testJson3 :: Either (NonEmptyList ForeignError) (StrMap PossibleValues)) -- yolo, just JSON.stringify

  -- output:
  -- (Right (NumberValue { a: 1, b: 123 })) <-- as expected
  -- (Right (StringValue { a: 1, b: "asdf" })) <-- also as expected
  -- (Right (fromFoldable [(Tuple "A1" (NumberValue { a: 1, b: 123 })),(Tuple "B1" (StringValue { a: 1, b: "asdf" }))])) <-- yup, a stringmap stringified is basically a bunch of tuples
  -- {"value0":{"A1":{"value0":{"b":123,"a":1}},"B1":{"value0":{"b":"asdf","a":1}}}}"
