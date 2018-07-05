module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (NonEmptyList)
import Effect (Effect)
import Effect.Console (logShow)
import Foreign (ForeignError)
import Foreign.Object (Object)
import Global.Unsafe (unsafeStringify)
import Simple.JSON (class ReadForeign, read', readJSON)

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
  readImpl f = NumberValue <$> read' f <|> StringValue <$> read' f

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

main :: Effect Unit
main = do
  logShow $ decodeJSON testJson1
  logShow $ decodeJSON testJson2
  logShow $ (readJSON testJson3 :: Either (NonEmptyList ForeignError) (Object PossibleValues)) -- context provided like so
  logShow <<< unsafeStringify $ (readJSON testJson3 :: Either (NonEmptyList ForeignError) (Object PossibleValues)) -- just as the raw form

  -- output:
  -- (Right (NumberValue { a: 1, b: 123 }))
  -- (Right (StringValue { a: 1, b: "asdf" }))
  -- (Right (fromFoldable [(Tuple "A1" (NumberValue { a: 1, b: 123 })),(Tuple "B1" (StringValue { a: 1, b: "asdf" }))]))
  -- "{\"value0\":{\"A1\":{\"value0\":{\"b\":123,\"a\":1}},\"B1\":{\"value0\":{\"b\":\"asdf\",\"a\":1}}}}"

