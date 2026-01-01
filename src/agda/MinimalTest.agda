open import Agda.Builtin.String using (String; primStringAppend)

-- | Minimal test module demonstrating string concatenation.
module MinimalTest where

concatStr : String → String → String
concatStr = primStringAppend

testStrInfix : String
testStrInfix = concatStr "foo" "bar"

testStrPrefix : String
testStrPrefix = concatStr "foo" "bar"
