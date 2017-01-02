module Test.Main where

import Prelude
import Data.Either (Either(..))
import Data.Monoid (mempty)
import Data.Options ((:=))
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Minimist (Arg(..), aliases, defaults, splitOnDoubleDash, interpretAsBooleans, stopEarly, parseArgs)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main = run [consoleReporter] do
    describe "purescript-minimist" do
        describe "Minimist" do
            it "should parse nothing" do
                let parsed = parseArgs [] mempty
                    expected = StrMap.singleton "_" $ ArgMulti []
                parsed `shouldEqual` expected
            it "should parse positional arguments" do
                let parsed = parseArgs ["foo", "bar"] mempty
                    expected = StrMap.singleton "_" (ArgMulti ["foo", "bar"])
                parsed `shouldEqual` expected
            it "should parse flags with flag option" do
                let opts = interpretAsBooleans := Left true
                    parsed = parseArgs ["--foo", "bar"] opts
                    expected = StrMap.fromFoldable [ Tuple "_" $ ArgMulti ["bar"]
                                                   , Tuple "foo" $ ArgFlag true
                                                   ]
                parsed `shouldEqual` expected
            it "should parse argument without flag option" do
                let parsed = parseArgs ["--foo", "bar"] mempty
                    expected = StrMap.fromFoldable [ Tuple "_" $ ArgMulti []
                                                   , Tuple "foo" $ ArgString "bar"
                                                   ]
                parsed `shouldEqual` expected
            it "should parse multiple named arguments" do
                let parsed = parseArgs ["--foo", "bar", "--foo", "baz"] mempty
                    expected = StrMap.fromFoldable [ Tuple "_" $ ArgMulti []
                                                   , Tuple "foo" $ ArgMulti ["bar", "baz"]
                                                   ]
                parsed `shouldEqual` expected
            it "should stop parsing args if parseEarly is set" do
                let opts = (interpretAsBooleans := Right ["a", "b"]) <> (stopEarly := true)
                    parsed = parseArgs ["-a", "something", "-b"] opts
                    expected = StrMap.fromFoldable [ Tuple "_" $ ArgMulti ["something", "-b"]
                                                   , Tuple "a" $ ArgFlag true
                                                   , Tuple "b" $ ArgFlag false
                                                   ]
                parsed `shouldEqual` expected
            it "should parse defaults with default option" do
                let opts = defaults := StrMap.singleton "foo" (ArgString "bar")
                    parsed = parseArgs mempty opts
                    expected = StrMap.fromFoldable [ Tuple "_" $ ArgMulti []
                                                   , Tuple "foo" $ ArgString "bar"
                                                   ]
                parsed `shouldEqual` expected
            it "should follow aliases in the aliases option" do
                let opts = aliases := StrMap.singleton "foo" ["foobar"]
                    parsed = parseArgs ["--foobar", "baz"] opts
                    expected = StrMap.fromFoldable [ Tuple "_" $ ArgMulti []
                                                   , Tuple "foobar" $ ArgString "baz"
                                                   , Tuple "foo" $ ArgString "baz"
                                                   ]
                parsed `shouldEqual` expected
            it "should split on -- if splitOnDoubleDash option is set" do
                let opts = splitOnDoubleDash := true
                    parsed = parseArgs ["foo", "--", "--bar", "baz"] opts
                    expected = StrMap.fromFoldable [ Tuple "_" $ ArgMulti ["foo"]
                                                   , Tuple "--" $ ArgMulti ["--bar", "baz"]
                                                   ]
                parsed `shouldEqual` expected
