-- Chapter 8, Section 1, Problem 1
import GlobRegex(globToRegex)
import Data.Char(toUpper)

upperRegex = map toUpper . globToRegex
-- Prelude> :l ch08.exercises.hs
-- [1 of 2] Compiling GlobRegex        ( GlobRegex.hs, interpreted )
-- [2 of 2] Compiling Main             ( ch08.exercises.hs, interpreted )
-- Ok, modules loaded: GlobRegex, Main.
-- *Main> upperRegex "d*"
-- "^D.*$"
-- *Main> upperRegex "d[abc*"
-- "^D[ABC**** Exception: unterminated character class
-- Note that the exception does not occur until upperRegex has processed
-- almost all of the regex; lazy processing does not catch the unterminated
-- character class until globToRegex is processing the end of the glob.
