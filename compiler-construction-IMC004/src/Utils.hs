module Utils where

import Text.Printf

import Text.ParserCombinators.UU.Core
import Text.ParserCombinators.UU.BasicInstances

import Parser
import CompileError

execParser_ :: SplParser a -> String -> (a, [Error LineColPos])
execParser_ p = parse_h ((,) <$> p <*> pEnd) . createStr (LineColPos 0 0 0)

runParser_ :: String -> SplParser a -> String -> Either CompileError a
runParser_ inputName p s | (a,b) <- execParser_ p s = do
    if null b
    then Right a
    else Left $ ParseError (printf "Failed parsing '%s' :\n%s\n" inputName (pruneError s b))
         -- We do 'pruneError' above because otherwise you can end
         -- up reporting huge correction streams, and that's
         -- generally not helpful... but the pruning does discard info...
    where -- | Produce a single simple, user-friendly error message
          pruneError :: String -> [Error LineColPos] -> String
          pruneError _ [] = ""
          pruneError _ (DeletedAtEnd x     : _) = printf "Unexpected '%s' at end." x
          pruneError s (Inserted _ pos exp : _) = prettyError s exp pos
          pruneError s (Deleted  _ pos exp : _) = prettyError s exp pos
          prettyError :: String -> [String] -> LineColPos -> String
          prettyError s exp p@(LineColPos line c abs) = printf "Expected %s at %s :\n%s\n%s\n%s\n"
                                                           (show_expecting p exp)
                                                           (show p)
                                                           aboveString
                                                           inputFrag
                                                           belowString
                             where
                                s' = map (\c -> if c=='\n' || c=='\r' || c=='\t' then ' ' else c) s
                                aboveString = replicate 30 ' ' ++ "v"
                                belowString = replicate 30 ' ' ++ "^"
                                inputFrag   = replicate (30 - abs) ' ' ++ (take 71 $ drop (abs - 30) s')

-- unsafe runParser
parse :: (SplParser a) -> String -> a
parse parser prog = unRight $ runParser_ "" parser prog

unRight :: Either a b -> b
unRight (Right x) = x
unRight _ = undefined
