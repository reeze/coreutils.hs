module Main where

import System.IO
import System.Environment
import System.Console.GetOpt
import Control.Applicative

data Opts = Opts { numberNoneBlank
				 , displayNonePrint
				 , displayEndDollar
				 , numberOutLines
				 , squeezeEmptyLines
				 , displayTab
				 , disableOutputBuffer
				 , displayControl :: Bool
				 }

options:: [OptDescr (Opts -> Opts)]
options = 
	[ Option ['b'] ["number non-blank"]
		(NoArg (\o -> o {numberNoneBlank = True}))
		"Number the non-blank output lines, starting at 1."
	, Option ['e'] ["end dollar"]
		(NoArg (\o -> o {displayNonePrint = True, displayEndDollar = True}))
		"Display non-printing characters (see the -v option), and display a dollar sign (`$') at the end of each line."
	, Option ['n'] ["number lines"]
		(NoArg (\o -> o {numberOutLines = True}))
		"Number the output lines, starting at 1."
	, Option ['s'] ["squeeze"]
		(NoArg (\o -> o {squeezeEmptyLines = True}))
		"Squeeze multiple adjacent empty lines, causing the output to be single spaced."
	, Option ['t'] ["diplay tabs"]
		(NoArg (\o -> o {displayNonePrint = True, displayTab = True}))
		"Display non-printing characters (see the -v option), and display tab characters as `^I'."
	, Option ['u'] ["disable ob"]
		(NoArg (\o -> o {disableOutputBuffer = True}))
		"Disable output buffering."
	, Option ['v'] ["visible"]
		(NoArg (\o -> o {disableOutputBuffer = True}))
		"Display non-printing characters so they are visible."
	]


printContent::Opts -> String -> IO ()
printContent opts text = putStr text

cat::[String] -> IO (String)
cat [] = cat ["-"]
cat ["-"] = getContents
cat fs =  foldr (\f text -> (++) <$> readFile f <*> text) (return "") fs

parseOpts :: [String] -> IO ([Opts -> Opts], [String])
parseOpts args = 
	case getOpt RequireOrder options args of
		(opts, files, []) -> return (opts, files)
		(_, _, errs) -> fail (concat errs ++ header)
	where
		header = "usage: cat [-benstuv] [file ...]"

allOpts :: Bool -> Opts
allOpts b = Opts { numberNoneBlank  = b
				 , displayNonePrint = b
				 , displayEndDollar = b
				 , numberOutLines   = b
				 , squeezeEmptyLines= b
				 , displayTab       = b
				 , disableOutputBuffer = b
				 , displayControl   = b
				 }

processOpts :: [Opts -> Opts] -> Opts
processOpts [] = allOpts False
processOpts opts = foldl (flip ($)) (allOpts False) opts

main = do
	args <- getArgs
	(optList, files) <- parseOpts args
	let opts = processOpts optList
	cat files >>= printContent opts