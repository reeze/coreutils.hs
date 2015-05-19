module Main where

import System.IO
import System.Environment
import System.Console.GetOpt
import GHC.IO.Handle.FD

main = do
	args <- getArgs
	let handler = case args of
		[]    -> return stdin
		("-":xs) -> return stdin
		(file:xs) -> openFile file ReadMode
	handler >>= (flip hSetBinaryMode) True
	content <- handler >>= hGetContents
	putStr content