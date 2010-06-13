-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
-- 
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Anansi
import Anansi.Loom.Debug
import Anansi.Loom.XHTML
import Anansi.Loom.LaTeX
import Anansi.Util

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO hiding (withFile)

data Output = Tangle | Weave

data Option
	= OptionOutput Output
	| OptionOutputPath TL.Text

optionInfo :: [OptDescr Option]
optionInfo =
	[ Option ['t'] ["tangle"] (NoArg (OptionOutput Tangle))
	  "Generate tangled source code (default)"
	, Option ['w'] ["weave"] (NoArg (OptionOutput Weave))
	  "Generate woven markup"
	, Option ['o'] ["out", "output"] (ReqArg (OptionOutputPath . TL.pack) "PATH")
	  "Output path (directory for tangle, file for weave)"
	]

usage :: String -> String
usage name = "Usage: " ++ name ++ " [OPTION...]"

getOutput :: [Option] -> Output
getOutput [] = Tangle
getOutput (x:xs) = case x of
	OptionOutput o -> o
	_ -> getOutput xs

getPath :: [Option] -> TL.Text
getPath [] = ""
getPath (x:xs) = case x of
	OptionOutputPath p -> p
	_ -> getPath xs

withFile :: TL.Text -> (Handle -> IO a) -> IO a
withFile path io = case path of
	"" -> io stdout
	_ -> withBinaryFile (TL.unpack path) WriteMode io

main :: IO ()
main = do
	args <- getArgs
	let (options, inputs, errors) = getOpt Permute optionInfo args
	unless (null errors) $ do
		name <- getProgName
		hPutStrLn stderr $ concat errors
		hPutStrLn stderr $ usageInfo (usage name) optionInfo
		exitFailure
	
	blocks <- concatMapM (parseFile . TL.pack) inputs
	
	let path = getPath options
	case getOutput options of
		Tangle -> case path of
			"" -> tangle debugTangle blocks
			_ -> tangle (realTangle path) blocks
		Weave -> withFile path $ \h -> weave loomLaTeX (BL.hPut h) blocks

debugTangle :: TL.Text -> ((TL.Text -> IO ()) -> IO a) -> IO a
debugTangle path io = do
	putStr "\n"
	putStrLn $ TL.unpack path
	putStrLn $ replicate (fromIntegral (TL.length path)) '='
	io (putStr . TL.unpack)

realTangle :: TL.Text -> TL.Text -> ((TL.Text -> IO ()) -> IO a) -> IO a
realTangle root path io = do
	let fullpath = combine (TL.unpack root) (TL.unpack path)
	createDirectoryIfMissing True $ takeDirectory fullpath
	withBinaryFile fullpath WriteMode $ \h -> io (hPutStr h . TL.unpack)
