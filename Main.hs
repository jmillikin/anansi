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
import Anansi.Loom.HTML
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
	| OptionLoom TL.Text

optionInfo :: [OptDescr Option]
optionInfo =
	[ Option ['t'] ["tangle"] (NoArg (OptionOutput Tangle))
	  "Generate tangled source code (default)"
	, Option ['w'] ["weave"] (NoArg (OptionOutput Weave))
	  "Generate woven markup"
	, Option ['o'] ["out", "output"] (ReqArg (OptionOutputPath . TL.pack) "PATH")
	  "Output path (directory for tangle, file for weave)"
	, Option ['l'] ["loom"] (ReqArg (OptionLoom . TL.pack) "NAME")
	  "Which loom should be used to weave output"
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

looms :: Monad m => [(TL.Text, Loom m)]
looms = [(loomName l, l) | l <- [loomLaTeX, loomHTML, loomDebug]]

getLoom :: Monad m => [Option] -> Loom m
getLoom [] = loomLaTeX
getLoom (x:xs) = case x of
	OptionLoom name -> case lookup name looms of
		Just loom -> loom
		Nothing -> error $ "Unknown loom: " ++ show name
	_ -> getLoom xs

main :: IO ()
main = do
	args <- getArgs
	let (options, inputs, errors) = getOpt Permute optionInfo args
	unless (null errors) $ do
		name <- getProgName
		hPutStrLn stderr $ concat errors
		hPutStrLn stderr $ usageInfo (usage name) optionInfo
		exitFailure
	
	let path = getPath options
	let loom = getLoom options
	
	parsed <- parseInputs inputs
	case parsed of
		Left err -> hPutStrLn stderr (formatError err)
		Right blocks -> case getOutput options of
			Tangle -> case path of
				"" -> tangle debugTangle blocks
				_ -> tangle (realTangle path) blocks
			Weave -> withFile path $ \h -> loomWeave loom (BL.hPut h) blocks

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

parseInputs :: [String] -> IO (Either ParseError [Block])
parseInputs inputs = do
	eithers <- mapM (parseFile . TL.pack) inputs
	return $ case catEithers eithers of
		Left err -> Left err
		Right bs -> Right $ concat bs

formatError :: ParseError -> String
formatError err = concat [filename, ":", line, ": error: ", message] where
	pos = parseErrorPosition err
	filename = TL.unpack $ positionFile pos
	line = show $ positionLine pos
	message = TL.unpack $ parseErrorMessage err
