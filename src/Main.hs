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

import Control.Monad.Writer
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Data.Text.Lazy.Encoding (encodeUtf8)
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
	| OptionNoLines

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
	, Option [] ["noline"] (NoArg OptionNoLines)
	  "Disable generating #line declarations in tangled code"
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

loomMap :: [(TL.Text, Loom)]
loomMap = [(loomName l, l) | l <- looms]

getLoom :: [Option] -> Loom
getLoom [] = loomLaTeX
getLoom (x:xs) = case x of
	OptionLoom name -> case lookup name loomMap of
		Just loom -> loom
		Nothing -> error $ "Unknown loom: " ++ show name
	_ -> getLoom xs

getEnableLines :: [Option] -> Bool
getEnableLines [] = True
getEnableLines (x:xs) = case x of
	OptionNoLines -> False
	_ -> getEnableLines xs

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
	let enableLines = getEnableLines options
	
	parsed <- parseInputs inputs
	case parsed of
		Left err -> do
			hPutStrLn stderr (formatError err)
			exitFailure
		Right blocks -> case getOutput options of
			Tangle -> case path of
				"" -> tangle debugTangle enableLines blocks
				_ -> tangle (realTangle path) enableLines blocks
			Weave -> let
				texts = execWriter $ loomWeave loom blocks
				in withFile path $ \h -> BL.hPut h $ encodeUtf8 texts

debugTangle :: TL.Text -> TL.Text -> IO ()
debugTangle path text = do
	putStr "\n"
	TLIO.putStrLn path
	putStrLn $ replicate (fromIntegral (TL.length path)) '='
	TLIO.putStr text

realTangle :: TL.Text -> TL.Text -> TL.Text -> IO ()
realTangle root path text = do
	let fullpath = combine (TL.unpack root) (TL.unpack path)
	createDirectoryIfMissing True $ takeDirectory fullpath
	let bytes = encodeUtf8 text
	withBinaryFile fullpath ReadWriteMode $ \h -> do
		equal <- fileContentsEqual h bytes
		unless equal $ do
			hSetFileSize h 0
			BL.hPut h bytes

fileContentsEqual :: Handle -> BL.ByteString -> IO Bool
fileContentsEqual h bytes = do
	hSeek h SeekFromEnd 0
	size <- hTell h
	hSeek h AbsoluteSeek 0
	
	if size /= toInteger (BL.length bytes)
		then return False
		else do
			-- FIXME: 'Int' overflow?
			contents <- BL.hGet h (fromInteger size)
			hSeek h AbsoluteSeek 0
			return $ bytes == contents

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

catEithers :: [Either e a] -> Either e [a]
catEithers = cat' [] where
	cat' acc [] = Right $ reverse acc
	cat' acc (e:es) = case e of
		Left err -> Left err
		Right x -> cat' (x : acc) es