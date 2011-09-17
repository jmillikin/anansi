{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010-2011 John Millikin <jmillikin@gmail.com>
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

module Anansi.Main
	( defaultMain
	) where

import           Prelude hiding (FilePath)

import           Control.Monad.Writer
import           Data.ByteString (ByteString)
import qualified Data.ByteString
import qualified Data.Map
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text
import qualified Data.Text.IO
import           Data.Text.Encoding (encodeUtf8)
import           Data.Version (showVersion)
import qualified Filesystem
import           Filesystem.Path (FilePath)
import qualified Filesystem.Path.CurrentOS as FP
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO hiding (withFile, FilePath)

import           Anansi.Parser
import           Anansi.Tangle
import           Anansi.Types

import           Paths_anansi (version)

data Output = Tangle | Weave
	deriving (Eq)

data Option
	= OptionOutput Output
	| OptionOutputPath FilePath
	| OptionLoom Text
	| OptionNoLines
	| OptionVersion
	| OptionHelp
	deriving (Eq)

optionInfo :: [OptDescr Option]
optionInfo =
	[ Option ['h'] ["help"] (NoArg OptionHelp) ""
	, Option [] ["version"] (NoArg OptionVersion) ""
	, Option ['t'] ["tangle"] (NoArg (OptionOutput Tangle))
	  "Generate tangled source code (default)"
	, Option ['w'] ["weave"] (NoArg (OptionOutput Weave))
	  "Generate woven markup"
	, Option ['o'] ["out", "output"] (ReqArg (OptionOutputPath . fromString) "PATH")
	  "Output path (directory for tangle, file for weave)"
	, Option ['l'] ["loom"] (ReqArg (OptionLoom . Data.Text.pack) "NAME")
	  "Which loom should be used to weave output"
	, Option [] ["noline"] (NoArg OptionNoLines)
	  "Disable generating #line pragmas in tangled code"
	]

usage :: String -> String
usage name = "Usage: " ++ name ++ " [OPTION...] <input file>"

getOutput :: [Option] -> Output
getOutput [] = Tangle
getOutput (x:xs) = case x of
	OptionOutput o -> o
	_ -> getOutput xs

getPath :: [Option] -> FilePath
getPath [] = ""
getPath (x:xs) = case x of
	OptionOutputPath p -> p
	_ -> getPath xs

withFile :: FilePath -> (Handle -> IO a) -> IO a
withFile path io = if FP.null path
	then io stdout
	else Filesystem.withFile path WriteMode io

getLoomName :: [Option] -> Maybe Text
getLoomName = loop where
	loop [] = Nothing
	loop (x:xs) = case x of
		OptionLoom name -> Just name
		_ -> loop xs

getEnableLines :: [Option] -> Bool
getEnableLines [] = True
getEnableLines (x:xs) = case x of
	OptionNoLines -> False
	_ -> getEnableLines xs

defaultMain :: Data.Map.Map Text Loom -> IO ()
defaultMain looms = do
	args <- getArgs
	let (options, inputs, errors) = getOpt Permute optionInfo args
	unless (null errors) $ do
		name <- getProgName
		hPutStrLn stderr (concat errors)
		hPutStrLn stderr (usageInfo (usage name) optionInfo)
		exitFailure
	
	when (OptionHelp `elem` options) $ do
		name <- getProgName
		putStrLn (usageInfo (usage name) optionInfo)
		exitSuccess
		
	when (OptionVersion `elem` options) $ do
		putStrLn ("anansi_" ++ showVersion version)
		exitSuccess
	
	input <- case inputs of
		[] -> do
			hPutStrLn stderr "An input file is required."
			exitFailure
		[x] -> return (fromString x)
		_ -> do
			hPutStrLn stderr "More than one input file provided."
			exitFailure
	
	let path = getPath options
	let enableLines = getEnableLines options
	
	parsedDoc <- parse Filesystem.readFile input
	doc <- case parsedDoc of
		Left err -> do
			hPutStrLn stderr (formatError err)
			exitFailure
		Right x -> return x
	
	case getOutput options of
		Tangle -> case path of
			"" -> tangle debugTangle enableLines (documentBlocks doc)
			_ -> tangle (realTangle path) enableLines (documentBlocks doc)
		Weave -> do
			loomName <- case getLoomName options of
				Just name -> return name
				Nothing -> case documentLoomName doc of
					Just name -> return name
					Nothing -> do
						hPutStrLn stderr "No loom specified (use :loom or --loom)."
						exitFailure
			
			loom <- case Data.Map.lookup loomName looms of
				Just loom -> return loom
				Nothing -> do
					hPutStrLn stderr ("Loom " ++ show loomName ++ " not recognized.")
					exitFailure
			
			withFile path (\h -> Data.ByteString.hPut h (encodeUtf8 (weave loom doc)))

debugTangle :: FilePath -> Text -> IO ()
debugTangle path text = do
	let strPath = either Data.Text.unpack Data.Text.unpack (FP.toText path)
	putStr "\n"
	putStrLn strPath
	putStrLn (replicate (fromIntegral (length strPath)) '=')
	Data.Text.IO.putStr text

realTangle :: FilePath -> FilePath -> Text -> IO ()
realTangle root path text = do
	let fullpath = FP.append root path
	Filesystem.createTree (FP.parent fullpath)
	let bytes = encodeUtf8 text
	Filesystem.withFile fullpath ReadWriteMode $ \h -> do
		equal <- fileContentsEqual h bytes
		unless equal $ do
			hSetFileSize h 0
			Data.ByteString.hPut h bytes

fileContentsEqual :: Handle -> ByteString -> IO Bool
fileContentsEqual h bytes = do
	hSeek h SeekFromEnd 0
	size <- hTell h
	hSeek h AbsoluteSeek 0
	
	if size /= toInteger (Data.ByteString.length bytes)
		then return False
		else do
			-- FIXME: 'Int' overflow?
			contents <- Data.ByteString.hGet h (fromInteger size)
			hSeek h AbsoluteSeek 0
			return (bytes == contents)

formatError :: ParseError -> String
formatError err = concat [filename, ":", line, ": error: ", message] where
	pos = parseErrorPosition err
	filename = either Data.Text.unpack Data.Text.unpack (FP.toText (positionFile pos))
	line = show (positionLine pos)
	message = Data.Text.unpack (parseErrorMessage err)
