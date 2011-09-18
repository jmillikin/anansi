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
import           Data.Version (showVersion)
import qualified Filesystem
import           Filesystem.Path (FilePath)
import qualified Filesystem.Path.CurrentOS as FP
import           System.Argv0 (getArgv0)
import           System.Console.GetOpt
import           System.Environment (getArgs)
import           System.Exit (exitFailure, exitSuccess)
import           System.IO hiding (withFile, FilePath)

import           Anansi.Parser
import           Anansi.Tangle
import           Anansi.Types

import           Paths_anansi (version)

data Mode = Tangle | Weave
	deriving (Eq)

data Option
	= OptionHelp
	| OptionVersion
	| OptionNumericVersion
	| OptionOutputPath FilePath
	| OptionNoLines
	deriving (Eq)

optionInfo :: [OptDescr Option]
optionInfo =
	[ Option ['h'] ["help"] (NoArg OptionHelp)
	  "Display this help, then exit."
	, Option [] ["version"] (NoArg OptionVersion)
	  "Display information about this program, then exit"
	, Option [] ["numeric-version"] (NoArg OptionNumericVersion)
	  "Display the numeric version of Anansi, then exit."
	, Option ['o'] ["out", "output"] (ReqArg (OptionOutputPath . fromString) "PATH")
	  "Output path (a directory when tangling, a file when weaving)."
	, Option [] ["disable-line-pragmas"] (NoArg OptionNoLines)
	  "Disable generating #line pragmas in tangled code. This works\
	  \ around a bug in Haddock."
	]

showUsage :: [String] -> IO a
showUsage errors = do
	argv0 <- getArgv0
	let name = either Data.Text.unpack Data.Text.unpack (FP.toText argv0)
	let info = usageInfo
		("Usage: " ++ name ++ " [OPTION...] <tangle|weave> input-file\n")
		optionInfo
	if null errors
		then do
			putStrLn info
			exitSuccess
		else do
			hPutStrLn stderr (concat errors)
			hPutStrLn stderr info
			exitFailure

getPath :: [Option] -> FilePath
getPath opts = case reverse [p | OptionOutputPath p <- opts] of
	[] -> ""
	(path:_) -> path

withFile :: FilePath -> (Handle -> IO a) -> IO a
withFile path io = if FP.null path
	then io stdout
	else Filesystem.withFile path WriteMode io

-- | Run Anansi with the provided looms. Loom names are namespaced by their
-- package name, such as @\"anansi.noweb\"@ or @\"anansi-hscolour.html\"@.
-- If your looms aren't available on Hackage, a Java-style name such as
-- @\"com.mycompany.myformat\"@ is a good alternative.
defaultMain :: Data.Map.Map Text Loom -> IO ()
defaultMain looms = do
	args <- getArgs
	let (options, inputs, errors) = getOpt Permute optionInfo args
	unless (null errors) (showUsage errors)
	when (OptionHelp `elem` options) (showUsage [])
	when (OptionVersion `elem` options) $ do
		putStrLn ("anansi_" ++ showVersion version)
		exitSuccess
	when (OptionNumericVersion `elem` options) $ do
		putStrLn (showVersion version)
		exitSuccess
	
	(mode, input) <- case inputs of
		[] -> showUsage ["A mode (either 'tangle' or 'weave') is required.\n"]
		[_] -> showUsage ["An input file is required.\n"]
		[raw_mode, input] -> do
			mode <- case raw_mode of
				"tangle" -> return Tangle
				"weave" -> return Weave
				_ -> showUsage ["Unrecognized mode: " ++ show raw_mode ++ ".\n"]
			return (mode, fromString input)
		_ -> showUsage ["More than one input file provided.\n"]
	
	-- used for error messages
	let inputName = either id id (FP.toText input)
	
	let path = getPath options
	let enableLines = not (OptionNoLines `elem` options)
	
	parsedDoc <- parse Filesystem.readFile input
	doc <- case parsedDoc of
		Left err -> do
			hPutStrLn stderr ("Parse error while processing document " ++ show inputName)
			hPutStrLn stderr (formatError err)
			exitFailure
		Right x -> return x
	
	case mode of
		Tangle -> case path of
			"" -> tangle debugTangle enableLines doc
			_ -> tangle (realTangle path) enableLines doc
		Weave -> do
			loomName <- case documentLoomName doc of
				Just name -> return name
				Nothing -> do
					hPutStrLn stderr ("Document " ++ show inputName ++ " does't specify a loom (use :loom).")
					exitFailure
			
			loom <- case Data.Map.lookup loomName looms of
				Just loom -> return loom
				Nothing -> do
					hPutStrLn stderr ("Loom " ++ show loomName ++ " not recognized.")
					exitFailure
			
			withFile path (\h -> Data.ByteString.hPut h (weave loom doc))

debugTangle :: FilePath -> ByteString -> IO ()
debugTangle path bytes = do
	let strPath = either Data.Text.unpack Data.Text.unpack (FP.toText path)
	putStr "\n"
	putStrLn strPath
	putStrLn (replicate (fromIntegral (length strPath)) '=')
	Data.ByteString.putStr bytes

realTangle :: FilePath -> FilePath -> ByteString -> IO ()
realTangle root path bytes = do
	let fullpath = FP.append root path
	Filesystem.createTree (FP.parent fullpath)
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
formatError err = concat [filename, ":", line, ": ", message] where
	pos = parseErrorPosition err
	filename = either Data.Text.unpack Data.Text.unpack (FP.toText (positionFile pos))
	line = show (positionLine pos)
	message = Data.Text.unpack (parseErrorMessage err)
