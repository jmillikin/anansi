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

import           Control.Applicative
import           Control.Monad.Writer
import           Data.ByteString (ByteString)
import qualified Data.ByteString
import           Data.List (sortBy)
import qualified Data.Map
import           Data.Ord (comparing)
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text
import           Data.Version (showVersion)
import qualified Filesystem
import           Filesystem.Path (FilePath)
import qualified Filesystem.Path.CurrentOS as FP
import           Options
import           System.Argv0 (getArgv0)
import           System.Environment (getArgs)
import           System.Exit (exitFailure, exitSuccess)
import           System.IO hiding (withFile, FilePath)

import           Anansi.Parser
import           Anansi.Tangle
import           Anansi.Types

import           Paths_anansi (version)

data MainOptions = MainOptions
	{ optShowVersion :: Bool
	, optShowNumericVersion :: Bool
	, optOutputPath :: FilePath
	}

data TangleOptions = TangleOptions
	{ optNoLines :: Bool
	}

data WeaveOptions = WeaveOptions

instance Options MainOptions where
	defineOptions = pure MainOptions
		<*> simpleOption "version" False
		    "Display information about this program, then exit"
		<*> simpleOption "numeric-version" False
		    "Display the numeric version of Anansi, then exit."
		<*> defineOption optionType_filePath (\o -> o
			{ optionShortFlags = ['o']
			, optionLongFlags = ["output", "out"]
			, optionDescription = "Output path (a directory when tangling, a file when weaving)."
			})

instance Options TangleOptions where
	defineOptions = pure TangleOptions
		<*> simpleOption "disable-line-pragmas" False
		    "Disable generating #line pragmas in tangled code. This works\
		    \ around a bug in Haddock."

instance Options WeaveOptions where
	defineOptions = pure WeaveOptions

optionType_filePath :: OptionType FilePath
optionType_filePath = optionType "path" FP.empty
	(Right . FP.decodeString)
	(show . either Data.Text.unpack Data.Text.unpack . FP.toText)

getUsage :: IO String
getUsage = do
	argv0 <- getArgv0
	let name = either Data.Text.unpack Data.Text.unpack (FP.toText argv0)
	return ("Usage: " ++ name ++ " [OPTION...] <tangle|weave> input-file\n")

loomInfo :: Data.Map.Map Text Loom -> String
loomInfo looms = unlines lines' where
	loomNames = sortBy nameKey (Data.Map.keys looms)
	lines' = ["Available looms are:"] ++ indent 2 loomNames
	indent n = map (\x -> replicate n ' ' ++ Data.Text.unpack x)
	
	-- sort loom names so anansi-foo comes after anansi.bar
	nameKey = comparing (Data.Text.split (== '.'))

withFile :: FilePath -> (Handle -> IO a) -> IO a
withFile path io = if FP.null path
	then io stdout
	else Filesystem.withFile path WriteMode io

tangleMain :: MainOptions -> TangleOptions -> [String] -> IO ()
tangleMain mainOpts opts args = do
	checkVersionOpts mainOpts
	(_, doc) <- parseInput args
	let enableLines = not (optNoLines opts)
	case optOutputPath mainOpts of
		"" -> tangle debugTangle enableLines doc
		path -> tangle (realTangle path) enableLines doc

weaveMain :: Data.Map.Map Text Loom -> MainOptions -> WeaveOptions -> [String] -> IO ()
weaveMain looms mainOpts _ args = do
	checkVersionOpts mainOpts
	(inputName, doc) <- parseInput args
	loomName <- case documentLoomName doc of
		Just name -> return name
		Nothing -> do
			hPutStrLn stderr ("Document "
			 ++ show inputName
			 ++ " does't specify a loom (use :loom).")
			hPutStrLn stderr (loomInfo looms)
			exitFailure
	loom <- case Data.Map.lookup loomName looms of
		Just loom -> return loom
		Nothing -> do
			hPutStrLn stderr ("Loom "
			 ++ show loomName
			 ++ " not recognized.")
			hPutStrLn stderr (loomInfo looms)
			exitFailure
	withFile (optOutputPath mainOpts) (\h -> Data.ByteString.hPut h (weave loom doc))

checkVersionOpts :: MainOptions -> IO ()
checkVersionOpts opts = do
	when (optShowVersion opts) $ do
		putStrLn ("anansi_" ++ showVersion version)
		exitSuccess
	when (optShowNumericVersion opts) $ do
		putStrLn (showVersion version)
		exitSuccess

parseInput :: [String] -> IO (String, Document)
parseInput [] = do
	getUsage >>= hPutStrLn stderr
	hPutStrLn stderr "An input file is required.\n"
	exitFailure
parseInput [inputName] = do
	parsed <- parse Filesystem.readFile (fromString inputName)
	case parsed of
		Left err -> do
			hPutStrLn stderr ("Parse error while processing document " ++ show inputName)
			hPutStrLn stderr (formatError err)
			exitFailure
		Right doc -> return (inputName, doc)
parseInput _ = do
	getUsage >>= hPutStrLn stderr
	hPutStrLn stderr "More than one input file provided.\n"
	exitFailure

-- | Run Anansi with the provided looms. Loom names are namespaced by their
-- package name, such as @\"anansi.noweb\"@ or @\"anansi-hscolour.html\"@.
-- If your looms aren't available on Hackage, a Java-style name such as
-- @\"com.mycompany.myformat\"@ is a good alternative.
defaultMain :: Data.Map.Map Text Loom -> IO ()
defaultMain looms = do
	let subcommands =
		[ subcommand "tangle" tangleMain
		, subcommand "weave" (weaveMain looms)
		]
	
	argv <- getArgs
	let parsed = parseSubcommand subcommands argv
	case parsedSubcommand parsed of
		Just cmd -> cmd
		Nothing -> case parsedError parsed of
			Just err -> do
				getUsage >>= hPutStrLn stderr
				hPutStr stderr (parsedHelp parsed)
				hPutStrLn stderr (loomInfo looms)
				hPutStrLn stderr err
				exitFailure
			Nothing -> do
				getUsage >>= hPutStrLn stdout
				hPutStr stdout (parsedHelp parsed)
				hPutStrLn stdout (loomInfo looms)
				exitSuccess

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
