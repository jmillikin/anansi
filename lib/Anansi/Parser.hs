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

module Anansi.Parser
	( ParseError (..)
	, parse
	) where

import           Prelude hiding (FilePath, lines, readFile)

import           Control.Applicative ((<|>), (<$>))
import           Control.Monad (liftM)
import           Control.Monad.Error (ErrorT, Error, runErrorT, throwError)
import           Control.Monad.Trans (lift)
import           Data.ByteString (ByteString)
import           Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import           Data.Sequence ((|>))
import           Data.Text (Text)
import qualified Data.Text
import           Data.Text.Encoding (decodeUtf8)
import           Filesystem.Path (FilePath)
import qualified Filesystem.Path.CurrentOS as Path
import qualified Text.Parsec as P

import           Anansi.Types

data Line
	= LineCommand Position Command
	| LineText Position Text
	deriving (Show)

data Command
	= CommandInclude Text
	| CommandFile Text
	| CommandDefine Text
	| CommandOption Text Text
	| CommandLoom Text
	| CommandColon
	| CommandEndBlock
	| CommandComment
	deriving (Show)

-- | ignore me
instance Error ParseError

parse :: Monad m
      => (FilePath -> m ByteString)
      -> FilePath
      -> m (Either ParseError Document)
parse readFile root = runErrorT (gen root >>= parseDocument) where
	gen path = do
		bytes <- lift (readFile path)
		lines <- getLines path bytes
		concatMapM (resolveIncludes path) lines
	
	relative x y = Path.append (Path.parent x) (Path.fromText y)
	
	resolveIncludes parent line = case line of
		LineCommand _ (CommandInclude path) -> gen (relative parent path)
		_ -> return [line]

getLines :: Monad m => FilePath -> ByteString -> ErrorT ParseError m [Line]
getLines path bytes = do
	let contents = Data.Text.unpack (decodeUtf8 bytes)
	parseResult <- P.runParserT parseLines () (Path.encodeString path) contents
	case parseResult of
		Right lines -> return lines
		Left err -> let
			msg = Data.Text.pack ("parseFile (internal error): " ++ show err)
			in throwError (ParseError (Position path 0) msg)

parseDocument :: Monad m => [Line] -> ErrorT ParseError m Document
parseDocument = loop (Seq.empty, Map.empty, Nothing) where
	loop (blocks, opts, loom) [] = return (Document (toList blocks) opts loom)
	loop acc (line:lines) = do
		(acc', lines') <- step acc line lines
		loop acc' lines'
	
	step (bs, opts, loom) line lines = case line of
		LineText _ text -> return ((bs |> BlockText text, opts, loom), lines)
		LineCommand pos cmd -> case cmd of
			CommandFile path -> do
				(block, lines') <- parseContent pos (BlockFile path) lines
				return ((bs |> block, opts, loom), lines')
			CommandDefine name -> do
				(block, lines') <- parseContent pos (BlockDefine name) lines
				return ((bs |> block, opts, loom), lines')
			CommandOption key value -> return ((bs, Map.insert key value opts, loom), lines)
			CommandColon -> return ((bs |> BlockText ":", opts, loom), lines)
			CommandComment -> return ((bs, opts, loom), lines)
			CommandLoom loomName -> return ((bs, opts, Just loomName), lines)
			CommandEndBlock -> let
				msg = "Unexpected block terminator"
				in throwError (ParseError pos msg)
			CommandInclude _ -> let
				msg = "Unexpected CommandInclude (internal error)"
				in throwError (ParseError pos msg)

type ParserM m = P.ParsecT String () (ErrorT ParseError m)

untilChar :: Monad m => Char -> ParserM m Text
untilChar c = Data.Text.pack <$> P.manyTill P.anyChar (P.try (P.char c))

parseError :: Monad m => Position -> Text -> ParserM m a
parseError pos msg = P.mkPT (\_ -> throwError (ParseError pos msg))

getPosition :: Monad m => ParserM m Position
getPosition = do
	pos <- P.getPosition
	return (Position
		(Path.decodeString (P.sourceName pos))
		(toInteger (P.sourceLine pos)))

parseLines :: Monad m => ParserM m [Line]
parseLines = do
	lines' <- P.many parseLine
	P.eof
	return lines'

parseLine :: Monad m => ParserM m Line
parseLine = command <|> text where
	command = do
		void (P.char ':')
		pos <- getPosition
		LineCommand pos <$> parseCommand
	
	text = do
		pos <- getPosition
		line <- untilChar '\n'
		return (LineText pos (Data.Text.append line "\n"))

parseCommand :: Monad m => ParserM m Command
parseCommand = parsed where
	string = P.try . P.string
	parsed = P.choice [file, include, define, option, loom, colon, comment, endBlock]
	file = do
		void (string "file " <|> string "f ")
		CommandFile <$> untilChar '\n'
	
	include = do
		void (string "include " <|> string "i ")
		CommandInclude <$> untilChar '\n'
	
	define = do
		void (string "define " <|> string "d ")
		name <- untilChar '\n'
		if Data.Text.any (== '|') name
			then do
				pos <- getPosition
				parseError
					(pos { positionLine = positionLine pos - 1})
					(Data.Text.pack ("Invalid macro name: " ++ show name))
			else return (CommandDefine name)
	
	option = do
		void (string "option ")
		eitherOption <- let
			valid = P.try $ do
				key <- P.manyTill (P.satisfy (/= '\n')) (P.try (P.char '='))
				value <- untilChar '\n'
				return (Right (Data.Text.pack key, value))
			invalid = do
				line <- untilChar '\n'
				return (Left line)
			in valid P.<|> invalid
		case eitherOption of
			Left badLine -> do
				pos <- getPosition
				parseError
					(pos { positionLine = positionLine pos - 1})
					(Data.Text.pack ("Invalid option: " ++ show badLine))
			Right (key, value) -> return (CommandOption key value)
	
	loom = do
		void (string "loom ")
		CommandLoom <$> untilChar '\n'
	
	colon = do
		void (P.char ':')
		return CommandColon
	
	comment = do
		void (P.char '#')
		void (untilChar '\n')
		return CommandComment
	
	endBlock = do
		line <- untilChar '\n'
		if Data.Text.all isSpace line
			then return CommandEndBlock
			else do
				pos <- getPosition
				let msg = Data.Text.pack ("unknown command: " ++ show (Data.Text.append ":" line))
				parseError (pos { positionLine = positionLine pos - 1 }) msg

isSpace :: Char -> Bool
isSpace ' ' = True
isSpace '\t' = True
isSpace _ = False

parseContent :: Monad m => Position -> ([Content] -> Block) -> [Line] -> ErrorT ParseError m (Block, [Line])
parseContent start block = loop [] where
	loop _ [] = unterminated
	loop acc (line:xs) = case line of
		LineText pos text -> do
			parsed <- parse' pos text
			loop (parsed : acc) xs
		LineCommand _ CommandEndBlock -> return (block (reverse acc), xs)
		LineCommand _ _ -> unterminated
	
	parse' pos text = do
		res <- P.runParserT (parser pos) () "" (Data.Text.unpack text)
		case res of
			Right content -> return content
			Left _ -> let
				trimmed = Data.Text.dropWhileEnd (== '\n') text
				msg = Data.Text.pack ("Invalid content line: " ++ show trimmed)
				in throwError (ParseError pos msg)
	
	unterminated = throwError (ParseError start "Unterminated content block")
	
	parser pos = do
		content <- contentMacro pos <|> contentText pos
		P.optional (P.char '\n')
		P.eof
		return content
	
	contentMacro pos = do
		(indent, c) <- P.try $ do
			indent <- P.many (P.satisfy isSpace)
			void (P.char '|')
			c <- P.satisfy (not . isSpace)
			return (indent, c)
		name <- untilChar '|'
		return (ContentMacro pos (Data.Text.pack indent) (Data.Text.strip (Data.Text.cons c name)))
	
	contentText pos = do
		text <- untilChar '\n'
		return (ContentText pos text)

void :: Monad m => m a -> m ()
void m = m >> return ()

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)
