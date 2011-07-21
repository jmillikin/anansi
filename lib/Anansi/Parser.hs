{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

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
	, parseFile
	) where

import           Prelude hiding (FilePath)

import           Control.Applicative ((<|>), (<$>))
import qualified Control.Exception as E
import qualified Control.Monad.State as S
import           Control.Monad.Trans (lift)
import           Data.List (unfoldr)
import           Data.Map (Map)
import qualified Data.Map
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text
import           Data.Text.Encoding (decodeUtf8)
import           Data.Typeable (Typeable)
import qualified System.File
import           System.FilePath (FilePath)
import qualified System.FilePath.CurrentOS as FP
import qualified Text.Parsec as P

import           Anansi.Types
import           Anansi.Util

data ParseError = ParseError
	{ parseErrorPosition :: Position
	, parseErrorMessage :: Text
	}
	deriving (Show)

-- too lazy to write proper error handling
data ParseExc = ParseExc ParseError
	deriving (Typeable, Show)

instance E.Exception ParseExc

data Command
	= CommandInclude Text
	| CommandFile Text
	| CommandDefine Text
	| CommandOption Text Text
	| CommandColon
	| CommandEndBlock
	| CommandComment
	deriving (Show)

data Line
	= LineCommand Position Command
	| LineText Position Text
	deriving (Show)

untilChar :: Char -> P.Parsec String u Text
untilChar c = Data.Text.pack <$> P.manyTill P.anyChar (P.try (P.char c))

getPosition :: Monad m => P.ParsecT s u m Position
getPosition = do
	pos <- P.getPosition
	return (Position (fromString (P.sourceName pos)) (toInteger (P.sourceLine pos)))

parseLines :: P.Parsec String u [Line]
parseLines = do
	lines' <- P.many parseLine
	P.eof
	return lines'

parseLine :: P.Parsec String u Line
parseLine = command <|> text where
	command = do
		void (P.char ':')
		pos <- getPosition
		LineCommand pos <$> parseCommand
	
	text = do
		pos <- getPosition
		line <- untilChar '\n'
		return (LineText pos (Data.Text.append line "\n"))

parseCommand :: P.Parsec String u Command
parseCommand = parsed where
	string = P.try . P.string
	parsed = P.choice [file, include, define, option, colon, comment, endBlock]
	file = do
		void (string "file " <|> string "f ")
		CommandFile <$> untilChar '\n'
	
	include = do
		void (string "include " <|> string "i ")
		CommandInclude <$> untilChar '\n'
	
	define = do
		void (string "define " <|> string "d ")
		-- TODO: verify no '|' in name
		CommandDefine <$> untilChar '\n'
	
	option = do
		void (string "option " <|> string "o ")
		key <- P.manyTill P.anyChar (P.try (P.satisfy isSpace))
		P.skipMany (P.satisfy isSpace)
		value <- untilChar '\n'
		return (CommandOption (Data.Text.pack key) value)
	
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
				E.throw (ParseExc (ParseError pos msg))

-- TODO: more unicode support
isSpace :: Char -> Bool
isSpace ' ' = True
isSpace '\t' = True
isSpace _ = False

parseBlocks :: [Line] -> Maybe (Either ParseError Block, [Line])
parseBlocks [] = Nothing
parseBlocks (line:xs) = parsed where
	parsed = case line of
		LineText _ text -> Just (Right (BlockText text), xs)
		LineCommand pos cmd -> case cmd of
			CommandFile path -> parseContent pos (BlockFile path) xs
			CommandDefine name -> parseContent pos (BlockDefine name) xs
			CommandOption key value -> Just (Right (BlockOption key value), xs)
			CommandColon -> Just (Right (BlockText ":"), xs)
			CommandEndBlock -> Just (Right (BlockText "\n"), xs)
			CommandComment -> Just (Right (BlockText ""), xs)
			CommandInclude _ -> let
				msg = "unexpected CommandInclude (internal error)"
				in Just (Left (ParseError pos msg), [])

parseContent :: Position -> ([Content] -> Block) -> [Line] -> Maybe (Either ParseError Block, [Line])
parseContent start block = parse [] where
	parse acc [] = Just (Right (block acc), [])
	parse acc (line:xs) = case line of
		LineText pos text -> case parse' pos text of
			Left err -> Just (Left err, [])
			Right parsed -> parse (acc ++ [parsed]) xs
		LineCommand _ CommandEndBlock -> Just (Right (block acc), xs)
		LineCommand _ _ -> let
			msg = "Unterminated content block"
			in Just (Left (ParseError start msg), [])
	
	parse' pos text = case P.parse (parser pos) "" (Data.Text.unpack text) of
		Right content -> Right content
		Left err -> let
			msg = Data.Text.pack ("Invalid content line " ++ show text ++ ": " ++ show err)
			in Left (ParseError pos msg)
	
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

type FileMap = Map FilePath [Line]

genLines :: Monad m => (FilePath -> m [Line]) -> FilePath -> S.StateT FileMap m [Line]
genLines getLines = genLines' where
	genLines' path = lift (getLines path) >>= concatMapM (resolveIncludes path)
	
	relative :: FilePath -> Text -> FilePath
	relative x y = FP.append (FP.parent x) (FP.fromText y)
	
	resolveIncludes root line = case line of
		LineCommand _ (CommandInclude path) -> genLines' (relative root path)
		_ -> return [line]

parseFile :: FilePath -> IO (Either ParseError [Block])
parseFile root = io where
	io = E.handle onError $ do
		lines' <- S.evalStateT (genLines getLines root) Data.Map.empty
		return (catEithers (unfoldr parseBlocks lines'))
	
	onError (ParseExc err) = return (Left err)
	
	getLines :: FilePath -> IO [Line]
	getLines path = do
		bytes <- System.File.readFile path
		let contents = Data.Text.unpack (decodeUtf8 bytes)
		case P.parse parseLines (show path) contents of
			Right x -> return x
			Left err -> let
				msg = Data.Text.pack ("getLines parse failed (internal error): " ++ show err)
				in E.throw (ParseExc (ParseError (Position path 0) msg))
