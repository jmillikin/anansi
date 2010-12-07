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
{-# LANGUAGE DeriveDataTypeable #-}
module Anansi.Parser
	( ParseError (..)
	, parseFile
	) where
import Prelude hiding (FilePath)
import Control.Applicative ((<|>), (<$>))
import Control.Monad.Trans (lift)
import qualified Control.Monad.State as S
import qualified Control.Exception as E
import Data.List (unfoldr)
import Data.Typeable (Typeable)
import qualified Text.Parsec as P
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Map as Map
import System.FilePath (FilePath)
import qualified System.FilePath.CurrentOS as FP
import Anansi.Types
import Anansi.Util

data ParseError = ParseError
	{ parseErrorPosition :: Position
	, parseErrorMessage :: TL.Text
	}
	deriving (Show)

-- too lazy to write proper error handling
data ParseExc = ParseExc ParseError
	deriving (Typeable, Show)

instance E.Exception ParseExc

data Command
	= CommandInclude TL.Text
	| CommandFile TL.Text
	| CommandDefine TL.Text
	| CommandColon
	| CommandEndBlock
	| CommandComment
	deriving (Show)

data Line
	= LineCommand Position Command
	| LineText Position TL.Text
	deriving (Show)

untilChar :: Char -> P.Parsec String u TL.Text
untilChar c = TL.pack <$> P.manyTill P.anyChar (P.try (P.char c))

getPosition :: Monad m => P.ParsecT s u m Position
getPosition = do
	pos <- P.getPosition
	return $ Position (FP.fromString (P.sourceName pos)) (toInteger (P.sourceLine pos))

parseLines :: P.Parsec String u [Line]
parseLines = do
	lines' <- P.many parseLine
	P.eof
	return lines'

parseLine :: P.Parsec String u Line
parseLine = command <|> text where
	command = do
		P.char ':'
		pos <- getPosition
		LineCommand pos <$> parseCommand
	
	text = do
		pos <- getPosition
		line <- untilChar '\n'
		return . LineText pos $ TL.append line "\n"

parseCommand :: P.Parsec String u Command
parseCommand = parsed where
	string = P.try . P.string
	parsed = P.choice [file, include, define, colon, comment, endBlock]
	file = do
		string "file " <|> string "f "
		CommandFile <$> untilChar '\n'
	
	include = do
		string "include " <|> string "i "
		CommandInclude <$> untilChar '\n'
	
	define = do
		string "define " <|> string "d "
		-- TODO: verify no '|' in name
		CommandDefine <$> untilChar '\n'
	
	colon = do
		P.char ':'
		return $ CommandColon
	
	comment = do
		P.char '#'
		untilChar '\n'
		return $ CommandComment
	
	endBlock = do
		line <- untilChar '\n'
		if TL.all isSpace line
			then return $ CommandEndBlock
			else do
				pos <- getPosition
				let msg = TL.pack $ "unknown command: " ++ show (TL.append ":" line)
				E.throw $ ParseExc $ ParseError pos msg

-- TODO: more unicode support
isSpace :: Char -> Bool
isSpace ' ' = True
isSpace '\t' = True
isSpace _ = False

parseBlocks :: [Line] -> Maybe (Either ParseError Block, [Line])
parseBlocks [] = Nothing
parseBlocks (line:xs) = parsed where
	parsed = case line of
		LineText _ text -> Just (Right $ BlockText text, xs)
		LineCommand pos cmd -> case cmd of
			CommandFile path -> parseContent pos (BlockFile path) xs
			CommandDefine name -> parseContent pos (BlockDefine name) xs
			CommandColon -> Just (Right $ BlockText ":", xs)
			CommandEndBlock -> Just (Right $ BlockText "\n", xs)
			CommandComment -> Just (Right $ BlockText "", xs)
			CommandInclude _ -> let
				msg = "unexpected CommandInclude (internal error)"
				in Just (Left $ ParseError pos msg, [])

parseContent :: Position -> ([Content] -> Block) -> [Line] -> Maybe (Either ParseError Block, [Line])
parseContent start block = parse [] where
	parse acc [] = Just (Right $ block acc, [])
	parse acc (line:xs) = case line of
		LineText pos text -> case parse' pos text of
			Left err -> Just (Left err, [])
			Right parsed -> parse (acc ++ [parsed]) xs
		LineCommand _ CommandEndBlock -> Just (Right $ block acc, xs)
		LineCommand _ _ -> let
			msg = "Unterminated content block"
			in Just (Left $ ParseError start msg, [])
	
	parse' pos text = case P.parse (parser pos) "" (TL.unpack text) of
		Right content -> Right content
		Left err -> let
			msg = TL.pack $ "Invalid content line " ++ show text ++ ": " ++ show err
			in Left $ ParseError pos msg
	
	parser pos = do
		content <- contentMacro pos <|> contentText pos
		P.optional $ P.char '\n'
		P.eof
		return content
	
	contentMacro pos = do
		(indent, c) <- P.try $ do
			indent <- P.many $ P.satisfy isSpace
			P.char '|'
			c <- P.satisfy (not . isSpace)
			return (indent, c)
		name <- untilChar '|'
		return $ ContentMacro pos (TL.pack indent) (TL.strip (TL.cons c name))
	
	contentText pos = do
		text <- untilChar '\n'
		return . ContentText pos $ text

type FileMap = Map.Map FilePath [Line]

genLines :: Monad m => (FilePath -> m [Line]) -> FilePath -> S.StateT FileMap m [Line]
genLines getLines = genLines' where
	genLines' path = lift (getLines path) >>= concatMapM (resolveIncludes path)
	
	textToPath :: TL.Text -> FilePath
	textToPath = FP.fromString . TL.unpack
	
	relative :: FilePath -> TL.Text -> FilePath
	relative x y = FP.append (FP.parent x) (textToPath y)
	
	-- relative x y = TL.pack $ replaceFileName (TL.unpack x) (TL.unpack y)
	
	resolveIncludes root line = case line of
		LineCommand _ (CommandInclude path) -> genLines' $ relative root path
		_ -> return [line]

parseFile :: FilePath -> IO (Either ParseError [Block])
parseFile root = io where
	io = E.handle onError $ do
		lines' <- S.evalStateT (genLines getLines root) Map.empty
		return . catEithers $ unfoldr parseBlocks lines'
	
	onError (ParseExc err) = return $ Left err
	
	getLines :: FilePath -> IO [Line]
	getLines path = do
		let strPath = FP.toString path
		bytes <- B.readFile strPath
		let contents = T.unpack (TE.decodeUtf8 bytes)
		case P.parse parseLines strPath contents of
			Right x -> return x
			Left err -> let
				msg = TL.pack $ "getLines parse failed (internal error): " ++ show err
				in E.throw $ ParseExc $ ParseError (Position path 0) msg
