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
module Anansi.Parser
	( parseFile
	) where
import Prelude hiding (FilePath)
import Control.Applicative ((<|>), (<$>))
import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as S
import Data.List (unfoldr)
import qualified Text.Parsec as P
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Map as Map
import Anansi.Types

data Command
	= CommandInclude TL.Text
	| CommandFile TL.Text
	| CommandDefine TL.Text
	| CommandColon
	| CommandEndBlock
	| CommandComment
	deriving (Show)

data Line
	= LineCommand Command
	| LineText TL.Text
	deriving (Show)

untilChar :: Char -> P.Parsec String u TL.Text
untilChar c = TL.pack <$> P.manyTill P.anyChar (P.try (P.char c))

parseLines :: P.Parsec String u [Line]
parseLines = do
	lines' <- P.many parseLine
	P.eof
	return lines'

parseLine :: P.Parsec String u Line
parseLine = command <|> text where
	command = do
		P.char ':'
		LineCommand <$> parseCommand
	
	text = do
		line <- untilChar '\n'
		return . LineText $ TL.append line "\n"

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
			else error $ "unknown command: " ++ show (TL.append ":" line)

-- TODO: more unicode support
isSpace :: Char -> Bool
isSpace ' ' = True
isSpace '\t' = True
isSpace _ = False

parseBlocks :: [Line] -> Maybe (Block, [Line])
parseBlocks [] = Nothing
parseBlocks (line:xs) = parsed where
	parsed = case line of
		LineText text -> Just (BlockText text, xs)
		LineCommand cmd -> case cmd of
			CommandFile path -> parseContent (BlockFile path) xs
			CommandDefine name -> parseContent (BlockDefine name) xs
			CommandColon -> Just (BlockText ":", xs)
			CommandEndBlock -> Just (BlockText "\n", xs)
			CommandComment -> Just (BlockText "", xs)
			CommandInclude _ -> error "unexpected CommandInclude"

parseContent :: ([Content] -> Block) -> [Line] -> Maybe (Block, [Line])
parseContent block = parse [] where
	parse acc [] = Just (block acc, [])
	parse acc (line:xs) = case line of
		LineText text -> parse (acc ++ [parse' text]) xs
		LineCommand CommandEndBlock -> Just (block acc, xs)
		LineCommand _ -> error $ "unexpected line: " ++ show line
	
	parse' text = case P.parse parser "" (TL.unpack text) of
		Right content -> content
		Left err -> error $ "content parse failed (text = " ++ show text ++ "): " ++ show err
	
	parser = do
		content <- contentMacro <|> contentText
		P.optional $ P.char '\n'
		P.eof
		return content
	
	contentMacro = do
		(indent, c) <- P.try $ do
			indent <- P.many $ P.satisfy isSpace
			P.char '|'
			c <- P.satisfy (not . isSpace)
			return (indent, c)
		name <- untilChar '|'
		
		let indent' = replace '\t' "        " indent
		return $ ContentMacro (TL.pack indent') (TL.strip (TL.cons c name))
	
	contentText = do
		text <- untilChar '\n'
		return . ContentText $ text

type FilePath = TL.Text
type FileMap = Map.Map FilePath [Line]

genLines :: Monad m => (FilePath -> m [Line]) -> FilePath -> S.StateT FileMap m [Line]
genLines getLines = genLines' where
	genLines' path = lift (getLines path) >>= concatMapM resolveIncludes
	
	resolveIncludes line = case line of
		LineCommand (CommandInclude path) -> genLines' path
		_ -> return [line]

parseFile :: TL.Text -> IO [Block]
parseFile root = io where
	io = do
		lines' <- S.evalStateT (genLines getLines root) Map.empty
		return $ unfoldr parseBlocks lines'
	
	getLines :: FilePath -> IO [Line]
	getLines path = do
		-- TODO: encode 'path' into UTF-8?
		bytes <- B.readFile $ TL.unpack path
		case P.parse parseLines "" (T.unpack $ TE.decodeUtf8 bytes) of
			Right x -> return x
			Left err -> error $ "lines parse failed: " ++ show err

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat $ mapM f xs

replace :: Eq a => a -> [a] -> [a] -> [a]
replace from to xs = flip concatMap xs $ \x -> if x == from
	then to
	else [x]
