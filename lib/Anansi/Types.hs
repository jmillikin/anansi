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

module Anansi.Types
	( Block (..)
	, Content (..)
	, Position (..)
	, ParseError (..)
	
	, Document (..)
	
	, Loom (..)
	, LoomM
	, LoomOptions (..)
	, parseLoomOptions
	, weave
	) where

import           Prelude hiding (FilePath)

import           Control.Monad.Reader (ReaderT, runReaderT)
import           Control.Monad.Writer (Writer, execWriter)
import           Data.ByteString (ByteString)
import qualified Data.Map
import           Data.Map (Map)
import qualified Data.Text
import           Data.Text (Text)
import           Filesystem.Path.CurrentOS (FilePath)

data Block
	= BlockText Text
	| BlockFile Text [Content]
	| BlockDefine Text [Content]
	deriving (Eq, Ord, Show)

data Content
	= ContentText Position Text
	| ContentMacro Position Text Text
	deriving (Eq, Ord, Show)

data Position = Position
	{ positionFile :: FilePath
	, positionLine :: Integer
	}
	deriving (Eq, Ord, Show)

data ParseError = ParseError
	{ parseErrorPosition :: Position
	, parseErrorMessage :: Text
	}
	deriving (Eq, Show)

data Document = Document
	{ documentBlocks :: [Block]
	, documentOptions :: Map Text Text
	, documentLoomName :: Maybe Text
	}
	deriving (Eq, Show)

type LoomM = ReaderT LoomOptions (Writer ByteString)
newtype Loom = Loom (Document -> ReaderT LoomOptions (Writer ByteString) ())

weave :: Loom -> Document -> ByteString
weave (Loom m) doc = execWriter (runReaderT
	(m doc)
	(parseLoomOptions (documentOptions doc)))

data LoomOptions = LoomOptions
	{ loomOptionTabSize :: Integer
	}
	deriving (Eq, Show)

parseLoomOptions :: Map Text Text -> LoomOptions
parseLoomOptions opts = LoomOptions
	{ loomOptionTabSize = case Data.Map.lookup "tab-size" opts of
	  	Just x -> read (Data.Text.unpack x)
	  	Nothing -> 8
	}
