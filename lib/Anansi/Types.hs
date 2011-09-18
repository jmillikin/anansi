{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

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
	
	, Loom
	, LoomM
	, LoomOptions (..)
	, parseLoomOptions
	, weave
	) where

import           Prelude hiding (FilePath)

import           Control.Monad (liftM)
import qualified Control.Monad.Reader as Reader
import           Control.Monad.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Writer as Writer
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
	
	-- | A macro reference within a content block. The first 'Text' is
	-- any indentation found before the first @\'|\'@, and the second is
	-- the name of the macro.
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
	
	-- | A map of @:option@ commands found in the document. If
	-- the same option is specified multiple times, the most recent will
	-- be used.
	, documentOptions :: Map Text Text
	
	-- | The last @:loom@ command given, if any. A document does not
	-- require a loom name if it's just going to be tangled, or will be
	-- woven by the user calling 'weave'. Documents woven by
	-- 'defaultMain' do require a loom name.
	, documentLoomName :: Maybe Text
	}
	deriving (Eq, Show)

-- | A loom contains all the logic required to convert a 'Document' into
-- markup suitable for processing with an external documentation tool.
--
-- Within a loom, use 'Reader.ask' to retrieve the 'LoomOptions', and
-- 'Writer.tell' to append data to the output.
type Loom = Document -> LoomM ()
newtype LoomM a = LoomM { unLoomM :: ReaderT LoomOptions (Writer ByteString) a }

instance Functor LoomM where
	fmap = liftM

instance Monad LoomM where
	return = LoomM . return
	(LoomM m) >>= f = LoomM $ do
		x <- m
		unLoomM (f x)

instance Reader.MonadReader LoomM where
	type Reader.EnvType LoomM = LoomOptions
	ask = LoomM Reader.ask
	local f (LoomM m) = LoomM (Reader.local f m)

instance Writer.MonadWriter LoomM where
	type Writer.WriterType LoomM = ByteString
	tell = LoomM . Writer.tell
	listen (LoomM m) = LoomM (Writer.listen m)
	pass m = LoomM (Writer.pass (unLoomM m))

-- | Write a document to some sort of document markup. This will typically be
-- rendered into documentation by external tools, such as LaTeX or a web
-- browser.
--
-- This writes a 'ByteString' rather than 'Text' so that looms have full
-- control over character encoding.
weave :: Loom -> Document -> ByteString
weave loom doc = execWriter (runReaderT
	(unLoomM (loom doc))
	(parseLoomOptions (documentOptions doc)))

-- | A set of processed @:option@ commands related to looms. Looms are always
-- free to check options manually, but this simplifies common cases.
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
