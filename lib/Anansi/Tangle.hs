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

module Anansi.Tangle (tangle) where

import           Prelude hiding (FilePath)

import qualified Control.Monad.State as S
import qualified Control.Monad.RWS as RWS
import qualified Data.ByteString.Char8 as ByteString
import           Data.ByteString.Char8 (ByteString)
import qualified Data.Map
import           Data.Map (Map)
import qualified Data.Text
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import qualified Text.Parsec as P
import           Filesystem.Path (FilePath)
import qualified Filesystem.Path.CurrentOS as FP

import           Anansi.Types

-- macro definitions, #line pragma formatter
type ContentMap = Map Text [Content]
data TangleEnv = TangleEnv ContentMap (Position -> Text)

-- current position, current indent
data TangleState = TangleState Position ByteString

type TangleT = RWS.RWST TangleEnv ByteString TangleState

buildMacros :: [Block] -> ContentMap
buildMacros blocks = S.execState (mapM_ accumMacro blocks) Data.Map.empty

accumMacro :: Block -> S.State ContentMap ()
accumMacro b = case b of
	BlockText _ -> return ()
	BlockFile _ _ -> return ()
	BlockDefine name content -> do
		macros <- S.get
		S.put (Data.Map.insertWith (\new old -> old ++ new) name content macros)

buildFiles :: [Block] -> ContentMap
buildFiles blocks = S.execState (mapM_ accumFile blocks) Data.Map.empty

accumFile :: Block -> S.State ContentMap ()
accumFile b = case b of
	BlockText _ -> return ()
	BlockDefine _ _ -> return ()
	BlockFile name content -> do
		let accum new old = old ++ new
		files <- S.get
		S.put (Data.Map.insertWith accum name content files)

-- | Write a 'Document' to files. Paths passed to the file writer are pulled
-- directly from the document, so if you need to process them further, that
-- logic must be placed in the writer computation.
--
-- In most cases, users will want to write @#line@ pragmas to tangled source,
-- so error messages will refer back to the original input files. Haddock does
-- not handle these pragmas properly, so disable them when the tangled sources
-- will be processed into API documentation.
tangle :: Monad m
       => (FilePath -> ByteString -> m ()) -- ^ File writer
       -> Bool -- ^ Enable writing #line declarations
       -> Document
       -> m ()
tangle writeFile' enableLine doc = mapM_ putFile files where
	blocks = documentBlocks doc
	state = TangleState (Position "" 0) ""
	
	fileMap = buildFiles blocks
	macros = buildMacros blocks
	files = Data.Map.toAscList fileMap
	
	putFile (pathT, content) = do
		let path = FP.fromText pathT
		let env = TangleEnv macros (if enableLine
			then formatPosition doc path
			else const "\n")
		(_, bytes) <- RWS.evalRWST (mapM_ putContent content) env state
		writeFile' path bytes

formatPosition :: Document -> FilePath -> Position -> Text
formatPosition doc = checkPath where
	fmtC = "#line ${line} ${quoted-path}"
	fmtGo = "//line ${path}:${line}"
	defaultOptions = Data.Map.fromList
		[ ("anansi.line-pragma-hs", fmtC)
		, ("anansi.line-pragma-c", fmtC)
		, ("anansi.line-pragma-cxx", fmtC)
		, ("anansi.line-pragma-cpp", fmtC)
		, ("anansi.line-pragma-cs", fmtC)
		, ("anansi.line-pragma-pl", fmtC)
		, ("anansi.line-pragma-go", fmtGo)
		]
	opts = fmap compileTemplate (Data.Map.union (documentOptions doc) defaultOptions)
	
	checkPath path = case FP.extension path of
		Just ext -> case Data.Map.lookup ("anansi.line-pragma-" `Data.Text.append` ext) opts of
			Just tmpl -> checkPos tmpl
			Nothing -> const "\n"
		Nothing -> const "\n"
	
	checkPos tmpl pos = formatTemplate tmpl (templateParams pos)
	
	templateParams pos = Data.Map.fromList
		[ ("line", show (positionLine pos))
		, ("path", Data.Text.unpack (either id id (FP.toText (positionFile pos))))
		, ("quoted-path", show (either id id (FP.toText (positionFile pos))))
		]

data TemplateChunk
	= TemplateChunkConst Text
	| TemplateChunkVar Text

type Template = [TemplateChunk]

compileTemplate :: Text -> Template
compileTemplate "" = []
compileTemplate txt = check (P.parse parser "" (Data.Text.unpack txt)) where
	check (Left _) = error "Internal error: compileTemplate failed."
	check (Right tmpl) = tmpl
	
	parser = do
		chunks <- P.many (P.choice [P.try twodollar, P.try var, dollar, text])
		P.eof
		return chunks
	twodollar = do
		_ <- P.string "$$"
		return (TemplateChunkConst "$")
	dollar = do
		_ <- P.char '$'
		return (TemplateChunkConst "$")
	var = do
		_ <- P.string "${"
		name <- P.many1 (P.satisfy (\c -> c == '-' || (c >= 'a' && c <= 'z')))
		_ <- P.char '}'
		return (TemplateChunkVar (Data.Text.pack name))
	text = do
		chars <- P.many1 (P.satisfy (/= '$'))
		return (TemplateChunkConst (Data.Text.pack chars))

formatTemplate :: Template -> Map Text String -> Text
formatTemplate [] _ = "\n"
formatTemplate chunks vars = Data.Text.concat ("\n" : map formatChunk chunks ++ ["\n"]) where
	formatChunk (TemplateChunkConst t) = t
	formatChunk (TemplateChunkVar name) = case Data.Map.lookup name vars of
		Just value -> Data.Text.pack value
		Nothing -> Data.Text.concat ["${", name, "}"]

putContent :: Monad m => Content -> TangleT m ()
putContent (ContentText pos t) = do
	TangleState _ indent <- RWS.get
	putPosition pos
	RWS.tell indent
	RWS.tell (encodeUtf8 t)
	RWS.tell "\n"

putContent (ContentMacro pos indent name) = addIndent putMacro where
	addIndent m = do
		TangleState lastPos old <- RWS.get
		RWS.put (TangleState lastPos (ByteString.append old (encodeUtf8 indent)) )
		_ <- m
		TangleState newPos _ <- S.get
		S.put (TangleState newPos old)
	putMacro = do
		putPosition pos
		lookupMacro name pos >>= mapM_ putContent

putPosition :: Monad m => Position -> TangleT m ()
putPosition pos = do
	TangleState lastPos indent <- RWS.get
	let expectedPos = Position (positionFile lastPos) (positionLine lastPos + 1)
	RWS.put (TangleState pos indent)
	if pos == expectedPos
		then return ()
		else do
			TangleEnv _ format <- RWS.ask
			RWS.tell (encodeUtf8 (format pos))

lookupMacro :: Monad m => Text -> Position -> TangleT m [Content]
lookupMacro name pos = do
	TangleEnv macros _ <- RWS.ask
	case Data.Map.lookup name macros of
		Nothing -> error (concat
			[ "unknown macro "
			, show name
			, " at "
			, Data.Text.unpack (either id id (FP.toText (positionFile pos)))
			, ":"
			, show (positionLine pos)
			])
		Just content -> return content
