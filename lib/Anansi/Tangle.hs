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
import           Control.Monad.Trans (lift)
import qualified Control.Monad.Writer as W
import           Data.Map (Map)
import qualified Data.Map
import           Data.Text (Text)
import qualified Data.Text
import           Filesystem.Path (FilePath)
import qualified Filesystem.Path.CurrentOS as FP

import           Anansi.Types
import           Anansi.Util

type ContentMap = Map Text [Content]

data TangleState = TangleState Position Text ContentMap
type TangleT m a = W.WriterT Text (S.StateT TangleState m) a

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

tangle :: Monad m
       => (FilePath -> Text -> m ())
       -> Bool -- ^ Enable writing #line declarations
       -> [Block]
       -> m ()
tangle writeFile' enableLine blocks = S.evalStateT (mapM_ putFile files) initState where
	initState = (TangleState (Position "" 0) "" macros)
	fileMap = buildFiles blocks
	macros = buildMacros blocks
	files = Data.Map.toAscList fileMap
	
	putFile (path, content) = do
		text <- W.execWriterT (mapM_ (putContent enableLine) content)
		lift (writeFile' (FP.fromText path) text)

putContent :: Monad m => Bool -> Content -> TangleT m ()
putContent enableLine (ContentText pos t) = do
	TangleState _ indent _ <- S.get
	putPosition enableLine pos
	W.tell indent
	W.tell t
	W.tell "\n"

putContent enableLine (ContentMacro pos indent name) = addIndent putMacro where
	addIndent m = do
		TangleState lastPos old macros <- S.get
		S.put (TangleState lastPos (Data.Text.append old indent) macros)
		void m
		TangleState newPos _ _ <- S.get
		S.put (TangleState newPos old macros)
	putMacro = do
		putPosition enableLine pos
		lookupMacro name >>= mapM_ (putContent enableLine)

putPosition :: Monad m => Bool -> Position -> TangleT m ()
putPosition enableLine pos = do
	TangleState lastPos indent macros <- S.get
	let expectedPos = Position (positionFile lastPos) (positionLine lastPos + 1)
	let filename = either id id (FP.toText (positionFile pos))
	let line = if enableLine
		then "\n#line " ++ show (positionLine pos) ++ " " ++ show filename ++ "\n"
		else "\n"
	S.put (TangleState pos indent macros)
	if pos == expectedPos
		then return ()
		else W.tell (Data.Text.pack line)

lookupMacro :: Monad m => Text -> TangleT m [Content]
lookupMacro name = do
	TangleState _ _ macros <- S.get
	case Data.Map.lookup name macros of
		Nothing -> error ("unknown macro: " ++ show name)
		Just content -> return content
