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
module Anansi.Tangle (tangle) where
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as S
import qualified Data.Text.Lazy as TL
import qualified Data.Map as Map
import Anansi.Types

type ContentMap = Map.Map TL.Text [Content]

buildMacros :: [Block] -> ContentMap
buildMacros blocks = S.execState (mapM_ accumMacro blocks) Map.empty

accumMacro :: Block -> S.State ContentMap ()
accumMacro b = case b of
	BlockText _ -> return ()
	BlockFile _ _ -> return ()
	BlockDefine name content -> do
		macros <- S.get
		S.put $ Map.insertWith (\new old -> old ++ new) name content macros

buildFiles :: [Block] -> ContentMap
buildFiles blocks = S.execState (mapM_ accumFile blocks) Map.empty

accumFile :: Block -> S.State ContentMap ()
accumFile b = case b of
	BlockText _ -> return ()
	BlockDefine _ _ -> return ()
	BlockFile name content -> do
		files <- S.get
		S.put $ Map.insertWith (\new old -> old ++ new) name content files

tangle :: Monad m => (TL.Text -> m ()) -> [Block] -> m ()
tangle w blocks = S.evalStateT (mapM_ putFile files) ("", macros) where
	fileMap = buildFiles blocks
	macros = buildMacros blocks
	files = Map.toAscList fileMap
	
	putFile (path, content) = do
		lift $ do
			w ""
			w path
			w $ TL.replicate (TL.length path) "="
			
		mapM_ putContent content
		
	putContent (ContentText t) = do
		(indent, _) <- S.get
		lift $ do
			w indent
			w t
			w "\n"
			
	putContent (ContentMacro indent name) = addIndent putMacro where
		addIndent m = do
			(old, macros) <- S.get
			S.put (TL.append old indent, macros)
			m
			S.put (old, macros)
		putMacro = lookupMacro name >>= mapM_ putContent

lookupMacro :: Monad m => TL.Text -> S.StateT (TL.Text, ContentMap) m [Content]
lookupMacro name = do
	(_, macros) <- S.get
	case Map.lookup name macros of
		Nothing -> error $ "unknown macro: " ++ show name
		Just content -> return content
