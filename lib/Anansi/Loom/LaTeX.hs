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

module Anansi.Loom.LaTeX (loomLaTeX) where

import           Control.Monad (forM_)
import qualified Control.Monad.State as S
import           Control.Monad.Writer (Writer, tell)
import qualified Data.Text.Lazy as TL

import           Anansi.Loom
import           Anansi.Types

data LoomState = LoomState { stateTabSize :: Integer }

type LoomM = S.StateT LoomState (Writer TL.Text)

loomLaTeX :: Loom
loomLaTeX = Loom "latex" (\bs -> S.evalStateT (mapM_ putBlock bs) initState) where
	initState = LoomState 8
	
	putBlock b = case b of
		BlockText text -> tell text
		BlockFile path content -> do
			tell "\\begin{alltt}\n"
			tell "{\\bf\\(\\gg\\) "
			tell =<< escape path
			tell "}\n"
			putContent content
			tell "\\end{alltt}\n"
			
		BlockDefine name content -> do
			tell "\\begin{alltt}\n"
			tell "{\\bf\\(\\ll\\)"
			tell =<< escape name
			tell "\\(\\gg\\)}\n"
			putContent content
			tell "\\end{alltt}\n"
		BlockOption key value -> if key == "tab-size"
			then S.put (LoomState (read (TL.unpack value)))
			else return ()
	
	putContent cs = forM_ cs $ \c -> case c of
		ContentText _ text -> tell =<< escape (TL.append text "\n")
		ContentMacro _ indent name -> tell =<< formatMacro indent name
	
	formatMacro indent name = do
		escIndent <- escape indent
		escName <- escape name
		return $ TL.concat [escIndent, "|\\emph{", escName, "}|\n"]

escape ::  TL.Text -> LoomM TL.Text
escape text = do
	tabSize <- S.gets stateTabSize
	
	return $ TL.concatMap (\c -> case c of
		'\t' -> TL.replicate (fromInteger tabSize) (TL.singleton ' ')
		'\\' -> "\\textbackslash{}"
		'{' -> "\\{"
		'}' -> "\\}"
		'_' -> "\\_"
		_ -> TL.singleton c) text
