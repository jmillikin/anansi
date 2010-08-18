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
module Anansi.Loom.LaTeX (loomLaTeX) where
import qualified Data.Text.Lazy as TL
import Control.Monad (forM_)
import Control.Monad.Writer (tell)
import Anansi.Types
import Anansi.Loom

loomLaTeX :: Loom
loomLaTeX = Loom "latex" $ mapM_ putBlock where
	putBlock b = case b of
		BlockText text -> tell text
		BlockFile path content -> do
			tell "\\begin{alltt}\n"
			tell "{\\bf\\(\\gg\\) "
			tell $ escape path
			tell "}\n"
			putContent content
			tell "\\end{alltt}\n"
			
		BlockDefine name content -> do
			tell "\\begin{alltt}\n"
			tell "{\\bf\\(\\ll\\)"
			tell $ escape name
			tell "\\(\\gg\\)}\n"
			putContent content
			tell "\\end{alltt}\n"
	
	putContent cs = forM_ cs $ \c -> case c of
		ContentText _ text -> tell . escape $ TL.append text "\n"
		ContentMacro _ indent name -> tell $ formatMacro indent name

formatMacro :: TL.Text -> TL.Text -> TL.Text
formatMacro indent name = TL.concat [escape indent, "|\\emph{", escape name, "}|\n"]

escape :: TL.Text -> TL.Text
escape = TL.concatMap $ \c -> case c of
	'\t' -> "        "
	'\\' -> "\\textbackslash{}"
	'{' -> "\\{"
	'}' -> "\\}"
	'_' -> "\\_"
	_ -> TL.singleton c
