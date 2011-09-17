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
import           Control.Monad.Reader (asks)
import           Control.Monad.Writer (tell)
import           Data.ByteString (ByteString)
import           Data.Monoid (mconcat)
import qualified Data.Text
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)

import           Anansi.Types

loomLaTeX :: Loom
loomLaTeX = Loom (mapM_ putBlock . documentBlocks) where
	putBlock b = case b of
		BlockText text -> tell (encodeUtf8 text)
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
	
	putContent cs = forM_ cs $ \c -> case c of
		ContentText _ text -> do
			escape text >>= tell
			tell "\n"
		ContentMacro _ indent name -> formatMacro indent name >>= tell
	
	formatMacro indent name = do
		escIndent <- escape indent
		escName <- escape name
		return (mconcat [escIndent, "|\\emph{", escName, "}|\n"])

escape ::  Text -> LoomM ByteString
escape text = do
	tabSize <- asks loomOptionTabSize
	
	return $ encodeUtf8 $ Data.Text.concatMap (\c -> case c of
		'\t' -> Data.Text.replicate (fromInteger tabSize) " "
		'\\' -> "\\textbackslash{}"
		'{' -> "\\{"
		'}' -> "\\}"
		'_' -> "\\_"
		_ -> Data.Text.singleton c) text
