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
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (encodeUtf8)
import Control.Monad (forM_)
import Anansi.Types
import Anansi.Loom

loomLaTeX :: Monad m => Loom m
loomLaTeX = Loom loomLaTeX'

loomLaTeX' :: Monad m => (BL.ByteString -> m ()) -> [Block] -> m ()
loomLaTeX' wbytes bs = mapM_ putBlock bs where
	w = wbytes . encodeUtf8
	
	putBlock b = case b of
		BlockText text -> w text
		BlockFile path content -> do
			w "\\nwbegincode{0}\\moddef{"
			w $ escape path
			w "}\\endmoddef\\nwstartdeflinemarkup\\nwenddeflinemarkup\n"
			putContent content
			w "\\nwendcode{}\n"
			
		BlockDefine name content -> do
			w "\\nwbegincode{0}\\moddef{"
			w $ escape name
			w "}\\endmoddef\\nwstartdeflinemarkup\\nwenddeflinemarkup\n"
			putContent content
			w "\\nwendcode{}\n"
	
	putContent cs = forM_ cs $ \c -> case c of
		ContentText text -> w . escape $ TL.append text "\n"
		ContentMacro indent name -> w $ formatMacro indent name
		
formatMacro :: TL.Text -> TL.Text -> TL.Text
formatMacro indent name = TL.concat [indent, "\\LA{}", escape name, "\\RA{}\n"]

escape :: TL.Text -> TL.Text
escape = TL.concatMap $ \c -> case c of
	'\t' -> "        "
	'\\' -> "\\\\"
	'{' -> "\\{"
	'}' -> "\\}"
	_ -> TL.singleton c
