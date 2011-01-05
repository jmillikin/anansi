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
module Anansi.Loom.NoWeb (loomNoWeb) where
import qualified Data.Text.Lazy as TL
import Control.Monad (forM_)
import Control.Monad.Writer (Writer, tell)
import qualified Control.Monad.State as S
import Anansi.Types
import Anansi.Loom

data LoomState = LoomState { stateTabSize :: Integer }

type LoomM = S.StateT LoomState (Writer TL.Text)

loomNoWeb :: Loom
loomNoWeb = Loom "latex-noweb" (\bs -> S.evalStateT (mapM_ putBlock bs) initState) where
	initState = LoomState 8
	
	putBlock b = case b of
		BlockText text -> tell text
		BlockFile path content -> do
			tell "\\nwbegincode{0}\\moddef{"
			tell =<< escapeText path
			tell "}\\endmoddef\\nwstartdeflinemarkup\\nwenddeflinemarkup\n"
			putContent content
			tell "\\nwendcode{}\n"
			
		BlockDefine name content -> do
			tell "\\nwbegincode{0}\\moddef{"
			tell =<< escapeText name
			tell "}\\endmoddef\\nwstartdeflinemarkup\\nwenddeflinemarkup\n"
			putContent content
			tell "\\nwendcode{}\n"
		BlockOption key value -> if key == "tab-size"
			then S.put (LoomState (read (TL.unpack value)))
			else return ()
	
	putContent cs = forM_ cs $ \c -> case c of
		ContentText _ text -> tell =<< escapeCode (TL.append text "\n")
		ContentMacro _ indent name -> tell =<< formatMacro indent name
	
	formatMacro indent name = do
		escIndent <- escapeCode indent
		escName <- escapeText name
		return $ TL.concat [escIndent, "\\LA{}", escName, "\\RA{}\n"]

escapeCode :: TL.Text -> LoomM TL.Text
escapeCode text = do
	tabSize <- S.gets stateTabSize
	
	return $ TL.concatMap (\c -> case c of
		'\t' -> TL.replicate (fromInteger tabSize) (TL.singleton ' ')
		'\\' -> "\\\\"
		'{' -> "\\{"
		'}' -> "\\}"
		'_' -> "\\_"
		_ -> TL.singleton c) text

escapeText :: TL.Text -> LoomM TL.Text
escapeText text = do
	tabSize <- S.gets stateTabSize
	
	return $ TL.concatMap (\c -> case c of
		'\t' -> TL.replicate (fromInteger tabSize) (TL.singleton ' ')
		'\\' -> "\\\\"
		'{' -> "\\{"
		'}' -> "\\}"
		'_' -> "\\_"
		'<' -> "{$<$}"
		'>' -> "{$>$}"
		'$' -> "\\$"
		_ -> TL.singleton c) text
