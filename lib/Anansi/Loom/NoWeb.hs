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

module Anansi.Loom.NoWeb (loomNoWeb) where

import           Control.Monad (forM_)
import           Control.Monad.Reader (asks)
import           Control.Monad.Writer (tell)
import           Data.ByteString (ByteString)
import           Data.Monoid (mconcat)
import qualified Data.Text
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)

import           Anansi.Types

-- | Generate LaTeX markup, emulating the behavior of NoWeb. This is useful
-- for porting existing NoWeb-based projects to Anansi without having to
-- rewrite the styling.
loomNoWeb :: Loom
loomNoWeb = mapM_ putBlock . documentBlocks where
	putBlock b = case b of
		BlockText text -> tell (encodeUtf8 text)
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
	
	putContent cs = forM_ cs $ \c -> case c of
		ContentText _ text -> do
			escapeCode text >>= tell
			tell "\n"
		ContentMacro _ indent name -> formatMacro indent name >>= tell
	
	formatMacro indent name = do
		escIndent <- escapeCode indent
		escName <- escapeText name
		return (mconcat [escIndent, "\\LA{}", escName, "\\RA{}\n"])

escapeCode :: Text -> LoomM ByteString
escapeCode text = do
	tabSize <- asks loomOptionTabSize
	
	return $ encodeUtf8 $ Data.Text.concatMap (\c -> case c of
		'\t' -> Data.Text.replicate (fromInteger tabSize) " "
		'\\' -> "\\\\"
		'{' -> "\\{"
		'}' -> "\\}"
		'_' -> "\\_"
		_ -> Data.Text.singleton c) text

escapeText :: Text -> LoomM ByteString
escapeText text = do
	tabSize <- asks loomOptionTabSize
	
	return $ encodeUtf8 $ Data.Text.concatMap (\c -> case c of
		'\t' -> Data.Text.replicate (fromInteger tabSize) " "
		'\\' -> "\\\\"
		'{' -> "\\{"
		'}' -> "\\}"
		'_' -> "\\_"
		'<' -> "{$<$}"
		'>' -> "{$>$}"
		'$' -> "\\$"
		_ -> Data.Text.singleton c) text
