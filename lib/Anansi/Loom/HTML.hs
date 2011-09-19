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

module Anansi.Loom.HTML (loomHTML) where

import           Control.Monad (forM_)
import           Control.Monad.Reader (asks)
import           Control.Monad.Writer (tell)
import           Data.ByteString (ByteString)
import           Data.Monoid (mconcat)
import qualified Data.Text
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)

import           Anansi.Types

-- | Generate simple, @\<pre\>@-based HTML. Users who would like to weave
-- specialized HTML to fit with their existing templates are encouraged to
-- copy this loom and modify it as needed.
loomHTML :: Loom
loomHTML = mapM_ putBlock . documentBlocks where
	putBlock b = case b of
		BlockText text -> tell (encodeUtf8 text)
		BlockFile path content -> do
			epath <- escape path
			let label = mconcat ["<b>&#xBB; ", epath, "</b>"]
			putContent label content
		BlockDefine name content -> do
			ename <- escape name
			let label = mconcat ["<b>&#xAB;", ename, "&#xBB;</b>"]
			putContent label content
	
	putContent label cs = do
		tell "<pre>"
		tell label
		tell "\n"
		forM_ cs $ \c -> case c of
			ContentText _ text -> do
				tell =<< escape text
				tell "\n"
			ContentMacro _ indent name -> tell =<< formatMacro indent name
		tell "</pre>"

formatMacro :: Text -> Text -> LoomM ByteString
formatMacro indent name = do
	ename <- escape name
	return $ mconcat
		[ encodeUtf8 indent
		, "<i>&#xAB;"
		, ename
		, "&#xBB;</i>\n"
		]

escape :: Text -> LoomM ByteString
escape txt = do
	tabSize <- asks loomOptionTabSize
	
	return $ encodeUtf8 $ Data.Text.concatMap (\c -> case c of
		'\t' -> Data.Text.replicate (fromInteger tabSize) " "
		'&' -> "&amp;"
		'<' -> "&lt;"
		'>' -> "&gt;"
		'"' -> "&quot;"
		'\'' -> "&apos;"
		_ -> Data.Text.singleton c) txt
