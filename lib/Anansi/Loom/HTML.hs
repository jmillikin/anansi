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
import           Control.Monad.Writer (tell)
import           Data.Monoid (mconcat)
import           Data.Text (Text)
import qualified Data.Text

import           Anansi.Types

loomHTML :: Loom
loomHTML = Loom (\doc -> mapM_ putBlock (documentBlocks doc)) where
	putBlock b = case b of
		BlockText text -> tell text
		BlockFile path content -> let
			label = mconcat ["<b>&#xBB; ", escape path, "</b>"]
			in putContent label content
		BlockDefine name content -> let
			label = mconcat ["<b>&#xAB;", escape name, "&#xBB;</b>"]
			in putContent label content
	
	putContent label cs = do
		tell "<pre>"
		tell label
		tell "\n"
		forM_ cs $ \c -> case c of
			ContentText _ text -> do
				tell (escape text)
				tell "\n"
			ContentMacro _ indent name -> tell (formatMacro indent name)
		tell "</pre>"

formatMacro :: Text -> Text -> Text
formatMacro indent name = mconcat [indent, "<i>&#xAB;", escape name, "&#xBB;</i>\n"]

escape :: Text -> Text
escape = Data.Text.concatMap $ \c -> case c of
	'&' -> "&amp;"
	'<' -> "&lt;"
	'>' -> "&gt;"
	'"' -> "&quot;"
	'\'' -> "&apos;"
	_ -> Data.Text.singleton c
