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

module Anansi
	(
	-- * Basic operations
	  defaultMain
	, parse
	, tangle
	, weave
	
	-- * Documents
	, Document
	, documentBlocks
	, documentOptions
	, documentLoomName
	, Block (..)
	, Content (..)
	
	, Position
	, positionFile
	, positionLine
	
	-- * Document parsing
	, ParseError
	, parseErrorPosition
	, parseErrorMessage
	
	-- * Looms
	, Loom (..)
	, LoomOptions
	, loomOptionTabSize
	
	-- ** Built-in looms
	, defaultLooms
	, loomDebug
	, loomHTML
	, loomLaTeX
	, loomNoWeb
	) where

import           Data.Map (Map, fromList)
import           Data.Text (Text)

import           Anansi.Loom.Debug
import           Anansi.Loom.HTML
import           Anansi.Loom.LaTeX
import           Anansi.Loom.NoWeb
import           Anansi.Main
import           Anansi.Parser
import           Anansi.Tangle
import           Anansi.Types

defaultLooms :: Map Text Loom
defaultLooms = fromList
	[ ("anansi.debug", loomDebug)
	, ("anansi.html", loomHTML)
	, ("anansi.latex", loomLaTeX)
	, ("anansi.noweb", loomNoWeb)
	]
