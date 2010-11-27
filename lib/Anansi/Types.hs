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
module Anansi.Types
	( Block (..)
	, Content (..)
	, Position (..)
	) where
import Data.Text.Lazy (Text)

data Block
	= BlockText Text
	| BlockFile Text [Content]
	| BlockDefine Text [Content]
	deriving (Show)

data Content
	= ContentText Position Text
	| ContentMacro Position Text Text
	deriving (Show)

data Position = Position
	{ positionFile :: Text
	, positionLine :: Integer
	}
	deriving (Show, Eq)