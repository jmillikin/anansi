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

module Anansi.Loom.Debug (loomDebug) where

import           Control.Monad (forM_)
import           Control.Monad.Writer (tell)
import           Data.Text (pack)

import           Anansi.Types

loomDebug :: Loom
loomDebug = Loom $ \doc -> do
	tell "\nweaving\n"
	tell "==========================\n"
	forM_ (documentBlocks doc) $ \b -> do
		tell (pack (show b ++ "\n"))
