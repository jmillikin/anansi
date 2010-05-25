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
module Main (main) where

import Anansi
import Anansi.Loom.Debug
import Anansi.Loom.XHTML
import System.IO (stdout)
import System.Environment (getArgs)
import Data.String
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
	[path] <- getArgs
	blocks <- parseFile $ fromString path
	weave loomXHTML (BL.hPut stdout) blocks
	putStrLn "\n\n\n"
	tangle (putStr . TL.unpack) blocks
	
	--weave loomDebug (BL.hPut stdout) blocks
