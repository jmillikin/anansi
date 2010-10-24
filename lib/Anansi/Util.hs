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
module Anansi.Util
	( concatMapM
	, replace
	, catEithers
	) where
import Control.Monad (liftM)

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat $ mapM f xs

replace :: Eq a => a -> [a] -> [a] -> [a]
replace from to xs = flip concatMap xs $ \x -> if x == from
	then to
	else [x]

catEithers :: [Either e a] -> Either e [a]
catEithers = cat' [] where
	cat' acc [] = Right $ reverse acc
	cat' acc (e:es) = case e of
		Left err -> Left err
		Right x -> cat' (x : acc) es
