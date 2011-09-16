{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (tests, main) where

import qualified Data.Map as Map
import           Data.Text (Text)
import           Test.Chell

import           Anansi
import           Anansi.Types

main :: IO ()
main = Test.Chell.defaultMain tests

tests :: [Suite]
tests =
	[ suite_Parse
	, suite_Tangle
	, suite_Weave
	]

suite_Parse :: Suite
suite_Parse = suite "parse"
	[]

suite_Tangle :: Suite
suite_Tangle = suite "tangle"
	[
	]

suite_Weave :: Suite
suite_Weave = suite "weave"
	[ test_WeaveDebug
	, test_WeaveHtml
	, test_WeaveLatex
	, test_WeaveNoweb
	]

test_WeaveDebug :: Suite
test_WeaveDebug = test $ assertions "debug" $ do
	$expect $ equalWeave loomDebug Map.empty
		[]
		"\n\
		\weaving\n\
		\==========================\n"
	$expect $ equalWeave loomDebug Map.empty
		[ BlockText "foo"
		, BlockText "bar"
		, BlockFile "file-1.hs" []
		, BlockFile "file-2.hs"
			[ ContentText (Position "test" 0) "foo"
			, ContentMacro (Position "test" 0) "foo" "bar"
			]
		]
		"\n\
		\weaving\n\
		\==========================\n\
		\BlockText \"foo\"\n\
		\BlockText \"bar\"\n\
		\BlockFile \"file-1.hs\" []\n\
		\BlockFile \"file-2.hs\"\
		\ [ContentText\
		  \ (Position {positionFile = FilePath \"test\", positionLine = 0})\
		  \ \"foo\",\
		\ContentMacro\
		  \ (Position {positionFile = FilePath \"test\", positionLine = 0})\
		  \ \"foo\" \"bar\"]\n"

test_WeaveHtml :: Suite
test_WeaveHtml = test $ assertions "html" $ do
	$expect $ equalWeave loomHTML Map.empty
		[]
		""
	$expect $ equalWeave loomHTML Map.empty
		[ BlockText "foo"
		, BlockText " < & \" ' > "
		, BlockText "bar\n"
		, BlockFile "file-1.hs" []
		, BlockDefine "macro < & \" ' > 2"
			[ ContentText (Position "test" 0) "\tfoo"
			]
		, BlockFile "file-2.hs"
			[ ContentText (Position "test" 0) "foo < & \" ' > bar"
			, ContentMacro (Position "test" 0) "foo" "bar"
			]
		]
		"foo < & \" ' > bar\n\
		\<pre><b>&#xBB; file-1.hs</b>\n\
		\</pre><pre><b>&#xAB;macro &lt; &amp; &quot; &apos; &gt; 2&#xBB;</b>\n\
		\\tfoo\n\
		\</pre><pre><b>&#xBB; file-2.hs</b>\n\
		\foo &lt; &amp; &quot; &apos; &gt; bar\n\
		\foo<i>&#xAB;bar&#xBB;</i>\n\
		\</pre>\n"

test_WeaveLatex :: Suite
test_WeaveLatex = test $ assertions "latex" $ do
	$expect $ equalWeave loomLaTeX Map.empty
		[]
		""
	$expect $ equalWeave loomLaTeX Map.empty
		[ BlockText "foo"
		, BlockText " { \\ _ } "
		, BlockText "bar\n"
		, BlockFile "file-1.hs" []
		, BlockDefine "macro { \\ _ } 2"
			[ ContentText (Position "test" 0) "\tfoo"
			]
		, BlockFile "file-2.hs"
			[ ContentText (Position "test" 0) "foo { \\ _ } bar"
			, ContentMacro (Position "test" 0) "foo" "bar"
			]
		]
		"foo { \\ _ } bar\n\
		\\\begin{alltt}\n\
		\{\\bf\\(\\gg\\) file-1.hs}\n\
		\\\end{alltt}\n\
		\\\begin{alltt}\n\
		\{\\bf\\(\\ll\\)macro \\{ \\textbackslash{} \\_ \\} 2\\(\\gg\\)}\n\
		\        foo\n\
		\\\end{alltt}\n\
		\\\begin{alltt}\n\
		\{\\bf\\(\\gg\\) file-2.hs}\n\
		\foo \\{ \\textbackslash{} \\_ \\} bar\n\
		\foo|\\emph{bar}|\n\
		\\\end{alltt}\n"


test_WeaveNoweb :: Suite
test_WeaveNoweb = test $ assertions "noweb" $ do
	$expect $ equalWeave loomNoWeb Map.empty
		[]
		""
	$expect $ equalWeave loomNoWeb Map.empty
		[ BlockText "foo"
		, BlockText " { < \t \\ _ $ > } "
		, BlockText "bar\n"
		, BlockFile "file-1.hs" []
		, BlockDefine "macro { < \t \\ _ $ > } 2"
			[ ContentText (Position "test" 0) "\tfoo"
			]
		, BlockFile "file-2.hs"
			[ ContentText (Position "test" 0) "foo { \t \\ _ } bar"
			, ContentMacro (Position "test" 0) "foo" "bar"
			]
		]
		"foo { < \t \\ _ $ > } bar\n\
		\\\nwbegincode{0}\\moddef{file-1.hs}\\endmoddef\\nwstartdeflinemarkup\\nwenddeflinemarkup\n\
		\\\nwendcode{}\n\
		\\\nwbegincode{0}\
		\\\moddef{macro \\{ {$<$}          \\\\ \\_ \\$ {$>$} \\} 2}\\endmoddef\
		\\\nwstartdeflinemarkup\\nwenddeflinemarkup\n\
		\        foo\n\
		\\\nwendcode{}\n\
		\\\nwbegincode{0}\\moddef{file-2.hs}\\endmoddef\
		\\\nwstartdeflinemarkup\\nwenddeflinemarkup\n\
		\foo \\{          \\\\ \\_ \\} bar\n\
		\foo\\LA{}bar\\RA{}\n\
		\\\nwendcode{}\n"

equalWeave :: Loom -> Map.Map Text Text -> [Block] -> Text -> Assertion
equalWeave loom opts blocks = equalLines (weave loom doc) where
	doc = Document
		{ documentBlocks = blocks
		, documentOptions = opts
		}
