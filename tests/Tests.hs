{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (tests, main) where

import qualified Control.Monad.State as State
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Text as Text
import           Data.Text (Text)

import qualified Filesystem.Path.Rules as Path
import           Test.Chell

import           Anansi
import           Anansi.Types

main :: IO ()
main = Test.Chell.defaultMain tests

tests :: [Suite]
tests =
	[ suite_Parse
	, test_Tangle
	, test_Weave
	]

suite_Parse :: Suite
suite_Parse = suite "parse"
	[]

test_Tangle :: Suite
test_Tangle = test $ assertions "tangle" $ do
	let blocks =
		[ BlockText "foo\n"
		, BlockFile "file-1.hs" []
		, BlockFile "file-2.hs"
			[ ContentText (Position "test" 0) "foo"
			]
		, BlockFile "file-2.hs"
			[ ContentText (Position "test" 1) "bar"
			, ContentText (Position "test" 3) "baz"
			, ContentMacro (Position "test" 4) "  " "macro-a"
			]
		, BlockDefine "macro-a"
			[ ContentText (Position "test2" 0) "macro-1"
			]
		, BlockDefine "macro-a"
			[ ContentText (Position "test2" 2) "macro-2"
			, ContentMacro (Position "test2" 4) "  " "macro-b"
			]
		, BlockDefine "macro-b"
			[ ContentText (Position "test2" 6) "macro-3"
			]
		, BlockFile "file-2.hs"
			[ ContentText (Position "test" 6) "qux"
			, ContentMacro (Position "test" 7) "  " "macro-b"
			]
		]
	$expect $ equalTangle True blocks "file-1.hs"
		""
	$expect $ equalTangle True blocks "file-2.hs"
		"\n\
		\#line 0 \"test\"\n\
		\foo\n\
		\bar\n\
		\\n\
		\#line 3 \"test\"\n\
		\baz\n\
		\\n\
		\#line 0 \"test2\"\n\
		\  macro-1\n\
		\\n\
		\#line 2 \"test2\"\n\
		\  macro-2\n\
		\\n\
		\#line 4 \"test2\"\n\
		\\n\
		\#line 6 \"test2\"\n\
		\    macro-3\n\
		\\n\
		\#line 6 \"test\"\n\
		\qux\n\
		\\n\
		\#line 6 \"test2\"\n\
		\  macro-3\n"
	$expect $ equalTangle False blocks "file-1.hs"
		""
	$expect $ equalTangle False blocks "file-2.hs"
		"\n\
		\foo\n\
		\bar\n\
		\\n\
		\baz\n\
		\\n\
		\  macro-1\n\
		\\n\
		\  macro-2\n\
		\\n\
		\\n\
		\    macro-3\n\
		\\n\
		\qux\n\
		\\n\
		\  macro-3\n"

equalTangle :: Bool -> [Block] -> Text -> Text -> Assertion
equalTangle enableLinePragma blocks filename expected = equalLines
	expected
	(case Map.lookup filename (runTangle enableLinePragma blocks) of
		Nothing -> ""
		Just txt -> txt)

runTangle :: Bool -> [Block] -> Map Text Text
runTangle enableLinePragma blocks = State.execState st Map.empty where
	st = tangle putFile enableLinePragma blocks
	putFile path txt = State.modify $ Map.insert
		(Text.pack (Path.encodeString Path.posix path))
		txt

test_Weave :: Suite
test_Weave = suite "weave"
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
			, ContentMacro (Position "test" 0) "  " "bar"
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
		  \ \"  \" \"bar\"]\n"

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
			, ContentMacro (Position "test" 0) "  " "bar"
			]
		]
		"foo < & \" ' > bar\n\
		\<pre><b>&#xBB; file-1.hs</b>\n\
		\</pre><pre><b>&#xAB;macro &lt; &amp; &quot; &apos; &gt; 2&#xBB;</b>\n\
		\\tfoo\n\
		\</pre><pre><b>&#xBB; file-2.hs</b>\n\
		\foo &lt; &amp; &quot; &apos; &gt; bar\n\
		\  <i>&#xAB;bar&#xBB;</i>\n\
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
			, ContentMacro (Position "test" 0) "  " "bar"
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
		\  |\\emph{bar}|\n\
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
			, ContentMacro (Position "test" 0) "  " "bar"
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
		\  \\LA{}bar\\RA{}\n\
		\\\nwendcode{}\n"

equalWeave :: Loom -> Map.Map Text Text -> [Block] -> Text -> Assertion
equalWeave loom opts blocks = equalLines (weave loom doc) where
	doc = Document
		{ documentBlocks = blocks
		, documentOptions = opts
		}
