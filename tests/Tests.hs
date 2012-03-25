{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (tests, main) where

import           Prelude hiding (FilePath)

import qualified Control.Monad.State as State
import           Control.Monad.Identity (runIdentity)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Text as Text
import           Data.Text (Text)

import           Filesystem.Path (FilePath)
import qualified Filesystem.Path.Rules as Path
import           Test.Chell

import           Anansi
import           Anansi.Types

main :: IO ()
main = Test.Chell.defaultMain tests

tests :: [Suite]
tests =
	[ test_Parse
	, test_Tangle
	, test_Weave
	]

test_Parse :: Suite
test_Parse = suite "parse"
	[ test_ParseFull
	, test_ParseInclude
	, test_ParseLoom
	, test_ParseUnknownCommand
	, test_ParseInvalidOption
	, test_ParseUnexpectedTerminator
	, test_ParseUnterminatedBlock
	, test_ParseInvalidContent
	, test_ParseIndentedMacro
	, test_ParseInvalidMacroName
	]

test_ParseFull :: Suite
test_ParseFull = assertions "full" $ do
	let bytes =
		"hello\n\
		\:option opt-1=foo\n\
		\:option opt-2=bar\n\
		\:#a comment\n\
		\::blue\n\
		\world\n\
		\:f test.hs\n\
		\foo\n\
		\:\n\
		\:d macro\n\
		\bar\n\
		\:\n\
		\:f test.hs\n\
		\|macro|\n\
		\:\n"
	let blocks =
		[ BlockText "hello\n"
		, BlockText ":"
		, BlockText "blue\n"
		, BlockText "world\n"
		, BlockFile "test.hs"
			[ ContentText (Position "test.in" 8) "foo"
			]
		, BlockDefine "macro"
			[ ContentText (Position "test.in" 11) "bar"
			]
		, BlockFile "test.hs"
			[ ContentMacro (Position "test.in" 14) "" "macro"
			]
		]
	let options = Map.fromList [("opt-1", "foo"), ("opt-2", "bar")]
	
	let res = runParse "test.in" [("test.in", bytes)]
	$assert (right res)
	let Right doc = res
	
	$expect (equalItems (documentBlocks doc) blocks)
	$expect (sameItems (documentOptions doc) options)
	$expect (equal (documentLoomName doc) Nothing)

test_ParseInclude :: Suite
test_ParseInclude = assertions "include" $ do
	$expect $ equal
		(runParse "data/test-1.in"
			[ ("data/test-1.in", ":i test-2.in\n")
			, ("./data/test-2.in", ":d macro\nfoo\n:\n")
			])
		(Right (Document
			{ documentOptions = Map.empty
			, documentBlocks =
				[ BlockDefine "macro"
					[ ContentText (Position "./data/test-2.in" 2) "foo"
					]
				]
			, documentLoomName = Nothing
			}))

test_ParseLoom :: Suite
test_ParseLoom = assertions "loom" $ do
	$expect $ equal
		(runParse "test.in" [("test.in", ":loom anansi.latex\n")])
		(Right (Document
			{ documentOptions = Map.empty
			, documentBlocks = []
			, documentLoomName = Just "anansi.latex"
			}))

test_ParseUnknownCommand :: Suite
test_ParseUnknownCommand = assertions "unknown-command" $ do
	$expect $ equal
		(runParse "test.in" [("test.in", ":bad\n")])
		(Left (ParseError (Position "test.in" 1) "unknown command: \":bad\""))

test_ParseInvalidOption :: Suite
test_ParseInvalidOption = assertions "invalid-option" $ do
	$expect $ equal
		(runParse "test.in" [("test.in", ":option foo bar\nbaz")])
		(Left (ParseError (Position "test.in" 1) "Invalid option: \"foo bar\""))

test_ParseUnexpectedTerminator :: Suite
test_ParseUnexpectedTerminator = assertions "unexpected-terminator" $ do
	$expect $ equal
		(runParse "test.in" [("test.in", ":\n")])
		(Left (ParseError (Position "test.in" 1) "Unexpected block terminator"))

test_ParseUnterminatedBlock :: Suite
test_ParseUnterminatedBlock = assertions "unterminated-block" $ do
	$expect $ equal
		(runParse "test.in" [("test.in", ":f foo.hs\n")])
		(Left (ParseError (Position "test.in" 1) "Unterminated content block"))
	$expect $ equal
		(runParse "test.in" [("test.in", ":d macro\n")])
		(Left (ParseError (Position "test.in" 1) "Unterminated content block"))
	$expect $ equal
		(runParse "test.in" [("test.in", ":f foo.hs\n:#\n")])
		(Left (ParseError (Position "test.in" 1) "Unterminated content block"))

test_ParseInvalidContent :: Suite
test_ParseInvalidContent = assertions "invalid-content" $ do
	$expect $ equal
		(runParse "test.in" [("test.in", ":f foo.hs\n|bad\n")])
		(Left (ParseError (Position "test.in" 2) "Invalid content line: \"|bad\""))
	$expect $ equal
		(runParse "test.in" [("test.in", ":f foo.hs\n|bad\n")])
		(Left (ParseError (Position "test.in" 2) "Invalid content line: \"|bad\""))

test_ParseIndentedMacro :: Suite
test_ParseIndentedMacro = assertions "indented-macro" $ do
	$expect $ equal
		(runParse "test.in" [("test.in", ByteString.unlines
			[ ":d macro-foo"
			, "foo"
			, ":"
			, ":f foo.hs"
			, "  |foo|"
			, "\t|foo|"
			, ":"
			])])
		(Right (Document
			{ documentOptions = Map.empty
			, documentBlocks =
				[ BlockDefine "macro-foo"
					[ ContentText (Position "test.in" 2) "foo"
					]
				,BlockFile "foo.hs"
					[ ContentMacro (Position "test.in" 5) "  " "foo"
					, ContentMacro (Position "test.in" 6) "\t" "foo"
					]
				]
			, documentLoomName = Nothing
			}))

test_ParseInvalidMacroName :: Suite
test_ParseInvalidMacroName = assertions "invalid-macro-name" $ do
	$expect $ equal
		(runParse "test.in" [("test.in", ":d foo|bar\n:\n")])
		(Left (ParseError (Position "test.in" 1) "Invalid macro name: \"foo|bar\""))

runParse :: FilePath -> [(FilePath, ByteString)] -> Either ParseError Document
runParse root files = runIdentity (parse getFile root) where
	getFile p = return bytes where
		Just bytes = lookup p files

test_Tangle :: Suite
test_Tangle = assertions "tangle" $ do
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
	$expect $ equalTangle True [] blocks "file-1.hs"
		""
	$expect $ equalTangle True [] blocks "file-2.hs"
		"#line 0 \"test\"\n\
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
	$expect $ equalTangle False [] blocks "file-1.hs"
		""
	$expect $ equalTangle False [] blocks "file-2.hs"
		"foo\n\
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
	
	-- test custom #line formatting
	$expect $ equalTangle True [("anansi.line-pragma-hs", "#line ${line}")] blocks "file-2.hs"
		"#line 0\n\
		\foo\n\
		\bar\n\
		\\n\
		\#line 3\n\
		\baz\n\
		\\n\
		\#line 0\n\
		\  macro-1\n\
		\\n\
		\#line 2\n\
		\  macro-2\n\
		\\n\
		\#line 4\n\
		\\n\
		\#line 6\n\
		\    macro-3\n\
		\\n\
		\#line 6\n\
		\qux\n\
		\\n\
		\#line 6\n\
		\  macro-3\n"

equalTangle :: Bool -> [(Text, Text)] -> [Block] -> Text -> ByteString -> Assertion
equalTangle enableLinePragma opts blocks filename expected = equalLines
	expected
	(let doc = Document blocks (Map.fromList opts) Nothing in
	(case Map.lookup filename (runTangle enableLinePragma doc) of
		Nothing -> ""
		Just txt -> txt))

runTangle :: Bool -> Document -> Map Text ByteString
runTangle enableLinePragma doc = State.execState st Map.empty where
	st = tangle putFile enableLinePragma doc
	putFile path txt = State.modify $ Map.insert
		(Text.pack (Path.encodeString Path.posix path))
		txt

test_Weave :: Suite
test_Weave = suite "weave"
	[ test_WeaveDebug
	, test_WeaveHtml
	, test_WeaveLatex
	, test_WeaveMarkdown
	, test_WeaveNoweb
	, test_ParseLoomOptions
	]

test_WeaveDebug :: Suite
test_WeaveDebug = assertions "debug" $ do
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
test_WeaveHtml = assertions "html" $ do
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
		\        foo\n\
		\</pre><pre><b>&#xBB; file-2.hs</b>\n\
		\foo &lt; &amp; &quot; &apos; &gt; bar\n\
		\  <i>&#xAB;bar&#xBB;</i>\n\
		\</pre>\n"

test_WeaveLatex :: Suite
test_WeaveLatex = assertions "latex" $ do
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

test_WeaveMarkdown :: Suite
test_WeaveMarkdown = assertions "markdown" $ do
	$expect $ equalWeave loomMarkdown Map.empty
		[]
		""
	$expect $ equalWeave loomMarkdown Map.empty
		[ BlockText "foo"
		, BlockText " _ * \\ & ` []() "
		, BlockText "bar\n"
		, BlockFile "file-1.hs" []
		, BlockDefine "macro _ * \\ & ` []() 2"
			[ ContentText (Position "test" 0) "\tfoo"
			]
		, BlockFile "file-2.hs"
			[ ContentText (Position "test" 0) "foo _ * \\ & ` []() bar"
			, ContentMacro (Position "test" 0) "  " "macro _ * \\ & ` []()"
			]
		]
		"foo _ * \\ & ` []() bar\n\
		\\n\
		\> **\xC2\xBB file-1.hs**\n\
		\\n\
		\\n\
		\\n\
		\> **\xC2\xAB\&macro \\_ \\* \\\\ &amp; \\` \\[\\]() 2\xC2\xBB**\n\
		\\n\
		\>             foo\n\
		\\n\
		\\n\
		\> **\xC2\xBB file-2.hs**\n\
		\\n\
		\>     foo _ * \\ & ` []() bar\n\
		\>       \xC2\xAB\&macro _ * \\ & ` []()\xC2\xBB\n\
		\\n"

test_WeaveNoweb :: Suite
test_WeaveNoweb = assertions "noweb" $ do
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

equalWeave :: Loom -> Map.Map Text Text -> [Block] -> ByteString -> Assertion
equalWeave loom opts blocks expected = equalLines expected (weave loom doc) where
	doc = Document
		{ documentBlocks = blocks
		, documentOptions = opts
		, documentLoomName = Nothing
		}

test_ParseLoomOptions :: Suite
test_ParseLoomOptions = assertions "parseLoomOptions" $ do
	$expect $ equal
		(parseLoomOptions Map.empty)
		(LoomOptions
			{ loomOptionTabSize = 8
			})
	$expect $ equal
		(parseLoomOptions (Map.fromList [("tab-size", "4")]))
		(LoomOptions
			{ loomOptionTabSize = 4
			})
