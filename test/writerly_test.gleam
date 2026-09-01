import dirtree.{Dirpath, Filepath} as _dt
import gleam/list
import gleam/option.{Some}
import gleam/string
import gleeunit
import gleeunit/should
import vxml.{type Attr, Attr, Line}
import vxml/blame.{Anchored, Movable, Src, no_blame} as _bl
import vxml/io_lines.{InputLine} as io_l
import writerly.{type Writerly, Paragraph} as wl

pub fn main() -> Nil {
  gleeunit.main()
}

fn trim_fixture_end(source: String) -> String {
  case string.ends_with(source, " ") {
    True -> source |> string.drop_end(1) |> trim_fixture_end
    False ->
      case string.ends_with(source, "\n") {
        True -> source |> string.drop_end(1)
        False -> source
      }
  }
}

fn trim_fixture(source: String) -> String {
  case string.starts_with(source, "\n") {
    True -> string.drop_start(source, 1) |> trim_fixture_end
    False -> source |> trim_fixture_end
  }
}

/// Turn a legibly indented two-space test fixture into four-space Writerly.
fn normalize_fixture(source: String) -> String {
  let lines =
    source
    |> trim_fixture
    |> io_l.string_to_input_lines("", 0)

  let assert [first, ..] = lines

  lines
  |> list.map(fn(l) { InputLine(..l, indent: { l.indent - first.indent } * 2) })
  |> io_l.input_lines_to_string
}

fn parse_fixture(source: String) -> Writerly {
  let assert Ok(writerly) =
    source
    |> normalize_fixture
    |> wl.string_to_writerly("fixture.wly")
  writerly
}

fn assert_round_trip(source: String) {
  source
  |> parse_fixture
  |> wl.writerly_to_string
  |> should.equal(normalize_fixture(source))
}

fn attr_pairs(attrs: List(Attr)) -> List(#(String, String)) {
  list.map(attrs, fn(attr) { #(attr.key, attr.val) })
}

pub fn assembler_reads_a_single_file_with_relative_blame_paths_test() {
  wl.assemble_input_lines("test/test1.wly")
  |> should.equal(
    Ok(
      #(Dirpath("test", [Filepath("test1.wly")]), [
        InputLine(Src([], "test1.wly", 1, 1, Movable), 0, "|> Book"),
        InputLine(Src([], "test1.wly", 2, 5, Movable), 4, "bob=2"),
        InputLine(Src([], "test1.wly", 3, 5, Movable), 4, "cuchua"),
        InputLine(Src([], "test1.wly", 4, 1, Movable), 0, ""),
      ]),
    ),
  )
}

pub fn assembler_nests_child_files_below_the_parent_document_test() {
  wl.assemble_input_lines("test/testA")
  |> should.equal(
    Ok(
      #(
        Dirpath("test/testA", [Filepath("__parent.wly"), Filepath("childA.wly")]),
        [
          InputLine(Src([], "__parent.wly", 1, 1, Movable), 0, "|> Book"),
          InputLine(Src([], "__parent.wly", 2, 5, Movable), 4, "a=b"),
          InputLine(
            Src([], "childA.wly", 1, 1, Movable),
            4,
            "It was a dark and stormy night.",
          ),
        ],
      ),
    ),
  )
}

pub fn path_selector_from_only_paths_test() {
  let include_chapter_1 = wl.path_selector_from_only_paths(["chapter-1"])
  include_chapter_1("book/chapter-1/section.wly")
  |> should.be_true
  include_chapter_1("book/chapter-2/section.wly")
  |> should.be_false

  let exclude_draft = wl.path_selector_from_only_paths(["!draft"])
  exclude_draft("book/chapter-1/section.wly")
  |> should.be_true
  exclude_draft("book/draft/section.wly")
  |> should.be_false

  let include_chapter_1_but_exclude_draft =
    wl.path_selector_from_only_paths(["chapter-1", "!draft"])
  include_chapter_1_but_exclude_draft("book/chapter-1/section.wly")
  |> should.be_true
  include_chapter_1_but_exclude_draft("book/chapter-1/draft/section.wly")
  |> should.be_false
  include_chapter_1_but_exclude_draft("book/chapter-2/section.wly")
  |> should.be_false
}

pub fn parser_builds_a_tag_with_an_attribute_test() {
  wl.string_to_writerlys("|> Book\n    a=b", "doc")
  |> should.equal(
    Ok([
      wl.Tag(
        Src([], "doc", 1, 1, Anchored),
        "Book",
        [
          Attr(Src([], "doc", 2, 5, Movable), "a", "b"),
        ],
        [],
      ),
    ]),
  )
}

pub fn parser_preserves_paragraph_source_blame_test() {
  let assert Ok(#(_tree, lines)) = wl.assemble_input_lines("test/test1.wly")
  lines
  |> wl.input_lines_to_writerlys()
  |> should.equal(
    Ok([
      wl.Tag(
        Src([], "test1.wly", 1, 1, Anchored),
        "Book",
        [
          Attr(Src([], "test1.wly", 2, 5, Movable), "bob", "2"),
        ],
        [
          Paragraph(Src([], "test1.wly", 3, 5, Movable), [
            Line(Src([], "test1.wly", 3, 5, Movable), "cuchua"),
          ]),
        ],
      ),
    ]),
  )
}

pub fn parser_preserves_blame_across_assembled_files_test() {
  let assert Ok(#(_tree, lines)) = wl.assemble_input_lines("test/testA")
  lines
  |> wl.input_lines_to_writerlys()
  |> should.equal(
    Ok([
      wl.Tag(
        Src([], "__parent.wly", 1, 1, Anchored),
        "Book",
        [
          Attr(Src([], "__parent.wly", 2, 5, Movable), "a", "b"),
        ],
        [
          Paragraph(Src([], "childA.wly", 1, 1, Movable), [
            Line(
              Src([], "childA.wly", 1, 1, Movable),
              "It was a dark and stormy night.",
            ),
          ]),
        ],
      ),
    ]),
  )
}

pub fn parser_rejects_invalid_tag_names_test() {
  let assert Error(wl.BadTag(_, "not a tag")) =
    wl.string_to_writerly("|> not a tag", "doc")
}

pub fn parser_rejects_indentation_not_divisible_by_four_test() {
  let assert Error(wl.IndentationNotMultipleOfFour(_, "too shallow")) =
    wl.string_to_writerly("|> Book\n  too shallow", "doc")
}

pub fn parser_rejects_skipped_indentation_levels_test() {
  let assert Error(wl.IndentationTooLarge(_, _, _, _)) =
    wl.string_to_writerly("|> Book\n        too deep", "doc")
}

pub fn parser_rejects_unclosed_code_blocks_test() {
  let assert Error(wl.CodeBlockNotClosed(_)) =
    wl.string_to_writerly("|> Book\n    ```gleam\n    pub fn main() {}", "doc")
}

pub fn parser_rejects_annotated_closing_code_fences_test() {
  let assert Error(wl.CodeBlockUnwantedAnnotationAtClose(_, _, "gleam")) =
    wl.string_to_writerly(
      "|> Book\n    ```gleam\n    body\n    ```gleam",
      "doc",
    )
}

pub fn parser_rejects_invalid_code_block_attribute_keys_test() {
  let assert Error(wl.BadKey(_, "bad key")) =
    wl.string_to_writerly("|> Book\n    ```gleam&bad key=value\n    ```", "doc")
}

pub fn single_root_parser_reports_missing_and_nonunique_roots_test() {
  let assert Error(wl.MissingRoot(_)) = wl.string_to_writerly("\n", "doc")
  let assert Error(wl.NonUniqueRoot(_)) =
    wl.string_to_writerly("first\n\nsecond", "doc")
}

pub fn writerly_tag_converts_to_vxml_element_test() {
  let assert Ok(wly_parsed) = wl.string_to_writerly("|> Book\n    a=b", "doc")

  wly_parsed
  |> wl.writerly_to_vxml()
  |> should.equal(
    vxml.V(
      Src([], "doc", 1, 1, Anchored),
      "Book",
      [Attr(Src([], "doc", 2, 5, Movable), "a", "b")],
      [],
    ),
  )
}

pub fn vxml_text_node_converts_to_writerly_paragraph_test() {
  let vxml_doc = "<> Book\n  a=b\n  <>\n    'first'\n    'second'"

  let assert Ok([vxml_parsed]) = vxml.parse_string(vxml_doc, "doc", True)

  vxml_parsed
  |> wl.vxml_to_writerly
  |> should.equal(
    Ok(
      wl.Tag(
        Src([], "doc", 1, 1, Anchored),
        "Book",
        [Attr(Src([], "doc", 2, 3, Movable), "a", "b")],
        [
          Paragraph(Src([], "doc", 4, 5, Movable), [
            Line(Src([], "doc", 4, 5, Movable), "first"),
            Line(Src([], "doc", 5, 5, Movable), "second"),
          ]),
        ],
      ),
    ),
  )
}

pub fn round_trip_preserves_escaped_code_fences_test() {
  "
  |> Book
    a=b
    ```
    \\```
    ```
  "
  |> assert_round_trip
}

pub fn round_trip_preserves_escaped_paragraph_indentation_test() {
  "
  |> Book
    A paragraph with
    \\ an escaped space
    at the beginning of the second line
  "
  |> assert_round_trip
}

pub fn round_trip_preserves_code_block_indentation_test() {
  "
  |> Book
    ```
      hallo
    \\```
    \\\\```
    ```
  "
  |> assert_round_trip
}

pub fn round_trip_preserves_comments_and_trailing_spaces_test() {
  "
  |> Book
    a=b
    !!someguy=aa
    t=w

    A paragraph with  
    \\ an escaped space
    \\\\ an escaped space
    at the beginning of the second line   
  "
  |> assert_round_trip
}

pub fn commented_attribute_encoding_test() {
  let source = "|> Book\n    !!   someguy=aa"
  let assert Ok(writerly) = wl.string_to_writerly(source, "doc")
  let assert wl.Tag(_, "Book", [Attr(_, key, val)], []) = writerly

  key |> should.equal("WriterlyCommentedAttribute3Spaces")
  val |> should.equal("someguy=aa")
  writerly |> wl.writerly_to_string |> should.equal(source)

  let empty_source = "|> Book\n    !!"
  let assert Ok(empty_writerly) = wl.string_to_writerly(empty_source, "doc")
  let assert wl.Tag(_, "Book", [Attr(_, empty_key, "")], []) = empty_writerly
  empty_key |> should.equal("WriterlyCommentedAttribute0Spaces")
  empty_writerly |> wl.writerly_to_string |> should.equal(empty_source)
}

pub fn commented_attribute_key_helpers_test() {
  wl.commented_attribute_spaces("WriterlyCommentedAttribute0Spaces")
  |> should.equal(Some(0))
  wl.commented_attribute_spaces("WriterlyCommentedAttribute100Spaces")
  |> should.equal(Some(100))
  wl.is_commented_attribute_key("WriterlyCommentedAttribute101Spaces")
  |> should.be_false
  wl.commented_attribute_spaces("WriterlyCommentedAttribute01Spaces")
  |> should.equal(Some(1))
}

pub fn code_block_info_string_and_attributes_round_trip_test() {
  let source =
    "|> Book\n    ```python&id=example&title=a\\&b&path=c\\\\d&empty=\n    body\n    ```"
  let assert Ok(
    wl.Tag(_, "Book", _, [wl.CodeBlock(_, attrs, [Line(_, "body")])]) as writerly,
  ) = wl.string_to_writerly(source, "doc")
  attr_pairs(attrs)
  |> should.equal([
    #(wl.code_block_info_string_attribute_key, "python"),
    #("id", "example"),
    #("title", "a&b"),
    #("path", "c\\d"),
    #("empty", ""),
  ])
  writerly |> wl.writerly_to_string |> should.equal(source)
}

pub fn code_block_info_string_attribute_may_appear_anywhere_test() {
  let writerly =
    wl.CodeBlock(
      no_blame,
      [
        Attr(no_blame, "id", "example"),
        Attr(no_blame, wl.code_block_info_string_attribute_key, " python "),
        Attr(no_blame, "class", " listing "),
      ],
      [],
    )

  let serialized = "```python&id=example&class=listing\n```"
  writerly |> wl.writerly_to_string |> should.equal(serialized)

  let assert Ok(wl.CodeBlock(_, attrs, [])) =
    wl.string_to_writerly(serialized, "doc")
  attr_pairs(attrs)
  |> should.equal([
    #(wl.code_block_info_string_attribute_key, "python"),
    #("id", "example"),
    #("class", "listing"),
  ])
}

fn assert_excessive_leading_spaces(source: String) {
  let assert Error(wl.ExcessiveLeadingAttributeSpaces(_, 100, 101)) =
    wl.string_to_writerly(source, "doc")
  Nil
}

pub fn parser_rejects_excessive_leading_attribute_spaces_test() {
  let excessive = string.repeat(" ", 101)

  assert_excessive_leading_spaces("|> Book\n    key=" <> excessive <> "hidden")

  assert_excessive_leading_spaces("|> Book\n    !!" <> excessive <> "key=value")

  assert_excessive_leading_spaces(
    "|> Book\n    ```python&id=" <> excessive <> "hidden\n    ```",
  )
}
