//// Parse, serialize, assemble, and convert Writerly documents.
////
//// Writerly is an indentation-based markup language descended from
//// [Elm-Markup](https://github.com/mdgriffith/elm-markup). It represents
//// elements, attributes, paragraphs, blank lines, comments, and fenced code
//// blocks, and converts them to and from VXML.

import dirtree.{type DirTree} as dt
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/regexp.{type Regexp}
import gleam/result
import gleam/string.{inspect as ins}
import on
import simplifile
import vxml.{type Attr, type Line, type VXML, Attr, Line, T, V}
import vxml/blame.{type Blame, prepend_comment as pc} as bl
import vxml/io_lines.{type InputLine, type OutputLine, InputLine, OutputLine} as io_l

// ************************************************************
// public types
// ************************************************************

/// A node in a parsed Writerly document.
///
/// Every node and its contained VXML values retain source-provenance `Blame`.
pub type Writerly {
  BlankLine(blame: Blame)
  Paragraph(blame: Blame, lines: List(Line))
  Comment(blame: Blame, lines: List(Line))
  CodeBlock(blame: Blame, attrs: List(Attr), lines: List(Line))
  Tag(blame: Blame, name: String, attrs: List(Attr), children: List(Writerly))
}

/// An error encountered while parsing Writerly source.
pub type ParseError {
  BadTag(blame: Blame, bad_name: String)
  BadKey(blame: Blame, bad_key: String)
  IndentationTooLarge(
    blame: Blame,
    expected: String,
    found: String,
    line: String,
  )
  IndentationNotMultipleOfFour(blame: Blame, line: String)
  CodeBlockInfoStartsWithSpace(blame: Blame, bad_info: String)
  CodeBlockNotClosed(blame: Blame)
  CodeBlockUnwantedAnnotationAtClose(
    blame: Blame,
    opening_blame: Blame,
    annotation: String,
  )
  ExcessiveLeadingAttributeSpaces(blame: Blame, maximum: Int, found: Int)
  NonUniqueRoot(blame: Blame)
  MissingRoot(blame: Blame)
}

/// An error encountered while assembling Writerly source files.
pub type AssemblyError {
  ReadFileError(String)
  ReadFileOrDirectoryError(String)
  NoFilesFound(String)
}

const commented_attribute_key_prefix = "WriterlyCommentedAttribute"

const commented_attribute_key_suffix = "Spaces"

const maximum_leading_attribute_spaces = 100

/// The synthetic VXML attribute that carries a code block's leading info
/// string separately from its structured `&key=value` annotations.
pub const code_block_info_string_attribute_key = "WriterlyCodeBlockInfoString"

/// Returns the encoded number of spaces in a Writerly commented attribute.
/// Only keys encoding between zero and 100 spaces are recognized.
pub fn commented_attribute_spaces(key: String) -> Option(Int) {
  case string.starts_with(key, commented_attribute_key_prefix) {
    False -> None
    True ->
      case string.ends_with(key, commented_attribute_key_suffix) {
        False -> None
        True -> {
          let encoded =
            key
            |> string.drop_start(string.length(commented_attribute_key_prefix))
            |> string.drop_end(string.length(commented_attribute_key_suffix))

          case int.parse(encoded) {
            Ok(spaces)
              if spaces >= 0 && spaces <= maximum_leading_attribute_spaces
            -> Some(spaces)
            _ -> None
          }
        }
      }
  }
}

/// Returns whether a key is a valid Writerly commented-attribute encoding.
pub fn is_commented_attribute_key(key: String) -> Bool {
  commented_attribute_spaces(key) != None
}

/// Constructs a Writerly commented-attribute key for a bounded space count.
pub fn commented_attribute_key(spaces: Int) -> Option(String) {
  case spaces >= 0 && spaces <= maximum_leading_attribute_spaces {
    True -> Some(make_commented_attribute_key(spaces))
    False -> None
  }
}

fn make_commented_attribute_key(spaces: Int) -> String {
  commented_attribute_key_prefix
  <> int.to_string(spaces)
  <> commented_attribute_key_suffix
}

fn split_leading_spaces(value: String, spaces: Int) -> #(Int, String) {
  case string.starts_with(value, " ") {
    True -> split_leading_spaces(string.drop_start(value, 1), spaces + 1)
    False -> #(spaces, value)
  }
}

fn parse_attribute_value(
  blame: Blame,
  raw_value: String,
) -> Result(String, ParseError) {
  let #(spaces, _) = split_leading_spaces(raw_value, 0)
  case spaces > maximum_leading_attribute_spaces {
    True ->
      Error(ExcessiveLeadingAttributeSpaces(
        blame,
        maximum_leading_attribute_spaces,
        spaces,
      ))
    False -> Ok(string.trim(raw_value))
  }
}

// The API covers five areas of responsibility:
//
//    1. assembling List(InputLine) from a filepath or dirpath
//    2. List(InputLine) -> Writerly (parsing)
//    3. Writerly -> VXML
//    4. VXML -> Writerly
//    5. Writerly -> List(OutputLine) / String / String (debug table)
//
// See 'PART 1', 'PART 2', 'PART 3', 'PART 4', 'PART 5' below.

// ************************************************************
// PART 1
//
// directory or filepath -> List(InputLine)   // Result
// 
// pub fn assemble_input_lines
// ************************************************************

fn file_is_not_commented(path: String) -> Bool {
  !{ string.contains(path, "/#") || string.starts_with(path, "#") }
}

fn file_is_selected_or_has_selected_descendant(
  path_selector: fn(String) -> Bool,
  path: String,
  all_paths: List(String),
) -> Bool {
  path_selector(path)
  || {
    string.ends_with(path, "__parent.wly")
    && {
      let prefix = path |> string.drop_end(12)
      list.any(all_paths, fn(x) {
        string.starts_with(x, prefix) && path_selector(x)
      })
    }
  }
}

fn shortname_for_blame(path: String, dirname: String) -> String {
  assert string.starts_with(path, dirname)
  let length_to_drop = case string.ends_with(dirname, "/") || dirname == "" {
    True -> string.length(dirname)
    False -> string.length(dirname) + 1
  }
  string.drop_start(path, length_to_drop)
}

fn input_lines_for_file_at_depth(
  dirname: String,
  path: String,
  depth: Int,
) -> Result(List(InputLine), AssemblyError) {
  let shortname = shortname_for_blame(path, dirname)
  case shortname == "" {
    True ->
      panic as {
        "no shortname left after removing dirname '"
        <> dirname
        <> "' from path '"
        <> path
        <> "'"
      }
    False -> shortname
  }

  case simplifile.read(path) {
    Ok(string) -> {
      Ok(io_l.string_to_input_lines(string, shortname, 4 * depth))
    }
    Error(_) -> {
      Error(ReadFileError(path))
    }
  }
}

fn path_2_dir_and_filename(path: String) -> #(String, String) {
  let reversed_path = path |> string.reverse
  let #(reversed_filename, reversed_dir) =
    reversed_path
    |> string.split_once("/")
    |> result.unwrap(#(reversed_path, ""))
  #(reversed_dir |> string.reverse, reversed_filename |> string.reverse)
}

fn dir_and_filename_2_path(dir: String, path: String) -> String {
  case dir {
    "" -> path
    _ -> dir <> "/" <> path
  }
}

fn drop_slash(s: String) {
  case string.ends_with(s, "/") {
    True -> string.drop_end(s, 1)
    False -> s
  }
}

type AssemblyInputKind {
  DirectoryInput
  FileInput
}

fn is_direct_relative_path(path: String) -> Bool {
  !string.contains(path, "/")
}

fn get_dirname_and_relative_paths_of_uncommented_wly_in_dir(
  dirpath_or_filepath: String,
) -> Result(#(String, List(String)), AssemblyError) {
  use #(dirname, fullpaths_including_dirname, input_kind) <- on.ok(
    case simplifile.get_files(dirpath_or_filepath) {
      Ok(files) -> {
        Ok(#(dirpath_or_filepath |> drop_slash, files, DirectoryInput))
      }
      Error(simplifile.Enotdir) -> {
        let #(dirname, filepath) =
          dirpath_or_filepath |> path_2_dir_and_filename
        Ok(#(dirname, [dir_and_filename_2_path(dirname, filepath)], FileInput))
      }
      Error(error) ->
        Error(ReadFileOrDirectoryError(
          "error accessing dirpath_or_filepath:"
          <> dirpath_or_filepath
          <> ", "
          <> ins(error),
        ))
    },
  )

  assert !string.ends_with(dirname, "/")
  let dirname_length = string.length(dirname)
  let relative_filepaths =
    fullpaths_including_dirname
    |> list.filter(string.ends_with(_, ".wly"))
    |> list.filter(file_is_not_commented)
    |> list.map(fn(path) {
      let path = string.drop_start(path, dirname_length)
      assert string.starts_with(path, "/")
      string.drop_start(path, 1)
    })

  case input_kind {
    DirectoryInput ->
      case list.any(relative_filepaths, is_direct_relative_path) {
        True -> Ok(#(dirname, relative_filepaths))
        False ->
          Error(NoFilesFound(
            "no direct .wly files found in: " <> dirpath_or_filepath,
          ))
      }
    FileInput -> Ok(#(dirname, relative_filepaths))
  }
}

fn input_lines_for_dirtree_at_depth(
  original_dirname: String,
  acc: String,
  tree: DirTree,
  depth: Int,
) -> Result(List(InputLine), AssemblyError) {
  let added_depth = fn(prefixes: List(String), path: String) {
    list.fold(prefixes, 0, fn(acc, prefix) {
      case
        string.starts_with(path, prefix) && path != prefix <> "__parent.wly"
      {
        True -> acc + 1
        False -> acc
      }
    })
  }

  let parent_prefixes = fn(contents: List(DirTree)) -> List(String) {
    list.fold(contents, [], fn(acc, tree) {
      case string.ends_with(tree.name, "__parent.wly") {
        True -> {
          let assert dt.Filepath(_) = tree
          let prefix = string.drop_end(tree.name, 12)
          assert !list.contains(acc, prefix)
          [prefix, ..acc]
        }
        False -> acc
      }
    })
  }

  case tree {
    dt.Filepath(path) -> {
      assert string.ends_with(path, ".wly")
      input_lines_for_file_at_depth(
        original_dirname,
        dir_and_filename_2_path(acc, path),
        depth,
      )
    }

    dt.Dirpath(path, contents) -> {
      let prefixes = parent_prefixes(contents)
      use list_of_lists <- on.ok(
        list.try_map(contents, fn(subtree) {
          let depth = depth + added_depth(prefixes, subtree.name)
          input_lines_for_dirtree_at_depth(
            original_dirname,
            dir_and_filename_2_path(acc, path),
            subtree,
            depth,
          )
        }),
      )
      list_of_lists
      |> list.flatten
      |> Ok
    }
  }
}

/// Assembles selected `.wly` files from a file or directory.
///
/// The selector receives paths relative to the input directory. A selected
/// file is included along with any `__parent.wly` files needed to contain it.
/// The returned directory tree records the selected layout; the input lines
/// retain their relative source paths and are indented according to that tree.
pub fn assemble_input_lines_with_path_selector(
  dirpath_or_filepath: String,
  path_selector: fn(String) -> Bool,
) -> Result(#(DirTree, List(InputLine)), AssemblyError) {
  use #(dirname, paths) <- on.ok(
    get_dirname_and_relative_paths_of_uncommented_wly_in_dir(
      dirpath_or_filepath,
    ),
  )

  use _, _ <- on.empty_nonempty(paths, fn() {
    Error(NoFilesFound("no files found in: " <> dirpath_or_filepath))
  })

  let paths =
    paths
    |> list.filter(file_is_selected_or_has_selected_descendant(
      path_selector,
      _,
      paths,
    ))

  let drop_suffix = fn(name) {
    case string.ends_with(name, "__parent.wly") {
      True -> string.drop_end(name, 12)
      False -> name
    }
  }

  let tree =
    dt.from_terminals(dirname, paths)
    |> dt.sort(fn(t1, t2) {
      string.compare(t1.name |> drop_suffix, t2.name |> drop_suffix)
    })

  use lines <- on.ok(input_lines_for_dirtree_at_depth(dirname, "", tree, 0))

  Ok(#(tree, lines))
}

/// Builds a path selector from inclusive and exclusive path fragments.
///
/// Ordinary fragments include paths containing that fragment. Fragments
/// beginning with `!` exclude matching paths. With only exclusions, all other
/// paths are included. With both forms, a path must match an inclusion and no
/// exclusion. An empty list selects every path.
pub fn path_selector_from_only_paths(
  only_paths: List(String),
) -> fn(String) -> Bool {
  let #(excluded_paths, included_paths) =
    list.partition(only_paths, string.starts_with(_, "!"))
  let excluded_paths = list.map(excluded_paths, string.drop_start(_, 1))

  case excluded_paths, included_paths {
    [], [] -> fn(_) { True }
    [], _ -> fn(path) { list.any(included_paths, string.contains(path, _)) }
    _, [] -> fn(path) { !list.any(excluded_paths, string.contains(path, _)) }
    _, _ -> fn(path) {
      list.any(included_paths, string.contains(path, _))
      && !list.any(excluded_paths, string.contains(path, _))
    }
  }
}

/// Assembles every eligible `.wly` file from a file or directory.
///
/// Files and directories with a path component beginning with `#` are ignored.
/// See `assemble_input_lines_with_path_selector` for selective assembly.
pub fn assemble_input_lines(
  dirpath_or_filepath: String,
) -> Result(#(DirTree, List(InputLine)), AssemblyError) {
  assemble_input_lines_with_path_selector(dirpath_or_filepath, fn(_) { True })
}

// ************************************************************
// PART 2
//
// List(InputLine) -> List(Writerly)      // Result
// String -> Writerly                     // Result
// 
// pub fn input_lines_to_writerlys
// pub fn input_lines_to_writerly
// pub fn string_to_writerlys
// pub fn string_to_writerly
// ************************************************************

type InputLines =
  List(InputLine)

type Encounter {
  EncounteredFileEnd
  EncounteredBlankLine(blame: Blame, indent: Int)
  EncounteredNonMod4Indent(blame: Blame, indent: Int, suffix: String)
  EncounteredHigherIndent(
    blame: Blame,
    indent: Int,
    suffix: String,
    original_indent: Int,
    higher_indent: Int,
  )
  EncounteredLowerIndent(blame: Blame, indent: Int, suffix: String)
  EncounteredTextLine(blame: Blame, suffix: String)
  EncounteredTagLine(blame: Blame, suffix: String)
  EncounteredCommentLine(blame: Blame, suffix: String)
  EncounteredCodeFence(blame: Blame, suffix: String)
}

fn nonempty_suffix_encounter(blame: Blame, suffix: String) -> Encounter {
  case suffix {
    "|>" <> _ -> EncounteredTagLine(blame |> bl.set_anchored, suffix)
    "!!" <> _ -> EncounteredCommentLine(blame, suffix)
    "```" <> _ -> EncounteredCodeFence(blame, suffix)
    _ -> EncounteredTextLine(blame, suffix)
  }
}

fn input_lines_encounter(
  indent: Int,
  head: InputLines,
) -> #(Encounter, InputLines) {
  use first, rest <- on.empty_nonempty(head, fn() { #(EncounteredFileEnd, []) })

  let InputLine(blame, first_indent, suffix) = first

  use <- on.true_false(suffix == "", fn() {
    #(EncounteredBlankLine(blame, first_indent), rest)
  })

  use <- on.true_false(first_indent % 4 != 0, fn() {
    #(EncounteredNonMod4Indent(blame, first_indent, suffix), rest)
  })

  use <- on.true_false(first_indent < indent, fn() {
    #(EncounteredLowerIndent(blame, first_indent, suffix), rest)
  })

  use <- on.true_false(first_indent > indent, fn() {
    #(
      EncounteredHigherIndent(blame, first_indent, suffix, indent, first_indent),
      rest,
    )
  })

  let encounter = nonempty_suffix_encounter(blame, suffix)

  #(encounter, rest)
}

fn drop_text_line_escape(
  blame: Blame,
  suffix: String,
  rgxs: OurRegexes,
) -> Line {
  case regexp.check(rgxs.includes_bol_te_escape, suffix) {
    True -> Line(blame |> bl.advance(1), suffix |> string.drop_start(1))
    False -> Line(blame, suffix)
  }
}

fn parse_text_lines_at_indent(
  indent: Int,
  head: InputLines,
  rgxs: OurRegexes,
) -> Result(#(List(Line), Encounter, InputLines), ParseError) {
  let #(encounter, rest) = input_lines_encounter(indent, head)

  case encounter {
    EncounteredTextLine(blame, suffix) -> {
      let line = drop_text_line_escape(blame, suffix, rgxs)
      use #(lines, encounter, rest) <- on.ok(parse_text_lines_at_indent(
        indent,
        rest,
        rgxs,
      ))
      Ok(#([line, ..lines], encounter, rest))
    }
    _ -> Ok(#([], encounter, rest))
  }
}

fn parse_comment_lines_at_indent(
  indent: Int,
  head: InputLines,
) -> Result(#(List(Line), Encounter, InputLines), ParseError) {
  let #(encounter, rest) = input_lines_encounter(indent, head)

  case encounter {
    EncounteredCommentLine(blame, suffix) -> {
      let line = Line(blame |> bl.advance(2), suffix |> string.drop_start(2))
      use #(lines, encounter, rest) <- on.ok(parse_comment_lines_at_indent(
        indent,
        rest,
      ))
      Ok(#([line, ..lines], encounter, rest))
    }
    _ -> Ok(#([], encounter, rest))
  }
}

fn parse_attrs_at_indent(
  indent: Int,
  head: InputLines,
  rgxs: OurRegexes,
) -> Result(#(List(Attr), Encounter, InputLines), ParseError) {
  let #(encounter, rest) = input_lines_encounter(indent, head)

  use #(blame, suffix) <- on.stay(case encounter {
    EncounteredTextLine(blame, suffix) -> on.Stay(#(blame, suffix))

    EncounteredCommentLine(blame, suffix) -> {
      let #(spaces, val) =
        suffix |> string.drop_start(2) |> split_leading_spaces(0)
      case spaces > maximum_leading_attribute_spaces {
        True ->
          on.Return(
            Error(ExcessiveLeadingAttributeSpaces(
              blame,
              maximum_leading_attribute_spaces,
              spaces,
            )),
          )
        False -> {
          let attr = Attr(blame, make_commented_attribute_key(spaces), val)
          use #(attrs, encounter, rest) <- on.error_ok(
            parse_attrs_at_indent(indent, rest, rgxs),
            fn(e) { on.Return(Error(e)) },
          )
          on.Return(Ok(#([attr, ..attrs], encounter, rest)))
        }
      }
    }

    _ -> on.Return(Ok(#([], encounter, rest)))
  })

  assert suffix != ""
  assert !string.starts_with(suffix, "!!")
  assert !string.starts_with(suffix, "|>")

  use #(key, val) <- on.error_ok(suffix |> string.split_once("="), fn(_) {
    Ok(#([], encounter, rest))
  })

  use <- on.true_false(
    key == ""
      || string.contains(key, " ")
      || !regexp.check(rgxs.is_valid_key, key),
    fn() { Ok(#([], encounter, rest)) },
  )

  use val <- on.ok(parse_attribute_value(blame, val))
  let attr = Attr(blame, key, val)
  use #(attrs, encounter, rest) <- on.ok(parse_attrs_at_indent(
    indent,
    rest,
    rgxs,
  ))
  Ok(#([attr, ..attrs], encounter, rest))
}

fn parse_writerlys_at_indent_from_nonempty_suffix(
  indent: Int,
  rest: InputLines,
  rgxs: OurRegexes,
  blame: Blame,
  suffix: String,
) -> Result(
  #(List(Writerly), List(Writerly), Encounter, InputLines),
  ParseError,
) {
  let encounter = nonempty_suffix_encounter(blame, suffix)
  parse_writerlys_at_indent_from_encounter(indent, rest, rgxs, encounter)
}

fn parse_writerlys_at_indent_from_encounter(
  indent: Int,
  rest: InputLines,
  rgxs: OurRegexes,
  encounter: Encounter,
) -> Result(
  #(List(Writerly), List(Writerly), Encounter, InputLines),
  ParseError,
) {
  case encounter {
    EncounteredFileEnd -> {
      Ok(#([], [], EncounteredFileEnd, []))
    }

    EncounteredBlankLine(blame, _) -> {
      let writerly = BlankLine(blame)
      use #(s1, s2, encounter, rest) <- on.ok(parse_writerlys_at_indent(
        indent,
        rest,
        rgxs,
      ))
      let #(s1, s2) = case s1 {
        [] -> #(s1, [writerly, ..s2])
        _ -> #([writerly, ..s1], s2)
      }
      Ok(#(s1, s2, encounter, rest))
    }

    EncounteredNonMod4Indent(blame, _, suffix) -> {
      Error(IndentationNotMultipleOfFour(blame, suffix))
    }

    EncounteredHigherIndent(blame, _, suffix, indent, higher_indent) -> {
      Error(IndentationTooLarge(
        blame,
        "expected: " <> ins(indent),
        "found: " <> ins(higher_indent),
        "line: '" <> suffix <> "'",
      ))
    }

    EncounteredLowerIndent(blame, suffix_indent, suffix) -> {
      assert suffix_indent <= indent
      assert suffix != ""
      case suffix_indent < indent {
        True -> Ok(#([], [], encounter, rest))
        False ->
          parse_writerlys_at_indent_from_nonempty_suffix(
            indent,
            rest,
            rgxs,
            blame,
            suffix,
          )
      }
    }

    EncounteredCommentLine(blame, suffix) -> {
      let line = Line(blame |> bl.advance(2), suffix |> string.drop_start(2))
      use #(lines, encounter, rest) <- on.ok(parse_comment_lines_at_indent(
        indent,
        rest,
      ))
      let writerly = Comment(blame, [line, ..lines])
      use #(s1, s2, encounter, rest) <- on.ok(
        parse_writerlys_at_indent_from_encounter(indent, rest, rgxs, encounter),
      )
      Ok(#([writerly, ..s1], s2, encounter, rest))
    }

    EncounteredTextLine(blame, suffix) -> {
      let line = drop_text_line_escape(blame, suffix, rgxs)
      use #(lines, encounter, rest) <- on.ok(parse_text_lines_at_indent(
        indent,
        rest,
        rgxs,
      ))
      let writerly = Paragraph(blame, [line, ..lines])
      use #(s1, s2, encounter, rest) <- on.ok(
        parse_writerlys_at_indent_from_encounter(indent, rest, rgxs, encounter),
      )
      Ok(#([writerly, ..s1], s2, encounter, rest))
    }

    EncounteredTagLine(blame, suffix) -> {
      let tag = suffix |> string.drop_start(2) |> string.trim
      use <- on.false_true(regexp.check(rgxs.is_valid_tag, tag), fn() {
        Error(BadTag(blame, tag))
      })
      use #(attrs, encounter, rest) <- on.ok(parse_attrs_at_indent(
        indent + 4,
        rest,
        rgxs,
      ))
      use #(s1, s2, encounter, rest) <- on.ok(
        parse_writerlys_at_indent_from_encounter(
          indent + 4,
          rest,
          rgxs,
          encounter,
        ),
      )
      let writerly = Tag(blame, tag, attrs, s1)
      use #(s3, s4, encounter, rest) <- on.ok(
        parse_writerlys_at_indent_from_encounter(indent, rest, rgxs, encounter),
      )
      let #(all_children, blanks) = case s3 {
        [] -> {
          assert s4 == []
          #([writerly], s2)
        }
        _ -> {
          #([writerly, ..list.append(s2, s3)], s4)
        }
      }
      Ok(#(all_children, blanks, encounter, rest))
    }

    EncounteredCodeFence(blame, suffix) -> {
      use attrs <- on.ok(code_block_info_to_attrs(
        blame |> bl.advance(3),
        suffix |> string.drop_start(3),
        rgxs,
      ))
      use #(lines, rest) <- on.ok(parse_code_block_at_indent(
        indent,
        rest,
        blame,
        rgxs,
      ))
      let writerly = CodeBlock(blame, attrs, lines)
      use #(s1, s2, encounter, rest) <- on.ok(parse_writerlys_at_indent(
        indent,
        rest,
        rgxs,
      ))
      Ok(#([writerly, ..s1], s2, encounter, rest))
    }
  }
}

fn parse_code_block_at_indent(
  indent: Int,
  head: InputLines,
  initial_blame: Blame,
  rgxs: OurRegexes,
) -> Result(#(List(Line), InputLines), ParseError) {
  use first, rest <- on.empty_nonempty(head, fn() {
    Error(CodeBlockNotClosed(initial_blame))
  })

  let InputLine(blame, first_indent, suffix) = first

  use <- on.true_false(first_indent > indent, fn() {
    let spaces = string.repeat(" ", first_indent - indent)
    let content = spaces <> suffix
    let line = Line(blame |> bl.advance(indent - first_indent), content)
    use #(lines, rest) <- on.ok(parse_code_block_at_indent(
      indent,
      rest,
      initial_blame,
      rgxs,
    ))
    Ok(#([line, ..lines], rest))
  })

  use <- on.true_false(suffix == "", fn() {
    let line = Line(blame, "")
    use #(lines, rest) <- on.ok(parse_code_block_at_indent(
      indent,
      rest,
      initial_blame,
      rgxs,
    ))
    Ok(#([line, ..lines], rest))
  })

  use <- on.true_false(first_indent < indent, fn() {
    Error(CodeBlockNotClosed(initial_blame))
  })

  use <- on.true_false(suffix |> string.starts_with("```"), fn() {
    let suffix = suffix |> string.drop_start(3) |> string.trim_end()
    case suffix {
      "" -> Ok(#([], rest))
      _ ->
        Error(CodeBlockUnwantedAnnotationAtClose(blame, initial_blame, suffix))
    }
  })

  let #(blame, suffix) = case
    regexp.check(rgxs.includes_bol_cb_escape, suffix)
  {
    True -> #(blame |> bl.advance(1), suffix |> string.drop_start(1))
    False -> #(blame, suffix)
  }

  let line = Line(blame, suffix)
  use #(lines, rest) <- on.ok(parse_code_block_at_indent(
    indent,
    rest,
    initial_blame,
    rgxs,
  ))
  Ok(#([line, ..lines], rest))
}

fn attrs_to_code_block_info(attrs: List(Attr)) -> String {
  let #(info_attrs, attrs) =
    list.partition(attrs, fn(attr) {
      attr.key == code_block_info_string_attribute_key
    })
  let info = case info_attrs {
    [] -> None
    [info] -> Some(info)
    _ -> panic as "multiple WriterlyCodeBlockInfoString attributes"
  }
  let escape = fn(s) {
    s
    |> string.replace("\\", "\\\\")
    |> string.replace("&", "\\&")
  }
  let keyval_maker = fn(attr: Attr) -> String {
    { attr.key <> "=" <> string.trim(attr.val) }
    |> escape
  }
  let keyvals = attrs |> list.map(keyval_maker)
  let info = case info {
    None -> ""
    Some(info) -> info.val |> string.trim |> escape
  }
  [info, ..keyvals] |> string.join("&")
}

fn code_block_info_to_attrs(
  blame: Blame,
  info: String,
  rgxs: OurRegexes,
) -> Result(List(Attr), ParseError) {
  let info = info |> string.trim_end()

  use <- on.true_false(info == "", fn() { Ok([]) })

  use <- on.true_false(info |> string.starts_with(" "), fn() {
    Error(CodeBlockInfoStartsWithSpace(blame, info))
  })

  use <- on.false_true(info |> string.contains("&"), fn() {
    Ok([Attr(blame, code_block_info_string_attribute_key, info)])
  })

  let pieces = regexp.split(rgxs.unescaped_ampersand, info)

  let pieces =
    list.map_fold(pieces, #(blame, ""), fn(acc, p) {
      let #(blame, last_piece) = acc
      let acc = #(blame |> bl.advance(last_piece |> string.length), p)
      #(acc, acc)
    })
    |> pair.second

  let pieces =
    list.index_map(pieces, fn(p, i) {
      let #(blame, p) = p
      let p = case i % 3 {
        0 -> p |> string.replace("\\&", "&") |> string.replace("\\\\", "\\")
        1 -> p |> string.replace("\\\\", "\\")
        2 -> p
        _ -> panic
      }
      #(blame, p)
    })

  let keyvals =
    list.fold(pieces, #(None, 0, []), fn(acc, p) {
      let #(maybe, i, so_far) = acc
      case i % 3 {
        0 -> {
          assert maybe == None
          #(Some(p), i + 1, so_far)
        }
        1 -> {
          let assert Some(#(prev_blame, prev_p)) = maybe
          let #(_, p) = p
          #(None, i + 1, [#(prev_blame, prev_p <> p), ..so_far])
        }
        2 -> {
          assert maybe == None
          #(None, i + 1, so_far)
        }
        _ -> panic
      }
    })

  let keyvals = case keyvals.0 {
    None -> keyvals.2 |> list.reverse
    Some(x) -> [x, ..keyvals.2] |> list.reverse
  }

  let assert [info, ..keyvals] = keyvals
  let info = Attr(info.0, code_block_info_string_attribute_key, info.1)

  use keyvals <- on.ok(
    list.try_map(keyvals, fn(kv) {
      let #(blame, kv) = kv
      let #(key, val) =
        string.split_once(kv, "=")
        |> result.unwrap(#(kv, ""))
      let key = string.trim(key)
      use val <- on.ok(parse_attribute_value(blame, val))
      case regexp.check(rgxs.is_valid_key, key) {
        False -> Error(BadKey(blame, key))
        True -> Ok(Attr(blame, key, val))
      }
    }),
  )

  let attrs = case info.val {
    "" -> keyvals
    _ -> [info, ..keyvals]
  }

  Ok(attrs)
}

fn parse_writerlys_at_indent(
  indent: Int,
  head: InputLines,
  rgxs: OurRegexes,
) -> Result(
  #(List(Writerly), List(Writerly), Encounter, InputLines),
  ParseError,
) {
  let #(encounter, rest) = input_lines_encounter(indent, head)
  parse_writerlys_at_indent_from_encounter(indent, rest, rgxs, encounter)
}

type OurRegexes {
  OurRegexes(
    is_valid_tag: Regexp,
    is_valid_key: Regexp,
    // te = 'text',       'includes_' = (as we parse source)
    includes_bol_te_escape: Regexp,
    // cb = 'code block', 'includes_' = (as we parse source)
    includes_bol_cb_escape: Regexp,
    // te = 'text',       'requires_' = (as we output source)
    requires_bol_te_escape: Regexp,
    // cb = 'code block', 'requires_' = (as we output source)
    requires_bol_cb_escape: Regexp,
    unescaped_ampersand: Regexp,
  )
}

fn our_regexes() -> OurRegexes {
  let assert Ok(is_valid_tag) =
    regexp.from_string("^[a-zA-Z_\\:][-a-zA-Z0-9\\._\\:]*$")
  let assert Ok(is_valid_key) =
    regexp.from_string("^[a-zA-Z_][-a-zA-Z0-9\\._\\:]*$")
  let assert Ok(includes_bol_te_escape) =
    regexp.from_string("^\\\\+(\\s|!!|```)")
  let assert Ok(includes_bol_cb_escape) = regexp.from_string("^\\\\+(```)")
  let assert Ok(requires_bol_te_escape) =
    regexp.from_string("^\\\\*(\\s|!!|```)")
  let assert Ok(requires_bol_cb_escape) = regexp.from_string("^\\\\*(```)")
  let assert Ok(unescaped_ampersand) =
    regexp.from_string("(?<!\\\\)(\\\\\\\\)*(&)")

  OurRegexes(
    is_valid_tag,
    is_valid_key,
    includes_bol_te_escape,
    includes_bol_cb_escape,
    requires_bol_te_escape,
    requires_bol_cb_escape,
    unescaped_ampersand,
  )
}

/// Parses input lines into zero or more top-level Writerly nodes.
pub fn input_lines_to_writerlys(
  lines: InputLines,
) -> Result(List(Writerly), ParseError) {
  let rgxs = our_regexes()
  use #(writerlys, _, _, _) <- on.ok(parse_writerlys_at_indent(0, lines, rgxs))
  Ok(writerlys)
}

fn is_not_blank_line(w: Writerly) -> Bool {
  case w {
    BlankLine(..) -> False
    _ -> True
  }
}

/// Parses input lines containing exactly one non-blank top-level node.
///
/// Top-level blank lines do not count toward cardinality. Returns `MissingRoot`
/// when there is no non-blank root and `NonUniqueRoot` when there is more than
/// one.
pub fn input_lines_to_writerly(
  lines: InputLines,
) -> Result(Writerly, ParseError) {
  use writerlys <- on.ok(input_lines_to_writerlys(lines))
  case list.filter(writerlys, is_not_blank_line) {
    [one] -> Ok(one)
    [] -> Error(MissingRoot(bl.no_blame))
    [one, ..] -> Error(NonUniqueRoot(one.blame))
  }
}

/// Parses a source string into zero or more top-level Writerly nodes.
///
/// `filename` is recorded in the source blame attached to parsed values.
pub fn string_to_writerlys(
  source: String,
  filename: String,
) -> Result(List(Writerly), ParseError) {
  source
  |> io_l.string_to_input_lines(filename, 0)
  |> input_lines_to_writerlys
}

/// Parses a source string containing exactly one non-blank top-level node.
///
/// `filename` is recorded in the source blame attached to parsed values.
pub fn string_to_writerly(
  source: String,
  filename: String,
) -> Result(Writerly, ParseError) {
  source
  |> io_l.string_to_input_lines(filename, 0)
  |> input_lines_to_writerly
}

// ************************************************************
// PART 3
//
// Writerly -> VXML, List(InputLine) -> VXML
//
// pub fn writerly_to_vxml
// pub fn input_lines_to_vxml
// ************************************************************

const writerly_blank_line_vxml_tag = "WriterlyBlankLine"

const writerly_code_block_vxml_tag = "WriterlyCodeBlock"

const writerly_comment_vxml_tag = "WriterlyComment"

/// Converts one Writerly node to its VXML representation.
///
/// Paragraphs become text nodes. Blank lines, comments, and code blocks use
/// the reserved `WriterlyBlankLine`, `WriterlyComment`, and
/// `WriterlyCodeBlock` element names.
pub fn writerly_to_vxml(t: Writerly) -> VXML {
  case t {
    BlankLine(blame) ->
      V(
        blame: blame,
        tag: writerly_blank_line_vxml_tag,
        attrs: [],
        children: [],
      )

    Paragraph(blame, lines) -> T(blame: blame, lines: lines)

    Comment(blame, lines) ->
      V(blame: blame, tag: writerly_comment_vxml_tag, attrs: [], children: [
        T(blame: blame, lines: lines),
      ])

    CodeBlock(blame, attrs, lines) ->
      V(
        blame: blame,
        tag: writerly_code_block_vxml_tag,
        attrs: attrs,
        children: case lines {
          [] -> []
          _ -> [T(blame: blame, lines: lines)]
        },
      )

    Tag(blame, tag, attrs, children) -> {
      V(
        blame: blame,
        tag: tag,
        attrs: attrs,
        children: children |> list.map(writerly_to_vxml),
      )
    }
  }
}

/// Converts Writerly nodes to VXML nodes in the same order.
pub fn writerlys_to_vxmls(writerlys: List(Writerly)) -> List(VXML) {
  writerlys |> list.map(writerly_to_vxml)
}

/// Parses input lines with one non-blank root and converts it to VXML.
pub fn input_lines_to_vxml(lines: InputLines) -> Result(VXML, ParseError) {
  input_lines_to_writerly(lines)
  |> result.map(writerly_to_vxml)
}

// ************************************************************
// PART 4
//
// VXML -> Writerly
//
// pub fn vxml_to_writerlys
// pub fn vxmls_to_writerlys
// pub fn vxml_to_writerly
// ************************************************************

fn is_whitespace(s: String) -> Bool {
  string.trim(s) == ""
}

fn add_escape_in_string(s: String, re: Regexp) -> String {
  case regexp.check(re, s) {
    True -> "\\" <> s
    False -> s
  }
}

fn add_escapes_in_lines(contents: List(Line), re: Regexp) -> List(Line) {
  list.map(contents, fn(line) {
    Line(line.blame, line.content |> add_escape_in_string(re))
  })
}

fn process_vxml_t_node(vxml: VXML) -> List(Writerly) {
  let assert T(_, lines) = vxml
  lines
  |> list.index_map(fn(line, i) { #(i, line) })
  |> list.filter(fn(pair) {
    let #(index, line) = pair
    !is_whitespace(line.content)
    || index == 0
    || index == list.length(lines) - 1
  })
  |> list.map(pair.second)
  |> fn(lines) {
    case lines {
      [] -> []
      [first, ..] -> [Paragraph(first.blame, lines)]
    }
  }
}

fn is_t(vxml: VXML) -> Bool {
  case vxml {
    T(_, _) -> True
    _ -> False
  }
}

/// Converts one VXML node to zero or more Writerly nodes.
///
/// An empty VXML text node produces no Writerly node. Reserved Writerly
/// elements must have the structure produced by `writerly_to_vxml`; malformed
/// reserved elements cause an assertion failure.
pub fn vxml_to_writerlys(vxml: VXML) -> List(Writerly) {
  // This would return Writerly rather than List(Writerly), except that an
  // empty text node produces no Writerly value.
  case vxml {
    V(blame, tag, attrs, children) -> {
      case tag {
        _ if tag == writerly_blank_line_vxml_tag -> {
          assert attrs == []
          assert children == []
          [BlankLine(blame)]
        }
        _ if tag == writerly_code_block_vxml_tag -> {
          assert list.all(children, is_t)
          let lines =
            children
            |> list.flat_map(fn(t) {
              let assert T(_, lines) = t
              lines
            })
          [CodeBlock(blame, attrs, lines)]
        }
        _ if tag == writerly_comment_vxml_tag -> {
          let assert [T(_, lines)] = children
          assert lines != []
          [Comment(blame, lines)]
        }
        _ -> {
          let children = children |> vxmls_to_writerlys
          [Tag(blame, tag, attrs, children)]
        }
      }
    }
    T(_, _) -> {
      vxml |> process_vxml_t_node
    }
  }
}

/// Converts VXML nodes to Writerly nodes, omitting empty text nodes.
pub fn vxmls_to_writerlys(vxmls: List(VXML)) -> List(Writerly) {
  vxmls
  |> list.map(vxml_to_writerlys)
  |> list.flatten
}

/// Converts VXML that corresponds to exactly one Writerly node.
///
/// Returns `Error(Nil)` for an empty text node. Panics if one VXML node expands
/// into multiple Writerly nodes.
pub fn vxml_to_writerly(vxml: VXML) -> Result(Writerly, Nil) {
  case vxml |> vxml_to_writerlys {
    [one] -> Ok(one)
    [] -> Error(Nil)
    _ -> panic as "expecting 0 or 1 writerlys"
  }
}

// ************************************************************
// PART 5-minus (annotating blames)
//
// Writerly -> Writerly
//
// pub fn annotate_blames
// ************************************************************

/// Adds structural descriptions to the blame comments throughout a tree.
///
/// This is intended for diagnostic tables. Source locations and node contents
/// are otherwise unchanged.
pub fn annotate_blames(writerly: Writerly) -> Writerly {
  case writerly {
    BlankLine(blame) -> BlankLine(blame |> pc("BlankLine"))
    Paragraph(blame, lines) ->
      Paragraph(
        blame |> pc("Blurb"),
        list.index_map(lines, fn(line, i) {
          Line(
            line.blame
              |> pc("Blurb > Line(" <> ins(i + 1) <> ")"),
            line.content,
          )
        }),
      )
    Comment(blame, lines) ->
      Comment(
        blame |> pc("Comment"),
        list.index_map(lines, fn(line, i) {
          Line(
            line.blame
              |> pc("Comment > Line(" <> ins(i + 1) <> ")"),
            line.content,
          )
        }),
      )
    CodeBlock(blame, attrs, lines) -> {
      let info = attrs_to_code_block_info(attrs)
      CodeBlock(
        blame |> pc("CodeBlock:" <> info),
        attrs,
        list.index_map(lines, fn(line, i) {
          Line(
            line.blame
              |> pc("CodeBlock > Line(" <> ins(i + 1) <> ")"),
            line.content,
          )
        }),
      )
    }
    Tag(blame, tag, attrs, children) ->
      Tag(
        blame |> pc("Tag"),
        tag,
        list.index_map(attrs, fn(attr, i) {
          Attr(
            attr.blame |> pc("Tag > Attr(" <> ins(i + 1) <> ")"),
            attr.key,
            attr.val,
          )
        }),
        children
          |> list.map(annotate_blames),
      )
  }
}

// ************************************************************
// API PART 5 (emitting to OutputLine & String)
//
// Writerly -> List(OutputLine)
// List(Writerly) -> List(OutputLine)
// Writerly -> String
// List(Writerly) -> String
//
// pub fn writerly_to_output_lines
// pub fn writerlys_to_output_lines
// pub fn writerly_to_string
// pub fn writerlys_to_string
// pub fn writerly_table
// ************************************************************

fn line_to_output_line(line: Line, indentation: Int) -> OutputLine {
  OutputLine(line.blame, indentation, line.content)
}

fn lines_to_output_lines(
  lines: List(Line),
  indentation: Int,
) -> List(OutputLine) {
  lines
  |> list.map(line_to_output_line(_, indentation))
}

fn attr_to_output_line(attr: Attr, indentation: Int) -> OutputLine {
  let content = case commented_attribute_spaces(attr.key) {
    Some(spaces) -> "!!" <> string.repeat(" ", spaces) <> attr.val
    None -> attr.key <> "=" <> attr.val
  }
  OutputLine(attr.blame, indentation, content)
}

fn attrs_to_output_lines(
  attrs: List(Attr),
  indentation: Int,
) -> List(OutputLine) {
  attrs |> list.map(attr_to_output_line(_, indentation))
}

fn first_child_is_blurb_and_first_line_of_blurb_could_be_read_as_attr_value_pair(
  nodes: List(Writerly),
) -> Bool {
  case nodes {
    [Paragraph(_, lines), ..] -> {
      let assert [first, ..] = lines
      case string.split_once(first.content, "=") {
        Error(_) -> False
        Ok(#(before, _)) -> {
          let before = string.trim(before)
          !string.contains(before, " ") && before != ""
        }
      }
    }
    _ -> False
  }
}

fn writerly_to_output_lines_internal(
  t: Writerly,
  indentation: Int,
  annotate_blames: Bool,
  rgxs: OurRegexes,
) -> List(OutputLine) {
  case t {
    BlankLine(blame) -> [OutputLine(blame, 0, "")]

    Paragraph(_, lines) ->
      lines
      |> add_escapes_in_lines(rgxs.requires_bol_te_escape)
      |> lines_to_output_lines(indentation)

    Comment(_, lines) ->
      lines
      |> list.map(fn(l) { Line(..l, content: "!!" <> l.content) })
      |> lines_to_output_lines(indentation)

    CodeBlock(blame, attrs, lines) -> {
      list.flatten([
        [
          OutputLine(
            blame,
            indentation,
            "```" <> attrs_to_code_block_info(attrs),
          ),
        ],
        lines
          |> add_escapes_in_lines(rgxs.requires_bol_cb_escape)
          |> lines_to_output_lines(indentation),
        [
          OutputLine(
            case annotate_blames {
              False -> blame
              True -> blame |> pc("CodeBlock end")
            },
            indentation,
            "```",
          ),
        ],
      ])
    }

    Tag(blame, tag, attrs, children) -> {
      let tag_line = OutputLine(blame, indentation, "|> " <> tag)
      let attr_lines = attrs_to_output_lines(attrs, indentation + 4)
      let children_lines =
        children
        |> list.map(writerly_to_output_lines_internal(
          _,
          indentation + 4,
          annotate_blames,
          rgxs,
        ))
        |> list.flatten
      let buffer_lines = case
        first_child_is_blurb_and_first_line_of_blurb_could_be_read_as_attr_value_pair(
          children,
        )
      {
        True -> {
          let blame = case annotate_blames {
            False -> blame |> bl.clear_comments
            True -> blame |> bl.clear_comments |> pc("(a-b separation line)")
          }
          [OutputLine(blame, 0, "")]
        }
        False -> []
      }
      list.flatten([[tag_line], attr_lines, buffer_lines, children_lines])
    }
  }
}

/// Serializes one Writerly node to VXML `OutputLine` values.
pub fn writerly_to_output_lines(writerly: Writerly) -> List(OutputLine) {
  let rgxs = our_regexes()
  writerly
  |> writerly_to_output_lines_internal(0, False, rgxs)
}

/// Serializes Writerly nodes to one flat list of VXML `OutputLine` values.
pub fn writerlys_to_output_lines(
  writerlys: List(Writerly),
) -> List(OutputLine) {
  writerlys
  |> list.map(writerly_to_output_lines)
  |> list.flatten
}

/// Serializes one Writerly node to Writerly source.
pub fn writerly_to_string(writerly: Writerly) -> String {
  writerly
  |> writerly_to_output_lines()
  |> io_l.output_lines_to_string
}

/// Serializes Writerly nodes to Writerly source in the same order.
pub fn writerlys_to_string(writerlys: List(Writerly)) -> String {
  writerlys
  |> writerlys_to_output_lines()
  |> io_l.output_lines_to_string
}

/// Renders a blame-annotated diagnostic table for one Writerly tree.
///
/// `banner` labels the table and `indent` sets its left margin.
pub fn writerly_table(
  writerly: Writerly,
  banner: String,
  indent: Int,
) -> String {
  let rgxs = our_regexes()
  writerly
  |> annotate_blames
  |> writerly_to_output_lines_internal(0, True, rgxs)
  |> io_l.output_lines_table(banner, indent)
}
