import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile

// ****************
// * public types *
// ****************

pub type Blame {
  Blame(filename: String, line_no: Int, comments: List(String))
}

pub type BlamedContent {
  BlamedContent(blame: Blame, content: String)
}

pub type BlamedAttribute {
  BlamedAttribute(blame: Blame, key: String, value: String)
}

pub type Writerly {
  BlankLine(blame: Blame)
  Blurb(blame: Blame, lines: List(BlamedContent))
  CodeBlock(blame: Blame, annotation: String, lines: List(BlamedContent))
  Tag(
    blame: Blame,
    name: String,
    attributes: List(BlamedAttribute),
    children: List(Writerly),
  )
}

pub type WriterlyParseError {
  WriterlyParseErrorEmptyTag(Blame)
  WriterlyParseErrorIllegalTagCharacter(Blame, String, String)
  WriterlyParseErrorIllegalAttributeKeyCharacter(Blame, String, String)
  WriterlyParseErrorIndentationTooLarge(Blame, String)
  WriterlyParseErrorIndentationNotMultipleOfFour(Blame)
  WriterlyParseErrorCodeBlockClosingUnwantedAnnotation(Blame)
  WriterlyParseErrorCodeBlockClosingMissing(Blame)
}

// ***************
// * local types *
// ***************

type FileRemaining {
  FileRemaining(filename: String, line_no: Int, lines: List(String))
}

type BadTagName {
  EmptyTag
  IllegalTagCharacter(String, String)
}

type TentativeTagName =
  Result(String, BadTagName)

type BadAttributeKey {
  IllegalAttributeKeyCharacter(String, String)
}

type TentativeAttributeKey =
  Result(String, BadAttributeKey)

type TentativeBlamedAttribute {
  TentativeBlamedAttribute(
    blame: Blame,
    key: TentativeAttributeKey,
    value: String,
  )
}

type ClosingBackTicksError {
  UndesiredAnnotation(FileRemaining)
  NoBackticksFound(FileRemaining)
}

type NonemptySuffixDiagnostic {
  Pipe(annotation: String)
  TripleBacktick(annotation: String)
  Other(content: String)
}

type TentativeWriterly {
  TentativeBlankLine(blame: Blame)
  TentativeBlurb(blame: Blame, contents: List(BlamedContent))
  TentativeCodeBlock(
    blame: Blame,
    annotation: String,
    contents: List(BlamedContent),
  )
  TentativeTag(
    blame: Blame,
    tag: TentativeTagName,
    attributes: List(TentativeBlamedAttribute),
    children: List(TentativeWriterly),
  )
  TentativeErrorIndentationTooLarge(blame: Blame, message: String)
  TentativeErrorIndentationNotMultipleOfFour(blame: Blame)
  TentativeErrorCodeBlockAnnotation(blame: Blame, message: String)
  TentativeErrorNoCodeBlockClosing(blame: Blame)
}

const ins = string.inspect

fn tentative_blamed_attribute_to_blamed_attribute(
  t: TentativeBlamedAttribute,
) -> Result(BlamedAttribute, WriterlyParseError) {
  case t.key {
    Ok(key) -> Ok(BlamedAttribute(blame: t.blame, key: key, value: t.value))
    Error(IllegalAttributeKeyCharacter(original_would_be_key, bad_char)) ->
      Error(WriterlyParseErrorIllegalAttributeKeyCharacter(
        t.blame,
        original_would_be_key,
        bad_char,
      ))
  }
}

fn tentative_blamed_attributes_to_blamed_attributes(
  attrs: List(TentativeBlamedAttribute),
) -> Result(List(BlamedAttribute), WriterlyParseError) {
  case attrs {
    [] -> Ok([])
    [first, ..rest] ->
      case tentative_blamed_attribute_to_blamed_attribute(first) {
        Error(error) -> Error(error)
        Ok(blamed_attribute) ->
          case tentative_blamed_attributes_to_blamed_attributes(rest) {
            Ok(blamed_attributes) ->
              Ok(list.prepend(blamed_attributes, blamed_attribute))

            Error(error) -> Error(error)
          }
      }
  }
}

fn parse_from_tentatives(
  tentatives: List(TentativeWriterly),
) -> Result(List(Writerly), WriterlyParseError) {
  case tentatives {
    [] -> Ok([])
    [first, ..rest] ->
      case parse_from_tentative(first) {
        Ok(parsed) ->
          case parse_from_tentatives(rest) {
            Ok(parseds) -> Ok(list.prepend(parseds, parsed))

            Error(error) -> Error(error)
          }

        Error(error) -> Error(error)
      }
  }
}

fn parse_from_tentative(
  tentative: TentativeWriterly,
) -> Result(Writerly, WriterlyParseError) {
  case tentative {
    TentativeErrorCodeBlockAnnotation(blame, _) ->
      Error(WriterlyParseErrorCodeBlockClosingUnwantedAnnotation(blame))

    TentativeErrorIndentationTooLarge(blame, message) ->
      Error(WriterlyParseErrorIndentationTooLarge(blame, message))

    TentativeErrorIndentationNotMultipleOfFour(blame) ->
      Error(WriterlyParseErrorIndentationNotMultipleOfFour(blame))

    TentativeErrorNoCodeBlockClosing(blame) ->
      Error(WriterlyParseErrorCodeBlockClosingMissing(blame))

    TentativeBlankLine(blame) -> Ok(BlankLine(blame))

    TentativeBlurb(blame, contents) -> Ok(Blurb(blame, contents))

    TentativeCodeBlock(blame, annotation, contents) ->
      Ok(CodeBlock(blame, annotation, contents))

    TentativeTag(
      blame,
      tentative_name,
      tentative_attributes,
      tentative_children,
    ) ->
      case tentative_name {
        Error(EmptyTag) -> Error(WriterlyParseErrorEmptyTag(blame))

        Error(IllegalTagCharacter(original_bad_name, bad_char)) ->
          Error(WriterlyParseErrorIllegalTagCharacter(
            tentative.blame,
            original_bad_name,
            bad_char,
          ))

        Ok(name) ->
          case
            tentative_blamed_attributes_to_blamed_attributes(
              tentative_attributes,
            )
          {
            Error(error) -> Error(error)

            Ok(attributes) ->
              case parse_from_tentatives(tentative_children) {
                Error(error) -> Error(error)

                Ok(children) ->
                  Ok(Tag(
                    blame: tentative.blame,
                    name: name,
                    attributes: attributes,
                    children: children,
                  ))
              }
          }
      }
  }
}

fn nonempty_suffix_diagnostic(suffix: String) -> NonemptySuffixDiagnostic {
  let assert False = suffix == ""

  case string.starts_with(suffix, "|>") {
    True -> Pipe(string.drop_left(suffix, 2))

    False ->
      case string.starts_with(suffix, "```") {
        True -> TripleBacktick(string.drop_left(suffix, 3))

        False -> Other(suffix)
      }
  }
}

fn line_indent(line: String) -> Int {
  string.length(line) - string.length(string.trim_left(line))
}

fn increment_line_number(
  in: FileRemaining,
  new_lines: List(String),
) -> FileRemaining {
  let assert True = list.length(new_lines) + 1 == list.length(in.lines)
  FileRemaining(
    filename: in.filename,
    line_no: in.line_no + 1,
    lines: new_lines,
  )
}

fn fast_forward_past_lines_of_indent_at_least(
  indent: Int,
  remaining: FileRemaining,
) -> FileRemaining {
  case remaining.lines {
    [] -> remaining

    [first, ..rest] ->
      case line_indent(first) < indent {
        True -> remaining

        False ->
          fast_forward_past_lines_of_indent_at_least(
            indent,
            increment_line_number(remaining, rest),
          )
      }
  }
}

fn tentative_blamed_attribute(
  blame: Blame,
  pair: #(String, String),
) -> TentativeBlamedAttribute {
  let #(key, value) = pair
  let assert False = string.contains(key, " ")
  let assert False = string.is_empty(key)
  let bad_character = contains_one_of(key, [".", "~"])

  case bad_character == "" {
    True -> TentativeBlamedAttribute(blame: blame, key: Ok(key), value: value)

    False ->
      TentativeBlamedAttribute(
        blame: blame,
        key: Error(IllegalAttributeKeyCharacter(key, bad_character)),
        value: value,
      )
  }
}

fn fast_forward_past_attribute_lines_at_indent(
  indent: Int,
  remaining: FileRemaining,
) -> #(List(TentativeBlamedAttribute), FileRemaining) {
  case remaining.lines {
    [] -> #([], remaining)
    [first, ..rest] -> {
      let suffix = string.trim_left(first)

      case suffix == "" {
        True -> #([], increment_line_number(remaining, rest))

        False -> {
          let first_indent = string.length(first) - string.length(suffix)
          case first_indent != indent {
            True -> #([], remaining)
            False ->
              case
                string.starts_with(suffix, "|>")
                || string.starts_with(suffix, "```")
              {
                True -> #([], remaining)

                False -> {
                  let attribute_pair =
                    suffix
                    |> string.split_once(" ")
                    |> result.unwrap(#(suffix, ""))

                  let #(more_attribute_pairs, remaining_after_attributes) =
                    fast_forward_past_attribute_lines_at_indent(
                      indent,
                      increment_line_number(remaining, rest),
                    )

                  #(
                    list.prepend(
                      more_attribute_pairs,
                      tentative_blamed_attribute(
                        blame_from(remaining),
                        attribute_pair,
                      ),
                    ),
                    remaining_after_attributes,
                  )
                }
              }
          }
        }
      }
    }
  }
}

fn fast_forward_past_other_lines_at_indent(
  indent: Int,
  remaining: FileRemaining,
) -> #(List(String), FileRemaining) {
  case remaining.lines {
    [] -> #([], remaining)
    [first, ..rest] -> {
      let suffix = string.trim_left(first)

      case suffix == "" {
        True -> #([], remaining)
        False -> {
          let first_indent = string.length(first) - string.length(suffix)

          case first_indent != indent {
            True -> #([], remaining)

            False ->
              case
                string.starts_with(suffix, "|>")
                || string.starts_with(suffix, "```")
              {
                True -> #([], remaining)

                False -> {
                  let #(more_others, remaining_after_others) =
                    fast_forward_past_other_lines_at_indent(
                      indent,
                      increment_line_number(remaining, rest),
                    )

                  #(list.prepend(more_others, suffix), remaining_after_others)
                }
              }
          }
        }
      }
    }
  }
}

fn fast_forward_to_closing_backticks(
  indent: Int,
  remaining: FileRemaining,
) -> Result(#(List(String), FileRemaining), ClosingBackTicksError) {
  case remaining.lines {
    [] -> Error(NoBackticksFound(remaining))
    [first, ..rest] -> {
      let suffix = string.trim_left(first)

      case suffix == "" {
        True ->
          case
            fast_forward_to_closing_backticks(
              indent,
              increment_line_number(remaining, rest),
            )
          {
            Ok(#(lines, remaining_after_closing_backticks)) -> {
              let added_line =
                string.repeat(" ", int.max(0, string.length(first) - indent))

              Ok(#(
                list.prepend(lines, added_line),
                remaining_after_closing_backticks,
              ))
            }

            error -> error
          }

        False -> {
          let first_indent = string.length(first) - string.length(suffix)

          case first_indent < indent {
            True -> Error(NoBackticksFound(remaining))

            False -> {
              let padded_suffix_length = string.length(first) - indent
              let assert True = padded_suffix_length >= string.length(suffix)
              let padded_suffix =
                string.pad_left(suffix, to: padded_suffix_length, with: " ")

              case first_indent > indent || !string.starts_with(suffix, "```") {
                True ->
                  case
                    fast_forward_to_closing_backticks(
                      indent,
                      increment_line_number(remaining, rest),
                    )
                  {
                    Ok(#(lines, remaining_after_closing_backticks)) ->
                      Ok(#(
                        list.prepend(lines, padded_suffix),
                        remaining_after_closing_backticks,
                      ))

                    error -> error
                  }

                False -> {
                  let assert True = string.starts_with(suffix, "```")
                  let assert True = first_indent == indent
                  let annotation = string.drop_left(suffix, 3) |> string.trim

                  case string.is_empty(annotation) {
                    True -> Ok(#([], increment_line_number(remaining, rest)))
                    False ->
                      Error(
                        UndesiredAnnotation(increment_line_number(
                          remaining,
                          rest,
                        )),
                      )
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

fn contains_one_of(thing: String, substrings: List(String)) -> String {
  case substrings {
    [] -> ""
    [first, ..rest] -> {
      case string.contains(thing, first) {
        True -> first
        False -> contains_one_of(thing, rest)
      }
    }
  }
}

fn check_good_tag_name(proposed_name) -> TentativeTagName {
  case string.is_empty(proposed_name) {
    True -> Error(EmptyTag)
    False -> {
      let something_illegal = contains_one_of(proposed_name, ["-", ".", " "])
      case string.is_empty(something_illegal) {
        True -> Ok(proposed_name)
        False -> Error(IllegalTagCharacter(proposed_name, something_illegal))
      }
    }
  }
}

fn blame_from(remaining: FileRemaining) -> Blame {
  Blame(filename: remaining.filename, line_no: remaining.line_no, comments: [])
}

fn increment_positive_line_no(blame: Blame) -> Blame {
  case blame.line_no > 0 {
    True ->
      Blame(
        filename: blame.filename,
        line_no: blame.line_no + 1,
        comments: blame.comments,
      )
    False ->
      Blame(
        filename: blame.filename,
        line_no: blame.line_no,
        comments: blame.comments,
      )
  }
}

fn contents_to_blamed_contents(
  blame: Blame,
  contents: List(String),
) -> List(BlamedContent) {
  case contents {
    [] -> []
    [first, ..rest] -> {
      let next_blame = increment_positive_line_no(blame)
      list.prepend(
        contents_to_blamed_contents(next_blame, rest),
        BlamedContent(blame: blame, content: first),
      )
    }
  }
}

fn tentative_parse(
  indent: Int,
  remaining: FileRemaining,
) -> #(List(TentativeWriterly), FileRemaining) {
  case remaining.lines {
    [] -> #([], remaining)

    [line, ..rest] -> {
      let suffix = string.trim_left(line)

      case suffix == "" {
        True -> {
          let #(siblings, remainder_after_indent) =
            tentative_parse(indent, increment_line_number(remaining, rest))

          let tentative_blank_line = TentativeBlankLine(blame_from(remaining))

          #(
            list.prepend(siblings, tentative_blank_line),
            remainder_after_indent,
          )
        }

        False -> {
          let line_indent = string.length(line) - string.length(suffix)

          case line_indent < indent {
            True -> #([], remaining)

            False ->
              case line_indent > indent {
                True -> {
                  let remaining_after_oversize_indent =
                    fast_forward_past_lines_of_indent_at_least(
                      line_indent,
                      remaining,
                    )

                  let #(siblings, remaining_after_indent) =
                    tentative_parse(indent, remaining_after_oversize_indent)

                  case line_indent % 4 == 0 {
                    True -> {
                      let error_message =
                        "indent too large "
                        <> ins(line_indent)
                        <> " > "
                        <> ins(indent)

                      #(
                        list.prepend(
                          siblings,
                          TentativeErrorIndentationTooLarge(
                            blame_from(remaining),
                            error_message,
                          ),
                        ),
                        remaining_after_indent,
                      )
                    }

                    False -> #(
                      list.prepend(
                        siblings,
                        TentativeErrorIndentationNotMultipleOfFour(blame_from(
                          remaining,
                        )),
                      ),
                      remaining_after_indent,
                    )
                  }
                }

                False -> {
                  let assert True = line_indent == indent

                  case nonempty_suffix_diagnostic(suffix) {
                    Pipe(annotation) -> {
                      let #(tentative_attributes, remaining_after_attributes) =
                        fast_forward_past_attribute_lines_at_indent(
                          indent + 4,
                          increment_line_number(remaining, rest),
                        )

                      let #(children, remaining_after_children) =
                        tentative_parse(indent + 4, remaining_after_attributes)

                      let tentative_tag =
                        TentativeTag(
                          blame: blame_from(remaining),
                          tag: check_good_tag_name(string.trim(annotation)),
                          attributes: tentative_attributes,
                          children: children,
                        )

                      let #(siblings, remaining_after_indent) =
                        tentative_parse(indent, remaining_after_children)

                      #(
                        list.prepend(siblings, tentative_tag),
                        remaining_after_indent,
                      )
                    }

                    TripleBacktick(annotation) ->
                      case
                        fast_forward_to_closing_backticks(
                          indent,
                          increment_line_number(remaining, rest),
                        )
                      {
                        Ok(#(lines, remaining_after_code_block)) -> {
                          let blame = blame_from(remaining)

                          let tentative_code_block =
                            TentativeCodeBlock(
                              blame: blame,
                              annotation: annotation,
                              contents: contents_to_blamed_contents(
                                blame,
                                lines,
                              ),
                            )

                          let #(siblings, remaining_after_indent) =
                            tentative_parse(indent, remaining_after_code_block)

                          #(
                            list.prepend(siblings, tentative_code_block),
                            remaining_after_indent,
                          )
                        }

                        Error(UndesiredAnnotation(remaining_after_error)) -> {
                          let error_message =
                            "closing backticks for backticks opened at L"
                            <> ins(remaining.line_no)
                            <> " carry unexpected annotation"

                          let tentative_error =
                            TentativeErrorCodeBlockAnnotation(
                              blame: blame_from(remaining),
                              message: error_message,
                            )

                          let #(siblings, remaining_after_indent) =
                            tentative_parse(indent, remaining_after_error)

                          #(
                            list.prepend(siblings, tentative_error),
                            remaining_after_indent,
                          )
                        }

                        Error(NoBackticksFound(remaining_after_indent)) -> {
                          let tentative_error =
                            TentativeErrorNoCodeBlockClosing(blame: blame_from(
                              remaining,
                            ))

                          #([tentative_error], remaining_after_indent)
                        }
                      }

                    Other(_) -> {
                      let #(more_others, remaining_after_others) =
                        fast_forward_past_other_lines_at_indent(
                          indent,
                          increment_line_number(remaining, rest),
                        )

                      let padded_suffix_length = string.length(line) - indent
                      let assert True =
                        padded_suffix_length >= string.length(suffix)
                      let padded_suffix =
                        string.pad_left(
                          suffix,
                          to: padded_suffix_length,
                          with: " ",
                        )

                      let blame = blame_from(remaining)
                      let contents =
                        list.prepend(more_others, padded_suffix)
                        |> contents_to_blamed_contents(blame, _)
                      let tentative_blurb =
                        TentativeBlurb(blame: blame, contents: contents)

                      let #(siblings, remaining_after_indent) =
                        tentative_parse(indent, remaining_after_others)

                      #(
                        list.prepend(siblings, tentative_blurb),
                        remaining_after_indent,
                      )
                    }
                  }
                }
              }
          }
        }
      }
    }
  }
}

fn tentative_parse_at_indent_0(
  remaining: FileRemaining,
) -> List(TentativeWriterly) {
  let #(parsed, remaining_beyond_parsed) = tentative_parse(0, remaining)
  let assert True = list.is_empty(remaining_beyond_parsed.lines)
  parsed
}

const margin_line_number_pad_to = 6

const margin_announce_pad_to = 30

fn margin_assembler(
  prefix: String,
  blame: Blame,
  announce: String,
  margin: String,
) -> String {
  prefix
  <> blame.filename
  <> ":"
  <> string.pad_right(ins(blame.line_no), margin_line_number_pad_to, " ")
  <> " "
  <> string.pad_right(announce, margin_announce_pad_to, " ")
  <> "."
  <> margin
}

fn margin_suppress_blame_assembler(
  prefix: String,
  blame: Blame,
  announce: String,
  margin: String,
) -> String {
  let would_be_blame =
    blame.filename
    <> ":"
    <> string.pad_right(ins(blame.line_no), margin_line_number_pad_to, " ")

  prefix
  <> string.repeat(" ", times: string.length(would_be_blame))
  <> " "
  <> string.pad_right(announce, margin_announce_pad_to, " ")
  <> "."
  <> margin
}

fn margin_error_assembler(
  prefix: String,
  blame: Blame,
  error_message: String,
) -> String {
  prefix
  <> blame.filename
  <> ":"
  <> string.pad_right(ins(blame.line_no), margin_line_number_pad_to, " ")
  <> " "
  <> error_message
}

fn map_with_special_first(
  z: List(a),
  fn1: fn(a) -> b,
  fn2: fn(a) -> b,
) -> List(b) {
  case z {
    [] -> []
    [first, ..rest] -> fn1(first) |> list.prepend(list.map(rest, fn2), _)
  }
}

fn pretty_print_tentative(
  margin_prefix: String,
  margin: String,
  t: TentativeWriterly,
) {
  let p = margin_prefix
  let m = margin

  case t {
    TentativeBlankLine(blame) ->
      io.println(margin_assembler(p, blame, "BLANK", m))

    TentativeBlurb(_, blamed_contents) -> {
      map_with_special_first(
        blamed_contents,
        fn(first) {
          io.println(
            margin_assembler(p, first.blame, "BLURB_ROOT", m) <> first.content,
          )
        },
        fn(after_first) {
          io.println(
            margin_assembler(p, after_first.blame, "BLURB", m)
            <> after_first.content,
          )
        },
      )

      Nil
    }

    TentativeCodeBlock(blame, annotation, blamed_contents) -> {
      io.println(
        margin_assembler(p, blame, "CODE_OPENING", m) <> "```" <> annotation,
      )

      list.map(blamed_contents, fn(blamed_content) {
        io.println(
          margin_assembler(p, blamed_content.blame, "CODE_BODY", m)
          <> blamed_content.content,
        )
      })

      io.println(
        margin_assembler(p, blame, "CODE_CLOSING", m) <> "```" <> annotation,
      )
    }

    TentativeTag(blame, tag_name, tentative_blamed_attributes, children) -> {
      io.println(
        margin_assembler(p, blame, "TAG", m) <> "|>" <> " " <> ins(tag_name),
      )

      list.map(tentative_blamed_attributes, fn(t) -> Nil {
        io.println(
          margin_assembler(p, t.blame, "ATTRIBUTE", m <> "  ")
          <> ins(t.key)
          <> " "
          <> t.value,
        )
      })

      case list.length(children) > 0 {
        True ->
          io.println(margin_suppress_blame_assembler(
            p,
            blame,
            "(pretty printer inserted)",
            m,
          ))
        False -> Nil
      }

      pretty_print_tentatives(p, m <> "  ", children)
    }

    TentativeErrorIndentationTooLarge(blame, message) ->
      io.println(margin_error_assembler(
        p,
        blame,
        "INDENTATION ERROR (LARGE): " <> message,
      ))

    TentativeErrorIndentationNotMultipleOfFour(blame) ->
      io.println(margin_error_assembler(
        p,
        blame,
        "INDENTATION ERROR (!MULT 4): ",
      ))

    TentativeErrorNoCodeBlockClosing(blame) ->
      io.println(margin_error_assembler(
        p,
        blame,
        "CLOSING BACKTICKS NOT FOUND ERROR",
      ))

    TentativeErrorCodeBlockAnnotation(blame, message) ->
      io.println(margin_error_assembler(
        p,
        blame,
        "CLOSING BACKTICKS UNWANTED ANNOTATION ERROR: " <> message,
      ))
  }
}

fn pretty_print_tentatives(
  margin_prefix: String,
  margin: String,
  tentatives: List(TentativeWriterly),
) {
  case tentatives {
    [] -> Nil
    [first, ..rest] -> {
      pretty_print_tentative(margin_prefix, margin, first)
      pretty_print_tentatives(margin_prefix, margin, rest)
    }
  }
}

pub fn pretty_print_writerly(margin_prefix: String, margin: String, t: Writerly) {
  let p = margin_prefix
  let m = margin

  case t {
    BlankLine(blame) -> io.println(margin_assembler(p, blame, "BLANK", m))

    Blurb(_, blamed_contents) -> {
      map_with_special_first(
        blamed_contents,
        fn(first) {
          io.println(
            margin_assembler(p, first.blame, "BLURB_ROOT", m) <> first.content,
          )
        },
        fn(after_first) {
          io.println(
            margin_assembler(p, after_first.blame, "BLURB", m)
            <> after_first.content,
          )
        },
      )

      Nil
    }

    CodeBlock(blame, annotation, blamed_contents) -> {
      io.println(
        margin_assembler(p, blame, "CODE_OPENING", m) <> "```" <> annotation,
      )

      list.map(blamed_contents, fn(blamed_content) {
        io.println(
          margin_assembler(p, blamed_content.blame, "CODE_BODY", m)
          <> blamed_content.content,
        )
      })

      io.println(
        margin_assembler(p, blame, "CODE_CLOSING", m) <> "```" <> annotation,
      )
    }

    Tag(blame, tag_name, tentative_blamed_attributes, children) -> {
      io.println(
        margin_assembler(p, blame, "TAG", m) <> "|>" <> " " <> ins(tag_name),
      )

      list.map(tentative_blamed_attributes, fn(t) -> Nil {
        io.println(
          margin_assembler(p, t.blame, "ATTRIBUTE", m <> "  ")
          <> ins(t.key)
          <> " "
          <> t.value,
        )
      })

      case list.length(children) > 0 {
        True ->
          io.println(margin_suppress_blame_assembler(
            p,
            blame,
            "(pretty printer inserted)",
            m,
          ))
        False -> Nil
      }

      pretty_print_writerlys(p, m <> "  ", children)
    }
  }
}

pub fn pretty_print_writerlys(
  margin_prefix: String,
  margin: String,
  writerlys: List(Writerly),
) {
  case writerlys {
    [] -> Nil
    [first, ..rest] -> {
      pretty_print_writerly(margin_prefix, margin, first)
      pretty_print_writerlys(margin_prefix, margin, rest)
    }
  }
}

fn tentative_parse_string(
  source: String,
  source_name: String,
  starting_line_number: Int,
) -> List(TentativeWriterly) {
  let lines = string.split(source, "\n")
  let remaining = FileRemaining(source_name, starting_line_number, lines)
  let parsed = tentative_parse_at_indent_0(remaining)
  // io.println("\n\n(tentative parse:)")
  // pretty_print_tentatives("(tentative)", "", parsed)
  // io.println("(tentative end)\n\n")
  parsed
}

pub fn parse_string(
  source: String,
  source_name: String,
  starting_line_number: Int,
) -> Result(List(Writerly), WriterlyParseError) {
  tentative_parse_string(source, source_name, starting_line_number)
  |> parse_from_tentatives
}

pub fn main() {
  let filename = "sample.emu"

  case simplifile.read(filename) {
    Error(e) -> io.println("Error reading " <> filename <> ": " <> ins(e))

    Ok(file) -> {
      case parse_string(file, filename, 1) {
        Ok(writerlys) -> pretty_print_writerlys("(writerlys)", "", writerlys)

        Error(error) -> {
          io.println("\nthere was a parsing error:")
          io.println(ins(error))
        }
      }

      Nil
    }
  }
}
