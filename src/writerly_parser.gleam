import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import gleam/string
import simplifile.{type FileError}

// ****************
// * public types *
// ****************

import vxml_parser.{
  type Blame, type BlamedAttribute, type BlamedContent, type BlamedLine,
  type VXML, Blame, BlamedAttribute, BlamedContent, BlamedLine, T, V,
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
  WriterlyParseErrorIndentationNotMultipleOfFour(Blame, String)
  WriterlyParseErrorCodeBlockClosingUnwantedAnnotation(Blame)
  WriterlyParseErrorCodeBlockClosingMissing(Blame)
}

// ***************
// * local types *
// ***************

type FileHead =
  List(BlamedLine)

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
  UndesiredAnnotation(Int, FileHead)
  NoBackticksFound(FileHead)
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
  TentativeErrorIndentationNotMultipleOfFour(blame: Blame, message: String)
  TentativeErrorCodeBlockAnnotation(blame: Blame, message: String)
  TentativeErrorNoCodeBlockClosing(blame: Blame)
}

// *************
// * constants *
// *************

const ins = string.inspect

// ************
// * FileHead *
// ************

fn current_line(head: FileHead) -> Option(BlamedLine) {
  case head {
    [] -> None
    [first, ..] -> Some(first)
  }
}

fn move_forward(head: FileHead) -> FileHead {
  let assert [_, ..rest] = head
  rest
}

// ************************
// * parse_from_tentative *
// ************************

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

    TentativeErrorIndentationNotMultipleOfFour(blame, message) ->
      Error(WriterlyParseErrorIndentationNotMultipleOfFour(blame, message))

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

fn fast_forward_past_lines_of_indent_at_least(
  indent: Int,
  head: FileHead,
) -> FileHead {
  case current_line(head) {
    None -> head

    Some(BlamedLine(_, suffix_indent, _)) ->
      case suffix_indent < indent {
        True -> head

        False ->
          fast_forward_past_lines_of_indent_at_least(indent, move_forward(head))
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
  head: FileHead,
) -> #(List(TentativeBlamedAttribute), FileHead) {
  case current_line(head) {
    None -> #([], head)

    Some(BlamedLine(blame, suffix_indent, suffix)) -> {
      case suffix == "" {
        True -> #([], move_forward(head))

        False -> {
          case suffix_indent != indent {
            True -> #([], head)

            False ->
              case
                string.starts_with(suffix, "|>")
                || string.starts_with(suffix, "```")
              {
                True -> #([], head)

                False -> {
                  let attribute_pair =
                    suffix
                    |> string.split_once(" ")
                    |> result.unwrap(#(suffix, ""))
                    |> tentative_blamed_attribute(blame, _)

                  let #(more_attribute_pairs, head_after_attributes) =
                    fast_forward_past_attribute_lines_at_indent(
                      indent,
                      move_forward(head),
                    )

                  #(
                    list.prepend(more_attribute_pairs, attribute_pair),
                    head_after_attributes,
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
  head: FileHead,
) -> #(List(BlamedContent), FileHead) {
  case current_line(head) {
    None -> #([], head)

    Some(BlamedLine(blame, suffix_indent, suffix)) -> {
      case suffix == "" {
        True -> #([], head)

        False -> {
          case suffix_indent != indent {
            True -> #([], head)

            False ->
              case
                string.starts_with(suffix, "|>")
                || string.starts_with(suffix, "```")
              {
                True -> #([], head)

                False -> {
                  let blamed_content = BlamedContent(blame, suffix)

                  let #(more_blamed_contents, head_after_others) =
                    fast_forward_past_other_lines_at_indent(
                      indent,
                      move_forward(head),
                    )

                  #(
                    list.prepend(more_blamed_contents, blamed_content),
                    head_after_others,
                  )
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
  head: FileHead,
) -> Result(#(List(BlamedContent), FileHead), ClosingBackTicksError) {
  case current_line(head) {
    None -> Error(NoBackticksFound(head))

    Some(BlamedLine(blame, suffix_indent, suffix)) -> {
      case suffix == "" {
        True ->
          case fast_forward_to_closing_backticks(indent, move_forward(head)) {
            Ok(#(blamed_contents, head_after_closing_backticks)) -> {
              let blamed_content =
                BlamedContent(
                  blame,
                  string.repeat(" ", int.max(0, suffix_indent - indent)),
                )

              Ok(#(
                list.prepend(blamed_contents, blamed_content),
                head_after_closing_backticks,
              ))
            }

            error -> error
          }

        False -> {
          case suffix_indent < indent {
            True -> Error(NoBackticksFound(head))

            False -> {
              let padded_suffix_length =
                suffix_indent + string.length(suffix) - indent
              let assert True = padded_suffix_length >= string.length(suffix)
              let padded_suffix =
                string.pad_left(suffix, to: padded_suffix_length, with: " ")
              let blamed_content = BlamedContent(blame, padded_suffix)

              case
                suffix_indent > indent || !string.starts_with(suffix, "```")
              {
                True ->
                  case
                    fast_forward_to_closing_backticks(
                      indent,
                      move_forward(head),
                    )
                  {
                    Ok(#(blamed_contents, head_after_closing_backticks)) ->
                      Ok(#(
                        list.prepend(blamed_contents, blamed_content),
                        head_after_closing_backticks,
                      ))

                    error -> error
                  }

                False -> {
                  let assert True = string.starts_with(suffix, "```")
                  let assert True = suffix_indent == indent
                  let annotation = string.drop_left(suffix, 3) |> string.trim

                  case string.is_empty(annotation) {
                    True -> Ok(#([], move_forward(head)))

                    False ->
                      Error(UndesiredAnnotation(
                        blame.line_no,
                        move_forward(head),
                      ))
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

fn tentative_parse_at_indent(
  indent: Int,
  head: FileHead,
) -> #(List(TentativeWriterly), FileHead) {
  case current_line(head) {
    None -> #([], head)

    Some(BlamedLine(blame, suffix_indent, suffix)) -> {
      case suffix == "" {
        True -> {
          let #(siblings, remainder_after_indent) =
            tentative_parse_at_indent(indent, move_forward(head))

          let tentative_blank_line = TentativeBlankLine(blame)

          #(
            list.prepend(siblings, tentative_blank_line),
            remainder_after_indent,
          )
        }

        False -> {
          case suffix_indent < indent {
            True -> {
              case suffix_indent > indent - 4 {
                True -> {
                  let error_message =
                    ins(suffix_indent) <> " spaces before " <> ins(suffix)

                  let error =
                    TentativeErrorIndentationNotMultipleOfFour(
                      blame,
                      error_message,
                    )

                  let #(siblings, head_after_indent) =
                    tentative_parse_at_indent(indent, move_forward(head))

                  #(list.prepend(siblings, error), head_after_indent)
                }

                False -> #([], head)
              }
            }

            False ->
              case suffix_indent > indent {
                True -> {
                  let head_after_oversize_indent =
                    fast_forward_past_lines_of_indent_at_least(
                      suffix_indent,
                      head,
                    )

                  let #(siblings, head_after_indent) =
                    tentative_parse_at_indent(
                      indent,
                      head_after_oversize_indent,
                    )

                  case suffix_indent % 4 == 0 {
                    True -> {
                      let error_message =
                        "indent too large "
                        <> ins(suffix_indent)
                        <> " > "
                        <> ins(indent)

                      let error =
                        TentativeErrorIndentationTooLarge(blame, error_message)

                      #(list.prepend(siblings, error), head_after_indent)
                    }

                    False -> {
                      let error_message =
                        ins(suffix_indent) <> " spaces before " <> ins(suffix)

                      let error =
                        TentativeErrorIndentationNotMultipleOfFour(
                          blame,
                          error_message,
                        )
                      #(list.prepend(siblings, error), head_after_indent)
                    }
                  }
                }

                False -> {
                  let assert True = suffix_indent == indent

                  case nonempty_suffix_diagnostic(suffix) {
                    Pipe(annotation) -> {
                      let #(tentative_attributes, head_after_attributes) =
                        fast_forward_past_attribute_lines_at_indent(
                          indent + 4,
                          move_forward(head),
                        )

                      let #(children, head_after_children) =
                        tentative_parse_at_indent(
                          indent + 4,
                          head_after_attributes,
                        )

                      let tentative_tag =
                        TentativeTag(
                          blame: blame,
                          tag: check_good_tag_name(string.trim(annotation)),
                          attributes: tentative_attributes,
                          children: children,
                        )

                      let #(siblings, head_after_indent) =
                        tentative_parse_at_indent(indent, head_after_children)

                      #(
                        list.prepend(siblings, tentative_tag),
                        head_after_indent,
                      )
                    }

                    TripleBacktick(annotation) ->
                      case
                        fast_forward_to_closing_backticks(
                          indent,
                          move_forward(head),
                        )
                      {
                        Ok(#(contents, head_after_code_block)) -> {
                          let blame = blame

                          let tentative_code_block =
                            TentativeCodeBlock(
                              blame: blame,
                              annotation: annotation,
                              contents: contents,
                            )

                          let #(siblings, head_after_indent) =
                            tentative_parse_at_indent(
                              indent,
                              head_after_code_block,
                            )

                          #(
                            list.prepend(siblings, tentative_code_block),
                            head_after_indent,
                          )
                        }

                        Error(UndesiredAnnotation(
                          error_line_number,
                          head_after_error,
                        )) -> {
                          let error_message =
                            "closing backticks on L"
                            <> ins(error_line_number)
                            <> " for backticks opened at L"
                            <> ins(blame.line_no)
                            <> " carry unexpected annotation"

                          let tentative_error =
                            TentativeErrorCodeBlockAnnotation(
                              blame,
                              error_message,
                            )

                          let #(siblings, head_after_indent) =
                            tentative_parse_at_indent(indent, head_after_error)

                          #(
                            list.prepend(siblings, tentative_error),
                            head_after_indent,
                          )
                        }

                        Error(NoBackticksFound(head_after_indent)) -> {
                          let tentative_error =
                            TentativeErrorNoCodeBlockClosing(blame)

                          #([tentative_error], head_after_indent)
                        }
                      }

                    Other(_) -> {
                      let blame = blame
                      let blamed_content = BlamedContent(blame, suffix)

                      let #(more_blamed_contents, head_after_others) =
                        fast_forward_past_other_lines_at_indent(
                          indent,
                          move_forward(head),
                        )

                      let tentative_blurb =
                        TentativeBlurb(
                          blame: blame,
                          contents: list.prepend(
                            more_blamed_contents,
                            blamed_content,
                          ),
                        )

                      let #(siblings, head_after_indent) =
                        tentative_parse_at_indent(indent, head_after_others)

                      #(
                        list.prepend(siblings, tentative_blurb),
                        head_after_indent,
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

//************************
//* blamed line building *
//************************

fn add_blames_map_fold(
  current_info: #(Int, String),
  // line_number, filename
  current_line: #(Int, String),
  // indent, suffix
) -> #(#(Int, String), BlamedLine) {
  let #(line_number, filename) = current_info
  let #(indent, suffix) = current_line
  #(
    #(line_number + 1, filename),
    BlamedLine(Blame(filename, line_number, []), indent, suffix),
  )
}

fn add_blames(
  pairs: List(#(Int, String)),
  proto_blame: #(Int, String),
) -> List(BlamedLine) {
  list.map_fold(pairs, proto_blame, add_blames_map_fold)
  |> pair.second
}

fn line_to_indent_suffix_pair(line: String, extra_indent: Int) -> #(Int, String) {
  let suffix = string.trim_left(line)
  let indent = string.length(line) - string.length(suffix)
  #(indent + extra_indent, suffix)
}

fn string_to_blamed_lines(
  extra_indent: Int,
  source: String,
  filename: String,
  starting_line_number: Int,
) -> List(BlamedLine) {
  string.split(source, "\n")
  |> list.map(line_to_indent_suffix_pair(_, extra_indent))
  |> add_blames(#(starting_line_number, filename))
}

//****************************************
//* tentative parsing api (blamed lines) *
//****************************************

fn tentative_parse_blamed_lines(
  head: FileHead,
  debug_messages: Bool,
) -> List(TentativeWriterly) {
  let #(parsed, final_head) = tentative_parse_at_indent(0, head)
  let assert True = list.is_empty(final_head)

  case debug_messages {
    True -> {
      io.println("\n\n(tentative parse:)")
      debug_print_tentatives_internal("(tentative)", "", parsed)
      io.println("(tentative end)\n\n")
    }
    False -> Nil
  }

  parsed
}

//**********************************
//* tentative parsing api (string) *
//**********************************

fn tentative_parse_string(
  source: String,
  filename: String,
  debug_messages: Bool,
) -> List(TentativeWriterly) {
  string_to_blamed_lines(0, source, filename, 1)
  |> tentative_parse_blamed_lines(debug_messages)
}

//***************************************
//* writerly parsing api (blamed lines) *
//***************************************

pub fn parse_blamed_lines(
  lines: List(BlamedLine),
  debug_messages: Bool,
) -> Result(List(Writerly), WriterlyParseError) {
  lines
  |> tentative_parse_blamed_lines(debug_messages)
  |> parse_from_tentatives
}

//*********************************
//* writerly parsing api (string) *
//*********************************

pub fn parse_string(
  source: String,
  filename: String,
  debug_messages: Bool,
) -> Result(List(Writerly), WriterlyParseError) {
  tentative_parse_string(source, filename, debug_messages)
  |> parse_from_tentatives
}

//************
//* printing *
//************

const pre_announce_pad_to = 60

const margin_announce_pad_to = 30

const debug_print_spaces = "    "

fn margin_assembler(
  pre_blame: String,
  blame: Blame,
  announce: String,
  margin: String,
) -> String {
  let up_to_line_number =
    pre_blame <> blame.filename <> ":" <> ins(blame.line_no)

  string.pad_right(up_to_line_number, pre_announce_pad_to, " ")
  <> string.pad_right(announce, margin_announce_pad_to, " ")
  <> "###"
  <> margin
}

fn margin_suppress_blame_assembler(
  pre_blame: String,
  _: Blame,
  _: String,
) -> String {
  string.pad_right(pre_blame, pre_announce_pad_to, " ")
  <> string.pad_right("", margin_announce_pad_to, " ")
  <> "###"
}

fn margin_error_assembler(
  pre_blame: String,
  blame: Blame,
  error_message: String,
) -> String {
  let up_to_line_number =
    pre_blame <> blame.filename <> ":" <> ins(blame.line_no)

  string.pad_right(up_to_line_number, pre_announce_pad_to, " ") <> error_message
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

//**********************
//* printing Tentative *
//**********************

fn debug_print_tentative_internal(
  pre_blame: String,
  indentation: String,
  t: TentativeWriterly,
) {
  case t {
    TentativeBlankLine(blame) ->
      margin_assembler(pre_blame, blame, "BLANK", indentation)
      |> io.println

    TentativeBlurb(_, blamed_contents) -> {
      map_with_special_first(
        blamed_contents,
        fn(first) {
          {
            margin_assembler(pre_blame, first.blame, "BLURB_ROOT", indentation)
            <> first.content
          }
          |> io.println
        },
        fn(after_first) {
          {
            margin_assembler(pre_blame, after_first.blame, "BLURB", indentation)
            <> after_first.content
          }
          |> io.println
        },
      )

      Nil
    }

    TentativeCodeBlock(blame, annotation, blamed_contents) -> {
      io.println(
        margin_assembler(pre_blame, blame, "CODE_OPENING", indentation)
        <> "```"
        <> annotation,
      )

      list.map(blamed_contents, fn(blamed_content) {
        io.println(
          margin_assembler(
            pre_blame,
            blamed_content.blame,
            "CODE_BODY",
            indentation,
          )
          <> blamed_content.content,
        )
      })

      io.println(
        margin_assembler(pre_blame, blame, "CODE_CLOSING", indentation)
        <> "```"
        <> annotation,
      )
    }

    TentativeTag(blame, tag_name, tentative_blamed_attributes, children) -> {
      io.println(
        margin_assembler(pre_blame, blame, "TAG", indentation)
        <> "|>"
        <> " "
        <> ins(tag_name),
      )

      list.map(tentative_blamed_attributes, fn(t) -> Nil {
        {
          margin_assembler(
            pre_blame,
            t.blame,
            "ATTRIBUTE",
            indentation <> debug_print_spaces,
          )
          <> ins(t.key)
          <> " "
          <> t.value
        }
        |> io.println
      })

      case list.length(children) > 0 {
        True -> {
          io.println(margin_suppress_blame_assembler(
            pre_blame,
            blame,
            "(printer inserted)",
          ))
        }
        False -> Nil
      }

      debug_print_tentatives_internal(
        pre_blame,
        indentation <> debug_print_spaces,
        children,
      )
    }

    TentativeErrorIndentationTooLarge(blame, message) ->
      margin_error_assembler(
        pre_blame,
        blame,
        "INDENTATION ERROR (LARGE): " <> message,
      )
      |> io.println

    TentativeErrorIndentationNotMultipleOfFour(blame, message) ->
      margin_error_assembler(
        pre_blame,
        blame,
        "INDENTATION ERROR (!MULT 4): " <> message,
      )
      |> io.println

    TentativeErrorNoCodeBlockClosing(blame) ->
      margin_error_assembler(
        pre_blame,
        blame,
        "CLOSING BACKTICKS NOT FOUND ERROR",
      )
      |> io.println

    TentativeErrorCodeBlockAnnotation(blame, message) ->
      margin_error_assembler(
        pre_blame,
        blame,
        "CLOSING BACKTICKS UNWANTED ANNOTATION ERROR: " <> message,
      )
      |> io.println
  }
}

fn debug_print_tentatives_internal(
  pre_blame: String,
  indentation: String,
  tentatives: List(TentativeWriterly),
) {
  case tentatives {
    [] -> Nil
    [first, ..rest] -> {
      debug_print_tentative_internal(pre_blame, indentation, first)
      debug_print_tentatives_internal(pre_blame, indentation, rest)
    }
  }
}

//*************************************
//* debug printing writerly as itself *
//*************************************

fn debug_print_writerly_internal(
  pre_blame: String,
  indentation: String,
  t: Writerly,
) {
  case t {
    BlankLine(blame) ->
      margin_assembler(pre_blame, blame, "BLANK", indentation)
      |> io.println

    Blurb(_, blamed_contents) -> {
      map_with_special_first(
        blamed_contents,
        fn(first) {
          {
            margin_assembler(pre_blame, first.blame, "BLURB_ROOT", indentation)
            <> first.content
          }
          |> io.println
        },
        fn(after_first) {
          {
            margin_assembler(pre_blame, after_first.blame, "BLURB", indentation)
            <> after_first.content
          }
          |> io.println
        },
      )

      Nil
    }

    CodeBlock(blame, annotation, blamed_contents) -> {
      {
        margin_assembler(pre_blame, blame, "CODE_OPENING", indentation)
        <> "```"
        <> annotation
      }
      |> io.println

      list.map(blamed_contents, fn(blamed_content) {
        {
          margin_assembler(
            pre_blame,
            blamed_content.blame,
            "CODE_BODY",
            indentation,
          )
          <> blamed_content.content
        }
        |> io.println
      })

      {
        margin_assembler(pre_blame, blame, "CODE_CLOSING", indentation)
        <> "```"
        <> annotation
      }
      |> io.println
    }

    Tag(blame, tag_name, tentative_blamed_attributes, children) -> {
      {
        margin_assembler(pre_blame, blame, "TAG", indentation)
        <> "|>"
        <> " "
        <> tag_name
      }
      |> io.println

      list.map(tentative_blamed_attributes, fn(t) {
        {
          margin_assembler(
            pre_blame,
            t.blame,
            "ATTRIBUTE",
            indentation <> debug_print_spaces,
          )
          <> t.key
          <> " "
          <> t.value
        }
        |> io.println
      })

      case list.length(children) > 0 {
        True ->
          io.println(margin_suppress_blame_assembler(
            pre_blame,
            blame,
            "(printer inserted)",
          ))

        False -> Nil
      }

      debug_print_writerlys_internal(
        pre_blame,
        indentation <> debug_print_spaces,
        children,
      )
    }
  }
}

fn debug_print_writerlys_internal(
  pre_blame: String,
  indentation: String,
  writerlys: List(Writerly),
) {
  case writerlys {
    [] -> Nil
    [first, ..rest] -> {
      debug_print_writerly_internal(pre_blame, indentation, first)
      debug_print_writerlys_internal(pre_blame, indentation, rest)
    }
  }
}

pub fn debug_print_writerlys(pre_blame: String, writerlys: List(Writerly)) {
  debug_print_writerlys_internal(pre_blame, "", writerlys)
}

//*******************************
//* converting Writerly to VXML *
//*******************************

const writerly_blank_line_vxml_tag = "WriterlyBlankLine"

const writerly_blurb_vxml_tag = "WriterlyBlurb"

const writerly_code_block_vxml_tag = "WriterlyCodeBlock"

const writerly_code_block_annotation_vxml_attribute_name = "language"

pub fn writerly_to_vxml(t: Writerly) -> VXML {
  case t {
    BlankLine(blame) ->
      V(
        blame: blame,
        tag: writerly_blank_line_vxml_tag,
        attributes: [],
        children: [],
      )

    Blurb(blame, blamed_contents) ->
      V(blame: blame, tag: writerly_blurb_vxml_tag, attributes: [], children: [
        T(blame: blame, contents: blamed_contents),
      ])

    CodeBlock(blame, annotation, blamed_contents) ->
      V(
        blame: blame,
        tag: writerly_code_block_vxml_tag,
        attributes: [
          BlamedAttribute(
            blame,
            writerly_code_block_annotation_vxml_attribute_name,
            annotation,
          ),
        ],
        children: [T(blame: blame, contents: blamed_contents)],
      )

    Tag(blame, tag, attributes, children) -> {
      V(
        blame: blame,
        tag: tag,
        attributes: attributes,
        children: writerlys_to_vxmls(children),
      )
    }
  }
}

pub fn writerlys_to_vxmls(writerlys: List(Writerly)) -> List(VXML) {
  list.map(writerlys, writerly_to_vxml)
}

//**************************
//* debug printing as VXML *
//**************************

fn add_quotes(s: String) -> String {
  "\"" <> s <> "\""
}

fn debug_print_vxml_internal(pre_blame: String, indentation: String, t: VXML) {
  case t {
    T(blame, blamed_contents) -> {
      { margin_assembler(pre_blame, blame, "TEXT_NODE", indentation) <> "<>" }
      |> io.println

      list.map(blamed_contents, fn(blamed_content) {
        {
          margin_assembler(
            pre_blame,
            blamed_content.blame,
            "TEXT_LINE",
            indentation,
          )
          <> debug_print_spaces
          <> add_quotes(blamed_content.content)
        }
        |> io.println
      })

      Nil
    }

    V(blame, tag, blamed_attributes, children) -> {
      {
        margin_assembler(pre_blame, blame, "TAG", indentation)
        <> "<>"
        <> " "
        <> tag
      }
      |> io.println

      list.map(blamed_attributes, fn(t) {
        {
          margin_assembler(pre_blame, t.blame, "ATTRIBUTE", indentation)
          <> debug_print_spaces
          <> t.key
          <> " "
          <> t.value
        }
        |> io.println
      })

      debug_print_vxmls_internal(
        pre_blame,
        indentation <> debug_print_spaces,
        children,
      )
    }
  }
}

fn debug_print_vxmls_internal(
  pre_blame: String,
  indentation: String,
  vxmls: List(VXML),
) {
  case vxmls {
    [] -> Nil
    [first, ..rest] -> {
      debug_print_vxml_internal(pre_blame, indentation, first)
      debug_print_vxmls_internal(pre_blame, indentation, rest)
    }
  }
}

pub fn debug_print_vxmls(pre_blame: String, vxmls: List(VXML)) {
  debug_print_vxmls_internal(pre_blame, "", vxmls)
}

//********
//* main *
//********

fn file_is_not_commented(path: String) -> Bool {
  !{ string.contains(path, "/#") || string.starts_with(path, "#") }
}

fn path_to_parent_file(path: String) -> String {
  let pieces = {
    string.split(path, "/") |> list.reverse
  }
  case pieces {
    [] -> "wut?"
    [_, ..rest] -> string.join(list.reverse(rest), "/") <> "/__parent.emu"
  }
}

fn depth_in_directory_tree(path: String, dirname: String) -> Int {
  {
    path
    |> string.drop_left(string.length(dirname) + 1)
    |> string.split("/")
    |> list.length
  }
  - 1
}

fn zero_one(b: Bool) -> Int {
  case b {
    True -> 1
    False -> 0
  }
}

fn add_tree_depth(path: String, dirname: String) -> #(Int, String) {
  let base_depth = depth_in_directory_tree(path, dirname)
  let would_be_parent_path = path_to_parent_file(path)
  let must_add_1 = {
    { simplifile.is_file(would_be_parent_path) |> result.unwrap(False) }
    && path != would_be_parent_path
  }
  #(base_depth + zero_one(must_add_1), path)
}

fn blamed_lines_for_file_at_depth(
  pair: #(Int, String),
  dirname: String,
) -> Result(List(BlamedLine), FileError) {
  let #(depth, filename) = pair
  let length_to_drop = case dirname == "" {
    True -> 0
    False -> string.length(dirname) + 1
  }
  let shortened_filename_for_blame = string.drop_left(filename, length_to_drop)

  case simplifile.read(filename) {
    Ok(string) ->
      Ok(string_to_blamed_lines(
        depth * 4,
        string,
        shortened_filename_for_blame,
        1,
      ))
    Error(error) -> Error(error)
  }
}

fn get_files(dirname: String) -> Result(List(String), FileError) {
  case simplifile.get_files(dirname) {
    Ok(files) -> Ok(files)
    Error(simplifile.Enotdir) -> Ok([dirname])
    Error(error) -> Error(error)
  }
}

pub fn assemble_blamed_lines(
  dirname: String,
) -> Result(List(BlamedLine), FileError) {
  case get_files(dirname) {
    Ok(files) -> {
      files
      |> list.filter(file_is_not_commented)
      |> list.sort(string.compare)
      |> list.map(add_tree_depth(_, dirname))
      |> list.map(blamed_lines_for_file_at_depth(_, dirname))
      |> result.all
      |> result.map(list.concat)
    }
    Error(error) -> Error(error)
  }
}

fn contents_test() {
  let dirname = "test/contents"

  case assemble_blamed_lines(dirname) {
    Ok(lines) -> {
      case parse_blamed_lines(lines, True) {
        Ok(writerlys) -> {
          debug_print_writerlys("(debug_print_writerlys)", writerlys)
          io.println("")
          io.println("")
          debug_print_vxmls("(vxmls)", writerlys |> writerlys_to_vxmls)
          io.println("")
        }

        Error(error) -> {
          io.println("\nthere was a parsing error:")
          io.println(ins(error))
        }
      }
      Nil
    }
    Error(error) -> {
      io.println("there was an error:" <> ins(error))
    }
  }
}

fn sample_test() {
  let filename = "test/contents"

  case simplifile.read(filename) {
    Error(e) -> io.println("Error reading " <> filename <> ": " <> ins(e))

    Ok(file) -> {
      case parse_string(file, filename, True) {
        Ok(writerlys) -> {
          debug_print_vxmls("(vxmls)", writerlys |> writerlys_to_vxmls)
        }

        Error(error) -> {
          io.println("\nthere was a parsing error:")
          io.println(ins(error))
        }
      }

      Nil
    }
  }
}

pub fn main() {
  let test_content = True
  case test_content {
    True -> contents_test()
    False -> sample_test()
  }
}
