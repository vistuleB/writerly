import blamedlines.{
  type Blame, type BlamedLine, Blame, BlamedLine, prepend_comment as pc,
}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/pair
import simplifile

// ****************
// * public types *
// ****************

import vxml_parser.{
  type BlamedAttribute, type BlamedContent, type VXML, BlamedAttribute,
  BlamedContent, T, V, debug_print_vxmls,
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
  WriterlyParseError(Blame)
}

pub type FileOrParseError {
  FileError(simplifile.FileError)
  ParseError(WriterlyParseError)
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
      Error(WriterlyParseError(blame))

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
    True -> Pipe(string.drop_start(suffix, 2))

    False ->
      case string.starts_with(suffix, "```") {
        True -> TripleBacktick(string.drop_start(suffix, 3))

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
                string.pad_start(suffix, to: padded_suffix_length, with: " ")
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
                  let annotation = string.drop_start(suffix, 3) |> string.trim

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
          case string.starts_with(suffix, "!!") {
            True -> {
              tentative_parse_at_indent(indent, move_forward(head))
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
                            string.repeat(" ", suffix_indent) <> suffix

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
  }
}

//****************************************
//* tentative parsing api (blamed lines) *
//****************************************

fn tentative_parse_blamed_lines(
  head: FileHead,
  debug: Bool,
) -> List(TentativeWriterly) {
  let #(parsed, final_head) = tentative_parse_at_indent(0, head)
  let assert True = list.is_empty(final_head)

  case debug {
    True -> {
      io.println("\n\n(tentative parse:)")
      debug_print_tentatives("(tentative)", parsed)
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
  debug: Bool,
) -> List(TentativeWriterly) {
  blamedlines.string_to_blamed_lines(source, filename)
  |> tentative_parse_blamed_lines(debug)
}

//***************************************
//* writerly parsing api (blamed lines) *
//***************************************

fn parse_blamed_lines_debug(
  lines: List(BlamedLine),
  debug: Bool,
) -> Result(List(Writerly), WriterlyParseError) {
  lines
  |> tentative_parse_blamed_lines(debug)
  |> parse_from_tentatives
}

pub fn parse_blamed_lines(
  lines: List(BlamedLine),
) -> Result(List(Writerly), WriterlyParseError) {
  parse_blamed_lines_debug(lines, False)
}

//*********************************
//* writerly parsing api (string) *
//*********************************

fn parse_string_debug(
  source: String,
  filename: String,
  debug: Bool,
) -> Result(List(Writerly), WriterlyParseError) {
  tentative_parse_string(source, filename, debug)
  |> parse_from_tentatives
}

pub fn parse_string(
  source: String,
  filename: String,
) -> Result(List(Writerly), WriterlyParseError) {
  parse_string_debug(source, filename, False)
}

//**********************
//* printing Tentative *
//**********************

fn tentative_error_blame_and_type_and_message(
  t: TentativeWriterly,
) -> #(Blame, String, String) {
  case t {
    TentativeBlankLine(_) -> panic as "not an error node"
    TentativeBlurb(_, _) -> panic as "not an error node"
    TentativeCodeBlock(_, _, _) -> panic as "not an error node"
    TentativeTag(_, _, _, _) -> panic as "not an error node"
    TentativeErrorIndentationTooLarge(blame, message) -> #(
      blame,
      "WriterlyParseErrorIndentationTooLarge",
      message,
    )
    TentativeErrorIndentationNotMultipleOfFour(blame, message) -> #(
      blame,
      "IndentationNotMultipleOfFour",
      message,
    )
    TentativeErrorNoCodeBlockClosing(blame) -> #(
      blame,
      "NoCodeBlockClosing",
      "",
    )
    TentativeErrorCodeBlockAnnotation(blame, message) -> #(
      blame,
      "CodeBlockAnnotation",
      message,
    )
  }
}

fn blamed_content_to_blamed_line(
  blamed_content: BlamedContent,
  indentation: Int,
) -> BlamedLine {
  BlamedLine(blamed_content.blame, indentation, blamed_content.content)
}

fn blamed_contents_to_blamed_lines(
  blamed_contents: List(BlamedContent),
  indentation: Int,
) -> List(BlamedLine) {
  blamed_contents
  |> list.map(blamed_content_to_blamed_line(_, indentation))
}

fn tentative_blamed_attribute_to_blamed_line(
  blamed_attribute: TentativeBlamedAttribute,
  indentation: Int,
) -> BlamedLine {
  case blamed_attribute.key {
    Ok(_) ->
      BlamedLine(
        blamed_attribute.blame,
        indentation,
        ins(blamed_attribute.key) <> " " <> blamed_attribute.value,
      )
    Error(IllegalAttributeKeyCharacter(bad_key, bad_char)) ->
      BlamedLine(
        blamed_attribute.blame
          |> pc("ERROR illegal character in key: " <> bad_char),
        indentation,
        bad_key <> " " <> blamed_attribute.value,
      )
  }
}

fn tentative_blamed_attributes_to_blamed_lines(
  blamed_attributes: List(TentativeBlamedAttribute),
  indentation: Int,
) -> List(BlamedLine) {
  blamed_attributes
  |> list.map(tentative_blamed_attribute_to_blamed_line(_, indentation))
}

fn tentative_to_blamed_lines_internal(
  t: TentativeWriterly,
  indentation: Int,
) -> List(BlamedLine) {
  case t {
    TentativeBlankLine(blame) -> {
      [BlamedLine(blame, 0, "")]
    }
    TentativeBlurb(_, blamed_contents) ->
      blamed_contents_to_blamed_lines(blamed_contents, indentation)
    TentativeCodeBlock(blame, annotation, blamed_contents) -> {
      list.flatten([
        [BlamedLine(blame, indentation, "```" <> annotation)],
        blamed_contents_to_blamed_lines(blamed_contents, indentation),
        [BlamedLine(blame, indentation, "```")],
      ])
    }
    TentativeTag(blame, maybe_tag, attributes, children) -> {
      let tag_line = case maybe_tag {
        Ok(tag) -> BlamedLine(blame, indentation, "|> " <> tag)
        Error(IllegalTagCharacter(bad_tag, bad_char)) ->
          BlamedLine(
            blame
              |> blamedlines.prepend_comment(
                "ERROR illegal tag character: " <> bad_char,
              ),
            indentation,
            "|> " <> bad_tag,
          )
        Error(EmptyTag) ->
          BlamedLine(
            blame |> blamedlines.prepend_comment("ERROR empty tag"),
            indentation,
            "<>",
          )
      }
      let attribute_lines =
        tentative_blamed_attributes_to_blamed_lines(attributes, indentation + 4)
      let children_lines =
        tentatives_to_blamed_lines_internal(children, indentation + 4)
      let blank_lines = case list.is_empty(children_lines) {
        True -> []
        False -> [BlamedLine(blame, 0, "")]
      }
      list.flatten([[tag_line], attribute_lines, blank_lines, children_lines])
    }
    _ -> {
      let #(blame, error_type, message) =
        tentative_error_blame_and_type_and_message(t)
      [
        BlamedLine(
          blame
            |> blamedlines.prepend_comment("ERROR " <> error_type),
          indentation,
          message,
        ),
      ]
    }
  }
}

fn tentatives_to_blamed_lines_internal(
  tentatives: List(TentativeWriterly),
  indentation: Int,
) -> List(BlamedLine) {
  tentatives
  |> list.map(tentative_to_blamed_lines_internal(_, indentation))
  |> list.flatten
}

fn debug_print_tentatives(banner: String, tentatives: List(TentativeWriterly)) {
  tentatives
  |> tentatives_to_blamed_lines_internal(0)
  |> blamedlines.blamed_lines_to_table_vanilla_bob_and_jane_sue(banner, _)
}

//*************************************
//* debug printing writerly as itself *
//*************************************

pub fn debug_annotate_blames(writerly: Writerly) -> Writerly {
  case writerly {
    BlankLine(blame) -> BlankLine(blame |> pc("BlankLine"))
    Blurb(blame, blamed_contents) ->
      Blurb(
        blame |> pc("Blurb"),
        list.index_map(blamed_contents, fn(blamed_content, i) {
          BlamedContent(
            blamed_content.blame
              |> pc("Blurb > BlamedContent(" <> ins(i + 1) <> ")"),
            blamed_content.content,
          )
        }),
      )
    CodeBlock(blame, annotation, blamed_contents) ->
      CodeBlock(
        blame |> pc("CodeBlock:" <> annotation),
        annotation,
        list.index_map(blamed_contents, fn(blamed_content, i) {
          BlamedContent(
            blamed_content.blame
              |> pc("CodeBlock > BlamedContent(" <> ins(i + 1) <> ")"),
            blamed_content.content,
          )
        }),
      )
    Tag(blame, tag, attributes, children) ->
      Tag(
        blame |> pc("Tag"),
        tag,
        list.index_map(attributes, fn(blamed_attribute, i) {
          BlamedAttribute(
            blamed_attribute.blame
              |> pc("Tag > BlamedAttribute(" <> ins(i + 1) <> ")"),
            blamed_attribute.key,
            blamed_attribute.value,
          )
        }),
        children
          |> list.map(debug_annotate_blames),
      )
  }
}

fn blamed_attribute_to_blamed_line(
  blamed_attribute: BlamedAttribute,
  indentation: Int,
) -> BlamedLine {
  BlamedLine(
    blamed_attribute.blame,
    indentation,
    blamed_attribute.key <> " " <> blamed_attribute.value,
  )
}

fn blamed_attributes_to_blamed_lines(
  blamed_attributes: List(BlamedAttribute),
  indentation: Int,
) -> List(BlamedLine) {
  blamed_attributes |> list.map(blamed_attribute_to_blamed_line(_, indentation))
}

fn starts_with_text_child(writerlys: List(Writerly)) -> Bool {
  case writerlys {
    [Blurb(_, _), ..] -> True
    _ -> False
  }
}

fn writerly_to_blamed_lines_internal(
  t: Writerly,
  indentation: Int,
  debug_annotations: Bool,
) -> List(BlamedLine) {
  case t {
    BlankLine(blame) -> [BlamedLine(blame, 0, "")]
    Blurb(_, blamed_contents) ->
      blamed_contents_to_blamed_lines(blamed_contents, indentation)
    CodeBlock(blame, annotation, blamed_contents) -> {
      list.flatten([
        [BlamedLine(blame, indentation, "```" <> annotation)],
        blamed_contents_to_blamed_lines(blamed_contents, indentation),
        [
          BlamedLine(
            case debug_annotations {
              False -> blame
              True -> blame |> pc("CodeBlock end")
            },
            indentation,
            "```",
          ),
        ],
      ])
    }
    Tag(blame, tag, attributes, children) -> {
      let tag_line = BlamedLine(blame, indentation, "|> " <> tag)
      let attribute_lines =
        blamed_attributes_to_blamed_lines(attributes, indentation + 4)
      let children_lines =
        writerlys_to_blamed_lines_internal(
          children,
          indentation + 4,
          debug_annotations,
        )
      let blank_lines = case
        starts_with_text_child(children) && list.length(children_lines) > 0
      {
        True -> [
          BlamedLine(
            case debug_annotations {
              False -> blame |> blamedlines.clear_comments
              True ->
                blame |> blamedlines.clear_comments |> pc("(separation line)")
            },
            0,
            "",
          ),
        ]
        False -> []
      }
      list.flatten([[tag_line], attribute_lines, blank_lines, children_lines])
    }
  }
}

fn writerlys_to_blamed_lines_internal(
  writerlys: List(Writerly),
  indentation: Int,
  debug_annotations: Bool,
) -> List(BlamedLine) {
  writerlys
  |> list.map(writerly_to_blamed_lines_internal(
    _,
    indentation,
    debug_annotations,
  ))
  |> list.flatten
}

pub fn debug_writerlys_to_string(
  banner: String,
  writerlys: List(Writerly),
) -> String {
  writerlys
  |> list.map(debug_annotate_blames)
  |> writerlys_to_blamed_lines_internal(0, True)
  |> blamedlines.blamed_lines_to_table_vanilla_bob_and_jane_sue(banner, _)
}

pub fn debug_writerly_to_string(banner: String, writerly: Writerly) -> String {
  [writerly]
  |> debug_writerlys_to_string(banner, _)
}

pub fn debug_print_writerlys(banner: String, writerlys: List(Writerly)) {
  debug_writerlys_to_string(banner, writerlys)
  |> io.print
}

pub fn debug_print_writerly(banner: String, writerly: Writerly) {
  [writerly]
  |> debug_print_writerlys(banner, _)
}

//*********************************
//* converting Writerly to string *
//*********************************

pub fn writerlys_to_blamed_lines(writerlys: List(Writerly)) -> List(BlamedLine) {
  writerlys
  |> writerlys_to_blamed_lines_internal(0, False)
}

pub fn writerlys_to_string(writerlys: List(Writerly)) -> String {
  writerlys
  |> writerlys_to_blamed_lines_internal(0, False)
  |> blamedlines.blamed_lines_to_string
}

pub fn writerly_to_string(writerly: Writerly) -> String {
  writerly
  |> writerly_to_blamed_lines_internal(0, False)
  |> blamedlines.blamed_lines_to_string
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

//********
//* main *
//********

fn file_is_not_commented(path: String) -> Bool {
  !{ string.contains(path, "/#") || string.starts_with(path, "#") }
}

fn file_is_not_hidden(path: String) -> Bool {
  let assert Ok(filename) = {
    path
    |> string.split("/")
    |> list.last
  }
  !string.starts_with(filename, ".")
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
    |> string.drop_start(string.length(dirname) + 1)
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

fn attribute_matches_arg(attr: BlamedAttribute, arg: #(String, String)) -> Bool {
  let #(key, value) = arg
  attr.key == key && attr.value == value
}

fn key_value_pairs_that_match_attribute(
  attr: BlamedAttribute,
  args: List(#(String, String))
) -> List(Bool) {
  list.map(
    args,
    attribute_matches_arg(attr, _)
  )
}

fn column_wise_or_pair(
  l1: List(Bool),
  l2: List(Bool),
) -> List(Bool) {
  let assert True = list.length(l1) == list.length(l2)
  list.map2(
    l1,
    l2,
    fn (b1, b2) { b1 || b2 }
  )
}

fn column_wise_or(
  lists: List(List(Bool)),
  common_length: Int,
) -> List(Bool) {
  list.fold(
    lists,
    list.repeat(False, common_length),
    column_wise_or_pair
  )
}

fn match_selector_internal(
  writerly: Writerly,
  key_value_pairs: List(#(String, String))
) -> #(List(Writerly), List(Bool)) {
  case writerly {
    Tag(blame, tag, attrs, children) -> {
      let bools =
        attrs
        |> list.map(key_value_pairs_that_match_attribute(_, key_value_pairs))
        |> column_wise_or(list.length(key_value_pairs))

      let list_pairs =
        list.map(
          children,
          match_selector_internal(_, key_value_pairs)
        )

      let final_bools =
        list_pairs
        |> list.map(pair.second)
        |> column_wise_or(list.length(key_value_pairs))
        |> column_wise_or_pair(bools)

      let assert True = list.all(
        list_pairs,
        fn (pair) {
          { pair |> pair.first |> list.is_empty } == { pair |> pair.second |> list.all(fn(b) {!b})}
        }
      )

      case list.any(bools, fn(b) { b }) {
        True -> #([writerly], final_bools)
        False -> {
          let children =
            list_pairs
            |> list.map(fn(pair) { pair |> pair.first })
            |> list.flatten
          case list.is_empty(children) {
            True -> {
              let assert True = !list.any(final_bools, fn(b) {b})
              #([], final_bools)
            }
            False -> #([Tag(blame, tag, attrs, children)], final_bools)
          }
        }
      }
    }

    _ -> #([], list.repeat(False, list.length(key_value_pairs)))
  }
}

fn writerlys_path_selector_filter(
  writerlys: List(Writerly),
  path_selector: #(String, List(#(String, String)))
) -> List(Writerly) {
  let #(path, key_value_pairs) = path_selector

  let list_pairs = list.map(
    writerlys,
    match_selector_internal(_, key_value_pairs)
  )

  let bools =
    list_pairs
    |> list.map(pair.second)
    |> column_wise_or(list.length(key_value_pairs))

  case {
    list.zip(bools, key_value_pairs)
    |> list.key_find(False)
  } {
    Error(Nil) -> Nil
    Ok(pair) -> panic as {"no matches for selector '" <> {pair |> pair.first } <> "=" <> {pair |> pair.second} <> "' in path " <> path}
  }

  list_pairs
  |> list.map(pair.first)
  |> list.flatten
}

fn take_while_not_prefix(
  lines: List(BlamedLine),
  prefix: String
) -> #(List(BlamedLine), List(BlamedLine)) {
  list.split_while(
    lines,
    fn(line) { !string.starts_with(line.blame.filename, prefix) }
  )
}

fn take_while_prefix(
  lines: List(BlamedLine),
  prefix: String
) -> #(List(BlamedLine), List(BlamedLine)) {
  list.split_while(
    lines,
    fn(line) { string.starts_with(line.blame.filename, prefix) }
  )
}

fn add_indent(
  lines: List(BlamedLine),
  indent: Int
) -> Result(List(BlamedLine), Nil) {
  let lines = 
    list.map(
      lines,
      fn(line) { BlamedLine(line.blame, line.indent + indent, line.suffix) }
    )
  case list.any(
    lines,
    fn(line) { line.indent < 0 }
  ) {
    True -> Error(Nil)
    False -> Ok(lines)
  }
}

fn blamed_lines_path_selector_filter(
  lines: List(BlamedLine),
  path_selector: #(String, List(#(String, String))),
  dirname: String,
) -> Result(List(BlamedLine), FileOrParseError) {
  let #(path, key_value_pairs) = path_selector

  let prefix = path |> shortname_for_blame(dirname)

  let #(without_prefix_1, remaining) = take_while_not_prefix(lines, prefix)
  let #(with_prefix, remaining) = take_while_prefix(remaining, prefix)
  let #(without_prefix_2, remaining) = take_while_not_prefix(remaining, prefix)

  let assert [] = remaining

  use first <- on_error_on_ok(
    list.first(with_prefix),
    fn (_) { panic as {"no lines match filename prefix '" <> prefix <> "' in writerly_parser.assemble_blamed_lines"} }
  )

  let with_prefix_indentation = first.indent

  use without_indent <- on_error_on_ok(
    add_indent(with_prefix, -with_prefix_indentation),
    fn (error) { Error(ParseError(WriterlyParseErrorIndentationTooLarge(first.blame, ins(error)))) }
  )

  let assert [first, ..] = without_indent
  let assert True = first.indent == 0

  use writerlys <- on_error_on_ok(
    parse_blamed_lines(without_indent),
    fn (error) { Error(ParseError(error)) }
  )

  let with_prefix_remaining =
    writerlys_path_selector_filter(writerlys, path_selector)
    |> writerlys_to_blamed_lines_internal(with_prefix_indentation, False)

  use <- on_lazy_true_on_false(
    list.is_empty(with_prefix_remaining),
    fn(){ panic as {"no elements in file prefix '" <> prefix <> "' match keys " <> ins(key_value_pairs)} }
  )

  [
    without_prefix_1,
    with_prefix_remaining,
    without_prefix_2,
  ]
  |> list.flatten
  |> Ok
}

fn blamed_lines_path_selectors_filter(
  lines: List(BlamedLine),
  path_selectors: List(#(String, List(#(String, String)))),
  dirname: String,
) -> Result(List(BlamedLine), FileOrParseError) {
  list.fold(
    path_selectors,
    Ok(lines),
    fn (res, path_selector) {
      result.then(
        res,
        blamed_lines_path_selector_filter(_, path_selector, dirname)
      )
    }
  )
}

fn shortname_for_blame(
  path: String,
  dirname: String,
) -> String {
  let length_to_drop = case string.ends_with(dirname, "/") || dirname == "" {
    True -> string.length(dirname)
    False -> string.length(dirname) + 1
  }
  string.drop_start(path, length_to_drop)
}

fn blamed_lines_for_file_at_depth(
  pair: #(Int, String),
  dirname: String,
) -> Result(List(BlamedLine), FileOrParseError) {
  let #(depth, path) = pair
  let shortname = shortname_for_blame(path, dirname)
  case shortname == "" {
    True -> panic as {"no shortname left after removing dirname '" <> dirname <> "' from path '" <> path <> "'"}
    False -> shortname
  }

  case simplifile.read(path) {
    Ok(string) ->
      Ok(blamedlines.string_to_blamed_lines_hard_mode(
        string,
        shortname,
        1,
        depth * 4,
      ))
    Error(error) -> Error(FileError(error))
  }
}

fn get_files(
  dirname: String,
) -> Result(#(Bool, List(String)), simplifile.FileError) {
  case simplifile.get_files(dirname) {
    Ok(files) -> Ok(#(True, files))
    Error(simplifile.Enotdir) -> Ok(#(False, [dirname]))
    Error(error) -> Error(error)
  }
}

pub fn assemble_blamed_lines_advanced_mode(
  dirname: String,
  path_selectors: List(#(String, List(#(String, String)))),
) -> Result(List(BlamedLine), FileOrParseError) {
  case get_files(dirname) {
    Ok(#(was_dir, files)) -> {
      files
      |> list.filter(file_is_not_commented)
      // |> list.filter(file_is_not_hidden)
      |> list.sort(string.compare)
      |> list.map(add_tree_depth(_, dirname))
      |> list.map(blamed_lines_for_file_at_depth(_, case was_dir {
        True -> dirname
        False -> ""
      }))
      |> result.all
      |> result.map(list.flatten)
      |> result.then(blamed_lines_path_selectors_filter(_, path_selectors, dirname))
    }
    Error(error) -> Error(FileError(error))
  }
}

pub fn assemble_blamed_lines(
  dirname: String,
) -> Result(List(BlamedLine), FileOrParseError) {
  assemble_blamed_lines_advanced_mode(dirname, [])
}

fn on_lazy_true_on_false(
  z: Bool,
  on_true: fn() -> a,
  on_false: fn() -> a,
) -> a {
  case z {
    True -> on_true()
    False -> on_false()
  }
}

fn on_error_on_ok(
  result r: Result(a, b),
  on_error on_error: fn(b) -> c,
  with_on_ok on_ok: fn(a) -> c,
) -> c {
  case r {
    Error(b) -> on_error(b)
    Ok(a) -> on_ok(a)
  }
}

pub fn assemble_and_parse_debug(
  dir_or_filename: String,
  debug: Bool,
) -> Result(List(Writerly), FileOrParseError) {
  use assembled <- on_error_on_ok(
    result: assemble_blamed_lines(dir_or_filename),
    on_error: Error,
  )

  use writerlys <- on_error_on_ok(
    result: parse_blamed_lines_debug(assembled, debug),
    on_error: fn(error) { Error(ParseError(error)) },
  )

  Ok(writerlys)
}

pub fn assemble_and_parse(
  dir_or_filename: String,
) -> Result(List(Writerly), FileOrParseError) {
  assemble_and_parse_debug(dir_or_filename, False)
}

fn contents_test() {
  let dirname = "test/contents"

  case assemble_blamed_lines_advanced_mode(
    dirname,
    [#(dirname <> "/chapter1/", [#("handle", "sec1")])]
  ) {
    Ok(lines) -> {
      case parse_blamed_lines_debug(lines, True) {
        Ok(writerlys) -> {
          debug_print_writerlys("(debug_print_writerlys)", writerlys)
          io.println("")
          io.println("")
          io.println(writerlys_to_string(writerlys))
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

//******************
//* vxml to writerly *
//******************

// pub type Writerly {
//   BlankLine(blame: Blame)
//   Blurb(blame: Blame, lines: List(BlamedContent))
//   CodeBlock(blame: Blame, annotation: String, lines: List(BlamedContent))
//   Tag(
//     blame: Blame,
//     name: String,
//     attributes: List(BlamedAttribute),
//     children: List(Writerly),
//   )
// }

fn vxml_to_writerly_internal(vxml: VXML) -> Writerly {
  case vxml {
    V(blame, tag, attributes, children) -> {
      let children = children |> vxmls_to_writerlys
      Tag(blame, tag, attributes, children)

    }
    T(blame, blamed_content) -> {
      Blurb(blame, blamed_content)
    }
  }

}

pub fn vxmls_to_writerlys(vxmls: List(VXML)) -> List(Writerly) {
  vxmls |> list.map(vxml_to_writerly_internal)
}



fn sample_test() {
  let filename = "test/sample.emu"

  case simplifile.read(filename) {
    Error(e) -> io.println("Error reading " <> filename <> ": " <> ins(e))

    Ok(file) -> {
      case parse_string_debug(file, filename, True) {
        Ok(writerlys) -> {
          debug_print_writerlys("(writerlys)", writerlys)
          io.println("")
          io.println(writerlys_to_string(writerlys))
          io.println("")
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
