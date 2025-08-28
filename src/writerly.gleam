import blame.{type Blame, prepend_comment as pc} as bl
import io_lines.{type InputLine, InputLine, type OutputLine, OutputLine} as io_l
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/pair
import gleam/result
import gleam/regexp
import gleam/string.{inspect as ins}
import simplifile
import vxml.{type BlamedAttribute, type BlamedContent, type VXML, BlamedAttribute, BlamedContent, T, V} as vx
import dirtree as dt

const debug = False

// ****************
// * utils
// ****************

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

// ****************
// * public types
// ****************

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

pub type ParseError {
  TagEmpty(Blame)
  TagIllegalCharacter(Blame, String, String)
  AttributeKeyIllegalCharacter(Blame, String, String)
  IndentationTooLarge(Blame, String)
  IndentationNotMultipleOfFour(Blame, String)
  CodeBlockNotClosed(Blame)
  CodeBlockUnwantedAnnotationAtClose(Blame)
}

pub type AssemblyError {
  FileError(simplifile.FileError)
  TwoFilesSameName(String) // because we accept both .emu and .wly extensions, but we want to avoid mixing error
}

pub type AssemblyOrParseError {
  ParseError(ParseError)
  AssemblyError(AssemblyError)
}

// ***************
// * local types *
// ***************

type FileHead =
  List(InputLine)

type BadTagName {
  Empty
  IllegalCharacter(String, String)
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
  UndesiredAnnotation(Blame, FileHead)
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
  TentativeErrorCodeBlockUnwantedAnnotationAtClose(blame: Blame, message: String)
  TentativeErrorCodeBlockNotClosed(blame: Blame)
}

// ************
// * FileHead *
// ************

fn current_line(head: FileHead) -> Option(InputLine) {
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
) -> Result(BlamedAttribute, ParseError) {
  case t.key {
    Ok(key) -> Ok(BlamedAttribute(blame: t.blame, key: key, value: t.value))
    Error(IllegalAttributeKeyCharacter(original_would_be_key, bad_char)) ->
      Error(AttributeKeyIllegalCharacter(
        t.blame,
        original_would_be_key,
        bad_char,
      ))
  }
}

fn tentative_blamed_attributes_to_blamed_attributes(
  attrs: List(TentativeBlamedAttribute),
) -> Result(List(BlamedAttribute), ParseError) {
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

fn tentatives_to_writerlys(
  tentatives: List(TentativeWriterly),
) -> Result(List(Writerly), ParseError) {
  case tentatives {
    [] -> Ok([])
    [first, ..rest] ->
      case parse_from_tentative(first) {
        Ok(parsed) ->
          case tentatives_to_writerlys(rest) {
            Ok(parseds) -> Ok(list.prepend(parseds, parsed))

            Error(error) -> Error(error)
          }

        Error(error) -> Error(error)
      }
  }
}

fn parse_from_tentative(
  tentative: TentativeWriterly,
) -> Result(Writerly, ParseError) {
  case tentative {
    TentativeErrorCodeBlockUnwantedAnnotationAtClose(blame, _) ->
      Error(CodeBlockUnwantedAnnotationAtClose(blame))

    TentativeErrorIndentationTooLarge(blame, message) ->
      Error(IndentationTooLarge(blame, message))

    TentativeErrorIndentationNotMultipleOfFour(blame, message) ->
      Error(IndentationNotMultipleOfFour(blame, message))

    TentativeErrorCodeBlockNotClosed(blame) -> Error(CodeBlockNotClosed(blame))

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
        Error(Empty) -> Error(TagEmpty(blame))

        Error(IllegalCharacter(original_bad_name, bad_char)) ->
          Error(TagIllegalCharacter(
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
              case tentatives_to_writerlys(tentative_children) {
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

  case suffix {
    "```" <> _ -> TripleBacktick(string.drop_start(suffix, 3))
    "|>" <> _ -> Pipe(string.drop_start(suffix, 2))
    _ -> Other(suffix)
  }
}

fn fast_forward_past_lines_of_indent_at_least(
  indent: Int,
  head: FileHead,
) -> FileHead {
  case current_line(head) {
    None -> head

    Some(InputLine(_, suffix_indent, _)) ->
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
  let assert False = string.contains(key, "=")
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

    Some(InputLine(blame, suffix_indent, suffix)) -> {
      case suffix == "" 
        || suffix_indent != indent
        || string.starts_with(suffix, "|>")
        || string.starts_with(suffix, "```")
      {
        True -> #([], head)

        False -> {
          case string.split_once(suffix, "=") {
            Error(_) -> #([], head)

            Ok(#(key, value)) -> {
              let key = string.trim(key)
              let value = string.trim(value)
              case string.contains(key, " ") || key == "" {
                True -> #([], head)

                False -> {
                  let attribute_pair =
                    #(key, string.trim(value))
                    |> tentative_blamed_attribute(blame, _)

                  let #(more_attribute_pairs, head_after_attributes) =
                    fast_forward_past_attribute_lines_at_indent(
                      indent,
                      move_forward(head),
                    )

                  #(
                    [attribute_pair, ..more_attribute_pairs],
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
}

fn fast_forward_past_other_lines_at_indent(
  indent: Int,
  head: FileHead,
) -> #(List(BlamedContent), FileHead) {
  case current_line(head) {
    None -> #([], head)

    Some(InputLine(blame, suffix_indent, suffix)) -> {
      case suffix == "" {
        True -> #([], head)

        False -> {
          case suffix_indent != indent
            || string.starts_with(suffix, "|>")
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

fn fast_forward_to_closing_backticks(
  indent: Int,
  head: FileHead,
) -> Result(#(List(BlamedContent), FileHead), ClosingBackTicksError) {
  case current_line(head) {
    None -> Error(NoBackticksFound(head))

    Some(InputLine(blame, suffix_indent, suffix)) -> {
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
                      Error(UndesiredAnnotation(blame, move_forward(head)))
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
    True -> Error(Empty)
    False -> {
      let something_illegal = contains_one_of(proposed_name, ["-", ".", " "])
      case string.is_empty(something_illegal) {
        True -> Ok(proposed_name)
        False -> Error(IllegalCharacter(proposed_name, something_illegal))
      }
    }
  }
}

fn tentative_first_non_blank_line_is_blurb(
  nodes: List(TentativeWriterly),
) -> Bool {
  case nodes {
    [TentativeBlankLine(_), ..rest] ->
      tentative_first_non_blank_line_is_blurb(rest)
    [TentativeBlurb(_, _), ..] -> True
    _ -> False
  }
}

fn remove_starting_escapes(contents: List(BlamedContent)) -> List(BlamedContent) {
  let assert Ok(re) = regexp.from_string("^\\\\+\\s")
  list.map(contents, fn(blamed_content) {
    let new_content = case regexp.check(re, blamed_content.content) {
      False -> blamed_content.content
      True -> blamed_content.content |> string.drop_start(1)
    }
    BlamedContent(blamed_content.blame, new_content)
  })
}

fn tentative_parse_at_indent(
  indent: Int,
  head: FileHead,
) -> #(List(TentativeWriterly), List(TentativeWriterly), FileHead) {
  case current_line(head) {
    None -> #([], [], head)

    Some(InputLine(blame, suffix_indent, suffix)) -> {
      case suffix == "" {
        True -> {
          let tentative_blank_line = TentativeBlankLine(blame)
          let #(siblings, siblings_trailing_blank_lines, remainder_after_indent) =
            tentative_parse_at_indent(indent, move_forward(head))

          case siblings {
            [] -> #(
              siblings,
              list.prepend(siblings_trailing_blank_lines, tentative_blank_line),
              remainder_after_indent,
            )
            _ -> #(
              list.prepend(siblings, tentative_blank_line),
              siblings_trailing_blank_lines,
              remainder_after_indent,
            )
          }
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

                  let #(
                    siblings,
                    siblings_trailing_blank_lines,
                    head_after_indent,
                  ) = tentative_parse_at_indent(indent, move_forward(head))

                  #(
                    list.prepend(siblings, error),
                    siblings_trailing_blank_lines,
                    head_after_indent,
                  )
                }

                False -> #([], [], head)
              }
            }

            False -> {
              case suffix_indent > indent {
                True -> {
                  let head_after_oversize_indent =
                    fast_forward_past_lines_of_indent_at_least(
                      suffix_indent,
                      head,
                    )

                  let #(
                    siblings,
                    siblings_trailing_blank_lines,
                    head_after_indent,
                  ) =
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

                      #(
                        list.prepend(siblings, error),
                        siblings_trailing_blank_lines,
                        head_after_indent,
                      )
                    }

                    False -> {
                      let error_message =
                        ins(suffix_indent) <> " spaces before " <> ins(suffix)

                      let error =
                        TentativeErrorIndentationNotMultipleOfFour(
                          blame,
                          error_message,
                        )

                      #(
                        list.prepend(siblings, error),
                        siblings_trailing_blank_lines,
                        head_after_indent,
                      )
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

                      let #(
                        children,
                        children_trailing_blank_lines,
                        head_after_children,
                      ) =
                        tentative_parse_at_indent(
                          indent + 4,
                          head_after_attributes,
                        )

                      // filter out syntax-imposed blank line:
                      let children = case children {
                        [TentativeBlankLine(_), ..rest] -> {
                          case tentative_first_non_blank_line_is_blurb(rest) {
                            True -> rest
                            False -> children
                          }
                        }
                        _ -> children
                      }

                      let tentative_tag =
                        TentativeTag(
                          blame: blame,
                          tag: check_good_tag_name(string.trim(annotation)),
                          attributes: tentative_attributes,
                          children: children,
                        )

                      let #(
                        siblings,
                        siblings_trailing_blank_lines,
                        head_after_indent,
                      ) = tentative_parse_at_indent(indent, head_after_children)

                      case siblings {
                        [] -> #(
                          [tentative_tag],
                          list.append(
                            children_trailing_blank_lines,
                            siblings_trailing_blank_lines,
                          ),
                          head_after_indent,
                        )
                        _ -> #(
                          list.prepend(
                            list.append(children_trailing_blank_lines, siblings),
                            tentative_tag,
                          ),
                          siblings_trailing_blank_lines,
                          head_after_indent,
                        )
                      }
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

                          let #(
                            siblings,
                            siblings_trailing_blank_lines,
                            head_after_indent,
                          ) =
                            tentative_parse_at_indent(
                              indent,
                              head_after_code_block,
                            )

                          #(
                            list.prepend(siblings, tentative_code_block),
                            siblings_trailing_blank_lines,
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
                            <> " for backticks opened at "
                            <> bl.blame_digest(blame)
                            <> " carry unexpected annotation"

                          let tentative_error =
                            TentativeErrorCodeBlockUnwantedAnnotationAtClose(
                              blame,
                              error_message,
                            )

                          let #(
                            siblings,
                            siblings_trailing_blank_lines,
                            head_after_indent,
                          ) =
                            tentative_parse_at_indent(indent, head_after_error)

                          #(
                            list.prepend(siblings, tentative_error),
                            siblings_trailing_blank_lines,
                            head_after_indent,
                          )
                        }

                        Error(NoBackticksFound(head_after_indent)) -> {
                          let tentative_error =
                            TentativeErrorCodeBlockNotClosed(blame)

                          #([tentative_error], [], head_after_indent)
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
                          ) |> remove_starting_escapes,
                        )

                      let #(
                        siblings,
                        siblings_trailing_blank_lines,
                        head_after_indent,
                      ) = tentative_parse_at_indent(indent, head_after_others)

                      #(
                        list.prepend(siblings, tentative_blurb),
                        siblings_trailing_blank_lines,
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

//****************************************
//* tentative parsing api (input lines) *
//****************************************

fn tentative_parse_input_lines(
  head: FileHead,
) -> List(TentativeWriterly) {
  let head = list.filter(head, fn(line) { !string.starts_with(line.suffix, "!!") })
  let #(parsed, _, final_head) = tentative_parse_at_indent(0, head)

  let parsed =
    list.drop_while(parsed, fn(writerly) {
      case writerly {
        TentativeBlankLine(_) -> True
        _ -> False
      }
    })

  let assert True = list.is_empty(final_head)

  case debug {
    True -> {
      io.println("\n\n(tentative parse:)")
      echo_tentatives(parsed, "tentative_parse_input_lines")
      io.println("(tentative end)\n\n")
    }
    False -> Nil
  }

  parsed
}

//***************************************
//* writerly parsing api (input lines) *
//***************************************

pub fn parse_input_lines(
  lines: List(InputLine),
) -> Result(List(Writerly), ParseError) {
  lines
  |> tentative_parse_input_lines
  |> tentatives_to_writerlys
}

//*********************************
//* writerly parsing api (string) *
//*********************************

fn parse_string(
  source: String,
  filename: String,
) -> Result(List(Writerly), ParseError) {
  source
  |> io_l.string_to_input_lines(filename, 0)
  |> parse_input_lines
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
      "IndentationTooLarge",
      message,
    )
    TentativeErrorIndentationNotMultipleOfFour(blame, message) -> #(
      blame,
      "IndentationNotMultipleOfFour",
      message,
    )
    TentativeErrorCodeBlockNotClosed(blame) -> #(
      blame,
      "CodeBlockNotClosed",
      "",
    )
    TentativeErrorCodeBlockUnwantedAnnotationAtClose(blame, message) -> #(
      blame,
      "CodeBlockUnwantedAnnotationAtClose",
      message,
    )
  }
}

fn blamed_content_to_output_line(
  blamed_content: BlamedContent,
  indentation: Int,
) -> OutputLine {
  OutputLine(blamed_content.blame, indentation, blamed_content.content)
}

fn blamed_contents_to_output_lines(
  blamed_contents: List(BlamedContent),
  indentation: Int,
) -> List(OutputLine) {
  blamed_contents
  |> list.map(blamed_content_to_output_line(_, indentation))
}

fn tentative_blamed_attribute_to_output_line(
  blamed_attribute: TentativeBlamedAttribute,
  indentation: Int,
) -> OutputLine {
  case blamed_attribute.key {
    Ok(_) ->
      OutputLine(
        blamed_attribute.blame,
        indentation,
        ins(blamed_attribute.key) <> "=" <> blamed_attribute.value,
      )
    Error(IllegalAttributeKeyCharacter(bad_key, bad_char)) ->
      OutputLine(
        blamed_attribute.blame
          |> pc("ERROR illegal character in key: " <> bad_char),
        indentation,
        bad_key <> " " <> blamed_attribute.value,
      )
  }
}

fn tentative_blamed_attributes_to_output_lines(
  blamed_attributes: List(TentativeBlamedAttribute),
  indentation: Int,
) -> List(OutputLine) {
  blamed_attributes
  |> list.map(tentative_blamed_attribute_to_output_line(_, indentation))
}

fn tentative_to_output_lines_internal(
  t: TentativeWriterly,
  indentation: Int,
) -> List(OutputLine) {
  case t {
    TentativeBlankLine(blame) -> {
      [OutputLine(blame, 0, "")]
    }
    TentativeBlurb(_, blamed_contents) ->
      blamed_contents_to_output_lines(blamed_contents, indentation)
    TentativeCodeBlock(blame, annotation, blamed_contents) -> {
      list.flatten([
        [OutputLine(blame, indentation, "```" <> annotation)],
        blamed_contents_to_output_lines(blamed_contents, indentation),
        [OutputLine(blame, indentation, "```")],
      ])
    }
    TentativeTag(blame, maybe_tag, attributes, children) -> {
      let tag_line = case maybe_tag {
        Ok(tag) -> OutputLine(blame, indentation, "|> " <> tag)
        Error(IllegalCharacter(bad_tag, bad_char)) ->
          OutputLine(
            blame
              |> bl.prepend_comment(
                "ERROR illegal tag character: " <> bad_char,
              ),
            indentation,
            "|> " <> bad_tag,
          )
        Error(Empty) ->
          OutputLine(
            blame |> bl.prepend_comment("ERROR empty tag"),
            indentation,
            "<>",
          )
      }
      let attribute_lines =
        tentative_blamed_attributes_to_output_lines(attributes, indentation + 4)
      let children_lines =
        tentatives_to_output_lines_internal(children, indentation + 4)
      let blank_lines = case list.is_empty(children_lines) {
        True -> []
        False -> [OutputLine(blame, 0, "")]
      }
      list.flatten([[tag_line], attribute_lines, blank_lines, children_lines])
    }
    _ -> {
      let #(blame, error_type, message) =
        tentative_error_blame_and_type_and_message(t)
      [
        OutputLine(
          blame
            |> bl.prepend_comment("ERROR " <> error_type),
          indentation,
          message,
        ),
      ]
    }
  }
}

fn tentatives_to_output_lines_internal(
  tentatives: List(TentativeWriterly),
  indentation: Int,
) -> List(OutputLine) {
  tentatives
  |> list.map(tentative_to_output_lines_internal(_, indentation))
  |> list.flatten
}

fn echo_tentatives(
  tentatives: List(TentativeWriterly),
  banner: String,
) -> List(TentativeWriterly) {
  tentatives
  |> tentatives_to_output_lines_internal(0)
  |> io_l.echo_output_lines(banner)
  tentatives
}

//*************************************
//* Writerly -> blamed lines internals
//*************************************

pub fn writerly_annotate_blames(writerly: Writerly) -> Writerly {
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
          |> list.map(writerly_annotate_blames),
      )
  }
}

fn blamed_attribute_to_output_line(
  blamed_attribute: BlamedAttribute,
  indentation: Int,
) -> OutputLine {
  OutputLine(
    blamed_attribute.blame,
    indentation,
    blamed_attribute.key <> "=" <> blamed_attribute.value,
  )
}

fn blamed_attributes_to_output_lines(
  blamed_attributes: List(BlamedAttribute),
  indentation: Int,
) -> List(OutputLine) {
  blamed_attributes |> list.map(blamed_attribute_to_output_line(_, indentation))
}

fn first_child_is_blurb_and_first_line_of_blurb_could_be_read_as_attribute_value_pair(nodes: List(Writerly)) -> Bool {
  case nodes {
    [Blurb(_, lines), ..] -> {
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
) -> List(OutputLine) {
  case t {
    BlankLine(blame) -> [OutputLine(blame, 0, "")]
    Blurb(_, blamed_contents) ->
      blamed_contents_to_output_lines(blamed_contents, indentation)
    CodeBlock(blame, annotation, blamed_contents) -> {
      list.flatten([
        [OutputLine(blame, indentation, "```" <> annotation)],
        blamed_contents_to_output_lines(blamed_contents, indentation),
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
    Tag(blame, tag, attributes, children) -> {
      let tag_line = OutputLine(blame, indentation, "|> " <> tag)
      let attribute_lines =
        blamed_attributes_to_output_lines(attributes, indentation + 4)
      let children_lines =
        children
        |> list.map(writerly_to_output_lines_internal(_, indentation + 4, annotate_blames))
        |> list.flatten
      let buffer_lines = case first_child_is_blurb_and_first_line_of_blurb_could_be_read_as_attribute_value_pair(children) {
        True -> {
          let blame = case annotate_blames {
            False -> blame |> bl.clear_comments
            True -> blame |> bl.clear_comments |> pc("(a-b separation line)")
          }
          [OutputLine(blame, 0, "")]
        }
        False -> []
      }
      list.flatten([[tag_line], attribute_lines, buffer_lines, children_lines])
    }
  }
}

//*********************************
//* Writerly -> output lines api
//*********************************

pub fn writerly_to_output_lines(
  writerly: Writerly,
) -> List(OutputLine) {
  writerly
  |> writerly_to_output_lines_internal(0, False)
}

pub fn writerlys_to_output_lines(
  writerlys: List(Writerly),
) -> List(OutputLine) {
  writerlys
  |> list.map(writerly_to_output_lines)
  |> list.flatten
}

//*********************************
//* Writerly -> String api
//*********************************

pub fn writerly_to_string(writerly: Writerly) -> String {
  writerly
  |> writerly_to_output_lines()
  |> io_l.output_lines_to_string
}

pub fn writerlys_to_string(
  writerlys: List(Writerly),
) -> String {
  writerlys
  |> writerlys_to_output_lines()
  |> io_l.output_lines_to_string
}


//*********************************
//* echo_writerly api
//*********************************

pub fn echo_writerly(writerly: Writerly, banner: String) -> Writerly {
  writerly
  |> writerly_annotate_blames
  |> writerly_to_output_lines_internal(0, True)
  |> io_l.echo_output_lines(banner)
  writerly
}

//*******************************
//* Writerly -> VXML
//*******************************

const writerly_blank_line_vxml_tag = "WriterlyBlankLine"
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

    Blurb(blame, blamed_contents) -> T(blame: blame, contents: blamed_contents)

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
        children: children |> list.map(writerly_to_vxml),
      )
    }
  }
}

pub fn writerlys_to_vxmls(
  writerlys: List(Writerly)
) -> List(VXML) {
  writerlys
  |> list.map(writerly_to_vxml)
}

//***************************
//* assemble_input_lines internals
//***************************

fn file_is_not_commented(path: String) -> Bool {
  !{ string.contains(path, "/#") || string.starts_with(path, "#") }
}

fn has_extension(path: String, exts: List(String)) {
  list.any(exts, string.ends_with(path, _))
}

fn is_parent(path: String) -> Bool {
  string.ends_with(path, "__parent.emu") || string.ends_with(path, "__parent.wly")
}

fn file_is_parent_or_is_selected(
  path_selectors: List(String),
  path: String,
) -> Bool {
  is_parent(path)
  || path_selectors == []
  || list.any(path_selectors, string.contains(path, _))
}

fn file_is_not_parent_or_has_selected_descendant_or_is_selected(
  path_selectors: List(String),
  selected_with_unwanted_parents: List(String),
  path: String,
) -> Bool {
  !is_parent(path)
  || path_selectors == []
  || list.any(path_selectors, string.contains(path, _))
  || list.any(
    selected_with_unwanted_parents,
    fn(x) { !is_parent(x) && string.starts_with(x, path |> string.drop_end(string.length("__parent.emu"))) }
  )
}

fn parent_path_without_extension(path: String) -> String {
  let pieces = string.split(path, "/") |> list.reverse
  case pieces {
    [] -> "wut?"
    [_, ..rest] -> string.join(list.reverse(rest), "/") <> "/__parent."
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
  let would_be_parent_path = parent_path_without_extension(path)
  let must_add_1 = {
    {
      { simplifile.is_file(would_be_parent_path <> "emu") |> result.unwrap(False) } ||
      { simplifile.is_file(would_be_parent_path <> "wly") |> result.unwrap(False) }
    }
    && !is_parent(path)
  }
  #(base_depth + zero_one(must_add_1), path)
}

fn shortname_for_blame(path: String, dirname: String) -> String {
  let length_to_drop = case string.ends_with(dirname, "/") || dirname == "" {
    True -> string.length(dirname)
    False -> string.length(dirname) + 1
  }
  string.drop_start(path, length_to_drop)
}

fn input_lines_for_file_at_depth(
  pair: #(Int, String),
  dirname: String,
) -> Result(List(InputLine), AssemblyError) {
  let #(depth, path) = pair
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
    Error(error) -> {
      io.println("error reading " <> path)
      Error(FileError(error))
    }
  }
}

fn get_files(
  dirname: String,
) -> Result(#(Bool, List(String)), simplifile.FileError) {
  case simplifile.get_files(dirname) {
    Ok(files) ->
      Ok(#(
        True,
        files
          |> list.filter(keeping: fn(file) {
            !string.contains(file, ".DS_Store")
          }),
      ))
    Error(simplifile.Enotdir) -> Ok(#(False, [dirname]))
    Error(error) -> Error(error)
  }
}

fn dir_and_filename(path: String) -> #(String, String) {
  let reversed_path = path |> string.reverse
  let #(reversed_filename, reversed_dir) =
    reversed_path
    |> string.split_once("/")
    |> result.unwrap(#(reversed_path, ""))
  #(reversed_dir |> string.reverse, reversed_filename |> string.reverse)
}

fn filename_compare(f1: String, f2: String) {
  case is_parent(f1) {
    True -> order.Lt
    False -> {
      case is_parent(f2) {
        True -> order.Gt
        False -> string.compare(f1, f2)
      }
    }
  }
}

fn lexicographic_sort_but_parent_comes_first(
  path1: String,
  path2: String,
) -> order.Order {
  let #(dir1, f1) = dir_and_filename(path1)
  let #(dir2, f2) = dir_and_filename(path2)
  let dir_order = string.compare(dir1, dir2)
  case dir_order {
    order.Eq -> filename_compare(f1, f2)
    _ -> dir_order
  }
}

fn has_duplicate(l: List(String)) -> Option(String) {
  case l {
    [] -> None
    [first, ..rest] -> {
      case list.contains(rest, first) {
        True -> Some(first)
        False -> has_duplicate(rest)
      }
    }
  }
}

fn check_no_duplicate_files(files: List(String)) -> Result(Nil, AssemblyError) {
  let files =
    files
    |> list.map(string.drop_end(_, 4))
  case has_duplicate(files) {
    Some(dup) -> Error(TwoFilesSameName(dup <> ".emu has both .emu & .wly versions"))
    None -> Ok(Nil)
  }
}

fn drop_slash(s: String) {
  case string.ends_with(s, "/") {
    True -> string.drop_end(s, 1)
    False -> string.drop_end(s, 0)
  }
}

pub fn assemble_input_lines_advanced_mode(
  dirname: String,
  path_selectors: List(String),
) -> Result(#(List(String), List(InputLine)), AssemblyError) {
  let dirname = drop_slash(dirname)
  case get_files(dirname) {
    Ok(#(was_dir, files)) -> {
      let selected_with_unwanted_parents =
        files
        |> list.filter(has_extension(_, [".emu", ".wly"]))
        |> list.filter(file_is_not_commented)
        |> list.filter(file_is_parent_or_is_selected(path_selectors, _))

      let sorted =
        selected_with_unwanted_parents
        |> list.filter(
          file_is_not_parent_or_has_selected_descendant_or_is_selected(
            path_selectors,
            selected_with_unwanted_parents,
            _,
          ),
        )
        |> list.sort(lexicographic_sort_but_parent_comes_first)

      use _ <- result.try(check_no_duplicate_files(sorted))

      let tree = 
        sorted
        |> list.map(string.drop_start(_, string.length(dirname) + 1))
        |> dt.directory_tree_from_dir_and_paths(dirname, _, False)
        |> dt.pretty_printer

      use lines <- result.try(
        sorted
        |> list.map(add_tree_depth(_, dirname))
        |> list.map(
          input_lines_for_file_at_depth(
            _,
            case was_dir {
              True -> dirname
              False -> ""
            }
          ),
        )
        |> result.all
        |> result.map(list.flatten)
      )

      Ok(#(tree, lines))
    }

    Error(e) -> Error(FileError(e))
  }
}

//***************************
//* assemble_input_lines
//***************************

pub fn assemble_input_lines(
  dirname: String,
) -> Result(#(List(String), List(InputLine)), AssemblyError) {
  assemble_input_lines_advanced_mode(dirname, [])
}

//***************************
//* assemble_and_parse
//***************************

pub fn assemble_and_parse(
  dir_or_filename: String,
) -> Result(List(Writerly), AssemblyOrParseError) {
  use #(_, assembled) <- on_error_on_ok(
    assemble_input_lines(dir_or_filename),
    fn(e) {Error(AssemblyError(e))},
  )

  use writerlys <- on_error_on_ok(
    parse_input_lines(assembled),
    fn(e) {Error(ParseError(e))},
  )

  Ok(writerlys)
}

fn contents_test() {
  let dirname = "test/contents"

  use #(_, lines) <- on_error_on_ok(
    assemble_input_lines(dirname),
    fn (error) {
      io.println("assemble_input_lines error:" <> ins(error))
    }
  )

  use writerlys <- on_error_on_ok(
    parse_input_lines(lines),
    fn (error) {
      io.println("parse_input_lines error:" <> ins(error))
    }
  )

  let vxmls =
    writerlys
    |> list.map(writerly_to_vxml)

  list.index_map(
    writerlys,
    fn (writerly, i) {
      echo_writerly(writerly, "contents_test writerly fragment # " <> ins(i + 1))
      io.println("")
    }
  )

  io.println("")

  list.index_map(
    vxmls,
    fn (vxml, i) {
      vx.echo_vxml(vxml, "contents_test vxml fragment # " <> ins(i + 1))
      io.println("")
    }
  )

  io.println("")
}

//***************************************************
//* vxml to writerly (canonical transformation) (?) *
//***************************************************

fn is_whitespace(s: String) -> Bool {
  string.trim(s) == ""
}

fn escape_left_spaces_in_string(s: String) -> String {
  let m = string.trim_start(s)
  let n = string.length(s) - string.length(m)
  case n > 0 {
    True -> "\\" <> string.repeat(" ", n) <> m
    False -> m
  }
}

fn escape_left_spaces(
  contents: List(BlamedContent),
) -> List(BlamedContent) {
  list.map(contents, fn(blamed_content) {
    BlamedContent(
      blamed_content.blame,
      blamed_content.content |> escape_left_spaces_in_string,
    )
  })
}

fn process_vxml_t_node(vxml: VXML) -> List(Writerly) {
  let assert T(_, blamed_contents) = vxml
  blamed_contents
  |> list.index_map(fn(blamed_content, i) { #(i, blamed_content) })
  |> list.filter(fn(pair) {
    let #(index, blamed_content) = pair
    !is_whitespace(blamed_content.content)
    || index == 0
    || index == list.length(blamed_contents) - 1
  })
  |> list.map(pair.second)
  |> escape_left_spaces
  |> fn(blamed_contents) {
    case blamed_contents {
      [] -> []
      [first, ..] -> [Blurb(first.blame, blamed_contents)]
    }
  }
}

fn is_t(vxml: VXML) -> Bool {
  case vxml {
    T(_, _) -> True
    _ -> False
  }
}

pub fn vxml_to_writerlys(vxml: VXML) -> List(Writerly) { // it would 'Writerly' not 'List(Writerly)' if for the fact that someone could give an empty text node
  case vxml {
    V(blame, tag, attributes, children) -> {
      case tag {
        "WriterlyBlankLine" -> {
          let assert True = list.is_empty(attributes)
          let assert True = list.is_empty(children)
          [BlankLine(blame)]
        }
        "WriterlyCodeBlock" -> {
          let assert True = list.all(children, is_t)
          let annotation = case list.find(attributes, fn(b) { b.key == "language" }) {
            Ok(thing) -> thing.value
            Error(Nil) -> ""
          }
          let lines =
            children
            |> list.map(
              fn(t) {
                let assert T(_, lines) = t
                lines
              }
            )
            |> list.flatten
          [CodeBlock(blame, annotation, lines)]
        }
        _ -> {
          let children = children |> vxmls_to_writerlys
          [Tag(blame, tag, attributes, children)]
        }
      }
    }
    T(_, _) -> {
      vxml |> process_vxml_t_node
    }
  }
}

pub fn vxmls_to_writerlys(vxmls: List(VXML)) -> List(Writerly) {
  vxmls
  |> list.map(vxml_to_writerlys)
  |> list.flatten
}

//**************
// tests/main
//**************

fn html_test() {
  let path = "test/ch5_ch.xml"

  use content <- on_error_on_ok(simplifile.read(path), fn(_) {
    io.println("could not read file " <> path)
  })

  use vxml <- on_error_on_ok(vx.xmlm_based_html_parser(content, path), fn(e) {
    io.println("xmlm_based_html_parser error: " <> ins(e))
  })

  let writerlys = vxml_to_writerlys(vxml)

  io.println("")
  io.println("list.length(writerlys) == " <> ins(list.length(writerlys)))
  io.println("")

  writerlys
  |> list.index_map(
    fn (writerly, i) {
      echo_writerly(writerly, "html_test " <> ins(i + 1))
      io.println("")
    }
  )

  let _ = simplifile.write(
    "test/ch5_ch.emu",
    writerlys
    |> list.map(writerly_to_string)
    |> string.concat
  )

  Nil
}

fn sample_test() {
  let filename = "test/sample.wly"

  use contents <- on_error_on_ok(
    simplifile.read(filename),
    fn(e) {io.println("there was an io error: " <> ins(e))}
  )

  use writerlys <- on_error_on_ok(
    parse_string(contents, filename),
    fn(e) {io.println("there was a parsing error: " <> ins(e))}
  )

  io.println("")
  io.println("list.length(writerlys) == " <> ins(list.length(writerlys)))
  io.println("")

  writerlys
  |> list.index_map(
    fn (writerly, i) {
      echo_writerly(writerly, "sample_test writerly " <> ins(i + 1))
      io.println("")
    }
  )

  let vxmls = writerlys |> list.map(writerly_to_vxml)

  io.println("list.length(vxmls) == " <> ins(list.length(vxmls)))
  io.println("")

  vxmls
  |> list.index_map(
    fn (wxml, i) {
      vx.echo_vxml(wxml, "sample_test vxml" <> ins(i + 1))
      io.println("")
    }
  )

  Nil
}

pub fn digest(w: Writerly) -> String {
  case w {
    BlankLine(_) -> "BlankLine"
    Blurb(_, _) -> ins(w)
    CodeBlock(_, _, _) -> ins(w)
    Tag(_, _, _, _) -> "Tag " <> w.name
  }
}

pub fn avoid_linter_complaint_about_unused_functions() {
  contents_test()
  sample_test()
  html_test()
}

pub fn main() {
  contents_test()
}
