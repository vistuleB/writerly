import blamedlines.{
  type Blame, type BlamedLine, BlamedLine, prepend_comment as pc,
}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/pair
import gleam/result
import gleam/string.{inspect as ins}
import simplifile
import vxml.{
  type BlamedAttribute, type BlamedContent, type VXML, BlamedAttribute,
  BlamedContent, T, V, debug_print_vxmls,
}

// ****************
// * public types *
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

    TentativeErrorNoCodeBlockClosing(blame) -> Error(WriterlyParseError(blame))

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

    Some(BlamedLine(blame, suffix_indent, suffix)) -> {
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

    Some(BlamedLine(blame, suffix_indent, suffix)) -> {
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

fn tentative_parse_at_indent(
  indent: Int,
  head: FileHead,
) -> #(List(TentativeWriterly), List(TentativeWriterly), FileHead) {
  case current_line(head) {
    None -> #([], [], head)

    Some(BlamedLine(blame, suffix_indent, suffix)) -> {
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
                            <> " for backticks opened at L"
                            <> ins(blame.line_no)
                            <> " carry unexpected annotation"

                          let tentative_error =
                            TentativeErrorCodeBlockAnnotation(
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
                            TentativeErrorNoCodeBlockClosing(blame)

                          #([tentative_error], [], head_after_indent)
                        }
                      }

                    Other(_) -> {
                      let blame = blame
                      let suffix = case string.starts_with(suffix, "\\ ") {
                        True -> string.drop_start(suffix, 1)
                        False -> suffix
                      }
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
//* tentative parsing api (blamed lines) *
//****************************************

fn tentative_parse_blamed_lines(
  head: FileHead,
  debug: Bool,
) -> List(TentativeWriterly) {
  let head =
    list.filter(head, fn(blamed_line) {
      !string.starts_with(blamed_line.suffix, "!!")
    })
  let head =
    list.filter(head, fn(blamed_line) {
      !string.starts_with(blamed_line.suffix, "!!")
    })
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
        ins(blamed_attribute.key) <> "=" <> blamed_attribute.value,
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
    blamed_attribute.key <> "=" <> blamed_attribute.value,
  )
}

fn blamed_attributes_to_blamed_lines(
  blamed_attributes: List(BlamedAttribute),
  indentation: Int,
) -> List(BlamedLine) {
  blamed_attributes |> list.map(blamed_attribute_to_blamed_line(_, indentation))
}

// fn starts_with_text_child(writerlys: List(Writerly)) -> Bool {
//   case writerlys {
//     [Blurb(_, _), ..] -> True
//     _ -> False
//   }
// }

fn first_non_blank_line_is_blurb(nodes: List(Writerly)) -> Bool {
  case nodes {
    [BlankLine(_), ..rest] -> first_non_blank_line_is_blurb(rest)
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
      let blank_lines = case first_non_blank_line_is_blurb(children) {
        // && list.length(children_lines) > 0 // <- why was this guy necessary?
        True -> [
          BlamedLine(
            case debug_annotations {
              False -> blame |> blamedlines.clear_comments
              True ->
                blame
                |> blamedlines.clear_comments
                |> pc("(a-b separation line)")
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

fn intersperse_blank_lines_between_blurbs(
  writerlys: List(Writerly),
) -> List(Writerly) {
  case writerlys {
    [] -> []
    [Blurb(_, _) as b1, Blurb(_, _) as b2, ..rest] -> [
      b1,
      BlankLine(
        b2.blame |> blamedlines.clear_comments |> pc("(b-b separation line)"),
      ),
      ..intersperse_blank_lines_between_blurbs([b2, ..rest])
    ]
    [first, ..rest] -> [first, ..intersperse_blank_lines_between_blurbs(rest)]
  }
}

fn writerlys_to_blamed_lines_internal(
  writerlys: List(Writerly),
  indentation: Int,
  debug_annotations: Bool,
) -> List(BlamedLine) {
  writerlys
  |> intersperse_blank_lines_between_blurbs
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
  || list.any(selected_with_unwanted_parents, fn(x) {
    !is_parent(x)
    && string.starts_with(
      x,
      path |> string.drop_end(string.length("__parent.emu")),
    )
  })
}

fn parent_path_without_extension(path: String) -> String {
  let pieces = {
    string.split(path, "/") |> list.reverse
  }
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

fn blamed_lines_for_file_at_depth(
  pair: #(Int, String),
  dirname: String,
) -> Result(List(BlamedLine), FileOrParseError) {
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
      Ok(blamedlines.string_to_blamed_lines_hard_mode(
        string,
        shortname,
        1,
        depth * 4,
      ))
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

fn filename_and_dir(path: String) -> #(String, String) {
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

fn lexicographic_sort_but_parent_emu_comes_first(
  path1: String,
  path2: String,
) -> order.Order {
  let #(dir1, f1) = filename_and_dir(path1)
  let #(dir2, f2) = filename_and_dir(path2)
  let dir_order = string.compare(dir1, dir2)
  case dir_order {
    order.Eq -> filename_compare(f1, f2)
    _ -> dir_order
  }
}

pub fn assemble_blamed_lines_advanced_mode(
  dirname: String,
  path_selectors: List(String),
) -> Result(List(BlamedLine), FileOrParseError) {
  case get_files(dirname) {
    Ok(#(was_dir, files)) -> {
      let selected_with_unwanted_parents =
        files
        |> list.filter(file_is_not_commented)
        |> list.filter(file_is_parent_or_is_selected(path_selectors, _))

      selected_with_unwanted_parents
      |> list.filter(
        file_is_not_parent_or_has_selected_descendant_or_is_selected(
          path_selectors,
          selected_with_unwanted_parents,
          _,
        ),
      )
      |> list.sort(lexicographic_sort_but_parent_emu_comes_first)
      |> list.map(add_tree_depth(_, dirname))
      |> list.map(
        blamed_lines_for_file_at_depth(_, case was_dir {
          True -> dirname
          False -> ""
        }),
      )
      |> result.all
      |> result.map(list.flatten)
    }
    Error(error) -> Error(FileError(error))
  }
}

pub fn assemble_blamed_lines(
  dirname: String,
) -> Result(List(BlamedLine), FileOrParseError) {
  assemble_blamed_lines_advanced_mode(dirname, [])
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

  case assemble_blamed_lines_advanced_mode(dirname, []) {
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

//***************************************************
//* vxml to writerly (canonical transformation) (?) *
//***************************************************

fn is_whitespace(s: String) -> Bool {
  string.trim(s) == ""
}

fn replace_left_spaces_by_ensp_in_string(s: String) -> String {
  let m = string.trim_start(s)
  string.repeat("&ensp;", string.length(s) - string.length(m)) <> m
}

fn replace_left_spaces_by_ensp(
  contents: List(BlamedContent),
) -> List(BlamedContent) {
  list.map(contents, fn(blamed_content) {
    BlamedContent(
      blamed_content.blame,
      blamed_content.content |> replace_left_spaces_by_ensp_in_string,
    )
  })
  list.map(contents, fn(blamed_content) {
    BlamedContent(
      blamed_content.blame,
      blamed_content.content |> replace_left_spaces_by_ensp_in_string,
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
  |> list.map(fn(pair) { pair |> pair.second })
  |> replace_left_spaces_by_ensp
  |> fn(blamed_contents) {
    case blamed_contents {
      [] -> []
      [first, ..] -> [Blurb(first.blame, blamed_contents)]
    }
  }
}

fn vxml_to_writerly_internal(vxml: VXML) -> List(Writerly) {
  case vxml {
    V(blame, tag, attributes, children) -> {
      case tag == "WriterlyBlankLine" {
        True -> {
          let assert True = list.is_empty(attributes)
          let assert True = list.is_empty(children)
          [BlankLine(blame)]
        }
        False -> {
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
  |> list.map(vxml_to_writerly_internal)
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

  use vxml <- on_error_on_ok(vxml.xmlm_based_html_parser(content, path), fn(e) {
    io.println("xmlm_based_html_parser error: " <> ins(e))
  })

  let writerlys = vxmls_to_writerlys([vxml])

  debug_print_writerlys("", writerlys)

  let _ = simplifile.write("test/ch5_ch.emu", writerlys_to_string(writerlys))

  Nil
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

pub fn avoid_linter_complaint_about_unused_functions() {
  contents_test()
  sample_test()
  html_test()
}

pub fn main() {
  contents_test()
}
