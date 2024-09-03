import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import simplifile

// ****************
// * public types *
// ****************

pub type Blame {
  Blame(filename: String, line_no: Int, comments: List(String))
}

pub type BlamedLine {
  BlamedLine(blame: Blame, indent: Int, suffix: String)
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
  TentativeErrorIndentationNotMultipleOfFour(blame: Blame)
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
                  let error = TentativeErrorIndentationNotMultipleOfFour(blame)

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
                      let error =
                        TentativeErrorIndentationNotMultipleOfFour(blame)
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

fn add_blames(
  filename: String,
  current_line_no: Int,
  pairs: List(#(Int, String)),
) -> List(BlamedLine) {
  case pairs {
    [] -> []
    [#(indent, suffix), ..rest] -> {
      let blamed_first =
        BlamedLine(Blame(filename, current_line_no, []), indent, suffix)
      list.prepend(
        add_blames(filename, current_line_no + 1, rest),
        blamed_first,
      )
    }
  }
}

fn string_to_blamed_lines(
  source: String,
  filename: String,
  starting_line_number: Int,
) -> List(BlamedLine) {
  string.split(source, "\n")
  |> list.map(fn(line) {
    let suffix = string.trim_left(line)
    let indent = string.length(line) - string.length(suffix)
    #(indent, suffix)
  })
  |> add_blames(filename, starting_line_number, _)
}

fn tentative_parse_at_indent_0(head: FileHead) -> List(TentativeWriterly) {
  let #(parsed, final_head) = tentative_parse_at_indent(0, head)
  let assert True = list.is_empty(final_head)
  parsed
}

fn tentative_parse_string(
  source: String,
  filename: String,
) -> List(TentativeWriterly) {
  let head = string_to_blamed_lines(source, filename, 1)
  let parsed = tentative_parse_at_indent_0(head)
  io.println("\n\n(tentative parse:)")
  pretty_print_tentatives("(tentative)", "", parsed)
  io.println("(tentative end)\n\n")
  parsed
}

pub fn parse_string(
  source: String,
  filename: String,
) -> Result(List(Writerly), WriterlyParseError) {
  tentative_parse_string(source, filename)
  |> parse_from_tentatives
}

//************
//* printing *
//************

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
  let takes_place_of_blame =
    blame.filename
    <> ":"
    <> string.pad_right(ins(blame.line_no), margin_line_number_pad_to, " ")
    |> string.length
    |> string.repeat(" ", _)

  prefix
  <> takes_place_of_blame
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

pub fn main() {
  let filename = "src/sample.emu"

  case simplifile.read(filename) {
    Error(e) -> io.println("Error reading " <> filename <> ": " <> ins(e))

    Ok(file) -> {
      case parse_string(file, filename) {
        Ok(writerlys) -> {
          io.println("\n\n(writerlys parse:)")
          pretty_print_writerlys("(writerlys)", "", writerlys)
          io.println("(writerlys end)\n\n")
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
