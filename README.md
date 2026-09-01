# Writerly

Writerly is a lightweight, indentation-based markup language for authoring
tree-structured documents that resemble ordinary prose. Its syntax is
descended from [Elm-Markup](https://github.com/mdgriffith/elm-markup).

This Gleam package constitutes Writerly's reference implementation. It can:

- parse Writerly source into a typed AST;
- serialize the same AST back to Writerly source;
- convert between Writerly and [VXML](https://hex.pm/packages/vxml);
- assemble a document distributed across a directory of `.wly` files,
  according to Writerly's own multi-file document specification.

The [Writerly extension for VS Code](https://github.com/vistuleB/writerly-vscode-extension)
provides syntax highlighting, diagnostics, document navigation, and support
for working with referenced files.

Further help turning a parsed VXML tree into a target format is provided by the
`vxml_pipeline` package.

## Example

````writerly
|> Chapter
    id=introduction

    This is a paragraph. Consecutive text lines belong
    to the same paragraph.

    |> Section
        class=example
        id=zz33455

        A blank line separates this paragraph from the next.

        ```gleam&id=hello
        pub fn main() {
          io.println("Hello, world!")
        }
        ```

    !! This comment remains part of the Writerly tree.
````

The pipe symbol `|>` starts a new node, with the tag name
and nothing else on the same line. Its attributes and children are indented
by four spaces. Blank lines are represented explicitly rather than discarded.

## Use from Gleam

Add the package to a Gleam project:

```sh
gleam add writerly
```

Parse a source string containing any number of top-level nodes:

```gleam
import writerly

pub fn parse(source: String) {
  writerly.string_to_writerlys(source, "example.wly")
}
```

Use `string_to_writerly` instead when exactly one non-blank top-level node is
required. The corresponding `input_lines_to_writerlys` and
`input_lines_to_writerly` functions accept VXML `InputLine` values, preserving
source provenance through their `Blame` fields.

Writerly trees can be converted to VXML and serialized again:

```gleam
pub fn convert(source: String) {
  let assert Ok(document) =
    writerly.string_to_writerly(source, "example.wly")

  let vxml = writerly.writerly_to_vxml(document)
  let serialized = writerly.writerly_to_string(document)

  #(vxml, serialized)
}
```

The central syntax tree is:

```gleam
pub type Writerly {
  BlankLine(blame: Blame)
  Paragraph(blame: Blame, lines: List(Line))
  Comment(blame: Blame, lines: List(Line))
  CodeBlock(blame: Blame, attrs: List(Attr), lines: List(Line))
  Tag(blame: Blame, name: String, attrs: List(Attr), children: List(Writerly))
}
```

## Multi-file documents

`assemble_input_lines` accepts either a `.wly` file or a directory. A directory
may use `__parent.wly` as its root file:

```text
book/
├── __parent.wly
├── 01-introduction.wly
├── 02-examples.wly
└── appendix/
    ├── __parent.wly
    └── 01-tables.wly
```

Sibling files are processed in lexicographic path order. Their contents are
indented beneath the contents of `__parent.wly` if a `__parent.wly` file is present.
Otherwise they are concatenated at their native level of indentation.
Nested directories are assembled recursively.
Files and directories whose names start with `#` or do not end with `.wly` are ignored.

Use `assemble_input_lines_with_path_selector` to select source paths, or
`path_selector_from_only_paths` to construct a selector from a list of paths.
Assembly returns `InputLine` values so that filenames and line numbers remain
available to later parsing and VXML processing.

## VXML representation

The conversion to VXML uses ordinary VXML element and text nodes, with three
reserved element names:

- `WriterlyBlankLine` represents a semantic blank line;
- `WriterlyCodeBlock` represents a fenced code block;
- `WriterlyComment` represents a comment block.

The synthetic attribute `WriterlyCodeBlockInfoString` stores the leading info
string from a code fence. Structured fence annotations become ordinary VXML
attributes. Commented-out element attributes use keys of the form
`WriterlyCommentedAttribute<N>Spaces`, where `N` records their original spacing.
Applications should use the public helper functions for recognizing and
constructing these keys rather than assembling them by hand.

## Syntax specification

### Source and indentation

A Writerly document is a sequence of lines. Its structural indentation is made
of spaces and must be a multiple of four. A child is indented exactly four
spaces farther than its parent. Unexpected deeper indentation is an error.

Writerly documents conventionally use the `.wly` filename extension.

### Elements

An element begins with `|>` followed by a tag name:

```writerly
|> article
```

Whitespace surrounding the name after `|>` is trimmed. A tag name must match:

```text
[A-Za-z_:][-A-Za-z0-9._:]*
```

Attributes and children occur on subsequent lines, indented four more spaces.
An element ends when the indentation returns to that element's level or lower.
Closing tags are not written.

### Attributes

Immediately after an element line, lines of the following form are parsed as
attributes:

```writerly
|> figure
    id=overview
    class=wide diagram
```

The first `=` separates the key and value. An attribute key must match:

```text
[A-Za-z_][-A-Za-z0-9._:]*
```

Attribute values are trimmed at both ends. More than 100 leading spaces after
the `=` are rejected. Empty values are allowed.

Attribute parsing stops at the first line that is not a valid attribute. If an
element's first text line resembles an attribute, insert a blank line before
it:

```writerly
|> Example

    equation=a+b
```

### Paragraphs and text lines

Consecutive ordinary lines at the same indentation form one `Paragraph`.
Writerly preserves their contents, including trailing whitespace.

A text line whose content would otherwise begin with structural syntax can be
escaped with a backslash. The initial backslash is removed:

```writerly
\    text beginning with spaces
\!! text, not a comment
\``` text, not a code fence
```

The serializer inserts this escape when required.

### Blank lines

Every empty source line becomes a `BlankLine`. Blank lines are semantic nodes:
adding or removing one changes the Writerly tree. Their meaning, if any, is
assigned by later processing.

### Comments

At ordinary child position, consecutive lines beginning with `!!` form one
`Comment` node. The `!!` marker is removed from the stored line content:

```writerly
!! first comment line
!! second comment line
```

In the attribute region immediately following an element, an `!!` line is
instead stored as a commented-out attribute. The number of spaces after `!!`
is preserved, up to a maximum of 100:

```writerly
|> figure
    !! class=temporarily-disabled
    src=figure.svg
```

### Fenced code blocks

A line beginning with three backticks opens a code block. A line containing
three backticks at the same indentation closes it:

````writerly
```gleam
pub fn answer() {
  42
}
```
````

The closing fence may have trailing whitespace but no annotation. Lines inside
the block retain their relative indentation. A content line that would look
like a closing fence is escaped with a leading backslash.

The text immediately following the opening backticks is the info string. It
must not begin with a space. An info string may be followed by structured
annotations written as `&key=value`:

````writerly
```gleam&id=answer&class=example
```
````

Annotation keys follow the attribute-key grammar. Values are trimmed and obey
the same 100-leading-space limit as element attributes. An ampersand or
backslash that belongs to the info string or an annotation is escaped with a
backslash. Parsing and serialization preserve the distinction between the
leading info string and structured annotations.

### Top-level cardinality

A document may contain any number of top-level nodes.
`string_to_writerlys` and `input_lines_to_writerlys` return all of them.
`string_to_writerly` and `input_lines_to_writerly` require exactly one
non-blank top-level node; otherwise they return `MissingRoot` or
`NonUniqueRoot`.
