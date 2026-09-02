# Writerly

[![Package Version](https://img.shields.io/hexpm/v/writerly)](https://hex.pm/packages/writerly)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/writerly/)

Writerly is a lightweight, indentation-based markup language for authoring
tree-structured documents that resemble ordinary prose. Its syntax is
descended from [Elm-Markup](https://github.com/mdgriffith/elm-markup).

This Gleam package constitutes Writerly's reference implementation. It can:

- convert between Writerly and [VXML](https://hex.pm/packages/vxml)
- assemble a document distributed across a directory of `.wly` files,
  according to Writerly's own
  [multi-file document specification](#multi-file-documents)

Writerly is deliberately tag- and attribute-agnostic: it assigns no built-in
meaning to a document's vocabulary. Each project typically defines that
vocabulary through a process known as *desugaring*, which transforms parsed
VXML into target-specific structure. The
[VXML Pipeline](https://hex.pm/packages/vxml_pipeline) package provides
desugaring support.

The
[Writerly extension for VS Code](https://github.com/vistuleB/writerly-vscode-extension)
provides syntax highlighting, diagnostics, and document navigation.

### Example

````writerly
|> Chapter
    title=The observatory on the hill
    handle=observatory

    The road to the observatory follows the river for
    several miles before turning uphill. In summer, tall
    grass hides the stones that once marked the route.

    At the summit stands a small white building with a
    copper dome. The telescope inside was installed in
    1927, but its clockwork drive still follows the stars
    across the sky.

    |> Section
        title=Recording an observation
        handle=recording-observations

        An observation begins with the time, the position
        of the telescope, and a short description of the
        weather. Blank lines separate paragraphs, while
        consecutive lines remain part of the same paragraph.

        A project may give _underscores_ a meaning such
        as emphasis, although Writerly itself leaves them
        as text.

        |> Note
            class=field-note

            Clouds near the horizon can make a bright
            star appear to fade and return several times.

        The measurements can then be processed by a program:

        ```gleam&id=record
        pub fn record(name, brightness) {
          #(name, brightness)
        }
        ```

    !! Recheck the installation date before publication.
````

The pipe symbol `|>` starts a new node, with the tag name and nothing else on
the same line. Its attributes and children are indented by four spaces.
Ordinary prose needs no marker. Consecutive text lines form a paragraph, while
blank lines are represented explicitly rather than discarded.

### Beginning-of-line escape sequences

Indentation, `!!`, and triple backticks have structural meaning at the start
of a line. Prefix them with a backslash when they should instead be text:

````writerly
\   text beginning with spaces
\!! text, not a comment
\```text, not a code fence
````

Writerly removes that one escape backslash and preserves the following text.
Inside a fenced code block, use the same technique for a content line that
would otherwise look like the closing fence. The serializer adds these
escapes when required.

### Use from Gleam

Add the package to a Gleam project:

```sh
gleam add writerly
```

Parse a source string with one non-blank top-level element directly to VXML:

```gleam
import writerly
import vxml/io_lines

pub fn parse(source: String) {
  source
  |> io_lines.string_to_input_lines("example.wly", 0)
  |> writerly.input_lines_to_vxml
}
```

`input_lines_to_vxml` preserves source provenance through VXML `Blame` values.
The lower-level `string_to_writerlys` and `input_lines_to_writerlys` functions
are available when a source contains multiple top-level nodes; convert their
results with `writerlys_to_vxmls`.

### Multi-file documents

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

### VXML representation

The conversion to VXML uses ordinary VXML element and text nodes, with three
reserved element names:

- `WriterlyBlankLine` represents a semantic blank line;
- `WriterlyCodeBlock` represents a fenced code block;
- `WriterlyComment` represents a comment block.

The synthetic attribute `WriterlyCodeBlockInfoStringPrefix` stores the
nonempty, unmarked prefix of a code fence's info string: the part before its
first unescaped `&key=value` annotation. The attribute is absent when that
prefix is empty. Structured fence annotations become ordinary VXML attributes.
Commented-out element attributes use keys of the form
`WriterlyCommentedAttribute<N>Spaces`, where `N` records their original spacing.
Applications should use the public helper functions for recognizing and
constructing these keys rather than assembling them by hand.

VXML-to-Writerly conversion and Writerly serialization return `Result` values.
Malformed reserved elements, empty text nodes, and malformed manually
constructed Writerly nodes are reported as `SerializationError` values.

### Cross-document references

Writerly has an official convention for cross-document references based on
*handles*. A handle connects a definition site to one or more usage sites.
Handle syntax is interpreted by editor tooling and desugaring rather than by
the core parser. A definition may be an ordinary element attribute:

```writerly
|> Theorem
    handle=odd-order-theorem
```

or an in-text marker:

```writerly
odd-order-theorem##<<
```

A usage places `>>` immediately before the handle name:

```writerly
The result now follows from Theorem >>odd-order-theorem.
```

A single backslash suppresses either kind of in-text recognition:

```writerly
odd-order-theorem\##<< is ordinary text
\>>odd-order-theorem is ordinary text
```

Inside fenced code-block content, in-text handle definitions remain active,
but `>>handle` usages are ordinary code text. Neither form is recognized on
the opening or closing fence line.

The [full handle grammar](#handle-grammar) at the end of this README describes
additional definition forms, escaping rules, and other capabilities.

In parsed VXML, `handle` remains an ordinary attribute and in-text handle forms
remain text. Projects interpret them later through desugaring. The
[VXML Pipeline](https://hex.pm/packages/vxml_pipeline) package supplies shared
handle desugarers.

The
[Writerly extension for VS Code](https://github.com/vistuleB/writerly-vscode-extension#handles)
provides navigation, completion, rename support, and diagnostics for handles
that follow the same grammar.

### Inline formatting

Unlike some other lightweight markup languages, Writerly assigns no special
meaning to underscores, asterisks, tildes, or similar inline delimiters. They
remain ordinary text. A project may introduce rich-text formatting with its
own delimiters and interpret them during desugaring.

Writerly provides structural elements, attributes, paragraphs, comments, and
fenced code blocks without prescribing a built-in rich-text vocabulary. This
is one respect in which Writerly may differ from syntax familiar from
Elm-Markup and other lightweight markup languages.

## Technical Syntax Reference

### Source and indentation

A Writerly document is a sequence of lines. Its structural indentation is made
of spaces and must be a multiple of four. A child is indented exactly four
spaces farther than its parent. Unexpected deeper indentation is an error.

Writerly documents conventionally use the `.wly` filename extension.

### Line classification

Parsing proceeds linearly. Structural indentation first determines the current
parent and the line's position within it. A line is then interpreted according
to that position:

1. An empty line is a semantic blank line.
2. In an element's initial attribute region, a valid `key=value` line is an
   attribute and an `!!` line is a commented attribute.
3. At ordinary child position, `|>` opens an element, `!!` begins a comment,
   and three initial backticks open a fenced code block.
4. Any other nonempty line is paragraph text.

A beginning-of-line escape suppresses recognition of comment and code-fence
markers and permits content to begin with otherwise structural spaces.

### Elements

An element begins with `|>` followed by a tag name:

```writerly
|> article
```

Whitespace surrounding the name after `|>` is trimmed. A tag name must match:

```text
[A-Za-z_:][-A-Za-z0-9._:]*
```

The result is an ordinary VXML element with the same tag name.
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

The key and value become an ordinary VXML attribute. Leading spaces and tabs
after the `=` are preserved as part of the value, up to a maximum of 100.
More than 100 leading spaces or tabs is a parsing error. Trailing spaces and
tabs are trimmed. Empty values are allowed.

Attribute parsing stops at the first line that is not a valid attribute. If an
element's first text line resembles an attribute, insert a blank line before
it:

```writerly
|> Example

    equation=a+b
```

### Paragraphs and text lines

Consecutive ordinary lines at the same indentation become the `Line` values
of one VXML text node. After removing structural indentation, Writerly
preserves the remaining text, including trailing whitespace.

Leading spaces in text content cannot be written directly because leading
spaces normally express structural indentation. To preserve them, place a
backslash immediately after the line's structural indentation. The backslash
is removed while the spaces following it are retained. The same escape allows
text to begin with `!!` or triple backticks:

```writerly
\    text beginning with spaces
\!! text, not a comment
\``` text, not a code fence
```

Exactly one escape backslash is removed. Further backslashes remain part of
the text. The serializer inserts one escape when required.

### Blank lines

Every empty source line becomes an empty VXML element named
`WriterlyBlankLine`. Blank lines are therefore semantic nodes: adding or
removing one changes the VXML tree. Their meaning, if any, is assigned by later
processing.

### Comments

At ordinary child position, consecutive lines beginning with `!!` become one
`WriterlyComment` VXML element containing a text node. The `!!` marker is
removed from the stored line content:

```writerly
!! first comment line
!! second comment line
```

In the attribute region immediately following an element, an `!!` line is
instead stored as a synthetic VXML attribute. The number of spaces after `!!`
is preserved in its key, up to a maximum of 100. More than 100 spaces is a
parsing error:

```writerly
|> figure
    !! class=temporarily-disabled
    src=figure.svg
```

### Fenced code blocks

A line beginning with three backticks opens a code block. A line containing
three backticks at the same indentation closes it. The result is a
`WriterlyCodeBlock` VXML element:

````writerly
```gleam
pub fn answer() {
  42
}
```
````

The closing fence may have trailing whitespace but no annotation. Lines inside
the block retain their relative indentation. Prefix a content line that would
look like a closing fence with a backslash; parsing removes exactly that one
backslash.

The text immediately following the opening backticks is the info string. It
must not begin with a space. An info string may be followed by structured
annotations written as `&key=value`:

````writerly
```gleam&id=answer&class=example
```
````

Annotation keys follow the attribute-key grammar. Their values preserve leading
spaces and tabs, trim trailing spaces and tabs, and obey the same 100-character
leading-whitespace limit as element attributes; exceeding that limit is a
parsing error. Write `\&` for a literal ampersand and `\\` for a literal
backslash in the info string or an annotation. Parsing and serialization
preserve the distinction between the leading info string and structured
annotations.

### Top-level cardinality

A Writerly source may contain any number of top-level nodes, but parsing it as
one VXML tree requires exactly one non-blank root. `input_lines_to_vxml`
returns `MissingRoot` or `NonUniqueRoot` when that requirement is not met. For
multiple roots, use `string_to_writerlys` or `input_lines_to_writerlys`, then
convert the result to a list of VXML nodes with `writerlys_to_vxmls`.

### Handle grammar

Handles are an official Writerly convention interpreted by editor tooling and
desugaring. The core Writerly parser preserves their source syntax as ordinary
attributes or text.

Handle names and decorators use these character classes:

```text
name interior: Unicode letters, numbers, marks, _ . : - ' ^
name final:    Unicode letters, numbers, marks, _ ' ^
decorator:     # followed by one or more Unicode letters, numbers, marks, _ : ' -
```

A name contains at least one character and cannot end with `.`, `:`, or `-`.
Zero or more decorators may follow it. Decorators are metadata and are not part
of the indexed handle name.

#### Attribute definitions

Within an element's attribute region, `handle` defines a handle. Its value has
one of these forms:

```text
name
name <<value>>
```

The bare form assigns an empty value. In the assigned form, the first `<<` and
final `>>` delimit the value. The value may itself contain `<<` or `>>`.

#### In-text definitions

An in-text definition has one of these forms:

```text
<boundary>name<decorators>##<<
##name<decorators>##<<
```

`<boundary>` is the beginning of text content, a space, `{`, `(`, `[`, or the
position immediately after an active value closer. The boundary is retained.
The second form uses an explicit `##` opener, which is consumed, and may occur
at any position.

The `##<<` terminator must be directly adjacent to the name or final decorator.
Any intervening backslash prevents recognition, regardless of the number of
backslashes.

An in-text definition may carry a same-line value:

```text
name##<<value>>##
```

The value begins after `##<<` and ends before the first active `>>##` on the
same line. Within a confirmed value, all other characters—including spaces,
brackets, pipes, and `##<<`—are value text. If the line contains no active
closer, the definition has an empty value and scanning restarts after `##<<` as
ordinary text; that remaining text may contain another definition.

A run of consecutive backslashes immediately before `>>##` controls whether
the closer is active. An even count, including zero, is active; an odd count
escapes the closer. The escaping backslash is removed from the value. An active
closer is consumed eagerly, and its ending position is a new definition
boundary. Definitions can therefore be chained:

```writerly
##first##<<first value>>##second##<<second value>>##
```

The complete in-text definition grammar applies to fenced code-block content,
but not to opening or closing fence lines.

#### Usages

A usage is `>>name`. Outside fenced code blocks it has no left-boundary
requirement. Usages are not recognized inside code-block content or on fence
lines.

A run of consecutive backslashes immediately before `>>` controls whether the
usage is active. An even count, including zero, is active; an odd count escapes
the usage:

```text
>>name       active
\>>name      escaped
\\>>name     active
\\\>>name    escaped
```

The Writerly parser preserves these backslashes. Removing usage escapes,
materializing definitions, interpreting definition values, and resolving usages
belong to later desugaring stages.

### Example

Given this Writerly source:

```writerly
|> Article
    id=intro
    |> Title
        A short example.
    |> Paragraph
        Writerly text can span
        more than one line.
```

parsing and conversion produce the following serialized VXML:

```vxml
<> Article
  id=intro
  <> Title
    <>
      'A short example.'
  <> Paragraph
    <>
      'Writerly text can span'
      'more than one line.'
```

The in-memory VXML additionally associates a `Blame` value with every element,
attribute, text node, and text line. The serialized form above omits those
values.
