# writerly_parser

## Intro

"Writerly" is a markup language inspired by Elm-Markup. It is built
on three primitives:

- xml-like nodes with key-value attribute pairs and children, where the children may be selfsame nodes or:
- multi-line text blurbs, or
- code blocks

Indentation is used to indicate parent-child relationships.

Lines of whitespace are used to separate key-value attribute definitions from text blurbs, and to separate
text blurbs from one another.

Here is a sample:

```
|> SomeTag
    attr1 value1
    attr2 value2

    |> Child1
        attr1 here everything after the space is the attribute value, starting with 'here'

        first line of a text blurb
        second line of a text blurb
        etc
        etc

        new
        text
        blurb
        |> ThirdTag

        |> ThirdTag

            child

            beast
  
    |> Child2

    here is a code block, with an annotation next to the opening quote (let's see
    how Markdown deals with the nested triple quotes):

    ```python
    j
     |> FakeTag
      ```python
      lll
    qqq
    ```
```

## Development

```sh
gleam run   # Run the project
```

## What's there

...