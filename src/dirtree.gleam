import gleam/option.{type Option, None, Some}
import gleam/list
import gleam/string
import gleam/result

pub type DirectoryOrFile {
  DirectoryOrFile(
    name: String,
    contents: List(DirectoryOrFile),
  )
}

fn directory_contents_internal(
  previous: List(DirectoryOrFile),
  under_construction: Option(#(String, List(String))),
  remaining: List(String),
) -> List(DirectoryOrFile) {
  case remaining, under_construction {
    [], None -> previous |> list.reverse
    [], Some(#(name, files)) -> {
      let constructed = DirectoryOrFile(
        name: name,
        contents: directory_contents_internal([], None, files |> list.reverse),
      )
      [constructed, ..previous] |> list.reverse
    }
    [first, ..rest], None -> {
      let under_construction = case string.split_once(first, "/") |> result.unwrap(#(first, "")) {
        #(dirname, path) if path != "" -> Some(#(dirname, [path]))
        #(dirname, _) -> Some(#(dirname, []))
      }
      directory_contents_internal(previous, under_construction, rest)
    }
    [first, ..rest], Some(#(name, files)) -> {
      case string.split_once(first, "/") |> result.unwrap(#(first, "")) {
        #(dirname, path) if dirname == name -> {
          let assert True = path != ""
          directory_contents_internal(previous, Some(#(name, [path, ..files])), rest)
        }
        #(dirname, path) -> {
          let constructed = DirectoryOrFile(
            name: name,
            contents: directory_contents_internal([], None, files |> list.reverse),
          )
          let under_construction = case path == "" {
            True -> Some(#(dirname, []))
            False -> Some(#(dirname, [path]))
          }
          directory_contents_internal([constructed, ..previous], under_construction, rest)
        }
      }
    }
  }
}

pub fn directory_tree_from_dir_and_paths(
  dirname: String,
  local_paths: List(String),
  sort: Bool,
) {
  let local_paths = case sort {
    False -> local_paths
    True -> list.sort(local_paths, string.compare)
  }
  |> list.filter(fn(s){!string.is_empty(s)})
  DirectoryOrFile(
    dirname,
    directory_contents_internal([], None, local_paths),
  )
}

fn directory_pretty_printer_add_margin(
  lines: List(String),
  is_last: Bool,
) -> List(String) {
  let t = "├─ "
  let b = "│  "
  let l = "└─ "
  let s = "   "
  case is_last {
    False -> list.index_map(
      lines,
      fn (line, i) {
        case i == 0 {
          True -> t <> line
          False -> b <> line
        }
      }
    )
    True -> list.index_map(
      lines,
      fn (line, i) {
        case i == 0 {
          True -> l <> line
          False -> s <> line
        }
      }
    )
  }
}

pub fn pretty_printer(dir: DirectoryOrFile) -> List(String) {
  let num_children = list.length(dir.contents)
  let xtra_margin = case string.reverse(dir.name) |> string.split_once("/") {
    Ok(#(_, after)) -> string.length(after) + 1
    _ -> 0
  }
  let xtra_margin = string.repeat(" ", xtra_margin)
  list.index_map(
    dir.contents,
    fn (child, i) {
      pretty_printer(child)
      |> directory_pretty_printer_add_margin(i == num_children - 1)
      |> list.map(fn(line){xtra_margin <> line})
    }
  )
  |> list.flatten
  |> list.prepend(dir.name)
}
