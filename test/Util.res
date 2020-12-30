@bs.val @bs.module("fs")
external readStdInAsUtf8Sync: (@bs.as(0) _, @bs.as("utf8") _) => string = "readFileSync"

// TODO: I think there's some way to improve these so they use options or are completely optional.
type dirOpts = {
  depth: Js.Nullable.t<int>,
  maxArrayLength: Js.Nullable.t<int>,
  colors: bool,
}

@bs.val @bs.scope("console")
external dir: ('a, dirOpts) => unit = "dir"
