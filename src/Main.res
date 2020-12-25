type rec line = {
  start: point,
  end_: point,
  thickness: option<int>,
}
and point = {
  x: float,
  y: float,
}

module Encode = {
  let point = r => {
    open! Json.Encode
    object_([("x", float(r.x)), ("y", float(r.y))] |> Array.to_list)
  }
  let line = r => {
    open Json.Encode
    object_(
      [
        ("start", point(r.start)),
        ("end", point(r.end_)),
        (
          "thickness",
          switch r.thickness {
          | Some(x) => int(x)
          | None => null
          },
        ),
      ] |> Array.to_list,
    )
  }
}

let point = r => {
  open Js.Json
  Js.Dict.fromArray([("x", number(r.x)), ("y", number(r.y))]) |> object_
}

let line = r => {
  open Js.Json
  Js.Dict.fromArray([
    ("start", point(r.start)),
    ("end", point(r.end_)),
    (
      "thickness",
      switch r.thickness {
      | Some(x) => number(float_of_int(x))
      | None => null
      },
    ),
  ]) |> object_
}

let data = {
  start: {
    x: 1.1,
    y: -0.4,
  },
  end_: {
    x: 5.3,
    y: 3.8,
  },
  thickness: Some(2),
}

let json = data->Encode.line->Js.Json.stringifyWithSpace(4)

Js.log(json)
