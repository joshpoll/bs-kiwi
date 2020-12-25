// Defines Kiwi Json encoding

type varOpt =
  | Suggest(float, Kiwi.Strength.t)
  // TODO: maybe stays should be inferred? a local change requires a suggest and derives.
  | Stay(Kiwi.Strength.t)
  // TODO: get rid of derived variables entirely? only really needed if we want to specify the
  // entire problem every time and/or if we need to keep track of all the variables
  | Derived

type variable = {
  id: string,
  varOpt: varOpt,
}

type rec expr =
  | Num(float)
  | Var(string)
  | Add(expr, expr)
  | Sub(expr, expr)
  | Mul(expr, float)
  | Div(expr, float)

type operator =
  | Le
  | Ge
  | Eq

type constraint_ = {
  lhs: expr,
  op: operator,
  rhs: expr,
  strength: Kiwi.Strength.t,
}

type variables = array<variable>
type constraints = array<constraint_>

type system = {
  variables: variables,
  constraints: constraints,
}

module Encode = {
  let varOpt = r => {
    open! Json.Encode
    switch r {
    | Suggest(value, strength) =>
      object_(
        [
          ("opt", string("suggest")),
          ("value", float(value)),
          ("strength", float(strength)),
        ] |> Array.to_list,
      )
    | Stay(strength) =>
      object_([("opt", string("stay")), ("strength", float(strength))] |> Array.to_list)
    | Derived => object_([("opt", string("derived"))] |> Array.to_list)
    }
  }

  let variable = r => {
    open! Json.Encode
    object_([("id", string(r.id)), ("varOpt", varOpt(r.varOpt))] |> Array.to_list)
  }

  let rec expr = r => {
    open! Json.Encode
    switch r {
    | Num(x) => object_([("tag", string("num")), ("value", float(x))] |> Array.to_list)
    | Var(v) => object_([("tag", string("var")), ("value", string(v))] |> Array.to_list)
    | Add(e1, e2) =>
      object_([("tag", string("+")), ("e1", expr(e1)), ("e2", expr(e2))] |> Array.to_list)
    | Sub(e1, e2) =>
      object_([("tag", string("-")), ("e1", expr(e1)), ("e2", expr(e2))] |> Array.to_list)
    | Mul(e1, f2) =>
      object_([("tag", string("*")), ("e1", expr(e1)), ("f2", float(f2))] |> Array.to_list)
    | Div(e1, f2) =>
      object_([("tag", string("/")), ("e1", expr(e1)), ("f2", float(f2))] |> Array.to_list)
    }
  }

  let operator = r => {
    open! Json.Encode
    switch r {
    | Le => string("<=")
    | Ge => string(">=")
    | Eq => string("==")
    }
  }

  let constraint_ = r => {
    open! Json.Encode
    object_(
      [
        ("lhs", expr(r.lhs)),
        ("op", operator(r.op)),
        ("rhs", expr(r.rhs)),
        ("strength", float(r.strength)),
      ] |> Array.to_list,
    )
  }

  let variables = Json.Encode.array(variable)

  let constraints = Json.Encode.array(constraint_)

  let system = r => {
    open! Json.Encode
    object_(
      [
        ("variables", variables(r.variables)),
        ("constraints", constraints(r.constraints)),
      ] |> Array.to_list,
    )
  }
}
