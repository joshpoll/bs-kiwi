// Kiwi interface

type solver
type variable

module Strength = {
  type t = float
  @bs.val @bs.module("kiwi") @bs.scope("Strength") external required: t = "required"
  @bs.val @bs.module("kiwi") @bs.scope("Strength") external strong: t = "strong"
  @bs.val @bs.module("kiwi") @bs.scope("Strength") external medium: t = "medium"
  @bs.val @bs.module("kiwi") @bs.scope("Strength") external weak: t = "weak"
}

module Operator = {
  type t

  @bs.val @bs.module("kiwi") @bs.scope("Operator") external le: t = "Le"
  @bs.val @bs.module("kiwi") @bs.scope("Operator") external ge: t = "Ge"
  @bs.val @bs.module("kiwi") @bs.scope("Operator") external eq: t = "Eq"
}

@bs.new @bs.module("kiwi") external mkSolver: unit => solver = "Solver"
@bs.new @bs.module("kiwi") external mkVariable_: unit => variable = "Variable"

@bs.send external addEditVariable: (solver, variable, float) => unit = "addEditVariable"
@bs.send external removeEditVariable: (solver, variable) => unit = "removeEditVariable"
@bs.send external suggestValue: (solver, variable, float) => unit = "suggestValue"

/* TODO: constraint/expr language using operator overloading */
// expressions can be created by plus, minus, multiply, divide

// expressions
type expression

@bs.new @bs.module("kiwi") external mkNumExpression: float => expression = "Expression"
@bs.new @bs.module("kiwi") external mkVarExpression: variable => expression = "Expression"
@bs.send external plus: (expression, expression) => expression = "plus"
@bs.send external minus: (expression, expression) => expression = "minus"
@bs.send external multiply: (expression, float) => expression = "multiply"
@bs.send external divide: (expression, float) => expression = "divide"

// constraints
type constraint_

@bs.new @bs.module("kiwi")
external mkConstraint_: (expression, Operator.t, expression, float) => constraint_ = "Constraint"

let mkConstraint = (~strength=Strength.required, e1, op, e2) => mkConstraint_(e1, op, e2, strength)

@bs.send external addConstraint: (solver, constraint_) => unit = "addConstraint"
@bs.send external hasConstraint: (solver, constraint_) => bool = "hasConstraint"
@bs.send external removeConstraint: (solver, constraint_) => unit = "removeConstraint"
@bs.send external updateVariables: (solver, unit) => unit = "updateVariables"
@bs.send external value: (variable, unit) => float = "value"
@bs.send external setName: (variable, string) => unit = "setName"
@bs.send external name: (variable, unit) => string = "name"

let mkVariable = (~name=?, ()) => {
  let v = mkVariable_()
  switch name {
  | None => ()
  | Some(name) => v->setName(name)
  }
  v
}

// We expose a stay interface because Kiwi didn't! TODO: Does this match Cassowary's?
let mkStay = (~strength=Strength.weak, v: variable) =>
  mkConstraint(~strength, mkVarExpression(v), Operator.eq, mkNumExpression(v->value()))

// module Ops = {
//   // expression combinators
//   let (+) = plus;
//   let (-) = minus;
//   let ( * ) = multiply;
//   let (/) = divide;

//   // constraint comparisons
//   let (<=) = (e1, e2) => mkConstraint(e1, Operator.le, e2);
//   let (>=) = (e1, e2) => mkConstraint(e1, Operator.ge, e2);
//   let (==) = (e1, e2) => mkConstraint(e1, Operator.eq, e2);
// };
