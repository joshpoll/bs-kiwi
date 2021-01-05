// Like Kiwi, but adds max and min. Only allowed at top-level of expressions for now, but that could
// change.

// type varOpt =
//   | Suggest(float, Kiwi.Strength.t)
//   // TODO: maybe stays should be inferred? a local change requires a suggest and derives.
//   | Stay(Kiwi.Strength.t)
//   // TODO: get rid of derived variables entirely? only really needed if we want to specify the
//   // entire problem every time and/or if we need to keep track of all the variables
//   | Derived

type variable = {
  id: string,
  varOpt: KiwiDeclarative.variableOption,
}

type rec aexpr =
  | Num(float)
  | Var(string)
  | Add(aexpr, aexpr)
  | Sub(aexpr, aexpr)
  | Mul(aexpr, float)
  | Div(aexpr, float)

type cexpr =
  | Max(array<aexpr>)
  | Min(array<aexpr>)

type expr =
  | AExpr(aexpr)
  | CExpr(cexpr)

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
  open! Json.Encode

  let varOpt = r =>
    switch r {
    | KiwiDeclarative.Suggest(value, strength) =>
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

  let variable = r => object_([("id", string(r.id)), ("varOpt", varOpt(r.varOpt))] |> Array.to_list)

  let rec aexpr = r =>
    switch r {
    | Num(x) => object_([("tag", string("num")), ("value", float(x))] |> Array.to_list)
    | Var(v) => object_([("tag", string("var")), ("value", string(v))] |> Array.to_list)
    | Add(ae1, ae2) =>
      object_([("tag", string("+")), ("ae1", aexpr(ae1)), ("ae2", aexpr(ae2))] |> Array.to_list)
    | Sub(ae1, ae2) =>
      object_([("tag", string("-")), ("ae1", aexpr(ae1)), ("ae2", aexpr(ae2))] |> Array.to_list)
    | Mul(ae1, f2) =>
      object_([("tag", string("*")), ("ae1", aexpr(ae1)), ("f2", float(f2))] |> Array.to_list)
    | Div(ae1, f2) =>
      object_([("tag", string("/")), ("ae1", aexpr(ae1)), ("f2", float(f2))] |> Array.to_list)
    }

  let cexpr = r =>
    switch r {
    | Max(aes) => object_([("tag", string("max")), ("aes", array(aexpr, aes))] |> Array.to_list)
    | Min(aes) => object_([("tag", string("min")), ("aes", array(aexpr, aes))] |> Array.to_list)
    }

  let expr = r =>
    switch r {
    | AExpr(ae) => object_([("tag", string("aexpr")), ("value", aexpr(ae))] |> Array.to_list)
    | CExpr(ce) => object_([("tag", string("cexpr")), ("value", cexpr(ce))] |> Array.to_list)
    }

  let operator = r =>
    switch r {
    | Le => string("<=")
    | Ge => string(">=")
    | Eq => string("==")
    }

  let constraint_ = r =>
    object_(
      [
        ("lhs", expr(r.lhs)),
        ("op", operator(r.op)),
        ("rhs", expr(r.rhs)),
        ("strength", float(r.strength)),
      ] |> Array.to_list,
    )

  let variables = Json.Encode.array(variable)

  let constraints = Json.Encode.array(constraint_)

  let system = r =>
    object_(
      [
        ("variables", variables(r.variables)),
        ("constraints", constraints(r.constraints)),
      ] |> Array.to_list,
    )
}
module Decode = {
  open! Json.Decode

  exception DecodeError

  let strengthName = json =>
    switch string(json) {
    | "required" => Kiwi.Strength.required
    | "strong" => Kiwi.Strength.strong
    | "medium" => Kiwi.Strength.medium
    | "weak" => Kiwi.Strength.weak
    | _ => raise(DecodeError)
    }

  let strengthNum = json => float(json)

  let strength = either(strengthName, strengthNum)

  let varOpt = json =>
    switch json |> field("opt", string) {
    | "suggest" =>
      KiwiDeclarative.Suggest(json |> field("value", float), json |> field("strength", strength))
    | "stay" => Stay(json |> field("strength", strength))
    | "derived" => Derived
    | _ => raise(DecodeError)
    }

  let variable = json => {
    id: json |> field("id", string),
    varOpt: json |> field("varOpt", varOpt),
  }

  let rec aexpr = json =>
    switch json |> field("tag", string) {
    | "num" => Num(json |> field("value", float))
    | "var" => Var(json |> field("value", string))
    | "+" => Add(json |> field("ae1", aexpr), json |> field("ae2", aexpr))
    | "-" => Sub(json |> field("ae1", aexpr), json |> field("ae2", aexpr))
    | "*" => Mul(json |> field("ae1", aexpr), json |> field("f2", float))
    | "/" => Div(json |> field("ae1", aexpr), json |> field("f2", float))
    | _ => raise(DecodeError)
    }

  let cexpr = json =>
    switch json |> field("tag", string) {
    | "max" => Max(json |> field("aes", array(aexpr)))
    | "min" => Min(json |> field("aes", array(aexpr)))
    | _ => raise(DecodeError)
    }

  let expr = json =>
    switch json |> field("tag", string) {
    | "aexpr" => AExpr(json |> field("value", aexpr))
    | "cexpr" => CExpr(json |> field("value", cexpr))
    | _ => raise(DecodeError)
    }

  let operator = json =>
    switch string(json) {
    | "<=" | "le" => Le
    | ">=" | "ge" => Ge
    | "==" | "eq" => Eq
    | _ => raise(DecodeError)
    }

  let constraint_ = json => {
    lhs: json |> field("lhs", expr),
    op: json |> field("op", operator),
    rhs: json |> field("rhs", expr),
    strength: json |> field("strength", strength),
  }

  let variables = Json.Decode.array(variable)

  let constraints = Json.Decode.array(constraint_)

  let system = json => {
    variables: json |> field("variables", variables),
    constraints: json |> field("constraints", constraints),
  }
}

let unzip3List = xs =>
  List.fold_right(
    ((a, b, c), (as_, bs, cs)) => (list{a, ...as_}, list{b, ...bs}, list{c, ...cs}),
    xs,
    (list{}, list{}, list{}),
  )

let unzip3 = xs => {
  let (as_, bs, cs) = xs->Array.to_list->unzip3List
  (as_->Array.of_list, bs->Array.of_list, cs->Array.of_list)
}

module Lower = {
  let counter = ref(0)

  let genFresh = () => {
    counter := counter.contents + 1
    string_of_int(counter.contents - 1)
  }
  let aexpr = ae => {
    let rec aexprAux = ae => {
      switch ae {
      | Num(x) => KiwiInterface.Num(x)
      | Var(vid) => Var(vid)
      | Add(ae1, ae2) => Add(aexprAux(ae1), aexprAux(ae2))
      | Sub(ae1, ae2) => Sub(aexprAux(ae1), aexprAux(ae2))
      | Mul(ae1, f2) => Mul(aexprAux(ae1), f2)
      | Div(ae1, f2) => Div(aexprAux(ae1), f2)
      }
    }
    (aexprAux(ae), [], [])
  }

  // The interesting bits: lowering max and min.
  let cexpr = ce => {
    let newVarName = switch ce {
    | Max(_) => "max_" ++ genFresh()
    | Min(_) => "min_" ++ genFresh()
    }
    let newVarExpr = Var(newVarName)
    let newVar = {
      id: newVarName,
      varOpt: Derived,
    }
    switch ce {
    | Max(aes) =>
      // throw out fields we know will be empty. TODO would be nice to have a monad here!
      let (max, _, _) = aexpr(newVarExpr)
      let (aes, _, _) = Belt.Array.map(aes, aexpr)->unzip3
      let boundConstraints = aes->Belt.Array.map(ae => {
          KiwiInterface.lhs: ae,
          op: Le,
          rhs: max,
          strength: Kiwi.Strength.required,
        })
      let tightConstraints = aes->Belt.Array.map(ae => {
          KiwiInterface.lhs: ae,
          op: Eq,
          rhs: max,
          strength: Kiwi.Strength.weak,
        })
      (
        max,
        [newVar],
        Belt.Array.concat(boundConstraints, tightConstraints),
      )
    | Min(aes) =>
      let (min, _, _) = aexpr(newVarExpr)
      let (aes, _, _) = Belt.Array.map(aes, aexpr)->unzip3
      let boundConstraints = aes->Belt.Array.map(ae => {
          KiwiInterface.lhs: ae,
          op: Ge,
          rhs: min,
          strength: Kiwi.Strength.required,
        })
      let tightConstraints = aes->Belt.Array.map(ae => {
          KiwiInterface.lhs: ae,
          op: Eq,
          rhs: min,
          strength: Kiwi.Strength.weak,
        })
      (
        min,
        [newVar],
        Belt.Array.concat(boundConstraints, tightConstraints),
      )
    }
  }

  let expr = e =>
    switch e {
    | AExpr(ae) => aexpr(ae)
    | CExpr(ce) => cexpr(ce)
    }

  let operator = op =>
    switch op {
    | Le => KiwiInterface.Le
    | Ge => KiwiInterface.Ge
    | Eq => KiwiInterface.Eq
    }

  let variable = ({id, varOpt}) => {KiwiInterface.id: id, varOpt: varOpt}

  let constraint_ = ({lhs, op, rhs, strength}): (
    array<variable>,
    array<KiwiInterface.constraint_>,
  ) => {
    let (loweredLHS, lhsVariables, lhsConstraints) = expr(lhs)
    let (loweredRHS, rhsVariables, rhsConstraints) = expr(rhs)
    (
      Array.append(lhsVariables, rhsVariables),
      [
        {
          KiwiInterface.lhs: loweredLHS,
          op: operator(op),
          rhs: loweredRHS,
          strength: strength,
        },
      ]
      ->Array.append(lhsConstraints)
      ->Array.append(rhsConstraints),
    )
  }

  let system = ({variables, constraints}) => {
    // TODO: convert constraints, possibly creating new variables and constraints in the process
    open! Belt
    let (constraintVars, loweredConstraints) = constraints->Array.map(constraint_)->Array.unzip
    let (constraintVars, loweredConstraints) = (
      Array.concatMany(constraintVars),
      Array.concatMany(loweredConstraints),
    )

    {
      KiwiInterface.variables: Array.concat(variables, constraintVars)->Array.map(variable),
      constraints: loweredConstraints,
    }
  }
}

// // takes an input spec, creates a new solver, solves the problem, and returns variable map. NOT INCREMENTAL (yet)!
// let solve = ({variables, constraints}) => {
//   open! Belt
//   // build variables and auxiliary map for building constraints
//   let varIdMap = HashMap.String.make(~hintSize=Array.length(variables))
//   let kiwiDeclarativeVars = ref(Map.make(~id=module(KiwiDeclarative.VariableComparable)))

//   variables->Array.forEach(v => {
//     let var = Kiwi.mkVariable(~name=v.id, ())
//     varIdMap->HashMap.String.set(v.id, var)
//     kiwiDeclarativeVars := kiwiDeclarativeVars.contents->Map.set(var, v.varOpt)
//   })

//   // build constraints
//   let kiwiDeclarativeConstraints = constraints->Array.map(constraint_(varIdMap))->Array.concatMany

//   // build solver and solve!
//   let _ =
//     KiwiDeclarative.mkSolver()->KiwiDeclarative.solve(
//       ~variables=kiwiDeclarativeVars.contents,
//       ~constraints=kiwiDeclarativeConstraints,
//     )

//   // extract solutions
//   let varSolutions = varIdMap->HashMap.String.copy
//   varSolutions
//   ->HashMap.String.toArray
//   ->Array.map(((vid, var)) => (vid, var->Kiwi.value()))
//   ->HashMap.String.fromArray
// }
