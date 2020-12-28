{
  open KiwiInterface
  let test = {variables: [{id: "foo", varOpt: Derived}], constraints: []}
  // Js.log(test->KiwiInterface.Encode.system->Js.Json.stringifyWithSpace(4))
}

{
  open KiwiInterface
  let test = {
    variables: [{id: "foo", varOpt: Derived}],
    constraints: [
      {
        lhs: Var("foo"),
        op: Eq,
        rhs: Num(5.),
        strength: Kiwi.Strength.required,
      },
    ],
  }
  // Js.log(test->KiwiInterface.Encode.system->Js.Json.stringifyWithSpace(4))
  // Js.log(test->KiwiInterface.solve->Belt.HashMap.String.toArray)
}

// Simple Max Encoding
{
  open KiwiInterface
  let test = {
    variables: [
      {id: "a", varOpt: Suggest(2., Kiwi.Strength.strong)},
      {id: "b", varOpt: Suggest(3., Kiwi.Strength.strong)},
      {id: "c", varOpt: Suggest(5., Kiwi.Strength.strong)},
      {id: "max", varOpt: Derived},
    ],
    constraints: [
      {
        lhs: Var("a"),
        op: Le,
        rhs: Var("max"),
        strength: Kiwi.Strength.required,
      },
      {
        lhs: Var("b"),
        op: Le,
        rhs: Var("max"),
        strength: Kiwi.Strength.required,
      },
      {
        lhs: Var("c"),
        op: Le,
        rhs: Var("max"),
        strength: Kiwi.Strength.required,
      },
    ],
  }
  Js.log(test->KiwiInterface.Encode.system->Js.Json.stringifyWithSpace(4))
  Js.log(test->KiwiInterface.solve->Belt.HashMap.String.toArray)
}

// Complex Max Encoding. Should hopefully ensure max is correct more often
{
  open KiwiInterface
  let test = {
    variables: [
      {id: "a", varOpt: Suggest(2., Kiwi.Strength.strong)},
      {id: "b", varOpt: Suggest(3., Kiwi.Strength.strong)},
      {id: "c", varOpt: Suggest(5., Kiwi.Strength.strong)},
      {id: "s_a", varOpt: Suggest(0., Kiwi.Strength.medium)},
      {id: "s_b", varOpt: Suggest(0., Kiwi.Strength.medium)},
      {id: "s_c", varOpt: Suggest(0., Kiwi.Strength.medium)},
      {id: "max", varOpt: Derived},
    ],
    constraints: [
      {
        lhs: Var("s_a"),
        op: Ge,
        rhs: Num(0.),
        strength: Kiwi.Strength.required,
      },
      {
        lhs: Var("s_b"),
        op: Ge,
        rhs: Num(0.),
        strength: Kiwi.Strength.required,
      },
      {
        lhs: Var("s_c"),
        op: Ge,
        rhs: Num(0.),
        strength: Kiwi.Strength.required,
      },
      {
        lhs: Add(Var("a"), Var("s_a")),
        op: Eq,
        rhs: Var("max"),
        strength: Kiwi.Strength.required,
      },
      {
        lhs: Add(Var("b"), Var("s_b")),
        op: Eq,
        rhs: Var("max"),
        strength: Kiwi.Strength.required,
      },
      {
        lhs: Add(Var("c"), Var("s_c")),
        op: Eq,
        rhs: Var("max"),
        strength: Kiwi.Strength.required,
      },
    ],
  }
  Js.log(test->KiwiInterface.Encode.system->Js.Json.stringifyWithSpace(4))
  Js.log(test->KiwiInterface.solve->Belt.HashMap.String.toArray)
}
