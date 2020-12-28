{
  open KiwiInterface
  let test = {variables: [{id: "foo", varOpt: Derived}], constraints: []}
  Js.log(test->KiwiInterface.Encode.system->Js.Json.stringifyWithSpace(4))
}

{
  open KiwiInterface
  let test = {variables: [{id: "foo", varOpt: Derived}], constraints: [{
    lhs: Var("foo"),
    op: Eq,
    rhs: Num(5.),
    strength: Kiwi.Strength.required
  }]}
  Js.log(test->KiwiInterface.Encode.system->Js.Json.stringifyWithSpace(4))
  Js.log(test->KiwiInterface.solve->Belt.HashMap.String.toArray)
}
