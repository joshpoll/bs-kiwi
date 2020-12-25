{
  open KiwiInterface
  let test = {variables: [{id: "foo", varOpt: Derived}], constraints: []}
  Js.log(test->KiwiInterface.Encode.system->Js.Json.stringifyWithSpace(4))
}