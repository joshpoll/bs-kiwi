open Kiwi

// https://stackoverflow.com/a/58268653
// Create an "Id" module, to basically give the comparison function a type
module VariableComparable = Belt.Id.MakeComparable({
  type t = variable
  let cmp = (c1, c2) => String.compare(c1->name(), c2->name())
})

// Define an alias for the set type
type variableSet = Belt.Set.t<variable, VariableComparable.identity>
type variableMap<'a> = Belt.Map.t<variable, 'a, VariableComparable.identity>

type variableOption =
  | Suggest(float, Strength.t)
  // TODO: maybe stays should be inferred? a local change requires a suggest and derives.
  | Stay(Strength.t)
  // TODO: get rid of derived variables entirely? only really needed if we want to specify the
  // entire problem every time and/or if we need to keep track of all the variables
  | Derived

type solver = (Kiwi.solver, list<constraint_>)

let mkSolver = (): solver => {
  let solver = mkSolver()
  (solver, list{})
}

let solve = (
  declarativeSolver: solver,
  ~variables: variableMap<variableOption>,
  ~constraints: list<constraint_>,
): solver => {
  let (solver, oldConstraints) = declarativeSolver
  // diff constraints
  // remove old constraints
  List.fold_left((_, c) =>
    if !List.exists(x => x == c, constraints) {
      solver->removeConstraint(c)
    }
  , (), oldConstraints)

  // add new constraints
  List.fold_left((_, c) =>
    if !(solver->hasConstraint(c)) {
      solver->addConstraint(c)
    }
  , (), constraints)

  Js.log2("variables", variables->Belt.Map.toArray)

  // Add variable suggests and stays
  let editVars = ref(list{})
  let stays = ref(list{})
  Belt.Map.forEach(variables, (var, vo) =>
    switch vo {
    | Suggest(value, strength) =>
      solver->addEditVariable(var, strength)
      editVars := list{var, ...editVars.contents}
      solver->suggestValue(var, value)
    | Stay(strength) =>
      let stayConstraint = mkStay(~strength, var)
      solver->addConstraint(stayConstraint)
      stays := list{stayConstraint, ...stays.contents}
    | Derived => ()
    }
  )

  // solve!
  solver->updateVariables()

  // remove stay constraints
  List.fold_left((_, s) => solver->removeConstraint(s), (), stays.contents)
  // remove edit variables
  List.fold_left((_, v) => solver->removeEditVariable(v), (), editVars.contents)

  (solver, constraints)
}
