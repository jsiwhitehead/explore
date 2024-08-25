// SIGNALS

type signalBase<'value>
type equalCheck<'value> = ('value, 'value) => bool

type atom<'value> = (unit => 'value, 'value => unit)
type computed<'value> = unit => 'value

@set external setBase: (signalBase<'value>, 'value) => unit = "value"
@get external getBase: signalBase<'value> => 'value = "value"

@module("./Signals.mjs")
external atomBase: ('value, equalCheck<'value>) => signalBase<'value> = "signal"
@module("./Signals.mjs")
external computedBase: (unit => 'value, equalCheck<'value>) => signalBase<'value> = "computed"

@module("./Signals.mjs")
external effect: (unit => unit) => unit = "effect"
@module("./Signals.mjs")
external effectWithCleanup: (unit => unit => unit) => unit => unit = "effect"
@module("./Signals.mjs")
external batch: (unit => unit) => unit = "batch"
@module("./Signals.mjs")
external untracked: (unit => 'value) => 'value = "untracked"

let atom = (initial, isEqual) => {
  let base = atomBase(initial, isEqual)
  (() => base->getBase, value => base->setBase(value))
}

let computed = (func, isEqual) => {
  let base = computedBase(func, isEqual)
  () => base->getBase
}

// TYPES

type primitive = [
  | #Bool(bool)
  | #Float(float)
  | #String(string)
]

type baseData<'t> = [
  | primitive
  | #Block(Dict.t<'t>, array<'t>)
]

type set<'t> =
  | Empty
  | Single('t)
  | Multi('t, array<'t>)

type rec data = baseData<data>

type rec value = set<signalData>
and signal<'t> = Atom(atom<'t>) | Computed(computed<'t>)
and semiData = [baseData<value> | #Func(array<value> => value)]
and signalData =
  | Data(semiData)
  | Signal(signal<value>)

type rec trigger = (code, code, array<(code, code)>)
and code = [
  | #Empty
  | baseData<code>
  | #Atom(code)
  | #BlockTrigger(Dict.t<code>, array<trigger>, array<code>)
  | #Scope(Dict.t<code>, array<code>)
  | #Label(string)
  | #Get(code, code)
  | #Operation(string, array<code>)
  | #Unpack(code)
  | #If(code, code)
  | #IfElse(code, code, code)
  | #For(code, string, code)
  | #ForIndex(code, string, string, code)
  | #Function(array<string>, code)
  | #Call(code, array<code>)
]

// and codeSignal =
//   | CodeData(code)
//   | CodeComputed(unit => code)
//   | CodeBlock(
//       unit => (Dict.t<codeSignal>, array<codeSignal>),
//       ((Dict.t<codeSignal>, array<codeSignal>)) => unit,
//     )
//   | CodeScope(
//       unit => (Dict.t<codeSignal>, array<codeSignal>),
//       ((Dict.t<codeSignal>, array<codeSignal>)) => unit,
//     )
//   | CodeScript(unit => string, string => unit)

// HELPERS

@val external isInteger: float => bool = "Number.isInteger"

// let isNumeric: string => bool = %raw(`(str) => {
//   if (typeof str != "string") return false;
//   return !isNaN(str) && !isNaN(parseFloat(str))
// }`)

external d: {..} => Js.Dict.t<'a> = "%identity"

let setToOption = set =>
  switch set {
  | Empty => None
  | Single(value) => Some(value)
  | Multi(first, _) => Some(first)
  }
let optionToSet = option =>
  switch option {
  | None => Empty
  | Some(value) => Single(value)
  }

let setToArray = set =>
  switch set {
  | Empty => []
  | Single(value) => [value]
  | Multi(first, rest) => [first, ...rest]
  }
let arrayToSet = array =>
  switch (array[0], array->Array.sliceToEnd(~start=1)) {
  | (None, _) => Empty
  | (Some(value), []) => Single(value)
  | (Some(first), rest) => Multi(first, rest)
  }

let dataToBool = (data: option<semiData>) =>
  switch data {
  | None | Some(#Bool(false)) => false
  | _ => true
  }

let dataEqual = (a: option<semiData>, b: option<semiData>) =>
  switch (a, b) {
  | (Some(a), Some(b)) =>
    switch (a, b) {
    | (#Block(_, _), #Block(_, _)) => false
    | (a, b) => a == b
    }
  | _ => false
  }

let valueEqual = (a: value, b: value) =>
  switch (a, b) {
  | (Empty, Empty) => true
  | (Single(Data(a)), Single(Data(b))) => dataEqual(Some(a), Some(b))
  | _ => false
  }

let atomValue = initial => atom(initial, valueEqual)

let computedValue = func => computed(func, valueEqual)

let get = signal =>
  switch signal {
  | Atom(get, _) => get()
  | Computed(get) => get()
  }

let rec dataToAtoms = (data: data): signalData => Signal(
  Atom(
    atomValue(
      Single(
        Data(
          switch data {
          | #Bool(bool) => #Bool(bool)
          | #Float(float) => #Float(float)
          | #String(string) => #String(string)
          | #Block(values, items) =>
            #Block(
              values->Dict.mapValues(data => Single(dataToAtoms(data))),
              items->Array.map(data => Single(dataToAtoms(data))),
            )
          },
        ),
      ),
    ),
  ),
)
and dataToSignalData = (data: data): signalData => Data(
  switch data {
  | #Bool(bool) => #Bool(bool)
  | #Float(float) => #Float(float)
  | #String(string) => #String(string)
  | #Block(values, items) =>
    #Block(
      values->Dict.mapValues(data => Single(dataToAtoms(data))),
      items->Array.map(data => Single(dataToAtoms(data))),
    )
  },
)
