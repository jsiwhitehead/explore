open Data

let filterMapDict = (dict, map) =>
  dict
  ->Dict.toArray
  ->Array.filterMap(((key, value)) => map(value)->Option.map(v => (key, v)))
  ->Dict.fromArray

let rec resolveValueOption = (value: value): option<semiData> =>
  switch setToOption(value) {
  | None => None
  | Some(data) => resolveDataOption(data)
  }
and resolveDataOption = (data: signalData): option<semiData> =>
  switch data {
  | Data(data) => Some(data)
  | Signal(signal) => resolveValueOption(signal->get)
  }

let rec resolveValueArray = (value: value): array<semiData> =>
  value
  ->setToArray
  ->Array.flatMap(data =>
    switch data {
    | Data(data) => [data]
    | Signal(signal) => resolveValueArray(signal->get)
    }
  )
let resolveItemsArray = (items: array<value>): array<value> =>
  items
  ->Array.flatMap(data => resolveValueArray(data))
  ->Array.map(data => Single(Data(data)))

let rec resolveDeepValueOption = (value: value): option<data> =>
  switch setToOption(value) {
  | None => None
  | Some(data) => resolveDeepDataOption(data)
  }
and resolveDeepDataOption = (data: signalData): option<data> =>
  switch data {
  | Data(data) => Some(resolveDeepSemi(data))
  | Signal(signal) => resolveDeepValueOption(signal->get)
  }
and resolveDeepValueArray = (value: value): array<data> =>
  value->setToArray->Array.flatMap(data => resolveDeepDataArray(data))
and resolveDeepDataArray = (data: signalData): array<data> =>
  switch data {
  | Data(data) => [resolveDeepSemi(data)]
  | Signal(signal) => resolveDeepValueArray(signal->get)
  }
and resolveDeepSemi = (data: semiData): data =>
  switch data {
  | #Bool(bool) => #Bool(bool)
  | #Float(bool) => #Float(bool)
  | #String(bool) => #String(bool)
  | #Block(values, items) =>
    #Block(
      values->filterMapDict(data => resolveDeepValueOption(data)),
      items->Array.flatMap(data => resolveDeepValueArray(data)),
    )
  | #Func(_) => #Bool(false)
  }

let rec resolveSignalAtom = (signal: signal<value>): option<value => unit> =>
  switch signal {
  | Atom(_, set) => Some(set)
  | Computed(get) => resolveValueAtom(get())
  }
and resolveValueAtom = (value: value): option<value => unit> =>
  switch setToOption(value) {
  | None => None
  | Some(data) => resolveDataAtom(data)
  }
and resolveDataAtom = (data: signalData): option<value => unit> =>
  switch data {
  | Data(_) => None
  | Signal(signal) => resolveSignalAtom(signal)
  }
