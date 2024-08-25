open Data
open Resolve

exception Has_Signal

let tryValueToData = arg =>
  switch setToOption(arg) {
  | None => None
  | Some(Data(data)) => Some(data)
  | Some(Signal(_)) => raise(Has_Signal)
  }
let signalValueToData = arg => setToOption(arg)->Option.flatMap(x => resolveDataOption(x))

let signalMap = map =>
  try {
    map(tryValueToData)
  } catch {
  | Has_Signal => Single(Signal(Computed(computedValue(() => map(signalValueToData)))))
  }

let mapOne = (value, map) => signalMap(resolve => map(resolve(value)))

let mapTwo = (value1, value2, map) => signalMap(resolve => map(resolve(value1), resolve(value2)))

let mapArray = (values, map) =>
  signalMap(resolve => map(values->Array.map(value => resolve(value))))

let rec tryGetIndex = (value, index) =>
  switch (value, index) {
  | (_, 0) => Error(0)
  | (Single(Data(data)) | Multi(Data(data), _), 1) => Ok(data)
  | (Single(Signal(_)) | Multi(Signal(_), _), _) => raise(Has_Signal)
  | (Multi(_, rest), index) => tryGetIndex(arrayToSet(rest), index - 1)
  | (_, index) => Error(index)
  }
let rec getIndex = (value, index) =>
  switch (value, index) {
  | (_, 0) => Error(0)
  | (Single(Data(data)) | Multi(Data(data), _), 1) => Ok(data)
  | (Single(Signal(signal)), index) => getIndex(signal->get, index)
  | (Multi(Signal(signal), rest), index) =>
    getIndex([...signal->get->setToArray, ...rest]->arrayToSet, index)
  | (Multi(_, rest), index) => getIndex(arrayToSet(rest), index - 1)
  | (_, index) => Error(index)
  }

let rec evaluateBlock = (values, items, context) => {
  let newContext = Dict.copy(context)
  let evalValues =
    values
    ->Dict.toArray
    ->Array.map(((key, code)) => {
      let value = evaluate(code, newContext)
      newContext->Dict.set(key, value)
      (key, value)
    })
    ->Dict.fromArray
  let evalItems = items->Array.map(code => evaluate(code, newContext))
  (evalValues, evalItems, newContext)
}
and evaluateTrigger = ((trigger, test, updates), value, context) => {
  let evalTrigger = evaluate(trigger, context)
  let evalTest = evaluate(test, context)
  let evalUpdates =
    updates->Array.map(((source, target)) => (evaluate(source, context), evaluate(target, context)))
  let isFirst = ref(true)
  Single(
    Signal(
      Computed(
        computedValue(() => {
          resolveDeepValueArray(evalTrigger)->ignore
          untracked(() => {
            if isFirst.contents {
              isFirst.contents = false
            } else if dataToBool(resolveValueOption(evalTest)) {
              batch(
                () => {
                  evalUpdates->Array.forEach(
                    ((source, target)) => {
                      let next =
                        resolveDeepValueOption(source)
                        ->Option.map(dataToSignalData)
                        ->optionToSet
                      resolveValueAtom(target)->Option.mapOr((), set => set(next))
                    },
                  )
                },
              )
            }
          })
          value
        }),
      ),
    ),
  )
}
and evaluate = (code: code, context: Dict.t<value>): value =>
  switch code {
  | #Empty => Empty
  | #Bool(bool) => Single(Data(#Bool(bool)))
  | #Float(float) => Single(Data(#Float(float)))
  | #String(string) => Single(Data(#String(string)))
  | #Atom(initial) => Single(Signal(Atom(atomValue(evaluate(initial, context)))))
  | #Block(values, items) =>
    let (evalValues, evalItems, _) = evaluateBlock(values, items, context)
    Single(Data(#Block(evalValues, evalItems)))
  | #BlockTrigger(values, triggers, items) =>
    let (evalValues, evalItems, newContext) = evaluateBlock(values, items, context)
    triggers->Array.reduce(Single(Data(#Block(evalValues, evalItems))), (value, trigger) =>
      evaluateTrigger(trigger, value, newContext)
    )
  | #Scope(values, items) =>
    let (_, evalItems, _) = evaluateBlock(values, items, context)
    evalItems->Array.flatMap(x => setToArray(x))->arrayToSet
  | #Label(label) =>
    switch context->Dict.get(label) {
    | Some(data) => data
    | None => Empty
    }
  | #Get(block, key) =>
    mapTwo(evaluate(block, context), evaluate(key, context), (block, key) =>
      switch (block, key) {
      | (Some(#Block(values, _)), Some(#String(string))) =>
        switch values->Dict.get(string) {
        | Some(data) => data
        | None => Empty
        }
      | (Some(#Block(_, items)), Some(#Float(float))) if isInteger(float) =>
        try {
          let index = ref(float->Float.toInt)
          optionToSet(
            items->Array.findMap(value =>
              switch tryGetIndex(value, index.contents) {
              | Ok(data) => Some(Data(data))
              | Error(nextIndex) =>
                index.contents = nextIndex
                None
              }
            ),
          )
        } catch {
        | Has_Signal =>
          let index = ref(float->Float.toInt)
          Single(
            Signal(
              Computed(
                computedValue(() =>
                  optionToSet(
                    items->Array.findMap(
                      value =>
                        switch getIndex(value, index.contents) {
                        | Ok(data) => Some(Data(data))
                        | Error(nextIndex) =>
                          index.contents = nextIndex
                          None
                        },
                    ),
                  )
                ),
              ),
            ),
          )
        }
      | _ => Empty
      }
    )
  | #Operation(op, args) =>
    switch (op, args->Array.map(arg => evaluate(arg, context))) {
    | ("?", [test, pass, fail]) => mapOne(test, test => dataToBool(test) ? pass : fail)
    | ("|", [a, b]) => mapOne(a, a => dataToBool(a) ? optionToSet(a->Option.map(x => Data(x))) : b)
    | ("&", [a, b]) => mapOne(a, a => dataToBool(a) ? b : optionToSet(a->Option.map(x => Data(x))))
    | ("=", [a, b]) => mapTwo(a, b, (a, b) => Single(Data(#Bool(dataEqual(a, b)))))
    | ("!=", [a, b]) => mapTwo(a, b, (a, b) => Single(Data(#Bool(!dataEqual(a, b)))))
    | ("-", [a]) =>
      mapOne(a, a =>
        switch a {
        | Some(#Float(float)) => Single(Data(#Float(-.float)))
        | _ => Empty
        }
      )
    | ("!", [a]) => mapOne(a, a => Single(Data(#Bool(!dataToBool(a)))))
    | ("_", args) =>
      mapArray(args, args => Single(
        Data(
          #String(
            Array.join(
              args->Array.filterMap(arg =>
                switch arg {
                | Some(#Float(float)) => Some(float->Float.toString)
                | Some(#String(string)) => Some(string)
                | _ => None
                }
              ),
              "",
            ),
          ),
        ),
      ))
    | (op, [a, b]) =>
      mapTwo(a, b, (a, b) =>
        switch (a, b) {
        | (Some(#Float(a)), Some(#Float(b))) =>
          switch op {
          | "<=" => Single(Data(#Bool(a <= b)))
          | ">=" => Single(Data(#Bool(a >= b)))
          | "<" => Single(Data(#Bool(a < b)))
          | ">" => Single(Data(#Bool(a > b)))
          | "+" => Single(Data(#Float(a +. b)))
          | "-" => Single(Data(#Float(a -. b)))
          | "*" => Single(Data(#Float(a *. b)))
          | "/" => Single(Data(#Float(a /. b)))
          | "%" => Single(Data(#Float(Float.mod(Float.mod(a -. 1.0, b) +. b, b) +. 1.0)))
          | "^" => Single(Data(#Float(Math.pow(a, ~exp=b))))
          | _ => Empty
          }
        | _ => Empty
        }
      )
    | _ => Empty
    }
  | #Unpack(value) =>
    mapOne(evaluate(value, context), value =>
      switch value {
      | Some(#Block(_, items)) => items->Array.flatMap(data => setToArray(data))->arrayToSet
      | _ => Empty
      }
    )
  | #If(test, thenCode) =>
    let evalThen = evaluate(thenCode, context)
    mapOne(evaluate(test, context), test => dataToBool(test) ? evalThen : Empty)
  | #IfElse(test, thenCode, elseCode) =>
    let evalThen = evaluate(thenCode, context)
    let evalElse = evaluate(elseCode, context)
    mapOne(evaluate(test, context), test => dataToBool(test) ? evalThen : evalElse)
  | #For(block, label, body) =>
    mapOne(evaluate(block, context), block =>
      switch block {
      | Some(#Block(_, items)) =>
        items
        ->Array.flatMap(data => {
          let newContext = context->Dict.copy
          newContext->Dict.set(label, data)
          setToArray(evaluate(body, newContext))
        })
        ->arrayToSet
      | _ => Empty
      }
    )
  | #ForIndex(block, label, indexLabel, body) =>
    mapOne(evaluate(block, context), block =>
      switch block {
      | Some(#Block(_, items)) =>
        arrayToSet(
          items->Array.flatMapWithIndex((data, index) => {
            let newContext = context->Dict.copy
            newContext->Dict.set(label, data)
            newContext->Dict.set(indexLabel, Single(Data(#Float(Float.fromInt(index + 1)))))
            setToArray(evaluate(body, newContext))
          }),
        )
      | _ => Empty
      }
    )
  | #Function(labels, body) =>
    Single(
      Data(
        #Func(
          args => {
            let newContext = Dict.assign(
              context->Dict.copy,
              Belt.Array.zip(labels, args)->Dict.fromArray,
            )
            evaluate(body, newContext)
          },
        ),
      ),
    )
  | #Call(func, args) =>
    let evalArgs = args->Array.map(code => evaluate(code, context))
    mapOne(evaluate(func, context), func =>
      switch func {
      | Some(#Func(func)) => func(evalArgs)
      | _ => Empty
      }
    )
  }

// and evaluate = (codeSignal, context) =>
//   switch codeSignal {
//   | CodeData(code) => evaluateCode(code, context)
//   | CodeComputed(getCode) => Single(Signal(computed(() => evaluateCode(getCode(), context))))
//   | CodeBlock(getBlock, _) =>
//     Single(
//       Signal(
//         computed(() => {
//           let (values, items) = getBlock()
//           evaluateCode(#Block(values, items), context)
//         }),
//       ),
//     )
//   | CodeScope(getScope, _) =>
//     Single(
//       Signal(
//         computed(() => {
//           let (values, items) = getScope()
//           evaluateCode(#Scope(values, items), context)
//         }),
//       ),
//     )
//   | CodeScript(getCode, setCode) =>
//     let isString = computed(() =>
//       try {
//         switch Parse.parse(getCode()) {
//         | #String(_) => true
//         | _ => false
//         }
//       } catch {
//       | Parse.Parse_Failed => false
//       }
//     )
//     Single(
//       Signal(
//         computed(() => {
//           if isString->get {
//             Single(
//               Signal(
//                 Atom(
//                   () => Single(Data(#String(getCode()->String.replaceRegExp(%re("/\\{/g"), "{")))),
//                   v =>
//                     switch v {
//                     | Single(Data(#String(string))) =>
//                       setCode(string->String.replaceRegExp(%re("/(?<!\\){/g"), "\\{"))
//                     | _ => ()
//                     },
//                 ),
//               ),
//             )
//           } else {
//             Single(
//               Signal(
//                 computedOption(Single(Data(#String(untracked(getCode)))), () =>
//                   try {
//                     Some(
//                       switch evaluateCode(Parse.parse(getCode()), context) {
//                       | Empty => Single(Data(#String("")))
//                       | value => value
//                       },
//                     )
//                   } catch {
//                   | Parse.Parse_Failed => None
//                   }
//                 ),
//               ),
//             )
//           }
//         }),
//       ),
//     )
//   }
