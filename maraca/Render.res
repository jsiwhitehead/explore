open Data

type node
type event
type nodeType = Text | Input | Wrap | Block

@val external getElementById: string => node = "document.getElementById"

@get external nodeType: node => nodeType = "__type"
@get external childNodes: node => array<node> = "childNodes"
@get external value: node => string = "value"

@send external stopPropagation: event => unit = "stopPropagation"
@get external target: event => node = "target"
@get external eventKey: event => string = "key"

let dictMap = (dict, map) =>
  dict->Dict.toArray->Array.map(((k, v)) => (k, map(k, v)))->Dict.fromArray

let dictKeepSome = dict =>
  dict->Dict.toArray->Array.filterMap(((k, v)) => v->Option.map(x => (k, x)))->Dict.fromArray

let rec resolveValueKeyed = (value: value, key: string): array<(string, semiData)> =>
  value
  ->setToArray
  ->Array.flatMapWithIndex((data, index) => {
    let newKey = `${key}.${index->Int.toString}`
    switch data {
    | Data(data) => [(newKey, data)]
    | Signal(signal) => resolveValueKeyed(signal->get, newKey)
    }
  })
let resolveItemsKeyed = (items: array<value>): array<(string, semiData)> =>
  items->Array.flatMapWithIndex((data, index) => resolveValueKeyed(data, index->Int.toString))

type context = {size: float, line: float, inline: [#inline | #wrap | #none]}

type props = {
  onmouseover: option<event => unit>,
  onmouseleave: option<event => unit>,
  onclick: option<event => unit>,
  onfocus: option<event => unit>,
  onblur: option<event => unit>,
  focus: bool,
  onkeydown: option<event => unit>,
  value: option<string>,
  oninput: option<event => unit>,
}
type style = {
  display: string,
  flexDirection: string,
  width: string,
  marginTop: string,
  marginBottom: string,
  marginLeft: string,
  marginRight: string,
  height: string,
  minHeight: string,
  maxWidth: string,
  gap: string,
  fontFamily: string,
  fontSize: string,
  lineHeight: string,
  fontWeight: string,
  fontStyle: string,
  textDecoration: string,
  textTransform: string,
  textAlign: string,
  textAlignLast: string,
  textIndent: string,
  whiteSpace: string,
  color: string,
  padding: string,
  background: string,
  borderTop: string,
  borderLeft: string,
  borderBottom: string,
  borderRight: string,
  cursor: string,
}

let createNode: (string, nodeType) => node = %raw(`(key, nodeType) => {
  let res;
  if (nodeType === "Text") {
    res = document.createTextNode("");
  } else if (nodeType === "Input") {
    res = document.createElement("div");
    res.replaceChildren(document.createElement("input"));
  } else if (nodeType === "Wrap") {
    res = document.createElement("div");
    res.replaceChildren(document.createElement("div"));
  } else if (nodeType === "Block") {
    res = document.createElement("div");
  }
  res.__type = nodeType;
  res.__key = key;
  return res;
}`)

let childNodesKeyed: node => Dict.t<node> = %raw(`(node) => {
  const inner = ["Input", "Wrap"].includes(node.__type) ? node.childNodes[0] : node;
  return [...inner.childNodes].reduce((res, c) => ({ ...res, [c.__key]: c }), {});
}`)

let updateTextNode: (node, string) => unit = %raw(`(node, text) => {
  node.textContent = text.normalize("NFD").replace(/\u0323/g, "").normalize("NFC");
}`)

let updateNodeProps: (node, props, style) => unit = %raw(`(node, props, style) => {
  const focusNode = node.__type === "Input" ? node.childNodes[0] : node;
  for (const key in props) {
    if (key === "focus") {
      if (props[key]) setTimeout(() => focusNode.focus());
    } else if (["onfocus", "onblur", "value" , "oninput"].includes(key)) {
      focusNode[key] = props[key] ?? null;
    } else {
      node[key] = props[key] ?? null;
    }
  }
  if (node.__type === "Input") node.childNodes[0].readOnly = !props.oninput;
  const marginNode = ["Input", "Wrap"].includes(node.__type) ? node.childNodes[0] : node;
  for (const key in style) {
    if (["marginTop", "marginBottom", "minHeight"].includes(key)) {
      marginNode.style[key] = style[key] ?? null;
    } else {
      node.style[key] = style[key] ?? null;
    }
  }
}`)

let updateChildren: (node, array<node>) => unit = %raw(`(node, nextChildren) => {
  const inner = ["Input", "Wrap"].includes(node.__type) ? node.childNodes[0] : node;
  const prevChildren = Array.from(inner.childNodes);
  const prevMap = prevChildren.reduce((res, c) => ({ ...res, [c.__key]: true }), {});
  const nextMap = nextChildren.reduce((res, c) => ({ ...res, [c.__key]: true }), {});
  prevChildren.forEach(prev => {
    if (!nextMap[prev.__key]) inner.removeChild(prev);
  });
  nextChildren.forEach((next, i) => {
    if (!prevMap[next.__key]) {
      const after = nextChildren.slice(i).find(c => prevMap[c.__key]);
      if (after) inner.insertBefore(after, next);
      else inner.appendChild(next);
    }
  });
}`)

let getNextNode = (key, prevNode, nextType) =>
  prevNode
  ->Option.flatMap(n => n->nodeType === nextType ? Some(n) : None)
  ->Option.getOr(createNode(key, nextType))

let atomKeys = ["hover", "click", "focus", "key", "input"]

type renderInfo = {flow: option<string>, gap: option<float>, input: bool}
type renderData =
  | String(string)
  | Block(Dict.t<Data.value>, array<Data.value>)

let resolveToRenderData = items =>
  resolveItemsKeyed(items)->Array.map(((key, data)) => (
    key,
    switch data {
    | #Bool(bool) => String(bool ? "Yes" : "No")
    | #Float(float) => String(Float.toString(float))
    | #String(string) => String(string)
    | #Func(_) => String("<< Function >>")
    | #Block(values, items) => Block(values, items)
    },
  ))

let getBool = (values, key) =>
  values
  ->Dict.get(key)
  ->Option.flatMap(data =>
    switch data {
    | #Bool(bool) => Some(bool)
    | #String(string) => Some(string !== "")
    | _ => None
    }
  )
let getFloat = (values, key) =>
  values
  ->Dict.get(key)
  ->Option.flatMap(data =>
    switch data {
    | #Float(float) => Some(float)
    | #String(string) => string->Float.fromString
    | _ => None
    }
  )
let getString = (values, key) =>
  values
  ->Dict.get(key)
  ->Option.flatMap(data =>
    switch data {
    | #String(string) => Some(string)
    | _ => None
    }
  )
let getStringFloat = (values, key) =>
  values
  ->Dict.get(key)
  ->Option.flatMap(data =>
    switch data {
    | #Float(float) => Some(float->Float.toString)
    | #String(string) => Some(string)
    | _ => None
    }
  )

let getItems = (flow, gap, items) => {
  if flow->Option.getOr(#String("inline")) != #String("inline") || gap != None {
    items->Array.map(((key, item)) => (
      key,
      switch item {
      | String(string) => Block(Dict.make(), [Single(Data(#String(string)))])
      | Block(_, _) => item
      },
    ))
  } else {
    items
  }
}

let getInline = (prevInline, flow, items) =>
  switch prevInline {
  | #wrap | #inline => #inline
  | #none =>
    flow == Some(#String("inline")) ||
      items->Array.some(((_, v)) =>
        switch v {
        | String(_) => true
        | Block(_, _) => false
        }
      )
      ? #wrap
      : #none
  }

let getContext = (prevContext, values, items) => {
  size: getFloat(values, "size")->Option.getOr(prevContext.size),
  line: getFloat(values, "line")->Option.getOr(prevContext.line),
  inline: switch prevContext.inline {
  | #wrap | #inline => #inline
  | #none =>
    getString(values, "flow") == Some("inline") ||
      items->Array.some(((_, v)) =>
        switch v {
        | String(_) => true
        | Block(_, _) => false
        }
      )
      ? #wrap
      : #none
  },
}

let valuesToProps = (values, atoms) => {
  onmouseover: atoms
  ->Dict.get("hover")
  ->Option.map(hover => _ => hover(Single(Data(#Bool(true))))),
  onmouseleave: atoms
  ->Dict.get("hover")
  ->Option.map(hover => _ => hover(Data.Empty)),
  onclick: atoms
  ->Dict.get("click")
  ->Option.map(click => event => {
    click(Single(Data(#Block(Dict.make(), []))))
    event->stopPropagation
  }),
  onfocus: atoms
  ->Dict.get("focus")
  ->Option.map(focus => _ => focus(Single(Data(#Bool(true))))),
  onblur: atoms
  ->Dict.get("focus")
  ->Option.map(focus => _ => focus(Data.Empty)),
  focus: getBool(values, "isFocus")->Option.getOr(false),
  onkeydown: atoms
  ->Dict.get("key")
  ->Option.map(key => event => {
    key(Single(Data(#Block(d({"key": Single(Data(#String(event->eventKey)))}), []))))
    event->stopPropagation
  }),
  value: getStringFloat(values, "input"),
  oninput: atoms
  ->Dict.get("input")
  ->Option.map(input => event => input(Single(Data(#String(event->target->value))))),
}

let blockToDirections = (values: Dict.t<Data.data>, items: array<Data.data>) => [
  values->Dict.get("top")->Option.orElse(items[0]->Option.orElse(None)),
  values
  ->Dict.get("right")
  ->Option.orElse(items[3]->Option.orElse(items[1]->Option.orElse(items[0]->Option.orElse(None)))),
  values
  ->Dict.get("bottom")
  ->Option.orElse(items[2]->Option.orElse(items[0]->Option.orElse(None))),
  values
  ->Dict.get("left")
  ->Option.orElse(items[1]->Option.orElse(items[0]->Option.orElse(None))),
]

let valuesToStyle = (values, atoms, context) => {
  let (textAlign, textAlignLast) = getString(values, "align")->Option.mapOr(
    ("inherit", "inherit"),
    align => {
      if align->String.includes("justify") {
        ("justify", align->String.sliceToEnd(~start=8))
      } else {
        (align, "none")
      }
    },
  )
  let lineGap = (context.line -. 1.) *. context.size /. 2.
  let isInput = values->Dict.get("input")->Option.isSome
  let borderDirections =
    values
    ->Dict.get("border")
    ->Option.mapOr(None, v =>
      switch v {
      | #String(string) => Some([string, string, string, string])
      | #Block(values, items) =>
        Some(
          blockToDirections(values, items)->Array.map(v =>
            switch v {
            | Some(#String(string)) => string
            | _ => "none"
            }
          ),
        )
      | _ => None
      }
    )
  {
    display: switch context.inline {
    | #inline => "inline"
    | #wrap => "block"
    | #none => "flex"
    },
    flexDirection: context.inline === #none
      ? getString(values, "flow")->Option.getOr("column")
      : "",
    width: getString(values, "width")->Option.getOr(context.inline === #none ? "100%" : ""),
    marginTop: context.inline === #wrap || isInput ? `-${Float.toString(lineGap)}px` : "",
    marginBottom: context.inline === #wrap || isInput ? `-${Float.toString(lineGap)}px` : "",
    marginLeft: values->Dict.get("maxWidth")->Option.isSome ? "auto" : "0",
    marginRight: values->Dict.get("maxWidth")->Option.isSome ? "auto" : "0",
    height: getString(values, "height")->Option.getOr("auto"),
    minHeight: getString(values, "minHeight")->Option.getOr(
      context.inline === #wrap || isInput
        ? `${Float.toString(context.line *. context.size)}px`
        : "",
    ),
    maxWidth: getFloat(values, "maxWidth")->Option.mapOr("auto", v => `${v->Float.toString}px`),
    gap: `${getFloat(values, "gap")->Option.getOr(0.)->Float.toString}px`,
    fontFamily: getString(values, "font")->Option.getOr("inherit"),
    fontSize: `${context.size->Float.toString}px`,
    lineHeight: context.line->Float.toString,
    fontWeight: getBool(values, "bold")->Option.mapOr("inherit", v => v ? "bold" : "normal"),
    fontStyle: getBool(values, "italic")->Option.mapOr("inherit", v => v ? "italic" : "normal"),
    textDecoration: getBool(values, "underline")->Option.mapOr("inherit", v =>
      v ? "underline" : "inherit"
    ),
    textTransform: getBool(values, "uppercase")->Option.mapOr("inherit", v =>
      v ? "uppercase" : "inherit"
    ),
    textAlign,
    textAlignLast,
    textIndent: getFloat(values, "indent")->Option.mapOr("inherit", v => `${v->Float.toString}px`),
    whiteSpace: getString(values, "whitespace")->Option.getOr("inherit"),
    color: getString(values, "color")->Option.getOr("inherit"),
    padding: values
    ->Dict.get("pad")
    ->Option.mapOr("", v =>
      switch v {
      | #Float(float) => `${float->Float.toString}px`
      | #String(string) => `${string}px`
      | #Block(values, items) =>
        blockToDirections(values, items)
        ->Array.map(v =>
          switch v {
          | Some(#Float(float)) => `${Float.toString(float)}px`
          | Some(#String(string)) => `${string}px`
          | _ => "0px"
          }
        )
        ->Array.join(" ")
      | _ => ""
      }
    ),
    background: getString(values, "fill")->Option.getOr(""),
    borderTop: borderDirections->Option.flatMap(v => v[0])->Option.getOr("none"),
    borderLeft: borderDirections->Option.flatMap(v => v[1])->Option.getOr("none"),
    borderBottom: borderDirections->Option.flatMap(v => v[2])->Option.getOr("none"),
    borderRight: borderDirections->Option.flatMap(v => v[3])->Option.getOr("none"),
    cursor: atoms->Dict.get("click")->Option.isSome ? "pointer" : "normal",
  }
}

let rec updateNode = (key, node, data, context) =>
  switch data {
  | String(text) =>
    let res = getNextNode(key, node, Text)
    updateTextNode(res, text)
    (res, () => ())
  | Block(values, items) =>
    let isInput =
      values
      ->Dict.get("input")
      ->Option.flatMap(Resolve.resolveValueAtom)
      ->Option.isSome
    let flow = values->Dict.get("flow")->Option.flatMap(Resolve.resolveDeepValueOption)
    let gap = values->Dict.get("gap")->Option.flatMap(Resolve.resolveDeepValueOption)
    let renderItems = getItems(flow, gap, (isInput ? [] : items)->resolveToRenderData)
    let nextInline = getInline(context.inline, flow, renderItems)
    let res = getNextNode(key, node, isInput ? Input : nextInline == #wrap ? Wrap : Block)
    let cleanup = Data.effectWithCleanup(() => {
      let atoms =
        values
        ->dictMap((key, value) =>
          atomKeys->Array.includes(key) ? Resolve.resolveValueAtom(value) : None
        )
        ->dictKeepSome
      let resValues =
        values
        ->dictMap((key, value) =>
          !(atomKeys->Array.includes(key)) || key === "input"
            ? Resolve.resolveDeepValueOption(value)
            : None
        )
        ->dictKeepSome
      let nextContext = getContext(context, resValues, renderItems)
      updateNodeProps(
        res,
        valuesToProps(resValues, atoms),
        valuesToStyle(resValues, atoms, nextContext),
      )
      if isInput {
        () => ()
      } else {
        Data.effectWithCleanup(() => updateItems(res, renderItems, nextContext))
      }
    })
    (res, cleanup)
  }
and updateItems = (node, items, context) => {
  let keyed = node->childNodesKeyed
  let pairs = items->Array.map(((k, d)) => updateNode(k, keyed->Dict.get(k), d, context))
  updateChildren(node, pairs->Array.map(((n, _)) => n))
  () => pairs->Array.forEach(((_, dispose)) => dispose())
}

let root = getElementById("app")

let render = value =>
  Data.effectWithCleanup(() =>
    updateItems(root, resolveToRenderData([value]), {size: 16., line: 1.5, inline: #none})
  )
