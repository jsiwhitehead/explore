open Data

@module({from: "../data/data.json", with: {type_: "json"}})
external jsonDocs: JSON.t = "default"

// %%raw(`
// console.log(JSON.stringify([...new Set(jsonDocs.map(d => d.author))], null, 2));
// `)

let authors = [
  "The Báb",
  "Bahá’u’lláh",
  "‘Abdu’l‑Bahá",
  "Shoghi Effendi",
  "The Universal House of Justice",
  "The International Teaching Centre",
  "The World Centre",
  "Compilation",
  "The Ruhi Institute",
  "John E. Esslemont",
]

// TYPES

type reference = {doc: int, paragraph: int, start: int, end: int}
type part =
  | String(string)
  | Quote(reference)
type paragraph =
  | Section(array<int>)
  | Title(array<int>, string)
  | Quote(array<part>)
  | Info(array<part>, array<reference>)
  | Call(array<part>, array<reference>)
  | Lines(array<int>, array<part>, array<reference>)
  | Paragraph(int, array<part>, array<reference>)
type document = {
  id: string,
  prayer: bool,
  author: string,
  path: array<string>,
  item: option<int>,
  title: option<string>,
  breadcrumb: array<string>,
  years: (float, float),
  source: option<string>,
  paragraphs: array<JSON.t>,
}

// HELPERS

let unique = array => Set.fromArray(array)->Set.values->Core__Iterator.toArray

let decodeInt = (json: JSON.t) =>
  switch json {
  | Number(number) if number->isInteger => number->Float.toInt
  | _ => raise(Not_found)
  }
let decodeFloat = (json: JSON.t) =>
  switch json {
  | Number(number) => number
  | _ => raise(Not_found)
  }
let decodeString = (json: JSON.t) =>
  switch json {
  | String(string) => string
  | _ => raise(Not_found)
  }
let decodeArray = (json: JSON.t, decode) =>
  switch json {
  | Array(array) => array->Array.map(decode)
  | _ => raise(Not_found)
  }
let decodeArraySafe = (json: option<JSON.t>, decode) =>
  switch json {
  | Some(Array(array)) => array->Array.map(decode)
  | _ => []
  }

// DECODE INFO

let decodeDoc = (json: JSON.t) =>
  switch json {
  | Object(dict) => {
      let author = dict->Dict.get("author")->Option.getExn->decodeString
      let path = dict->Dict.get("path")->Option.getExn->decodeArray(decodeString)
      let item = dict->Dict.get("item")->Option.map(decodeInt)
      let title = dict->Dict.get("title")->Option.map(decodeString)
      {
        id: dict->Dict.get("id")->Option.getExn->decodeString,
        prayer: dict->Dict.get("type")->Option.map(decodeString) == Some("Prayer"),
        author,
        path,
        item,
        title,
        breadcrumb: [
          Some(
            [
              "Commissioned by the Universal House of Justice",
              "The Research Department",
              "The Office of Social and Economic Development",
              "Bahá’í International Community",
            ]->Array.includes(author)
              ? "The World Centre"
              : author,
          ),
          ...path->Array.map(p => Some(p)),
          title->Option.orElse(item->Option.map(item => item->Int.toString)),
        ]->Array.keepSome,
        years: switch dict->Dict.get("years")->Option.getExn {
        | Array([start, end]) => (start->decodeFloat, end->decodeFloat)
        | _ => raise(Not_found)
        },
        source: dict->Dict.get("source")->Option.map(decodeString),
        paragraphs: dict->Dict.get("paragraphs")->Option.getExn->decodeArray(x => x),
      }
    }
  | _ => raise(Not_found)
  }

let docsArray = switch jsonDocs {
| Array(array) => array->Array.map(decodeDoc)
| _ => []
}

// DECODE PARAGRAPH

let decodeReference = (json: JSON.t) =>
  switch json {
  | Object(dict) => {
      doc: dict->Dict.get("doc")->Option.getExn->decodeInt,
      paragraph: dict->Dict.get("paragraph")->Option.getExn->decodeInt,
      start: dict->Dict.get("start")->Option.getExn->decodeInt,
      end: dict->Dict.get("end")->Option.getExn->decodeInt,
    }
  | _ => raise(Not_found)
  }

let decodeParts = (json: JSON.t) =>
  json->decodeArray(item =>
    switch item {
    | String(string) => String(string)
    | Object(dict) => Quote(decodeReference(Object(dict)))
    | _ => raise(Not_found)
    }
  )

let decodeParagraph = (json: JSON.t) =>
  switch json {
  | Object(dict) =>
    switch (
      dict->Dict.get("section"),
      dict->Dict.get("quote"),
      dict->Dict.get("type"),
      dict->Dict.get("lines"),
    ) {
    | (Some(Array(section)), None, None, None) =>
      switch dict->Dict.get("title") {
      | Some(String(string)) => Title(section->Array.map(decodeInt), string)
      | _ => Section(section->Array.map(decodeInt))
      }
    | (None, Some(Boolean(true)), _, _) =>
      Quote(
        dict
        ->Dict.get("parts")
        ->Option.getExn
        ->decodeParts,
      )
    | (None, None, Some(String("info")), None) =>
      Info(
        dict
        ->Dict.get("parts")
        ->Option.getExn
        ->decodeParts,
        dict
        ->Dict.get("citations")
        ->decodeArraySafe(decodeReference),
      )
    | (None, None, Some(String("call")), None) =>
      Call(
        dict
        ->Dict.get("parts")
        ->Option.getExn
        ->decodeParts,
        dict
        ->Dict.get("citations")
        ->decodeArraySafe(decodeReference),
      )
    | (None, None, None, Some(Array(lines))) =>
      Lines(
        lines->Array.map(decodeInt),
        dict
        ->Dict.get("parts")
        ->Option.getExn
        ->decodeParts,
        dict
        ->Dict.get("citations")
        ->decodeArraySafe(decodeReference),
      )
    | (None, None, None, None) =>
      Paragraph(
        dict
        ->Dict.get("index")
        ->Option.getExn
        ->decodeInt,
        dict
        ->Dict.get("parts")
        ->Option.getExn
        ->decodeParts,
        dict
        ->Dict.get("citations")
        ->decodeArraySafe(decodeReference),
      )
    | _ => raise(Not_found)
    }
  | _ => raise(Not_found)
  }

// GET REFERENCE TEXT

let rec partsToText = parts =>
  parts
  ->Array.map(part =>
    switch part {
    | String(string) => string
    | Quote(quote) => quote->getReference
    }
  )
  ->Array.join("")

and paragraphToText = paragraph =>
  switch paragraph {
  | Section(_) => "* * *"
  | Title(_, title) => title
  | Quote(parts) => parts->partsToText
  | Info(parts, _) => parts->partsToText
  | Call(parts, _) => parts->partsToText
  | Lines(_, parts, _) => parts->partsToText
  | Paragraph(_, parts, _) => parts->partsToText
  }

and getReference = reference =>
  (docsArray->Array.getUnsafe(reference.doc)).paragraphs
  ->Array.getUnsafe(reference.paragraph)
  ->decodeParagraph
  ->paragraphToText
  ->String.slice(~start=reference.start, ~end=reference.end)

let paragraphToRender = paragraph =>
  switch paragraph {
  | Section(_) => ("section", ["* * *"], 0)
  | Title(section, title) => ("title", [title], section->Array.length)
  | Quote(parts) => ("quote", [parts->partsToText], 0)
  | Info(parts, _) => ("info", [parts->partsToText], 0)
  | Call(parts, _) => ("call", [parts->partsToText], 0)
  | Lines(lines, parts, _) => {
      let text = parts->partsToText
      (
        "lines",
        lines
        ->Array.sliceToEnd(~start=1)
        ->Array.mapWithIndex((_, i) =>
          text->String.slice(~start=lines->Array.getUnsafe(i), ~end=lines->Array.getUnsafe(i + 1))
        ),
        0,
      )
    }
  | Paragraph(index, parts, _) => (index == 1 ? "first" : "", [parts->partsToText], 0)
  }

// API

// let getDoc = id => docsArray->Array.find(doc => doc.id == id)

let getParagraphs = selected =>
  selected->Array.length > 0
    ? docsArray
      ->Array.find(doc => doc.breadcrumb == selected)
      ->Option.map(doc =>
        doc.paragraphs->Array.map(json => json->decodeParagraph->paragraphToRender)
      )
    : None

let getList = selected =>
  selected->Array.length == 0
    ? authors
    : docsArray
      ->Array.filter(doc =>
        !doc.prayer &&
        doc.breadcrumb->Array.slice(~start=0, ~end=selected->Array.length) == selected
      )
      ->Array.filterMap(doc => doc.breadcrumb->Array.get(selected->Array.length))
      ->unique
      // ->Array.filter(collection => !(collection->String.includes("Bahá’í Prayers")))
