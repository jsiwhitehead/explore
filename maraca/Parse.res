open Data

type g
type s
type m

@module("ohm-js")
external makeGrammar: string => g = "grammar"

@send external createSemantics: g => s = "createSemantics"
@send external match: (g, string) => m = "match"

@send external addAttribute: (s, string, 'a) => unit = "addAttribute"

@send external succeeded: m => bool = "succeeded"
@get external message: m => string = "message"

@get external ast: 'a => 'b = "ast"
@get external sourceString: 'a => string = "sourceString"

let grammar = makeGrammar(`Script {

  start
    = space* (s_value | chunk)* space*

  value
    = ternary

  ternary
    = or space* "?" space* or space* ":" space* ternary -- ternary
    | or

  or
    = or space* "|" space* and -- or
    | and

  and
    = and space* "&" space* equal -- and
    | equal

  equal
    = equal space* ("!=" | "=") space* compare -- equal
    | compare

  compare
    = compare space* ("<=" | ">=" | "<" | ">") space* sum -- compare
    | sum

  sum
    = sum space* ("+" | "-") space+ product -- sum
    | product

  product
    = product space* ("*" | "/" | "%") space* power -- product
    | power

  power
    = power space* "^" space* unary -- power
    | unary

  unary
    = ("-" | "!") space* unary -- unary
    | "..." space* unary -- unpack
    | apply

  apply
    = apply "." label -- key
    | apply "[" space* value space* "]" -- get
    | apply "(" space* items space* ")" -- call
    | atom

  items
    = listOf<value, separator> space* ","?

  atom
    = string | number | boolean | label | brackets

  chunk
    = (char | escape)+

  char
    = ~("\\\\" | "{") any

  string
    = "'" (s_value | s_chunk)* "'"

  s_value
    = "{" space* value space* "}"

  s_chunk
    = (s_char | escape)+

  s_char
    = ~("'" | "\\\\" | "{") any

  escape
    = "\\\\" any

  number
    = digit+ ("." digit+)?

  boolean
    = ("yes" | "no") ~alnum

  label
    = alnum+

  brackets
    = "(" space* value space* ")"

  separator
    = space* "," space*
    | (linespace* "\\n")+ linespace*

  linespace
    = ~"\\n" "\\x00".."\\x20"

}`)

let semantics = createSemantics(grammar)

let binary = (a, _, b, _, c) => #Operation(b->sourceString, [a->ast, c->ast])

semantics->addAttribute(
  "ast",
  {
    "start": (_, a, _) =>
      if Array.length(a->ast) <= 1 {
        switch (a->ast)[0] {
        | Some(x) => x
        | None => #String("")
        }
      } else {
        #Operation("_", a->ast)
      },
    "ternary_ternary": (a, _, _, _, b, _, _, _, c) => #Operation("?", [a->ast, b->ast, c->ast]),
    "or_or": binary,
    "and_and": binary,
    "equal_equal": binary,
    "compare_compare": binary,
    "sum_sum": binary,
    "product_product": binary,
    "power_power": binary,
    "unary_unary": (a, _, b) => #Operation(a->sourceString, [b->ast]),
    "unary_unpack": (_, _, a) => #Unpack(a->ast),
    "apply_key": (a, _, b) => #Get(a->ast, #String(b->sourceString)),
    "apply_get": (a, _, _, b, _, _) => #Get(a->ast, b->ast),
    "apply_call": (a, _, _, b, _, _) => #Call(a->ast, b->ast),
    "items": (a, _, _) => a->ast,
    "chunk": a => #String(String.replaceRegExp(a->sourceString, %re("/\\(.)/g"), "$1")),
    "string": (_, a, _) =>
      if Array.length(a->ast) <= 1 {
        switch (a->ast)[0] {
        | Some(x) => x
        | None => #String("")
        }
      } else {
        #Operation("_", a->ast)
      },
    "s_value": (_, _, a, _, _) => a->ast,
    "s_chunk": a => #String(String.replaceRegExp(a->sourceString, %re("/\\(.)/g"), "$1")),
    "number": (a, b, c) =>
      #Float(Float.parseFloat(`${a->sourceString}${b->sourceString}${c->sourceString}`)),
    "boolean": a => #Bool(a->sourceString === "yes"),
    "label": a => #Label(a->sourceString),
    "brackets": (_, _, a, _, _) => a->ast,
    "nonemptyListOf": (a, _, b) => [a->ast, ...b->ast],
    "_iter": %raw(`(...children) => children.map((c) => c.ast)`),
    "_terminal": () => None,
  },
)

exception Parse_Failed

let parse = (script: string): code => {
  let m = grammar->match(script)
  if m->succeeded {
    %raw(`semantics(m).ast`)
  } else {
    Console.log(m->message)
    raise(Parse_Failed)
  }
}
