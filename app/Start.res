open Docs
open Data
open Resolve

let s = (strings, _) => strings[0]->Option.getOr("")->Parse.parse
let l = items => #Block(Dict.make(), items)

let code = #Scope(
  d({
    "selected": #Atom(#Block(Dict.make(), [])),
    "paragraphs": s`{getParagraphs(selected)}`,
  }),
  [
    #Block(
      d({
        "font": s`Atkinson Hyperlegible, sans-serif`,
        "color": s`#333`,
        "size": s`17`,
        "gap": s`30`,
        "pad": s`50`,
        "fill": s`#fafaed`,
        "minHeight": s`100%`,
      }),
      [
        #Block(
          d({"bold": s`yes`, "flow": s`row`, "gap": s`10`}),
          [
            #BlockTrigger(
              d({"click": #Atom(#Empty)}),
              [(s`{click}`, s`{yes}`, [(#Block(Dict.make(), []), s`{selected}`)])],
              [s`All`],
            ),
            #ForIndex(
              s`{selected}`,
              "label",
              "index",
              #Scope(
                Dict.make(),
                [
                  s`▸`,
                  #BlockTrigger(
                    d({"click": #Atom(#Empty)}),
                    [(s`{click}`, s`{yes}`, [(s`{slice(selected, 1, index)}`, s`{selected}`)])],
                    [s`{label}`],
                  ),
                ],
              ),
            ),
          ],
        ),
        #IfElse(
          s`{paragraphs}`,
          #Block(
            d({"gap": s`25`, "maxWidth": s`670`}),
            [
              #For(
                s`{paragraphs}`,
                "paragraph",
                #Block(
                  d({
                    "size": s`{paragraph.type = 'title' ? 25 - paragraph.level * 2 : 17}`,
                    "bold": s`{paragraph.type = 'quote' | (paragraph.type = 'title' & paragraph.level <= 2)}`,
                    "italic": s`{paragraph.type = 'info' | (paragraph.type = 'title' & paragraph.level > 2)}`,
                    "uppercase": s`{paragraph.type = 'call' | (paragraph.type = 'title' & paragraph.level = 1)}`,
                    "align": s`{paragraph.type = 'section' ? 'center' : paragraph.type = 'info' | paragraph.type = 'call' ? 'justify-center' : 'left'}`,
                    "pad": l([
                      s`{paragraph.type = 'title' & paragraph.level = 1 ? 20 : 0}`,
                      s`{
                        paragraph.type = 'quote' ? 20 :
                        paragraph.type = 'info' | paragraph.type = 'call' ? 40 :
                        paragraph.type = 'lines' ? 70 :
                        paragraph.type = 'title' & paragraph.level > 2 ? (paragraph.level - 2) * 20 :
                        0
                      }`,
                      s`0`,
                    ]),
                    "indent": s`{paragraph.type = '' ? 20 : paragraph.type = 'lines' ? -30 : 0}`,
                    "gap": s`8.5`,
                    "whitespace": s`pre-wrap`,
                  }),
                  [s`{...paragraph.text}`],
                ),
              ),
            ],
          ),
          #Block(
            d({"gap": s`20`}),
            [
              #For(
                s`{getList(selected)}`,
                "label",
                #BlockTrigger(
                  d({"click": #Atom(#Empty)}),
                  [
                    (
                      s`{click}`,
                      s`{yes}`,
                      [(#Block(Dict.make(), [s`{...selected}`, s`{label}`]), s`{selected}`)],
                    ),
                  ],
                  [s`{label}`],
                ),
              ),
            ],
          ),
        ),
      ],

      // [
      //   #Block(
      //     d({
      //       "bold": s`yes`,
      //       "gap": s`10`,
      //     }),
      //     [
      //       #String(doc.author),
      //       #String(doc.path->Array.join(", ")),
      //       #String(
      //         doc.title->Option.getOr(doc.item->Option.mapOr("", item => item->Int.toString)),
      //       ),
      //     ],
      //   ),
      //   doc.paragraphs->renderParagraphs,
      // ],
    ),
  ],
)

// "maxWidth": s`670`,

let f = map => Single(
  Data(
    #Func(
      args => Single(
        Signal(Computed(computedValue(() => map(args->Array.map(resolveValueOption))))),
      ),
    ),
  ),
)

Render.render(
  Evaluate.evaluate(
    code,
    d({
      "slice": f(args =>
        switch args {
        | [Some(#Block(_, items)), Some(#Float(start)), Some(#Float(end))]
          if isInteger(start) && isInteger(end) =>
          Single(
            Data(
              #Block(
                Dict.make(),
                items->Array.slice(~start=start->Float.toInt - 1, ~end=end->Float.toInt),
              ),
            ),
          )
        | _ => Empty
        }
      ),
      "getParagraphs": f(args =>
        switch args {
        | [Some(#Block(_, selected))] =>
          getParagraphs(
            selected->Array.filterMap(item =>
              switch resolveValueOption(item) {
              | Some(#String(string)) => Some(string)
              | _ => None
              }
            ),
          )->Option.mapOr(Empty, paragraphs => Single(
            Data(
              #Block(
                Dict.make(),
                paragraphs->Array.map(
                  ((paraType, text, level)) => Single(
                    Data(
                      #Block(
                        d({
                          "type": Single(Data(#String(paraType))),
                          "text": Single(
                            Data(
                              #Block(Dict.make(), text->Array.map(t => Single(Data(#String(t))))),
                            ),
                          ),
                          "level": Single(Data(#Float(level->Int.toFloat))),
                        }),
                        [],
                      ),
                    ),
                  ),
                ),
              ),
            ),
          ))
        | _ => Empty
        }
      ),
      "getList": f(args =>
        switch args {
        | [Some(#Block(_, selected))] =>
          Single(
            Data(
              l(
                getList(
                  selected->Array.filterMap(item =>
                    switch resolveValueOption(item) {
                    | Some(#String(string)) => Some(string)
                    | _ => None
                    }
                  ),
                )->Array.map(collection => Single(Data(#String(collection)))),
              ),
            ),
          )
        | _ => Empty
        }
      ),
    }),
  ),
)->ignore
