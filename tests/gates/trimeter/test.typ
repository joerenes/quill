#set page(width: auto, height: auto, margin: 0pt)
#import "/src/quill.typ": *

#quantum-circuit(
  1, trimeter(), setwire(2), 1, [\ ],
  1, trimeter($Z$), setwire(2), 1, [\ ],
  1, trimeter($X$, min-height: 0pt), setwire(2), 1, [\ ],
  1, trimeter(apex-angle: 70deg, fill: yellow, stroke: blue, label: "a"), 1
)

#pagebreak()

#quantum-circuit(..tequila.build(
  tequila.h(0), tequila.cx(0, 1), tequila.barrier(),
  tequila.trimeter((0, 1)), tequila.trimeter(2, $Z$, output-wire: none),
))
