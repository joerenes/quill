#set page(width: auto, height: auto, margin: 0pt)
#import "/src/quill.typ": *

#quantum-circuit(
  1, $H$, ground(), setwire(0), 1, [\ ],
  1, ground(size: 14pt, spacing: 2pt, stroke: blue), setwire(0), 2, [\ ],
  4
)

#pagebreak()

#quantum-circuit(..tequila.build(
  tequila.h(0), tequila.cx(0, 1), tequila.ground(0), tequila.x(1), tequila.ground(2)
))
