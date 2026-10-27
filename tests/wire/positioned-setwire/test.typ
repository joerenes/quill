#set page(width: auto, height: auto, margin: 0pt)
#import "/src/quill.typ": *

// Positioned wire changes on rows that only contain absolutely placed gates
#quantum-circuit(
  gate($H$, x: 1, y: 0), meter(x: 2, y: 0), setwire(2, x: 2, y: 0),
  gate($X$, x: 2, y: 1), setwire(0, x: 3, y: 1),
  setwire(0, x: 0, y: 2), lstick($q$, x: 1, y: 2), setwire(1, x: 1, y: 2), gate($Z$, x: 2, y: 2),
  gate(none, x: 4, y: 2, box: false), 
)

#pagebreak()

// Mixed with sequential changes and stroke changes, ordered by column
#quantum-circuit(
  $H$, 1, setwire(2), 1, $X$, 1, setwire(2, x: 1, y: 0, stroke: blue), setwire(1, x: 4, y: 0), [\ ],
  setwire(1, x: 2, stroke: (dash: "dashed")), $Z$, 1, 1, 1, 1, 1, 
)
