#set page(width: auto, height: auto, margin: 0pt)
#import "/src/quill.typ" as quill: tequila as tq

// Measurements turn the wire classical, unless `output-wire: none`
#quill.quantum-circuit(..tq.build(
  tq.h(0), tq.cx(0, 1), 
  tq.meter(0), tq.measure(1, output-wire: none), tq.x(1),
))

#pagebreak()

// lstick/rstick start and end wires; an ancilla starting late
#quill.quantum-circuit(..tq.build(
  tq.lstick(0, $a$), tq.lstick(1, $b$, n: 2), 
  tq.h(0), tq.cx(0, 1), tq.barrier(), tq.lstick(3, $c$), tq.cx(3, 1),
  tq.rstick(1, $d$),
  tq.measure(2),
))
