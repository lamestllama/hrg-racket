# hrg-racket

Reflection-based graph-grammar expansion in Racket.

## Idea

Take the inferred grammar from [hrg](../hrg) (or any HRG-style grammar
with templates + compositions + slot caps) and emit it as Racket code.
Each template becomes a macro; each composition becomes a binding /
arity constraint between macro instances. The program then uses
reflection (`module->namespace`, `local-expand`, `syntax-parse`) to
read its own syntax tree, recognise subgraphs of the host graph that
match its template definitions, and rewrite them as template
applications.

Pipeline:

```
graph G  →  hrg infer  →  Grammar { templates, compositions, cover }
                                   ↓ emit
                       Racket module containing:
                          - one macro per template
                          - one composition rule per pair
                          - the cover as nested macro applications
                                   ↓ load + reflect
                       Self-recognising program: given a graph,
                       expand it into the most-template-applications
                       form using its own definitions as the pattern
                       library.
```

## Phases

1. **Round-trip** — `graph → grammar → racket → expand → same graph`
   (deterministic, no recognition needed)
2. **Recognition** — given a graph that's *similar* to the training
   graph, find the best cover using the existing templates
3. **Self-expansion** — the program rewrites itself toward the
   most-collapsed form using its own templates

## Dependencies

- Racket 8+ (`syntax-parse`, `module->namespace`)
- Optional: hrg Python package for the inference step (or import a
  serialised Grammar)
