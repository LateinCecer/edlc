
# About

This document covers move semantics on the MIR level in the EDL compiler.

# Which Operations Consume Source Data?

| Operation              | Consuming |
|------------------------|-----------|
| `move`                 | yes       |
| `copy`                 | no        |
| `ref`                  | no        |
| `deref`                | no        |
| `function arguments`   | yes       |
| `jump` arguments       | no        |
| `cond jump` condition  | yes       |
| `cond jump` arguments  | no        |
| `switch` condition     | yes       |
| `switch` arguments     | no        |
| `return` arguments     | yes       |
| `panic` arguments      | yes       |
| `type init` arguments  | yes       |
| `array init` arguments | yes       |


