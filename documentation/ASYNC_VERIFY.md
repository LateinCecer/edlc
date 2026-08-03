
# Verification on Async Code

There are some rules that are necessary to uphold consistency on the async analysis pipeline.
These must be followed by the programmer and verified by the compiler at compile time.

# Functions With Return Marked `async`

For functions that have a return value marked as `async` we must make sure that the caller actually knows all of the
dependencies and shared references for the return value.
This means that the async state of each return sealing statement _must_ only contain source states inherited from the
function parameters or local parameters.
Globals must be excluded, as the syntax does not have a way to declare which globals are captured by the return value.

## Capturing Globals

As explained above, globals cannot be referenced in the output state of return values.
While it would _technically_ be possible to include syntax that specifies which globals are captured, that would
massively overload the syntax.

# Functions Marked `async`

# Plain Functions

For plain functions not marked with any `async` modifiers cannot carry and async state dependencies.
Therefore, their return values may not have any dependencies or shared references in the async state.
