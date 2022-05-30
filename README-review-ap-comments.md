# Code review comments for Archontis

General:

- Comments should be above the line they are referencing.
- Some lines are quite long, so you need to scroll to read the whole line.
  Consider breaking them to the next line.
- Don't need to specify that something is a helper function. Just say what it does.

`getCloseByEdge`:

- I would change the if-else chain to a match case. Change the conditions to variables and match on the variable.
- Reorder the conditions so the `None` is either first or last.

`movePortUpdate`:

- Why have `baseOffset` and then immediately `baseOffset'`?

`findOffsetSameEdge`:

- Replace the if-else structure with a match statement.

You could probably move `portsOnEdge` to be a function inside `movePortUpdate` rather than
defining it twice.

`isTouchingEdge`:

- When creating a very long record, have each field on a new line, rather than one very long line.
- Confusing name - I expected this to return a bool when reading the name.
