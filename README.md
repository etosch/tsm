# TSM specifications

specs can be found by contacting Lee Spector: lspector@hampshire.edu

# Usage

The main entry point for the tag space machine evaluator is the function eval-tsm, which takes a TSM map as a single argument and evaluates it until the :x stack is empty.

## Notes:

* Stacks are implemented with a PersistentVector. Consequently, pushing and popping occurs at the end of the vector, rather than the beginning. Thus, the output will be printed in reverse. 
* eval-tsm is not currently a macro; when manually constructing examples and testing code, make sure you indicate that instructions are symbols.

# TODO

* Random code generator
* Implement macro to hide the need to make instructions symbols