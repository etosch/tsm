# TSM specifications

specs can be found by contacting Lee Spector: lspector@hampshire.edu

# Usage

The main entry point for the tag space machine evaluator is the function eval-tsm, which takes a TSM map as a single argument and evaluates it until the :x stack is empty.

# Evolution

Code for running evolutionary algorithms is in the tsm.evolve namespace. tsm.evolve.gp implements a genetic programming paradigm with the following specifications:
* [Random Code] Random code is generated in the tsm.evolve.random namespace. It generates a random tag space of a fixed size. Random code currently includes a noop instruction, so that all programs/tag spaces are the same size. While not computationally effcient, this is done for the convenience of later analysis.
* [Crossover] Crossover is single point and occurs on the tag space. Since the tag space is an ordered map, this gives us the potential of evolving an ordering over the program.
* [Mutation] Mutation operates on the tag space. Mutation can either change the tag of a tagged item or the value of a tagged item.

## Notes:

* Stacks are implemented with a PersistentVector. Consequently, pushing and popping occurs at the end of the vector, rather than the beginning. Thus, the output will be printed in reverse. 
* eval-tsm is not currently a macro; when manually constructing examples and testing code, make sure you indicate that instructions are symbols.

# TODO

* Implement macro to hide the need to make instructions symbols