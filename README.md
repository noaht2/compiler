# An MMIX compiler

This compiles a simple Turing‐complete programming language to MMIXAL, the assembly language for [the MMIX computer](http://mmix.cs.hm.edu).

## How to use
```sh
make all
./Main program.lisp  # The filename of the source code should end in .lisp
mmix program.mmo
```

## Example programs
```lisp
(letrec tri (n)
	(if (zero? n)
		0
		(- n (- 0 (tri (- n 1)))))
	(tri 10))
```
```lisp
((λ (x y) (− x (− 0 y))) 2 3)
```

## Technical details
The parser is built out of monadic parser combinators.
The compiler converts the programs to continuation‐passing style.
The language is dynamically typed with first‐class functions and lexical scope.

## Books used
- Richard Bird. [*Thinking functionally with Haskell*](https://www.cambridge.org/gb/academic/subjects/computer-science/programming-languages-and-applied-logic/thinking-functionally-haskell?format=HB&isbn=9781107087200). Cambridge University Press
- Harold Abelson and Gerald Jay Sussman, with Julie Sussman. [*Structure and Interpretation of Computer Programs*](https://mitpress.mit.edu/9780262011532/). With a foreword by Alan J. Perlis. 2nd edition. The MIT Press
- Daniel P. Friedman and Mitchell Wand. [*Essentials of Programming Languages*](https://eopl3.com). With a foreword by Hal Abelson. 3rd edition. The MIT Press
- Donald E. Knuth. [*The Art of Computer Programming*](https://www-cs-faculty.stanford.edu/~knuth/taocp.html). Addison–Wesley
