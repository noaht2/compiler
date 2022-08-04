# An MMIX compiler

This compiles a simple Turing-complete (except for memory constraints) programming language to MMIXAL, the assembly language for [the MMIX computer](http://mmix.cs.hm.edu).

## How to use
```sh
make all
./Main program.s
mmix program.mmo
```

## Example program
```
letrec tri(n) = (if zero?(n) then 0 else (n - (0 - (tri(n - 1))))) in (tri(10))
```
