Shelta
======

Shelta is a minimal Forth-like language.  It has barely any semantics of its
own; it relies on inline machine code to write anything resembling an actual
program in it.

In the spirit of compilers for languages such as FALSE and brainfuck, a
Shelta-to-8086 compiler was implemented (with some help from Ben Olmstead) as
an MS-DOS `.COM` executable containing less than 512 bytes of 80286 machine
code.

What's more, it has also been bootstrapped — that is to say, a Shelta compiler
was written in Shelta, which was compiled with the original compiler, and then
compiled again with the resulting compiler, producing a wholly self-hosted
executable.

For more information, see the files in the `doc` directory of this distribution.
