                       Shelta <Maentwrog Mk IV>
                       ------------------------
              * * * NEAR-BETA VERSION v1999.12.23 * * *

Shelta <Maentwrog Mk IV> NEAR-BETA (c)1999 Chris Pressey, Cat's-Eye Technologies.
All rights reserved.  No warranty for suitability of any kind.
Consider yourself lucky if your head doesn't blow up.
This product is freely redistributable provided that all distributed copies
include the entire original unmodified archive.

What is Shelta <Maentwrog Mk IV>?
---------------------------------

Shelta <Maentwog Mk IV> (which I'll generally just call Shelta during
the scope of it's documentation) is a set of interrelated software
systems:

- Shelta the Language - somewhere between FORTH, FALSE, and Assembler
- SHELTA86.COM the Tool - a Shelta compiler written in 8086 asm
- SHELTA.BAT the Compiler - organizer ('make') for Shelta files & libraries
- SHELTAS.COM and SHELTAS2.COM - Shelta compilers written in Shelta
- The GUPI Protocol - a standardized library of Shelta definitions

What's the history of Shelta?
-----------------------------

My first programming language ever was called Maentwrog, a term taken
from that wholly remarkable book, _The Meaning of Liff_, by Douglas
Adams.

Maentwrog sucked.  But, it worked, kind of.  It was interpreted, but
as I recall, it wasn't even tokenized... making the interpreter more
than a little slow.  It was basically a subset of FORTH - not much
special there.

My second programming language ever was based on Maentwrog, and it
spawned a big hit called Befunge.  Befunge left Maentwrog in the dust,
because there WAS much special there - Befunge is 2D, and that's
trippy.  If you haven't tried to program in Befunge yet... try it!

However, I've always felt that I fell somewhat short of the mark I
was trying to make with Befunge-93.  After all, it was inspired by
FALSE and Brainf*ck, but unlike either of them, it was not a small
machine-dependent compiler.  It was a big, portable interpreter.
(In 1998 I rewrote the interpreter in assembly language to make a
Befunge-93 interpreter that fit into 2K.  But it's just not the same.)
The urge to write a tiny compiler has been gnawing at me for the
past few years.

As such, Maentwrog has not gone totally forgotten.  Over the years I've
made a few attempts at reworking the Maentwrog language, with little
success, until now.  The main thing holding back Maentwrog for so
long was it's lack of strict design principles.  Only now has the
subconscious philosophy of Maentwrog evolved to a point where it means
anything.  The result is Shelta.

What is Shelta's Etymology?
---------------------------

_The Oxford Dictionary of Current English_, 1996, describes Shelta as an
"ancient hybrid secret language used by Irish tinkers, Gypsies, etc."
Shelta <Maentwrog Mk IV> is targeted at a similar present-day audience.
Would you sometimes rather consider yourself a tinker (or a Gypsy) than
a computer programmer?  Then Shelta may just be for you.

What is Shelta's Philosophy?
----------------------------

Shelta's philosophy is one of simplicity of translation.  Shelta is
easy to implement in assembly language.  Shelta is nearly as low-level
as assembly language.  Very small Shelta compilers can be built.

Shelta is also relatively easy to bootstrap - that is, it's not that
difficult to implement a Shelta compiler in Shelta itself.  In fact,
that (along with writing a ridiculously small compiler) was my main
motivation for designing Shelta and building SHELTA86.COM.  For more
information on the bootstrapped Shelta compilers and bootstrapping in
general, see the file bootstrp.txt.

In and of itself, Shelta has no actual functional semantics: only
structural ones.  It relies on a either library of functions (such as
GUPI, described below) or inline machine language in order to be
considered Turing-Complete.  That is, not unlike the ancient Shelta
language, Shelta <Maentwrog Mk IV> is hybridized: the actual programming
is usually done in Shelta/GUPI.

What are Shelta's Influences?
-----------------------------

Shelta is influenced largely by the wholly remarkable programming
language FALSE, by Wouter van Oortmerssen - "FORTH with lambda
functions".  However, it is lower-level than FALSE.  It is more like
FORTH in some ways - for example, multicharacter user-defined names
can be used to name unlimited variables, not just the a-z in FALSE.
Lastly, it is unlike FORTH and more like Assembler in that there is
no FORTH-like environment nor any fixed-size blocks of text as
input files.

What is Shelta's Syntax?
------------------------

Tokens are delimited by whitespace - any whitespace and as much of it
as you like, but as long as two non-whitespace characters are adjacent,
they are considered part of the same token.  Shelta's idea of
whitespace is, in ASCII, everything from #32 (space) down to #1 (^A).
(#0 (NUL) is considered synonymous with an end-of-file condition.)

The exception to the above rule is a comment block, which begins
(anywhere) with a ";" character and ends at the next ";" character.
This can occur even in the middle of a token, so "HE; foo ;LLO" is
taken to be the token "HELLO".

User-defined tokens - depicted with "Name" in the following table -
can contain any non-whitespace characters, and can begin with any
non-whitespace characters except for "[", "]", "\", '^', "_" and "`".
(This includes digits - "1" by itself is a name, not a number.)

What are the recognized tokens of Shelta?
-----------------------------------------

	[	Begin block.
*	]	End block, push pointer.
	]=Name	End block, name pointer.
	]:Name	End block, name pointer to compile-time-only block.
*	]Name	End block, push named pointer.
	^Name	Push pointer to previously named block.
	_^Name	Insert pointer to previously named block.
	Name	Insert contents of previously named block.

*	`ABC	Insert string.
	_123	Insert decimal byte.
	__1234	Insert decimal word.
	\123	Push decimal word.

* = not available in SHELTA86.

What are some common syntactic idioms in Shelta?
------------------------------------------------

	[ `ABC ]		Push pointer to string.
	[ _5 _5 _5 _3 ]		Push pointer to byte array.
	[ __1234 __9999 ]	Push pointer to word array.
	[ _5 _5 ]=my-data	Name a byte array.
        [ _^my-data ]=my-refs   Name an array of references to data.
	_88			Insert anonymous inline machine code.
	[ _88 ]:xyz 		Define xyz as inline machine code.
	xyz			Insert named inline machine code.
        [ bar baz ]:foo		Declare 'foo' as an inline proc
        foo			Insert 'foo' as an inline proc.
        [ ]=bar                 Define named label.

Where do you use : instead of = after ]?
----------------------------------------

Originally, Shelta did not distinguish between blocks used as
updatable stores, subroutines, or templates for inlined instructions.
As such, it would include all of them into the resulting executable,
even the blocks only used at compile-time to define inline instructions.

By using : instead of = after ], the Shelta compiler will treat the
block as containing information which is only used at compile-time.
This is essentially a contract between the programmer and the compiler;
the programmer promises not to expect the ^Name or _^Name syntax to work
on the block, and the compiler ensures the block does not show up
extraneously in the resulting executable.

What are some of the quirks of Shelta?
--------------------------------------

Shelta's lambda syntax is not uniform.  On the top level, an empty
block such as this:
  [ ]=label
is not necessarily defined to actually work.  It is only defined to
produce sensible results when nested within another block like this:
  [ [ ]=label foo ]=block
Also, this is NOT the same thing as saying:
  [ [ foo ]=block1 bar ]=block2
This linearizes block1 out of block2, almost as if you had said
  [ foo ]=block1 [ bar ]=block2
Except that the identifier block1 is 'supposed' to be local to block2
(it ISN'T, but it might be good programming practice to treat it that
way anyway! :-)

To make things even worse, nesting more than two levels deep like so
  [ foo [ bar [ baz ] quuz ] phlef ]
probably doesn't do what you expect.  Feel free to experiment, though.

Shelta does not have or use forward references.  That seems to be no
problem, with the lambda-like declarations, but it can often force you
to write weird and awkwardly structured code.  If you need to refer to
the current block from within it, you can always name a block twice:

  [ foo ^this bar ]=this              ; won't work! ;
  [ [ ]=-this foo ^-this bar ]=this   ; works! ^this == ^-this ;

What is SHELTA86.COM?
---------------------

The Shelta compiler is implemented in 8086 assembly language and assembles
to a tiny (LESS THAN HALF A KILOBYTE! :-) executable program.  There are
several restrictions on the program in order to trim fat:

- Input file goes in standard input, .COM file comes out standard output.
  File should end in a ^@ (NUL) character to indicate EOF.
  This NUL should be preceded by whitespace (otherwise it'll form
  part of a token - you don't want that! :-)
- If a file error occurs, error code 32 is returned.
- If an undefined token is found, error code 16 is returned.
- The forms ] (End block push pointer,) ]Name (End block push named
  pointer,) and `xyz (Insert String) are not supported.  It is not
  difficult to work around these by explicitly naming and pushing
  blocks and using ASCII decimal sequences for strings.  These
  inequities could even be addressed by a simple pre-processor.

What is SHELTA.BAT?
-------------------

SHELTA.BAT allows one to harness a Shelta compiler such as SHELTA86.COM,
without having to directly put up with it's silly interface.

Usage:
	SHELTA compiler project-file {library-files...}

'compiler' is one of: 86 (the assembly-language compiler,) S (the
compiler written in Shelta and compiled with 86), or S2 (the
compiler written in Shelta and compiled with S.)  (See the file
bootstrp.txt for more information on the Shelta compilers written
in Shelta.)

You don't need to append '.she' to project-file or any library-file
you choose to include, SHELTA will do that for you and will
automatically name the output 'project-file.COM'.

SHELTA should support up to nine arguments, so you can specify seven
library files on the command line (there's no 'include' directive in
Shelta itself.)

As an example of how to use SHELTA, here's how to build and test one
of the example Shelta/GUPI programs, "Hello, world!":

(Updated Dec 8 2002 to reflect new directory structure:)

  cd shelta-<<version>>
  bin\shelta s2 prj\hello
  prj\hello

How can I specify what libraries for SHELTA.BAT to use by default?
------------------------------------------------------------------

By default SHELTA.BAT includes the following libraries:

  8086\8086.she       8086 subset definition
  8086\gupi.she       General GUPI library (defined in 8086 subset)
  8086\string.she     GUPI string functions (defined in 8086 subset)
  8086\dos.she        DOS-dependent GUPI I/O (defined in 8086 subset)
  gupi\linklist.she   Linked list library (defined in GUPI)

You can edit SHELTA.BAT to change which libraries it uses by default.
(It is just a .BAT file after all.)

8086.she: One could presumably replace these inline instructions with
equivalent instructions for another relative-addressing processor,
change a few lines of SHELTA86.ASM, and voila!  You could compile
Shelta to some other CPU.  It'd be a cute trick...

gupi.she: While Shelta comes with GUPI, you don't need to use GUPI
with Shelta!  You can comment out gupi.she and completely redefine the
semtantics of your Shelta.  For example, you could use a very small
set of instructions (a tar pit) and use Shelta to compile languages
very similar to Brainf*ck, Malbolge, etc.

dos.she: You can replace the dos.she library with the bios.she library;
it does the same thing but goes directly through the BIOS instead, and
you can write code that will work without DOS loaded (so you could even
build your own OS or embedded controller code with Shelta! ;-)

What is GUPI?
-------------

GUPI stands for Generic Utilitarian Programming Interface.  GUPI
is a set of Shelta definitions that acts as a standard library.

(Fact is, I'm not a big fan of how GUPI turned out; it is a
contrived and contingent beast, rather than the beautifully
designed and conceptually airtight scheme I had hoped for.
But I figure that if it was good enough to get me this far, it's
worth keeping around, and the hybrid design of Shelta makes it
easy to swap it for something else at a later time, anyway.)

The GUPI semantics as presented here work on a stack of word values
in a FORTH-like manner.  Note that GUPI is not yet well documented.
Nor is it guaranteed not to change (although it looks unlikely;
any major change will warrant it's own library; "GUPII" perhaps? :-)

What are some of the naming conventions of GUPI?
------------------------------------------------

Generally speaking...
The suffix b indicates 'byte'.
The suffix c indicates 'character'.
The suffix if indicates 'decision on a boolean'.
The suffix s indicates 'string with length' (block).
The suffix w indicates 'word' (normally 16-bit).
The suffix z indicates 'null-terminated string' (ASCIIZ).

Lack of any suffix usually indicates 'any type'.

What are the basic GUPI commands?
---------------------------------

	pop		pop and discard top stack element
	dup		duplicate top stack element
	swap		pop a, pop b, push a, push b

	to		pop pointer, machine unary jump to pointer
	do		pop pointer, machine sub call pointer
        toif		pop pointer, pop boolean, unary jump if nonzero
	doif		pop pointer, pop boolean, sub call if nonzero
	begin		pop return pointer and push onto call stack
	end		push return pointer from call stack

 begin, end, and do/doif lead to the following GUPI idiom:
   [ begin baz end ]=bar	Declare 'bar' as a subroutine.
   ^bar do			Call 'bar' as a subroutine call.

What are the memory-access commands?
------------------------------------

	getb		pop pointer, push byte data at pointer
	putb		pop pointer, pop byte value, write at pointer
	getw		pop pointer, push word data at pointer
	putw		pop pointer, pop word value, write at pointer

What are the arithmetic and logic commands?
-------------------------------------------

	++		pop a, push a + 1
	--		pop a, push a - 1
	**		pop a, push a << 1
	//		pop a, push a >> 1
	<<		pop a, pop b, push b << a
	>>		pop a, pop b, push b >> a
	+		pop a, pop b, push b + a
	-		pop a, pop b, push b - a
	*		pop a, pop b, push b * a
	/		pop a, pop b, push b / a
	%		pop a, pop b, push b mod a
*1	/%		pop a, pop b, push b / a, push b mod a
	!		pop a, push binary not a
	zero		pop a, push 1 if a = 0, push 0 otherwise
	&		pop a, pop b, push a binary and b
	|		pop a, pop b, push a binary or b
	~		pop a, pop b, push a binary xor b

*1.  The algorithm commonly used for binary division actually computes
  the results of both division and modulo (remainder for a > 0).  If
  both results are desired by the program, using /% is usually nearly
  twice as efficient as using / and % seperately.

Indirect arithmetic?
--------------------

	@++		pop pointer, increment word at pointer
	@--		pop pointer, decrement word at pointer

How does GUPI interface with the operating system?
--------------------------------------------------

	outs		pop length, pop pointer, send bytes to stdout
	outc		pop word, send low byte to stdout
	inc		wait for input on stdin, push character read
	qinc		quietly wait for input on stdin, push character
	chkin		immediately return input status (is a char waiting?)
	flin		flush all unread input
	halt		pop a, stop program and return to operating system
			with error code 'a'

And dynamic memory?
-------------------

	malloc		pop size, push ptr to memory of length size
	mfree		pop ptr, reset heap ptr to ptr
		(Note that mfree will free ALL pointers that were
		allocated with malloc since the pointer that is being
		freed was allocated.  It's good for local
		linked lists and such, but be careful!)

What is "Portable Shelta/GUPI"?
-------------------------------

The short answer is, "Portable Shelta/GUPI" is the subset of the
union of the Shelta and GUPI languages where, through patience
and restraint - i.e. discipline - the Shelta/GUPI programmer
does not use any machine-dependent or self-modifying code, and
restricts themselves to the GUPI functions that do likewise or
are specified precisely and abstractly enough to be ported,
that is, re-written in some other machine or VM bytecode.

Where can I get updates on Shelta's condition?
----------------------------------------------

Shelta's official web site is located at:

  http://www.catseye.mb.ca/esoteric/shelta/

Happy tinkering!

Chris Pressey, Dec 23 1999
Cat's-Eye Technologies, Winnipeg, Manitoba, Canada
