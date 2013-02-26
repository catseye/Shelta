IDEAL

;  shelta86.asm v1999.10.20 (c)1999 Chris Pressey, Cat's-Eye Technologies.
;  Implements an assembler/compiler for the Shelta language, in 8086 assembly.

;  * Special thanks to Ben Olmstead (BEM) for his suggestions for how to
;    reduce SHELTA86.COM's size even further.

MODEL	tiny
P8086

DATASEG

symth		dw	symt
codeh		dw	code
stach		dw	stac
safeh		dw	safe + 2
macrh		dw	macr

ttable		dw	BeginBlock, PushWord, EndBlock, PushPointer, LiteralByte ; , String
;			[           \         ]         ^            _               `

UDATASEG

token		db	128 dup (?)

safestart	dw	?
namestart	dw	?
toklength	dw	?

safe		db	16384 dup (?)
symt		db	16384 dup (?)	; 16K + 16K = 32K
code		db	4096 dup (?)	; 
macr		db	4096 dup (?)	; + 8K = 40K
stac		db	256 dup (?)

CODESEG
ORG 0100h

; EQUATES

safeadj		EQU	(offset safe - 0104h)
codeadj		EQU	(offset code - 0104h)

; Main program.
PROC		Main

WhileFile:

; ----- begin scanning token

		call	ScanChar	; get char -> al
		or	al, al
		jz	@@EndFile
		cmp	al, 32
		jbe	WhileFile	; repeat if char is whitespace

		mov	di, offset token
		cld

@@TokenLoop:	stosb			; put char in token
		call	ScanChar	; get char
		cmp	al, 32
		ja	@@TokenLoop	; repeat if char is not whitespace

@@Terminate:	mov	[byte di], 0  ; return null-terminated token

; ----- end scanning token

		mov	si, offset token + 1

		mov	al, [byte token]
		sub	al, '['
		cmp	al, 4
		ja	@@Unroll

		xor	ah, ah
		shl	ax, 1
		xchg	bx, ax
		mov	ax, [offset ttable + bx]
		jmp	ax		; jump to handler as listed in ttable

@@Unroll:	dec	si		; start at first character of token
		call	LookupSymbol	; destroys DI & SI, but that's OK

		; copy cx bytes from ax to codeh

		xchg	ax, si
		mov	di, [codeh]		; use di to track codeh
		rep	movsb

UpCodeH:	mov	[codeh], di
		jmp	short WhileFile

@@EndFile:	; put in a jump over the safe area

		mov	ax, [safeh]
		sub	ax, offset safe - 1
		mov	bx, offset token	; re-use token
		mov	[byte bx], 0e9h
		mov	[word bx + 1], ax
		mov	[byte bx + 3], 90h

		mov	cx, 4
		mov	dx, offset token
		call	WriteIt

		; make the first word of the safe area an offset
		; to just past the last word of the code 

		mov	cx, [safeh]
		mov	dx, offset safe
		sub	cx, dx
		mov	ax, cx
		add	ax, [codeh]
		sub	ax, codeadj
		mov	[word safe], ax

		call	WriteIt

		mov	cx, [codeh]
		mov	dx, offset code
		sub	cx, dx
		call	WriteIt
		
		xor	al, al

GlobalExit:	mov     ah, 4ch		; exit to DOS
		int     21h
ENDP		Main

PROC		WriteIt

		mov	ah, 40h
		mov	bx, 1
		int	21h
		jnc	@@OK
		mov	al, 32
		jmp	short GlobalExit
@@OK:		ret
ENDP		WriteIt

; -------------------------------- HANDLERS --------------------------- ;
; When coming into any handler, di will equal the address of the null
; (that is, the number of characters in the token + offset token)

; ==== [ ==== BEGIN BLOCK ==== ;

BeginBlock:	mov	di, [stach]			; push [ onto stack
		mov	ax, [codeh]
		stosw					; mov	[bx], ax
		mov	[stach], di
                jmp     WhileFile

; ==== ] ==== END BLOCK ==== ;

EndBlock:	;mov	si, offset token + 1	; si = token + 1 until...
		;cmp	[byte ds:si], '='
		;je	@@Smaller
		;cmp	[byte ds:si], ':'
		;je	@@Smaller
		;jmp	short @@CarryOn
						; remove : or = from length
@@Smaller:	dec	di			; di left over from scanning token

@@CarryOn:	mov	bx, di			; di now free to hold something until @@WName
		sub	bx, si			; get length

		mov	ax, [safeh]
		mov	[safestart], ax
		mov	[namestart], ax
		xchg	ax, di			; di now holds safe area head location

		mov	[toklength], bx		; length of token
		sub	[stach], 2
		mov	bx, [stach]		; pop [ from stack

		mov	ax, [bx]		; ax = codeh when [ happened

		mov	bp, [codeh]		; find length
		sub	bp, ax
		; mov	bp, bx			; bp = length of data between [ ... ]
						; until @@WName below... ugh

		cmp	[stach], offset stac
		je	@@StackEmpty


		mov	bx, [stach]
		sub	bx, 2
		mov	cx, [bx]

		; namestart = [namestart] - (cx - ax)

		sub	cx, ax
		sub	[namestart], cx

		; if dlength > 0,

@@StackEmpty:	;or	bp, bp
		;jz	@@Empty

		cmp	[byte si], ':'		; si still = offset token + 1
		jne	@@PreCopyLoop

		mov	di, [macrh]		; use macro area instead of safe if :
		mov	[namestart], di

		; copy everything from ax to codeh into the di area

@@PreCopyLoop:	mov	dx, ax
		mov	cx, bp 	; 		[codeh]		sub	cx, ax
		push	si
		xchg	si, ax
		rep	movsb
		pop	si

		; change codeh back to dx (old codeh before [)

		mov	[codeh], dx

		;mov	si, offset token + 1
		cmp	[byte si], ':'		; si still = offset token + 1
		je	@@UpdateMacr

		mov	[safeh], di
		jmp	short @@Empty
@@UpdateMacr:	mov	[macrh], di
		;jmp	short @@NameIt

		; write push instruction if '=' or ':' not used

@@Empty:	;cmp	[byte si], '='			; si still = offset token + 1
		;je	@@NameIt

		;mov	ax, [safestart]
		;sub	ax, safeadj
		;mov	bx, [word codeh]
		;mov	[byte bx], 0b8h
		;mov	[word bx + 1], ax
		;mov	[byte bx + 3], 50h
		;add	[codeh], 4

		;cmp	[byte si], 0			; still offset token + 1!
                ;je      @@Anonymous

		; insert namestart into dictionary

@@NameIt:	mov	cx, [namestart]
		mov	ax, [toklength]

		;cmp	[byte si], '='
		;je	@@Bigger
		;cmp	[byte si], ':'
		;je	@@Bigger
		;jmp	short @@WName

@@Bigger:	inc	si

@@WName:	; Destroys DI but that's OK.
		; INPUT:  bx = ADDRESS of token to insert, ax = length of symbol,
		; cx = pointer to data, dx = length of data
		; OUTPUT: ds:bx = pointer to newly allocated symbol

		mov	di, [symth]		; di no longer contains macrh/safeh
		add	ax, 6			; 1 word for length, 1 for ptr, 1 for data length
		add	[symth], ax

		stosw	; mov	[word di], ax	; place ax length in symt

		sub	ax, 6
		xchg	cx, ax			; cx <- ax; ax <- cx
		stosw	; mov	[word di], cx	; place cx (ptr to data)
		xchg	ax, bp		
		stosw	; mov	[word di], bp	; place bp (ptr length)

		rep	movsb

		mov	[symth], di

@@Anonymous:    jmp     WhileFile

; ==== ^ ==== PUSH POINTER ==== ;

PushPointer:	;mov	si, offset token + 1
		call	LookupSymbol		; destroys di & si, should be OK

		sub	ax, safeadj
		mov	di, [word codeh]
		jmp	short WritePush

; ==== ` ==== STRING ==== ;
;
;String:		;mov	si, offset token + 1
;		mov	di, [codeh]
;@@Loop:		mov	al, [byte ds:si]
;		stosb
;		inc	si
;		cmp	[byte ds:si], 0
;		jne	@@Loop
;                jmp     UpCodeH

; ==== _ ==== LITERAL BYTE ==== ;

LiteralByte:	;mov	si, offset token + 1
		cmp	[byte si], '_'
		je	LiteralWord
		cmp	[byte si], '^'
		je	LiteralSymbol
		call	DecipherDecimal		; destroys DI, that's OK
		stosb	; mov	[byte bx], al
CheapTrick:	mov	[codeh], di
                jmp     WhileFile

; ==== __ ==== LITERAL WORD ==== ;

LiteralWord:	inc	si
		call	DecipherDecimal		; destroys DI, that's OK
FunkyTrick:	stosw	; mov	[word bx], ax
		jmp	short CheapTrick

; ==== _^ ==== LITERAL SYMBOL ==== ;

LiteralSymbol:	inc	si
		call	LookupSymbol		; destroys DI & SI, that's OK

		sub	ax, safeadj

		mov	di, [word codeh]
		jmp	short FunkyTrick
		;mov	[word bx], ax
		;inc	[codeh]
		;jmp	short CheapTrick

; ==== \ ==== PUSH WORD ==== ;

PushWord:	;mov	si, offset token + 1
		call	DecipherDecimal		; destroys di, that's OK

WritePush:	mov	[byte di], 0b8h	; B8h, low byte, high byte, 50h
		inc	di
		stosw   ;	mov	[word di + 1], ax
		mov	al, 50h
		stosb
		mov	[codeh], di
                jmp     WhileFile

; -------------------------------- SUBROUTINES --------------------------- ;

PROC		DecipherDecimal   ; uses and destroys DI
		; INPUT: si = address of token
		; OUTPUT: ax = value, di = codeh


		xor	di, di

@@Loop:		lodsb	; mov	al, [byte ds:si], inc si

		mov	bx, di
		mov	cl, 3
		shl	bx, cl
		mov	cx, di
		shl	cx, 1
		add	bx, cx

		sub	al, '0'
		cbw
		add	bx, ax
		mov	di, bx

		cmp	[byte ds:si], '0'
		jae	@@Loop

		xchg	ax, di
		mov	di, [word codeh]
		ret
ENDP		DecipherDecimal

PROC            ScanChar
; Scans a single character from the input file, placing
; it in register al, which will be 0 upon error
; or eof (so don't embed nulls in the Shelta source...)

		mov	ah, 7		; read from stdin one byte
		int	21h
		cmp	al, ';'		; check for comment
		je	@@Comment
		ret
@@Comment:	mov	ah, 7		; read from stdin one byte
		int	21h
		cmp	al, ';'		; check for comment
		jne	@@Comment
		jmp	short ScanChar

ENDP            ScanChar

PROC		LookupSymbol
		; INPUT:  si = address of symbol to find, di = address of null termination
		; OUTPUT: ds:ax = pointer to contents or zero if not found
		; cx = length of contents

		mov	bx, offset symt		; bx starts at symbol table
		mov	bp, si
		sub	di, si

@@Loop:		mov	ax, [word bx]		; first word = token size

		mov	dx, bx			; keep track of start of this symt entry

		sub	ax, 6
		cmp	ax, di
		jne	@@Exit			; if it doesn't fit, you must acquit

		add	bx, 6			; bx now points to token in symbol table

;   exit if right token

		xor	si, si			; reset si to token
@@Inner:	mov	al, [byte ds:bx]	; get byte from bx=symt
		cmp	[byte bp + si], al	; compare to si=token
		jne	@@Exit
		inc	bx
		inc	si
		cmp	si, di			; hit the length yet?
		jb	@@Inner			; no, repeat

		;   a match!

		mov	bx, dx
		mov	cx, [word bx + 4]	; third word = data length
		mov	ax, [word bx + 2]	; second word = data ptr 
		ret

@@Exit:		mov	bx, dx
		mov	ax, [word bx]
		add	bx, ax
		cmp	bx, [symth]
		jb	@@Loop

		mov	al, 16		; return 16 if unknown identifier
		jmp	GlobalExit

ENDP		LookupSymbol

END		Main
