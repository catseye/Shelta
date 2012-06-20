;  shelta86.s
;  v1.1.2009.0307
;  (c)2009 Chris Pressey, Cat's Eye Technologies.

;  Implements an assembler/compiler for the Shelta language, in 8086 machine
;  language, in the format of the NASM assembler.

;  * Special thanks to Ben Olmstead (BEM) for his suggestions for how to
;    reduce SHELTA86.COM's size even further.

org             0100h
bits            16
cpu             8086

;-------------- Code

; Main program.

WhileFile:

; ----- begin scanning token

                call    word ScanChar   ; get char -> al
                or      al, al
                jz      EndFile
                cmp     al, 32
                jbe     WhileFile       ; repeat if char is whitespace

                mov     di, token
                cld

.TokenLoop:     stosb                   ; put char in token
                call    word ScanChar   ; get char
                cmp     al, 32
                ja      .TokenLoop      ; repeat if char is not whitespace

                mov     byte [di], 0    ; null-terminate the token

; ----- end scanning token

                mov     si, token + 1

                mov     al, [token]
                sub     al, '['
                cmp     al, 5
                ja      .Unroll
                xor     ah, ah
                shl     ax, 1
                xchg    bx, ax
                mov     ax, [ttable + bx]
                call    ax              ; call handler as listed in ttable
                jmp     short WhileFile

.Unroll:        dec     si              ; start at first character of token
                call    word LookupSymbol ; destroys DI & SI, but that's OK

                ; copy cx bytes from ax to codeh

                xchg    ax, si
                mov     di, [codeh]     ; use di to track codeh
                rep     movsb

UpCodeH:        mov     [codeh], di
                jmp     short WhileFile

EndFile:        ; put in a jump over the safe area

                mov     di, token       ; re-use token
                mov     al, 0e9h
                stosb
                mov     ax, [safeh]
                sub     ax, safe - 1
                stosw
                mov     al, 090h
                stosb

                mov     cx, 4
                mov     dx, token
                call    word WriteIt

                ; make the first word of the safe area an offset
                ; to just past the last word of the code 

                mov     cx, [safeh]
                mov     dx, safe
                sub     cx, dx
                mov     ax, cx
                add     ax, [codeh]
                sub     ax, codeadj
                mov     [safe], ax

                call    word WriteIt

                mov     cx, [codeh]
                mov     dx, code
                sub     cx, dx
                call    word WriteIt
                
                xor     al, al

GlobalExit:     mov     ah, 4ch         ; exit to DOS
                int     21h

WriteIt:
                mov     ah, 40h
                mov     bx, 1
                int     21h
                jnc     .OK
                mov     al, 32
                jmp     short GlobalExit
.OK:            ret

; -------------------------------- HANDLERS --------------------------- ;
; When coming into any handler, di will equal the address of the null
; (that is, the number of characters in the token + offset token)

; ==== [ ==== BEGIN BLOCK ==== ;

BeginBlock:     mov     di, [stach]     ; push [ onto stack
                mov     ax, [codeh]
                stosw                   ; mov   [bx], ax
                mov     [stach], di
                ret

; ==== ] ==== END BLOCK ==== ;

EndBlock:       dec     di              ; di left over from scanning token

                mov     bx, di          ; di now free to hold something until .WName
                sub     bx, si          ; get length of token
                mov     [toklength], bx ; store it for later

                mov     ax, [safeh]
                mov     [safestart], ax
                mov     dx, ax          ; dx = namestart initially = safestart = safeh
                xchg    ax, di          ; di now holds safe area head location

                sub     word [stach], byte 2
                mov     bx, [stach]     ; pop [ from stack
                mov     ax, [bx]        ; ax = codeh when [ happened

                mov     bp, [codeh]     ; find length
                sub     bp, ax          ; bp = length of code between [ ... ] (codeh - old codeh)

                cmp     word [stach], stac
                je      .StackEmpty

                mov     cx, [bx - 2]     ; cx = contents popped from stack

                ; namestart:dx = namestart:dx - (contents:cx - tokenlength:ax)

                sub     cx, ax
                sub     dx, cx

.StackEmpty:    cmp     byte [si], ':'  ; si still = offset token + 1
                jne     .PreCopy

                mov     di, [macrh]     ; copy into macro area instead of safe area if :
                mov     dx, di

                ; copy everything from ax to codeh into the di area

.PreCopy:       push    ax
                mov     cx, bp
                push    si
                xchg    si, ax
                rep     movsb
                pop     si
                pop     ax

                ; restore codeh back to old codeh before [

                mov     [codeh], ax
                cmp     byte [si], ':'  ; si still = offset token + 1
                jne     .UpdateSafe

                mov     [macrh], di
                jmp     short .NameIt

.UpdateSafe:    mov     [safeh], di

                ; write push instruction if '=' or ':' not used

                cmp     byte [si], '='  ; si still = offset token + 1
                je      .NameIt

                mov     ax, [safestart]
                sub     ax, safeadj

                mov     di, [codeh]      ; di no longer contains macrh/safeh
                jmp     short WritePush

                ; insert namestart into dictionary

.NameIt:        mov     cx, dx
                mov     ax, [toklength]

                inc     si

.WName:         ; Insert token into the symbol table.
                ; DESTROYS: DI
                ; INPUT:    si = pointer to token text
                ;           ax = length of token text
                ;           cx = pointer to data associated with token
                ;           bp = length of data associated with token

                mov     di, [symth]     ; di no longer contains macrh/safeh
                add     ax, 6           ; 1 word for length, 1 for ptr, 1 for data length

                stosw                   ; place ax length in symt

                sub     ax, 6
                xchg    cx, ax          ; cx <- ax; ax <- cx
                stosw                   ; place cx (ptr to data)
                xchg    ax, bp          
                stosw                   ; place bp (ptr length)

                rep     movsb

                mov     [symth], di

                ret

; ==== ^ ==== PUSH POINTER ==== ;

PushPointer:    call    LookupSymbol    ; destroys di & si, should be OK

                sub     ax, safeadj
                mov     di, [codeh]
                jmp     short WritePush

; ==== ` ==== STRING ==== ;

String:         mov     di, [codeh]
.Loop:          mov     al, [si]
                stosb
                inc     si
                cmp     byte [si], 0
                jne     .Loop
                mov     [codeh], di
                ret

; ==== _ ==== LITERAL BYTE ==== ;

LiteralByte:    cmp     byte [si], '_'
                je      LiteralWord
                cmp     byte [si], '^'
                je      LiteralSymbol
                call    DecipherDecimal ; sets DI to [codeh]
                jmp     short GnarlyTrick

; ==== __ ==== LITERAL WORD ==== ;

LiteralWord:    inc     si
                call    DecipherDecimal ; sets DI to [codeh]
FunkyTrick:     stosw
                jmp     short CheapTrick

; ==== _^ ==== LITERAL SYMBOL ==== ;

LiteralSymbol:  inc     si
                call    LookupSymbol    ; destroys DI & SI, that's OK

                sub     ax, safeadj

                mov     di, [codeh]
                jmp     short FunkyTrick

; ==== \ ==== PUSH WORD ==== ;

PushWord:       call    DecipherDecimal ; sets DI to [codeh]

WritePush:      mov     byte [di], 0b8h ; B8h, low byte, high byte, 50h
                inc     di
                stosw
                mov     al, 50h
GnarlyTrick:    stosb
CheapTrick:     mov     [codeh], di
                ret

; -------------------------------- SUBROUTINES --------------------------- ;

DecipherDecimal:
                ; INPUT: si = address of token
                ; OUTPUT: ax = value, di = codeh
                ; uses and destroys DI

                xor     di, di

.Loop:          lodsb

                mov     bx, di
                mov     cl, 3
                shl     bx, cl
                mov     cx, di
                shl     cx, 1
                add     bx, cx

                sub     al, '0'
                cbw
                add     bx, ax
                mov     di, bx

                cmp     byte [si], '0'
                jae     .Loop

                xchg    ax, di
                mov     di, [codeh]
                ret

; Scans a single character from the input file, placing
; it in register al, which will be 0 upon error
; or eof (so don't embed nulls in the Shelta source...)

ScanChar:
                mov     ah, 7           ; read from stdin one byte
                int     21h
                cmp     al, ';'         ; check for comment
                je      .Comment
                ret
.Comment:       mov     ah, 7           ; read from stdin one byte
                int     21h
                cmp     al, ';'         ; check for comment
                jne     .Comment
                jmp     short ScanChar

LookupSymbol:
                ; INPUT:  si = address of symbol to find, di = address of null termination
                ; OUTPUT: ds:ax = pointer to contents or zero if not found
                ; cx = length of contents

                mov     bx, symt        ; bx starts at symbol table
                mov     bp, si
                sub     di, si

.Loop:          mov     ax, [bx]        ; first word = token size

                mov     dx, bx          ; keep track of start of this symt entry

                sub     ax, 6
                cmp     ax, di
                jne     .Exit           ; if it doesn't fit, you must acquit

;   exit if right token

                xor     si, si          ; reset si to token
.Inner:         mov     al, [bx + 6]    ; get byte from bx+6=pointer to token text
                cmp     [bp + si], al   ; compare to si=token
                jne     .Exit
                inc     bx
                inc     si
                cmp     si, di          ; hit the length yet?
                jb      .Inner          ; no, repeat

                ;   a match!

                mov     bx, dx
                mov     cx, [bx + 4]    ; third word = data length
                mov     ax, [bx + 2]    ; second word = data ptr 
                ret

.Exit:          mov     bx, dx
                mov     ax, [bx]
                add     bx, ax
                cmp     bx, [symth]
                jb      .Loop

                mov     al, 16          ; return 16 if unknown identifier
                jmp     GlobalExit

;-------------- Initialized Data

symth:          dw      symt
codeh:          dw      code
stach:          dw      stac
safeh:          dw      safe + 2
macrh:          dw      macr

ttable:         dw      BeginBlock, PushWord, EndBlock, PushPointer, LiteralByte, String
;                       [           \         ]         ^            _            `

;-------------- Uninitialized Data

section .bss

token:          resb    128

safestart:      resw    1
toklength:      resw    1

safe:           resb    16384
symt:           resb    16384   ; 16K + 16K = 32K
code:           resb    4096
macr:           resb    4096    ; + 8K = 40K
stac:           resb    256

;-------------- Equates

safeadj         equ     (safe - 0104h)
codeadj         equ     (code - 0104h)
