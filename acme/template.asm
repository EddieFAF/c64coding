;*********************************************************************72
;**  Project Name: Example Project                                    **
;**  ---------------------------------------------------------------  **
;**  Filename: Example_File.asm                                       **
;**  ---------------------------------------------------------------  **
;**  Author (c): xxxxxxxxxxx                                          **
;**  File Date: 2016-08-20                                            **
;***********************************************************************
;***********************************************************************
;** 1.)                                                               **
;**  Compiling and pre-processing Instructions                        **
;** 2.)                                                               **
;**  Symbolic CONSTANTS and Variables which the programmer directly   **
;**  assigns a value for (interpreted as value or memory address).    **
;**  These Symbols need to be resolved in the first compiler pass.    **
;** 3.)                                                               **
;**  Macros                                                           **
;** 4.)                                                               **
;**  BASIC Starter                                                    **
;** 5.)                                                               **
;**  Executable Code                                                  **
;** 6.)                                                               **
;**  Source Code Includes                                             **
;** 7.)                                                               **
;**  Labled Subroutines / (a.k.a.) Functions                          **
;** 8.)                                                               **
;**  Labled Data (Variables and Data Blocks) which the compiler       **
;**  automatically (reserves and) assignes (a) (starting) memory      **
;**  (addresses) for.                                                 **
;**  These Symbols are resolved in the second compiler pass.          **
;** 9.)                                                               **
;**  Included Labled Data (Resources from external files) which the   **
;**  compiler automatically (reserves and) assignes (a) (starting)    **
;**  memory (address) for.                                            **
;**  These Symbols are resolved in the second compiler pass.          **
;***********************************************************************
;***********************************************************************
;** 1.)                                                               **
;**  Compiling and pre-processing Instructions                        **
;***********************************************************************
;-- Set output file name and output file type:
!to "filename.prg", cbm
;-- Create a list of Symbols (Symbol List) encountered during 
;-- compilation and save in file:
!sl "symbols_found.txt"
;-- Set program counter to default starting address where
;-- we expect this program to be loaded to (2049 = $0801):
* = $0801
;***********************************************************************
;** 2.)                                                               **
;**  Symbolic CONSTANTS and Variables which the programmer directly   **
;**  assigns a value for (interpreted as value or memory address).    **
;**  These Symbols need to be resolved in the first compiler pass.    **
;***********************************************************************
; SCREEN_CHAR           = 52224
;***********************************************************************
;** 3.)                                                               **
;**  Macros                                                           **
;***********************************************************************
;!macro COPY_16 .from, .to {
;  lda .from
;  sta .to
;  lda .from +1
;  sta .to +1
;  }
;***********************************************************************
;** 4.)                                                               **
;**  BASIC Starter                                                    **
;***********************************************************************
;-- RAM Address = $0801 = 2049
;-- BASIC lines always start with pointer to next
;-- BASIC line, in low-bite, high-bite order:
;-- Next BASIC line starts at: 2049 + 27 = 2076
;-- $2076 = $081C in lo/hi = $1C, $08
;-- BASIC line number 10 in lo/hi = $000a = $0a, $00
;-- Token for REM = $8F
;-- PETSCII Code for Space = $20
;-- Total number of bytes x = 6
!byte $1C, $08, $0a, $00, $8f, $20    ; 10 REM
;-- RAM Address = 2049 + 6 = 2055
;-- We start machine code at 2112 = $0840.
;-- The SYS line after the REMs needs 9 Bytes
;-- and the line-terminating ZERO of the REM
;-- line needs one byte. So we have:
;-- 2112 - 2055 - 9 - 1 = 47 Bytes for REM text!
!text "*** PROJECT NAME ***"          ; !text translates to PETSCII codes
;--   |-- Keep this width--| to avoid next BASIC line calculations.
;-- RAM Address = 2055 + 20 = 2075
; ZERO Byte marking end of BASIC line
!byte $00
;-- RAM Address = 2075 + 1 = 2076
;-- Next BASIC line starts at: 2076 + 6 + 4 + 3 = 2089
;-- 2089 = $0829 in lo/hi = $29, $08
;-- BASIC line number 20 = $0014 in lo/hi = $14, $00
; £9E = Token for SYS command
; $20 = ASCII for Space
!byte $29, $08, $14, $00, $9E, $20    ; 20 SYS
!text "2112"                          ; entry point 2112 = $ 0840
!byte $00, $00                        ; end of BASIC line
;-- NULL, NULL instead of pointer to next BASIC
;-- line signifies end of BASIC program:
!byte $00,                            ; end of BASIC program
;-- Next instruction could start at:
;-- 2076 + 6 + 4 + 3 = 2089.
;-- To leave room for expanding REM,
;-- we start machine code at 2112 = $0840
;***********************************************************************
;** 5.)                                                               **
;**  Executable Code                                                  **
;***********************************************************************
;-- Set program counter to starting address given
;-- in BASIC Start above (2112 = $0840):
;-- Perhaps this also means: Assemble everything after here to this
;-- address, since this is where SYS will jump to:
* = $0840
;-- Jump to main function / subroutine
jmp main
;***********************************************************************
;** 6.)                                                               **
;**  Source Code Includes                                             **
;***********************************************************************
; !src "const.asm"
; !src "macros.asm"
; !src "sprites.asm"
; !src "levels.asm"
; !src "globals.asm"
; !src "irq.asm"
; !src "graphics.asm"
; !src "game.asm"
;***********************************************************************
;** 7.)                                                               **
;**  Labled Subroutines / (a.k.a.) Functions                          **
;***********************************************************************
!zone
; main function / subroutine
main
;  jsr clear_screen
;  jsr turn_off_basic
;  jsr game_init
;  jsr irq_setup
  .forever
;  jsr game_loop
  rts            ; Exit loop to test BASIC  
  jmp .forever
;***********************************************************************
;** 8.)                                                               **
;**  Labled Data (Variables and Data Blocks) which the compiler       **
;**  automatically (reserves and) assignes (a) (starting) memory      **
;**  (addresses) for.                                                 **
;**  These Symbols are resolved in the second compiler pass.          **
;***********************************************************************
;-----------------------------------------------------------------------
;-- Declare my own variables placed by the compiler.
;-- This MUST be done at the end of the program, otherwise it seems
;-- that you screw up your program / the program pointer or something!
;-----------------------------------------------------------------------
;MY_VAR_SpriteNumber
;          !byte 00
;MY_VAR_SpritePosX
;          !byte 00, 00, 00, 00, 00, 00, 00, 00
;MY_VAR_SpritePosXExtend
;;-- Sprite number:     76543210
;                !byte %00000000
;MY_VAR_SpritePosY
;          !byte 00, 00, 00, 00, 00, 00, 00, 00
;***********************************************************************
;** 9.)                                                               **
;**  Included Labled Data (Resources from external files) which the   **
;**  compiler automatically (reserves and) assignes (a) (starting)    **
;**  memory (address) for.                                            **
;**  These Symbols are resolved in the second compiler pass.          **
;***********************************************************************
;-----------------------------------------------------------------------
;-- Contents of resource (BINARY) files are copied (as a first step)
;-- directly into the regular program code / memory area here 
;-- between $C000 (49152) and $D000 (53248), by using ACME's pseudo 
;-- OP-code "!binary". This is done right after the declaration of 
;-- Symbols which will represent the starting addresses of said file 
;-- contents loaded / included into memory here for later use.
;-----------------------------------------------------------------------
;-- Call:     !binary FILENAME [, [SIZE] [, [SKIP]]]
;-- Purpose:  Insert binary file directly into output file.
;-- Aliases:  "!bin"
;-----------------------------------------------------------------------
;-- Include character set "j.chr" and save starting address
;-- to it in CHARSET CONSTANT:
;CHARSET !binary "j.chr"
;-- Include sprites in "j.spr" and save starting address
;-- to it in SPRITES CONSTANT:
;SPRITES !binary "j.spr"
;*********************************************************************72
;** File END                                                          **
;***********************************************************************
