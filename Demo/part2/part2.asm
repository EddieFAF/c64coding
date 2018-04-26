;*********************************************************************72
;**  Project Name: Demo Project                                       **
;**  ---------------------------------------------------------------  **
;**  Filename: Part2.asm                                              **
;**  ---------------------------------------------------------------  **
;**  Author (c): [FAF]Eddie                                           **
;**  File Date: 2018-04-11                                            **
;***********************************************************************
!src "../stdlib/stdlib.a"
!to "part2.prg", cbm
!sl "part2.map"
!cpu 6510
!ct pet

                        ; constants 
C_SCREEN_RAM            = $0400
C_UNPACK_ROUTINE        = $0810
C_UNPACK_DEST           = $0824
C_UNPACK_SOURCE         = $0834
C_APPLY_INTERRUPT       = $0840
C_EXIT_PART             = $084C
C_COLOUR_RAM            = $d800

      *=$0A00

!src "../stdlib/macros.asm"

;***********************************************************************
;**  Source Code Includes                                             **
;***********************************************************************
; !src "const.asm"
; !src "sprites.asm"
;***********************************************************************
;**  Labled Subroutines / (a.k.a.) Functions                          **
;***********************************************************************
!zone
; main function / subroutine
sync_intro
            jsr gfx_init
            jsr charset_init
            lda #$00
            jsr $1000
            lda #$2C
            ldx #<irq1
            ldy #>irq1
            jmp C_APPLY_INTERRUPT

gfx_init
            ldx #$00
-           txa
            sta $0400,x
            sta $0418,x
            inx
            bne -
            rts

charset_init
            ldx #$00
-           lda CHARSET,x
            sta $2000,x
            lda CHARSET+$100,x
            sta $2100,x
            lda CHARSET+$200,x
            sta $2200,x
            lda CHARSET+$300,x
            sta $2300,x
            inx
            bne -
            rts

; ************************************************
; ** IRQ Routinen                               **
; ************************************************
irq1
            inc VIC2InteruptStatus     ; acknowledge interrupt
            lda #$19
            sta $d018
            lda #$1b
            sta $d011
            lda #$31
            cmp $d012
            bne *-3
            ldx #$0B
            dex
            bne *-1
            lda #$01
            sta VIC2BorderColour
            lda #$01
            sta VIC2ScreenColour
            ldx #$09            ; delay
            dex
            bne *-1
            lda #$00
            sta VIC2ScreenColour
            lda #$00
            sta VIC2BorderColour
            lda #$6F
            ldx #<irq2
            ldy #>irq2
            jmp C_APPLY_INTERRUPT

irq2
            inc VIC2InteruptStatus     ; acknowledge interrupt
            lda #$06
            sta $d020
            sta $d021

            lda #$7f               ; detect space bar 
            sta CIA1KeyboardColumnJoystickA
            lda CIA1KeyboardRowsJoystickB
            and #$10
            bne update_irq
            jmp C_EXIT_PART

update_irq  lda #$8F
            ldx #<irq3
            ldy #>irq3
            jmp C_APPLY_INTERRUPT

irq3
            inc VIC2InteruptStatus     ; acknowledge interrupt
            lda #$00
            sta $d020
            sta $d021

            dec $D020
            jsr $1003
            inc $D020

            lda #$2C
            ldx #<irq1
            ldy #>irq1
            jmp C_APPLY_INTERRUPT

;***********************************************************************
;**  Included Labled Data (Resources from external files) which the   **
;**  compiler automatically (reserves and) assignes (a) (starting)    **
;**  memory (address) for.                                            **
;***********************************************************************
      *=$1000
      !binary "Supremacy.sid",,$7E
;*=$2000
CHARSET !binary "lazy_jones.64c",,2
;*********************************************************************72
;** File END                                                          **
;***********************************************************************
