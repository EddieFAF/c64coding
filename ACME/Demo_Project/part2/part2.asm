;*********************************************************************72
;**  Project Name: Demo Project                                       **
;**  ---------------------------------------------------------------  **
;**  Filename: Part2.asm                                              **
;**  ---------------------------------------------------------------  **
;**  Author (c): [FAF]Eddie                                           **
;**  File Date: 2018-04-11                                            **
;***********************************************************************
!src "../../stdlib/stdlib.a"
!to "part2.prg", cbm
!sl "part2.map"
!cpu 6510
!ct pet

!src "../../stdlib/BASICEntry80d.a"

*=BASICEntry
    jmp main


!src "../../stdlib/macros.asm"

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
main
            jsr gfx_init
            jsr charset_init
            jsr irq_setup
.forever
;  jsr game_loop
            rts            ; Exit loop to test BASIC
            jmp .forever

irq_setup
            sei
            lda #$7f
            sta CIA1InterruptControl ; turn off the CIA interrupts
            sta CIA1InterruptControl
            and VIC2ScreenControlV ; clear high bit of raster line
            sta VIC2ScreenControlV
            +irqEnd $2C, irq1
            lda #$01            ; enable raster interrupts
            sta VIC2InteruptControl
            cli
            rts

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
            ;+irqEnd $6f, irq1b
            inc VIC2InteruptStatus     ; acknowledge interrupt
            jmp $ea31

;***********************************************************************
;**  Included Labled Data (Resources from external files) which the   **
;**  compiler automatically (reserves and) assignes (a) (starting)    **
;**  memory (address) for.                                            **
;***********************************************************************
;*=$2000
CHARSET !binary "lazy_jones.64c",,2
;*********************************************************************72
;** File END                                                          **
;***********************************************************************
