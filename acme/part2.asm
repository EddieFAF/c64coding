; Part2: Ausblenden Bildschirm durch Reihen von Sprites, die sich stapeln.
; Assembler: ACME
      *= $0801
!byte $0c,$08,$0a,$00,$9e,$32,$30,$36,$34,$00,$00,$00,$00


!src "../stdlib/stdlib.a"
!src "../stdlib/macros.asm"

*=$0810
        ; Vorbereitungen ----------------------------------------------------]
        ; -------------------------------------------------------------------]
        lda #%01111111
        sta VIC2SpriteEnable
        lda #VIC2SpriteXBorderLeft
        sta VIC2Sprite0X
        lda #VIC2SpriteXBorderLeft+48
        sta VIC2Sprite1X
        lda #VIC2SpriteXBorderLeft+96
        sta VIC2Sprite2X
        lda #VIC2SpriteXBorderLeft+144
        sta VIC2Sprite3X
        lda #VIC2SpriteXBorderLeft+192
        sta VIC2Sprite4X
        lda #(VIC2SpriteXBorderLeft+240)-256
        sta VIC2Sprite5X
        lda #(VIC2SpriteXBorderLeft+288)-256
        sta VIC2Sprite6X
        lda #%01100000
        sta $d010
        lda #VIC2SpriteYBorderBottom-43 ; Y-Position an der unteren Kante
        sta VIC2Sprite0Y
        sta VIC2Sprite1Y
        sta VIC2Sprite2Y
        sta VIC2Sprite3Y
        sta VIC2Sprite4Y
        sta VIC2Sprite5Y
        sta VIC2Sprite6Y
        lda #$ff
        sta VIC2SpriteDoubleWidth
        sta VIC2SpriteDoubleHeight
        lda #$30
        sta $07f8               ; Sprite 0
        sta $07f9               ; Sprite 1
        sta $07fa               ; Sprite 2
        sta $07fb
        sta $07fc
        sta $07fd
        sta $07fe
        lda #VIC2Colour_LightGreen
        sta VIC2Sprite0Colour
        sta VIC2Sprite1Colour
        sta VIC2Sprite2Colour
        sta VIC2Sprite3Colour
        sta VIC2Sprite4Colour
        sta VIC2Sprite5Colour
        sta VIC2Sprite6Colour
        ldx #0
-       lda .sprite_line_data,X
        sta $0c00,X
        inx
        cpx #63
        bne -


start:  sei         ; set up interrupt
        jsr $e544
        lda #$7f
        sta $dc0d     ; turn off the CIA interrupts
        sta $dd0d
        and $d011     ; clear high bit of raster line
        sta $d011

        +irqEnd $32,.irq1

        lda #$01      ; enable raster interrupts
        sta $d01a
        cli
        jmp *       ; Endlosschleife
        rts         ; back to BASIC

        ; Start Intro -------------------------------------------------------]
        ; -------------------------------------------------------------------]
.irq1:
        ldx #$03
        dex
        bne *-1
        nop
        lda #$0e  ; Hellblau
        sta $d020
        lda #$06  ; Dunkelblau
        sta $d021

        ; Zeichensatz ausschalten
        lda #$16 ;font Kleinbuchstaben?
        sta $d018
        lda #$1b
        sta $d011
        lda #$C8 ; kein Multicolor
        sta $d016

        +irqEnd $b2, .irq2

        inc $d019     ; acknowledge interrupt
        jmp $ea31

        ; Logo (ab Zeile 48) ------------------------------------------------]
        ; -------------------------------------------------------------------]
.irq2:
        ldx #$04
        dex
        bne *-1
        lda #$0d
        sta $d020
        lda #$05
        sta $d021

        lda #$1b
        sta $d011
        ; Zeichensatz einschalten
        lda #$15
        sta $d018
        lda #$C8
        sta $d016

        lda #$62
        sta $d012
        ldx #<.irq1
        ldy #>.irq1
        stx $0314
        sty $0315
        inc $d019     ; acknowledge interrupt
        jmp $ea31

; Derzeit massive Blöcke
.sprite_line_data:
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte $00
