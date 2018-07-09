.RELEASE = 0
.DEBUG   = 1

.fade_speed = $02 ; Verzögerung (01=schnell, 04=langsam)
.startzeile = 0 ; Erste Zeile
.endzeile   = 7 ; Letzte Zeile

!src "stdlib/stdlib.a"
!src "stdlib/macros.asm"

!if .RELEASE { !set C_APPLY_INTERRUPT = apply_interrupt } else { !set C_APPLY_INTERRUPT = .APPLY_INTERRUPT }

      *= $0801
!byte $0c,$08,$0a,$00,$9e,$32,$30,$36,$34,$00,$00,$00,$00

*=$0810

!if .RELEASE=0 {
        jmp .sync_intro
.APPLY_INTERRUPT:
        sta $D012
        stx $0314
        sty $0315
        jmp $ea81
}

; main function / subroutine
.sync_intro:
        inc VIC2InteruptStatus
;        jsr $e544
        jsr .gfx_init

        sei
        lda #$7f
        sta $dc0d     ; turn off the CIA interrupts
        sta $dd0d
        and $d011     ; clear high bit of raster line
        sta $d011
        +irqEnd $2C, .p1irq1

        lda #$01      ; enable raster interrupts
        sta $d01a
        cli
        jmp *


.p1irq1:
        inc VIC2InteruptStatus     ; acknowledge interrupt
        lda #$00
        sta $d021
        lda #$3b
        sta $d011
        lda #$18
        sta $d016
        +SetScreenAndCharLocation $0400, $2000

        lda #$77
        ldx #<.p1irq2
        ldy #>.p1irq2
        jmp C_APPLY_INTERRUPT

.p1irq2:
        inc VIC2InteruptStatus     ; acknowledge interrupt
        ldx #$03
        dex
        bne *-1
        lda #$00                   ; Center BG Color
        sta VIC2BorderColour
        lda #$0C
        sta VIC2ScreenColour
        lda #$1b
        sta $d011
        lda #$C8
        sta $d016
        +SetScreenAndCharLocation $0400, $1800

        !if .DEBUG=1 {
        inc $d020
        }
subroutine:
        jsr .detect_space
        !if .DEBUG=1 {
        dec $d020
        }

        lda #$2F
        ldx #<.p1irq1
        ldy #>.p1irq1
        jmp C_APPLY_INTERRUPT

.detect_space:
        lda #$7f               ; detect space bar
        sta CIA1KeyboardColumnJoystickA
        lda CIA1KeyboardRowsJoystickB
        and #$10
        bne +
        ldx #<.fade_down
        ldy #>.fade_down
        stx subroutine+1
        sty subroutine+2
+       rts

        ; Fadeout downward --------------------------------------------------]
        ; -------------------------------------------------------------------]
.fade_down: ; Delay
        ldx .zaehler
        inx
        cpx #.fade_speed
        bne +
        ldx #$00
        stx .zaehler
        jmp .fader
+       stx .zaehler
        rts

.fader:
        ldx .richtung
        cpx #$01
        beq .hoch
        jsr .fade_down1 ; runter
        ldx #$01
        stx .richtung ; auf UP stellen
        rts

.hoch:
        jsr .fade_up1
        ldx #$00
        stx .richtung ; auf DN stellen
        ldx .zeile_up
        cpx #.startzeile-1 ; alle 8 Zeilen fertig?
        bne +
        lda #<rt
        sta subroutine+1
        lda #>rt
        sta subroutine+2
+       rts

.richtung: !byte $00

        ; Fadeout downward --------------------------------------------------]
        ; -------------------------------------------------------------------]
.fade_down1: ; Löschen
        ldx .zeile_dn
        lda .zl,x
        sta $FB
        lda .zh,x
        sta $FC
        ldy .counter_dn
        ldx #$00
-       lda #$00
        sta ($FB),y
        lda $FB
        clc
        adc #$08
        sta $FB
        bcc +
        inc $FC
+       inx
        cpx #$28
        bne -
        iny
        iny
        sty .counter_dn
        cpy #$08
        beq +
        rts ; Eine Zeile abgearbeitet

+       ldy #$00 ; Alle Zeilen abgearbeitet
        sty .counter_dn
        ldx .zeile_dn
        inx
        stx .zeile_dn
rt:     rts


        ; Fadeout upward ----------------------------------------------------]
        ; -------------------------------------------------------------------]
.fade_up1:
        ldx .zeile_up   ; Vorher auf 8 setzen
        lda .zl,x
        sta $FB
        lda .zh,x
        sta $FC

        ldy .counter_up
        ldx #$00
-       lda #$00
        sta ($FB),y
        lda $FB
        clc
        adc #$08
        sta $FB
        bcc +
        inc $FC
+       inx
        cpx #$28
        bne -
        dey
        dey
        sty .counter_up
        cpy #$FF
        beq +
        rts ; Eine Zeile fertig

+       ldy #$07 ; Alle Zeilen abgearbeitet
        sty .counter_up
        ldx .zeile_up
        dex
        stx .zeile_up
        rts ; neu


        ; Pointer -----------------------------------------------------------]
.zl     !byte <$2000,<$2140,<$2280,<$23c0,<$2500,<$2640,<$2780,<$28C0
        !byte <$2A00,<$2B40,<$2C80,<$2DC0,<$2F00,<$3040,<$3180,<$32C0
        !byte <$3400,<$3540,<$3680,<$37C0,<$3900,<$3A40,<$3B80,<$3CC0
        !byte <$3E00
.zh     !byte >$2000,>$2140,>$2280,>$23c0,>$2500,>$2640,>$2780,>$28C0
        !byte >$2A00,>$2B40,>$2C80,>$2DC0,>$2F00,>$3040,>$3180,>$32C0
        !byte >$3400,>$3540,>$3680,>$37C0,>$3900,>$3A40,>$3B80,>$3CC0
        !byte >$3E00
.zeile_up  !byte $08
.zeile_dn  !byte $00
.counter_up: !byte $07
.counter_dn: !byte $00
.zaehler: !byte $00


.gfx_init:
        ldx #$00
-       lda .colora,x
        sta $0400,x
        lda .colora+$40,x
        sta $0440,x
        lda .colorb,x
        sta $d800,x
        lda .colorb+$40,x
        sta $D840,x
        inx
        bne -

        lda #.startzeile
        sta .zeile_dn
        lda #.endzeile
        sta .zeile_up
        lda #$00
        sta .counter_dn
        lda #$07
        sta .counter_up

        ldx .zeile_dn
        lda .zl,x
        sta $FB
        lda .zh,x
        sta $FC
        rts


*=$2000
!src "KrillDemo/xsdnite_320x200mc1.inc"
