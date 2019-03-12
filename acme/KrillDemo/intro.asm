;***********************************************************************
;**  Project Name: Demo Project                                       **
;**  ---------------------------------------------------------------  **
;**  Filename: intro.asm                                              **
;**  ---------------------------------------------------------------  **
;**  Author (c): [FAF]Eddie                                           **
;**  File Date: 2018-06-11                                            **
;***********************************************************************
.RELEASE = 0
.SOUNDDISPLAY = 0

!src "../../stdlib/stdlib.a"
!src "../../stdlib/macros.asm"
!src "loader.inc"

.soundinit               = $1000
.soundplay               = $1003
.startzeile              = $0568

!if .RELEASE { !set C_EXIT_PART = exit_intro_part } else { !set C_EXIT_PART = $fce2 }
!if .RELEASE { !set C_APPLY_INTERRUPT = apply_interrupt } else { !set C_APPLY_INTERRUPT = .APPLY_INTERRUPT }

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
;        +ClearScreen $4400, $00
        jsr .screen_dump
        jsr .gfx_init
        jsr .screen_init
        jsr .sprite_init
        jsr .star_init
;        lda #%00000011 ;<- Bank 0
;        sta $dd00
        lda #$00
        jsr .soundinit

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

        ; IRQ Routinen ------------------------------------------------------]
        ; -------------------------------------------------------------------]
.p1irq1:
        inc VIC2InteruptStatus     ; acknowledge interrupt
        +SetScreenAndCharLocation $0400, $2000
;       lda #$16
;       sta $d018
        lda #$18
        sta $d016
        lda #$3b
        sta $d011
        lda #$31
        cmp $d012
        bne *-3
        ldx #$0B
        dex
        bne *-1
.c10:   lda #$01
        sta VIC2BorderColour
        sta VIC2ScreenColour
        ldx #$09            ; delay
        dex
        bne *-1
.c11:   lda #$00                  ; Upper BG Color
        sta VIC2BorderColour
        sta VIC2ScreenColour

        !if .SOUNDDISPLAY=1 {
          dec VIC2BorderColour
          jsr .soundplay
          inc VIC2BorderColour
        } else {
          jsr .soundplay
        }

        jsr .sprite_move

        lda #$77
        ldx #<.p1irq2
        ldy #>.p1irq2
        jmp C_APPLY_INTERRUPT

.p1irq2:
        inc VIC2InteruptStatus     ; acknowledge interrupt
        ldx #$03
        dex
        bne *-1
.c20:   lda #$0C                   ; Center BG Color
        sta VIC2BorderColour
        sta VIC2ScreenColour
        lda #$78
        cmp $d012
        bne *-3
        ldx #$08
        dex
        bne *-1
        lda #$1b
        sta $d011
        lda #$C8
        sta $d016
        +SetScreenAndCharLocation $0400, $1800

        jsr .starfield

        lda #$B7
        ldx #<.p1irq3
        ldy #>.p1irq3
        jmp C_APPLY_INTERRUPT

.p1irq3:
        inc VIC2InteruptStatus     ; acknowledge interrupt
        ldx #$03
        dex
        bne *-1
        lda #$00                   ; lower BG Color
        sta VIC2BorderColour
        sta VIC2ScreenColour
        +SetScreenAndCharLocation $0400, $3000

        lda #$FA
        ldx #<.p1irq4
        ldy #>.p1irq4
        jmp C_APPLY_INTERRUPT

.p1irq4:
        inc VIC2InteruptStatus     ; acknowledge interrupt
        ldx #$03
        dex
        bne *-1
        lda #$01
        sta VIC2BorderColour
        sta VIC2ScreenColour
        ldx #$0A
        dex
        bne *-1
        lda #$00
        sta VIC2BorderColour
        sta VIC2ScreenColour

        lda #$7f               ; detect space bar
        sta CIA1KeyboardColumnJoystickA
        lda CIA1KeyboardRowsJoystickB
        and #$10
        bne .update_irq
;        jsr .exit_part
        jmp C_EXIT_PART

.update_irq:
        lda #$2C
        ldx #<.p1irq1
        ldy #>.p1irq1
        jmp C_APPLY_INTERRUPT

        ; Sub Routinen ------------------------------------------------------]
        ; -------------------------------------------------------------------]
.screen_dump:
        ldx #$00
-       lda #$20
        sta $0400,x
        sta $0500,x
        sta $0600,x
        sta $06E8,x
        lda #$0B
        sta $d800,x
        sta $d900,x
        sta $dA00,x
        sta $dAE8,x
        inx
        bne -
        rts

.screen_init:
        ldx #$00
-       lda .txt,x
        sta .startzeile+0*40,x       ; Zeile 1
        sta .startzeile+1*40,x       ; Zeile 2
        sta .startzeile+2*40,x       ; Zeile 3
        sta .startzeile+3*40,x       ; Zeile 4
        sta .startzeile+4*40,x       ; Zeile 5
        sta .startzeile+5*40,x       ; Zeile 6
        sta .startzeile+6*40,x       ; Zeile 7
        lda #$0b
        sta $d400+.startzeile+0*40,x
        sta $d400+.startzeile+1*40,x
        sta $d400+.startzeile+2*40,x
        sta $d400+.startzeile+3*40,x
        sta $d400+.startzeile+4*40,x
        sta $d400+.startzeile+5*40,x
        sta $d400+.startzeile+6*40,x
        inx
        cpx #$28
        bne -
        rts

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
        rts

.star_init:
        ldx #$00
.copyscreen
        lda stardata,x
        sta $06A8,x
        lda stardata+24,x
        sta $06A8+24,x
        lda #$0c ;Light grey stars for now
        sta $dAA8,x
        sta $dAA8+24,x
        inx
        bne .copyscreen
        rts

.sprite_init:
        ldx #$00
        ldy #$00
-       lda .xcord,x
        sta $d000,y
        lda .ycord,x
        sta $d001,y
        iny
        iny
        txa
        clc
        adc #$08
        tax
        cpy #$10
        bne -

        lda #$ff
        sta $d015
        ldx #$00
        ldy #$d4
-       tya
        sta $07f8,x
        iny
        inx
        cpx #$08
        bne -
        rts

.sprite_move:
        ldx .counter
        inx
        cpx #$04
        beq .sprite_update
        stx .counter
        rts

.sprite_update:
        ldx #$00
        stx .counter
        ldx .spr_pos_x
        lda .xcord,x
        clc
        adc #24*0
        sta $d000
        ror $d010
        clc
        lda .xcord,x
        adc #24*1
        sta $d002
        ror $d010
        clc
        lda .xcord,x
        adc #24*2
        sta $d004
        ror $d010
        clc
        lda .xcord,x
        adc #24*3
        sta $d006
        ror $d010
        clc
        lda .xcord,x
        adc #24*4
        sta $d008
        ror $d010
        clc
        lda .xcord,x
        adc #24*5
        sta $d00a
        ror $d010
        clc
        lda .xcord,x
        adc #24*6
        sta $d00c
        ror $d010
        clc
        lda .xcord,x
        adc #24*7
        sta $d00e
        ror $d010

;        lsr $d010 ; we only use six sprites, so $d010
;        lsr $d010 ; needs to be shifted to the right 2 times
        inc .spr_pos_x

        ; Y Coordinates -----------------------------------------------------]
        ldx .spr_pos_y
        lda .ycord,x
        sta $d001
        inx
        inx
        inx
        inx
        lda .ycord,x
        sta $d003
        inx
        inx
        inx
        inx
        lda .ycord,x
        sta $d005
        inx
        inx
        inx
        inx
        lda .ycord,x
        sta $d007
        inx
        inx
        inx
        inx
        lda .ycord,x
        sta $d009
        inx
        inx
        inx
        inx
        lda .ycord,x
        sta $d00b
        inx
        inx
        inx
        inx
        lda .ycord,x
        sta $d00d
        inx
        inx
        inx
        inx
        lda .ycord,x
        sta $d00f

        inc .spr_pos_y
        rts

.spr_pos_x !byte $00
.spr_pos_y !byte $00

.counter !byte $00

        ; Star field Source -------------------------------------------------]
        ; -------------------------------------------------------------------]
.starfield
        inc .starcontrol
        lda .starcontrol
        cmp #$03
        beq .doscroll
        rts
.doscroll
        lda #0
        sta .starcontrol

        ldx #0
        jsr .starscroll
        jsr .starscroll
        ldx #1
        jsr .starscroll
        ldx #2
        jsr .starscroll
        jsr .starscroll
        jsr .starscroll
        ldx #3
        jsr .starscroll
        ldx #4
        jsr .starscroll
        jsr .starscroll
        jsr .starscroll
        ldx #5
        jsr .starscroll
        jsr .starscroll
        ldx #6
        jsr .starscroll
        ldx #7
        jsr .starscroll
        jsr .starscroll
        rts

.starscroll
        lda $3220,x
        rol $3208,x
        rol $3210,x
        rol $3218,x
        rol $3220,x
        sta $3208,x
        rts

.starcontrol !byte $00

        ; The starfield char data here. Hell yeah! --------------------------]
        ; That's what we want :o) -------------------------------------------]
        ; -------------------------------------------------------------------]

;                    *=$4000
stardata
!text "BCDABCDABCDABCDABCDABCDABCDABCDABCDABCDA"
!text "CDABCDABCDABCDABCDABCDABCDABCDABCDABCDAB"
!text "ABCDABCDABCDABCDABCDABCDABCDABCDABCDABCD"
!text "DABCDABCDABCDABCDABCDABCDABCDABCDABCDABC"
!text "CDABCDABCDABCDABCDABCDABCDABCDABCDABCDAB"
!text "ABCDABCDABCDABCDABCDABCDABCDABCDABCDABCD"
!text "BCDABCDABCDABCDABCDABCDABCDABCDABCDABCDA"

*=$1000
!bin "Nightshift.sid",,$7e

        * = $3200

        ;Shift + * (Blank it)
        !byte %00000000 ;Weird eh? Those are for the stars
        !byte %00000000
        !byte %00000000
        !byte %00000000
        !byte %00000000
        !byte %00000000
        !byte %00000000
        !byte %00000000
        ;Shift + A
        !byte %00000000
        !byte %00000000
        !byte %00000000
        !byte %00000000
        !byte %00100000
        !byte %00000000
        !byte %00000000
        !byte %00000000
        !byte %01000000
        ;Shift + B
        !byte %00000000
        !byte %00000000
        !byte %00010000
        !byte %00000000
        !byte %00000000
        !byte %00000000
        !byte %00000100
        !byte %00000000
        ;Shift + C
        !byte %00000000
        !byte %01000000
        !byte %00000000
        !byte %00000000
        !byte %00000000
        !byte %00000000
        !byte %00001000
        !byte %00000000
        ;Shift + D
        !byte %00000010
        !byte %00000000
        !byte %00000000
        !byte %00010000
        !byte %00000000
        !byte %00000000
        !byte %00000000

.ycord:
!byte $ce,$cf,$d1,$d2,$d4,$d5,$d7,$d8
!byte $da,$db,$dc,$dd,$df,$e0,$e0,$e1
!byte $e2,$e2,$e3,$e3,$e3,$e3,$e3,$e3
!byte $e3,$e3,$e2,$e2,$e1,$e0,$df,$de
!byte $dd,$dc,$da,$d9,$d8,$d6,$d5,$d3
!byte $d2,$d0,$ce,$cd,$cb,$c9,$c8,$c6
!byte $c5,$c3,$c2,$c1,$bf,$be,$bd,$bc
!byte $bb,$ba,$b9,$b9,$b8,$b8,$b8,$b8
!byte $b8,$b8,$b8,$b8,$b9,$b9,$ba,$bb
!byte $bb,$bc,$be,$bf,$c0,$c1,$c3,$c4
!byte $c6,$c7,$c9,$ca,$cc,$ce,$cf,$d1
!byte $d2,$d4,$d5,$d7,$d8,$da,$db,$dc
!byte $dd,$df,$e0,$e0,$e1,$e2,$e2,$e3
!byte $e3,$e3,$e3,$e3,$e3,$e3,$e3,$e2
!byte $e2,$e1,$e0,$df,$de,$dd,$dc,$da
!byte $d9,$d8,$d6,$d5,$d3,$d2,$d0,$ce
!byte $cd,$cb,$c9,$c8,$c6,$c5,$c3,$c2
!byte $c1,$bf,$be,$bd,$bc,$bb,$ba,$b9
!byte $b9,$b8,$b8,$b8,$b8,$b8,$b8,$b8
!byte $b8,$b9,$b9,$ba,$bb,$bb,$bc,$be
!byte $bf,$c0,$c1,$c3,$c4,$c6,$c7,$c9
!byte $ca,$cc,$ce,$cf,$d1,$d2,$d4,$d5
!byte $d7,$d8,$da,$db,$dc,$dd,$df,$e0
!byte $e0,$e1,$e2,$e2,$e3,$e3,$e3,$e3
!byte $e3,$e3,$e3,$e3,$e2,$e2,$e1,$e0
!byte $df,$de,$dd,$dc,$da,$d9,$d8,$d6
!byte $d5,$d3,$d2,$d0,$ce,$cd,$cb,$c9
!byte $c8,$c6,$c5,$c3,$c2,$c1,$bf,$be
!byte $bd,$bc,$bb,$ba,$b9,$b9,$b8,$b8
!byte $b8,$b8,$b8,$b8,$b8,$b8,$b9,$b9
!byte $ba,$bb,$bb,$bc,$be,$bf,$c0,$c1
!byte $c3,$c4,$c6,$c7,$c9,$ca,$cc,$cd

.xcord:
!byte $59,$54,$4f,$4b,$46,$42,$3d,$39
!byte $35,$31,$2e,$2b,$28,$25,$22,$20
!byte $1e,$1c,$1b,$1a,$1a,$19,$19,$1a
!byte $1b,$1c,$1d,$1f,$21,$23,$26,$29
!byte $2c,$30,$33,$37,$3b,$40,$44,$48
!byte $4d,$52,$56,$5b,$5f,$64,$69,$6d
!byte $71,$76,$7a,$7e,$81,$85,$88,$8b
!byte $8e,$90,$92,$94,$95,$96,$97,$98
!byte $98,$97,$97,$96,$95,$93,$91,$8f
!byte $8c,$89,$86,$83,$80,$7c,$78,$74
!byte $6f,$6b,$66,$62,$5d,$59,$54,$4f
!byte $4b,$46,$42,$3d,$39,$35,$31,$2e
!byte $2b,$28,$25,$22,$20,$1e,$1c,$1b
!byte $1a,$1a,$19,$19,$1a,$1b,$1c,$1d
!byte $1f,$21,$23,$26,$29,$2c,$30,$33
!byte $37,$3b,$40,$44,$48,$4d,$52,$56
!byte $5b,$5f,$64,$69,$6d,$71,$76,$7a
!byte $7e,$81,$85,$88,$8b,$8e,$90,$92
!byte $94,$95,$96,$97,$98,$98,$97,$97
!byte $96,$95,$93,$91,$8f,$8c,$89,$86
!byte $83,$80,$7c,$78,$74,$6f,$6b,$66
!byte $62,$5d,$59,$54,$4f,$4b,$46,$42
!byte $3d,$39,$35,$31,$2e,$2b,$28,$25
!byte $22,$20,$1e,$1c,$1b,$1a,$1a,$19
!byte $19,$1a,$1b,$1c,$1d,$1f,$21,$23
!byte $26,$29,$2c,$30,$33,$37,$3b,$40
!byte $44,$48,$4d,$52,$56,$5b,$5f,$64
!byte $69,$6d,$71,$76,$7a,$7e,$81,$85
!byte $88,$8b,$8e,$90,$92,$94,$95,$96
!byte $97,$98,$98,$97,$97,$96,$95,$93
!byte $91,$8f,$8c,$89,$86,$83,$80,$7c
!byte $78,$74,$6f,$6b,$66,$62,$5d,$59

*=$3500

spritedata:
;sprite_1
+SpriteLine %........................
+SpriteLine %######.........######...
+SpriteLine %######.........######...
+SpriteLine %#######.......#######...
+SpriteLine %..######.....######.....
+SpriteLine %...######...######......
+SpriteLine %...######...######......
+SpriteLine %.....###########........
+SpriteLine %......#########.........
+SpriteLine %.....###########........
+SpriteLine %...######...######......
+SpriteLine %...######...######......
+SpriteLine %..#######...#######.....
+SpriteLine %#######.......#######...
+SpriteLine %######.........######...
+SpriteLine %######.........######...
+SpriteLine %######.........######...
+SpriteLine %######.........######...
+SpriteLine %######.........######...
+SpriteLine %........................
+SpriteLine %........................
!byte $03

;sprite_2
+SpriteLine %........................
+SpriteLine %....#################...
+SpriteLine %...##################...
+SpriteLine %..###################...
+SpriteLine %#######.................
+SpriteLine %######..................
+SpriteLine %#######.................
+SpriteLine %..###############.......
+SpriteLine %...###############......
+SpriteLine %....###############.....
+SpriteLine %..............#######...
+SpriteLine %...............######...
+SpriteLine %...............######...
+SpriteLine %######.........######...
+SpriteLine %######.........######...
+SpriteLine %#######.......#######...
+SpriteLine %..#################.....
+SpriteLine %...###############......
+SpriteLine %...##############.......
+SpriteLine %........................
+SpriteLine %........................
!byte $0d

;sprite_3
+SpriteLine %........................
+SpriteLine %#############...........
+SpriteLine %###############.........
+SpriteLine %################........
+SpriteLine %######.....######.......
+SpriteLine %######......######......
+SpriteLine %######......#######.....
+SpriteLine %######........######....
+SpriteLine %######.........######...
+SpriteLine %######.........######...
+SpriteLine %######.........######...
+SpriteLine %######.........######...
+SpriteLine %######.........######...
+SpriteLine %######........######....
+SpriteLine %######......#######.....
+SpriteLine %######......######......
+SpriteLine %######.....######.......
+SpriteLine %################........
+SpriteLine %###############.........
+SpriteLine %#############...........
+SpriteLine %........................
+SpriteLine %........................
!byte $0d

;sprite_4
+SpriteLine %........................
+SpriteLine %######...########.......
+SpriteLine %######...#########......
+SpriteLine %######...##########.....
+SpriteLine %############...######...
+SpriteLine %############...######...
+SpriteLine %############...######...
+SpriteLine %##########.....######...
+SpriteLine %#########......######...
+SpriteLine %#########......######...
+SpriteLine %#######........######...
+SpriteLine %######.........######...
+SpriteLine %######.........######...
+SpriteLine %######.........######...
+SpriteLine %######.........######...
+SpriteLine %######.........######...
+SpriteLine %######.........######...
+SpriteLine %######.........######...
+SpriteLine %######.........######...
+SpriteLine %........................
+SpriteLine %........................
!byte $0d

;sprite_5
+SpriteLine %........................
+SpriteLine %.......##########.......
+SpriteLine %......############......
+SpriteLine %.......##########.......
+SpriteLine %.........######.........
+SpriteLine %.........######.........
+SpriteLine %.........######.........
+SpriteLine %.........######.........
+SpriteLine %.........######.........
+SpriteLine %.........######.........
+SpriteLine %.........######.........
+SpriteLine %.........######.........
+SpriteLine %.........######.........
+SpriteLine %.........######.........
+SpriteLine %.........######.........
+SpriteLine %.........######.........
+SpriteLine %.......##########.......
+SpriteLine %......############......
+SpriteLine %.......##########.......
+SpriteLine %........................
+SpriteLine %........................
!byte $0d

;sprite_6
+SpriteLine %........................
+SpriteLine %#################.......
+SpriteLine %##################......
+SpriteLine %###################.....
+SpriteLine %..............#######...
+SpriteLine %...............######...
+SpriteLine %...............######...
+SpriteLine %...............######...
+SpriteLine %...............######...
+SpriteLine %...............######...
+SpriteLine %...............######...
+SpriteLine %...............######...
+SpriteLine %...............######...
+SpriteLine %...............######...
+SpriteLine %...............######...
+SpriteLine %...............######...
+SpriteLine %...............######...
+SpriteLine %...............######...
+SpriteLine %...............######...
+SpriteLine %........................
+SpriteLine %........................
!byte $0d

;sprite_7
+SpriteLine %........................
+SpriteLine %....#################...
+SpriteLine %...##################...
+SpriteLine %..###################...
+SpriteLine %#######.................
+SpriteLine %######..................
+SpriteLine %######..................
+SpriteLine %###############.........
+SpriteLine %###############.........
+SpriteLine %###############.........
+SpriteLine %######..................
+SpriteLine %######..................
+SpriteLine %######..................
+SpriteLine %######..................
+SpriteLine %######..................
+SpriteLine %#######.................
+SpriteLine %..###################...
+SpriteLine %...##################...
+SpriteLine %....#################...
+SpriteLine %........................
+SpriteLine %........................
!byte $0d

.spr_x:
        !byte $50,$20,$FF,$88,$90,$a0,$a4,$c0
.spr_y:
        !byte $d0,$d6,$b8,$d8,$db,$e2,$e4,$e0


.txt:
!ct scr
        ;      1234567890123456789012345678901234567890
        !text "  FAF World Domination proudly present  "

*=$2000
!src "xsdnite_320x200mc1.inc"
