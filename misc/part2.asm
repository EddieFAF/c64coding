
; Assembler: ACME
      *= $0801
!byte $0c,$08,$0a,$00,$9e,$32,$30,$36,$34,$00,$00,$00,$00


!src "stdlib/stdlib.a"
!src "stdlib/macros.asm"

*=$0810
            ; Vorbereitungen -------------------------------------------------]
            ; ----------------------------------------------------------------]
            lda #%01111111
            sta VIC2SpriteEnable
            ldx #$00
            ldy #$00
-           lda .sprite_xpos,x
            sta VIC2Sprite0X,y
            inx
            iny
            iny
            cpx #$08
            bne -
            lda #%01100000
            sta $d010
            ldy #$00
-           lda .sprite_ypos_new
            sta VIC2Sprite0Y,y
            iny
            iny
            cpy #$10
            bne -
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
            lda #VIC2Colour_LightBlue
            sta VIC2Sprite0Colour
            sta VIC2Sprite1Colour
            sta VIC2Sprite2Colour
            sta VIC2Sprite3Colour
            sta VIC2Sprite4Colour
            sta VIC2Sprite5Colour
            sta VIC2Sprite6Colour
            ldx #0
-           lda .sprite_line_data,X
            sta $0c00,X
            inx
            cpx #128
            bne -


start:      sei                     ; set up interrupt
            jsr $e544
            lda #$7f
            sta $dc0d               ; turn off the CIA interrupts
            sta $dd0d
            and $d011               ; clear high bit of raster line
            sta $d011

            +irqEnd $32,.irq1

            lda #$01                ; enable raster interrupts
            sta $d01a
            cli
            jmp *                   ; Endlosschleife
            rts                     ; back to BASIC

            ; Start Intro ----------------------------------------------------]
            ; ----------------------------------------------------------------]
.irq1:
.c1:        lda #$0e
            sta $d020
.c2:        lda #$06
            sta $d021

            ; Zeichensatz ausschalten
            lda #$16 ;font
            sta $d018
.c3:        lda #$1b
            sta $d011
            lda #$C8 ;#$08
            sta $d016

;            inc $d020
sub:        jsr .sprite_move1
;            dec $d020

            inc $d019     ; acknowledge interrupt
            jmp $ea31


.count      !byte $01
.sprite_pos_count:
            !byte $00

            ; Sprite Movement ------------------------------------------------]
            ; ----------------------------------------------------------------]
.sprite_move1:
            dec .count
            beq +
rt:         rts

+           ldx #$01
            stx .count
            ldy #$00
            ldx .sprite_pos_count
            lda .sprite_ypos_new,x
            sec
subtr:      sbc #$00
-           sta VIC2Sprite0Y,y
            iny
            iny
            cpy #$10
            bne -
            ldx .sprite_pos_count
            inx
            stx .sprite_pos_count
.sprite_l:  cpx #$40
            bne .spr_weiter

; Schleife abgearbeitet, Vorbereitungen für nächsten Lauf
            ldx .sprite_row         ; Aktuelle Zeile
            lda .sprite_offset,x    ; Abzug
            sta subtr+1
            lda .screen_pos_low,x   ; Bildschirm malen LOW
            sta .scr_pos+1
            lda .screen_pos_high,x  ; Bildschrim malen HIGH
            sta .scr_pos+2
            lda .sprite_start,x     ; Start in der Tabelle
            sta .sprite_pos_count
            lda #$00
            sta VIC2Sprite0Y        ; Sprites zurücksetzen
            sta VIC2Sprite1Y
            sta VIC2Sprite2Y
            sta VIC2Sprite3Y
            sta VIC2Sprite4Y
            sta VIC2Sprite5Y
            sta VIC2Sprite6Y
            sta VIC2Sprite7Y

            inx
            cpx #05
            bne +
            ldx #<.fade_out
            ldy #>.fade_out
            stx sub+1
            sty sub+2
            lda #$0e
            sta .c2+1        ; Setze Hintergrund
            lda #$00
            sta VIC2SpriteEnable ; alle Sprites ausschalten
            lda #$04
            sta .count
            lda #$00
            sta .c3+1
            rts
            ldx #$00

+           stx .sprite_row
            ldx #$00
            lda #$A0        ; Zeichen
.scr_pos:   sta $0720,x     ; Bildschirmposition
            inx
            cpx #$C8
            bne .scr_pos
.spr_weiter:
            rts

.sprite_row:
            !byte $00
.screen_pos_low:
            !byte $20, $58, $90, $C8, $00
.screen_pos_high:
            !byte $07, $06, $05, $04, $04
.sprite_offset:
            !byte 39, 79, 119, 159, 199

.sprite_start: ; Startposition in der Tabelle
            !byte $00, $08, $10, $18, $20

.sprite_length: ; Anzahl Cycles in der Tabelle
            !byte $40, $38, $30, $28, $20
.sprite_line_data:
;                        765432107654321076543210
            +SpriteLine %########################
            +SpriteLine %########################
            +SpriteLine %########################
            +SpriteLine %########################
            +SpriteLine %########################
            +SpriteLine %########################
            +SpriteLine %########################
            +SpriteLine %########################
            +SpriteLine %########################
            +SpriteLine %########################
            +SpriteLine %########################
            +SpriteLine %########################
            +SpriteLine %########################
            +SpriteLine %########################
            +SpriteLine %########################
            +SpriteLine %########################
            +SpriteLine %########################
            +SpriteLine %########################
            +SpriteLine %########################
            +SpriteLine %########################
            +SpriteLine %########################
            !byte $00

.sprite_xpos:
            !byte 24,72,120,168,216,08,56,00

PI = 3.14159265358979323846
    ; cos[0,pi/2] scaled to 0-255 range
.sprite_ypos_new
    !for x, 1 ,64 {
        !byte sin(float(x-1) / 63 * PI/2) * 208 + 0.5
    }
    ; "x-1" converts interval [1,256] to interval [0,255]
    ; "float()" makes sure this calculation is done in float mode now
    ; "/255*half_PI" converts interval [0,255] to interval [0,PI/2]
    ; "cos()" returns cosine. Wow.
    ; "*255" converts interval [0,1] to interval [0,255]
    ; "+0.5" ensures correct rounding to integer

            ; Bildschirm ausblenden -----------------------------------------]
            ; ---------------------------------------------------------------]
.fade_out:
            dec .count
            beq .fade_out_screen
            rts

.fade_count: !byte $00
.fade_out_screen:
            ldx #$08
            stx .count
            ldx .fade_count
            inx
            cpx #$10
            bne +
            lda #<rt
            sta sub+1
            lda #>rt
            sta sub+2
            ldx #$00
+           stx .fade_count

            lda .c1+1 ;hole aktuelle farbe
            and #$0f
            tax
            lda fade_table,x   ;hole fade value
            sta .c1+1          ;schreibe neuen wert

            lda .c2+1 ;hole aktuelle farbe
            and #$0f
            tax
            lda fade_table,x   ;hole fade value
            sta .c2+1          ;schreibe neuen wert
            rts

!src "stdlib/tables.asm"
