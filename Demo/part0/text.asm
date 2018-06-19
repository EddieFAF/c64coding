; ********************************
; ** Texteinblendung - Routinen **
; ********************************

screen_init:
; ********************************
; ** Setzen der Farben
; ********************************
            ldx #$00
-           lda char_colors,x
            sta $D968,x
            lda char_colors+$18,x
            sta $D980,x
            inx
            bne -
            rts

sprite_text_init:
; ********************************
; ** Spritedaten initialisieren **
; ********************************
		    ldx #0
-   	    lda sprite_text_data,X
            sta $0340,X
 		    inx 
 		    cpx #63
            bne -
            rts

sprite_text_set:
; ********************************
; ** Sprites einrichten         **
; ********************************
            lda #$03                ; Sprite 0+1 aktivieren
            sta VIC2SpriteEnable
            lda #$00                ; Schwarz
            sta VIC2Sprite0Colour   ; Farbe Sprite 0
            sta VIC2Sprite1Colour   ; Farbe Sprite 1
            lda #$0D
            sta $07f8               ; Sprite 0
            sta $07f9               ; Sprite 1
            ldx #$10                ; x
            ldy #115                ; y
            stx VIC2Sprite0X
            sty VIC2Sprite0Y
            ldx #$10
            ldy #136
            stx VIC2Sprite1X
            sty VIC2Sprite1Y
            lda #$03                ; Sprite 0+1 Ylarge
            sta VIC2SpriteDoubleHeight
            rts

sprite_text_move:
; **********************
; ** Text einblenden
; **********************
            dec spr_counter
            dec spr_counter
            bne sprite_move_loop
            ldx charpos
            cpx #$28
            bne +
            jmp raster_set_space ; Warten auf Space setzen (neu: Part laden)
+           lda char_screen,x
            sta $0568,x
            lda char_screen+40,x
            sta $0590,x
            lda char_screen+80,x
            sta $05B8,x
            lda char_screen+120,x
            sta $05E0,x
            lda char_screen+160,x
            sta $0608,x
            lda char_screen+200,x
            sta $0630,x
            lda char_screen+240,x
            sta $0658,x

            lda #$08
            sta spr_counter
            inc charpos
sprite_move_loop:
            inc VIC2Sprite0X
            inc VIC2Sprite0X
            inc VIC2Sprite1X
            inc VIC2Sprite1X
            bne +
            lda #$03
            sta VIC2SpriteXMSB
+           rts

sprite_back:
; **********************
; ** Text ausblenden
; **********************
            dec spr_counter
            dec spr_counter
            bne sprite_back_loop
            ldx charpos
            cpx #$00
            bne +
            jmp set_fadeout 
+           lda #$20
            sta $0568,x
            sta $0590,x
            sta $05B8,x
            sta $05E0,x
            sta $0608,x
            sta $0630,x
            sta $0658,x

            lda #$08
            sta spr_counter
            dec charpos
sprite_back_loop
            dec VIC2Sprite0X
            dec VIC2Sprite0X
            dec VIC2Sprite1X
            dec VIC2Sprite1X
            bpl +
            lda #$00
            sta VIC2SpriteXMSB
+           rts

sprite_set_back:
; **********************
; ** Setze Zeichen löschen
; **********************
            lda #<sprite_back
            sta sprite_change_2+1
            lda #>sprite_back
            sta sprite_change_2+2

            lda #$03
            sta VIC2SpriteEnable
            lda #$28
            sta charpos
            lda #$08
            sta spr_counter
            rts

sprite_set_rts:
; **********************
; ** setze RTS
; **********************
            lda #<sprite_set_rts_exit
            sta sprite_change_2+1
            lda #>sprite_set_rts_exit
            sta sprite_change_2+2
            lda #$00
            sta VIC2SpriteEnable ;Sprites abschalten
sprite_set_rts_exit
            rts

raster_set_space:
; **********************
; ** setze 'Warten auf Space'
; **********************
            lda #<wait_space
            sta sprite_change_2+1
            lda #>wait_space
            sta sprite_change_2+2
            lda #$00
            sta VIC2SpriteEnable
            rts

wait_space:
; **********************
; ** Warten auf Space
; **********************
            lda #$7f       ; detect space bar
            sta $dc00
            lda $dc01
            and #$10
            bne +
            jmp sprite_set_back
+           rts

; **********************
spr_counter !byte 08
charpos     !byte 00

!src "loading_text.inc"

