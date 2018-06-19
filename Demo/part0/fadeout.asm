; *****************************
; ** Fade Out
; *****************************

; **********************
; ** Setze Zeichen löschen
; **********************
set_fadeout lda #<fade_out
            sta sprite_change_2+1
            lda #>fade_out
            sta sprite_change_2+2
            lda #$00
            sta tabcount
            sta dela
            rts

fade_out    ldx dela
            inx
            stx dela
            cpx #$04
            beq col_fade
            rts

col_fade    ldx #$00
            stx dela
            ldx tabcount
            inx
            stx tabcount
            cpx #$08
            bne col_cont
            jmp sprite_set_rts

col_cont    ldx tabcount
            lda fade_tab1,x
            sta raster1a+1
            sta raster1b+1
            lda fade_tab2,x
            sta raster2+1
            lda fade_tab3,x
            sta raster3a+1
            sta raster3b+1
            rts

fade_tab1: ; Raster
            !byte $01, $0f, $0a, $08, $0c, $09, $0b, $00

fade_tab2: ; Hintergrund
            !byte $06, $0e, $03, $01, $03, $0e, $06, $00

fade_tab3: ; Rahmen
            !byte $0e, $0e, $03, $01, $03, $0e, $06, $00
