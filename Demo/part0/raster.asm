; **************************
; ** Raster - Routinen    **
; **************************

color_move  ldx dela
            inx
            stx dela
            cpx #$02
            beq col_mov
            rts
        
col_mov     ldx #$00
            stx dela
            ldx tabcount
            inx
            stx tabcount
            cpx #$21
            bne +
            jmp set_new_irq
+           ldx #$00
-           lda rowcolor1+$02,x
            sta rowcolortable,x
            inx
            cpx #$21
            bne -

            ldx #$00
-           lda rowcolor2,x
            sta rowcolortable+$21,x
            inx
            cpx #$21
            bne -
            jsr copy_tabs
            rts

;umkopieren der Tabellen
copy_tabs   ldx #$00
-           lda rowcolor1+1,x
            sta rowcolor1,x
            inx
            cpx #$22
            bne -
            lda #$00
            sta rowcolor1+$22

            ldx #$22
-           lda rowcolor2,x
            sta rowcolor2+1,x
            dex
            bpl -
            lda #$00
            sta rowcolor2
            rts


!src "spritedata.asm"

!align 255,0
delaytable
            !byte 9                            ;letzte Zeile vor der Anzeige
            !byte 2, 8, 8, 9, 9, 9, 9, 9       ; 1. Textzeile
            !byte 2, 8, 8, 9, 9, 9, 9, 10      ; 2.
            !byte 2, 8, 8, 9, 9, 9, 9, 10      ; 3.
            !byte 2, 8, 8, 9, 9, 9, 9, 9       ; 4.
            !byte 2, 8, 8, 9, 9, 9, 9, 9       ; 5.

            !byte 2, 8, 8, 9, 9, 9, 9, 9       ; 6.
            !byte 2, 8, 8, 9, 9, 9, 9, 10      ; 7.
            !byte 2, 8, 8, 9, 9, 9, 9, 10      ; 8.
            !byte 2, 8, 8, 9, 9, 9, 9, 9       ; 9.
            !byte 2, 8, 8, 9, 9, 9, 9, 9       ;10. Textzeile

            !byte 2, 8, 8, 9, 9, 9, 9, 9       ;11.
            !byte 2, 8, 8, 9, 9, 9, 9, 10      ;12.
            !byte 2, 8, 8, 9, 9, 9, 9, 10      ;13.
            !byte 2, 8, 8, 9, 9, 9, 9, 9       ;14.
            !byte 2, 8, 8, 9, 9, 9, 9, 9       ;15.

rowcolortable !fill $50,6

rowcolor1
            !byte 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6
            !byte 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6
            !byte 6, 6, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
            !byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
            !byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
            !byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
rowcolor2
            !byte 1, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6
            !byte 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6
            !byte 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6
            !byte 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6
            !byte 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6
            !byte $f6
