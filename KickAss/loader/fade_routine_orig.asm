; **** fade in ** **
fade_in:
        lda #8
        sta fade_count
fade_in_loop:
        ldy #$ff ; 255 bytes zu copieren
        sty $02 ;zwischen speichern

        lda #00
        sta dst_color_ptr ; setze zp pointer auf farbram
        lda #$d8
        sta dst_color_ptr+1
        lda current_screen_color_ptr ; hole pointer auf ziehlfarben und setze zp pointer
          ;die ziehlfarben sind hier der farbbuffer der ja eh irgendwo im speicher stehn
        sta src_color_ptr
        lda current_screen_color_ptr+1
        sta src_color_ptr+1


fade_in_set_colors:
        lda (src_color_ptr),y ;hole zielfarbe
        asl ;und schiebe ins high nibble
        asl
        asl
        asl
        sta fade_in_offset ;setze offset
        lda (dst_color_ptr),y ;hole aktuelle farbe im farbram
        and #$0f ;und isoliere low nibble (muss geamcht werden sonst kommt mist raus)
        ora fade_in_offset ; "addiere" offset
        ;akku zeigt jetzt auf tabelle targetcollor*16+currentcolor
        ;also auf die richtige zeile und spalte in der tabelle!
        tay ;akku nach Y
        lda fade_table,y ;hole fade value
        ldy $02; Y wiederherstellen
        sta (dst_color_ptr),y ; screibe neuen farbwert
        ldy $02
        dey
        bne fade_in_set_colors ;noch nich 255 byte kopiert
        dec fade_count
        bne fade_loop ; noch nich 8 frames gefaded
        rts

;**** fade out **** ****
fade_out:
        ldx #8
        stx fade_count ; wieviele frames um auszu faden
        lda #0
        sta src_color_ptr ; setze zp pointer auf farbram
        lda #$d8
        sta src_color_ptr+1
-    
        ldy #$ff                          ;Schleifenzähler (255 Zeichen je Page)

fade_out_loop:    
        lda $d800,y ;hole aktuelle farbe
        and #$0f
        tax
        lda fade_table,x   ;hole fade value
        sta $d800,y          ;schreibe neuen wert
        dey                               ;Schleifenzähler verringern
        bne fade_out_loop                        ;Solange größer 0 -> @loop:
        dec fade_count
        bne -
        lda #0
        sta fade_count
        rts
fade_tmp:
        !byte 0
fade_count:
        !byte 0
; ----

fade_table:
    ;high nibble=target color
    ;low nibble =source color
    ;       0   1   2  3    4   5   6   7   8   9   a   b   c   d   e  f
    !byte $00,$0d,$09,$0c,$02,$08,$02,$0f,$02,$00,$08,$09,$04,$03,$04,$05    ;0
    !byte $06,$01,$08,$0d,$0c,$03,$0b,$01,$0a,$02,$0f,$04,$03,$01,$03,$07    ;1
    !byte $09,$07,$02,$0c,$02,$04,$0b,$0f,$02,$02,$08,$02,$04,$03,$04,$0a    ;2
    !byte $06,$0d,$04,$03,$0c,$03,$0b,$0f,$0c,$02,$0c,$04,$03,$03,$03,$03    ;3
    !byte $06,$0d,$04,$0c,$04,$0c,$0b,$0f,$04,$02,$04,$04,$04,$03,$04,$0a    ;4
    !byte $06,$0d,$08,$05,$0c,$05,$0b,$0f,$0c,$02,$0c,$04,$05,$03,$05,$05    ;5
    !byte $06,$0d,$0b,$0e,$0b,$08,$06,$0f,$0b,$0b,$08,$06,$04,$03,$04,$0c;    ;6
    !byte $09,$07,$08,$0f,$0a,$0f,$0b,$07,$0a,$02,$0f,$04,$0f,$07,$0f,$07    ;7
    !byte $09,$07,$08,$0c,$08,$0c,$0b,$0f,$08,$02,$08,$08,$08,$03,$04,$0c    ;8
    !byte $09,$07,$09,$0c,$02,$08,$0b,$0f,$02,$09,$08,$09,$08,$03,$04,$0c    ;9
    !byte $09,$07,$04,$0c,$0a,$0c,$0b,$0f,$0a,$02,$0a,$04,$0a,$0f,$0c,$0a    ;a
    !byte $06,$0d,$0b,$0e,$0b,$08,$0b,$0f,$0b,$0b,$08,$0b,$04,$03,$04,$0c    ;b
    !byte $06,$0d,$08,$0c,$0c,$0c,$0b,$0f,$0c,$02,$0c,$04,$0c,$03,$0c,$0c    ;c
    !byte $09,$0d,$08,$0d,$0c,$03,$0b,$0d,$0c,$02,$0f,$04,$0f,$0d,$03,$0d    ;d
    !byte $06,$0d,$04,$0e,$0c,$0e,$0b,$0f,$0c,$02,$0c,$04,$0e,$03,$0e,$0e    ;e
    !byte $06,$0d,$08,$0f,$0c,$0f,$0b,$0f,$0c,$02,$0f,$04,$0f,$0f,$0f,$0f    ;f
fade_in_offset:
    !byte 0
; ----
