; **** fade in ** **
fade_in:
        ldx #$00
        stx delay
        ldx fade_count
        inx
        cpx #$10
        bne +
        lda jumptarget
        sta subroutine+1
        lda jumptarget+1
        sta subroutine+2
        ldx #$00
+       stx fade_count

        ldx fade_count2
        lda original_MC1,x ;hole zielfarbe
        asl ;und schiebe ins high nibble
        asl
        asl
        asl
        sta fade_in_offset ;setze offset
        lda mc1+1 ;hole aktuelle farbe
        and #$0f ;und isoliere low nibble (muss gemacht werden sonst kommt mist raus)
        ora fade_in_offset ; "addiere" offset
        ;akku zeigt jetzt auf tabelle targetcolor*16+currentcolor
        ;also auf die richtige zeile und spalte in der tabelle!
        tax ;akku nach X
        lda fade_table,x ;hole fade value
        sta mc1+1

        ldx fade_count2
        lda original_MC2,x ;hole zielfarbe
        asl ;und schiebe ins high nibble
        asl
        asl
        asl
        sta fade_in_offset ;setze offset
        lda mc2+1 ;hole aktuelle farbe
        and #$0f ;und isoliere low nibble (muss gemacht werden sonst kommt mist raus)
        ora fade_in_offset ; "addiere" offset
        ;akku zeigt jetzt auf tabelle targetcolor*16+currentcolor
        ;also auf die richtige zeile und spalte in der tabelle!
        tax ;akku nach X
        lda fade_table,x ;hole fade value
        sta mc2+1

        ldx fade_count2
        lda original_BG,x ;hole zielfarbe
        asl ;und schiebe ins high nibble
        asl
        asl
        asl
        sta fade_in_offset ;setze offset
        lda bg+1 ;hole aktuelle farbe
        and #$0f ;und isoliere low nibble (muss gemacht werden sonst kommt mist raus)
        ora fade_in_offset ; "addiere" offset
        ;akku zeigt jetzt auf tabelle targetcolor*16+currentcolor
        ;also auf die richtige zeile und spalte in der tabelle!
        tax ;akku nach X
        lda fade_table,x ;hole fade value
        sta bg+1
        rts


;**** fade out ****
fade_out:
        ldx #$00
        stx delay
        ldx fade_count
        inx
        cpx #$10
        bne +
        lda jumptarget
        sta subroutine+1
        lda jumptarget+1
        sta subroutine+2
        ldx #$00
+       stx fade_count

        lda mc1+1 ;hole aktuelle farbe
        and #$0f
        tax
        lda fade_table,x   ;hole fade value
        sta mc1+1          ;schreibe neuen wert

        lda mc2+1 ;hole aktuelle farbe
        and #$0f
        tax
        lda fade_table,x   ;hole fade value
        sta mc2+1          ;schreibe neuen wert

        lda bg+1 ;hole aktuelle farbe
        and #$0f
        tax
        lda fade_table,x   ;hole fade value
        sta bg+1          ;schreibe neuen wert
        rts

fade_count:
    !byte 0
; ----

; **** fade in ** **
fade_in_low:
        ldx #$00
        stx delay_low
        ldx fade_count_low
        inx
        cpx #$10
        bne +
        lda jumptarget_low
        sta subroutine2+1
        lda jumptarget_low+1
        sta subroutine2+2
        ldx #$00
+       stx fade_count_low

        ldx fade_count2_low
        lda original_MC1,x
        asl
        asl
        asl
        asl
        sta fade_in_offset_low
        lda mc1_low+1
        and #$0f
        ora fade_in_offset_low
        tax
        lda fade_table,x
        sta mc1_low+1

        ldx fade_count2_low
        lda original_MC2,x
        asl
        asl
        asl
        asl
        sta fade_in_offset_low
        lda mc2_low+1
        and #$0f
        ora fade_in_offset_low
        tax
        lda fade_table,x
        sta mc2_low+1

        ldx fade_count2_low
        lda original_BG,x
        asl
        asl
        asl
        asl
        sta fade_in_offset_low
        lda bg_low+1
        and #$0f
        ora fade_in_offset_low
        tax
        lda fade_table,x
        sta bg_low+1
        rts

;**** fade out ****
fade_out_low:
        ldx #$00
        stx delay_low
        ldx fade_count_low
        inx
        cpx #$10
        bne +
        lda jumptarget_low
        sta subroutine2+1
        lda jumptarget_low+1
        sta subroutine2+2
        ldx #$00
+       stx fade_count_low

        lda mc1_low+1
        and #$0f
        tax
        lda fade_table,x
        sta mc1_low+1

        lda mc2_low+1
        and #$0f
        tax
        lda fade_table,x
        sta mc2_low+1

        lda bg_low+1
        and #$0f
        tax
        lda fade_table,x
        sta bg_low+1
        rts

fade_count_low:
    !byte 0
; ----

fade_table:
    ;high nibble=target color
    ;low nibble =source color
    ;      0   1   2   3   4   5   6   7   8   9   a   b   c   d   e   f
    !byte $00,$0d,$09,$0c,$02,$08,$02,$0f,$02,$00,$08,$09,$04,$03,$04,$05    ;0
    !byte $06,$01,$08,$0d,$0c,$03,$0b,$01,$0a,$02,$0f,$04,$03,$01,$03,$07    ;1
    !byte $09,$07,$02,$0c,$02,$04,$0b,$0f,$02,$02,$08,$02,$04,$03,$04,$0a    ;2
    !byte $06,$0d,$04,$03,$0c,$03,$0b,$0f,$0c,$02,$0c,$04,$03,$03,$03,$03    ;3
    !byte $06,$0d,$04,$0c,$04,$0c,$0b,$0f,$04,$02,$04,$04,$04,$03,$04,$0a    ;4
    !byte $06,$0d,$08,$05,$0c,$05,$0b,$0f,$0c,$02,$0c,$04,$05,$03,$05,$05    ;5
    !byte $06,$0d,$0b,$0e,$0b,$08,$06,$0f,$0b,$0b,$08,$06,$04,$03,$04,$0c    ;6
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
fade_in_offset_low: !byte $00

; ----