
; ----------------------------------------------------------------------#
;       FCG             /      TLC           /         RWE              #
; FLASH CRACKING GROUP  /  THE LIGHT CIRCLE  /  RADWAR ENTERPRISES      #
;                                           1941 <-> 1985 <-> 2015      #
;                                                                       #
;         G R A N D D A D D Y   C O D I N G   S E N I O R S             #
;                                                                       #
; fcg/rwe/tlc/gcs   resource preservation project (rpp)                 #
;                                    done by -duke!         /\          #
;                                                          /  \         #
;               #########                                 /\   \        #
; created using c64reasm3 on jan. 29th, 2015             /  \   \       #
;               #########                               /\   \   \      #
; prj: the popular dynamic duo intro by flash/fcg      /  \   \   \     #
;   reverse engineered by duke! to test c64reasm3     /____\___\___\    #
; prj. no.: 19                                                          #
;                                                                       #
; jmp in adress: $111b                                                  #
; ----------------------------------------------------------------------#
;   manually translated into ACME in a rush (few minutes) by TheRyk/MYD
; ----------------------------------------------------------------------#
!to "DynamicDuoIntro.prg", cbm
!sl "DynamicDuoIntro_labels.txt"

*= $0800
!byte $00,$0c,$08,$0a,$00,$9e,$38,$31,$39,$32,$00,$00,$00,$00; SYS 4379
;      * = $0801 "Basic Upstart"
;     :BasicUpstart(FakeEntry) ; 10 sys$0810
; -----------------------------------------------------------------------------#

      * = $0810
label0810:
    !byte $70, $6C, $64, $7C, $66, $63, $7E, $00
    !byte $30, $68, $60, $60, $60, $71, $1E, $00
    !byte $70, $78, $6C, $66, $63, $63, $7E, $00
    !byte $38, $60, $60, $7C, $60, $70, $1F, $00
    !byte $1C, $30, $30, $3E, $30, $30, $30, $00
    !byte $18, $34, $60, $60, $66, $73, $1E, $00
    !byte $6C, $6C, $66, $7E, $63, $63, $63, $00
    !byte $10, $10, $18, $18, $1C, $1C, $1C, $00
    !byte $7E, $18, $0C, $06, $63, $63, $3E, $00
    !byte $64, $6C, $78, $70, $78, $6E, $63, $00
    !byte $40, $40, $60, $60, $70, $70, $7F, $00
    !byte $64, $6C, $7E, $7A, $6B, $63, $63, $00
    !byte $6C, $6C, $76, $76, $6B, $67, $63, $00
    !byte $1C, $2E, $66, $63, $63, $73, $1E, $00
    !byte $38, $36, $33, $33, $3E, $30, $30, $00
    !byte $1C, $2E, $66, $63, $65, $72, $1D, $00
    !byte $70, $6C, $66, $7C, $78, $6C, $63, $00
    !byte $30, $68, $60, $3C, $03, $73, $3E, $00
    !byte $7E, $6C, $0C, $06, $06, $07, $07, $00
    !byte $6C, $6C, $66, $66, $63, $73, $3E, $00
    !byte $66, $66, $66, $36, $36, $1E, $0C, $00
    !byte $6C, $66, $66, $6B, $6B, $7F, $32, $00
    !byte $6C, $6C, $38, $3C, $66, $63, $63, $00
    !byte $66, $66, $36, $3C, $1C, $0C, $0C, $00
    !byte $7C, $6C, $18, $30, $30, $60, $7F, $00
    !byte $3C, $30, $30, $18, $18, $18, $1E, $00
    !byte $0C, $12, $30, $7C, $30, $62, $FC, $00
    !byte $3C, $0C, $0C, $06, $06, $06, $1E, $00
    !byte $00, $18, $3C, $7E, $18, $18, $18, $18
    !byte $00, $10, $30, $7F, $7F, $30, $10, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $0E, $0E, $0C, $08, $08, $20, $30, $00
    !byte $66, $66, $22, $00, $00, $00, $00, $00
    !byte $6C, $6C, $7F, $36, $7F, $1B, $1B, $00
    !byte $18, $3E, $60, $3C, $06, $7C, $18, $00
    !byte $66, $6E, $0C, $18, $30, $73, $63, $00
    !byte $3C, $66, $3C, $38, $67, $66, $3F, $00
    !byte $0C, $0C, $18, $00, $00, $00, $00, $00
    !byte $0C, $18, $30, $30, $18, $0C, $06, $00
    !byte $30, $18, $0C, $06, $06, $0C, $18, $00
    !byte $00, $66, $3C, $0C, $1E, $33, $00, $00
    !byte $00, $18, $18, $7F, $0C, $0C, $00, $00
    !byte $00, $00, $00, $00, $00, $18, $18, $08
    !byte $00, $00, $00, $7F, $70, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $18, $18, $00
    !byte $00, $06, $0C, $18, $18, $30, $60, $00
    !byte $7F, $63, $63, $63, $73, $73, $7F, $00
    !byte $18, $18, $18, $1C, $1C, $1C, $1C, $00
    !byte $7F, $73, $03, $7F, $60, $70, $7F, $00
    !byte $7F, $73, $03, $1E, $03, $77, $7F, $00
    !byte $60, $60, $70, $76, $7F, $0E, $0E, $00
    !byte $7F, $60, $70, $7F, $03, $73, $7F, $00
    !byte $7F, $67, $70, $7F, $63, $73, $7F, $00
    !byte $7F, $73, $03, $03, $07, $07, $07, $00
    !byte $7F, $63, $73, $3E, $63, $73, $7F, $00
    !byte $7F, $63, $73, $7F, $03, $73, $7F, $00
    !byte $00, $00, $18, $18, $00, $0C, $0C, $00
    !byte $00, $00, $18, $18, $00, $0C, $0C, $04
    !byte $0E, $18, $30, $30, $18, $0C, $07, $00
    !byte $00, $7F, $70, $00, $7F, $70, $00, $00
    !byte $70, $18, $0E, $07, $06, $0C, $38, $00
    !byte $1E, $27, $03, $0E, $18, $20, $30, $18
    !byte $55, $A6, $99, $A6, $99, $A6, $99, $A6
    !byte $55, $66, $99, $66, $99, $66, $AA, $80
    !byte $55, $66, $99, $66, $99, $66, $A9, $06
    !byte $00, $50, $94, $64, $9A, $66, $9A, $66
    !byte $05, $16, $19, $16, $19, $16, $19, $16
    !byte $40, $60, $A0, $60, $A0, $60, $A0, $60
    !byte $40, $60, $A0, $60, $A1, $61, $A1, $61
    !byte $01, $16, $59, $66, $99, $66, $99, $66
    !byte $00, $A0, $98, $68, $9A, $66, $9A, $66
    !byte $00, $01, $05, $06, $19, $16, $19, $16
    !byte $15, $66, $99, $66, $99, $66, $9A, $68
    !byte $55, $66, $99, $66, $99, $66, $AA, $00
    !byte $50, $6A, $99, $66, $99, $66, $99, $66
    !byte $00, $00, $80, $80, $A1, $61, $A1, $61
    !byte $54, $66, $9A, $66, $9A, $66, $A8, $00
    !byte $99, $A6, $99, $A6, $99, $A6, $99, $A6
    !byte $80, $80, $80, $80, $80, $80, $80, $80
    !byte $01, $01, $01, $01, $01, $01, $01, $01
    !byte $9A, $66, $9A, $66, $9A, $66, $9A, $66
    !byte $19, $16, $19, $16, $19, $16, $05, $06
    !byte $A0, $60, $98, $65, $99, $66, $99, $66
    !byte $00, $00, $00, $55, $99, $66, $99, $66
    !byte $19, $16, $59, $66, $99, $66, $99, $66
    !byte $A1, $61, $A1, $61, $A1, $61, $81, $81
    !byte $19, $16, $19, $16, $19, $16, $19, $16
    !byte $A0, $60, $A0, $65, $99, $66, $99, $66
    !byte $19, $16, $19, $56, $99, $66, $99, $66
    !byte $A1, $61, $A1, $61, $A1, $61, $A1, $61
    !byte $00, $05, $19, $16, $19, $16, $19, $16
    !byte $01, $41, $A1, $61, $A1, $61, $A1, $61
    !byte $80, $80, $80, $80, $80, $80, $80, $55
    !byte $01, $01, $01, $01, $01, $01, $05, $56
    !byte $01, $00, $00, $00, $00, $00, $00, $00
    !byte $59, $15, $01, $01, $01, $01, $01, $01
    !byte $99, $66, $9A, $66, $9A, $66, $9A, $66
    !byte $9A, $A0, $00, $00, $00, $00, $00, $00
    !byte $99, $6A, $A0, $60, $A0, $60, $A0, $60
    !byte $99, $AA, $00, $00, $00, $00, $00, $00
    !byte $99, $A6, $19, $16, $19, $16, $19, $16
    !byte $9A, $66, $9A, $66, $9A, $66, $99, $66
    !byte $00, $00, $00, $00, $00, $00, $40, $55
    !byte $00, $00, $00, $00, $00, $00, $00, $55
    !byte $00, $00, $00, $00, $00, $00, $00, $54
    !byte $99, $A6, $99, $A6, $99, $AA, $00, $00
    !byte $99, $66, $99, $66, $99, $AA, $00, $00
    !byte $9A, $66, $98, $68, $A0, $00, $00, $00
    !byte $01, $01, $01, $01, $01, $00, $00, $00
    !byte $9A, $66, $9A, $66, $9A, $A8, $00, $00
    !byte $19, $16, $19, $16, $19, $0A, $00, $00
    !byte $A0, $60, $A0, $60, $A0, $80, $00, $00
    !byte $A1, $61, $A1, $61, $A1, $80, $00, $00
    !byte $A1, $61, $A0, $60, $A0, $80, $00, $00
    !byte $99, $66, $59, $66, $15, $02, $00, $00
    !byte $00, $05, $0A, $09, $0A, $09, $0A, $09
    !byte $00, $55, $66, $99, $66, $99, $66, $9A
    !byte $00, $55, $66, $99, $66, $99, $66, $AA
    !byte $00, $50, $65, $99, $66, $99, $66, $99
    !byte $00, $00, $01, $41, $41, $A1, $61, $A1
    !byte $00, $54, $66, $9A, $66, $9A, $66, $9A
    !byte $00, $00, $01, $01, $01, $01, $01, $01
    !byte $00, $00, $01, $05, $06, $19, $16, $19
    !byte $00, $15, $66, $99, $66, $99, $66, $9A
    !byte $00, $00, $00, $40, $40, $A0, $60, $A0
    !byte $0A, $09, $0A, $09, $0A, $09, $0A, $09
    !byte $68, $98, $68, $98, $68, $98, $68, $98
    !byte $66, $19, $16, $19, $16, $19, $16, $19
    !byte $61, $A1, $61, $A1, $61, $A1, $61, $A1
    !byte $66, $9A, $66, $9A, $66, $9A, $66, $9A
    !byte $16, $19, $16, $19, $16, $19, $16, $19
    !byte $68, $A0, $60, $A0, $60, $A0, $60, $A0
    !byte $60, $A0, $60, $A0, $60, $A0, $60, $A0
    !byte $16, $19, $16, $19, $16, $19, $16, $59
    !byte $66, $9A, $66, $9A, $66, $9A, $66, $99
    !byte $00, $00, $00, $00, $00, $00, $00, $80
    !byte $01, $01, $01, $01, $01, $01, $01, $05
    !byte $60, $A0, $60, $A0, $60, $A0, $60, $98
    !byte $0A, $09, $0A, $09, $0A, $09, $0A, $00
    !byte $65, $99, $66, $99, $66, $99, $AA, $00
    !byte $55, $99, $66, $99, $66, $99, $AA, $00
    !byte $66, $99, $66, $99, $66, $9A, $A0, $00
    !byte $61, $A1, $61, $80, $80, $00, $00, $00
    !byte $66, $99, $66, $59, $66, $15, $02, $00
    !byte $56, $99, $66, $99, $66, $99, $AA, $00
    !byte $66, $9A, $66, $98, $68, $A0, $00, $00
    !byte $16, $19, $16, $05, $06, $01, $00, $00
    !byte $65, $99, $66, $99, $66, $59, $2A, $00
    !byte $60, $A0, $60, $80, $80, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00

; -----------------------------------------------------------------------------#
label0cc0:
        ldx $54
        ldy #$00
label0cc4:
        lda label0810-$10,x
        sta label0fc0,y
        inx
        iny
        iny
        iny
        cpy #$18
        bne label0cc4
        rts
; -----------------------------------------------------------------------------#
label0cd3:
        ldy #$00
        ldx #$3c
-
        clc
        rol label0fc0+2,x
        rol label0fc0+1,x
        rol label0fc0+0,x
        rol label0f80+2,x
        rol label0f80+1,x
        rol label0f80+0,x
        rol label0f40+2,x
        rol label0f40+1,x
        rol label0f40+0,x
        rol label0f00+2,x
        rol label0f00+1,x
        rol label0f00+0,x
        rol label0ec0+2,x
        rol label0ec0+1,x
        rol label0ec0+0,x
        rol label0e80+2,x
        rol label0e80+1,x
        rol label0e80+0,x
        rol label0e40+2,x
        rol label0e40+1,x
        rol label0e40+0,x
        rol label0e00+2,x
        rol label0e00+1,x
        rol label0e00+0,x
        rol label0dc0+2,x
        rol label0dc0+1,x
        rol label0dc0+0,x
        dex
        dex
        dex
        iny
        cpy #$15
        bne -
        inc $55
        lda $55
        cmp #$08
        beq +
        jmp label0d7a
; -----------------------------------------------------------------------------#
+
        lda #$00
        sta $55
        ldy #$00
label0d42:
        lda ($ac),y
        cmp #$ff
        beq label0d5c
        clc
        asl
        asl
        asl
        sta $54
        bcs label0d6f
        jsr label0d67
label0d53:
        inc $ac
        bne label0d59
        inc $ad
label0d59:
        jmp label0d7a
; -----------------------------------------------------------------------------#
label0d5c:
        lda #$9c
        sta $ac
        lda #$12
        sta $ad
        jmp label0d42
; -----------------------------------------------------------------------------#
label0d67:
        lda #$08
        sta label0cc4+2
        jmp label0cc0
; -----------------------------------------------------------------------------#
label0d6f:
        lda #$09
        sta label0cc4+2
        jsr label0cc0
        jmp label0d53
; -----------------------------------------------------------------------------#
label0d7a:
        jmp label0d90
; -----------------------------------------------------------------------------#
    !byte $00
    !byte $00
    !byte $00
; -----------------------------------------------------------------------------#
; sprite x/y positions for scroller
label0d80:
    !byte $18, $f0, $48, $f0, $78, $f0, $A8, $f0
    !byte $D8, $f0, $08, $f0, $38, $f0, $68, $f0
; -----------------------------------------------------------------------------#
label0d90:
        lda $dc01
        cmp #$ef
        beq +
-
        lda $d012
        cmp #$ff
        bcc -
        jmp label0cd3
; -----------------------------------------------------------------------------#
+
        rts
; -----------------------------------------------------------------------------#
label0da2:
        lda label1028
        and #$03
        tax
        lda label0db6,x

        ldy #$07
-
        sta $d027,y
        dey
        bpl -
        jmp $ea7e
; -----------------------------------------------------------------------------#
label0db6:
    !byte $00, $01, $0F, $0C, $00, $00, $00, $00
    !byte $00, $00
; -----------------------------------------------------------------------------#
label0dc0:
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00

label0e00:
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00

label0e40:
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00

label0e80:
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00

label0ec0:
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00

label0f00:
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00

label0f40:
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00

label0f80:
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00

label0fc0:
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00, $00, $00
    !byte $00

; -----------------------------------------------------------------------------#
; raster pos. tab for color cycling effect in DD Logo
label1000:
    !byte   $52, $54, $56, $58, $5A, $5C, $5E, $60
    !byte   $62, $64, $66, $68, $6A, $6C, $6E, $70
    !byte   $72, $74, $76, $78, $7A, $7C, $7E, $80
    !byte   $82, $84, $86, $88, $8A, $8C, $8E, $90
    !byte   $92, $94, $96, $98, $9A, $9C, $9E, $A0
; -----------------------------------------------------------------------------#
; color scroll area 1
label1028:
    !byte   $00, $00, $00, $00, $00, $00, $00, $00
    !byte   $00, $00, $00, $00, $00, $00, $00, $00
    !byte   $00, $00, $00, $00, $00, $00, $00, $00
    !byte   $00, $00, $00, $00, $00, $00, $00, $00
    !byte   $00, $00, $00, $00, $00, $00, $00, $00
; color scroll area 2
label1050:
    !byte   $00, $00, $00, $00, $00, $00, $00, $00
    !byte   $00, $00, $00, $00, $00, $00, $00, $00
    !byte   $00, $00, $00, $00, $00, $00, $00, $00
    !byte   $00, $00, $00, $00, $00, $00, $00, $00
    !byte   $00, $00, $00, $00, $00, $00, $00, $00

; -----------------------------------------------------------------------------#
label1078:
        ldx #$00
label107a:
        lda label1028,x
        sta $02
        ldy label1050,x
        lda label1000,x
-
        cmp $d012
        bne -
        sty $d022
        lda $02
        sta $d023
        inx
        cpx #$28
        bne label107a
        dec $ff
        bpl label10c3
        ldy label1028
        ldx #$00
label10a0:
        lda label1028+1,x
        sta label1028,x
        inx
        cpx #$27
        bne label10a0
        sty label1000+$4f
        ldy label1078-2
        ldx #$25
-
        lda label1050,x
        sta label1050+1,x
        dex
        bpl -
        sty label1050
        lda #$02
        sta $ff
label10c3:
        lda #$01
        sta $d019
        jmp label0da2
; -----------------------------------------------------------------------------#
; colors
label10cb:
    !byte   $55, $55, $55, $55, $55, $55, $55, $55
    !byte   $BB, $BB, $BB, $BB, $BB, $BB, $BB, $BB
    !byte   $22, $22, $22, $22, $22, $22, $22, $22
    !byte   $66, $66, $66, $66, $66, $66, $66, $66
    !byte   $99, $99, $99, $99, $99, $99, $99, $99
; -----------------------------------------------------------------------------#
; colors
label10f3:
    !byte   $11, $FF, $88, $99, $00, $99, $88, $FF
    !byte   $11, $FF, $EE, $66, $00, $66, $EE, $FF
    !byte   $11, $FF, $AA, $22, $00, $22, $AA, $FF
    !byte   $11, $FF, $CC, $BB, $00, $BB, $CC, $FF
    !byte   $11, $FF, $DD, $55, $00, $55, $DD, $FF
; -----------------------------------------------------------------------------#
label111b:
        sei
        ldx #$00
        stx $d020
        stx $d021
        stx $d011
-
        lda label1400,x
        sta $0400,x
        lda label1500,x
        sta $0500,x
        lda label1600,x
        sta $0600,x
        lda label1700,x
        sta $0700,x
        inx
        bne -
        lda #$00
        sta $d015
        lda #$12
        sta $d018
        lda #$d8
        sta $d016
        lda #$0e
        sta $d022
        lda #$06
        sta $d023
        lda #$04
        sta label1167+2
        lda #$d8
        sta label1172+2
        ldy #$04
label1167:
        lda $0500,x
        beq label117b
        cmp #$40
        bcs +
        lda #$01
label1172:
        sta $d900,x
        bne label117f
+
        lda #$09
        bne label1172
label117b:
        lda #$02
        bne label1172
label117f:
        inx
        bne label1167
        inc label1167+2
        inc label1172+2
        dey
        bne label1167
        lda #$7f
        sta $dc0d
        lda #$81
        sta $d01a
        lda #$42
        sta $d012
        lda #$1b
        sta $d011
        lda #>label1078
        sta $0315
        lda #<label1078
        sta $0314
-
        lda label10cb,x
        sta label1028,x
        lda label10f3,x
        sta label1050,x
        inx
        cpx #$28
        bne -
        lda #$02
        sta $ff

        ldy #$3e
        ldx #$07
-
        tya
        sta $07f8,x
        dey
        dex
        bpl -

        ldx #$0f
-
        lda label0d80,x
        sta $d000,x
        dex
        bpl -
        lda #$e0
        sta $d010
        inx
        txa
-
        sta label0dc0,x
        sta label0e00,x
        sta label0f00,x
        inx
        bne -
        lda #$01
-
        sta $d027,x
        inx
        cpx #$08
        bne -
        lda #$ff
        sta $d015
        sta $d01d
        lda #$00
        sta $d017
        sta $d01c
        ldx #<scrolltext
        ldy #>scrolltext
        stx $ac
        sty $ad
        sta $55
        cli
        jsr label0d90
-
        lda $d012
        cmp #$f8
        bne -
        lda #$0b
        sta $d011
        sei
        lda #$31
        sta $0314
        lda #$ea
        sta $0315
        lda #$f0
        sta $d01a
        jsr $fda3
        lda #$00
        sta $d015
        lda #$15
        sta $d018
        jsr $e544
        lda #$c8
        sta $d016
        lda #$1b
        sta $d011
        ldx #$00
        txa
-
        sta $d800,x
        sta $d900,x
        sta $da00,x
        sta $db00,x
        inx
        bne -

-
        lda label1266,x
        sta $0100,x
        inx
        cpx #$36
        bne -
        jmp $0100
; -----------------------------------------------------------------------------#
label1266:
        lda #$34
        sta $01
        ldx #$e8
        ldy #$17
        stx $fa
        sty $fb
        ldx #$01
        ldy #$08
        stx $fc
        sty $fd
        ldx #$e8
label127c:
        ldy #$00
-
        lda ($fa),y
        sta ($fc),y
        iny
        bne -
        inc $fb
        inc $fd
        dex
        bne label127c
        ldx #$37
        ldy #$ae
        stx $ae
        sty $af
        lda #$37
        sta $01
        cli
        jmp $fce2
; -----------------------------------------------------------------------------#
; Scrolltext
scrolltext:
    !byte   $20, $54, $48, $49, $53, $20, $49, $53  ; $129C-$12A3 | THIS IS|
    !byte   $20, $41, $20, $50, $52, $45, $56, $49  ; $12A4-$12AB | A PREVI|
    !byte   $45, $57, $20, $43, $4F, $50, $59, $20  ; $12AC-$12B3 |EW COPY |
    !byte   $53, $4F, $20, $54, $48, $45, $20, $46  ; $12B4-$12BB |SO THE F|
    !byte   $49, $4E, $49, $53, $48, $45, $44, $20  ; $12BC-$12C3 |INISHED |
    !byte   $56, $45, $52, $53, $49, $4F, $4E, $20  ; $12C4-$12CB |VERSION |
    !byte   $4D, $59, $20, $44, $49, $46, $46, $45  ; $12CC-$12D3 |MY DIFFE|
    !byte   $52, $20, $21, $20, $20, $20, $49, $54  ; $12D4-$12DB |R !   IT|
    !byte   $20, $57, $41, $53, $20, $44, $45, $4C  ; $12DC-$12E3 | WAS DEL|
    !byte   $49, $56, $45, $52, $45, $44, $20, $42  ; $12E4-$12EB |IVERED B|
    !byte   $59, $20, $54, $48, $45, $20, $53, $49  ; $12EC-$12F3 |Y THE SI|
    !byte   $4C, $45, $4E, $54, $20, $53, $45, $52  ; $12F4-$12FB |LENT SER|
    !byte   $56, $49, $43, $45, $20, $21, $20, $20  ; $12FC-$1303 |VICE !  |
    !byte   $20, $20, $20, $20, $47, $52, $45, $45  ; $1304-$130B |    GREE|
    !byte   $54, $49, $4E, $47, $53, $20, $54, $4F  ; $130C-$1313 |TINGS TO|
    !byte   $3A, $20, $48, $45, $41, $44, $42, $41  ; $1314-$131B |: HEADBA|
    !byte   $4E, $47, $45, $52, $2C, $20, $45, $41  ; $131C-$1323 |NGER, EA|
    !byte   $47, $4C, $45, $53, $4F, $46, $54, $20  ; $1324-$132B |GLESOFT |
    !byte   $49, $4E, $43, $2E, $2C, $20, $53, $54  ; $132C-$1333 |INC., ST|
    !byte   $41, $52, $46, $4F, $52, $43, $45, $2C  ; $1334-$133B |ARFORCE,|
    !byte   $20, $52, $41, $54, $54, $2C, $20, $53  ; $133C-$1343 | RATT, S|
    !byte   $54, $4F, $41, $54, $2C, $20, $4D, $41  ; $1344-$134B |TOAT, MA|
    !byte   $53, $54, $45, $52, $2C, $20, $52, $41  ; $134C-$1353 |STER, RA|
    !byte   $44, $57, $41, $52, $2C, $20, $46, $4C  ; $1354-$135B |DWAR, FL|
    !byte   $41, $53, $48, $2C, $20, $53, $59, $4E  ; $135C-$1363 |ASH, SYN|
    !byte   $54, $41, $58, $20, $32, $30, $30, $31  ; $1364-$136B |TAX 2001|
    !byte   $20, $26, $20, $44, $2E, $43, $2E, $53  ; $136C-$1373 | & D.C.S|
    !byte   $2E, $20, $20, $20, $20, $20, $20, $20  ; $1374-$137B |.       |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $137C-$1383 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1384-$138B |        |
    !byte   $FF, $00, $00, $00, $00, $00, $00, $00  ; $138C-$1393 |........|
    !byte   $00, $00, $00, $00, $00, $00, $00, $00  ; $1394-$139B |........|
    !byte   $00, $00, $00, $00, $00, $00, $00, $00  ; $139C-$13A3 |........|
    !byte   $00, $00, $00, $00, $00, $00, $00, $00  ; $13A4-$13AB |........|
    !byte   $00, $00, $00, $00, $00, $00, $00, $00  ; $13AC-$13B3 |........|
    !byte   $00, $00, $00, $00, $00, $00, $00, $00  ; $13B4-$13BB |........|
    !byte   $00, $00, $00, $00, $00, $00, $00, $00  ; $13BC-$13C3 |........|
    !byte   $00, $00, $00, $00, $00, $00, $00, $00  ; $13C4-$13CB |........|
    !byte   $00, $00, $00, $00, $00, $00, $00, $00  ; $13CC-$13D3 |........|
    !byte   $00, $00, $00, $00, $00, $00, $00, $00  ; $13D4-$13DB |........|
    !byte   $00, $00, $00, $00, $00, $00, $00, $00  ; $13DC-$13E3 |........|
    !byte   $00, $00, $00, $00, $00, $00, $00, $00  ; $13E4-$13EB |........|
    !byte   $00, $00, $00, $00, $00, $00, $00, $00  ; $13EC-$13F3 |........|
    !byte   $00, $00, $00, $00, $00, $00, $00, $00  ; $13F4-$13FB |........|
    !byte   $00, $00, $00, $00; generated range: $1400-$17FF
; -----------------------------------------------------------------------------#
;Screen Data
label1400:
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1400-$1407 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1408-$140F |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1410-$1417 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1418-$141F |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1420-$1427 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1428-$142F |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1430-$1437 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1438-$143F |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1440-$1447 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1448-$144F |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1450-$1457 |        |
    !byte   $20, $20, $20, $20, $14, $08, $05, $20  ; $1458-$145F |    the |
    !byte   $17, $0F, $12, $0C, $04, $20, $06, $01  ; $1460-$1467 |world fa|
    !byte   $0D, $0F, $15, $13, $20, $20, $20, $20  ; $1468-$146F |mous    |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1470-$1477 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1478-$147F |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1480-$1487 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1488-$148F |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1490-$1497 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1498-$149F |        |
    !byte   $20, $20, $20, $20, $20, $20, $40, $41  ; $14A0-$14A7 |      @A|
    !byte   $42, $43, $44, $45, $20, $44, $46, $47  ; $14A8-$14AF |BCDE DFG|
    !byte   $41, $42, $48, $49, $4A, $4B, $4C, $4D  ; $14B0-$14B7 |ABHIJKLM|
    !byte   $47, $41, $42, $48, $44, $46, $47, $41  ; $14B8-$14BF |GABHDFGA|
    !byte   $4B, $4E, $20, $20, $20, $20, $20, $20  ; $14C0-$14C7 |KN      |
    !byte   $20, $20, $20, $20, $20, $20, $4F, $50  ; $14C8-$14CF |      OP|
    !byte   $51, $52, $53, $54, $55, $56, $57, $52  ; $14D0-$14D7 |QRSTUVWR|
    !byte   $20, $51, $52, $58, $59, $55, $5A, $5B  ; $14D8-$14DF | QRXYUZ[|
    !byte   $52, $5C, $5D, $52, $58, $5B, $52, $20  ; $14E0-$14E7 |R\]RX[R |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $14E8-$14EF |        |
    !byte   $20, $20, $20, $20, $20, $20, $4F, $5E  ; $14F0-$14F7 |      O^|
    !byte   $5F, $52, $20, $61, $62, $63, $51, $52  ; $14F8-$14FF |_R abcQR|
label1500:
    !byte   $20, $51, $52, $58, $64, $65, $66, $5B  ; $1500-$1507 | QRXdef[|
    !byte   $52, $58, $5B, $52, $58, $5B, $67, $68  ; $1508-$150F |RX[RX[gh|
    !byte   $69, $6A, $20, $20, $20, $20, $20, $20  ; $1510-$1517 |ij      |
    !byte   $20, $20, $20, $20, $20, $20, $6B, $6C  ; $1518-$151F |      kl|
    !byte   $6C, $6D, $20, $6E, $6F, $20, $6E, $6F  ; $1520-$1527 |lm no no|
    !byte   $20, $6E, $6F, $70, $71, $20, $70, $72  ; $1528-$152F | nopq pr|
    !byte   $6F, $70, $72, $6F, $70, $73, $74, $6C  ; $1530-$1537 |opropstl|
    !byte   $6C, $6F, $20, $20, $20, $20, $20, $20  ; $1538-$153F |lo      |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1540-$1547 |        |
    !byte   $20, $20, $20, $20, $20, $75, $76, $77  ; $1548-$154F |     uvw|
    !byte   $78, $79, $7A, $20, $7B, $7A, $7C, $7D  ; $1550-$1557 |xyz {z|}|
    !byte   $77, $78, $7E, $20, $20, $20, $20, $20  ; $1558-$155F |wx~     |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1560-$1567 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1568-$156F |        |
    !byte   $20, $20, $20, $20, $20, $7F, $80, $20  ; $1570-$1577 |     . |
    !byte   $81, $82, $83, $20, $51, $83, $84, $85  ; $1578-$157F |... Q...|
    !byte   $20, $81, $86, $20, $20, $20, $20, $20  ; $1580-$1587 | ..     |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1588-$158F |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1590-$1597 |        |
    !byte   $20, $20, $20, $20, $20, $7F, $80, $20  ; $1598-$159F |     . |
    !byte   $87, $82, $88, $89, $8A, $83, $84, $8B  ; $15A0-$15A7 |........|
    !byte   $20, $87, $86, $20, $20, $20, $20, $20  ; $15A8-$15AF | ..     |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $15B0-$15B7 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $15B8-$15BF |        |
    !byte   $20, $20, $20, $20, $20, $8C, $8D, $8E  ; $15C0-$15C7 |     ...|
    !byte   $8F, $90, $91, $8E, $92, $93, $94, $95  ; $15C8-$15CF |........|
    !byte   $8E, $8F, $96, $20, $20, $20, $20, $20  ; $15D0-$15D7 |...     |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $15D8-$15DF |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $15E0-$15E7 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $15E8-$15EF |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $15F0-$15F7 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $15F8-$15FF |        |
label1600:
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1600-$1607 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1608-$160F |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1610-$1617 |        |
    !byte   $10, $12, $05, $13, $05, $0E, $14, $13  ; $1618-$161F |presents|
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1620-$1627 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1628-$162F |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1630-$1637 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1638-$163F |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1640-$1647 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1648-$164F |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1650-$1657 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1658-$165F |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1660-$1667 |        |
    !byte   $01, $0C, $0C, $05, $19, $0B, $01, $14  ; $1668-$166F |alleykat|
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1670-$1677 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1678-$167F |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1680-$1687 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1688-$168F |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1690-$1697 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1698-$169F |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $16A0-$16A7 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $16A8-$16AF |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $16B0-$16B7 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $16B8-$16BF |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $16C0-$16C7 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $16C8-$16CF |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $16D0-$16D7 |        |
    !byte   $20, $20, $03, $12, $01, $03, $0B, $05  ; $16D8-$16DF |  cracke|
    !byte   $04, $20, $0F, $0E, $3A, $20, $31, $33  ; $16E0-$16E7 |d on: 13|
    !byte   $2F, $30, $39, $2F, $38, $36, $20, $20  ; $16E8-$16EF |/09/86  |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $16F0-$16F7 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $16F8-$16FF |        |
label1700:
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1700-$1707 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1708-$170F |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1710-$1717 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1718-$171F |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1720-$1727 |        |
    !byte   $20, $20, $20, $20, $20, $20, $10, $12  ; $1728-$172F |      pr|
    !byte   $05, $13, $13, $20, $13, $10, $01, $03  ; $1730-$1737 |ess spac|
    !byte   $05, $20, $20, $20, $20, $20, $20, $20  ; $1738-$173F |e       |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1740-$1747 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1748-$174F |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1750-$1757 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1758-$175F |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1760-$1767 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1768-$176F |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1770-$1777 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1778-$177F |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1780-$1787 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1788-$178F |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1790-$1797 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $1798-$179F |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $17A0-$17A7 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $17A8-$17AF |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $17B0-$17B7 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $17B8-$17BF |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $17C0-$17C7 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $17C8-$17CF |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $17D0-$17D7 |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $17D8-$17DF |        |
    !byte   $20, $20, $20, $20, $20, $20, $20, $20  ; $17E0-$17E7 |       .|
; -----------------------------------------------------------------------------#
      * = $2000
FakeEntry:
      ldx   #$f
-
      lda   fakebytes,x
      sta   $0800,x
      dex
      bne   -
      jmp   label111b         ; jump to demo entry point
; -----------------------------------------------------------------------------#
fakebytes:
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $60, $70, $78, $6C, $7E, $63, $61, $00
; -----------------------------------------------------------------------------#



