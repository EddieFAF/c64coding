
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
; created using c64reasm3 on jan. 24th, 2015             /  \   \       #
;               #########                               /\   \   \      #
; prj: the popular fairlight intro by woodo            /  \   \   \     #
;   reverse engineered by duke! to test c64reasm3     /____\___\___\    #
; prj. no.: 17                                                          #
;                                                                       #
; ----------------------------------------------------------------------#
;        quickly converted to ACME syntax by TheRyk/Mayday!             #
; ----------------------------------------------------------------------#
!to "FairlightIntro.prg", cbm
!sl "FairlightIntro_labels.txt"
*= $0800
!byte $00,$0c,$08,$0a,$00,$9e,$34,$39,$31,$35,$32,$00,$00,$00

*= $C000; "Main code"
; -----------------------------------------------------------------------------#
        sei
        lda #>IRQ1
        sta $0315
        lda #<IRQ1
        sta $0314
        lda #$01
        sta $d012
        sta $d01a
        lda #$7f
        sta $dc0d
        lda #$1b
        sta $d011
        lda #$94
        sta $dd00
        lda #$12
        sta $d018
        lda #$09
        ldx #$00
-       sta $d800,x
        inx
        bne -
        ldx #$20
-       sta $d900,x
        dex
        bne -
        lda #$01
-       sta $d920,x
        sta $da00,x
        sta $db00,x
        inx
        bne -
        lda #$00
        sta $d020
        sta $d021
        lda #$0a
        sta $d023
        lda #$02
        sta $d022
        lda #$d8
        sta $d016
        lda #$ff
        sta $d015
        lda #$18
        sta $d000
        lda #$48
        sta $d002
        lda #$78
        sta $d004
        lda #$a8
        sta $d008
        lda #$d8
        sta $d00a
        lda #$08
        sta $d00c
        lda #$38
        sta $d00e
        lda #$c0
        sta $d010
        lda #$ff
        sta $d01c
        sta $d01d
        lda #$0d
        ldx #$07
-       sta $d027,x
        dex
        bpl -
        lda #$05
        sta $d025
        lda #$01
        sta $d026
        lda #$00
        sta $02
        jsr labelc200
        cli
        jmp *
; -----------------------------------------------------------------------------#
labelc0b2:
        lda #$01
        sta $d019
        jsr labelcc5e
        inc $02
        ldx $02
        lda sintab,x
        ldy #$0e
-       sta $d001,y
        dey
        dey
        bpl -
        lda $d001
        cmp #$32
        bne labelc0d9
        lda #$00
labelc0d3:
        sta $d01b
        jmp labelc0f0
; -----------------------------------------------------------------------------#
labelc0d9:
        cmp #$7b
        bne labelc0e2
        lda #$ff
        jmp labelc0d3
; -----------------------------------------------------------------------------#
labelc0e2:
        cmp #$33
        bne labelc0f0
        lda labelc0f0+1
        cmp #$1f
        beq labelc0f0
        inc labelc0f0+1
labelc0f0:
        lda #$1f
        sta $d418
        lda #$98
        ldx #$00
        cmp $d012
        bne *-3
labelc0fe:
        lda labelc280,x
        tay
        lda $d012
        cmp $d012
        beq *-3
        sty $d021
        inx
        cpx #$2a
        bne labelc0fe
-       lda $d012
        cmp #$d2
        bne -
        lda $09
        sta $d016
        ldx #$64
        dex
        bne *-1
        lda #$d8
        sta $d016
        dec $09
        lda $09
        cmp #$ff
        bne labelc15c
        lda #$07
        sta $09
        ldx #$00
-       lda labelc720+1,x
        sta labelc720,x
        inx
        cpx #$27
        bne -
        ldx #$00
        lda ($39,x)
        sta labelc748-1
        inc $39
        lda $39
        cmp #$00
        bne labelc15c
        inc $3a
        lda $3a
        cmp #$cc
        bne labelc15c
        lda #$ca
        sta $3a
labelc15c:
        lda $db20
        pha
        ldx #$00
-       lda $db21,x
        sta $db20,x
        inx
        cpx #$27
        bne -
        pla
        sta $db47
        jmp $ea31
; -----------------------------------------------------------------------------#
IRQ1:
        lda #$01
        sta $d019
        lda labelc0f0+1
        cmp #$1f
        bne labelc187
        jsr $ffe4
        cmp #$20
        beq labelc18d
labelc187:
        jsr $ffe4
        jmp labelc0b2
; -----------------------------------------------------------------------------#
labelc18d:
        sei
        lda #$ea
        sta $0315
        lda #$31
        sta $0314
        jsr $ff81
        lda #$97
        sta $dd00
        cli
        jmp $fce2
; -----------------------------------------------------------------------------#

        *= $c200
; -----------------------------------------------------------------------------#
labelc200:
        jsr labelcc19
        lda #$10
        sta labelc0f0+1
        ldx #$c0
        lda #$00
-       sta $da40,x
        dex
        bne -
        lda #>scrolltext
        sta $3a
        lda #<scrolltext
        sta $39
        ldx #$00
-       lda labelc258,x
        sta $db20,x
        inx
        cpx #$28
        bne -
        lda #$00
        sta $c6
        sta $c5
        rts
; -----------------------------------------------------------------------------#
      ; unused area from c22e-$c257
      *= $c258; "data"
; -----------------------------------------------------------------------------#
; colors used by color cycling effect in scroller
labelc258:
    !byte $01, $01, $01, $01, $01, $0F, $0F, $0F
    !byte $0F, $0F, $0C, $0C, $0C, $0C, $0C, $0B
    !byte $0B, $0B, $0B, $0B, $01, $01, $01, $01
    !byte $01, $0F, $0F, $0F, $0F, $0F, $0C, $0C
    !byte $0C, $0C, $0C, $0B, $0B, $0B, $0B, $0B
; -----------------------------------------------------------------------------#
; colors for raster bars
labelc280:
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $02
    !byte $0A, $01, $01, $01, $0A, $0A, $02, $00
    !byte $00, $00, $00, $00, $00, $06, $0E, $0E
    !byte $01, $01, $0E, $0E, $06, $00, $00, $00
    !byte $00, $00
; 22 unused bytes
    !byte $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
; -----------------------------------------------------------------------------#
; sinetab for sprite y movement
sintab:
    !byte $57, $59, $5C, $5F, $61, $64, $66, $69
    !byte $6B, $6D, $6F, $71, $73, $75, $76, $78
    !byte $79, $7A, $7A, $7B, $7B, $7B, $7B, $7B
    !byte $7B, $7A, $79, $78, $77, $76, $74, $73
    !byte $71, $6F, $6D, $6A, $68, $65, $63, $60
    !byte $5E, $5B, $58, $56, $53, $50, $4E, $4B
    !byte $48, $46, $43, $41, $3F, $3D, $3B, $39
    !byte $38, $36, $35, $34, $33, $32, $32, $32
    !byte $32, $32, $32, $32, $33, $34, $35, $36
    !byte $38, $39, $3B, $3D, $3F, $41, $43, $46
    !byte $48, $4B, $4E, $50, $53, $56, $58, $5B
    !byte $5E, $60, $63, $65, $68, $6A, $6D, $6F
    !byte $71, $73, $74, $76, $77, $78, $79, $7A
    !byte $7B, $7B, $7B, $7B, $7B, $7B, $7A, $7A
    !byte $79, $78, $76, $75, $73, $71, $6F, $6D
    !byte $6B, $69, $66, $64, $61, $5F, $5C, $59
    !byte $57, $54, $51, $4E, $4C, $49, $47, $44
    !byte $42, $40, $3E, $3C, $3A, $38, $37, $35
    !byte $34, $33, $33, $32, $32, $32, $32, $32
    !byte $32, $33, $34, $35, $36, $37, $39, $3A
    !byte $3C, $3E, $40, $43, $45, $48, $4A, $4D
    !byte $4F, $52, $55, $57, $5A, $5D, $5F, $62
    !byte $65, $67, $6A, $6C, $6E, $70, $72, $74
    !byte $75, $77, $78, $79, $7A, $7B, $7B, $7B
    !byte $7B, $7B, $7B, $7B, $7A, $79, $78, $77
    !byte $75, $74, $72, $70, $6E, $6C, $6A, $67
    !byte $65, $62, $5F, $5D, $5A, $57, $55, $52
    !byte $4F, $4D, $4A, $48, $45, $43, $40, $3E
    !byte $3C, $3A, $39, $37, $36, $35, $34, $33
    !byte $32, $32, $32, $32, $32, $32, $33, $33
    !byte $34, $35, $37, $38, $3A, $3C, $3E, $40
    !byte $42, $44, $47, $49, $4C, $4E, $51, $54
; -----------------------------------------------------------------------------#
; sprite data for the look a like raster bar sprites...
spritedata: ; $c3c0:
    !byte $55, $55, $55
    !byte $55, $55, $55
    !byte $AA, $AA, $AA
    !byte $55, $55, $55
    !byte $AA, $AA, $AA
    !byte $AA, $AA, $AA
    !byte $AA, $AA, $AA
    !byte $FF, $FF, $FF
    !byte $AA, $AA, $AA
    !byte $FF, $FF, $FF
    !byte $FF, $FF, $FF
    !byte $FF, $FF, $FF
    !byte $AA, $AA, $AA
    !byte $FF, $FF, $FF
    !byte $AA, $AA, $AA
    !byte $AA, $AA, $AA
    !byte $AA, $AA, $AA
    !byte $55, $55, $55
    !byte $AA, $AA, $AA
    !byte $55, $55, $55
    !byte $55, $55, $55
    !byte $00
; -----------------------------------------------------------------------------#
; screen data
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $21, $00
    !byte $00, $00, $21, $20, $20, $21, $00, $00
    !byte $00, $21, $20, $21, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $21, $20, $20, $21, $20, $21, $21, $21
    !byte $20, $21, $21, $20, $20, $21, $21, $20
    !byte $20, $21, $20, $21, $20, $21, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $00, $00, $20, $21, $00, $21, $21, $21
    !byte $00, $00, $21, $20, $20, $21, $21, $20
    !byte $21, $21, $00, $00, $20, $21, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $21, $20, $20, $21, $20, $21, $21, $21
    !byte $20, $21, $00, $00, $00, $21, $00, $00
    !byte $00, $21, $20, $21, $20, $21, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $2E, $2E, $2E, $14, $08
    !byte $05, $20, $08, $0F, $0D, $05, $20, $0F
    !byte $06, $20, $14, $08, $05, $20, $12, $05
    !byte $01, $0C, $20, $03, $12, $01, $03, $0B
    !byte $05, $12, $13, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $10
    !byte $12, $05, $13, $05, $0E, $14, $13, $3A
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $03, $0F, $0D
    !byte $02, $01, $14, $20, $13, $03, $08, $0F
    !byte $0F, $0C, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $03, $12
    !byte $01, $03, $0B, $05, $04, $20, $32, $31
    !byte $2E, $31, $31, $2E, $38, $37, $20, $02
    !byte $19, $20, $13, $14, $12, $09, $04, $05
    !byte $12, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
labelc720:
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
labelc748:
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $09, $0E, $14
    !byte $12, $0F, $20, $02, $19, $20, $17, $0F
    !byte $0F, $04, $0F, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $20, $20
; -----------------------------------------------------------------------------#
; sprite pointer
    !byte $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
; -----------------------------------------------------------------------------#
; charset
    !byte $55, $AA, $AA, $FF, $FF, $AA, $AA, $55
    !byte $3E, $60, $C6, $FE, $C6, $C6, $66, $00
    !byte $FC, $06, $CE, $FC, $C6, $CE, $FC, $00
    !byte $1E, $30, $60, $60, $60, $66, $3C, $00
    !byte $FC, $06, $C6, $C6, $CC, $DC, $F0, $00
    !byte $FE, $00, $C0, $F8, $C0, $E6, $7C, $00
    !byte $FE, $00, $C0, $F8, $C0, $C0, $60, $00
    !byte $7C, $E6, $C0, $CE, $C6, $CE, $7C, $00
    !byte $C6, $06, $C6, $FE, $C6, $C6, $66, $00
    !byte $FC, $00, $30, $30, $30, $30, $7C, $00
    !byte $7E, $00, $18, $18, $98, $D8, $70, $00
    !byte $C6, $0C, $D8, $F0, $D8, $CC, $46, $00
    !byte $C0, $00, $C0, $C0, $C0, $CE, $FC, $00
    !byte $26, $70, $FE, $D6, $D6, $C6, $66, $00
    !byte $66, $E0, $F6, $FE, $CE, $C6, $66, $00
    !byte $7C, $E6, $C6, $C6, $C6, $CE, $7C, $00
    !byte $FC, $06, $C6, $FC, $C0, $C0, $60, $00
    !byte $7C, $E6, $C6, $C6, $CE, $FE, $76, $00
    !byte $FC, $06, $C6, $FC, $D8, $CC, $66, $00
    !byte $7C, $E6, $C0, $7C, $06, $CE, $7C, $00
    !byte $FE, $00, $38, $38, $38, $38, $1C, $00
    !byte $C6, $C0, $C6, $C6, $C6, $6E, $3E, $00
    !byte $C6, $C0, $C6, $C6, $66, $36, $1C, $00
    !byte $66, $C0, $D6, $D6, $FE, $76, $32, $00
    !byte $66, $E0, $7C, $18, $7C, $EE, $66, $00
    !byte $C6, $C0, $C6, $6C, $38, $38, $38, $00
    !byte $7E, $46, $0C, $18, $30, $66, $7C, $00
    !byte $00, $00, $00, $03, $0F, $5F, $5F, $5F
    !byte $00, $00, $C0, $C0, $80, $FF, $FF, $F0
    !byte $5F, $5F, $5F, $5F, $0F, $00, $00, $00
    !byte $F0, $E0, $E0, $80, $80, $00, $00, $00
    !byte $00, $07, $0E, $0F, $1E, $1F, $1E, $1F
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $B9, $B9, $B9, $B9, $B9, $B9, $B9, $B9
    !byte $66, $66, $66, $00, $00, $00, $00, $00
    !byte $66, $66, $FF, $66, $FF, $66, $66, $00
    !byte $18, $3E, $60, $3C, $06, $7C, $18, $00
    !byte $62, $66, $0C, $18, $30, $66, $46, $00
    !byte $3C, $66, $3C, $38, $67, $66, $3F, $00
    !byte $06, $0C, $18, $00, $00, $00, $00, $00
    !byte $0C, $18, $30, $30, $30, $18, $0C, $00
    !byte $30, $18, $0C, $0C, $0C, $18, $30, $00
    !byte $00, $66, $3C, $FF, $3C, $66, $00, $00
    !byte $00, $18, $18, $7E, $18, $18, $00, $00
    !byte $00, $00, $00, $00, $00, $18, $18, $30
    !byte $00, $00, $00, $7E, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $18, $18, $00
    !byte $00, $03, $06, $0C, $18, $30, $60, $00
    !byte $7C, $E6, $C6, $C6, $C6, $CE, $7C, $00
    !byte $38, $60, $18, $18, $18, $18, $1C, $00
    !byte $3C, $60, $06, $1C, $30, $66, $7C, $00
    !byte $7C, $00, $06, $3C, $06, $66, $3C, $00
    !byte $1C, $20, $6C, $CC, $FE, $0C, $0E, $00
    !byte $FE, $00, $C0, $FC, $06, $CE, $7C, $00
    !byte $3C, $66, $C0, $FC, $C6, $CE, $7C, $00
    !byte $FE, $00, $0C, $0C, $18, $18, $18, $00
    !byte $7C, $E0, $C6, $7C, $C6, $CE, $7C, $00
    !byte $7C, $E0, $C6, $7E, $06, $CE, $7C, $00
    !byte $00, $00, $18, $00, $00, $18, $00, $00
    !byte $00, $00, $18, $00, $00, $18, $18, $30
    !byte $0E, $18, $30, $60, $30, $18, $0E, $00
    !byte $00, $00, $7E, $00, $7E, $00, $00, $00
    !byte $70, $18, $0C, $06, $0C, $18, $70, $00
    !byte $3C, $66, $06, $0C, $18, $00, $18, $00
; -----------------------------------------------------------------------------#
; scroll text
scrolltext:
!ct scr 
    !tx "cracked on the 21st of november 1987... "
    !tx "  now you can train yourself to kill com"
    !tx "munists and iranians...    latest top pi"
    !tx "rates : beastie boys  ikari  ace  hotlin"
    !tx "e  danish gold  new wizax  tpi  tlc  ant"
    !tx "itrax  c64cg  triad  1001 crew  yeti  tr"
    !tx "iton t  fcs  sca    overseas : eaglesoft"
    !tx "  fbr  sol  nepa  abyss  xpb  ts  tih   "
    !tx "       pray that you will get an invitat"
    !tx "ion to our great copy party in stockholm"
    !tx " in december...        fuckings to watch"
    !tx "er of the silents. you'll not destroy th"
    !tx "is party...       l8r           "
; -----------------------------------------------------------------------------#
; Sid Player Stuff
labelcc00:
    !byte $00, $00, $00, $08
; -----------------------------------------------------------------------------#
labelcc04:
    !byte $10, $2e, $4e, $00, $00, $00, $08
; -----------------------------------------------------------------------------#
labelcc0b:
    !byte $40, $8a, $0a, $00, $00, $00, $07
; -----------------------------------------------------------------------------#
labelcc12:
    !byte $40, $0a, $0a, $00, $7d, $d6, $1f
; -----------------------------------------------------------------------------#
labelcc19:
        ldx #$18
-       lda labelcc00,x
        sta $d400,x
        dex
        bpl -
        lda labelcc04
        sta $0355
        lda labelcc0b
        sta $0356
        lda labelcc12
        sta $0357
        ldx #$02
        lda #$01
-       sta $0358,x
        dex
        bpl -
        ldx #$08
        lda #$00
-       sta $035b,x
        dex
        bpl -
        lda #$01
        sta $0354
        ldx #$05
-       lda labelce00,x
        sta $0364,x
        dex
        bpl -
        inc $0358
        rts
; -----------------------------------------------------------------------------#
labelcc5e:
        dec $0354
        beq labelcc66
        jmp labelcd1d
; -----------------------------------------------------------------------------#
labelcc66:
        lda #$0a
        sta $0354
        ldx #$02
labelcc6d:
        dec $0358,x
        bne labelccd1
        txa
        sta $9e
        asl
        pha
        asl
        asl
        sec
        sbc $9e
        tay
        lda $0355,x
        sta $d404,y
        pla
        tay
labelcc85:
        lda $0364,y
        sta $9e
        clc
        adc #$03
        sta $0364,y
        lda $0365,y
        sta $9f
        adc #$00
        sta $0365,y
        ldy #$02
        lda ($9e),y
        beq labelccc8
        sta $035e,x
        dey
        lda ($9e),y
        sta $035b,x
        lda #$01
        sta $0361,x
labelccae:
        dey
        lda ($9e),y
        sta $0358,x
        bne labelccd1
        txa
        asl
        tay
        lda labelce00,y
        sta $0364,y
        lda labelce00+1,y
        sta $0365,y
        jmp labelcc85
; -----------------------------------------------------------------------------#
labelccc8:
        dey
        lda #$00
        sta $0361,x
        jmp labelccae
; -----------------------------------------------------------------------------#
labelccd1:
        dex
        bpl labelcc6d
        lda $a0
        and #$07
        clc
        adc #$04
        sta $d40a
        lda $035b
        sta $d400
        lda $035e
        sta $d401
        lda $035c
        sta $d407
        lda $035f
        sta $d408
        lda $035d
        sta $d40e
        lda $0360
        sta $d40f
        lda $0355
        ora $0361
        sta $d404
        lda $0356
        ora $0362
        sta $d40b
        lda $0357
        ora $0363
        sta $d412
labelcd1d:
        rts
; -----------------------------------------------------------------------------#
     *= $ce00
; -----------------------------------------------------------------------------#
; SID Player vectors & song data
labelce00:
    !byte $57, $CE, $06, $CE, $57, $CE, $03, $85
    !byte $06, $03, $E2, $04, $06, $2C, $05, $03
    !byte $CF, $05, $03, $CF, $05, $06, $85, $06
    !byte $03, $85, $06, $03, $E2, $04, $06, $2C
    !byte $05, $03, $CF, $05, $03, $CF, $05, $06
    !byte $85, $06, $03, $85, $06, $02, $85, $06
    !byte $01, $0A, $0D, $06, $2C, $05, $03, $CF
    !byte $05, $02, $E2, $04, $01, $CF, $05, $06
    !byte $85, $06, $03, $85, $06, $03, $E2, $04
    !byte $06, $2C, $05, $03, $CF, $05, $03, $E2
    !byte $04, $06, $42, $03, $00, $00, $00, $60
    !byte $00, $00, $03, $29, $34, $01, $8C, $3A
    !byte $01, $08, $3E, $01, $29, $34, $04, $A1
    !byte $45, $02, $08, $3E, $01, $8D, $3A, $01
    !byte $08, $3E, $04, $8D, $3A, $06, $29, $34
    !byte $03, $29, $34, $01, $8D, $3A, $01, $08
    !byte $3E, $01, $29, $34, $04, $A1, $45, $02
    !byte $08, $3E, $01, $8C, $3A, $01, $08, $3E
    !byte $04, $A1, $45, $06, $27, $4E, $03, $27
    !byte $4E, $01, $CD, $52, $01, $27, $4E, $01
    !byte $CD, $52, $04, $27, $4E, $02, $A1, $45
    !byte $01, $08, $3E, $01, $A1, $45, $04, $8D
    !byte $3A, $06, $29, $34, $03, $29, $34, $01
    !byte $8D, $3A, $01, $08, $3E, $01, $29, $34
    !byte $04, $A1, $45, $02, $08, $3E, $01, $8C
    !byte $3A, $01, $08, $3E, $02, $8C, $3A, $02
    !byte $78, $2E, $06, $29, $34, $02, $0A, $0D
    !byte $02, $0A, $0D, $01, $A3, $0E, $01, $82
    !byte $0F, $04, $B3, $14, $02, $89, $13, $02
    !byte $68, $11, $02, $82, $0F, $02, $A3, $0E
    !byte $03, $82, $0F, $01, $68, $11, $02, $A3
    !byte $0E, $02, $0A, $0D, $02, $0A, $0D, $01
    !byte $A3, $0E, $01, $82, $0F, $02, $B3, $14
    !byte $02, $89, $13, $02, $68, $11, $02, $82
    !byte $0F, $02, $A3, $0E, $02, $82, $0F, $06
    !byte $C4, $09, $02, $0A, $0D, $02, $A3, $0E
    !byte $02, $82, $0F, $02, $B3, $14, $02, $89
    !byte $13, $02, $68, $11, $02, $82, $0F, $02
    !byte $A3, $0E, $02, $82, $0F, $06, $82, $0F
    !byte $02, $0A, $0D, $02, $0A, $0D, $01, $A3
    !byte $0E, $01, $82, $0F, $02, $B3, $14, $02
    !byte $B3, $14, $02, $89, $13, $02, $68, $11
    !byte $02, $82, $0F, $01, $A3, $0E, $01, $82
    !byte $0F, $06, $0A, $0D, $06, $0A, $0D, $09
    !byte $82, $0F, $03, $A3, $0E, $06, $0A, $0D
    !byte $06, $13, $27, $09, $D0, $22, $03, $04
    !byte $1F, $06, $14, $1A, $03, $14, $1A, $02
    !byte $14, $1A, $01, $13, $27, $06, $66, $29
    !byte $03, $D0, $22, $02, $04, $1F, $01, $46
    !byte $1D, $06, $14, $1A, $03, $14, $1A, $03
    !byte $8A, $13, $06, $B3, $14, $04, $04, $1F
    !byte $05, $D0, $22, $03, $14, $1A, $00, $00
    !byte $00, $20, $FF, $FF, $FF, $FF, $FF, $FF
    !byte $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
    !byte $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
    !byte $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
