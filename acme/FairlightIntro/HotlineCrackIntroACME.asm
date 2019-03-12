
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
; prj: hotline crack intro by tmc                      /  \   \   \     #
;   reverse engineered by duke! to test c64reasm3     /____\___\___\    #
; prj. no.: 2k15 / 22                                                   #
;                                                                       #
; jmp in adress: $2200                                                  #
;                                                                       #
; Syntax quickly translated to ACME by TheRyk/Mayday!                   #
; ----------------------------------------------------------------------#
!to "HotlineCrackIntro.prg", cbm

*= $0800
;    !byte $00,$0c,$08,$0a,$00,$9e,$35,$34,$34,$30,$00,$00,$00,$00,$00; SYS 5440 /$1540
    !byte $00,$0c,$08,$0a,$00,$9e,$38,$37,$30,$34,$00,$00,$00,$00,$00; SYS 8704 /$2200
; -----------------------------------------------------------------------------#
;      * = $0801 "Basic Upstart"
;      :BasicUpstart(FakeEntry)
; -----------------------------------------------------------------------------#
      * = $0810; "Hotline 003 by tmc"
        !byte $56, $5A, $56, $5A, $56, $5A, $56, $5A
        !byte $FF, $FF, $7F, $7F, $5E, $5B, $56, $5A
        !byte $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA
        !byte $AA, $AA, $FF, $AA, $95, $B5, $BD, $BF
        !byte $A9, $A9, $FF, $AA, $55, $55, $55, $55
        !byte $AA, $AA, $FF, $AA, $56, $56, $56, $56
        !byte $56, $56, $56, $56, $56, $56, $56, $56
        !byte $56, $D6, $F6, $FE, $AA, $55, $AA, $AA
        !byte $FF, $FF, $FF, $FF, $AA, $55, $AA, $AA
        !byte $BF, $BF, $BF, $BF, $AA, $55, $AA, $AA
        !byte $BF, $BF, $BF, $BF, $BF, $BF, $BF, $BF
        !byte $FF, $7F, $5F, $57, $56, $56, $56, $56
        !byte $FF, $FF, $FF, $FF, $AA, $AA, $AA, $AA
        !byte $FF, $FF, $FF, $FF, $BF, $BF, $BF, $BF
        !byte $BF, $BF, $BF, $BF, $BF, $BF, $BF, $BF
        !byte $BF, $BF, $BF, $BF, $7F, $5F, $57, $55
        !byte $AA, $AA, $AA, $AA, $55, $55, $55, $55
        !byte $56, $56, $56, $56, $55, $55, $55, $55
        !byte $56, $56, $56, $56, $56, $56, $56, $56
        !byte $FF, $FF, $FF, $FF, $55, $55, $55, $55
        !byte $FF, $FF, $FF, $FF, $56, $56, $56, $56
        !byte $FF, $FF, $FF, $FF, $55, $55, $55, $55
        !byte $56, $D6, $F6, $FE, $55, $55, $55, $55
        !byte $40, $40, $55, $55, $45, $44, $54, $00
        !byte $BF, $9B, $9B, $9B, $95, $AA, $AA, $AA
        !byte $AA, $AA, $AA, $BF, $9B, $9B, $9B, $95
        !byte $AA, $AA, $66, $99, $55, $55, $55, $55
        !byte $56, $5A, $56, $59, $55, $55, $55, $55
        !byte $AF, $BF, $AF, $BF, $AF, $BF, $AF, $BF
        !byte $FF, $FF, $FF, $FF, $EF, $BF, $AF, $BF
label0900:
        !byte $00, $25, $25, $25, $25, $25, $25, $AA
        !byte $AF, $BF, $6F, $BF, $5F, $5F, $57, $55
        !byte $EA, $EA, $EA, $EA, $EA, $EA, $EA, $EA
        !byte $AA, $AA, $AB, $A9, $A9, $A9, $A9, $A9
        !byte $AA, $AA, $EA, $EA, $EA, $EA, $EA, $EA
        !byte $A9, $A9, $A9, $A9, $A9, $A9, $A9, $A9
        !byte $AA, $AA, $FF, $AA, $55, $55, $55, $55
        !byte $EA, $EA, $FF, $AA, $55, $55, $55, $55
        !byte $E9, $E9, $FF, $AA, $55, $55, $55, $55
        !byte $EA, $EA, $EA, $EA, $EA, $EA, $AA, $AA
        !byte $A9, $A9, $A9, $A9, $A9, $A9, $AA, $AA
        !byte $EA, $EA, $FF, $AA, $56, $56, $56, $56
        !byte $E9, $E9, $E9, $E9, $E9, $E9, $E9, $E9
        !byte $A9, $A9, $E9, $E9, $E9, $E9, $E9, $E9
        !byte $FF, $FF, $FF, $FF, $AA, $55, $A9, $A9
        !byte $FF, $FF, $FF, $FF, $AA, $D5, $E9, $E9
        !byte $FF, $FF, $FF, $FF, $AA, $D5, $EA, $EA
        !byte $E9, $E9, $E9, $E9, $E9, $E9, $AA, $AA
        !byte $EA, $EA, $FF, $AA, $55, $55, $55, $55
        !byte $00, $00, $00, $00, $00, $00, $00, $00
        !byte $00, $00, $00, $00, $00, $00, $00, $00
        !byte $00, $00, $00, $00, $00, $00, $00, $00
        !byte $00, $00, $00, $00, $00, $00, $00, $00
        !byte $00, $00, $00, $00, $00, $00, $00, $00
        !byte $00, $00, $00, $00, $00, $00, $00, $00
        !byte $00, $00, $00, $00, $00, $00, $00, $00
        !byte $00, $00, $00, $00, $00, $00, $00, $00
        !byte $00, $00, $00, $00, $00, $00, $00, $00
        !byte $00, $00, $00, $00, $00, $00, $00, $00
        !byte $00, $00, $00, $00, $00, $00, $00, $00
        !byte $00, $00, $00, $00, $00, $00, $00, $00
        !byte $00, $00, $00, $00, $00, $00, $00, $00

; -----------------------------------------------------------------------------#
; Sprite Data
label0a00:
    !byte $79, $9B, $F7
    !byte $79, $9B, $F7
    !byte $31, $D8, $C6
    !byte $31, $F8, $C7
    !byte $31, $B8, $C7
    !byte $79, $98, $C6
    !byte $79, $98, $C6
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

label0a40:
    !byte $C7, $83, $C6
    !byte $EF, $C3, $F6
    !byte $6C, $C3, $36
    !byte $CC, $C3, $E3
    !byte $8C, $C3, $30
    !byte $CF, $C3, $F7
    !byte $67, $83, $C7
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
    !byte $3F

label0a80:
    !byte $61, $FB, $19
    !byte $61, $FB, $BB
    !byte $60, $63, $FB
    !byte $E0, $63, $5B
    !byte $60, $63, $1B
    !byte $E0, $63, $1B
    !byte $C0, $63, $19
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

label0ac0:
    !byte $E0, $00, $00
    !byte $F0, $00, $00
    !byte $30, $00, $00
    !byte $00, $00, $00
    !byte $30, $00, $00
    !byte $F0, $00, $00
    !byte $E0, $00, $00
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

label0b00:
    !byte $00, $00, $01
    !byte $00, $00, $05
    !byte $00, $00, $15
    !byte $00, $00, $55
    !byte $00, $01, $59
    !byte $00, $55, $66
    !byte $05, $55, $98
    !byte $15, $66, $60
    !byte $59, $9A, $A0
    !byte $66, $A9, $68
    !byte $6A, $A2, $BA
    !byte $EE, $AB, $E8
    !byte $FB, $BA, $A0
    !byte $3F, $EE, $E0
    !byte $0F, $FF, $B8
    !byte $00, $FF, $EE
    !byte $00, $0F, $FB
    !byte $00, $00, $FF
    !byte $00, $00, $3F
    !byte $00, $00, $0F
    !byte $00, $00, $03
    !byte $00

label0b40:
    !byte $40, $00, $00
    !byte $50, $00, $00
    !byte $54, $00, $00
    !byte $55, $00, $00
    !byte $65, $50, $00
    !byte $99, $55, $00
    !byte $26, $55, $50
    !byte $09, $99, $54
    !byte $0A, $A6, $65
    !byte $29, $6A, $99
    !byte $AE, $9A, $AA
    !byte $2B, $EA, $BB
    !byte $0A, $AE, $EF
    !byte $0B, $BB, $FC
    !byte $2E, $FF, $F0
    !byte $BB, $FF, $00
    !byte $EF, $F0, $00
    !byte $FF, $00, $00
    !byte $FC, $00, $00
    !byte $F0, $00, $00
    !byte $C0, $00, $00
    !byte $00
; -----------------------------------------------------------------------------#
; MAP DATA for Hotline Logo (112*12)

      * = $1000 ; "Map Data"
label1000:
    !byte $03, $01, $01, $01, $01, $01, $01, $01
    !byte $01, $01, $01, $01, $01, $01, $01, $01
    !byte $01, $01, $01, $01, $01, $01, $01, $01
    !byte $01, $01, $01, $01, $01, $01, $01, $01
    !byte $01, $01, $01, $01, $01, $01, $01, $01
    !byte $01, $01, $01, $01, $01, $01, $01, $01
    !byte $01, $01, $01, $01, $01, $01, $01, $01
    !byte $01, $01, $01, $01, $01, $01, $01, $01
    !byte $01, $01, $01, $01, $01, $01, $01, $01
    !byte $01, $01, $01, $01, $01, $01, $01, $01
    !byte $01, $01, $01, $01, $01, $1F, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
label1070:
    !byte $02, $1B, $23, $05, $26, $26, $07, $24
    !byte $23, $05, $26, $26, $07, $24, $23, $05
    !byte $26, $26, $26, $26, $26, $26, $26, $26
    !byte $07, $24, $23, $05, $26, $26, $26, $26
    !byte $26, $26, $26, $26, $07, $24, $23, $05
    !byte $26, $26, $07, $24, $04, $04, $04, $04
    !byte $04, $04, $23, $05, $26, $26, $26, $26
    !byte $26, $26, $07, $24, $23, $05, $26, $26
    !byte $26, $26, $26, $26, $26, $26, $07, $24
    !byte $23, $05, $26, $26, $26, $26, $26, $26
    !byte $26, $26, $07, $24, $1B, $1E, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
label10e0:
    !byte $02, $04, $25, $0C, $20, $20, $08, $22
    !byte $25, $0C, $20, $20, $08, $22, $25, $0C
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $08, $22, $25, $0C, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $08, $22, $25, $0C
    !byte $20, $20, $08, $22, $04, $04, $04, $04
    !byte $04, $04, $25, $0C, $20, $20, $20, $20
    !byte $20, $20, $08, $22, $25, $0C, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $08, $22
    !byte $25, $0C, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $08, $22, $04, $1E, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
label1150:
    !byte $02, $04, $25, $0C, $20, $20, $08, $22
    !byte $25, $0C, $20, $20, $08, $22, $25, $0C
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $08, $22, $25, $0C, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $08, $22, $25, $0C
    !byte $20, $20, $08, $22, $04, $04, $04, $04
    !byte $04, $04, $25, $0C, $20, $20, $20, $20
    !byte $20, $20, $08, $22, $25, $0C, $20, $20
    !byte $20, $20, $20, $20, $20, $20, $08, $22
    !byte $25, $0C, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $08, $22, $04, $1E, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
label11c0:
    !byte $02, $04, $25, $0C, $20, $20, $13, $27
    !byte $06, $11, $20, $20, $08, $22, $25, $0C
    !byte $20, $20, $0D, $30, $2E, $0F, $20, $20
    !byte $08, $22, $2A, $0B, $0A, $2E, $0F, $20
    !byte $20, $0D, $30, $0A, $09, $29, $25, $0C
    !byte $20, $20, $08, $22, $04, $04, $04, $04
    !byte $04, $04, $2A, $0B, $2E, $0F, $20, $20
    !byte $0D, $30, $09, $29, $25, $0C, $20, $20
    !byte $0D, $30, $2E, $0F, $20, $20, $08, $22
    !byte $25, $0C, $20, $20, $17, $15, $15, $15
    !byte $16, $30, $09, $29, $04, $1E, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
label1230:
    !byte $02, $04, $25, $0C, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $08, $22, $25, $0C
    !byte $20, $20, $13, $2B, $2D, $0C, $20, $20
    !byte $08, $22, $04, $04, $04, $25, $0C, $20
    !byte $20, $13, $2B, $24, $04, $04, $25, $0C
    !byte $20, $20, $13, $2B, $24, $04, $04, $04
    !byte $04, $04, $04, $04, $25, $0C, $20, $20
    !byte $13, $2B, $24, $04, $25, $0C, $20, $20
    !byte $13, $2B, $2D, $0C, $20, $20, $08, $22
    !byte $25, $0C, $20, $20, $20, $20, $20, $20
    !byte $08, $22, $04, $04, $04, $1E, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
label12a0:
    !byte $02, $04, $25, $0C, $20, $20, $20, $20
    !byte $20, $20, $20, $20, $08, $22, $25, $0C
    !byte $20, $20, $20, $08, $2C, $0C, $20, $20
    !byte $08, $22, $04, $04, $04, $25, $0C, $20
    !byte $20, $20, $08, $22, $04, $04, $25, $0C
    !byte $20, $20, $20, $08, $22, $04, $04, $04
    !byte $04, $04, $04, $04, $25, $0C, $20, $20
    !byte $20, $08, $22, $04, $25, $0C, $20, $20
    !byte $20, $08, $2C, $0C, $20, $20, $08, $22
    !byte $25, $0C, $20, $20, $20, $20, $20, $20
    !byte $08, $22, $04, $04, $04, $1E, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
label1310:
    !byte $02, $04, $25, $0C, $20, $20, $20, $0D
    !byte $2F, $0F, $20, $20, $08, $22, $25, $0C
    !byte $20, $20, $20, $13, $28, $11, $20, $20
    !byte $08, $22, $04, $04, $04, $25, $0C, $20
    !byte $20, $20, $08, $22, $04, $04, $25, $0C
    !byte $20, $20, $20, $13, $32, $26, $26, $26
    !byte $07, $24, $23, $05, $06, $11, $20, $20
    !byte $20, $13, $2B, $24, $25, $0C, $20, $20
    !byte $20, $08, $2C, $0C, $20, $20, $08, $22
    !byte $25, $0C, $20, $20, $20, $17, $17, $17
    !byte $18, $32, $07, $24, $04, $1E, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
label1380:
    !byte $02, $04, $25, $0C, $20, $20, $20, $08
    !byte $2C, $0C, $20, $20, $08, $22, $25, $0C
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $08, $22, $04, $04, $04, $25, $0C, $20
    !byte $20, $20, $08, $22, $04, $04, $25, $0C
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $08, $22, $25, $0C, $20, $20, $20, $20
    !byte $20, $20, $08, $22, $25, $0C, $20, $20
    !byte $20, $08, $2C, $0C, $20, $20, $08, $22
    !byte $25, $0C, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $08, $22, $04, $1E, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
label13f0:
    !byte $02, $04, $25, $0C, $20, $20, $20, $08
    !byte $2C, $0C, $20, $20, $08, $22, $25, $0C
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $08, $22, $04, $04, $04, $25, $0C, $20
    !byte $20, $20, $08, $22, $04, $04, $25, $0C
    !byte $20, $20, $20, $20, $20, $20, $20, $20
    !byte $08, $22, $25, $0C, $20, $20, $20, $20
    !byte $20, $20, $08, $22, $25, $0C, $20, $20
    !byte $20, $08, $2C, $0C, $20, $20, $08, $22
    !byte $25, $0C, $20, $20, $20, $20, $20, $20
    !byte $20, $20, $08, $22, $04, $1E, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
label1460:
    !byte $02, $1A, $2A, $0B, $0A, $0A, $0A, $09
    !byte $31, $0B, $0A, $0A, $09, $29, $2A, $0B
    !byte $0A, $0A, $0A, $0A, $0A, $0A, $0A, $0A
    !byte $09, $29, $04, $04, $04, $2A, $0B, $0A
    !byte $0A, $0A, $09, $29, $04, $04, $2A, $0B
    !byte $0A, $0A, $0A, $0A, $0A, $0A, $0A, $0A
    !byte $09, $29, $2A, $0B, $0A, $0A, $0A, $0A
    !byte $0A, $0A, $09, $29, $2A, $0B, $0A, $0A
    !byte $0A, $09, $31, $0B, $0A, $0A, $09, $29
    !byte $2A, $0B, $0A, $0A, $0A, $0A, $0A, $0A
    !byte $0A, $0A, $09, $29, $1A, $1E, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
label14d0:
    !byte $1D, $1C, $1C, $1C, $1C, $1C, $1C, $1C
    !byte $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C
    !byte $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C
    !byte $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C
    !byte $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C
    !byte $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C
    !byte $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C
    !byte $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C
    !byte $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C
    !byte $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C
    !byte $1C, $1C, $1C, $1C, $1C, $21, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
    !byte $00, $00, $00, $00, $00, $00, $00, $00
; -----------------------------------------------------------------------------#
label1540:
        sei
        jmp label1594
; -----------------------------------------------------------------------------#
label1544:
        sei
        jsr $fda3
        jsr $fd15
        jsr $ff5b
        lda #$f0
        sta $d01a
        sei
        lda #$00
        sta $d020
        sta $d021
        ldx #$00
label155e:
        lda label157e,x
        sta $0400,x
        inx
        cpx #$16
        bne label155e
        lda #$00
        sta $ae
        sta $2d
        lda #$c0
        sta $af
        sta $2e
        lda #$38
        sta $01
        ldx #$00
        jmp $0400
; -----------------------------------------------------------------------------#
label157e:
        lda label2200,x
        sta $0801,x
        inx
        bne label157e
        inc $0405
        inc $0402
        bne label157e
        dec $01
        jmp $fce2
; -----------------------------------------------------------------------------#
label1594:
        lda #$7f
        sta $dc0d
        lda #$f1
        sta $d01a
        lda #$03
        sta $8c
        lda #$08
        sta $b0
        lda #$00
        sta $b1
        lda #$1b
        sta $d011
        lda #$00
        sta $d012
        sta $40
        sta $3f
        sta $47
        lda #$d1
        sta $0314
        lda #$17
        sta $0315
        jsr label19b6
        jsr label1884
        ldx #$00
label15cc:
        lda label1769,x
        sta $d400,x
        inx
        cpx #$19
        bne label15cc
        ldx #$00
label15d9:
        lda #$0f
        sta $dad0,x
        sta $db00,x
        lda #$24
        sta $06d0,x
        sta $06e8,x
        lda #$0b
        sta $d800,x
        sta $d900,x
        lda #$00
        sta $0400,x
        sta $0500,x
        inx
        bne label15d9
        ldx #$00
label15fe:
        lda #$00
        sta $d9e0,x
        lda #$20
        sta $05e0,x
        inx
        cpx #$f0
        bne label15fe
        ldx #$28
label160f:
        stx $07f8
        inx
        stx $07f9
        inx
        stx $07fa
        inx
        stx $07fb
        inx
        stx $07fc
        inx
        stx $07fd
        lda #$3f
        sta $d015
        lda #$00
        sta $d017
        sta $d01d
        lda #$00
        sta $d027
        sta $d028
        sta $d029
        sta $d02a
        lda #$0c
        sta $d02b
        sta $d02c
        lda #$0f
        sta $d025
        lda #$0b
        sta $d026
        lda #$1f
        sta $d010
        lda #$30
        sta $d01c
        lda #$00
        sta $d000
        lda #$18
        sta $d002
        lda #$30
        sta $d004
        lda #$48
        sta $d006
        lda #$f3
        sta $d001
        sta $d003
        sta $d005
        sta $d007
        lda #$58
        sta $d008
        lda #$94
        sta $d009
        lda #$00
        sta $d00a
        lda #$a6
        sta $d00b
        cli
        ldx #$00
label1696:
        dec $d008
        lda #$0a
        jsr label1782
        inx
        cpx #$19
        bne label1696
        lda #$0f
        sta $48
        lda #$06
        sta $49
        ldx #$00
label16ad:
        stx $44
        lda #$41
        sta $d404
        ldy #$17
label16b6:
        tya
        clc
        adc #$14
        sta $d401
        lda label179e,x
        sta ($48),y
        iny
        lda #$20
        sta ($48),y
        dey
        lda #$01
        jsr label1782
        dey
        cpy $44
        bne label16b6
        lda #$40
        sta $d404
        inx
        cpx #$17
        bne label16ad
        ldx #$00
label16de:
        inc $d00a
        lda #$0a
        jsr label1782
        inx
        cpx #$19
        bne label16de
        lda #$5e
        sta $48
        lda #$06
        sta $49
        ldx #$1b
label16f5:
        stx $43
        lda #$41
        sta $d404
        ldy #$01
label16fe:
        sty $42
        lda #$2d
        sbc $42
        sta $d401
        lda label17b5,x
        sta ($48),y
        dey
        lda #$20
label170f:
        sta ($48),y
        iny
        lda #$01
        jsr label1782
        iny
        cpy $43
        bne label16fe
        lda #$40
label171e:
        sta $d404
        dex
        cpx #$01
        bne label16f5
        ldx #$00
label1728:
        dec $d00a
        inc $d008
        lda #$08
        jsr label1782
        inx
        cpx #$19
        bne label1728
label1738:
        lda #$ff
        jsr label1782
        jsr label1782
        jsr label1782
        ldy #$00
label1745:
        ldx #$00
label1747:
        lda label175f,y
        sta $da60,x
        inx
        cpx #$17
        bne label1747
        lda #$1e
        jsr label1782
        iny
        cpy #$0a
        bne label1745
        jmp label1738
; -----------------------------------------------------------------------------#
; Color values for
label175f:
    !byte   $00, $09, $09, $08, $07, $01, $07, $08
    !byte   $09, $00
; -----------------------------------------------------------------------------#
; SID Values
label1769:
      !byte $02, $00, $FF, $03, $00, $00, $80, $28
    !byte   $04, $00, $07, $41, $0F, $FF, $1A, $02
    !byte   $00, $07, $41, $0F, $FF, $01, $10, $56
    !byte   $3F
; -----------------------------------------------------------------------------#
label1782:
        sty $47
        stx $46
        tax
        ldy #$00
label1789:
        dey
        bne label1789
        lda $dc01
        cmp #$ef
        beq label179b
        dex
        bne label1789
        ldy $47
        ldx $46
        rts
; -----------------------------------------------------------------------------#
label179b:
        jmp label1544
; -----------------------------------------------------------------------------#
; status text
label179e:
!ct scr
    !tx "presents on 01-08-1987:"
label17b5:
    !tx "    dr.jackle and mr.wide   "

; -----------------------------------------------------------------------------#
label17d1:
        lda #$01
        sta $d019
        ldx #$92
        stx $d012
        lda #$00
        sta $d021
        sta $d020
        lda #$13
        sta $d018
        lda #$06
        sta $d022
        lda #$0e
        sta $d023
label17f2:
        lda #$d4
        sta $d016
        jsr label19c6
        jsr label1970
        jsr label1970
        jsr label1970
        lda #<label1810
        sta $0314
        lda #>label1810
        sta $0315
        jmp $ea81
; -----------------------------------------------------------------------------#
label1810:
        lda #$01
        sta $d019
        ldx #$00
        stx $d012
        lda #$19
        sta $d018
        lda #$09
        sta $d022
        lda #$08
        sta $d023
        lda #$c8
        sta $d016
        ldx #$00
        stx $d012
        ldy #$00
label1835:
        iny
        cpy #$32
        bne label1835
label183a:
        lda label1890,x
        sta $d021
        ldy #$09
label1842:
        dey
        bne label1842
        inx
        cpx #$18
        bne label183a
        lda #$d8
        sta $d016
        lda #$00
        sta $d021
        lda #<label17d1
        sta $0314
        lda #>label17d1
        sta $0315
        jsr label18ae
        ldx #$00
label1863:
        lda label2120,x
        sta label0900,x
        inx
        cpx #$08
        bne label1863
        ldy label1890
        ldx #$00
label1873:
        lda label1890+1,x
        sta label1890,x
        inx
        cpx #$1e
        bne label1873
        sty label18ad
        jmp $ea7e
; -----------------------------------------------------------------------------#
label1884:
        ldx #$00
label1886:
        txa
        sta $d400,x
        inx
        cpx #$0f
        bne label1886
        rts
; -----------------------------------------------------------------------------#
label1890:
    !byte $09, $09, $00, $00, $00, $06, $06, $0E
    !byte $0E, $03, $03, $01, $01, $03, $03, $0E
    !byte $0E, $06, $06, $00, $00, $00, $09, $09
    !byte $08, $08, $07, $07, $08
label18ad:
    !byte $08
; -----------------------------------------------------------------------------#
label18ae:
        ldy #$04
label18b0:
        dec $b0
        dey
        bne label18b0
        lda #$d0
        clc
        adc $b0
        tax
        lda $b0
        bmi label18c4
        txa
        sta label17f2+1
        rts
; -----------------------------------------------------------------------------#
label18c4:
        lda #$08
        sta $b0
        ldx #$00
label18ca:
        lda $0401,x
        sta $0400,x
        lda $0429,x
        sta $0428,x
        lda $0451,x
        sta $0450,x
        lda $0479,x
        sta $0478,x
        lda $04a1,x
        sta $04a0,x
        lda $04c9,x
        sta $04c8,x
        lda $04f1,x
        sta $04f0,x
        lda $0519,x
        sta $0518,x
        lda $0541,x
        sta $0540,x
label1900:
        lda $0569,x
        sta $0568,x
        lda $0591,x
        sta $0590,x
        lda $05b9,x
        sta $05b8,x
        inx
        cpx #$27
        bne label18ca
        ldx $b1
        lda label1000,x
        sta $0427
        lda label1070,x
        sta $044f
        lda label10e0,x
        sta $0477
        lda label1150,x
        sta $049f
        lda label11c0,x
        sta $04c7
        lda label1230,x
        sta $04ef
        lda label12a0,x
        sta $0517
        lda label1310,x
        sta $053f
        lda label1380,x
        sta $0567
        lda label13f0,x
        sta $058f
        lda label1460,x
        sta $05b7
        lda label14d0,x
        sta $05df
        inc $b1
        lda $b1
        cmp #$70
        bne label196d
        lda #$00
        sta $b1
label196d:
        jmp label18ae
; -----------------------------------------------------------------------------#
label1970:
        inc $40
        ldx $40
        cpx #$00
        beq label1993
        cpx #$01
        beq label1993
        cpx #$02
        beq label1993
        cpx #$03
        beq label1993
        ldx #$ff
        stx $40
        jmp label1970
; -----------------------------------------------------------------------------#
label198b:
    !byte $20, $20, $20, $20

label198f:
    !byte $d8, $e0, $e8, $f0
; -----------------------------------------------------------------------------#
label1993:
        lda label198f,x
        sta label19a5+1
        lda label198b,x
        sta label19a5+2
        jsr label19a3
        rts
; -----------------------------------------------------------------------------#
label19a3:
        ldx #$00
label19a5:
        lda label20f0,x
        sta label2120,x
        inx
        cpx #$08
        bne label19a5
        rts
; -----------------------------------------------------------------------------#
label19b1:
        lda ($8e),y
        ldx $3e
        rts
; -----------------------------------------------------------------------------#
label19b6:
        lda #>label1a70
        sta $3b
        lda #<label1a70
        sta $3a
        lda #$00
        sta $3e
        rts
; -----------------------------------------------------------------------------#
label19c3:
        jsr label19b6
label19c6:
        lda #<label2000
        sta $8e
        lda #>label2000
        sta $8f
        ldy #$00
        lda ($3a),y
        beq label19c3
        tax
label19d5:
        cpx #$00
        beq label19e7
        lda $8e
        clc
        adc #$08
        sta $8e
        bcc label19e4
        inc $8f
label19e4:
        dex
        bne label19d5
label19e7:
        ldx #$00
label19e9:
        lda $06d1,x
        sta $06d0,x
        lda $06f9,x
        sta $06f8,x
        lda $0721,x
        sta $0720,x
        lda $0749,x
        sta $0748,x
        lda $0771,x
        sta $0770,x
        lda $0799,x
        sta $0798,x
        lda $07c1,x
        sta $07c0,x
        inx
        cpx #$27
        bne label19e9
        ldy #$07
label1a1a:
        jsr label19b1
        and label1a68,x
        cmp label1a68,x
        bne label1a2a
        lda #$24
        jmp label1a2c
; -----------------------------------------------------------------------------#
label1a2a:
        lda #$00
label1a2c:
        sta $0340,y
        dey
        bpl label1a1a
        lda $0342
        sta $071f
        lda $0343
        sta $0747
        lda $0344
        sta $076f
        lda $0345
        sta $0797
        lda $0346
        sta $07bf
        lda #$24
        sta $07e7
        inc $3e
        lda $3e
        cmp #$08
        bne label1a67
        lda #$00
        sta $3e
        inc $3a
        bne label1a67
        inc $3b
label1a67:
        rts
; -----------------------------------------------------------------------------#
label1a68:
        !byte $80, $40, $20, $10, $08, $04, $02, $01
; -----------------------------------------------------------------------------#
label1a70:
        !tx "                      cracked by popeye "
        !tx "  thanx to ronnie for the game.....     "
        !tx "att. popeye is alsow on amiga now.      "
        !tx "   greetings to : rad - triad - fac - th"
        !tx "e movers - plutonium crackers - trianon "
        !tx "- count. dracula - rdi - ronnie - fairli"
        !tx "ght - reflex - fcs - d.s.compware - knp "
        !tx "(danish gold) - paco crew - bca - jayce "
        !tx "- fanatic duo - haddock (the vikings) - "
        !tx "the phantom - hell rider - red sector - "
        !tx "bcs - annika - goodfather - gamebusters "
        !tx "- jazzcat - 1001 crew - proline - dr.h -"
        !tx " tdm - omricon - usa team - electro - he"
        !tx "man - yeti - ice - fct - starline - tsf "
        !tx "- sheild - tau - dgs - fd       "
; -----------------------------------------------------------------------------#
; charset data
            * = $2000 ;"Charset data"
label2000:
        !byte $FF, $6B, $6B, $6B, $6B, $6B, $6B, $55
        !byte $FF, $FF, $83, $39, $21, $39, $39, $FF
        !byte $FF, $FF, $03, $39, $23, $39, $23, $FF
        !byte $FF, $FF, $81, $3F, $3F, $3F, $81, $FF
        !byte $FF, $FF, $03, $39, $39, $39, $23, $FF
        !byte $FF, $FF, $81, $3F, $27, $3F, $81, $FF
        !byte $FF, $FF, $01, $3F, $27, $3F, $3F, $FF
        !byte $FF, $FF, $83, $3F, $31, $39, $83, $FF
        !byte $FF, $FF, $39, $39, $21, $39, $39, $FF
        !byte $FF, $FF, $C3, $E7, $E7, $E7, $C3, $FF
        !byte $FF, $FF, $E1, $F3, $F3, $33, $87, $FF
        !byte $FF, $FF, $39, $33, $27, $33, $39, $FF
        !byte $FF, $FF, $3F, $3F, $3F, $3F, $01, $FF
        !byte $FF, $FF, $39, $11, $29, $39, $39, $FF
        !byte $FF, $FF, $39, $19, $29, $31, $39, $FF
        !byte $FF, $FF, $83, $39, $39, $39, $A3, $FF
        !byte $FF, $FF, $03, $39, $23, $3F, $3F, $FF
        !byte $FF, $FF, $83, $39, $29, $31, $A1, $FF
        !byte $FF, $FF, $03, $39, $23, $37, $39, $FF
        !byte $FF, $FF, $81, $3F, $83, $F9, $03, $FF
        !byte $FF, $FF, $01, $E7, $E7, $E7, $E7, $FF
        !byte $FF, $FF, $39, $39, $39, $39, $81, $FF
        !byte $FF, $FF, $39, $39, $39, $93, $C7, $FF
        !byte $FF, $FF, $39, $39, $29, $11, $39, $FF
        !byte $FF, $FF, $39, $39, $83, $39, $39, $FF
        !byte $FF, $FF, $39, $39, $89, $F9, $03, $FF
        !byte $FF, $FF, $01, $F3, $EF, $9F, $01, $FF
        !byte $00, $94, $94, $94, $94, $94, $94, $AA
        !byte $00, $52, $52, $52, $52, $52, $52, $AA
        !byte $00, $49, $49, $49, $49, $49, $49, $AA
label20f0:
        !byte $00, $25, $25, $25, $25, $25, $25, $AA
        !byte $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
        !byte $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
        !byte $FF, $FF, $E7, $E7, $E7, $FF, $E7, $FF
        !byte $FF, $FF, $93, $93, $FF, $FF, $FF, $FF
        !byte $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
label2120:
        !byte $00, $25, $25, $25, $25, $25, $25, $AA
        !byte $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
        !byte $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
        !byte $FF, $FF, $E7, $F7, $FF, $FF, $FF, $FF
        !byte $FF, $FF, $E3, $CF, $9F, $CF, $E3, $FF
        !byte $FF, $FF, $C7, $F3, $F9, $F3, $C7, $FF
        !byte $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
        !byte $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
        !byte $FF, $FF, $FF, $FF, $FF, $E7, $F7, $FF
        !byte $FF, $FF, $FF, $FF, $83, $FF, $FF, $FF
        !byte $FF, $FF, $FF, $FF, $FF, $E7, $FF, $FF
        !byte $FF, $FF, $9F, $CF, $E7, $F3, $F9, $FF
        !byte $FF, $FF, $83, $31, $29, $19, $83, $FF
        !byte $FF, $FF, $E7, $C7, $E7, $E7, $81, $FF
        !byte $FF, $FF, $03, $F9, $83, $3F, $01, $FF
        !byte $FF, $FF, $03, $F9, $C9, $F9, $03, $FF
        !byte $FF, $FF, $39, $39, $81, $F9, $F9, $FF
        !byte $FF, $FF, $01, $3F, $03, $F9, $03, $FF
        !byte $FF, $FF, $81, $3F, $03, $39, $83, $FF
        !byte $FF, $FF, $01, $79, $C3, $E7, $CF, $FF
        !byte $FF, $FF, $83, $39, $83, $39, $83, $FF
        !byte $FF, $FF, $83, $39, $83, $F9, $03, $FF
        !byte $FF, $FF, $FF, $E7, $FF, $E7, $FF, $FF
        !byte $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
        !byte $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
        !byte $FF, $FF, $FF, $83, $FF, $83, $FF, $FF
        !byte $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
        !byte $FF, $FF, $83, $39, $F3, $FF, $EF, $FF

; -----------------------------------------------------------------------------#
      * = $2200 ;"FakeEntry"
label2200:
FakeEntry:
            ldx   #$0f
-
            lda   FakeBytes,x
            sta   $0800,x
            dex
            bne   -
            jmp   $1540
; -----------------------------------------------------------------------------#
FakeBytes:
        !byte $00, $00, $00, $00, $00, $00, $00, $00
        !byte $FF, $FF, $FF, $FF, $EE, $BB, $AA, $AA
; -----------------------------------------------------------------------------#
