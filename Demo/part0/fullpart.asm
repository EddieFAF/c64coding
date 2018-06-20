;***********************************************************************
;**  Project Name: Loader (alle Teile eingebunden)                    **
;**  ---------------------------------------------------------------  **
;**  Filename: fullpart.a                                             **
;**  ---------------------------------------------------------------  **
;**  Author (c): Eddie                                                **
;**  File Date: 2018-02-28                                            **
;***********************************************************************
!cpu 6510
!sl "fullpart.lbl"
!initmem $ea        ; Speicher vorfüllen
!src "../../stdlib/macros.asm" ; Stellt ein paar Macros bereit
!src "../../stdlib/stdlib.a"

;my Routine, that starts with a nice BASIC line
!macro der_text {
  !pet "faf world domination"
}
year = 1971
!src "../../stdlib/basicstart_template.asm"

            ;jsr $0900   ; loader Init
            jmp .start
            *=$0900
            !bin "../../ACME/install-c64.prg",,2

            *=$9000
            !bin "../../ACME/loader-c64.prg",,2

            * = $c000   ; Nach $c000 verlegen, da stört es die Parts nicht

.dela        !byte 00
.tabcount    !byte 00


; ---------------------------------------------------------------------------
; Sprite Linie - Routinen
; ---------------------------------------------------------------------------
.sprite_line_init:
; ********************************
; ** Spritedaten initialisieren **
; ********************************
            ldx #0
-           lda .sprite_line_data,X
            sta $0340,X
            inx
            cpx #63
            bne -
            rts

.sprite_line_set:
; ********************************
; ** Sprites einrichten         **
; ********************************
            lda #%11111111          ; Sprite 0+1 aktivieren
            sta VIC2SpriteEnable
            ldx #$01
            stx VIC2Sprite0Colour        ; Farbe Sprite 0
            stx VIC2Sprite1Colour        ; Farbe Sprite 1
            stx VIC2Sprite2Colour        ; Farbe Sprite 2
            stx VIC2Sprite3Colour
            stx VIC2Sprite4Colour
            stx VIC2Sprite5Colour
            stx VIC2Sprite6Colour
            stx VIC2Sprite7Colour

            lda #$0D
            sta $07f8               ; Sprite 0
            sta $07f9               ; Sprite 1
            sta $07fa               ; Sprite 2
            sta $07fb
            sta $07fc
            sta $07fd
            sta $07fe
            sta $07ff
            ldx #$00
-           lda .spr_pos,x
            sta VIC2Sprite0X,x
            inx
            cpx #$10
            bne -
            lda #%00000000
            sta VIC2SpriteDoubleWidth
            lda #%00000000
            sta VIC2SpriteXMSB
            rts

.spr_pos:   !byte 158, 141
            !byte 158, 141
            !byte 158, 141
            !byte 158, 141

            !byte 182, 141
            !byte 182, 141
            !byte 182, 141
            !byte 182, 141

.sprite_line_space:
; **********************
; ** Warten auf Space
; **********************
            lda #$7f       ; detect space bar
            sta $dc00
            lda $dc01
            and #$10
            beq .sprite_line_move
            rts

.sprite_line_move:
            ldx VIC2Sprite4X
            inx
            stx VIC2Sprite4X
            cpx #$00
            bne +
            lda VIC2SpriteXMSB
            ora #%10010000
            sta VIC2SpriteXMSB
+           ldx VIC2Sprite0X
            dex
            stx VIC2Sprite0X
            cpx #134
            beq .sprite1
            cpx #110
            beq .sprite2
            cpx #86
            beq .sprite3
            cpx #62
            beq .sprite4
            cpx #38
            beq .sprite5
            cpx #20
            beq .sprite_line_stop
            rts

.sprite1:   ldx #134
            stx VIC2Sprite0X
            lda VIC2SpriteDoubleWidth
            ora #%00100010
            sta VIC2SpriteDoubleWidth
            lda #134
            sta VIC2Sprite1X
            lda #182
            sta VIC2Sprite5X
            rts

.sprite2:   ldx #110
            stx VIC2Sprite2X
            ldx #230
            stx VIC2Sprite6X
            rts

.sprite3:   ldx #86
            stx VIC2Sprite2X
            lda VIC2SpriteDoubleWidth
            ora #%01000100
            sta VIC2SpriteDoubleWidth
            rts

.sprite4:   ldx #62
            stx VIC2Sprite3X
            ldx #022
            stx VIC2Sprite7X
            rts

.sprite5:   ldx #38
            stx VIC2Sprite3X
            lda VIC2SpriteDoubleWidth
            ora #%10001000
            sta VIC2SpriteDoubleWidth
            rts

; **********************
; ** stoppe Bewegung
; **********************
.sprite_line_stop:
            lda #<.color_move
            sta .irq_jump_target+1
            lda #>.color_move
            sta .irq_jump_target+2
            lda #$00
            sta VIC2SpriteEnable
            rts

; **********************
; ** setze RTS
; **********************
.sprite_line_set_rts:
            lda #<.sprite_line_set_rts_exit
            sta .irq_jump_target+1
            lda #>.sprite_line_set_rts_exit
            sta .irq_jump_target+2
.sprite_line_set_rts_exit:
            rts
; ---------------------------------------------------------------------------

; ********************************
; ** Texteinblendung - Routinen **
; ********************************

.screen_init:
; ********************************
; ** Setzen der Farben
; ********************************
            ldx #$00
-           lda .char_colors,x
            sta $D968,x
            lda .char_colors+$18,x
            sta $D980,x
            inx
            bne -
            rts

.sprite_text_init:
; ********************************
; ** Spritedaten initialisieren **
; ********************************
            ldx #0
-           lda .sprite_text_data,X
            sta $0340,X
            inx
            cpx #63
            bne -
            rts

.sprite_text_set:
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

.sprite_text_move:
; **********************
; ** Text einblenden
; **********************
            dec .spr_counter
            dec .spr_counter
            bne .sprite_move_loop
            ldx .charpos
            cpx #$28
            bne +
            jmp .raster_set_space ; Warten auf Space setzen (neu: Part laden)
+           lda .char_screen,x
            sta $0568,x
            lda .char_screen+40,x
            sta $0590,x
            lda .char_screen+80,x
            sta $05B8,x
            lda .char_screen+120,x
            sta $05E0,x
            lda .char_screen+160,x
            sta $0608,x
            lda .char_screen+200,x
            sta $0630,x
            lda .char_screen+240,x
            sta $0658,x

            lda #$08
            sta .spr_counter
            inc .charpos
.sprite_move_loop:
            inc VIC2Sprite0X
            inc VIC2Sprite0X
            inc VIC2Sprite1X
            inc VIC2Sprite1X
            bne +
            lda #$03
            sta VIC2SpriteXMSB
+           rts

.sprite_back:
; **********************
; ** Text ausblenden
; **********************
            dec .spr_counter
            dec .spr_counter
            bne .sprite_back_loop
            ldx .charpos
            cpx #$00
            bne +
            jmp .set_fadeout
+           lda #$20
            sta $0568,x
            sta $0590,x
            sta $05B8,x
            sta $05E0,x
            sta $0608,x
            sta $0630,x
            sta $0658,x

            lda #$08
            sta .spr_counter
            dec .charpos
.sprite_back_loop:
            dec VIC2Sprite0X
            dec VIC2Sprite0X
            dec VIC2Sprite1X
            dec VIC2Sprite1X
            bpl +
            lda #$00
            sta VIC2SpriteXMSB
+           rts

.sprite_set_back:
; **********************
; ** Setze Zeichen löschen
; **********************
            lda #<.sprite_back
            sta .sprite_change_2+1
            lda #>.sprite_back
            sta .sprite_change_2+2

            lda #$03
            sta VIC2SpriteEnable
            lda #$28
            sta .charpos
            lda #$08
            sta .spr_counter
            rts

.sprite_set_rts:
; **********************
; ** setze RTS
; **********************
            lda #<.sprite_set_rts_exit
            sta .sprite_change_2+1
            lda #>.sprite_set_rts_exit
            sta .sprite_change_2+2
            lda #$00
            sta VIC2SpriteEnable ;Sprites abschalten
.sprite_set_rts_exit:
            rts

.raster_set_space:
; **********************
; ** setze 'Warten auf Space'
; **********************
            lda #<.wait_space
            sta .sprite_change_2+1
            lda #>.wait_space
            sta .sprite_change_2+2
            lda #$00
            sta VIC2SpriteEnable
            rts

.wait_space:
; **********************
; ** Warten auf Space
; **********************
            lda #$7f       ; detect space bar
            sta $dc00
            lda $dc01
            and #$10
            bne +
            jmp .sprite_set_back
+           rts

; **********************
.spr_counter: !byte 08
.charpos:     !byte 00


.charset_colors:
!byte 14,0

.char_screen:
!byte 32,32,32,32,32,32,112,64,110,32,32,32,32,96,96,96,96,96,32,32,112,64,114,64,110,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32
!byte 32,64,32,64,64,32,93,32,66,32,112,64,64,64,114,64,64,64,114,64,125,32,107,64,91,64,64,64,114,64,64,64,110,32,64,64,32,64,32,32
!byte 32,32,32,32,32,32,66,32,66,32,66,32,66,32,107,64,125,32,66,32,66,32,66,32,66,32,66,32,66,32,66,32,66,32,32,32,32,32,32,32
!byte 32,32,32,32,32,32,66,32,107,64,115,32,66,32,66,32,66,32,66,32,66,32,66,32,66,32,66,32,66,32,66,32,66,32,32,32,32,32,32,32
!byte 32,32,32,32,32,32,66,32,66,32,66,32,66,32,66,32,66,32,66,32,66,32,66,32,66,32,66,32,66,32,66,32,66,32,32,32,32,32,32,32
!byte 32,64,32,64,64,32,109,64,64,64,113,64,64,64,113,64,64,64,113,64,64,64,113,64,113,64,113,64,91,64,110,32,66,32,64,64,32,64,32,32
!byte 32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,109,64,64,64,125,32,32,32,32,32,32,32

.char_colors:
!byte 14,14,14,14,14,14,1,15,10,14,14,14,14,1,1,1,1,1,14,14,1,1,1,1,1,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14
!byte 14,11,14,12,1,14,15,14,8,14,9,11,12,15,1,1,1,1,1,1,1,14,1,1,1,1,1,1,1,1,1,13,15,14,1,12,14,11,14,14
!byte 14,14,14,14,14,14,10,14,11,14,11,14,15,14,1,1,1,14,1,14,1,14,1,14,1,14,1,14,1,14,13,14,3,14,14,14,14,14,14,14
!byte 14,14,14,14,14,14,8,14,9,11,12,14,1,14,1,14,1,14,1,14,1,14,1,14,1,14,1,14,1,14,15,14,10,14,14,14,14,14,14,14
!byte 14,14,14,14,14,14,11,14,11,14,15,14,1,14,1,14,1,14,1,14,1,14,1,14,1,14,1,14,13,14,3,14,14,14,14,14,14,14,14,14
!byte 14,11,14,12,1,14,9,11,12,15,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,13,15,3,10,14,4,14,1,12,14,11,14,14
!byte 14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,3,10,14,4,6,14,14,14,14,14,14,14

; ---------------------------------------------------------------------------
; F A D E - O U T
; ---------------------------------------------------------------------------

; **********************
; ** Setze Zeichen löschen
; **********************
.set_fadeout:
            lda #<.fade_out
            sta .sprite_change_2+1
            lda #>.fade_out
            sta .sprite_change_2+2
            lda #$00
            sta .tabcount
            sta .dela
            rts

.fade_out:   ldx .dela
            inx
            stx .dela
            cpx #$04
            beq .col_fade
            rts

.col_fade:   ldx #$00
            stx .dela
            ldx .tabcount
            inx
            stx .tabcount
            cpx #$08
            bne .col_cont
            jmp .sprite_set_rts

.col_cont:   ldx .tabcount
            lda .fade_tab1,x
            sta .raster1a+1
            sta .raster1b+1
            lda .fade_tab2,x
            sta .raster2+1
            lda .fade_tab3,x
            sta .raster3a+1
            sta .raster3b+1
            rts

.fade_tab1: ; Raster
            !byte $01, $0f, $0a, $08, $0c, $09, $0b, $00

.fade_tab2: ; Hintergrund
            !byte $06, $0e, $03, $01, $03, $0e, $06, $00

.fade_tab3: ; Rahmen
            !byte $0e, $0e, $03, $01, $03, $0e, $06, $00
; ---------------------------------------------------------------------------

; **********************
.start:

RASTER      = $6f                              ;Hier den 1. Raster-IRQ auslösen

            jsr $e544                          ;Bildschirm löschen
            +SetBorderColor 14
            +SetBackgroundColor 6

            jsr .sprite_line_init
            jsr .sprite_line_set

            sei                                ;IRQs sperren
            lda #<.myIRQ                       ;Adresse unserer Routine in
            sta IRQServiceRoutineLo            ;den RAM-Vektor
            lda #>.myIRQ
            sta IRQServiceRoutineHi
            lda #%00000001                     ;Raster-IRQs vom VIC-II aktivieren
            sta VIC2InteruptControl
            lda #RASTER                        ;Hier soll unsere Linie erscheinen
            sta VIC2Raster
            lda VIC2ScreenControlV             ;Zur Sicherheit höchste BIT
            and #%01111111                     ;für die Rasterzeile löschen
            sta VIC2ScreenControlV
            lda #%01111111                     ;Timer-IRQs abschalten
            sta CIA1InterruptControl
            lda CIA1InterruptControl
            lda #%0000001                      ;evtl. aktiven Raster-IRQ bestätigen
            sta VIC2InteruptStatus
            cli

            jmp *

;*** an Pagegrenze ausrichten, damit die Sprünge passen
!align 255,0

.myIRQ:      lda #<.doubleIRQ
            sta IRQServiceRoutineLo
            lda #>.doubleIRQ
            sta IRQServiceRoutineHi
            tsx
            stx .doubleIRQ+1
            nop
            nop
            nop
            lda #%00000001

            inc $d012
            sta VIC2InteruptStatus
            cli

            ldx #$08
            dex
            bne *-1

            nop
            nop
            nop
            nop
            nop
            nop

.doubleIRQ:  ldx #$00
            txs
            nop
            nop
            nop
            nop
            bit $01
            ldx $D012
            lda #$01
            cpx $D012

            beq .myIRQMain

.myIRQMain:  ldx #$ff
.nextColor:  inx                                 ;Schleifenzähler erhöhen
            ldy .delaytable,X                    ;Wartezeit holen
            dey                                 ;verringern
            bne *-1                             ;solange größer 0 zurück zum DEY
            lda .rowcolortable,X                 ;Farbe holen
            sta VIC2ScreenColour                ;und ins Register für die Hintergrundfarbe
            cpx #$42
            bne .nextColor                       ;solange die Farbe positiv ist -> @loop
            +SetBackgroundColor 6
            lda #<.myIRQ                         ;Original IRQ-Vektor setzen
            sta IRQServiceRoutineLo
            lda #>.myIRQ
            sta IRQServiceRoutineHi

            lda #RASTER
            sta VIC2Raster

            lda #%00000001                      ;IRQ bestätigen
            sta VIC2InteruptStatus
.irq_jump_target:
            jsr .sprite_line_move
            jmp $ea81                           ;zum Ende des 'Timer-Interrupts' springen

; *************************************************
; ** Vorbereitung auf neue IRQ Routinen          **
; *************************************************
.set_new_irq:
            lda #$15                            ; Kleinbuchstaben
            sta VIC2MemorySetup

            jsr .screen_init                     ; Bildschirm vorbereiten
            jsr .sprite_text_init                ; Sprites vorbereiten
            jsr .sprite_text_set                 ; Sprites einschalten
        ; register first interrupt
            sei
            lda #$7f
            sta CIA1InterruptControl             ; turn off the CIA interrupts
            sta CIA1InterruptControl
            and VIC2ScreenControlV               ; clear high bit of raster line
            sta VIC2ScreenControlV
            +irqEnd $71, .irq1
            lda #$01                             ; enable raster interrupts
            sta VIC2InteruptControl
            cli
            jmp *

; ************************************************
; ** IRQ Routinen                               **
; ************************************************
.irq1:       ldx #$05
            dex
            bne *-1
.raster3a:   lda #$0e
            sta VIC2BorderColour
.raster1a:   lda #$01
            sta VIC2ScreenColour
            ldx #$08
            dex
            bne *-1
            lda #$00
            sta VIC2ScreenColour
            +irqEnd $B0, .irq2
            inc VIC2InteruptStatus     ; acknowledge interrupt
            jmp $ea31

.irq2:       lda #$B2
            cmp $d012
            bne *-3
            ldx #$09
            dex
            bne *-1
.raster3b:   lda #$0e
            sta VIC2BorderColour
.raster1b:   lda #$01
            sta VIC2ScreenColour
            ldx #$04
            dex
            bne *-1
.raster2:    lda #$06
            sta VIC2ScreenColour
.sprite_change_2:
            jsr .sprite_text_move ; Einblenden
            +irqEnd $71, .irq1
            inc VIC2InteruptStatus      ; acknowledge interrupt
            jmp $ea31

; ---------------------------------------------------------------------------
; R A S T E R - R O U T I N E N
; ---------------------------------------------------------------------------
.color_move: ldx .dela
            inx
            stx .dela
            cpx #$02
            beq .col_mov
            rts

.col_mov:    ldx #$00
            stx .dela
            ldx .tabcount
            inx
            stx .tabcount
            cpx #$21
            bne +
            jmp .set_new_irq
+           ldx #$00
-           lda .rowcolor1+$02,x
            sta .rowcolortable,x
            inx
            cpx #$21
            bne -

            ldx #$00
-           lda .rowcolor2,x
            sta .rowcolortable+$21,x
            inx
            cpx #$21
            bne -
            jsr .copy_tabs
            rts

;umkopieren der Tabellen
.copy_tabs:  ldx #$00
-           lda .rowcolor1+1,x
            sta .rowcolor1,x
            inx
            cpx #$22
            bne -
            lda #$00
            sta .rowcolor1+$22

            ldx #$22
-           lda .rowcolor2,x
            sta .rowcolor2+1,x
            dex
            bpl -
            lda #$00
            sta .rowcolor2
            rts


; ---------------------------------------------------------------------------
; S P R I T E D A T A
; ---------------------------------------------------------------------------

!align 63,0
.sprite_line_data:
            !byte %00000000,%00000000,%00000000
            !byte %00000000,%00000000,%00000000
            !byte %00000000,%00000000,%00000000
            !byte %00000000,%00000000,%00000000
            !byte %00000000,%00000000,%00000000
            !byte %00000000,%00000000,%00000000
            !byte %00000000,%00000000,%00000000
            !byte %00000000,%00000000,%00000000
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %00000000,%00000000,%00000000
            !byte %00000000,%00000000,%00000000
            !byte %00000000,%00000000,%00000000
            !byte %00000000,%00000000,%00000000
            !byte %00000000,%00000000,%00000000
            !byte %00000000,%00000000,%00000000
            !byte %00000000,%00000000,%00000000
            !byte %00000000,%00000000,%00000000
            !byte %00000000,%00000000,%00000000
            !byte %00000000,%00000000,%00000000
            !byte %00000000,%00000000,%00000000
            !byte $00

!align 63,0
.sprite_text_data:
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte %11111111,%11111111,%11111111
            !byte $00
; ---------------------------------------------------------------------------

!align 255,0
.delaytable:
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

.rowcolortable: !fill $50,6

.rowcolor1:
            !byte 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6
            !byte 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6
            !byte 6, 6, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
            !byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
            !byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
            !byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.rowcolor2:
            !byte 1, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6
            !byte 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6
            !byte 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6
            !byte 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6
            !byte 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6
            !byte $f6

