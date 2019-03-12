;*********************************************************************72
;**  Project Name: Demo Project                                       **
;**  ---------------------------------------------------------------  **
;**  Filename: Part1.asm                                              **
;**  ---------------------------------------------------------------  **
;**  Author (c): [FAF]Eddie                                           **
;**  File Date: 2018-04-11                                            **
;***********************************************************************
;**  Beschreibung:                                                    **
;**  Fadeout Basic Bildschirm zeilenweise                             **
;**  Einblenden des Parts                                             **
;***********************************************************************
;**  Code: $0801-$0d73
;**  Sound: $1000-$2291
;**  GFX: $4000-$4cd2
;***********************************************************************
; ACME Style
.RELEASE = 0

            !src "../../../lib/acme/stdlib.a" ; Sprungziele
            !src "../../../lib/acme/macros.asm"
            !src "../loader.inc"

.soundinit              = $1000
.soundplay              = $1003

                        ; constants
C_SCREEN_RAM            = $0400
C_COLOUR_RAM            = $d800

.speed_char            = $02
.delay_char            = $08
.speed_border          = $04
.speed_screen          = $04
.delay_screen          = $20


!if .RELEASE { !set C_EXIT_PART = exit_intro_part } else { !set C_EXIT_PART = $fce2 }
!if .RELEASE { !set C_APPLY_INTERRUPT = apply_interrupt } else { !set C_APPLY_INTERRUPT = APPLY_INTERRUPT }

!if .RELEASE=0 {
;my Routine, that starts with a nice BASIC line
!macro der_text {
  !pet "faf world domination"
}
year = 1971
!src "../../../lib/acme/basicstart_template.asm"
} else { *=$0A00 }

!if .RELEASE=0 {
            jmp sync_intro
APPLY_INTERRUPT:
            sta $D012
            stx $0314
            sty $0315
            jmp $ea81
}

sync_intro: lda #$15        ; Kleinbuchstaben
            sta VIC2MemorySetup
            lda #$00
            jsr .soundinit  ; Sound Init
            ; setzen Start Farbram
            lda #<$D800
            sta $FB
            lda #>$D800
            sta $FC
            lda VIC2BorderColour ; Farben initialisieren
            sta .c10+1

            lda VIC2ScreenColour
            sta .c11+1

!if .RELEASE=0 {
            sei
            lda #$7f
            sta $dc0d       ; turn off the CIA interrupts
            sta $dd0d
            and $d011       ; clear high bit of raster line
            sta $d011
            +irqEnd $2F, .p1irq1

            lda #$01        ; enable raster interrupts
            sta $d01a
            cli
            jmp *           ; Endlosschleife
}

            lda #$2F
            ldx #<.p1irq1
            ldy #>.p1irq1
            jmp C_APPLY_INTERRUPT

            ; IRQ Routinen --------------------------------------------------]
            ; ---------------------------------------------------------------]

            ; IRQ Routine Teil 1 --------------------------------------------]
            ; Gesamter sichtbarer Bildschirm --------------------------------]
            ; Sound ---------------------------------------------------------]
            ; ---------------------------------------------------------------]
.p1irq1:    inc VIC2InteruptStatus     ; acknowledge interrupt
            lda #$1b
            sta VIC2ScreenControlV
            lda #$15 ; #%10000000
            sta VIC2MemorySetup    ; $D018 Speicherbereiche
            lda #$C8 ; #%00011000
            sta VIC2ScreenControlH ; $D016

            lda #$31
            cmp $d012
            bne *-3
            ldx #$0A
            dex
            bne *-1
.c10:       lda #$0e
            sta VIC2BorderColour
.c11:       lda #$06
            sta VIC2ScreenColour

.subroutine:
            jsr .charfade_warte

            jsr .soundplay  ; Sound play

            lda #$F0
            ldx #<.p1irq3
            ldy #>.p1irq3
            jmp C_APPLY_INTERRUPT

            ; IRQ unterhalb des Bildschirms ---------------------------------]
            ; ---------------------------------------------------------------]
.p1irq3:    inc VIC2InteruptStatus     ; acknowledge interrupt
            lda #$FA
            cmp $d012
            bne *-3
            ldx #$0A        ; delay
            dex
            bne *-1

            lda .part_done  ; Wenn noch nicht Ende des Parts,
                            ; dann p1irq, sonst p2irq1
            cmp #$01
            bne +
            lda #$2F
            ldx #<.p2irq1
            ldy #>.p2irq1
            jmp C_APPLY_INTERRUPT

+           lda #$2F
            ldx #<.p1irq1
            ldy #>.p1irq1
            jmp C_APPLY_INTERRUPT


            ; IRQ Routine Teil 2 --------------------------------------------]
            ; Obere Hälfte --------------------------------------------------]
.p2irq1:    inc VIC2InteruptStatus     ; acknowledge interrupt
            lda #$1b
            sta VIC2ScreenControlV
            lda #$15 ; #%10000000
            sta VIC2MemorySetup    ; $D018 Speicherbereiche
            lda #$C8 ; #%00011000
            sta VIC2ScreenControlH ; $D016

            lda #$31
            cmp $d012
            bne *-3
            ldx #$0A
            dex
            bne *-1
.bc10:      lda #$00
            sta VIC2BorderColour
.bc11:      lda #$00
            sta VIC2ScreenColour
            ldx #$09            ; delay
            dex
            bne *-1
.bc20:      lda #$00
            sta VIC2ScreenColour
.bc21:      lda #$00
            sta VIC2BorderColour
            lda #$A0
            ldx #<.p2irq2
            ldy #>.p2irq2
            jmp C_APPLY_INTERRUPT

            ; IRQ Mitte (Routinen) ------------------------------------------]
            ; ---------------------------------------------------------------]
.p2irq2:    inc VIC2InteruptStatus     ; acknowledge interrupt
            lda #$a2
            cmp $d012
            bne *-3
.bsubroutine:
            jsr .bpre_warte

            lda #$F0
            ldx #<.p2irq3
            ldy #>.p2irq3
            jmp C_APPLY_INTERRUPT

            ; IRQ unterhalb des Bildschirms ---------------------------------]
            ; ---------------------------------------------------------------]
.p2irq3:    inc VIC2InteruptStatus     ; acknowledge interrupt
            lda #$FA
            cmp $d012
            bne *-3
            ldx #$0A        ; delay
            dex
            bne *-1
.bc30:      lda #$00
            sta VIC2BorderColour
.bc31:      lda #$00
            sta VIC2ScreenColour
            ldx #$09        ; delay
            dex
            bne *-1
            nop
            nop
.bc40:      lda #$00
            sta VIC2ScreenColour
.bc41:      lda #$00
            sta VIC2BorderColour
            lda .fade_count
            cmp #$10
            bne +
            lda #$2F
            ldx #<.p3irq1
            ldy #>.p3irq1
            jmp C_APPLY_INTERRUPT

+           lda #$2F
            ldx #<.p2irq1
            ldy #>.p2irq1
            jmp C_APPLY_INTERRUPT



            ; IRQ Routine Teil 3 --------------------------------------------]
            ; Oberer Bildschirm ---------------------------------------------]
.p3irq1:    inc VIC2InteruptStatus     ; acknowledge interrupt
            lda #%00111011
            sta VIC2ScreenControlV
            lda #%10000000
            sta VIC2MemorySetup    ; $D018 Speicherbereiche
            lda #%00011000
            sta VIC2ScreenControlH ; $D016
            lda #%00000010
            sta $dd00

            lda #$31
            cmp $d012
            bne *-3
            ldx #$0A
            dex
            bne *-1
            lda #$01
            sta VIC2BorderColour
            lda #$01
            sta VIC2ScreenColour
            ldx #$09            ; delay
            dex
            bne *-1
            lda #$00
            sta VIC2ScreenColour
            lda #$00
            sta VIC2BorderColour
            lda #$6F
            ldx #<.p3irq1b
            ldy #>.p3irq1b
            jmp C_APPLY_INTERRUPT

            ; IRQ Mitte Bildschirm ------------------------------------------]
            ; Ende Grafikmodus ----------------------------------------------]
.p3irq1b:   inc VIC2InteruptStatus     ; acknowledge interrupt
            lda #$71
            cmp $d012
            bne *-3
            ldx #$16
            dex
            bne *-1
            lda #%00000011
            sta $dd00
            lda #$1b
            sta VIC2ScreenControlV ; $D011 Grafikmodus
            +SetScreenAndCharLocation $0400, $1000
            lda #%00010000
            sta VIC2ScreenControlH ; $D016
            lda #$00
            sta VIC2ScreenColour
            lda #%11111101         ; detect space bar
            sta CIA1KeyboardColumnJoystickA
            lda CIA1KeyboardRowsJoystickB
            and #%00100000	; S
            bne .bupdate_irq
            jmp C_EXIT_PART

.bupdate_irq:
            lda #$A0
            ldx #<.p3irq2
            ldy #>.p3irq2
            jmp C_APPLY_INTERRUPT

            ; IRQ Untere Hälfte ---------------------------------------------]
            ; ---------------------------------------------------------------]
.p3irq2:    inc VIC2InteruptStatus     ; acknowledge interrupt
            lda #$a2
            cmp $d012
            bne *-3
            inc $d020
            jsr .soundplay             ; Sound Play
            dec $d020
            lda #$F0
            ldx #<.p3irq3
            ldy #>.p3irq3
            jmp C_APPLY_INTERRUPT

            ; IRQ Unten -----------------------------------------------------]
            ; Ende des Bildschirms ------------------------------------------]
.p3irq3:    inc VIC2InteruptStatus     ; acknowledge interrupt
            lda #$F9
            cmp $d012
            bne *-3
            ldx #$0A            ; delay
            dex
            bne *-1
            lda #$01
            sta VIC2BorderColour
            lda #$01
            sta VIC2ScreenColour
            ldx #$09            ; delay
            dex
            bne *-1
            nop
            nop
            lda #$06
            sta VIC2ScreenColour
            lda #$0e
            sta VIC2BorderColour
            lda #$2F
            ldx #<.p3irq1
            ldy #>.p3irq1
            jmp C_APPLY_INTERRUPT

            ; Beginn der Unterroutinen --------------------------------------]
            ; ---------------------------------------------------------------]

            ; Verzögerung, bevor Effekt startet
.charfade_warte:
            ldx .charfade_delay
            inx
.charfade_warte_delay:
            cpx #.delay_char
            bne .charfade_warte_update
            ;setzen des faders
            ldx #<.charfade_out
            ldy #>.charfade_out
            stx .subroutine+1
            sty .subroutine+2
            ldx #$00
.charfade_warte_update:
            stx .charfade_delay
.sub_rts:   rts

            ; Zeilenweise Bildschirm faden ----------------------------------]
            ; ---------------------------------------------------------------]
.charfade_out: ; Verzögerung
            ldx .charfade_delay
            inx
            cpx #.speed_char ; nur jeden zweiten Aufruf ausführen, sonst zu schnell
            bne +
            jsr .fade_out
            ldx #$00
+           stx .charfade_delay
            rts

.fade_out:  ; Neue Zeile vorbereiten
            ldx #$00
            stx .charfade_delay
            ldx .charfade_count
            inx
            cpx #$08
            bne .charfade_real
            lda $FB
            clc
            adc #$28
            sta $FB
            bcc +
            inc $FC
            lda $FC
            ; Ende des Bildschirms erreicht?
            cmp #$DC
            beq .charfade_done
+           ldx #$00
            stx .charfade_count
            rts

            ; Bildschirm ist ausgeblendet, jetzt leeren und weiter
            ; mit Rahmen ausblenden
.charfade_done:
            jsr $e544
            lda #$00
            sta .charfade_delay
            sta .charfade_count
            ldx #<.borderfade_out
            ldy #>.borderfade_out
            stx .subroutine+1
            sty .subroutine+2
            rts

            ; Die eigentliche Ausblendroutine -------------------------------]
            ; ---------------------------------------------------------------]
.charfade_real:
            stx .charfade_count

            ldy #$00
            lda $d021 ;hole zielfarbe
            asl
            asl
            asl
            asl
            sta .charfade_offset ;setze offset
-           lda ($FB),y
            and #$0F
            ora .charfade_offset
            tax
            lda fade_table,x
            sta ($FB),y

            iny
            cpy #$28 ; 40 Zeichen
            bne -
            rts

            ; Rahmen ausblenden ---------------------------------------------]
            ; ---------------------------------------------------------------]
.borderfade_out: ; Verzögerung
            ldx .charfade_delay
            inx
            cpx #.speed_border ; noch langsamer, jeder 8te Durchlauf
            bne +
            jsr .border_out
            ldx #$00
+           stx .charfade_delay
            rts

.border_out: ; Der Effekt
            ldx #$00
            stx .charfade_delay
            ldx .charfade_count
            inx
            cpx #$08
            bne +
            ; Ende des Effekts und Vorbereitung nächste Stufe
            jsr .gfx
            ldx #<.sub_rts
            ldy #>.sub_rts
            stx .subroutine+1
            sty .subroutine+2
            lda #$01
            sta .part_done

            ldx #$00
+           stx .charfade_count
            ; Das eigentliche Faden
            lda #$00
            asl
            asl
            asl
            asl
            sta .charfade_offset
            lda .c10+1
            and #$0f
            ora .charfade_offset
            tax
            lda fade_table,x
            sta .c10+1

            lda .c11+1
            and #$0f
            ora .charfade_offset
            tax
            lda fade_table,x
            sta .c11+1
            rts


            ; ---------------------------------------------------------------]
.bpre_warte:    ; Verzögerung, bevor Effekt startet
            ldx .delay
            inx
.bpre_warte_delay:
            cpx #.delay_screen ; $40
            bne .bpre_warte_update
            ;setzen des faders
            ldx #<.bscreen_fade_in
            ldy #>.bscreen_fade_in
            stx .bsubroutine+1
            sty .bsubroutine+2
            ldx #$00
.bpre_warte_update:
            stx .delay
            rts

            ; Bildschirm einblenden -----------------------------------------]
            ; ---------------------------------------------------------------]
.bscreen_fade_in:
            ldx .pre_fade_delay
            inx
            cpx #.speed_screen ; $04
            bne .bscreen_fade_in1
            jsr .bfade_in ; Einblendung starten, Stufe für Stufe
            ldx #$00
.bscreen_fade_in1:
            stx .pre_fade_delay
            rts

            ; Der eigentliche Fade-In Effekt --------------------------------]
            ; ---------------------------------------------------------------]
.bfade_in:  ldx #$00
            stx .delay
            ldx .fade_count
            inx
            cpx #$10
            bne +
+           stx .fade_count

            lda .Original_c10 ;hole zielfarbe
            asl
            asl
            asl
            asl
            sta .fade_in_offset ;setze offset
            lda .bc10+1 ;hole aktuelle farbe
            and #$0f ;und isoliere low nibble (muss gemacht werden sonst kommt mist raus)
            ora .fade_in_offset ; "addiere" offset
            tax ;akku nach X
            lda fade_table,x ;hole fade value
            sta .bc10+1

            lda .Original_c11
            asl
            asl
            asl
            asl
            sta .fade_in_offset
            lda .bc11+1
            and #$0f
            ora .fade_in_offset
            tax
            lda fade_table,x
            sta .bc11+1

            lda .Original_c20
            asl
            asl
            asl
            asl
            sta .fade_in_offset
            lda .bc20+1
            and #$0f
            ora .fade_in_offset
            tax
            lda fade_table,x
            sta .bc20+1

            lda .Original_c21
            asl
            asl
            asl
            asl
            sta .fade_in_offset
            lda .bc21+1
            and #$0f
            ora .fade_in_offset
            tax
            lda fade_table,x
            sta .bc21+1

            lda .Original_c30
            asl
            asl
            asl
            asl
            sta .fade_in_offset
            lda .bc30+1
            and #$0f
            ora .fade_in_offset
            tax
            lda fade_table,x
            sta .bc30+1

            lda .Original_c31
            asl
            asl
            asl
            asl
            sta .fade_in_offset
            lda .bc31+1
            and #$0f
            ora .fade_in_offset
            tax
            lda fade_table,x
            sta .bc31+1

            lda .Original_c40
            asl
            asl
            asl
            asl
            sta .fade_in_offset
            lda .bc40+1
            and #$0f
            ora .fade_in_offset
            tax
            lda fade_table,x
            sta .bc40+1

            lda .Original_c41
            asl
            asl
            asl
            asl
            sta .fade_in_offset
            lda .bc41+1
            and #$0f
            ora .fade_in_offset
            tax
            lda fade_table,x
            sta .bc41+1
            rts

            ; Grafik vorbereiten, kopieren Screen und FarbRAM ---------------]
            ; ---------------------------------------------------------------]
.gfx:       ldx #$00
-           lda colora,x
            sta $6000,x
            lda colora+$40,x
            sta $6000+$40,x
            lda colorb,x
            sta $d800,x
            lda colorb+$40,x
            sta $d800+$40,x
            inx
            bne -
            rts


            ; Diverse Tabellen und Zwischenspeicher -------------------------]
            ; ---------------------------------------------------------------]

            ; Originalen Farbwerte vorbelegen
.Original_c10: !byte $01
.Original_c11: !byte $01
.Original_c20: !byte $00
.Original_c21: !byte $00
.Original_c30: !byte $01
.Original_c31: !byte $01
.Original_c40: !byte $06
.Original_c41: !byte $0e

.border_table:
            !byte $00, $03, $01, $03, $0e, $04, $06, $00
            !byte $00, $00

.charfade_delay:     !byte $00
.charfade_count:     !byte $00
.charfade_offset:    !byte $00
.delay:              !byte $00
.pre_fade_delay:     !byte $00
.fade_count:         !byte $00
.fade_in_offset:     !byte $00
.part_done:          !byte $00

            ; Color Fade Table ----------------------------------------------]
            ; ---------------------------------------------------------------]
            !src "../../../lib/acme/tables.asm"

            *=$1000
            !bin "../ode_to_64.bin"

            *=$4000
            !source "gfx.txt"
