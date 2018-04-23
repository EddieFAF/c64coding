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

            jsr $0900   ; loader Init
            jmp start
            *=$0900
            !bin "../../install-c64.prg",,2
            
            *=$9000
            !bin "../../loader-c64.prg",,2
            
            * = $c000   ; Nach $c000 verlegen, da stört es die Parts nicht

dela        !byte 00 
tabcount    !byte 00


;!src "../subroutines.asm"
!src "spriteline.asm" ; Import Spriteroutinen Linie
!src "text.asm"       ; Import Textdarstellung
!src "fadeout.asm"    ; Import Fadeout Routinen

; **********************
start

RASTER          = $6f                       ;Hier den 1. Raster-IRQ auslösen
 
            jsr $e544                           ;Bildschirm löschen
            +SetBorderColor 14
            +SetBackgroundColor 6

            jsr sprite_line_init
            jsr sprite_line_set

            sei                                 ;IRQs sperren
            lda #<myIRQ                         ;Adresse unserer Routine in
            sta IRQServiceRoutineLo             ;den RAM-Vektor
            lda #>myIRQ
            sta IRQServiceRoutineHi
            lda #%00000001                      ;Raster-IRQs vom VIC-II aktivieren
            sta VIC2InteruptControl
            lda #RASTER                         ;Hier soll unsere Linie erscheinen
            sta VIC2Raster                      
            lda VIC2ScreenControlV              ;Zur Sicherheit höchste BIT
            and #%01111111                      ;für die Rasterzeile löschen
            sta VIC2ScreenControlV
            lda #%01111111                      ;Timer-IRQs abschalten
            sta CIA1InterruptControl
            lda CIA1InterruptControl
            lda #%0000001                       ;evtl. aktiven Raster-IRQ bestätigen
            sta VIC2InteruptStatus
            cli 

            jmp *
 
;*** an Pagegrenze ausrichten, damit die Sprünge passen
!zone
!align 255,0
 
myIRQ       lda #<doubleIRQ
            sta IRQServiceRoutineLo
            lda #>doubleIRQ
            sta IRQServiceRoutineHi
            tsx
            stx doubleIRQ+1
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

doubleIRQ   ldx #$00
            txs
            nop
            nop
            nop
            nop
            bit $01
            ldx $D012
            lda #$01
            cpx $D012

            beq myIRQMain

myIRQMain   ldx #$ff
nextColor   inx                                 ;Schleifenzähler erhöhen
            ldy delaytable,X                    ;Wartezeit holen
            dey                                 ;verringern
            bne *-1                             ;solange größer 0 zurück zum DEY
            lda rowcolortable,X                 ;Farbe holen
            sta VIC2ScreenColour                ;und ins Register für die Hintergrundfarbe
            cpx #$42
            bne nextColor                       ;solange die Farbe positiv ist -> @loop
            +SetBackgroundColor 6
            lda #<myIRQ                         ;Original IRQ-Vektor setzen
            sta IRQServiceRoutineLo
            lda #>myIRQ
            sta IRQServiceRoutineHi

            lda #RASTER
            sta VIC2Raster

            lda #%00000001                      ;IRQ bestätigen
            sta VIC2InteruptStatus
irq_jump_target
            jsr sprite_line_move
            jmp $ea81                           ;zum Ende des 'Timer-Interrupts' springen

; *************************************************
; ** Vorbereitung auf neue IRQ Routinen          **
; *************************************************
set_new_irq lda #$15                            ; Kleinbuchstaben
            sta VIC2MemorySetup

            jsr screen_init                     ; Bildschirm vorbereiten
            jsr sprite_text_init                ; Sprites vorbereiten
            jsr sprite_text_set                 ; Sprites einschalten
        ; register first interrupt
            sei
            lda #$7f
            sta CIA1InterruptControl                 ; turn off the CIA interrupts
            sta CIA1InterruptControl
            and VIC2ScreenControlV                 ; clear high bit of raster line
            sta VIC2ScreenControlV
            +irqEnd $71, irq1
            lda #$01                            ; enable raster interrupts
            sta VIC2InteruptControl
            cli
            jmp *

; ************************************************
; ** IRQ Routinen                               **
; ************************************************
irq1        ldx #$05
            dex
            bne *-1
raster3a    lda #$0e
            sta VIC2BorderColour
raster1a    lda #$01
            sta VIC2ScreenColour
            ldx #$08
            dex
            bne *-1
            lda #$00
            sta VIC2ScreenColour
            +irqEnd $B0, irq2
            inc VIC2InteruptStatus     ; acknowledge interrupt
            jmp $ea31

irq2        lda #$B2
            cmp $d012
            bne *-3
            ldx #$09
            dex
            bne *-1
raster3b    lda #$0e
            sta VIC2BorderColour
raster1b    lda #$01
            sta VIC2ScreenColour
            ldx #$04
            dex
            bne *-1
raster2     lda #$06
            sta VIC2ScreenColour
sprite_change_2        
            jsr sprite_text_move ; Einblenden
            +irqEnd $71, irq1
            inc VIC2InteruptStatus      ; acknowledge interrupt
            jmp $ea31

!src "raster.asm" ; Import der Rasterroutinen und Tabellen

