;*********************************************************************72
;**  Project Name: Example Project                                    **
;**  ---------------------------------------------------------------  **
;**  Filename: Example_File.asm                                       **
;**  ---------------------------------------------------------------  **
;**  Author (c): xxxxxxxxxxx                                          **
;**  File Date: 2016-08-20                                            **
;***********************************************************************
;Meine persönliche Reihenfolge ist anders.
; -Macros,
; -Includes von Daten mit fester Adresse (Musik, Grafik...)
;-Basic-Zeile und ein erstes bisschen Code wie Kopierschleifen o.Ä. dazu.
;-Dann für größere Funktionsblöcke, z.B. Diskroutinen, Zeichenroutinen, Raster-IRQs...:
; [ -Variablen
; -Unterprogramme: Deren lokale Variablen, dann der Code dazu
; -Der eigentliche Code]
; Und zu guter Letzt die Hauptroutine, die all die anderen Sachen verwendet.

; Soundcheck

sound = $8E00
soundinit = $8E00
soundplay = $8E03

!src "../stdlib/macros.asm" ; Stellt ein paar Macros bereit

        *=$0810

start
        sei
        +SetBorderColor 0
        +SetBackgroundColor 0
        ;lda #$00
        ;jsr $e544
        +SetVICBank0
        +SetScreenAndCharLocation $0400, $2000
        
        lda #$7f
        sta $dc0d ; turn off the CIA interrupts
        sta $dd0d
        and $d011 ; clear high bit of raster line
        sta $d011
        +irqEnd $52, irq1
        lda #$01 ; enable raster interrupts
        sta $d01a
        lda #$00
        jsr soundinit
        cli
        jmp *
        rts

; ----- @IRQ Start@ -----
irq1
        inc $d020
        jsr soundplay                                   ;   Music Play
        dec $d020
        +irqEnd $82,irq2
        inc $d019
        jmp $ea31

irq2 ; Zeile 52
        +irqEnd $52,irq1
        inc $d019
        jmp $ea31


; ============================
        *= sound
;!bin "music.bin"
!bin "end.sid",,$7e
