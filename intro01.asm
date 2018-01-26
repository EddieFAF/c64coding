
// Assembler: Kick Assembler
BasicUpstart2(start)

#import "macros.inc"
#import "variables.inc"
#import "functions.inc"

.var Zeilenstart = $6A // 106
.var Zeilenende  = $C1 // 193
.var music = LoadSid("..\Nightshift.sid")    //<- Here we load the sid file

start:
        sei         // set up interrupt
        :SetBorderColor(0)
        :SetBackgroundColor(0)
        lda #$7f
        sta $dc0d     // turn off the CIA interrupts
        sta $dd0d
        and $d011     // clear high bit of raster line
        sta $d011

        :irqEnd #$00:#irq1

        :ClearScreen($0400,01)
// Grafik vorbereiten
    lda colora
    sta $d020
    lda colora+1
    sta $d021

    ldx #$00
loaddccimage:
    lda colora+2,x
    sta $0400,x
    lda colora+$102,x
    sta $0500,x

    lda colorb,x
    sta $d800,x
    lda colorb+$100,x
    sta $d900,x
    inx
    bne loaddccimage
// Grafik bereit

        lda #$01      // enable raster interrupts
        sta $d01a
		lda #$00
		jsr music.init
        cli
        rts         // back to BASIC


// BEGIN IRQ Verlauf //
irq1:
        // Zeichensatz ausschalten
        lda #$14 //font
        sta $d018
        lda #$1b
        sta $d011
        lda #$08
        sta $d016
		inc $d020
		jsr music.play
		dec $d020

        :irqEnd #48:#irq2
        inc $d019     // acknowledge interrupt
        jmp $ea31

irq2: // Zeile 52
        :irqEnd #Zeilenstart-8:#irq3
        :SetBorderColor(11)
        :SetBackgroundColor(11)

        lda #$3b
        sta $d011
        // Zeichensatz einschalten
        lda #$18 // Zeichensatz bei $3000 einschalten
        sta $d018
        :SetMultiColorMode()
        inc $d019     // acknowledge interrupt
        jmp $ea31

irq3:

        lda #Zeilenstart // Startzeile
        cmp $d012
        bne *-3
        ldx #$0a // Delay
        dex
        bne *-1
		:SetBackgroundColor(15)
		:SetBorderColor(15)
		ldx #$03
		dex
		bne *-1
		:SetBorderColor(0)
        :SetBackgroundColor(0)

        lda #$1b
        sta $d011

        lda #Zeilenende // Endzeile
        cmp $d012
        bne *-3
        ldx #$0a // Delay
        dex
        bne *-1
        :SetBackgroundColor(15)
        :SetBorderColor(15)
        ldx #$09
        dex
        bne *-1
        nop
        nop
        nop
        nop
        :SetBorderColor(11)
        :SetBackgroundColor(11)
        // Zeichensatz ausschalten
        lda #$14
        sta $d018

        :irqEnd #00:#irq1
        inc $d019     // acknowledge interrupt
        jmp $ea31

    *=music.location "Music"
music_data:      .fill music.size, music.getData(i)

      *=$2000 "Grafik"
#import "../eddie_logo.txt"
