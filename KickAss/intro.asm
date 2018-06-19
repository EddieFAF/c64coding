
// Assembler: Kick Assembler
BasicUpstart2(start)

#import "functions.inc"
#import "macros.inc"

        .var Zeilenstart = $6A // erste Rasterzeile von Bildschirmzeile 08
        .var Zeilenende  = $C1 // Letzte Rasterzeile von Bildschirmzeile 18
        .var C_D18       = $FB
        .var C_D16       = $FC
        .const C_CHAR    = $3800
        .const C_DELAY   = $04

// ----------------------------------------
start:  sei         // set up interrupt
        jsr $e544
        lda $d018
        sta C_D18
        lda $d016
        sta C_D16
        lda #$00
        sta $d020
        sta $d021
        lda #$7f
        sta $dc0d     // turn off the CIA interrupts
        sta $dd0d
        and $d011     // clear high bit of raster line
        sta $d011

        //Grafik kopieren
        ldx #$00
!l:     lda grafik,x
        sta $2000,x
        lda grafik+$100,x
        sta $2100,x
        lda grafik+$200,x
        sta $2200,x
        lda grafik+$300,x
        sta $2300,x
        lda grafik+$400,x
        sta $2400,x
        lda grafik+$500,x
        sta $2500,x
        lda grafik+$600,x
        sta $2600,x
        lda grafik+$700,x
        sta $2700,x
        lda grafik+$800,x
        sta $2800,x
        lda colora,x
        sta $0400,x
        lda colora+$018,x
        sta $0418,x
        lda colorb,x
        sta $d800,x
        lda colorb+$018,x
        sta $d818,x
        inx
        bne !l-

// Fill Screen
        ldx #$00
!l:     lda #$00
        sta $0400+7*40,x // Zeile 8
        lda #$01
        sta $0400+17*40,x // Zeile 18
        inx
        cpx #$28
        bne !l-

        // Screen neu füllen
        jsr copy

        :irqEnd #$00:#irq1

        ldx #$08
        stx coldelay
        lda #$01      // enable raster interrupts
        sta $d01a
        cli
        rts         // back to BASIC

// =====================================================
// = Start Intro
// =====================================================

// BEGIN IRQ Verlauf

// Zeichensatz ausschalten
irq1:   lda C_D18 //#$14 //font
        sta $d018
        lda #$1b
        sta $d011
        lda C_D16 //#$08
        sta $d016

        :irqEnd #$32:#irq2

        inc $d019     // acknowledge interrupt
        jmp $ea31

// Zeile 48 (Logo)
irq2:   lda #Zeilenstart-2
        sta $d012
irq2l:  ldx #<irq3
irq2h:  ldy #>irq3
        stx $0314
        sty $0315
        lda #$00
        sta $d020
        sta $d021

        lda #$3b //#$3b
        sta $d011
        // Zeichensatz einschalten
        lda #$19
        sta $d018
        lda #$18
        sta $d016
        inc $d019     // acknowledge interrupt
        jmp $ea31

// ----------------------------------------
// - Rasterlinie oben
// ----------------------------------------
irq3:   lda #Zeilenstart // Startzeile
        cmp $d012
        bne *-3
        ldx #$0A // Delay
        dex
        bne *-1
        lda #$08
        sta $d016
        lda #$1b
        sta $d011
        lda #$1E // Zeichensatz bei $3800 einschalten
        sta $d018
        lda #Zeilenstart+2
        cmp $d012
        bne *-3
        ldx #$09
        dex
        bne *-1
        ldy colcount
        lda coltab,y   // Zeilenanfang
        sta $d020
        sta $d021
        ldx #$0A
        dex
        bne *-1
        lda #$00
        sta $d020
        sta $d021

        :irqEnd #Zeilenende-3:#irq4
        inc $d019     // acknowledge interrupt
        jmp $ea31

// ----------------------------------------
// - Rasterlinie unten
// ----------------------------------------
irq4:   lda #Zeilenende // Endzeile
        cmp $d012
        bne *-3
        ldx #$0A // Delay
        dex
        bne *-1
        ldy colcount
        lda coltab,y
        sta $d020
        sta $d021
        ldx #$0A
        dex
        bne *-1
        lda #$00
        sta $d020
        sta $d021
        // Zeichensatz ausschalten
        lda C_D18
        sta $d018
        lda C_D16
        sta $d016

sub:    jsr col_update

        :irqEnd #$00:#irq1 // Letzer IRQ, also wieder auf Anfang setzen
        inc $d019     // acknowledge interrupt
        jmp $ea31

// ----- @Nach Farbverlauf@ -----
// ----------------------------------------
// - Rasterlinie oben
// ----------------------------------------
irqn3:  lda #Zeilenstart // Startzeile
        cmp $d012
        bne *-3
        ldx #$0A // Delay
        dex
        bne *-1
        lda #$08
        sta $d016
        lda #$1b
        sta $d011
        lda #$14 // Zeichensatz ausschalten
        sta $d018
        lda #Zeilenstart+2
        cmp $d012
        bne *-3
        ldx #$09
        dex
        bne *-1
        ldy colcount
        lda coltab,y   // Zeilenanfang
        sta $d020
        sta $d021
        ldx #$0A
        dex
        bne *-1
        lda #$00
        sta $d020
        sta $d021

        :irqEnd #Zeilenende-3:#irq4
        inc $d019     // acknowledge interrupt
        jmp $ea31


// ----------------------------------------
// - Farbverlauf
// ----------------------------------------
col_update: dec coldelay
        beq weiter
ret:    rts

weiter: ldx #C_DELAY
        stx coldelay
        ldx colcount
        inx
        stx colcount
        cpx #$10
        bne copy
        lda #<ret
        ldy #>ret
        sta sub+1
        sty sub+2
        lda #<irqn3
        ldy #>irqn3
        sta irq2l+1
        sty irq2h+1
// Fill Screen with text
        ldx #$00
!l:     lda screen_text,x
        sta $0400+7*40,x
        lda screen_text+$B8,x
        sta $04B8+7*40,x
        inx
        bne !l-

        // Screen neu füllen
copy:   ldx colcount
        ldy #$00
!l:     lda coltab+2,x
        sta $d800+7*40,y
        sta $d800+7*40+36,y
        sta $d800+17*40,y
        sta $d800+17*40+36,y
        lda coltab+3,x
        sta $d800+7*40+4,y
        sta $d800+7*40+32,y
        sta $d800+17*40+4,y
        sta $d800+17*40+32,y
        lda coltab+4,x
        sta $d800+7*40+8,y
        sta $d800+7*40+28,y
        sta $d800+17*40+8,y
        sta $d800+17*40+28,y
        lda coltab+5,x
        sta $d800+7*40+12,y
        sta $d800+7*40+24,y
        sta $d800+17*40+12,y
        sta $d800+17*40+24,y
        lda coltab+6,x
        sta $d800+7*40+16,y
        sta $d800+7*40+20,y
        sta $d800+17*40+16,y
        sta $d800+17*40+20,y
        iny
        cpy #$04
        bne !l-
        rts


coldelay: .byte C_DELAY
colcount: .byte 00

coltab: .byte 000, 000, 000, 000, 000, 000, 000, 000
        .byte 000, 000, 006, 004, 014, 003, 001, 001
        .byte 001, 001, 001, 001, 001, 001, 001, 001
        .byte 001, 001, 001, 001, 001, 001, 001, 001

screen_text: .text "                                        "
             .text "1234567890123456789012345678901234567890"
             .text "  Here goes the text in the middle of   "
             .text "     the screen. no more coding...      "
             .text "           w e l c o m e                "
             .text "           w e l c o m e                "
             .text "           w e l c o m e                "
             .text "           w e l c o m e                "
             .text "     the screen. no more coding...      "
             .text "  Here goes the text in the middle of   "
             .text " * * * * * * * * * * * * * * * * * * * *"
grafik: 
#import "..\..\Resources\mp_eddie.txt"

// ----- @Charset@ -----
*=C_CHAR "Charset"
intro_char: .byte %00000000
            .byte %00000000
            .byte %11111111
            .byte %00000000
            .byte %00000000
            .byte %00000000
            .byte %00000000
            .byte %00000000

            .byte %00000000
            .byte %00000000
            .byte %00000000
            .byte %00000000
            .byte %00000000
            .byte %00000000
            .byte %00000000
            .byte %11111111
