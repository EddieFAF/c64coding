
// Assembler: Kick Assembler
BasicUpstart2(start)

#import "../../../demo/lib/kickass/macros.inc"
#import "../../../demo/lib/kickass/variables.inc"
#import "../../../demo/lib/kickass/functions.inc"

.var VICSCREENBLOCKNO       = 1 //Nr. (0 -15) des 1KB-Blocks für den Textbildschirm      | Standard: 1
.var VICCHARSETBLOCKNO      = 6 // 4=$2000 5=$2800, 6=$3000, 7=$3800
.var RELEASE                = true
.var MC_color2              = $0c
.var MC_color1              = $0d
.var BG_color               = $0b
.var Flashline              = $05E0
.var FlashCol               = $D9E0
.var StartZeileOben         = $0478+4
.var StartZeileUnten        = $06A8+4
.var Char1                  = 4 // 4
.var Char2                  = 5 // 5

.var music = LoadSid("../../../demo/res/music/Nightshift.sid")    //<- Here we load the sid file
        //:equalCharPack("loading320x200.png", $3800, $2800)

start:
        sei         // set up interrupt
        :SetBorderColor(0)
        :SetBackgroundColor(0)
        lda #$7f
        sta $dc0d     // turn off the CIA interrupts
        sta $dd0d
        and $d011     // clear high bit of raster line
        sta $d011

        :irqEnd #$30:#irq1

        jsr gfx_init
        jsr screen_init
        jsr char_init
        lda #$01      // enable raster interrupts
        sta $d01a
        lda #$00
        jsr music.init
        cli
        jmp *
        rts         // back to BASIC

// =====================================================
// = Start Intro
// =====================================================

// BEGIN IRQ Verlauf //
irq1:
        // Zeichensatz ausschalten
        lda #16+Char1*2 // 4 font at $2000
        sta $d018
        :SetMultiColorMode()
        lda #$1b
        sta $d011
mc1:    lda #$00    // MC#1
        sta $d022
mc2:    lda #$00    // MC#2
        sta $d023
bg:     lda #$00    // BG
        sta $d021

subroutine2:
        jsr doublefade_low

        :irqEnd #$7F:#irq2 // oben
        inc $d019     // acknowledge interrupt
        jmp $ea81

irq2:
        lda #$89
        cmp $d012
        bne *-3
        ldx #$0A
        dex
        bne *-1
        :SetBorderColor(6)
        :SetBackgroundColor(6)

        // Zeichensatz einschalten
        lda #VICSCREENBLOCKNO*16+VICCHARSETBLOCKNO*2 // Zeichensatz bei $2000 einschalten
        sta $d018
        lda $d016
        and #%11101111
        sta $d016
        :irqEnd #$A0:#irq3 // unten
        inc $d019     // acknowledge interrupt
        jmp $ea81

irq3:
        lda #$A2 // Startzeile
        cmp $d012
        bne *-3
        ldx #$0a // Delay
        dex
        bne *-1
        :SetBackgroundColor(0)
        :SetBorderColor(0)

        // Zeichensatz ausschalten
        lda #16+Char2*2 //5
        sta $d018
        :SetMultiColorMode()
mc1_low: lda #$00 //MC1
        sta $d022
mc2_low: lda #$00 //MC2
        sta $d023
bg_low: lda #$00 //BG
        sta $d021
subroutine:
        jsr doublefade

        .if(RELEASE==false) :SetBorderColor(6)
        jsr colwash2
        .if(RELEASE==false) :SetBorderColor(0)

        .if(RELEASE==false) :SetBorderColor(5)
        jsr music.play
        .if(RELEASE==false) :SetBorderColor(0)

        :irqEnd #$FE:#irq4
        inc $d019     // acknowledge interrupt
        jmp $ea81

irq4:
        // Zeichensatz ausschalten
        lda #$14
        sta $d018
        :irqEnd #$30:#irq1
        inc $d019
        jmp $ea31

check_space:
// **********************
// ** Warten auf Space
// **********************
//         jsr $ffe4
//         cmp #$20
//         bne !+

// //            lda #$7f       // detect space bar
// //            sta $dc00
// //            lda $dc01
// //            and #$10
//         bne !+
//         jmp set_fadeout
// !:      rts

counter:    .byte 0
counter_low: .byte 0

// delay1:
//         ldx counter
//         inx
//         stx counter
//         cpx #$80
//         beq !+
//         rts
// !:      jmp doublefade

char_init:
        ldx #$00
!:      lda char_data,x
        sta $3000,x
        lda char_data+$0100,x
        sta $3100,x
        lda char_data+$0200,x
        sta $3200,x
        lda char_data+$0300,x
        sta $3300,x
        inx
        bne !-
        rts

screen_init:
        ldx #$00
!:      lda text,x
        sta Flashline,x
        inx
        cpx #$28
        bne !-
        rts

gfx_init:
        ldx #$00
!:      lda #$02
        sta $0400,x
        sta $04B8,x
        lda #$06
        sta $0630,x
        sta $06e8,x
        lda #$08
        sta $d800,x
        sta $d900,x
        sta $da00,x
        sta $dae8,x
        inx
        bne !-

        ldx #$00
!:      lda screen_char1,x
        sta StartZeileOben,x
        lda screen_color1,x
        sta StartZeileOben+$D400,x

        lda screen_char2,x
        sta StartZeileUnten,x
        lda screen_color2,x
        sta StartZeileUnten+$D400,x
        inx
        cpx #240
        bne !-
        rts

colwash2: 
        ldx #$00        // load x-register with #$00
        lda color3+$27  // load the last color from the second color table
cycle2: ldy color3,x    // remember color at currently looked color2 table location
        sta color3,x    // overwrite location with color from accumulator
        sta FlashCol,x     // ... and write it to Color Ram
        tya             // transfer our remembered color back to accumulator 
        inx             // increment x-register to go to next iteraton
        cpx #$27        // have we gone through 39 iterations yet?
        bne cycle2      // if no, repeat
        sta color3+$27  // if yes, store the final color from accu into color2 table
        sta FlashCol+$27   // and write it into Color Ram
        rts             // return from subroutine

fade_out1:
        ldx delay
        inx
        stx delay
        cpx #$04
        bne rt
        jmp fade_out
rt:     rts

fade_in1:
        ldx delay
        inx
        stx delay
        cpx #$04
        beq !+
        rts
!:      jmp fade_in

delay: .byte $00
delay_low: .byte $00

doublefade:
        ldx counter
        inx
        stx counter
        cpx #$40
        beq !+
        rts
!:      jmp set_faderoutine

richtung: .byte 01
richtung_low: .byte 01
jumptarget: .byte <doublefade,>doublefade
jumptarget_low: .byte <doublefade_low,>doublefade_low

set_faderoutine:
        ldx #$00
        stx counter
        stx delay
        lda richtung // 1=vorwärts, 0=rückwärts
        cmp #$01
        beq vorwaerts
rueckwaerts:
        lda #$01
        sta richtung

        lda #<doublefade
        sta jumptarget
        lda #>doublefade
        sta jumptarget+1

        lda #<fade_out1  // Fadeout als aktuelle Aktion setzen
        sta subroutine+1
        lda #>fade_out1
        sta subroutine+2
        rts
vorwaerts:
        lda #$00
        sta richtung
        ldx fade_count2
        inx
        cpx #$04
        bne !+
        ldx #$00
!:      stx fade_count2

        lda #<doublefade
        sta jumptarget
        lda #>doublefade
        sta jumptarget+1

        lda #<fade_in1  // Fadein als aktuelle Aktion setzen
        sta subroutine+1
        lda #>fade_in1
        sta subroutine+2
        rts

// -------------------------------------------------------------------------- //
fade_out1_low:
        ldx delay_low
        inx
        stx delay_low
        cpx #$04
        bne rtl
        jmp fade_out_low
rtl:    rts

fade_in1_low:
        ldx delay_low
        inx
        stx delay_low
        cpx #$04
        beq !+
        rts
!:      jmp fade_in_low

doublefade_low:
        ldx counter_low
        inx
        stx counter_low
        cpx #$70
        beq !+
        rts
!:      jmp set_faderoutine_low

set_faderoutine_low:
        ldx #$00
        stx counter_low
        stx delay_low
        lda richtung_low // 1=vorwärts, 0=rückwärts
        cmp #$01
        beq vorwaerts_low
rueckwaerts_low:
        lda #$01
        sta richtung_low

        lda #<doublefade_low
        sta jumptarget_low
        lda #>doublefade_low
        sta jumptarget_low+1

        lda #<fade_out1_low  // Fadeout als aktuelle Aktion setzen
        sta subroutine2+1
        lda #>fade_out1_low
        sta subroutine2+2
        rts
vorwaerts_low:
        lda #$00
        sta richtung_low
        ldx fade_count2_low
        inx
        cpx #$04
        bne !+
        ldx #$00
!:      stx fade_count2_low

        lda #<doublefade_low
        sta jumptarget_low
        lda #>doublefade_low
        sta jumptarget_low+1

        lda #<fade_in1_low  // Fadein als aktuelle Aktion setzen
        sta subroutine2+1
        lda #>fade_in1_low
        sta subroutine2+2
        rts

// ---------------------------------------------------------------------------//
fade_count2:        .byte $00
fade_count2_low:    .byte $00
original_BG:        .byte $0b,$0b,$06,$09
original_MC2:       .byte $0d,$0f,$03,$0a
original_MC1:       .byte $0c,$0c,$0e,$08

        #import "include/fade_routine.inc"

text:

        .text "  ...please stand by while loading...   "
 .byte 82,70,64,70,64,68,64,70,64,70,82,70,64,70,82,70,12,15,1,4,9,14,7,33,68,64,70,64,68,64,70,82,70,64,70,64,68,64,68,69

color3:      .byte $0e,$0e,$02,$02,$08,$08,$0a,$0a,$0f,$0f
             .byte $07,$07,$01,$01,$01,$01,$01,$01,$01,$01
             .byte $07,$07,$0f,$0f,$03,$03,$0e,$0e,$06,$06
             .byte $06,$03,$0e,$07,$01,$07,$0e,$03,$06,$06

// Sonstige Daten

//        *=$2000
*=$2000 "Charset1"
        #import "include/charset1.inc"
*=* "Screen1"
        #import "include/screen1.inc"
*=* "Screen2"
        #import "include/screen2.inc"
*=$2800 "Charset2"
        #import "include/charset2.inc"
*=* "Charset1x1"
char_data: // Lade direkt und verschiebe später
        .import c64 "../../../demo/res/fonts/aeg_collection_09.64c"

        *=music.location "Music"
music_data:
        .fill music.size, music.getData(i)
