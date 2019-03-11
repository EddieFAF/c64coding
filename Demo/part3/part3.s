//*********************************************************************72
//**  Project Name: Demo Project                                       **
//**  ---------------------------------------------------------------  **
//**  Filename: Part3.asm                                              **
//**  ---------------------------------------------------------------  **
//**  Author (c): [FAF]Eddie                                           **
//**  File Date: 2018-04-11                                            **
//***********************************************************************
// Assembler: KickAss

#import "../../stdlib/stdlib_k.a"
#import "../../stdlib/macros.inc"
#import "../../stdlib/functions.inc"
//.import source "../../loader/loader.sym"

.var VICSCREENBLOCKNO       = 1 //Nr. (0 -15) des 1KB-Blocks für den Textbildschirm      | Standard: 1
.var VICCHARSETBLOCKNO      = 6 // 4=$2000 5=$2800, 6=$3000, 7=$3800
.var C_APPLY_INTERRUPT       = $0840 //loader.apply_interrupt // $0840
.var C_EXIT_PART             = $084c //loader.exit_intro_part // $084C

.var RELEASE                = 1

.var MC_color2              = $0c
.var MC_color1              = $0d
.var BG_color               = $0b


.if (RELEASE==1) { .eval C_EXIT_PART = $c1ab } else { .eval C_EXIT_PART = $fce2 }
.if (RELEASE==1) { .eval C_APPLY_INTERRUPT = $c19f } else { .eval C_APPLY_INTERRUPT = APPLY_INTERRUPT }

.if (RELEASE==0) { BasicUpstart2(sync_intro) } else { *=$0A00 }

APPLY_INTERRUPT:
.if (RELEASE==0) {
        sta $D012
        stx $0314
        sty $0315
        jmp $ea81
}

sync_intro:
        lda #$00
        sta $0286
        jsr $e544
        jsr screen_init
        jsr char_init
        lda #$00
        sta VIC2BorderColour
        sta VIC2ScreenColour
.if (RELEASE==0) {
        sei
        lda #$7f
        sta $dc0d     // turn off the CIA interrupts
        sta $dd0d
        and $d011     // clear high bit of raster line
        sta $d011
        :irqEnd #$30:#irq1

        lda #$01      // enable raster interrupts
        sta $d01a
        cli
        jmp *
}

        lda #$00
        jsr $1000
        lda #$30
        ldx #<irq1
        ldy #>irq1
        jmp C_APPLY_INTERRUPT

// =====================================================
// = Start Intro
// =====================================================

irq1:
        ldx #$04
        dex
        bne *-1
        lda #$0C
        sta VIC2BorderColour
        sta VIC2ScreenColour
        lda #$3b
        sta $d011
subroutine:
        jsr screen_fade //warte
        inc $d019
        lda #$91
        ldx #<irq2
        ldy #>irq2
        jmp C_APPLY_INTERRUPT

irq2:
        ldx #$04
        dex
        bne *-1
        lda #$00
        sta VIC2BorderColour
        sta VIC2ScreenColour
        lda #$1b
        sta $d011
        inc $d019
        lda #$A2
        ldx #<irq3
        ldy #>irq3
        jmp C_APPLY_INTERRUPT

irq3:
        ldx #$04
        dex
        bne *-1
        lda #$0C
        sta VIC2BorderColour
        sta VIC2ScreenColour
        lda #$10+6*2
        sta $d018
        inc $d019

        inc VIC2BorderColour
        jsr $1003
        dec VIC2BorderColour
        lda #$30
        ldx #<irq1
        ldy #>irq1
        jmp C_APPLY_INTERRUPT

// ************************************************************************** //
// ** Subroutinen
// ************************************************************************** //

warte:
        ldx delay
        inx
warte_delay:
        cpx #$40
        bne warte_update
        //setzen des faders
        ldx #<screen_fade
        ldy #>screen_fade
        stx subroutine+1
        sty subroutine+2
        ldx #$00
warte_update:
        stx delay
        rts

screen_fade:
        ldx fade_counter
        inx
        cpx #$02
        bne screen_fade1
        jsr fade_update
        ldx #$00
screen_fade1:
        stx fade_counter
        rts

fade_update:
        ldx #$00
!:      lda fade_table,x
        sta $d9e0,x
        inx
        sta $da08,x
        cpx #$28
        bne !-
        jsr update_fade_table
        rts

update_fade_table:
        lda richtung
        beq update_fade_table2

        // Fadein
        ldy fade_counter2
        ldx #$00
!:      lda tabmaster,y
        sta fade_table,x
        iny
        inx
        cpx #$28
        bne !-
        ldy fade_counter2
        iny
        cpy #$50
        bne !+
        //setze auf fade_out
        lda #$00
        sta richtung
        lda #$40
        sta warte_delay+1
        //jsr set_delay
        ldx #<warte
        ldy #>warte
        stx subroutine+1
        sty subroutine+2
        ldy #$78
!:      sty fade_counter2
        rts

update_fade_table2: //ausblenden
        ldy fade_counter2
        ldx #$00
!:      lda tabmaster,y
        sta fade_table,x
        dey
        inx
        cpx #$28
        bne !-
        ldy fade_counter2
        dey
        cpy #$28
        bne !+
        //setze auf fade_in
        lda #$01
        sta richtung
        lda #$04
        sta warte_delay+1
        jsr screen_update
        //jsr set_delay
        ldx #<warte
        ldy #>warte
        stx subroutine+1
        sty subroutine+2
        ldy #$00
!:      sty fade_counter2
        rts

screen_update:
        ldx counter
        inx
        stx counter
        lda tcnt+1
        clc
        adc #$28
        sta tcnt+1
        cmp #$A0
        bne weiter
        lda #$00
        sta counter
        sta tcnt+1
weiter:
        jsr screen_copy
        rts

screen_copy:
        ldx #$00
tcnt:   lda text,x
        sta $05e0,x
        clc
        adc #$40
        sta $0608,x
        inx
        cpx #$28
        bne tcnt
        rts

screen_init:
        ldx #$00
!:      lda colora+2,x
        sta $0400,x
        lda colora+2+$e0,x
        sta $04e0,x
        inx
        bne !-

        ldx #$00
!:      lda text,x
        sta $05e0,x
        clc
        adc #$40
        sta $0608,x
        lda #$0B
        sta $d9e0,x
        sta $da08,x
        inx
        cpx #$28
        bne !-
        rts

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

//check_space:
// **********************
// ** Warten auf Space
// **********************
//         jsr $ffe4
//         cmp #$20
//         bne !+
//         jmp ausgang
// !:      rts

counter:
        .byte $00
delay:  .byte $00
richtung: .byte $01 // $00=out, $01=in
fade_counter:
        .byte $00
fade_counter2:
        .byte $00
fade_table:
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00

tabmaster:
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00

        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $0b, $0b, $0c, $0c, $0d, $0d, $0f, $0f
        .byte $01, $01, $01, $01, $01, $01, $01, $01
        .byte $01, $01, $01, $01, $01, $01, $01, $01

        .byte $01, $01, $01, $01, $01, $01, $01, $01
        .byte $01, $01, $01, $01, $01, $01, $01, $01
        .byte $01, $01, $01, $01, $01, $01, $01, $01
        .byte $01, $01, $01, $01, $01, $01, $01, $01
        .byte $01, $01, $01, $01, $01, $01, $01, $01

        .align 256
text:
        .text "          welcome to xs dnite!          "
        .text "     a new production done by eddie     "
        .text "   featuring the first crossdev parts   "
        .text "    done with kickassembler and acme    "
        .text "1234567890123456789012345678901234567890"




        *=$1000
.import binary "Nightshift.sid",$7e

        *=$2000
        #import "gfx_k.inc"

char_data: // Lade direkt und verschiebe später
        .import c64 "devils_collection_26_y.64c"
