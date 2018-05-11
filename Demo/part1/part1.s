
#import "../stdlib/functions.inc" // Stellt ein paar Macros bereit
#import "../stdlib/stdlib_k.a" // Sprungziele
#import "../stdlib/macros.inc"

.import source "../loader/loader.sym"  // Labels des Loaders

                        // constants 
.var C_SCREEN_RAM            = $0400
.var C_UNPACK_ROUTINE        = loader.unpack_next // $0810
.var C_UNPACK_DEST           = loader.unpack_literal // $0824
.var C_UNPACK_SOURCE         = loader.unpack_getbyte // $0834
//.var C_APPLY_INTERRUPT       = loader.apply_interrupt // $0840
.var C_APPLY_INTERRUPT       = APPLY_INTERRUPT // $0840
.var C_EXIT_PART             = loader.exit_intro_part // $084C
.var C_COLOUR_RAM            = $d800

.var RELEASE = false

.if (RELEASE==false) { BasicUpstart2(sync_intro) } else { *=$0A00 }

APPLY_INTERRUPT:
.if (RELEASE==false) {
      sta $D012
      stx $0314
      sty $0315
      jmp $ea81
}

sync_intro:
      lda #$15            // Kleinbuchstaben
      sta VIC2MemorySetup
      lda #$00
      jsr $1000           // Sound Init
      jsr gfx             // Grafik vorbereiten
      // jsr init_colors_pre

.if (RELEASE==false) {
      sei
      lda #$7f
      sta $dc0d     // turn off the CIA interrupts
      sta $dd0d
      and $d011     // clear high bit of raster line
      sta $d011
      :irqEnd #$2F:#irq1_pre

      lda #$01      // enable raster interrupts
      sta $d01a
      cli
      jmp *
}

      lda #$2F
      ldx #<irq1_pre
      ldy #>irq1_pre
      .if (RELEASE==true){ jmp C_APPLY_INTERRUPT } else { jmp APPLY_INTERRUPT }


// ************************************************
// ** IRQ Routinen                               **
// ************************************************

irq1_pre:
      inc VIC2InteruptStatus     // acknowledge interrupt
      lda #$1b
      sta VIC2ScreenControlV
      lda #%10000000
      sta VIC2MemorySetup    // $D018 Speicherbereiche
      lda #%00011000
      sta VIC2ScreenControlH // $D016
      lda #%00000010
      sta $dd00

      lda #$31
      cmp $d012
      bne *-3
      ldx #$0A
      dex
      bne *-1
c10:  lda #$00
      sta VIC2BorderColour
c11:  lda #$00
      sta VIC2ScreenColour
      ldx #$09            // delay
      dex
      bne *-1
c20:  lda #$00
      sta VIC2ScreenColour
c21:  lda #$00
      sta VIC2BorderColour
      lda #$A0
      ldx #<irq2_pre
      ldy #>irq2_pre
      .if (RELEASE==true){ jmp C_APPLY_INTERRUPT } else { jmp APPLY_INTERRUPT }

irq2_pre:
      inc VIC2InteruptStatus     // acknowledge interrupt
      lda #$a2
      cmp $d012
      bne *-3
      inc $d020
subroutine:
      jsr pre_warte
      dec $d020

      lda #$F0
      ldx #<irq1_pre
      ldy #>irq1_pre
      .if (RELEASE==true){ jmp C_APPLY_INTERRUPT } else { jmp APPLY_INTERRUPT }

irq3_pre:
      inc VIC2InteruptStatus     // acknowledge interrupt
      lda #$F9
      cmp $d012
      bne *-3
      ldx #$0A            // delay
      dex
      bne *-1
c30:  lda #$00
      sta VIC2BorderColour
c31:  lda #$00
      sta VIC2ScreenColour
      ldx #$09            // delay
      dex
      bne *-1
      nop
      nop
c40:  lda #$00
      sta VIC2ScreenColour
c41:  lda #$00
      sta VIC2BorderColour
      lda #$2F
      ldx #<irq1_pre
      ldy #>irq1_pre
      .if (RELEASE==true){ jmp C_APPLY_INTERRUPT } else { jmp APPLY_INTERRUPT }

set_second_irq:
      ldx #<sub_rts
      ldy #>sub_rts
      stx subroutine+1
      sty subroutine+2
      rts

      lda #$2F
      ldx #<irq1
      ldy #>irq1
      .if (RELEASE==true){ jmp C_APPLY_INTERRUPT } else { jmp APPLY_INTERRUPT }
sub_rts: rts

Original_c10: .byte $01
Original_c11: .byte $01
Original_c20: .byte $00
Original_c21: .byte $00
Original_c30: .byte $01
Original_c31: .byte $01
Original_c40: .byte $06
Original_c41: .byte $0e

init_colors_pre:
      lda #$01
      sta c10+1
      sta c11+1
      sta c30+1
      sta c31+1
      lda #$00
      sta c20+1
      sta c21+1
      lda #$06
      sta c40+1
      lda #$0e
      sta c41+1
      rts
// -----------------------------------
pre_warte:
      ldx delay
      inx
pre_warte_delay:
      cpx #$40
      bne pre_warte_update
      //setzen des faders
      ldx #<screen_fade_in
      ldy #>screen_fade_in
      stx subroutine+1
      sty subroutine+2
      ldx #$00
pre_warte_update:
      stx delay
      rts

screen_fade_in:
      ldx pre_fade_delay
      inx
      cpx #$04
      bne screen_fade_in1
      jsr fade_in
      ldx #$00
screen_fade_in1:
      stx pre_fade_delay
      rts

// **** fade in ** **
fade_in:
      ldx #$00
      stx delay
      ldx fade_count
      inx
      cpx #$10
      bne !+
      lda #$2F
      sta $d012
      ldx #<irq1
      stx $0314
      ldy #>irq1
      sty $0315
      jmp $EA81
      ldx #$00
!:    stx fade_count

      lda Original_c10 //hole zielfarbe
      asl
      asl
      asl
      asl
      sta fade_in_offset //setze offset
      lda c10+1 //hole aktuelle farbe
      and #$0f //und isoliere low nibble (muss gemacht werden sonst kommt mist raus)
      ora fade_in_offset // "addiere" offset
      tax //akku nach X
      lda fade_table,x //hole fade value
      sta c10+1

      lda Original_c11
      asl
      asl
      asl
      asl
      sta fade_in_offset
      lda c11+1
      and #$0f
      ora fade_in_offset
      tax
      lda fade_table,x
      sta c11+1

      lda Original_c20
      asl
      asl
      asl
      asl
      sta fade_in_offset
      lda c20+1
      and #$0f
      ora fade_in_offset
      tax
      lda fade_table,x
      sta c20+1

      lda Original_c21
      asl
      asl
      asl
      asl
      sta fade_in_offset
      lda c21+1
      and #$0f
      ora fade_in_offset
      tax
      lda fade_table,x
      sta c21+1

      lda Original_c30
      asl
      asl
      asl
      asl
      sta fade_in_offset
      lda c30+1
      and #$0f
      ora fade_in_offset
      tax
      lda fade_table,x
      sta c30+1

      lda Original_c31
      asl
      asl
      asl
      asl
      sta fade_in_offset
      lda c31+1
      and #$0f
      ora fade_in_offset
      tax
      lda fade_table,x
      sta c31+1

      lda Original_c40
      asl
      asl
      asl
      asl
      sta fade_in_offset
      lda c40+1
      and #$0f
      ora fade_in_offset
      tax
      lda fade_table,x
      sta c40+1

      lda Original_c41
      asl
      asl
      asl
      asl
      sta fade_in_offset
      lda c41+1
      and #$0f
      ora fade_in_offset
      tax
      lda fade_table,x
      sta c41+1
      rts

fade_table:
    //high nibble=target color
    //low nibble =source color
    //      0   1   2   3   4   5   6   7   8   9   a   b   c   d   e   f
    .byte $00,$0d,$09,$0c,$02,$08,$02,$0f,$02,$00,$08,$09,$04,$03,$04,$05    //0
    .byte $06,$01,$08,$0d,$0c,$03,$0b,$01,$0a,$02,$0f,$04,$03,$01,$03,$07    //1
    .byte $09,$07,$02,$0c,$02,$04,$0b,$0f,$02,$02,$08,$02,$04,$03,$04,$0a    //2
    .byte $06,$0d,$04,$03,$0c,$03,$0b,$0f,$0c,$02,$0c,$04,$03,$03,$03,$03    //3
    .byte $06,$0d,$04,$0c,$04,$0c,$0b,$0f,$04,$02,$04,$04,$04,$03,$04,$0a    //4
    .byte $06,$0d,$08,$05,$0c,$05,$0b,$0f,$0c,$02,$0c,$04,$05,$03,$05,$05    //5
    .byte $06,$0d,$0b,$0e,$0b,$08,$06,$0f,$0b,$0b,$08,$06,$04,$03,$04,$0c    //6
    .byte $09,$07,$08,$0f,$0a,$0f,$0b,$07,$0a,$02,$0f,$04,$0f,$07,$0f,$07    //7
    .byte $09,$07,$08,$0c,$08,$0c,$0b,$0f,$08,$02,$08,$08,$08,$03,$04,$0c    //8
    .byte $09,$07,$09,$0c,$02,$08,$0b,$0f,$02,$09,$08,$09,$08,$03,$04,$0c    //9
    .byte $09,$07,$04,$0c,$0a,$0c,$0b,$0f,$0a,$02,$0a,$04,$0a,$0f,$0c,$0a    //a
    .byte $06,$0d,$0b,$0e,$0b,$08,$0b,$0f,$0b,$0b,$08,$0b,$04,$03,$04,$0c    //b
    .byte $06,$0d,$08,$0c,$0c,$0c,$0b,$0f,$0c,$02,$0c,$04,$0c,$03,$0c,$0c    //c
    .byte $09,$0d,$08,$0d,$0c,$03,$0b,$0d,$0c,$02,$0f,$04,$0f,$0d,$03,$0d    //d
    .byte $06,$0d,$04,$0e,$0c,$0e,$0b,$0f,$0c,$02,$0c,$04,$0e,$03,$0e,$0e    //e
    .byte $06,$0d,$08,$0f,$0c,$0f,$0b,$0f,$0c,$02,$0f,$04,$0f,$0f,$0f,$0f    //f
fade_in_offset:
    .byte 0

delay:  .byte $00
pre_fade_delay: .byte $00
fade_count: .byte $00

// *********************************************************** //
irq1:
      inc VIC2InteruptStatus     // acknowledge interrupt
      lda #%00111011
      sta VIC2ScreenControlV
      lda #%10000000
      sta VIC2MemorySetup    // $D018 Speicherbereiche
      lda #%00011000
      sta VIC2ScreenControlH // $D016
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
      ldx #$09            // delay
      dex
      bne *-1
      lda #$00
      sta VIC2ScreenColour
      lda #$00
      sta VIC2BorderColour
      lda #$6F
      ldx #<irq1b
      ldy #>irq1b
      .if (RELEASE==true){ jmp C_APPLY_INTERRUPT } else { jmp APPLY_INTERRUPT }

// Ende Grafikmodus
irq1b:
      inc VIC2InteruptStatus     // acknowledge interrupt
      lda #$71
      cmp $d012
      bne *-3
      ldx #$16
      dex
      bne *-1
      lda #%00000011
      sta $dd00
      lda #$1b
      sta VIC2ScreenControlV // Grafikmodus
      lda #$15
      sta VIC2MemorySetup    // $D018 Speicherbereiche
      lda #%00001000
      sta VIC2ScreenControlH // $D016
      lda #$00
      sta VIC2ScreenColour
      lda #$7f               // detect space bar 
      sta CIA1KeyboardColumnJoystickA
      lda CIA1KeyboardRowsJoystickB
      and #$10
      bne update_irq
      jmp C_EXIT_PART

update_irq:
      lda #$A0
      ldx #<irq2
      ldy #>irq2
      .if (RELEASE==true){ jmp C_APPLY_INTERRUPT } else { jmp APPLY_INTERRUPT }

irq2:
      inc VIC2InteruptStatus     // acknowledge interrupt
      lda #$a2
      cmp $d012
      bne *-3
      inc $d020
      jsr $1003                   // Sound Play
      dec $d020
      lda #$F0
      ldx #<irq3
      ldy #>irq3
      .if (RELEASE==true){ jmp C_APPLY_INTERRUPT } else { jmp APPLY_INTERRUPT }

irq3:
      inc VIC2InteruptStatus     // acknowledge interrupt
      lda #$F9
      cmp $d012
      bne *-3
      ldx #$0A            // delay
      dex
      bne *-1
      lda #$01
      sta VIC2BorderColour
      lda #$01
      sta VIC2ScreenColour
      ldx #$09            // delay
      dex
      bne *-1
      nop
      nop
      lda #$06
      sta VIC2ScreenColour
      lda #$0e
      sta VIC2BorderColour
      lda #$2F
      ldx #<irq1
      ldy #>irq1
      .if (RELEASE==true){ jmp C_APPLY_INTERRUPT } else { jmp APPLY_INTERRUPT }

// Grafik vorbereiten
gfx:  ldx #$00
!:    lda colora,x
      sta $6000,x
      lda colora+$40,x
      sta $6000+$40,x
      lda colorb,x
      sta $d800,x
      lda colorb+$40,x
      sta $d800+$40,x
      inx
      bne !-
      rts

      *=$1000
      .import binary "ode_to_64.bin"

      *=$4000
      .import source "gfx_k.txt"
