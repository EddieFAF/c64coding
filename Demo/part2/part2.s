//*********************************************************************72
//**  Project Name: Demo Project                                       **
//**  ---------------------------------------------------------------  **
//**  Filename: Part2.asm                                              **
//**  ---------------------------------------------------------------  **
//**  Author (c): [FAF]Eddie                                           **
//**  File Date: 2018-04-11                                            **
//***********************************************************************
#import "../stdlib/stdlib_k.a"
#import "../stdlib/functions.inc"
.import source "../loader/loader.sym"

.var RELEASE = false

.var music = LoadSid("One_Man_and_His_Droid.sid")
                        // constants 
.var C_SCREEN_RAM            = $0400
.var C_UNPACK_ROUTINE        = loader.unpack_next // $0810
.var C_UNPACK_DEST           = loader.unpack_literal // $0824
.var C_UNPACK_SOURCE         = loader.unpack_getbyte // $0834
.var C_APPLY_INTERRUPT       = loader.apply_interrupt // $0840
.var C_EXIT_PART             = loader.exit_intro_part // $084C
.var C_COLOUR_RAM            = $d800

.if (RELEASE==false) { BasicUpstart2(sync_intro) } else { *=$0A00 }

APPLY_INTERRUPT:
.if (RELEASE==false) {
      sta $D012
      stx $0314
      sty $0315
      jmp $ea81
}

// main function / subroutine
*=* "Main"
sync_intro:
      jsr $e544
      jsr gfx_init
      jsr charset_init
      jsr screen_init
      lda #$00
      jsr music.init // Initialize Music Playback

.if (RELEASE==false) {
      sei
      lda #$7f
      sta $dc0d     // turn off the CIA interrupts
      sta $dd0d
      and $d011     // clear high bit of raster line
      sta $d011
      :irqEnd #$2C:#starter.irq1

      lda #$01      // enable raster interrupts
      sta $d01a
      cli
      jmp *
}
      lda #$2C
      ldx #<starter.irq1
      ldy #>starter.irq1
      .if (RELEASE==true){ jmp C_APPLY_INTERRUPT } else { jmp APPLY_INTERRUPT }


gfx_init:
      ldx #$00
!:    txa
      sta $0400,x
      sta $0418,x
      inx
      bne !-
      rts

screen_init:
      ldx #$00
!:    txa
      sta $0720,x
      sta $db20,x
      inx
      cpx #$A0
      bne !-
      rts

charset_init:
      ldx #$00
!:    lda CHARSET,x
      sta $2000,x
      lda CHARSET+$100,x
      sta $2100,x
      lda CHARSET+$200,x
      sta $2200,x
      lda CHARSET+$300,x
      sta $2300,x
      inx
      bne !-
      rts

.import source "part2_irqstart.s"

// ************************************************
// ** IRQ Routinen                               **
// ************************************************
.namespace main {
irq1:
      inc VIC2InteruptStatus     // acknowledge interrupt
      lda #$19
      sta $d018
      lda #$1b
      sta $d011
      lda #$31
      cmp $d012
      bne *-3
      ldx #$0B
      dex
      bne *-1
      lda #$01
      sta VIC2BorderColour
      sta VIC2ScreenColour
      ldx #$09            // delay
      dex
      bne *-1
      lda #$00
      sta VIC2ScreenColour
      sta VIC2BorderColour
      lda #$6F
      ldx #<irq2
      ldy #>irq2
      .if (RELEASE==true){ jmp C_APPLY_INTERRUPT } else { jmp APPLY_INTERRUPT }

irq2:
      inc VIC2InteruptStatus     // acknowledge interrupt
      ldx #$03
      dex
      bne *-1
      lda #$06
      sta VIC2BorderColour
      sta VIC2ScreenColour

      lda #$7f               // detect space bar 
      sta CIA1KeyboardColumnJoystickA
      lda CIA1KeyboardRowsJoystickB
      and #$10
      bne update_irq
      jmp C_EXIT_PART

update_irq:
      lda #$8F
      ldx #<irq3
      ldy #>irq3
      .if (RELEASE==true){ jmp C_APPLY_INTERRUPT } else { jmp APPLY_INTERRUPT }

irq3:
      inc VIC2InteruptStatus     // acknowledge interrupt
      ldx #$03
      dex
      bne *-1
      lda #$00
      sta VIC2BorderColour
      sta VIC2ScreenColour

      dec VIC2BorderColour
      jsr music.play
      inc VIC2BorderColour

      lda #$D0
      ldx #<irq3b
      ldy #>irq3b
      .if (RELEASE==true){ jmp C_APPLY_INTERRUPT } else { jmp APPLY_INTERRUPT }

irq3b:
      inc VIC2InteruptStatus     // acknowledge interrupt
      lda #$D1
      cmp $D012
      bne *-3
      ldx #$0b
      dex
      bne *-1
      lda #$15
      sta $d018
      lda #$05
      sta VIC2BorderColour
      sta VIC2ScreenColour
      lda #$F2
      cmp $D012
      bne *-3
      ldx #$0B
      dex
      bne *-1
      lda #$00
      sta VIC2BorderColour
      sta VIC2ScreenColour


      lda #$F6
      ldx #<irq4
      ldy #>irq4
      .if (RELEASE==true){ jmp C_APPLY_INTERRUPT } else { jmp APPLY_INTERRUPT }

irq4:
      inc VIC2InteruptStatus     // acknowledge interrupt
      lda #$F8
      cmp $d012
      bne *-3
      ldx #$0B
      dex
      bne *-1
      lda #$01
      sta VIC2BorderColour
      sta VIC2ScreenColour
      ldx #$0B            // delay
      dex
      bne *-1
      lda #$00
      sta VIC2BorderColour
      sta VIC2ScreenColour

      lda #$2C
      ldx #<irq1
      ldy #>irq1
      .if (RELEASE==true){ jmp C_APPLY_INTERRUPT } else { jmp APPLY_INTERRUPT }

}
//***********************************************************************
//**  Included Labled Data (Resources from external files) which the   **
//**  compiler automatically (reserves and) assignes (a) (starting)    **
//**  memory (address) for.                                            **
//***********************************************************************
//*=$2000
*=music.location "Music"
.fill music.size, music.getData(i)

*=$2000 "Charset"
CHARSET: .import c64 "lazy_jones.64c"
//*********************************************************************72
//** File END                                                          **
//***********************************************************************
