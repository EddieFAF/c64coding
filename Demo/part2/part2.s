//*********************************************************************72
//**  Project Name: Demo Project                                       **
//**  ---------------------------------------------------------------  **
//**  Filename: Part2.asm                                              **
//**  ---------------------------------------------------------------  **
//**  Author (c): [FAF]Eddie                                           **
//**  File Date: 2018-04-28                                            **
//***********************************************************************
#import "../stdlib/stdlib_k.a"
#import "../stdlib/functions.inc"
.import source "../loader/loader.sym"

.var RELEASE = false

.var music = LoadSid("One_Man_and_His_Droid.sid")
                        // constants
.var C_SCREEN_RAM             = $0400
.var C_UNPACK_ROUTINE         = loader.unpack_next // $0810
.var C_UNPACK_DEST            = loader.unpack_literal // $0824
.var C_UNPACK_SOURCE          = loader.unpack_getbyte // $0834
.var C_APPLY_INTERRUPT        = loader.apply_interrupt // $0840
.var C_EXIT_PART              = loader.exit_intro_part // $084C
.var C_COLOUR_RAM             = $d800

.var scroll_line              = $0568
.var scroll_speed             = $01
.var scroll_line_4x4          = $0720
.var scroll_speed_4x4         = $02

.var C_CHARSET                = $2000
.var C_CHARSET_HIGH           = $2100
.var REG_ZERO_FE              = $fe
.var REG_ZERO_FD              = $fd

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
      jsr music.init

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
      lda #$00
      sta $db20,x
      inx
      cpx #$A0
      bne !-
      lda #<scroll1x1_text
      ldy #>scroll1x1_text
      sta read+1
      sty read+2
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
      lda #$18
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

      lda scroll_delay
      sta $d016

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

      jsr scroll4x4

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
      lda #$19
      sta $d018
      lda #$05
      sta VIC2BorderColour
      sta VIC2ScreenColour

      lda scroll_delay_4x4
      sta $d016

      jsr scroll1x1

      lda #$F2
      cmp $D012
      bne *-3
      ldx #$0B
      dex
      bne *-1
      lda #$00
      sta VIC2BorderColour
      sta VIC2ScreenColour
      lda #200
      sta $d016

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
scroll1x1:
      lda scroll_delay
      sec
      sbc #scroll_speed
      bcc !+
      sta scroll_delay
      rts
!:    and #$07
      sta scroll_delay
      jsr scroll1x1_hardscroll
      rts

scroll1x1_hardscroll:
      ldy #0
!:    lda scroll_line+1,y
      sta scroll_line,y
      iny
      cpy #$28
      bne !-

read: lda scroll_line+$27
      cmp #$00
      bne !+
      lda #<scroll1x1_text
      ldy #>scroll1x1_text
      sta read+1
      sty read+2
      jmp read
!:    sta scroll_line+$27
      inc read+1
      lda read+1
      cmp #$00
      bne !+
      inc read+2
!:    rts

scroll4x4:
      lda scroll_delay_4x4
      sec
      sbc #scroll_speed_4x4
      bcc !+
      sta scroll_delay_4x4
      rts
!:    and #$07
      sta scroll_delay_4x4
      jsr scroll4x4_hardscroll
      rts

scroll4x4_hardscroll:
      ldy #0
!:    lda scroll_line_4x4+1,y
      sta scroll_line_4x4,y
      lda scroll_line_4x4+41,y
      sta scroll_line_4x4+40,y
      lda scroll_line_4x4+81,y
      sta scroll_line_4x4+80,y
      lda scroll_line_4x4+121,y
      sta scroll_line_4x4+120,y
      iny
      cpy #$28
      bne !-

            ldx scroller_char_step + 1
            bpl render_next_scroll_colm

scroller_message_index:
            ldx #000
read_next_scroller_char:
            lda scroll4x4_text, x
            bpl advance_scroller_index
            lda scroll4x4_text
            ldx #001
            ldy #>scroll4x4_text
            sty read_next_scroller_char + 2
            jmp save_scroller_index

advance_scroller_index:
            inx
            bne save_scroller_index
            inc read_next_scroller_char + 2
save_scroller_index:
            stx scroller_message_index + 1

            ldy #>CHARSET
            cmp #031
            bcc calc_scrollchar_src_low
            ldy #>CHARSET+$100

calc_scrollchar_src_low:
            and #031
            asl
            asl
            asl

            sty render_scroller_column + 2
            sty render_scroller_column2 + 2
            sta render_scroller_column + 1
            sta render_scroller_column2 + 1

            lda #192
            sta scroller_character_mask + 1
            lda #003
            sta scroller_char_step + 1

render_next_scroll_colm:
            ldx #000
            stx REG_ZERO_FD
render_scroller_column:
            lda CHARSET, x
scroller_character_mask:
            and #192
scroller_char_step:
            ldy #255
            beq skip_shift_1
shift_scroll_mask_loop1:
            lsr
            lsr
            dey
            bne shift_scroll_mask_loop1
skip_shift_1:
            asl
            asl
            sta REG_ZERO_FE

            inx
render_scroller_column2: lda CHARSET, x
            and scroller_character_mask + 1
            ldy scroller_char_step + 1
            beq skip_shift_2
shift_scroll_mask_loop2: lsr
            lsr
            dey
            bne shift_scroll_mask_loop2

skip_shift_2:           clc
            adc REG_ZERO_FE
            adc #064

scroller_render_offset: ldy REG_ZERO_FD
            sta scroll_line_4x4 + $27, y
            tya
            adc #040
            sta REG_ZERO_FD
            inx
            cpx #008
            bne render_scroller_column
            dec scroller_char_step + 1
            lda scroller_character_mask + 1
            lsr
            lsr
            sta scroller_character_mask + 1
            rts


// ************************************************************************** //
scroll_delay:     .byte $00
scroll_delay_4x4: .byte $00
scroll1x1_text:   .text "dies ist ein 1x1 scrolltext, der sehr lang ist und"
                  .text " daher auch ueber viele zeilen gehen kann.... "
                  .byte $00
scroll4x4_text:   .text "dies ist ein 4x4 scrolltext, der sehr lang ist und"
                  .text " daher auch ueber viele zeilen gehen kann.... "
                  .byte $FF

//***********************************************************************
//**  Included Labled Data (Resources from external files) which the   **
//**  compiler automatically (reserves and) assignes (a) (starting)    **
//**  memory (address) for.                                            **
//***********************************************************************
*=music.location "Music"
.fill music.size, music.getData(i)

*=$2000 "Charset"
CHARSET: .import c64 "aeg_collection_09.64c"
// 4x4 dot matrix
      .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $0e, $0e, $0e, $00
      .byte $00, $00, $00, $00, $e0, $e0, $e0, $00, $00, $00, $00, $00, $ee, $ee, $ee, $00
      .byte $0e, $0e, $0e, $00, $00, $00, $00, $00, $0e, $0e, $0e, $00, $0e, $0e, $0e, $00
      .byte $0e, $0e, $0e, $00, $e0, $e0, $e0, $00, $0e, $0e, $0e, $00, $ee, $ee, $ee, $00
      .byte $e0, $e0, $e0, $00, $00, $00, $00, $00, $e0, $e0, $e0, $00, $0e, $0e, $0e, $00
      .byte $e0, $e0, $e0, $00, $e0, $e0, $e0, $00, $e0, $e0, $e0, $00, $ee, $ee, $ee, $00
      .byte $ee, $ee, $ee, $00, $00, $00, $00, $00, $ee, $ee, $ee, $00, $0e, $0e, $0e, $00
      .byte $ee, $ee, $ee, $00, $e0, $e0, $e0, $00, $ee, $ee, $ee, $00, $ee, $ee, $ee, $00

//*********************************************************************72
//** File END                                                          **
//***********************************************************************
