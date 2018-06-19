// -- multi-part loader example - demo part 3 --
//
// Platform: C64
// Code: Jesder / 0xc64
// Site: http://www.0xc64.com
//

                        // zero page registers
.var REG_ZERO_52             = $52

                        // kernal routines
.var KER_CLRSCREEN           = $e544              // clear screen

                        // common registers
.var REG_INTSERVICE_LOW      = $0314              // interrupt service routine low byte
.var REG_INTSERVICE_HIGH     = $0315              // interrupt service routine high byte
.var REG_RASTERLINE          = $d012              // raster line position 
.var REG_SCREENCTL_2         = $d016              // screen control register #2
.var REG_MEMSETUP            = $d018              // memory setup register
.var REG_INTFLAG             = $d019              // interrupt flag register
.var REG_BORCOLOUR           = $d020              // border colour register
.var REG_BGCOLOUR            = $d021              // background colour register

                        // constants
.var C_SCREEN_RAM            = $0400
.var C_UNPACK_ROUTINE        = $0810
.var C_UNPACK_DEST           = $0824
.var C_UNPACK_SOURCE         = $0834
.var C_APPLY_INTERRUPT       = $0840
.var C_EXIT_PART             = $084c
.var C_CHARSET               = $3000
.var C_TABLE_DATA            = $9000
.var C_SCROLL_INDEX          = $9001
.var C_SCROLL_MESSAGE_INDEX  = $9002
.var C_SCROLL_RASTER_BUFFER  = $9010              // raster bar render table (8 bytes)
.var C_SCROLL_MESSAGE        = $9500
.var C_COLOUR_RAM            = $d800

                        // program start
                        *=$0950 "Main"

                        // intro sync -------------------------------------------------------------------]
                        // ------------------------------------------------------------------------------]
sync_intro:             inc REG_INTFLAG                 // acknowledge interrupt

                        jsr KER_CLRSCREEN               // clear screen

                        lda #000                        // set border/background
                        sta REG_BORCOLOUR
                        sta REG_BGCOLOUR

                        lda #000                        // initialise table data
                        ldx #000
init_table_data_memory: sta C_TABLE_DATA,x
                        inx
                        bne init_table_data_memory

                        lda #<font_data
                        ldx #>font_data
                        sta C_UNPACK_SOURCE + 1
                        stx C_UNPACK_SOURCE + 2
                        lda #<C_CHARSET
                        ldx #>C_CHARSET
                        sta C_UNPACK_DEST + 1
                        stx C_UNPACK_DEST + 2
                        jsr C_UNPACK_ROUTINE

                        clc
                        ldx #000
generate_inverse_font:  lda C_CHARSET,x
                        sta REG_ZERO_52                 // generate inverse byte
                        lda #$ff
                        sbc REG_ZERO_52
                        sta C_CHARSET + $200,x
                        lda C_CHARSET + $100,x
                        sta REG_ZERO_52
                        lda #$ff
                        sbc REG_ZERO_52
                        sta C_CHARSET + $300,x
                        inx
                        bne generate_inverse_font

                        ldx #106                        // relocate scroller text
relocate_scroll_text:   lda message,x
                        sta C_SCROLL_MESSAGE,x
                        dex
                        bpl relocate_scroll_text

                        ldx #39                        // init scroller line
clear_scroller_line:    lda #96
                        sta $0400 + $3c0,x
                        lda #000
                        sta $d800 + $3c0,x
                        dex
                        bpl clear_scroller_line

                        ldx #39                        // init messages
render_messages:        lda message1,x
                        sta C_SCREEN_RAM + $28,x
                        lda message2,x
                        sta C_SCREEN_RAM + $78,x
                        lda #10
                        sta C_COLOUR_RAM + $28,x
                        sta C_COLOUR_RAM + $78,x
                        dex
                        bpl render_messages

                        lda #28                        // switch to demo font
                        sta REG_MEMSETUP

                        lda #25
                        ldx #<update_music
                        ldy #>update_music
                        jmp C_APPLY_INTERRUPT

                        // music update ---------------------------------------------------------]
                        // ----------------------------------------------------------------------]
update_music:           inc REG_INTFLAG                 // acknowledge interrupt

set_postmusic_line:     lda #242                        // register music player interrupt
set_postmusic_low:      ldx #<render_scroller
set_postmusic_high:     ldy #>render_scroller
                        jmp C_APPLY_INTERRUPT

                        // render scroller raster bars ------------------------------------------]
                        // ----------------------------------------------------------------------]
render_scroller:        inc REG_INTFLAG                 // acknowledge interrupt

                        nop                             // remove raster jitter
                        nop

                        lda #$c0                        // 40 column mode
                        ora scroll_magnitude            // add hardware scroll
                        sta REG_SCREENCTL_2

                        lda C_SCROLL_RASTER_BUFFER + 1
                        sta REG_BGCOLOUR

                        ldx #1
next_animated_bar:      lda C_SCROLL_RASTER_BUFFER + 1,x
                        sta REG_BGCOLOUR
                        ldy #9                        // delay until line complete
latch_raster:           dey
                        bpl latch_raster

                        inx
                        cpx #8                        // render 24 bars
                        bne next_animated_bar

scroller_rasters_done:  lda #000                        // reset background colour
                        sta REG_BGCOLOUR

                        lda #$c8                        // 40 column mode + no scroll
                        sta REG_SCREENCTL_2

                        lda #252
                        ldx #<update_scroller
                        ldy #>update_scroller
                        jmp C_APPLY_INTERRUPT

                        // update scroller  -----------------------------------------------]
                        // ----------------------------------------------------------------]
update_scroller:        inc REG_INTFLAG                 // acknowledge interrupt

                        dec scroll_magnitude            // update hardware scroll size
                        bpl update_scroller_bars

                        lda #7                        // reset hardware scroll size
                        sta scroll_magnitude

                        ldx #000                        // time to shift characters
shift_scroll_chars:     lda C_SCREEN_RAM + $3c1,x
                        sta C_SCREEN_RAM + $3c0,x
                        inx
                        cpx #39
                        bne shift_scroll_chars

                        ldx C_SCROLL_MESSAGE_INDEX      // add next message character to buffer
load_next_scroll_char:  lda C_SCROLL_MESSAGE,x
                        cmp #255                        // detect end of message
                        bne advance_message_index

                        lda #>C_SCROLL_MESSAGE
                        sta load_next_scroll_char + 2
                        lda #96
                        ldx #000
                        jmp save_scrollmsg_index

advance_message_index:  inx
                        bne save_scrollmsg_index
                        inc load_next_scroll_char + 2

save_scrollmsg_index:   stx C_SCROLL_MESSAGE_INDEX      // updated message character index
                        sta C_SCREEN_RAM + $3e7

update_scroller_bars:   dec scroll_raster_smoothing     // apply smoothing
                        bpl scroll_raster_done
                        lda #002
                        sta scroll_raster_smoothing

                        clc
                        ldx white_raster_sine_index     // determine start height of white rasters
                        lda scroller_bar_sine,x
                        lsr                       
                        adc #8
                        tay

                        ldx #7                        // render white raster bars on scroller
render_white_rasters:   lda white_raster_bar,y
                        sta C_SCROLL_RASTER_BUFFER,x
                        dey
                        dex
                        bpl render_white_rasters

                        clc
                        ldx red_raster_sine_index       // determine start height of red rasters
                        lda scroller_bar_sine,x
                        adc #003
                        tay

                        ldx #002                        // render red raster bars on scroller
render_red_rasters:     lda red_raster_bar,x
                        sta C_SCROLL_RASTER_BUFFER,y
                        dey
                        dex
                        bpl render_red_rasters

                        clc                             // advance white raster sine
                        lda white_raster_sine_index
                        adc #1
                        and #15
                        sta white_raster_sine_index

                        clc                             // advance red raster sine
                        lda red_raster_sine_index
                        adc #1
                        and #15
                        sta red_raster_sine_index

scroll_raster_done:     lda #25
                        ldx #<update_music
                        ldy #>update_music
                        jmp C_APPLY_INTERRUPT

                        // data tables -----------------------------------------------------------------------------------------------------]
                        // -----------------------------------------------------------------------------------------------------------------]
scroll_raster_smoothing: .byte 1
red_raster_bar:          .byte 10, 2, 10
white_raster_bar:       .byte 11, 11, 11, 12, 12, 15, 15, 1, 1, 15, 15, 12, 12, 3, 3
scroller_bar_sine:      .byte 0, 0, 0, 1, 1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1, 1
red_raster_sine_index:  .byte 8
white_raster_sine_index: .byte 8
scroll_magnitude:       .byte 7

message1:               .byte 32, 32, 32, 32, 32, 19, 9, 13, 16, 12, 5, 32, 12, 15, 1, 4
                        .byte 5, 18, 32, 5, 24, 1, 13, 16, 12, 5, 32, 45, 32, 48, 24, 3
                        .byte 54, 52, 32, 32, 32, 32, 32, 32
message2:               .byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 42, 42, 32, 6
                        .byte 9, 14, 1, 12, 32, 16, 1, 18, 20, 32, 42, 42, 32, 32, 32, 32
                        .byte 32, 32, 32, 32, 32, 32, 32, 32

message:                .byte 83, 73, 77, 80, 76, 69, 96, 77, 85, 76, 84, 73, 109, 80, 65, 82
                        .byte 84, 96, 73, 78, 84, 82, 79, 96, 76, 79, 65, 68, 69, 82, 96, 69
                        .byte 88, 65, 77, 80, 76, 69, 96, 66, 89, 96, 74, 69, 83, 68, 69, 82
                        .byte 96, 111, 96, 112, 88, 67, 118, 116, 110, 110, 110, 110, 110, 96, 96, 84
                        .byte 72, 73, 83, 96, 73, 83, 96, 84, 72, 69, 96, 70, 73, 78, 65, 76
                        .byte 96, 80, 65, 82, 84, 96, 73, 78, 96, 84, 72, 69, 96, 69, 88, 65
                        .byte 77, 80, 76, 69, 110, 110, 96, 96, 96, 255

font_data:              .byte $00, $00, $3c, $40, $5c, $52, $3c, $c2, $03, $00, $7c, $c6, $de, $c6, $c6, $c2
                        .byte $03, $00, $7c, $c6, $dc, $c6, $dc, $c2, $03, $00, $7e, $c2, $03, $c0, $7e, $c2
                        .byte $03, $00, $fc, $c2, $03, $c6, $dc, $c2, $03, $00, $7e, $c0, $dc, $c0, $7e, $c2
                        .byte $03, $00, $7e, $c0, $dc, $c0, $c0, $c2, $03, $00, $7c, $c0, $ce, $c6, $7c, $c2
                        .byte $03, $00, $c6, $c6, $de, $c6, $c6, $c2, $03, $00, $c2, $05, $18, $c2, $03, $00
                        .byte $fe, $06, $06, $c6, $7c, $c2, $03, $00, $c6, $c6, $dc, $c6, $c6, $c2, $03, $00
                        .byte $c2, $03, $c0, $e0, $7e, $c2, $03, $00, $7e, $c2, $04, $db, $c2, $03, $00, $7c
                        .byte $c2, $04, $c6, $c2, $03, $00, $7c, $c2, $03, $c6, $7c, $c2, $03, $00, $7c, $c6
                        .byte $dc, $c0, $c0, $c2, $03, $00, $7c, $c6, $c6, $ce, $7e, $c2, $03, $00, $7c, $c6
                        .byte $dc, $c6, $c6, $c2, $03, $00, $7e, $c0, $7c, $06, $fc, $c2, $03, $00, $7e, $c2
                        .byte $04, $18, $c2, $03, $00, $c2, $04, $c6, $7c, $c2, $03, $00, $c2, $03, $c6, $6c
                        .byte $38, $c2, $03, $00, $c2, $04, $db, $7e, $c2, $03, $00, $c6, $c6, $6c, $c6, $c6
                        .byte $c2, $03, $00, $cc, $cc, $78, $30, $30, $c2, $03, $00, $fe, $0e, $38, $e0, $fe
                        .byte $c2, $03, $00, $70, $c2, $03, $40, $70, $c2, $0b, $00, $1c, $c2, $03, $04, $1c
                        .byte $c2, $19, $00, $18, $c2, $03, $24, $18, $00, $18, $18, $28, $28, $c2, $1d, $00
                        .byte $82, $00, $18, $24, $28, $16, $24, $1a, $00, $38, $24, $14, $0c, $c2, $05, $00
                        .byte $1c, $20, $c2, $03, $40, $20, $1c, $00, $38, $04, $c2, $03, $02, $04, $c2, $0a
                        .byte $00, $10, $10, $7c, $10, $10, $c2, $06, $00, $1c, $1c, $04, $08, $00, $02, $04
                        .byte $08, $10, $20, $40, $c2, $06, $00, $06, $06, $00, $00, $02, $04, $08, $10, $20
                        .byte $40, $c2, $03, $00, $7c, $c2, $03, $c6, $7c, $c2, $03, $00, $18, $38, $18, $18
                        .byte $3c, $c2, $03, $00, $fc, $06, $7c, $c0, $fe, $c2, $03, $00, $fc, $06, $7c, $06
                        .byte $fc, $c2, $03, $00, $c6, $c6, $7e, $06, $06, $c2, $03, $00, $fc, $c0, $fc, $06
                        .byte $fc, $c2, $03, $00, $7c, $c0, $fc, $c6, $7c, $c2, $03, $00, $fc, $0e, $c2, $03
                        .byte $06, $c2, $03, $00, $7c, $c6, $7c, $c6, $7c, $c2, $03, $00, $7c, $c6, $76, $06
                        .byte $06, $c2, $03, $00, $18, $18, $00, $00, $18, $c2, $08, $00, $10, $00, $00, $0c
                        .byte $10, $60, $10, $0c, $c2, $18, $00, $18, $c2, $00
