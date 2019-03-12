// -- multi-part loader example - demo part 1 --
//
// Platform: C64
// Code: Jesder / 0xc64
// Site: http://www.0xc64.com
//
                        // kernal routines

.var KER_CLRSCREEN           = $e544              // clear screen


                        // common registers

.var REG_INTFLAG             = $d019              // interrupt flag register
.var REG_BORCOLOUR           = $d020              // border colour register
.var REG_BGCOLOUR            = $d021              // background colour register
.var REG_KEYBOARD_PORT_A     = $dc00
.var REG_KEYBOARD_PORT_B     = $dc01


                        // constants

.var C_SCREEN_RAM            = $0400
.var C_UNPACK_ROUTINE        = $0810
.var C_UNPACK_DEST           = $0824
.var C_UNPACK_SOURCE         = $0834
.var C_APPLY_INTERRUPT       = $0840
.var C_EXIT_PART             = $084c
.var C_COLOUR_RAM            = $d800

                        // program start
 *=$0950 "Part1"

                        // intro sync -------------------------------------------------------------]
                        // ------------------------------------------------------------------------]
sync_intro:             inc REG_INTFLAG                 // acknowledge interrupt
                        jsr KER_CLRSCREEN               // clear screen
                        lda #000                        // set border/background
                        sta REG_BORCOLOUR
                        sta REG_BGCOLOUR

                        ldx #39 // init messages
render_messages:        lda message1,x
                        sta C_SCREEN_RAM + $28,x
                        lda message2,x
                        sta C_SCREEN_RAM + $78,x
                        lda #000
                        sta C_COLOUR_RAM + $28,x
                        sta C_COLOUR_RAM + $78,x
                        dex
                        bpl render_messages

                        lda #49
                        ldx #<render_border_top
                        ldy #>render_border_top
                        jmp C_APPLY_INTERRUPT

                        // latch raster bar ------------------------------------------------]
                        // -----------------------------------------------------------------]
latch_raster:           dex
                        bpl latch_raster
                        rts

                        // render single line bar ------------------------------------------]
                        // -----------------------------------------------------------------]
render_bar:             ldx #000                        // stabalise raster
                        jsr latch_raster

                        lda #001                        // switch on single line border bar
                        sta REG_BORCOLOUR

                        ldx #006                        // complete raster line
                        jsr latch_raster
                        rts

                        // render border top -----------------------------------------------]
                        // -----------------------------------------------------------------]
render_border_top:      inc REG_INTFLAG                 // acknowledge interrupt

                        jsr render_bar

                        lda #000                        // border bar complete
                        sta REG_BORCOLOUR

                        lda #250
                        ldx #<render_border_bot
                        ldy #>render_border_bot
                        jmp C_APPLY_INTERRUPT

                        // render border bottom ---------------------------------------------]
                        // ------------------------------------------------------------------]
render_border_bot:      inc REG_INTFLAG                 // acknowledge interrupt
                        jsr render_bar
                        lda #006                        // border bar complete
                        sta REG_BORCOLOUR

                        lda #255
                        ldx #<update_colours
                        ldy #>update_colours
                        jmp C_APPLY_INTERRUPT

                        // update colours ----------------------------------------------------]
                        // -------------------------------------------------------------------]
update_colours:         inc REG_INTFLAG                 // acknowledge interrupt
                        lda #$7f                        // detect space bar
                        sta REG_KEYBOARD_PORT_A
                        lda REG_KEYBOARD_PORT_B
                        and #$10
                        bne update_colour_ram
                        jmp C_EXIT_PART

update_colour_ram:      dec colour_advance_delay        // smooth colour cycle
                        bpl update_colours_done
                        lda #001
                        sta colour_advance_delay

                        ldx #38                        // shift colour ram right
shift_cols_right:       lda C_COLOUR_RAM + $28,x
                        sta C_COLOUR_RAM + $29,x
                        dex
                        bpl shift_cols_right

                        ldx #000                        // shift colour ram left
shift_cols_left:        lda C_COLOUR_RAM + $79,x
                        sta C_COLOUR_RAM + $78,x
                        inx
                        cpx #39
                        bne shift_cols_left

                        ldx colour_index                // grab next colour in sequence
                        lda colour_sequence,x
                        sta C_COLOUR_RAM + $28          // insert new colour into ram
                        sta C_COLOUR_RAM + $9f                        
                        inx                             // advance colour index
                        txa
                        and #15                        // loop back to 0
                        sta colour_index

update_colours_done:    lda #49
                        ldx #<render_border_top
                        ldy #>render_border_top
                        jmp C_APPLY_INTERRUPT

                        // data tables -------------------------------------------------------------]
                        // -------------------------------------------------------------------------]
message1:               .byte 32, 32, 32, 32, 32, 32, 19, 9, 13, 16, 12, 5, 32, 12, 15, 1
                        .byte 4, 5, 18, 32, 5, 24, 1, 13, 16, 12, 5, 32, 45, 32, 48, 24
                        .byte 3, 54, 52, 32, 32, 32, 32, 32
message2:               .byte 32, 32, 32, 32, 32, 32, 32, 32, 32, 16, 18, 5, 19, 19, 32, 19
                        .byte 16, 1, 3, 5, 32, 20, 15, 32, 3, 15, 14, 20, 9, 14, 21,5
                        .byte 32, 32, 32, 32, 32, 32, 32, 32
colour_advance_delay:   .byte 1
colour_index:           .byte 0
colour_sequence:        .byte 9, 8, 5, 13, 13, 5, 8, 9, 2, 10, 7, 1, 7, 10, 2, 9
