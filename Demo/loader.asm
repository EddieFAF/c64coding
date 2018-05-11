// -- intro part loader example --
//
// Platform: C64
// Code: Jesder / 0xc64
// Site: http://www.0xc64.com
//
                        // zero page registers
.var REG_ZERO_I_DEV_NUM      = $99
.var REG_ZERO_O_DEV_NUM      = $9a
.var REG_ZERO_DEVICE_NO      = $ba


                        // common registers
.var REG_INTSERVICE_LOW      = $0314              // interrupt service routine low byte
.var REG_INTSERVICE_HIGH     = $0315              // interrupt service routine high byte
.var REG_STOP_LOW            = $0328
.var REG_STOP_HIGH           = $0329
.var REG_SCREENCTL_1         = $d011              // screen control register #1
.var REG_RASTERLINE          = $d012              // raster line position 
.var REG_INTCONTROL          = $d01a              // interrupt control register
.var REG_BORCOLOUR           = $d020              // border colour register
.var REG_SID                 = $d400
.var REG_SID_VOLUME          = $d418              // sid volume & filter register
.var REG_INTSTATUS_1         = $dc0d              // interrupt control and status register #1
.var REG_INTSTATUS_2         = $dd0d              // interrupt control and status register #2


                        // kernal routines
.var K_SETLFS                = $ffba
.var K_SETNAME               = $ffbd
.var K_CLOSE_FILE            = $ffc3
.var K_CLOSE_CHANNEL         = $ffcc
.var K_LOAD_FILE             = $ffd5


                        // constants
.var C_PLACEHOLDER           = $ffff

                        // program start
                        *= $0801 "Main"                 // begin (2049)

                        .byte $0b, $08, $01, $00, $9e, $32, $30, $36
                        .byte $31, $00, $00, $00        // BASIC sys call

                        jmp load_next_part


                        // unpack routine ---------------------------------------------]
                        // ------------------------------------------------------------]
unpack_next:            ldx #$01                        // unpack rle resources
                        jsr unpack_getbyte 
                        cmp #$c2                        // rle control code 
                        bne unpack_literal

                        jsr unpack_getbyte
                        cmp #$00                        // rle control + 0 = eof 
                        beq unpack_return

                        tax 
                        jsr unpack_getbyte

unpack_literal:         sta C_PLACEHOLDER
                        inc unpack_literal + 1 
                        bne unpack_return2 
                        inc unpack_literal + 2 

unpack_return2:         dex
                        bne unpack_literal 
                        beq unpack_next

unpack_getbyte:         lda C_PLACEHOLDER
                        inc unpack_getbyte + 1 
                        bne unpack_return 
                        inc unpack_getbyte + 2 

unpack_return:          rts 


                        // apply interrupt routine -------------------------------------------]
                        // -------------------------------------------------------------------]
apply_interrupt:        sta REG_RASTERLINE
                        stx REG_INTSERVICE_LOW
                        sty REG_INTSERVICE_HIGH
                        jmp $ea81


                        // exit part routine -------------------------------------------------]
                        // -------------------------------------------------------------------]
exit_intro_part:        lda #<clean_current_part        // reset main loop to load next part
                        sta forever + 1
                        lda #>clean_current_part
                        sta forever + 2

                        lda #000                        // switch off interrupts
                        sta REG_INTSTATUS_1
                        sta REG_INTSTATUS_2

                        lda #240                        // disable raster interrupts
                        sta REG_INTCONTROL

                        ldx #$31                        // restore interrupt pointer
                        ldy #$ea
                        jmp apply_interrupt


                        // clean up routine --------------------------------------------------]
                        // -------------------------------------------------------------------]
clean_up_intro_part:    sei

                        jsr $fda3

                        ldy #31                        // restore irq registers
irq_reset:              lda $fd30,y
                        sta $0314,y
                        dey
                        bpl irq_reset

                        lda #3                        // restore input / output device numbers
                        sta REG_ZERO_O_DEV_NUM
                        lda #000
                        sta REG_ZERO_I_DEV_NUM

                        ldx #$1f                        // restore sprites
sprite_reset:           lda $ecb8,x
                        sta $cfff,x
                        dex
                        bne sprite_reset

                        jsr $e51b
                        jsr $ff5e

                        ldy #32                        // restore sid chip
sid_reset:              lda #31
                        sta REG_SID,y
                        lda #000
                        sta REG_SID,y
                        dey
                        bpl sid_reset
                        lda #15
                        sta REG_SID_VOLUME

                        jsr $e544
                        
                        cli

                        rts


                        // loader variables -------------------------------------------------------]
                        // ------------------------------------------------------------------------]

loader_part_name_low:   .byte <loader_part_name_1, <loader_part_name_2, <loader_part_name_3
loader_part_name_high:  .byte >loader_part_name_1, >loader_part_name_2, >loader_part_name_3
loader_part_index:      .byte 000

load_part_name_length:  .byte 5, 5, 5

loader_part_name_1:     .text "PART1"
loader_part_name_2:     .text "PART2"
loader_part_name_3:     .text "PART3"


                        // loader main ------------------------------------------------------------]
                        // ------------------------------------------------------------------------]

clean_current_part:     jsr clean_up_intro_part         // clean up existing part
load_next_part:         jsr load_part                   // load next intro part
                        jsr sync_part                   // execute set up routine
forever:                jmp forever                     // idle until next part required


                        // load part routine -----------------------------------------------------]
                        // -----------------------------------------------------------------------]

load_part:              sei                             // hook loader "interrupt"
                        lda #<loader_interrupt
                        ldx #>loader_interrupt
                        sta REG_STOP_LOW
                        stx REG_STOP_HIGH
                        cli

                        lda #8                        // set device parameters
                        ldx REG_ZERO_DEVICE_NO
                        ldy #000
                        jsr K_SETLFS

set_filename_length:    lda load_part_name_length       // set file name parameters
set_filename_low:       ldx loader_part_name_low
set_filename_high:      ldy loader_part_name_high
                        jsr K_SETNAME

                        lda #000                        // load intro part
                        ldx #<load_address
                        ldy #>load_address
                        jsr K_LOAD_FILE

                        lda #000                        // close file system
                        jsr K_CLOSE_FILE
                        jsr K_CLOSE_CHANNEL

                        inc set_filename_length + 1     // advance name pointers
                        inc set_filename_low + 1
                        inc set_filename_high + 1

                        inc loader_part_index

                        sei                             // restore stop pointer
                        lda #$ed
                        ldx #$f6
                        sta REG_STOP_LOW
                        stx REG_STOP_HIGH
                        cli

                        lda #<forever                   // restore eternal loop
                        sta forever + 1
                        lda #>forever
                        sta forever + 2

                        rts

                        
                        // loader interrupt ---------------------------------------------]
                        // --------------------------------------------------------------]

loader_interrupt:       stx REG_BORCOLOUR
                        jmp $f6fe


                        // sync new part ------------------------------------------------]
                        // --------------------------------------------------------------]

sync_part:              sei
                        lda #$7f
                        sta REG_INTSTATUS_1             // turn off the CIA interrupts
                        sta REG_INTSTATUS_2
                        and REG_SCREENCTL_1             // clear high bit of raster line
                        sta REG_SCREENCTL_1

                        ldy #000
                        sty REG_RASTERLINE

                        lda #<load_address              // load interrupt address
                        ldx #>load_address
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        lda #$01                        // enable raster interrupts
                        sta REG_INTCONTROL
                        cli

                        rts


                        // loader interrupt --------------------------------------------------]
                        // -------------------------------------------------------------------]

// include "fastload.s"

load_address:           nop

