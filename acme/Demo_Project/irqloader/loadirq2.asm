; -- intro part loader example --
;
; Platform: C64
; Code: Jesder / 0xc64
; Site: http:;www.0xc64.com
;

!src "../stdlib/stdlib.a"

                        ; zero page registers
REG_ZERO_I_DEV_NUM      = $99
REG_ZERO_O_DEV_NUM      = $9a
REG_ZERO_DEVICE_NO      = $ba

load_address = $5FFE
load_target = $c300



                        ; common registers
REG_INTSERVICE_LOW      = $0314              ; interrupt service routine low byte
REG_INTSERVICE_HIGH     = $0315              ; interrupt service routine high byte
REG_STOP_LOW            = $0328
REG_STOP_HIGH           = $0329
REG_SCREENCTL_1         = $d011              ; screen control register #1
REG_RASTERLINE          = $d012              ; raster line position 
REG_INTCONTROL          = $d01a              ; interrupt control register
REG_SID                 = $d400
REG_SID_VOLUME          = $d418              ; sid volume & filter register
REG_INTSTATUS_1         = $dc0d              ; interrupt control and status register #1
REG_INTSTATUS_2         = $dd0d              ; interrupt control and status register #2


RELEASE = 1
                        ; constants
C_PLACEHOLDER           = $ffff

STARTZEILE              = 10
;                        BasicUpstart2(start)
*=$8000

                        ; program start
                        ;*= $080D "Main"                 ; begin (2049)

start:


                        jsr initloader

                        ldx #$00
-                       lda char,x
                        sta $2000,x
                        lda char+$100,x
                        sta $2100,x
                        lda char+$200,x
                        sta $2200,x
                        lda char+$300,x
                        sta $2300,x
                        lda char+$400,x
                        sta $2400,x
                        lda char+$500,x
                        sta $2500,x
                        inx
                        bne -

                        ldx #$00
-                       lda screen_char1,x
                        sta $0400+STARTZEILE*40,x
                        lda screen_color1,x
                        sta $d800+STARTZEILE*40,x
                        inx
                        cpx #200
                        bne -

                        jmp load_next_part


                        ; apply interrupt routine -------------------------------------------]
                        ; -------------------------------------------------------------------]
apply_interrupt:        sta REG_RASTERLINE
                        stx REG_INTSERVICE_LOW
                        sty REG_INTSERVICE_HIGH
                        jmp $ea81


                        ; exit part routine -------------------------------------------------]
                        ; -------------------------------------------------------------------]
exit_intro_part:        lda #<clean_current_part        ; reset main loop to load next part
                        sta forever + 1
                        lda #>clean_current_part
                        sta forever + 2

                        lda #000                        ; switch off interrupts
                        sta REG_INTSTATUS_1
                        sta REG_INTSTATUS_2

                        lda #240                        ; disable raster interrupts
                        sta REG_INTCONTROL

                        ldx #$31                        ; restore interrupt pointer
                        ldy #$ea
                        jmp apply_interrupt


                        ; clean up routine --------------------------------------------------]
                        ; -------------------------------------------------------------------]
clean_up_intro_part:    sei

                        jsr $fda3

                        ldy #31                        ; restore irq registers
irq_reset:              lda $fd30,y
                        sta $0314,y
                        dey
                        bpl irq_reset

                        lda #3                        ; restore input / output device numbers
                        sta REG_ZERO_O_DEV_NUM
                        lda #000
                        sta REG_ZERO_I_DEV_NUM

                        ldx #$1f                        ; restore sprites
sprite_reset:           lda $ecb8,x
                        sta $cfff,x
                        dex
                        bne sprite_reset

                        jsr $e51b
                        jsr $ff5e

                        ldy #32                        ; restore sid chip
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


                        ; loader variables -------------------------------------------------------]
                        ; ------------------------------------------------------------------------]

loader_part_name_low:   !byte <loader_part_name_1, <loader_part_name_2, <loader_part_name_3
loader_part_name_high:  !byte >loader_part_name_1, >loader_part_name_2, >loader_part_name_3
loader_part_index:      !byte 0

load_part_name_length:  !byte 2, 2, 2

loader_part_name_1:     !text "01"
loader_part_name_2:     !text "02"
loader_part_name_3:     !text "03"


                        ; loader main ------------------------------------------------------------]
                        ; ------------------------------------------------------------------------]

clean_current_part:     jsr clean_up_intro_part         ; clean up existing part
load_next_part:         jsr load_part                   ; load next intro part
                        jsr sync_part                   ; execute set up routine
forever:                jmp forever                     ; idle until next part required


                        ; load part routine -----------------------------------------------------]
                        ; -----------------------------------------------------------------------]

load_part:              sei                             ; hook loader "interrupt"
                        lda #$7f
                        sta $dc0d
                        sta $dd0d
                        and $d011
                        sta $d011

                        ldy #58
                        sty $d012

                        lda #<loader_interrupt
                        ldx #>loader_interrupt
                        sta $0314
                        stx $0315

                        lda #$01
                        sta $d01a
                        cli

                        clc
                        ldx loader_part_index
set_filename_high:      lda loader_part_name_high,x
                        tay
set_filename_low:       lda loader_part_name_low,x
                        tax
!if (RELEASE=1) {
                        jsr loadfile_exomizer
}

;                        inc set_filename_low + 1
;                        inc set_filename_high + 1

                        inc loader_part_index


                        lda #<forever                   ; restore eternal loop
                        sta forever + 1
                        lda #>forever
                        sta forever + 2

!if RELEASE=1 {
                        rts
} ELSE { JMP * }

                        ; loader interrupt ---------------------------------------------]
                        ; --------------------------------------------------------------]

loader_interrupt:
                        lda #STARTZEILE*8+49
                        cmp $d012
                        bne *-3
                        ldx #$0A
                        dex
                        bne *-1
c1:                     lda #$00
                        sta VIC2BorderColour
                        sta VIC2ScreenColour
                        ldx #$0B
                        dex
                        bne *-1
                        lda #$18
                        sta $D018
                        lda #$18
                        sta $D016
                        lda #$06
                        sta VIC2ScreenColour
                        lda #$0e
                        sta $D022
                        lda #$03
                        sta $D023

                        lda #<irq2
                        ldx #>irq2
                        sta $0314
                        stx $0315

                        lda #(STARTZEILE+5)*8+48
                        sta $d012

                        asl $d019
                        jmp $ea81

irq2:                   lda #(STARTZEILE+5)*8+50
                        cmp $d012
                        bne *-3
                        ldx #$0A
                        dex
                        bne *-1
                        lda #$15
                        sta $D018
                        lda #200
                        sta $D016
                        lda #$00
                        sta VIC2ScreenColour
                        ldx #$0E
                        dex
                        bne *-1
                        lda #$0e
                        sta VIC2BorderColour
                        lda #$06
                        sta VIC2ScreenColour
                        
                        lda #<loader_interrupt
                        ldx #>loader_interrupt
                        sta $0314
                        stx $0315

                        ldy #STARTZEILE*8+46
                        sty $d012

                        asl $d019
                        jmp $ea31


                        ; sync new part ------------------------------------------------]
                        ; --------------------------------------------------------------]

sync_part:              sei
                        lda #$7f
                        sta REG_INTSTATUS_1             ; turn off the CIA interrupts
                        sta REG_INTSTATUS_2
                        and REG_SCREENCTL_1             ; clear high bit of raster line
                        sta REG_SCREENCTL_1

                        ldy #000
                        sty REG_RASTERLINE

                        lda #$00                       ; load interrupt address
                        ldx #$0a
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        lda #$01                        ; enable raster interrupts
                        sta REG_INTCONTROL
                        cli

                        rts


                        ; loader interrupt --------------------------------------------------]
                        ; -------------------------------------------------------------------]

; include "fastload.s"
                !src "cfg_exom.s"
                !src "loader.s"

*=$7800
char:
!src "charset1.inc"
!src "screen1.inc"

