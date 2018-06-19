; -- intro part loader example --
;
; Platform: C64
; Code: Jesder / 0xc64
; Site: http:;www.0xc64.com
;
!sl "loader.inc"

                        ; zero page registers
REG_ZERO_I_DEV_NUM      = $99
REG_ZERO_O_DEV_NUM      = $9a
REG_ZERO_DEVICE_NO      = $ba

load_address = $5FFE
load_target = $c300

!src "loadersymbols-c64.inc"


                        ; common registers
REG_INTSERVICE_LOW      = $0314              ; interrupt service routine low byte
REG_INTSERVICE_HIGH     = $0315              ; interrupt service routine high byte
REG_STOP_LOW            = $0328
REG_STOP_HIGH           = $0329
REG_SCREENCTL_1         = $d011              ; screen control register #1
REG_RASTERLINE          = $d012              ; raster line position 
REG_INTCONTROL          = $d01a              ; interrupt control register
REG_BORCOLOUR           = $d020              ; border colour register
REG_SID                 = $d400
REG_SID_VOLUME          = $d418              ; sid volume & filter register
REG_INTSTATUS_1         = $dc0d              ; interrupt control and status register #1
REG_INTSTATUS_2         = $dd0d              ; interrupt control and status register #2


                        ; kernal routines
K_SETLFS                = $ffba
K_SETNAME               = $ffbd
K_CLOSE_FILE            = $ffc3
K_CLOSE_CHANNEL         = $ffcc
K_LOAD_FILE             = $ffd5

RELEASE = 1
                        ; constants
C_PLACEHOLDER           = $ffff

;                        BasicUpstart2(start)
*=$8000

                        ; program start
                        ;*= $080D "Main"                 ; begin (2049)

start:
                        ldx #$00
-                       lda loader_part,x
                        sta load_target,x
                        lda loader_part+$100,x
                        sta load_target+$100,x
                        lda loader_part+$200,x
                        sta load_target+$200,x
                        lda loader_part+$300,x
                        sta load_target+$300,x
                        lda loader_part+$400,x
                        sta load_target+$400,x
                        inx
                        bne -

                        jsr install
                        dec $D020
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

                        ldy #50
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
                        jsr loadcompd

;                        inc set_filename_low + 1
;                        inc set_filename_high + 1

                        inc loader_part_index


                        lda #<forever                   ; restore eternal loop
                        sta forever + 1
                        lda #>forever
                        sta forever + 2

                        rts


                        ; loader interrupt ---------------------------------------------]
                        ; --------------------------------------------------------------]

loader_interrupt:
                        jsr latch
c1:                     lda #$06
                        sta $d020
                        sta $d021

                        lda #<widebot
                        ldx #>widebot
                        sta $0314
                        stx $0315

                        lda #250
                        sta $d012

                        asl $d019
                        jmp $ea81

widebot:                jsr latch
                        lda #$0e
                        sta $d020

                        lda #<loader_interrupt
                        ldx #>loader_interrupt
                        sta $0314
                        stx $0315

                        ldy #50
                        sty $d012

                        asl $d019
                        jmp $ea31

latch:                  ldx #02
                        dex
                        bne *-1
                        rts


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

*=$8200
install_part:
!bin "install-c64.prg",,2

loader_part:
!bin "loader-c64.prg",,2

