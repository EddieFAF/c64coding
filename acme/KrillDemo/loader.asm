RELEASE = 1
STARTZEILE              = 10

!src "loadersymbols-c64.inc"
!src "stdlib/stdlib.a"

      *= $0801
!byte $0c,$08,$0a,$00,$9e,$32,$30,$36,$34,$00,$00,$00,$00

*=$0810
        ; Initialise IRQ loader and setup IRQ interrupt
        ; routine:
        jsr init

        ; Load Koala Painter file named "AA" from disk:
        clc
        ; Vector pointing to a string containing loaded
        ; file name:
        ldx #<file1
        ldy #>file1

        ; Load file to a memory address of $A000:
        jsr loadcompd

        ; Loop over:
		
		jsr koala
forever:
        jmp forever

file1:  !text "01"
        !byte $00

file2:  !text "02"
        !byte $00



koala     ; Copy screen and colours data into $4400 and $d800:
          ldx #$00
          lda $7f40,x
          sta $4400,x
          lda $8040,x
          sta $4500,x
          lda $8140,x
          sta $4600,x
          lda $8240,x
          sta $4700,x
          lda $8328,x
          sta $d800,x
          lda $8428,x
          sta $d900,x
          lda $8528,x
          sta $da00,x
          lda $8628,x
          sta $db00,x
          inx
          bne *-49

          ; Display bitmap at $6000, where it has been previously decrunched:
          lda $dd00
          and #$fc
          ora #$02
          sta $dd00
          lda #$18
          sta $d016
          lda #$18
          sta $d018
          lda #$3b
          sta $d011

          rts
		  
init:   ; Setup background and border colours:
        lda #$00
        sta $d020
        sta $d021

        ; Blank screen to border colour:
        lda #$00
        sta $d011

        ; Call loader installation routine:
        jsr install

        ; Initialise the soundtrack:
        lda #$00
        jsr $1000

        ; Configure IRQ interrupts:
        sei
        lda #$36 ; switch off the BASIC ROM
        sta $01
        lda #<loader_interrupt
        sta $0314
        lda #>loader_interrupt
        sta $0315
        lda #$00
        sta $d012
        lda #$01
        sta $d019
        lda #$81
        sta $d01a
        lda #$7b
        sta $dc0d
        lda #$00
        sta $dc0e
        cli

        rts

        ; apply interrupt routine -------------------------------------------]
        ; -------------------------------------------------------------------]
apply_interrupt:
        sta VIC2Raster
        stx IRQServiceRoutineLo
        sty IRQServiceRoutineHi
        jmp $ea81

        ; exit part routine -------------------------------------------------]
        ; -------------------------------------------------------------------]
exit_intro_part:
        lda #<clean_current_part        ; reset main loop to load next part
        sta forever + 1
        lda #>clean_current_part
        sta forever + 2

        lda #000                        ; switch off interrupts
        sta CIA1InterruptControl
        sta CIA2InterruptControl

        lda #240                        ; disable raster interrupts
        sta VIC2InteruptControl

        ldx #$31                        ; restore interrupt pointer
        ldy #$ea
        jmp apply_interrupt


        ; clean up routine --------------------------------------------------]
        ; -------------------------------------------------------------------]
clean_up_intro_part:
        sei

        jsr $fda3

        ldy #31                        ; restore irq registers
irq_reset:
        lda $fd30,y
        sta $0314,y
        dey
        bpl irq_reset

        lda #3                        ; restore input / output device numbers
        sta ZPCurrentOutputDevice
        lda #000
        sta ZPCurrentInputDevice

        ldx #$1f                        ; restore sprites
sprite_reset:
        lda $ecb8,x
        sta $cfff,x
        dex
        bne sprite_reset

        jsr $e51b
        jsr $ff5e

        ldy #32                        ; restore sid chip
sid_reset:
        lda #31
        sta SID,y
        lda #000
        sta SID,y
        dey
        bpl sid_reset
        lda #15
        sta SIDVolumeFilter

        jsr $e544

        cli

        rts

        ; sync new part -----------------------------------------------------]
        ; -------------------------------------------------------------------]
sync_part:
        sei
        lda #$7f
        sta CIA1InterruptControl        ; turn off the CIA interrupts
        sta CIA2InterruptControl
        and VIC2ScreenControlV          ; clear high bit of raster line
        sta VIC2ScreenControlV

        ldy #000
        sty VIC2Raster

        lda #$00                       ; load interrupt address
        ldx #$0a
        sta IRQServiceRoutineLo
        stx IRQServiceRoutineHi

        lda #$01                        ; enable raster interrupts
        sta VIC2InteruptControl
        cli

        rts

        ; loader interrupt --------------------------------------------------]
        ; -------------------------------------------------------------------]

loader_interrupt:
        lda #STARTZEILE*8+49
        cmp $d012
        bne *-3
        ldx #$0A
        dex
        bne *-1
c1:     lda #$00
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

irq2:   lda #(STARTZEILE+5)*8+50
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


loader:
         *= $0B00
!bin "loader-c64.prg",,2

;         *= $0e00
;!src "wrap.asm"
;!src "exodecrunch.asm"

;music:
;        *= $1000
;!bin "Nightshift.sid",,$7e

         *= $2000
!bin "install-c64.prg",,2

