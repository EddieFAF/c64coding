; Start address of a loading procedure binary:
loader = $8200

 ; Execution address of a loading procedure:
load = $8200

 ; Start address of an installation procedure binary:
installer = $C300

 ; Execution address of an installation procedure:
install = $91D8

         *= installer
!bin "install-c64.prg",2

         *= loader
!bin "loader-c64.prg",2


        *= $1000
!bin "Nightshift.sid",$7e

         *= $c000
!src "decruncher.src"


init    ; Setup background and border colours:
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
        lda #<irq
        sta $0314
        lda #>irq
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

irq     inc $d020 ; play music and at the same time

        jsr $1003 ; visualize how much raster time is
        dec $d020 ; consumed by a player routine

        inc $d019
        jmp $ea7e



        *= $c100

        ; Initialise IRQ loader and setup IRQ interrupt
        ; routine:
        jsr init

loop    ; Load Koala Painter file named "AA" from disk:
        clc
        ; Vector pointing to a string containing loaded
        ; file name:
        ldx #<file1
        ldy #>file1

        ; Load file to a memory address of $A000:
        jsr load

        ; Decrunch loaded file into $6000:
        ldy #<$a000
        ldx #>$a000
        jsr decrunch

        ; Loop over:
		rts
        jmp loop

file1   !text "01"
        !byte $00

file2   !text "02"
        !byte $00

