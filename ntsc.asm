// Reliable PAL/NTSC-Detector by Ninja/The Dreams/TempesT
// for Go64!/CW issue 06/2000 (detailed description there)
// This routine can't be fooled like $02a6 and works also with a SCPU

BasicUpstart2(DetectC64Model)

DetectC64Model:

.var nmivec        = $0318                     // NMI-vector

    sei                         // disable interrupts
    ldx nmivec
    ldy nmivec+1                // remember old NMI-vector
    stx nmivect
    sty nmivect+1

    lda #<rtil
    sta nmivec
    lda #>rtil                // let NMI-vector point to
    sta nmivec+1                // a RTI

    // Use CIA #1 Timer B to count cycled in a frame
    lda #$ff
    sta $dc06
    sta $dc07  // Latch #$ffff to Timer B

    bit $d011
    bpl *-3    // Wait untill Raster > 256
    bit $d011
    bmi *-3    // Wait untill Raster = 0

    ldx #%00011001
    stx $dc0f  // Start Timer B (One shot mode (Timer stops automatically when underflow))

    bit $d011
    bpl *-3    // Wait untill Raster > 256
    bit $d011
    bmi *-3    // Wait untill Raster = 0

    sec
    sbc $dc07  // Hibyte number of cycles used
    and #%00000011
    sta VICSTANDARD
    tax
    lda textlo,x
    ldy texthi,x
    jsr $ab1e
    lda nmivect
    sta nmivec
    lda nmivect+1                // let NMI-vector point to
    sta nmivec+1                // a RTI
    cli
    rts
rtil:
    rti                         // go immediately back after
                                // a NMI
nmivect: .word 00
VICSTANDARD: .byte 0
textlo: .byte <pal, <ntsc1, <ntsc2, <paln
texthi: .byte >pal, >ntsc1, >ntsc2, >paln

pal: .text "PAL C64 ERKANNT."
      .byte 00
ntsc1: .text "NTSC"
      .byte 0
ntsc2: .text "NTSC2"
      .byte 00
paln: .text "PAL-N"
      .byte 0

