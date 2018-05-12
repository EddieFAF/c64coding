
#import "../stdlib/functions.inc" // Stellt ein paar Macros bereit
#import "../stdlib/stdlib_k.a" // Sprungziele
#import "../stdlib/macros.inc"

.import source "../loader/loader.sym"  // Labels des Loaders

                        // constants 
.var C_SCREEN_RAM            = $0400
.var C_APPLY_INTERRUPT       = loader.apply_interrupt // $0840
//.var C_APPLY_INTERRUPT       = APPLY_INTERRUPT // $0840
.var C_EXIT_PART             = loader.exit_intro_part
.var C_COLOUR_RAM            = $d800

.const speed_border          = $04
.const speed_char            = $02
.const speed_screen          = $04
.const delay_char            = $10
.const delay_screen          = $20

.var RELEASE = true

.if(RELEASE==false) { .eval C_EXIT_PART = $fce2 }

.if (RELEASE==false) { BasicUpstart2(part1a.sync_intro) } else { *=$0A00 }

APPLY_INTERRUPT:
.if (RELEASE==false) {
    sta $D012
    stx $0314
    sty $0315
    jmp $ea81
}

.import source "part1_a.s"
.import source "part1_b.s"

fade_table:
    //high nibble=target color
    //low nibble =source color
    //      0   1   2   3   4   5   6   7   8   9   a   b   c   d   e   f
    .byte $00,$0d,$09,$0c,$02,$08,$02,$0f,$02,$00,$08,$09,$04,$03,$04,$05    //0
    .byte $06,$01,$08,$0d,$0c,$03,$0b,$01,$0a,$02,$0f,$04,$03,$01,$03,$07    //1
    .byte $09,$07,$02,$0c,$02,$04,$0b,$0f,$02,$02,$08,$02,$04,$03,$04,$0a    //2
    .byte $06,$0d,$04,$03,$0c,$03,$0b,$0f,$0c,$02,$0c,$04,$03,$03,$03,$03    //3
    .byte $06,$0d,$04,$0c,$04,$0c,$0b,$0f,$04,$02,$04,$04,$04,$03,$04,$0a    //4
    .byte $06,$0d,$08,$05,$0c,$05,$0b,$0f,$0c,$02,$0c,$04,$05,$03,$05,$05    //5
    .byte $06,$0d,$0b,$0e,$0b,$08,$06,$0f,$0b,$0b,$08,$06,$04,$03,$04,$0c    //6
    .byte $09,$07,$08,$0f,$0a,$0f,$0b,$07,$0a,$02,$0f,$04,$0f,$07,$0f,$07    //7
    .byte $09,$07,$08,$0c,$08,$0c,$0b,$0f,$08,$02,$08,$08,$08,$03,$04,$0c    //8
    .byte $09,$07,$09,$0c,$02,$08,$0b,$0f,$02,$09,$08,$09,$08,$03,$04,$0c    //9
    .byte $09,$07,$04,$0c,$0a,$0c,$0b,$0f,$0a,$02,$0a,$04,$0a,$0f,$0c,$0a    //a
    .byte $06,$0d,$0b,$0e,$0b,$08,$0b,$0f,$0b,$0b,$08,$0b,$04,$03,$04,$0c    //b
    .byte $06,$0d,$08,$0c,$0c,$0c,$0b,$0f,$0c,$02,$0c,$04,$0c,$03,$0c,$0c    //c
    .byte $09,$0d,$08,$0d,$0c,$03,$0b,$0d,$0c,$02,$0f,$04,$0f,$0d,$03,$0d    //d
    .byte $06,$0d,$04,$0e,$0c,$0e,$0b,$0f,$0c,$02,$0c,$04,$0e,$03,$0e,$0e    //e
    .byte $06,$0d,$08,$0f,$0c,$0f,$0b,$0f,$0c,$02,$0f,$04,$0f,$0f,$0f,$0f    //f

part_done: .byte $00

    *=$1000
    .import binary "ode_to_64.bin"

    *=$4000
    .import source "gfx_k.txt"
