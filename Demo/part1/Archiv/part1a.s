
#import "../stdlib/functions.inc" // Stellt ein paar Macros bereit
#import "../stdlib/stdlib_k.a" // Sprungziele
#import "../stdlib/macros.inc"

.import source "../loader/loader.sym"  // Labels des Loaders

                        // constants 
.var C_SCREEN_RAM            = $0400
.var C_UNPACK_ROUTINE        = loader.unpack_next // $0810
.var C_UNPACK_DEST           = loader.unpack_literal // $0824
.var C_UNPACK_SOURCE         = loader.unpack_getbyte // $0834
//.var C_APPLY_INTERRUPT       = loader.apply_interrupt // $0840
.var C_APPLY_INTERRUPT       = APPLY_INTERRUPT // $0840
.var C_EXIT_PART             = loader.exit_intro_part // $084C
.var C_COLOUR_RAM            = $d800

.var RELEASE = false

.if (RELEASE==false) { BasicUpstart2(sync_intro) } else { *=$0A00 }

APPLY_INTERRUPT:
.if (RELEASE==false) {
    sta $D012
    stx $0314
    sty $0315
    jmp $ea81
}

sync_intro:
    lda #$15            // Kleinbuchstaben
    sta VIC2MemorySetup
    lda #$00
    jsr $1000           // Sound Init
    // setzen Farbram
    lda #<$D800
    sta $FB
    lda #>$D800
    sta $FC

.if (RELEASE==false) {
    sei
    lda #$7f
    sta $dc0d     // turn off the CIA interrupts
    sta $dd0d
    and $d011     // clear high bit of raster line
    sta $d011
    :irqEnd #$2F:#irq1_pre

    lda #$01      // enable raster interrupts
    sta $d01a
    cli
    jmp *
}

    lda #$2F
    ldx #<irq1_pre
    ldy #>irq1_pre
    .if (RELEASE==true){ jmp C_APPLY_INTERRUPT } else { jmp APPLY_INTERRUPT }


// ************************************************
// ** IRQ Routinen                               **
// ************************************************

irq1_pre:
    inc VIC2InteruptStatus     // acknowledge interrupt
    lda #$1b
    sta VIC2ScreenControlV
    lda #$15 // #%10000000
    sta VIC2MemorySetup    // $D018 Speicherbereiche
    lda #$C8 // #%00011000
    sta VIC2ScreenControlH // $D016
    //lda #$01 //#%00000010
    //sta $dd00

    lda #$31
    cmp $d012
    bne *-3
    ldx #$0A
    dex
    bne *-1
c10:
    lda #$07
    sta VIC2BorderColour
c11:
    lda #$06
    sta VIC2ScreenColour
    ldx #$0A            // delay
    dex
    bne *-1
c20:
    lda #$07
    sta VIC2BorderColour
c21:
    lda #$06
    sta VIC2ScreenColour

subroutine:
    jsr charfade_warte

    jsr $1003

    lda #$F0
    ldx #<irq3_pre
    ldy #>irq3_pre
    .if (RELEASE==true){ jmp C_APPLY_INTERRUPT } else { jmp APPLY_INTERRUPT }

irq3_pre:
    inc VIC2InteruptStatus     // acknowledge interrupt
    lda #$FA
    cmp $d012
    bne *-3
    ldx #$0A            // delay
    dex
    bne *-1
c30:
    lda #$07
    sta VIC2BorderColour
c31:
    lda #$06
    sta VIC2ScreenColour
    ldx #$0A            // delay
    dex
    bne *-1
    nop
    nop
c40:
    lda #$0e
    sta VIC2BorderColour
c41:
    lda #$06
    sta VIC2ScreenColour

!:  lda #$2F
    ldx #<irq1_pre
    ldy #>irq1_pre
    .if (RELEASE==true){ jmp C_APPLY_INTERRUPT } else { jmp APPLY_INTERRUPT }


// -----------------------------------
// Verzögerung, bevor Effekt startet
charfade_warte:
    ldx charfade_delay
    inx
charfade_warte_delay:
    cpx #$40
    bne charfade_warte_update
    //setzen des faders
    ldx #<charfade_out
    ldy #>charfade_out
    stx subroutine+1
    sty subroutine+2
    ldx #$00
charfade_warte_update:
    stx charfade_delay
sub_rts:
    rts

charfade_out:
    ldx charfade_delay
    inx
    cpx #$02 // nur jeden zweiten Aufruf ausführen, sonst zu schnell
    bne !+
    jsr fade_out
    ldx #$00
!:  stx charfade_delay
    rts

fade_out:
    ldx #$00
    stx charfade_delay
    ldx charfade_count
    inx
    cpx #$08
    bne charfade_real
    lda $FB
    clc
    adc #$28
    sta $FB
    bcc !+
    inc $FC
    lda $FC
    // Ende des Bildschirms erreicht?
    cmp #$DC
    beq charfade_done
!:  ldx #$00
    stx charfade_count
    rts

charfade_done:
    jsr $e544
    lda #$00
    sta charfade_delay
    sta charfade_count
    ldx #<borderfade_out
    ldy #>borderfade_out
    stx subroutine+1
    sty subroutine+2
    rts

charfade_real:
    stx charfade_count

    ldy #$00
    lda $d021 //hole zielfarbe
    asl
    asl
    asl
    asl
    sta charfade_offset //setze offset
!:
    // lda $d800,y //hole aktuelle farbe
    // and #$0f //und isoliere low nibble (muss gemacht werden sonst kommt mist raus)
    // ora charfade_offset // "addiere" offset
    // tax //akku nach X
    // lda fade_table,x //hole fade value
    // sta $d800,y

    lda ($FB),y
    and #$0F
    ora charfade_offset
    tax
    lda fade_table,x
    sta ($FB),y

    iny
    cpy #$28 // 40 Zeichen
    bne !-
    rts

borderfade_out:
    ldx charfade_delay
    inx
    cpx #$08 // noch langsamer, jeder 8te Durchlauf
    bne !+
    jsr border_out
    ldx #$00
!:  stx charfade_delay
    rts

border_out:
    ldx #$00
    stx charfade_delay
    ldx charfade_count
    inx
    cpx #$08
    bne !+
    ldx #<sub_rts
    ldy #>sub_rts
    stx subroutine+1
    sty subroutine+2

    ldx #$00
!:  stx charfade_count

    lda #$00
    asl
    asl
    asl
    asl
    sta charfade_offset
    lda c10+1
    and #$0f
    ora charfade_offset
    tax
    lda fade_table,x
    sta c10+1
    sta c20+1
    sta c30+1
    sta c40+1
    rts

border_table:
    .byte $00, $03, $01, $03, $0e, $04, $06, $00
    .byte $00, $00

charfade_delay: .byte $00
charfade_count: .byte $00
charfade_offset: .byte 0

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

    *=$1000
    .import binary "ode_to_64.bin"

