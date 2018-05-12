.filenamespace part1a

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
    lda #$0e
    sta VIC2BorderColour
c11:
    lda #$06
    sta VIC2ScreenColour
    ldx #$0A            // delay
    dex
    bne *-1
c20:
    lda #$0e
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
    lda #$0e
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

    lda part_done
    cmp #$01
    bne !+
    lda #$2F
    ldx #<part1b.irq1_pre
    ldy #>part1b.irq1_pre
    .if (RELEASE==true){ jmp C_APPLY_INTERRUPT } else { jmp APPLY_INTERRUPT }

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
    cpx #delay_char
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
    cpx #speed_char // nur jeden zweiten Aufruf ausführen, sonst zu schnell
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

// ************************************************************************** //
borderfade_out:
    ldx charfade_delay
    inx
    cpx #speed_border // noch langsamer, jeder 8te Durchlauf
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
    jsr part1b.gfx
    ldx #<sub_rts
    ldy #>sub_rts
    stx subroutine+1
    sty subroutine+2
    lda #$01
    sta part_done

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

    lda c11+1
    and #$0f
    ora charfade_offset
    tax
    lda fade_table,x
    sta c11+1
    sta c21+1
    sta c31+1
    sta c41+1

    rts

border_table:
    .byte $00, $03, $01, $03, $0e, $04, $06, $00
    .byte $00, $00

charfade_delay: .byte $00
charfade_count: .byte $00
charfade_offset: .byte 0
