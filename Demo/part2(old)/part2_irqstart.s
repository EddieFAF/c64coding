// ************************************************************************** //
// ** IRQ starter
// ************************************************************************** //
.namespace starter {

irq1:
      inc VIC2InteruptStatus     // acknowledge interrupt
      lda #$19
      sta $d018
      lda #$1b
      sta $d011
      lda #$31
      cmp $d012
      bne *-3
      ldx #$0B
      dex
      bne *-1
c10:  lda #$00
      sta VIC2BorderColour
      sta VIC2ScreenColour
      ldx #$09            // delay
      dex
      bne *-1
c11:  lda #$00
      sta VIC2ScreenColour
      sta VIC2BorderColour
      lda #$6F
      ldx #<irq2
      ldy #>irq2
      .if (RELEASE==true){ jmp C_APPLY_INTERRUPT } else { jmp APPLY_INTERRUPT }

irq2:
      inc VIC2InteruptStatus     // acknowledge interrupt
      ldx #$03
      dex
      bne *-1
c20:  lda #$00
      sta $d020
      sta $d021
      lda #$8F
      ldx #<irq3
      ldy #>irq3
      .if (RELEASE==true){ jmp C_APPLY_INTERRUPT } else { jmp APPLY_INTERRUPT }

irq3:
      inc VIC2InteruptStatus     // acknowledge interrupt
      ldx #$03
      dex
      bne *-1
c30:  lda #$00
      sta $d020
      sta $d021

      jsr music.play
subroutine:
      jsr fade_in_all
      ldx counter
      cpx #$40          // delay
      bne update_irq
      lda #$D0
      ldx #<main.irq3b
      ldy #>main.irq3b
      .if (RELEASE==true){ jmp C_APPLY_INTERRUPT } else { jmp APPLY_INTERRUPT }

update_irq:
      lda #$D0
      ldx #<irq3b
      ldy #>irq3b
      .if (RELEASE==true){ jmp C_APPLY_INTERRUPT } else { jmp APPLY_INTERRUPT }

irq3b:
      inc VIC2InteruptStatus     // acknowledge interrupt
      lda #$D1
      cmp $D012
      bne *-3
      ldx #$0b
      dex
      bne *-1
      lda #$15
      sta $d018
c40:  lda #$00
      sta $d020
      sta $d021
      lda #$F2
      cmp $D012
      bne *-3
      ldx #$0B
      dex
      bne *-1
c41:  lda #$00
      sta $D020
      sta $D021

      lda #$F6
      ldx #<irq4
      ldy #>irq4
      .if (RELEASE==true){ jmp C_APPLY_INTERRUPT } else { jmp APPLY_INTERRUPT }

irq4:
      inc VIC2InteruptStatus     // acknowledge interrupt
      lda #$F8
      cmp $d012
      bne *-3
      ldx #$0B
      dex
      bne *-1
c50:  lda #$00
      sta VIC2BorderColour
      sta VIC2ScreenColour
      ldx #$09            // delay
      dex
      bne *-1
c51:  lda #$00
      sta VIC2ScreenColour
      sta VIC2BorderColour

      lda #$2C
      ldx #<irq1
      ldy #>irq1
      .if (RELEASE==true){ jmp C_APPLY_INTERRUPT } else { jmp APPLY_INTERRUPT }

countdown:
      ldx counter
      inx
      stx counter
      stx $0618
      rts

counter: .byte $00

fade_in_all:
      ldx delay
      inx
      stx delay
      cpx #$08
      bne !+
      jmp fade_in
!:    rts

// **** fade in ** **
fade_in:
        ldx #$00
        stx delay
        ldx fade_count
        inx
        cpx #$10
        bne !+
        lda #<countdown
        sta subroutine+1
        lda #>countdown
        sta subroutine+2
        ldx #$00
!:      stx fade_count
        
        lda original_C10 //hole zielfarbe
        asl //und schiebe ins high nibble
        asl
        asl
        asl
        sta fade_in_offset //setze offset
        lda c10+1 //hole aktuelle farbe
        and #$0f //und isoliere low nibble (muss gemacht werden sonst kommt mist raus)
        ora fade_in_offset // "addiere" offset
        //akku zeigt jetzt auf tabelle targetcolor*16+currentcolor
        //also auf die richtige zeile und spalte in der tabelle!
        tax //akku nach X
        lda fade_table,x //hole fade value
        sta c10+1

        lda original_C11
        asl
        asl
        asl
        asl
        sta fade_in_offset 
        lda c11+1 
        and #$0f
        ora fade_in_offset
        tax
        lda fade_table,x
        sta c11+1

        lda original_C20
        asl
        asl
        asl
        asl
        sta fade_in_offset 
        lda c20+1
        and #$0f
        ora fade_in_offset
        tax
        lda fade_table,x
        sta c20+1

        lda original_C30
        asl
        asl
        asl
        asl
        sta fade_in_offset 
        lda c30+1
        and #$0f
        ora fade_in_offset
        tax
        lda fade_table,x
        sta c30+1

        lda original_C40
        asl
        asl
        asl
        asl
        sta fade_in_offset 
        lda c40+1
        and #$0f
        ora fade_in_offset
        tax
        lda fade_table,x
        sta c40+1

        lda original_C41
        asl
        asl
        asl
        asl
        sta fade_in_offset 
        lda c41+1
        and #$0f
        ora fade_in_offset
        tax
        lda fade_table,x
        sta c41+1

        lda original_C50
        asl
        asl
        asl
        asl
        sta fade_in_offset 
        lda c50+1
        and #$0f
        ora fade_in_offset
        tax
        lda fade_table,x
        sta c50+1

        lda original_C51
        asl
        asl
        asl
        asl
        sta fade_in_offset 
        lda c51+1
        and #$0f
        ora fade_in_offset
        tax
        lda fade_table,x
        sta c51+1

rt:     rts

// ----

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
fade_in_offset:
    .byte 0
fade_in_offset_low: .byte $00
delay: .byte $00
fade_count: .byte $00

original_C10: .byte $01
original_C11: .byte $00
original_C20: .byte $0e
original_C30: .byte $00
original_C40: .byte $08
original_C41: .byte $00
original_C50: .byte $01
original_C51: .byte $00

}
