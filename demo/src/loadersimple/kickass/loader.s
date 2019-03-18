//***********************************************************************
//**  Project Name: Loader (alle Teile eingebunden)                    **
//**  ---------------------------------------------------------------  **
//**  Filename: fullpart.a                                             **
//**  ---------------------------------------------------------------  **
//**  Author (c): Eddie                                                **
//**  File Date: 2018-02-28                                            **
//***********************************************************************
//!initmem $ea        // Speicher vorfuellen
#import "../../../lib/kickass/stdlib_k.a"
#import "../../../lib/kickass/macros.inc"
#import "../../../lib/kickass/functions.inc"

BasicUpstart2(start)

start:

                jsr $e544                               //      Bildschirm loeschen
                SetBorderColor(0)
                SetBackgroundColor(0)

                jsr screeninit

                sei                                     //      IRQs sperren
                lda #$7f
                sta $dc0d                               //      turn off the CIA interrupts
                sta $dd0d
                and $d011                               //      clear high bit of raster line
                sta $d011
                :irqEnd #$71:#irq1

                lda #$01      // enable raster interrupts
                sta $d01a
                cli
                rts

// ** IRQ Routinen **
irq1:
                lda #$78
                cmp $d012
                bne *-3
                ldx #$0B
                dex
                bne *-1
                lda #$0E
                sta $d020
                sta $d021
                ldx #$09
                dex
                bne *-1
                lda #$00
                sta $d020
                sta $d021
                :irqEnd #$81:#irq2
                inc VIC2InteruptStatus      // acknowledge interrupt
                jmp $ea81

irq2:
                lda #$82
                cmp $d012
                bne *-3
                ldx #$0B
                dex
                bne *-1
                lda #$0E
                sta $d020
                sta $d021
                ldx #$03
                dex
                bne *-1
                lda #$00
                sta $d020
                sta $d021
                :irqEnd #$e8:#irq3
                inc VIC2InteruptStatus      // acknowledge interrupt
                jmp $ea81

irq3:
                lda #$ea
                cmp $d012
                bne *-3
                ldx #$0B
                dex
                bne *-1
                lda #$00
                sta $d020
                sta $d021

                jsr colmove

                :irqEnd #$71:#irq1
                inc VIC2InteruptStatus      // acknowledge interrupt
                jmp $ea81

// ** Vorbereitung des Bildschirms **
screeninit:
                ldx #$00
!:              lda text1,x
                sta $0568,x
                lda #$00
                sta $d968,x
                inx
                cpx #$28
                bne !-
                rts

// ** Farbcycling **
colmove:
                ldx count
                inx
                cpx #$08
                beq !+
                stx count
                rts
!:              ldx #$00
                stx count
                ldx pos
                lda col,x
                ldx #$00
!:              sta $d968,x
                inx
                cpx #$28
                bne !-
                ldx pos
                inx
                cpx #$10
                bne !+
                ldx #$00
!:              stx pos
                rts


count:          .byte 00
pos:            .byte 00

text1:
                .text "               loading...               "
text2:
                .text "              depacking..               "

col:
                .byte $00, $06, $0e, $03, $01, $01, $01, $01
                .byte $03, $0e, $06, $00, $00, $00, $00, $00


