
!cpu 6510
!sl "part1.lst"

!initmem $00

!src "../../stdlib/macros.asm" ; Stellt ein paar Macros bereit
!src "../../stdlib/stdlib.a" ; Sprungziele

*= $0800
!byte $00,$0c,$08,$0a,$00,$9e,$32,$33,$30,$34,$00,$00,$00,$00

        * = $0900

start       lda #$15            ; Kleinbuchstaben
            sta VIC2MemorySetup
            jsr $1000           ; Sound Init
            jsr gfx             ; Grafik vorbereiten

        ; register first interrupt
            sei
            lda #$7f
            sta CIA1InterruptControl ; turn off the CIA interrupts
            sta CIA1InterruptControl
            and VIC2ScreenControlV ; clear high bit of raster line
            sta VIC2ScreenControlV
            +irqEnd $2C, irq1
            lda #$01            ; enable raster interrupts
            sta VIC2InteruptControl
            cli
            jmp *

; ************************************************
; ** IRQ Routinen                               **
; ************************************************
irq1        lda #%00111011
            sta VIC2ScreenControlV
            lda #%10000000
            sta VIC2MemorySetup    ; $D018 Speicherbereiche
            lda #%00011000
            sta VIC2ScreenControlH ; $D016
            lda #%00000010
            sta $dd00

            lda #$31
            cmp $d012
            bne *-3
            ldx #$0A
            dex
            bne *-1
            lda #$01
            sta VIC2BorderColour
            lda #$01
            sta VIC2ScreenColour
            ldx #$09            ; delay
            dex
            bne *-1
            lda #$00
            sta VIC2ScreenColour
            lda #$00
            sta VIC2BorderColour
            +irqEnd $6f, irq1b
            inc VIC2InteruptStatus     ; acknowledge interrupt
            jmp $ea31

; Ende Grafikmodus
irq1b       lda #$71
            cmp $d012
            bne *-3
            ldx #$16
            dex
            bne *-1
            lda #%00000011
            sta $dd00
            lda #$1b
            sta VIC2ScreenControlV ; Grafikmodus
            lda #$15
            sta VIC2MemorySetup    ; $D018 Speicherbereiche
            lda #%00001000
            sta VIC2ScreenControlH ; $D016
            lda #$00
            sta VIC2ScreenColour
            +irqEnd $A0, irq2
            inc VIC2InteruptStatus     ; acknowledge interrupt
            jmp $ea31

irq2        lda #$a2
            cmp $d012
            bne *-3
            inc $d020
            jsr $1003
            dec $d020
            +irqEnd $F0, irq3
            inc VIC2InteruptStatus     ; acknowledge interrupt
            jmp $ea31

irq3        lda #$F9
            cmp $d012
            bne *-3
            ldx #$0A            ; delay
            dex
            bne *-1
            lda #$01
            sta VIC2BorderColour
            lda #$01
            sta VIC2ScreenColour
            ldx #$09            ; delay
            dex
            bne *-1
            nop
            nop
            lda #$06
            sta VIC2ScreenColour
            lda #$0e
            sta VIC2BorderColour
            +irqEnd $2C, irq1
            inc VIC2InteruptStatus     ; acknowledge interrupt
            jmp $ea31

; Grafik vorbereiten
gfx         ldx #$00
-           lda colora,x
            sta $6000,x
            lda colora+$40,x
            sta $6000+$40,x
            lda colorb,x
            sta $d800,x
            lda colorb+$40,x
            sta $d800+$40,x
            inx
            bne -
            rts

            *=$1000
            !bin "ode_to_64.bin"

            *=$4000
            !src "gfx.txt"
