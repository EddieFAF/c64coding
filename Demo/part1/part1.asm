
!cpu 6510
!sl "part1.lst"

!initmem $00

!src "../stdlib/macros.asm" ; Stellt ein paar Macros bereit
!src "../stdlib/stdlib.a" ; Sprungziele

                        ; constants 
C_SCREEN_RAM            = $0400
C_UNPACK_ROUTINE        = $0810
C_UNPACK_DEST           = $0824
C_UNPACK_SOURCE         = $0834
C_APPLY_INTERRUPT       = $0840
C_EXIT_PART             = $084C
C_COLOUR_RAM            = $d800

;*= $0800
;!byte $00,$0c,$08,$0a,$00,$9e,$32,$33,$30,$34,$00,$00,$00,$00

        * = $0A00

sync_intro  lda #$15            ; Kleinbuchstaben
            sta VIC2MemorySetup
            jsr $1000           ; Sound Init
            jsr gfx             ; Grafik vorbereiten

            lda #$2C
            ldx #<irq1
            ldy #>irq1
            jmp C_APPLY_INTERRUPT


; ************************************************
; ** IRQ Routinen                               **
; ************************************************
irq1
            inc VIC2InteruptStatus     ; acknowledge interrupt
            lda #%00111011
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
            lda #$6F
            ldx #<irq1b
            ldy #>irq1b
            jmp C_APPLY_INTERRUPT

; Ende Grafikmodus
irq1b
            inc VIC2InteruptStatus     ; acknowledge interrupt
            lda #$71
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

            lda #$7f               ; detect space bar 
            sta CIA1KeyboardColumnJoystickA
            lda CIA1KeyboardRowsJoystickB
            and #$10
            bne update_irq
            jmp C_EXIT_PART

update_irq  lda #$A0
            ldx #<irq2
            ldy #>irq2
            jmp C_APPLY_INTERRUPT

irq2
            inc VIC2InteruptStatus     ; acknowledge interrupt
            lda #$a2
            cmp $d012
            bne *-3
            inc $d020
            jsr $1003                   ; Sound Play
            dec $d020
            lda #$F0
            ldx #<irq3
            ldy #>irq3
            jmp C_APPLY_INTERRUPT

irq3
            inc VIC2InteruptStatus     ; acknowledge interrupt
            lda #$F9
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
            lda #$2F
            ldx #<irq1
            ldy #>irq1
            jmp C_APPLY_INTERRUPT

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
