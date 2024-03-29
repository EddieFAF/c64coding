
; Assembler: ACME
      *= $0801
!byte $0c,$08,$0a,$00,$9e,$32,$30,$36,$34,$00,$00,$00,$00


!src "../demo/lib/acme/stdlib.a"
!src "../demo/lib/acme/macros.asm"

        .Zeilenstart = $6A ; erste Rasterzeile von Bildschirmzeile 08
        .Zeilenende  = $BF ; Letzte Rasterzeile von Bildschirmzeile 18
        C_DELAY   = $04

        ; ----------------------------------------
start:  sei         ; set up interrupt
        jsr $e544
        lda #$00
        sta $d020
        sta $d021
        lda #$7f
        sta $dc0d     ; turn off the CIA interrupts
        sta $dd0d
        and $d011     ; clear high bit of raster line
        sta $d011

        ;Grafik kopieren
        ldx #$00
-       lda .grafik,x
        sta $2000,x
        lda .grafik+$100,x
        sta $2100,x
        lda .grafik+$200,x
        sta $2200,x
        lda .grafik+$300,x
        sta $2300,x
        lda .grafik+$400,x
        sta $2400,x
        lda .grafik+$500,x
        sta $2500,x
        lda .grafik+$600,x
        sta $2600,x
        lda .grafik+$700,x
        sta $2700,x
        lda .grafik+$800,x
        sta $2800,x
        lda .colora,x
        sta $0400,x
        lda .colora+$018,x
        sta $0418,x
        lda .colorb,x
        sta $d800,x
        lda .colorb+$018,x
        sta $d818,x
        inx
        bne -

; Fill Screen with text
        ldx #$00
-       lda #$00
        sta $d800+8*40,x
        sta $d868+8*40,x
        lda .screen_text,x
        sta $0400+8*40,x
        lda .screen_text+$68,x
        sta $0468+8*40,x
        inx
        bne -

; Fill Screen (Lines)
        ldx #$00
-       lda #$00
        sta $0400+7*40,x ; Zeile 8
        lda #$01
        sta $0400+17*40,x ; Zeile 18
        inx
        cpx #$28
        bne -

        ; Zeilen einmalig füllen
        jsr .copy

        +irqEnd $00,.irq1

        ldx #$08
        stx .coldelay
        lda #$01      ; enable raster interrupts
        sta $d01a
        cli
        jmp *
        rts         ; back to BASIC

; =====================================================
; = Start Intro
; =====================================================

; BEGIN IRQ Verlauf

; Zeichensatz ausschalten
.irq1:  lda #$16 ;font
        sta $d018
        lda #$1b
        sta $d011
        lda #$C8 ;#$08
        sta $d016

        +irqEnd $32, .irq2

        inc $d019     ; acknowledge interrupt
        jmp $ea31

; Zeile 48 (Logo)
.irq2:
        lda #$00
        sta $d020
        sta $d021

        lda #$3b ;#$3b
        sta $d011
        ; Zeichensatz einschalten
        lda #$19
        sta $d018
        lda #$18
        sta $d016

        lda #.Zeilenstart-2
        sta $d012
.irq2l:  ldx #<.irq3
.irq2h:  ldy #>.irq3
        stx $0314
        sty $0315
        inc $d019     ; acknowledge interrupt
        jmp $ea31

; ----------------------------------------
; - Rasterlinie oben
; ----------------------------------------
.irq3:
        lda #.Zeilenstart ; Startzeile
        cmp $d012
        bne *-3
        ldx #$0A ; Delay
        dex
        bne *-1
        lda #$C8
        sta $d016
        lda #$1b
        sta $d011
        lda #$1E ; Zeichensatz bei $3800 einschalten
        sta $d018

        lda #.Zeilenstart+2
        cmp $d012
        bne *-3
        ldx #$0A
        dex
        bne *-1
        ldy .colcount
        lda .coltab,y   ; Zeilenanfang
        sta $d020
        sta $d021
        ldx #$0A
        dex
        bne *-1
        lda #$00
        sta $d020
        sta $d021

        lda #.Zeilenstart+8
        cmp $d012
        bne *-3
        lda #$16
        sta $d018

        +irqEnd .Zeilenende-11, .irq4
        inc $d019     ; acknowledge interrupt
        jmp $ea31

; ----------------------------------------
; - Rasterlinie unten
; ----------------------------------------
.irq4:   lda #.Zeilenende-4
        cmp $d012
        bne *-3
        lda #$1E
        sta $d018

        lda #.Zeilenende ; Endzeile
        cmp $d012
        bne *-3
        ldx #$0A ; Delay
        dex
        bne *-1
        ldy .colcount
        lda .coltab,y
        sta $d020
        sta $d021
        ldx #$0A
        dex
        bne *-1
        lda #$00
        sta $d020
        sta $d021

        lda #.Zeilenende+2
        cmp $d012
        bne *-3
        ldx #$0b
        dex
        bne *-1
        ; Zeichensatz ausschalten
        lda #$16
        sta $d018
        lda #$C8
        sta $d016

.sub:    jsr .delayer

        +irqEnd $00, .irq1 ; Letzer IRQ, also wieder auf Anfang setzen
        inc $d019     ; acknowledge interrupt
        jmp $ea31


; ----------------------------------------
; - Farbverlauf
; ----------------------------------------
.col_update: dec .coldelay
        beq .weiter
.ret:    rts

.weiter: ldx #C_DELAY
        stx .coldelay
        ldx .colcount
        inx
        stx .colcount
        cpx #$10
        bne .copy
        lda #<.delayer2
        ldy #>.delayer2
        sta .sub+1
        sty .sub+2

        ; Screen neu füllen
.copy:   ldx .colcount
        ldy #$00
-       lda .coltab+2,x
        sta $d800+7*40,y
        sta $d800+7*40+36,y
        sta $d800+17*40,y
        sta $d800+17*40+36,y
        lda .coltab+3,x
        sta $d800+7*40+4,y
        sta $d800+7*40+32,y
        sta $d800+17*40+4,y
        sta $d800+17*40+32,y
        lda .coltab+4,x
        sta $d800+7*40+8,y
        sta $d800+7*40+28,y
        sta $d800+17*40+8,y
        sta $d800+17*40+28,y
        lda .coltab+5,x
        sta $d800+7*40+12,y
        sta $d800+7*40+24,y
        sta $d800+17*40+12,y
        sta $d800+17*40+24,y
        lda .coltab+6,x
        sta $d800+7*40+16,y
        sta $d800+7*40+20,y
        sta $d800+17*40+16,y
        sta $d800+17*40+20,y
        iny
        cpy #$04
        bne -
        rts

        ; Verzögerung vor Effekt 1 -----------------------------------------]
        ; ------------------------------------------------------------------]
.delayer:
        ldx .count
        dex
        stx .count
        bpl +
        ldx #<.col_update
        ldy #>.col_update
        stx .sub+1
        sty .sub+2
        ldx #$40 ; Wert für Verzögerer 2
        stx .count
+       rts

        ; Verzögerung vor Effekt 2 -----------------------------------------]
        ; ------------------------------------------------------------------]
.delayer2:
        ldx .count
        dex
        stx .count
        bpl +
        ldx #<.screen_fade
        ldy #>.screen_fade
        stx .sub+1
        sty .sub+2
+       rts

.count: !byte $20 ; Verzögerungszähler

        ; Zeilen einblenden ------------------------------------------------]
        ; ------------------------------------------------------------------]
.screen_fade:
        dec .screen_fade_delay
        beq +
        rts

+       ldx #C_DELAY
        stx .screen_fade_delay
        ldy #$00
        ldx .screen_fade_count
-       lda .coltab+8,x
        sta $d800+8*40,y
        lda .coltab+7,x
        sta $d800+9*40,y
        lda .coltab+6,x
        sta $d800+10*40,y
        lda .coltab+5,x
        sta $d800+11*40,y
        lda .coltab+4,x
        sta $d800+12*40,y
        lda .coltab+3,x
        sta $d800+13*40,y
        lda .coltab+2,x
        sta $d800+14*40,y
        lda .coltab+1,x
        sta $d800+15*40,y
        lda .coltab,x
        sta $d800+16*40,y
        iny
        cpy #$28
        bne -
        inx
        cpx #$10
        bne +
        ldx #<.ret
        ldy #>.ret
        stx .sub+1
        sty .sub+2
        ldx #$00
+       stx .screen_fade_count
        rts


.coldelay: !byte C_DELAY
.colcount: !byte 00
.screen_fade_delay: !byte C_DELAY
.screen_fade_count: !byte $00


.coltab: !byte 000, 000, 000, 000, 000, 000, 000, 000
        !byte 000, 000, 006, 004, 014, 003, 001, 001
        !byte 001, 001, 001, 001, 001, 001, 001, 001
        !byte 001, 001, 001, 001, 001, 001, 001, 001

!ct scr
.screen_text: ;!text "                                        "
             !text "1234567890123456789012345678901234567890"
             !text "  Here goes the text in the middle of   "
             !text "     the screen. no more coding...      "
             !text "           w e l c o m e                "
             !text "           w e l c o m e                "
             !text "           w e l c o m e                "
             !text "           w e l c o m e                "
             !text "     the screen. no more coding...      "
             !text "  Here goes the text in the middle of   "
*=$2000
.grafik:
!src "mp_eddie.txt"

; ----- @Charset@ -----
*=$3800
.intro_char: !byte %00000000
            !byte %00000000
            !byte %11111111
            !byte %00000000
            !byte %00000000
            !byte %00000000
            !byte %00000000
            !byte %00000000

            !byte %00000000
            !byte %00000000
            !byte %00000000
            !byte %00000000
            !byte %00000000
            !byte %11111111
            !byte %00000000
            !byte %00000000
