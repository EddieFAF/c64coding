; Erste Versuche mit Zeichensatz

      *= $0801
!byte $0c,$08,$0a,$00,$9e,$32,$30,$36,$34,$00,$00,$00,$00


!src "../stdlib/variables.asm"

CHARZIEL = $3000
;music = LoadSid("Nightshift.sid")    ;<- Here we load the sid file
C_BITMAP_CACHE  = $4000    ; bitmap unpack location
C_SCROLL_LINE = 12
COUNTER = $fa
.musicinit = $1000
.musicplay = $1003

*=$0810
start:
        jsr $e544 ; Bildschirm löschen

        lda #00
        sta $d020
        sta $d021

        ldx #40       ; init colour map
-       lda #01
        sta $d800+C_SCROLL_LINE*40, x
        dex
        bpl -

        ; Charset kopieren nach CHARZIEL
        ldx #$00
-       lda .charset,x
        sta CHARZIEL,x
        lda .charset +$100,x
        sta CHARZIEL+$100,x
        lda .charset +$200,x
        sta CHARZIEL+$200,x
        inx
        bne -

        lda colora
        sta $d020
        lda colora+1
        sta $d021

        ldx #$00
-       lda colora+2,x
        sta $0400,x
        lda colora+$102,x
        sta $0500,x

        lda colorb,x
        sta $d800,x
        lda colorb+$100,x
        sta $d900,x
        inx
        bne -

        jsr .musicinit
        sei           ; set up interrupt
        lda #$7f
        sta $dc0d     ; turn off the CIA interrupts
        sta $dd0d
        and $d011     ; clear high bit of raster line
        sta $d011

        ldy #00       ; trigger on first scan line
        sty $d012

        lda #<irq1    ; load interrupt address
        ldx #>irq1
        sta $0314
        stx $0315

        lda #$01      ; enable raster interrupts
        sta $d01a
        cli
        rts           ; back to BASIC


; BEGIN IRQ Verlauf ;
irq1:
        ; Zeichensatz ausschalten
        lda #$18 ;font
        sta $d018

        lda #$3b
        sta $d011
        lda #$18
        sta $d016

        lda $d016      ; default to no scroll on start of screen
        and #248      ; mask register to maintain higher bits
        sta $d016
        ldy #52      ; irq zeile
        sty $d012
        lda #<irq2    ; load interrupt address
        ldx #>irq2
        sta $0314
        stx $0315
        inc $d019     ; acknowledge interrupt
        jmp $ea31

irq2: ; Zeile 50
        inc $d020
        jsr .musicplay
        dec $d020

        ldy #C_SCROLL_LINE*8+44       ; trigger on last scan line
        sty $d012
        lda #<irq3    ; load interrupt address
        ldx #>irq3
        sta $0314
        stx $0315
        inc $d019     ; acknowledge interrupt
        jmp $ea31

irq3:
        lda #$1b
        sta $d011

        ; Zeichensatz einschalten
        lda #28 ; Zeichensatz bei $3000 einschalten
        sta $d018
        lda $d016     ; grab scroll register
        and #248      ; mask lower 3 bits
        adc .offset    ; apply scroll
        sta $d016

        lda #$8E ; Startzeile
        cmp $d012
        bne *-3
        ldx #$0b ; Delay
        dex
        bne *-1
        lda #$06 ; Farbe setzen
        sta $d020
        sta $d021
        lda #$9E ; Endzeile
        cmp $d012
        bne *-3
        ldx #$0b ; Delay
        dex
        bne *-1
        lda #$00 ; Farbe zurücksetzen
        sta $d020
        sta $d021

        ; Zeichensatz ausschalten
        lda #$15
        sta $d018

        lda $d016      ; default to no scroll on start of screen
        and #248      ; mask register to maintain higher bits
        sta $d016
        lda #$00
        sta $d020
        sta $d021

        jsr scroll

        ldy #00
        sty $d012
        lda #<irq1    ; load interrupt address
        ldx #>irq1
        sta $0314
        stx $0315
        inc $d019     ; acknowledge interrupt
        jmp $ea31

; Scroll Routine
scroll:
        dec .smooth      ; smooth scroll
        bne continue

        dec .offset      ; update scroll
        bpl resetsmooth

;Hard Scroll
        lda #07       ; reset scroll offset
        sta .offset

shiftrow:  ; Alle 39 Zeichen eins nach links setzen
        ldx #00
-       lda $0400+C_SCROLL_LINE*40+1, x
        sta $0400+C_SCROLL_LINE*40, x
        inx
        cpx #39
        bne -

        ldx .nextchar    ; welches Zeichen kommt als nÃ¤chstes
        lda .message, x  ; lesen
        sta $0400+C_SCROLL_LINE*40+39 ; und an das Ende der Zeile schreiben
        inx
        lda .message, x
        cmp #$ff      ; Ende des Textes erreicht?
        bne +
        ldx #00
+       stx .nextchar

resetsmooth:
        ldx #01       ; set smoothing
        stx .smooth

        ldx .offset      ; update colour map
        lda .colours, x
        sta $d800+C_SCROLL_LINE*40
        lda .colours+8, x
        sta $d800+C_SCROLL_LINE*40+1
        lda .colours+16, x
        sta $d800+C_SCROLL_LINE*40+38
        lda .colours+24, x
        sta $d800+C_SCROLL_LINE*40+39
continue:
        rts

; HardScroll Routine
.offset   !byte 07      ; start at 7 for left scroll
.smooth   !byte 01
.nextchar !byte 00


.colours
    !byte 06, 06, 06, 06, 04, 04, 04, 04
    !byte 14, 14, 14, 14, 03, 03, 03, 03
    !byte 03, 03, 03, 03, 14, 14, 14, 14
    !byte 04, 04, 04, 04, 06, 06, 06, 06

.index       !byte 00 ; starting colour index

.message:
    !scr "dies sollte ein beispiel text sein. hier koennte auch ein laengerer text erfolgen   "
    !byte $ff ; Endekennung

      *=$1000
.music_data:
    !bin "Flat_Beat.sid",,$7e

      *=$2000
    !src "eddie_logo_acme.txt"

      *=$1d00
.charset:
    !bin "aeg_collection_03.64c",,2
