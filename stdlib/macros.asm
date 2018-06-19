; Macros für ACME

!macro SetBorderColor .color {
  lda #.color
  sta $d020
}

!macro SetBackgroundColor .color {
  lda #.color
  sta $d021
}

!macro SetMultiColor1 .color {
  lda #.color
  sta $d022
}

!macro SetMultiColor2 .color {
  lda #.color
  sta $d023
}

!macro SetMultiColorMode {
  lda $d016
  ora #16
  sta $d016 
}

!macro SetScrollMode {
  lda $D016
  eor #%00001000
  sta $D016
}

; Set VIC banks
; $DD00 = %xxxxxx11 -> bank0: $0000-$3fff
; $DD00 = %xxxxxx10 -> bank1: $4000-$7fff
; $DD00 = %xxxxxx01 -> bank2: $8000-$bfff
; $DD00 = %xxxxxx00 -> bank3: $c000-$ffff
!macro SetVICBank0 {
  lda $DD00
  and #%11111100
  ora #%00000011
  sta $DD00
}

!macro SetVICBank1 {
  lda $DD00
  and #%11111100
  ora #%00000010
  sta $DD00
}

!macro SetVICBank2 {
  lda $DD00
  and #%11111100
  ora #%00000001
  sta $DD00
}

!macro SetVICBank3 {
  lda $DD00
  and #%11111100
  ora #%00000000
  sta $DD00
}

; Clear Color RAM
!macro ClearColorRam .clearByte {
  lda #.clearByte
  ldx #0
-
  sta $D800, x
  sta $D800 + $100, x
  sta $D800 + $200, x
  sta $D800 + $300, x
  inx
  bne -
}

; Clear Screen
!macro ClearScreen .screen, .clearByte {
  lda #.clearByte
  ldx #0
-
  sta .screen, x
  sta .screen + $100, x
  sta .screen + $200, x
  sta .screen + $300, x
  inx
  bne -
}

; Set screen and char location
!macro SetScreenAndCharLocation .screen, .charset {
  lda #((.screen & $3FFF) / 64) | ((.charset & $3FFF) / 1024)
  sta $D018
}

; IRQ Enter
!macro ENTER {
    pha
    tya
    pha
    txa
    pha
}
 
; IRQ Exit (Vector, Zeile)
!macro EXIT .intvector, .rasterline {
    ldx #>.intvector
    ldy #<.intvector
    stx $FFFF
    sty $FFFE
    lda #.rasterline
    sta $D012
    sec
    rol $D019
    jmp _quitirq
}
 
!macro POKE .value, .address{
    lda .value
    sta .address
}
 
!macro XDEL .pausex{
    ldx #.pausex
_xxpause dex
    bne _xxpause
}
 
!macro YDEL .pausey{
    ldy #.pausey
_pause2 dey
    bne _pause2
}

!macro irqEnd .raster, .irq {
    lda #.raster
    sta $d012
    lda #<.irq
    sta $0314
    lda #>.irq
    sta $0315
}

; Variablen fehlen
;   Standard   Text    Mode    (ECM, BMM, MCM = 0, 0, 0)
!macro Standart_Text_Mode {
        lda $d011
        and #%10111111
        and #%11011111
        sta $d011
        lda $d016
        and #%11101111
        sta $d016
}

!macro Screen_Ram  adr {
        lda VMCSB
        and #VIC_VideoMask
        ora #(adr >> 10) <<4
        sta VMCSB   
}

; screenChars  = $0400 ; the 40x25 buffer
; screenPixels = $1000 ; the pixel data for font or bitmap ($1000 or $9000 are always charrom)

!macro SetVIC .screenChars, .screenPixels {
; Select VIC bank
 lda # ((.screenChars ^ $ffff) >> 14)
 sta $dd00

; Set VIC screen and font pointers
 lda # (((.screenChars & $3fff) / $0400) << 4) + (((.screenPixels & $3fff) / $0800) << 1)
 sta $d018
 
}
 