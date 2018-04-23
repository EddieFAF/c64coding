; -------------------------------------------------------------------
; example code for usage of exomizer decrunch routine with ACME
; by Spider Jerusalem, 2012
;
; binaries were mempacked with exomizer without loadadress
; exomizer mem -l none -o OUTPUTFILE.bin INPUTFILE.prg
;
; it shows two pictures in standard koala-painter format
; that are both alternately decrunched to $6000-$8711
; (crunched binaries remain in memory)
; and plays a tune that is decrunched to $8800 in an irq
;
; KOALA pictures "Fyrebyrd" and "Library" by Wayne Schmidt
; crappy SID tune "Instant New" by Spider Jerusalem
;
; ATTENTION:
; YOU PROBABLY HAVE TO CHANGE INCLUDE-PATHS IF YOU DON'T USE PROPER OS!
; EVEN BETTER: SWITCH TO PROPER OS!
; -------------------------------------------------------------------
  !to "exod-example.prg",cbm

RELEASE = 1
; -------------------------------------------------------------------
  *= $0801
 !byte $0c, $08, $00, $00
 !byte $9e, $20, $32, $30, $36, $34, $00, $00, $00
; -------------------------------------------------------------------
  *= $0810
  jmp start
; -------------------------------------------------------------------
; BINARY AND SOURCE INCLUDES
; -------------------------------------------------------------------
exod_addr:                ; include exodecrunch code and wrapper
  !src "../wrap.asm"
  !src "../exodecrunch.asm"

pic01_start:              ; include mempacked picture binaries
  !bin "./pic01-mempacked.bin"
pic01_end:

pic02_start:
  !bin "./pic02-mempacked.bin"
pic02_end:

music_start:              ; include mempacked music binary
  !bin "./music-mempacked.bin"
music_end:
; -------------------------------------------------------------------
; INIT VIC
; -------------------------------------------------------------------
start:
  lda #$00
  sta $d020
  sta $d021               ; go play a game if you don't know what this does!
  
  lda #$8b
  sta $d011               ; blank screen
  
  jsr init_music
  
  lda #$02
  sta $dd00               ; vic-bank $4000 - $7fff  
  
  lda #$78
  sta $d018               ; vidmem at $5c00, bitmap at $6000
; -------------------------------------------------------------------
; MAINLOOP
; -------------------------------------------------------------------  
show_first_pic:  
  lda #<pic01_end
  sta opbase+1
  lda #>pic01_end
  sta opbase+2            ; prepare exodecrunch for pic 01
  
  jsr exod_addr           ; decrunch first picture
  
  jsr show_koala
  
  jsr wait_key

  lda #$ab
  sta $d011               ; blank screen

show_second_pic:
  lda #<pic02_end
  sta opbase+1
  lda #>pic02_end
  sta opbase+2            ; prepare exodecrunch for pic 02
  
  jsr exod_addr           ; decrunch second picture
  
  jsr show_koala
  
  jsr wait_key

  lda #$ab
  sta $d011               ; blank screen
  
  jmp show_first_pic

; -------------------------------------------------------------------
; DISPLAY A KOALA PICTURE LOCATED AT $6000
; -------------------------------------------------------------------
show_koala:
  ldx #$fa
-
  lda $7f40-1,x
  sta $5c00-1,x
  lda $7f40+$fa-1,x
  sta $5c00+$fa-1,x
  lda $7f40+(2*$fa)-1,x
  sta $5c00+(2*$fa)-1,x
  lda $7f40+(3*$fa)-1,x
  sta $5c00+(3*$fa)-1,x   ; copy screen data into vidmem

  lda $8328-1,x
  sta $d800-1,x
  lda $8328+$fa-1,x
  sta $d800+$fa-1,x
  lda $8328+(2*$fa)-1,x
  sta $d800+(2*$fa)-1,x
  lda $8328+(3*$fa)-1,x
  sta $d800+(3*$fa)-1,x   ; copy color data into colram
  
  dex
  bne -
  
  lda $8710
  sta $d021
  
  lda #$18
  sta $d016               ; multicolor mode
  
  lda #$bb
  sta $d011               ; bitmap mode
  rts
; -------------------------------------------------------------------
; WAIT FOR KEYPRESS VIA KEYBOARD BUFFER
; -------------------------------------------------------------------
wait_key:
  lda #$00
  sta $c6
-
  lda $c6
  beq -
  rts
; -------------------------------------------------------------------
; INIT MUSIC
; -------------------------------------------------------------------
init_music:
  lda #<music_end
  sta opbase+1
  lda #>music_end
  sta opbase+2            ; prepare exodecrunch for music
  
  jsr exod_addr           ; decrunch music
  
  lda #$00
  tay
  tax
  jsr $8800               ; initialize music player
  
  sei
  
  lda #$7f
  sta $dc0d
  sta $dd0d
  
  lda $dc0d
  lda $dd0d
  
  inx
  stx $d01a               ; setup rasterirq for playing music
  stx $d012
  
  lda #<irq
  sta $0314
  lda #>irq
  sta $0315
  
  cli
  rts
; -------------------------------------------------------------------
; IRQ TO PLAY MUSIC
; -------------------------------------------------------------------
irq:
  lda #$ff
  sta $d019               ; ack irq

  inc $d020
  jsr $8803               ; call music player routine
  dec $d020
  
  jmp $ea31               ; jmp to rom irq routine
