// Blend charsets

// By Monte Carlos 

// This little routine blends in a charset at $2000 into $2800
// (only the first 64 chars, $0200 bytes), pixel by pixel with random values. 


         *= $1000

         sei
         lda #51
         sta $01
         ldy #$00
a1:      lda $d000,y  //copy 64 chars
         sta $2000,y  //from ROM to $2000
         lda $d100,y
         sta $2100,y
         lda #$00
         sta $2800,y  //clear $2800-$2a00
         sta $2900,y
         iny
         bne a1
         lda #55
         sta $01
         cli

         jmp a2

//---------------------------------------

rand:    inc val
         ldy val
         lda $dc04    //generate random
         eor $fedc,y  //number $00-$ff
         eor $d012
         sta $57      //save it in $57
         rts

val:     .byte 0

//---------------------------------------

a2:      jsr $e518   //initialize screen
         lda #$01    //
         sta $0286   //clear screen, set
         lda #$93    //color white
         jsr $ffd2   //
         ldy #$00
a3:      tya
         sta $0400,y //show chars 0-63
         iny         //on top of screen
         cpy #$40
         bne a3
         lda #26     //switch charset
         sta $d018   //on ($2800)

         lda #$00
         sta $58     //random-counter

//---------------------------------------
//create random list of values from 0-63
//for each bit of a char and save it
//to $03c0-$03ff

a4:      jsr rand    //filter out
         and #$c0    //values 64-255
         bne a4      //and get new random

         lda $58
         bne a5
a7:      ldx $58      //
         lda $57      //accept first/
         sta $03c0,x  //unique value
         inc $58      //for the list
         lda $58      //
         cmp #64      //64 values?
         beq a8       //list finished!
         jmp a4

a5:      ldy #$00     //
a6:      lda $03c0,y  //
         cmp $57      //test if value in
         beq a4       //$57 is unique
         iny          //(beq a4 if not)
         cpy $58      //
         bne a6       //

         jmp a7       //unique value

//---------------------------------------

a8:      ldy #$20
         ldx #$00
         lda #$28  //$fa/$fb points at
         stx $fa   //$2000 (source)
         sty $fb
         stx $fc   //$fc/$fd points at
         sta $fd   //$2800 (target)

         stx $f9   //counter


         ldx #$00

a12:     stx $02

         lda $03c0,x //
         clc         //
         adc $03c0,x //duplicate content
                     //of $03c0,x
         tay         //
                     //
         lda bitpnt,y//and y points at
         sta $5a     //the byte
         iny         //
         lda bitpnt,y//& after increasing
         sta $5b     //at the bit on the
                     //basis of table
                     //bitpnt (see below)


         ldy $5a      //
         ldx $5b      //
         lda ($fa),y  //
         and bits,x   //
         beq a9       //blend pixel in
         lda bits,x   //if bit is on
         clc          //
         adc ($fc),y  //
         sta ($fc),y  //

a9:      lda #$08
         clc
         adc $fa
         sta $fa     //next char (source)
         bcc a10
         inc $fb

a10:     lda #$08
         clc
         adc $fc
         sta $fc     //next char (target)
         bcc a11
         inc $fd

a11:     ldx $02

         inc $f9
         lda $f9
         cmp #64
         bne a12

         lda #$00
         sta $f9

         ldy #$20   //reset pointers
         ldx #$00   //and start with the
         lda #$28   //next pixel
         stx $fa
         sty $fb
         stx $fc
         sta $fd

         ldy #$00
w2:      dey
         ldx #$00
w1:      dex       //little delay
         cpx #$80
         bne w1
         cpy #$e0
         bne w2

         ldx $02

         inx
         cpx #64
         bne a12

         rts


//---------------------------------------

bitpnt:  .byte 0,0,0,1,0,2,0,3,0,4,0,5
         .byte 0,6,0,7
         .byte 1,0,1,1,1,2,1,3,1,4,1,5
         .byte 1,6,1,7
         .byte 2,0,2,1,2,2,2,3,2,4,2,5
         .byte 2,6,2,7
         .byte 3,0,3,1,3,2,3,3,3,4,3,5
         .byte 3,6,3,7
         .byte 4,0,4,1,4,2,4,3,4,4,4,5
         .byte 4,6,4,7
         .byte 5,0,5,1,5,2,5,3,5,4,5,5
         .byte 5,6,5,7
         .byte 6,0,6,1,6,2,6,3,6,4,6,5
         .byte 6,6,6,7
         .byte 7,0,7,1,7,2,7,3,7,4,7,5
         .byte 7,6,7,7

bits:    .byte 128,64,32,16,8,4,2,1
