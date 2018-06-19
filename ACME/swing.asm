;Logo swing routine

swingpointer = $02
swingstore = $03
                       * = $0810
                       sei
                       jsr $ff81 ;Init screen
                       lda #$18
                       sta $d018 ;Logo font at $2000
                       lda #$0e
                       sta $d020
                       sta $d021

;Fill the whole screen colours to cyan multicolour as we are using
;the most popular blue scheme. 

            ldx #$00
shadeitcyan lda #$0b
            sta $d800,x
            sta $d900,x
            sta $da00,x
            sta $dae8,x
            inx
            bne shadeitcyan
            lda #$06
            ldx #$0e
            sta $d022
            stx $d023

;Now for the IRQ init routine


            lda #<irq
            ldx #>irq
            sta $0314
            stx $0315
            lda #$31
            ldx #$7f
            sta $d012
            stx $dc0d
            lda #$1b
            sta $d011
            lda #$01
            sta $d01a
            cli
loop        lda #$00
            sta $06
synchronize cmp $06
            beq synchronize
            jsr logoswing
            jmp loop

;Main IRQ routine for the logo swing

irq         inc $d019
            lda #$32
            sta $d012
            lda swingstore
            sta $d016
            jmp $ea7e

logoswing   lda #$ff
            sec
            ldx swingpointer
            sbc $3800,x ;Value of sinus (Which I made in Bonzai's sinus calculator)
            lsr
            lsr
            lsr
            tax
            ldy #$00
screen      lda $3000+0 ;Matrix to read from
            sta $0400,y ;Screen loc. to write to
            lda $3000+64,x
            sta $0400+40,y
            lda $3000+64*2,x
            sta $0400+80,y
            lda $3000+64*3,x
            sta $0400+120,y
            lda $3000+64*4,x
            sta $0400+160,y
            lda $3000+64*5,x
            sta $0400+200,y
            lda $3000+64*6,x
            sta $0400+240,y
            inx           ;Read X 256 times
            iny
            cpy #$27      ;Read y 39 times
            bne screen
            ldx swingpointer 
            lda $3800,x
            and #$07
            ora #$d0 ;Kill this if your logo is not multicolour
            sta swingstore ;The value to put into $D016
            inc swingpointer ;Add 1 to the swing pointer
            rts

;Binaries for the source.

            * = $2000
            !bin "aeg_collection_09.64c",,2
;            * = $3000
;            !binary "logomatrix.prg",,2
;            * = $3800
;            !binary "sinusdata.prg",,2
