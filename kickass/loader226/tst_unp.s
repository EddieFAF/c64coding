//-------------------------------------------------------------------------------
//Fastloader test with unpacked file
//-------------------------------------------------------------------------------
                #import "cfg_unp.s"
                #import "loader.s"

                *=$0801

                .byte $0b,$08           //Address of next BASIC instruction
                .word 2002              //Line number
                .byte $9e               //SYS-token
                .byte $32,$30,$36,$31   //2061 in ASCII
                .byte $00,$00,$00       //BASIC program end

start:          jsr initloader
                ldx #<filename
                ldy #>filename
                jsr loadfile            //Load file
                bcc ok
                sta $d020               //If error, show errorcode in border
exit:           jsr getin
                tax
                beq exit
                jmp 64738

ok:             lda #$02                //Show the picture we just loaded
                sta $dd00
                lda #59
                sta $d011
                lda #$18
                sta $d016
                lda #$80
                sta $d018
                lda #$00
                sta $d020
                lda #$01
                sta $d021
                ldx #$00
copycolors:     lda $6400,x
                sta $d800,x
                lda $6500,x
                sta $d900,x
                lda $6600,x
                sta $da00,x
                lda $6700,x
                sta $db00,x
                inx
                bne copycolors
                jmp exit

filename:       .text "UNPACKED P*"
                .byte 0

