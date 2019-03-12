;-------------------------------------------------------------------------------
;Fastloader test with unpacked file
;-------------------------------------------------------------------------------

                processor 6502
                org $0801

                dc.b $0b,$08           ;Address of next BASIC instruction
                dc.w 2018              ;Line number
                dc.b $9e               ;SYS-token
                dc.b $32,$30,$36,$31   ;2061 in ASCII
                dc.b $00,$00,$00       ;BASIC program end

start:          jsr initloader
                jsr initmusicplayback
                ldx #<filename
                ldy #>filename
                jsr loadfile            ;Load file
                bcc ok
                sta $d020               ;If error, show errorcode in border
exit:           jsr getin
                tax
                beq exit
                jmp 64738

ok:             jsr stopmusicplayback
                lda #$02                ;Show the picture we just loaded
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

initmusicplayback:
                sei
                lda #<irq1
                sta $0314
                lda #>irq1
                sta $0315
                lda #$84                         ;Set low bits of raster
                sta $d012                       ;position
                lda $d011
                and #$7f                        ;Set high bit of raster
                sta $d011                       ;position (0)
                lda #$7f                        ;Set timer interrupt off
                sta $dc0d
                lda #$01                        ;Set raster interrupt on
                sta $d01a
                lda $dc0d                       ;Acknowledge timer interrupt
                
                jsr $e544
                ldx #$00
.1              lda text1,x
                sta $0400+11*40,x
                inx
                cpx #$28
                bne .1
                lda #%00010100
                sta $d018
                lda #$00
                jsr $1000
                cli
                rts

stopmusicplayback:
                sei
                lda #<$ea31
                sta $0314
                lda #>$ea31
                sta $0315
                lda #$00
                sta $d01a
                lda #$81
                sta $dc0d
                inc $d019
                lda #$00
                sta $d418
                cli
                rts

irq1:           lda #$8c
                cmp $d012
                bne *-3
                ldx #$0A
                dex
                bne *-1
                lda #$06
                sta $d020
                sta $d021

                lda #$94
                cmp $d012
                bne *-3
                ldx #$0A
                dex
                bne *-1
                lda #$00
                sta $d020
                sta $d021
                lda #$ca
                sta $d012
                ldx #<irq3
                ldy #>irq3
                stx $0314
                sty $0315
                dec $d019
                jmp $ea31
                
irq3:           inc $d020
                jsr $1003
                dec $d020
                lda #$7A
                sta $d012
                ldx #<irq1
                ldy #>irq1
                stx $0314
                sty $0315
                dec $d019
                jmp $ea31

text1:          SCRL "1234567890 Loading...2345678901234567890"

filename:       dc.b "UNPACKED P*",0

                include cfg_unp.s
                include loader.s

music:
                org $1000

                incbin music.bin
