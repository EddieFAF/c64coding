 
 *=$c000

		sei
		lda #$7f
		sta $dc0d
		sta $dd0d
		and $d011
		sta $d011

		ldy #50
		sty $d012

		lda #<widetop
		ldx #>widetop
		sta $0314
		stx $0315

		lda #$01
		sta $d01a
		cli
		rts

widetop:	jsr latch
		lda #$06
		sta $d020
		sta $d021

		lda #<widebot
		ldx #>widebot
		sta $0314
		stx $0315

		lda #250
		sta $d012

		asl $d019
		jmp $ea81

widebot:	jsr latch
		lda #$0e
		sta $d020

		lda #<widetop
		ldx #>widetop
		sta $0314
		stx $0315

		ldy #50
		sty $d012

		asl $d019
		jmp $ea31

latch:	ldx #02
		dex
		bne latch+2
		rts
