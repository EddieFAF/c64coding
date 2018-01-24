// -------------------------------------------------------------------
// The decruncher jsr:s to the get_crunched_byte address when it wants to
// read a crunched byte. This subroutine has to preserve x and y register
// and must not modify the state of the carry flag.
// -------------------------------------------------------------------
// -------------------------------------------------------------------
// this function is the heart of the decruncher.
// It initializes the decruncher zeropage locations and precalculates the
// decrunch tables and decrunches the data
// This function will not change the interrupt status bit and it will not
// modify the memory configuration.
// -------------------------------------------------------------------
// -------------------------------------------------------------------
// if literal sequences is not used (the data was crunched with the -c
// flag) then the following line can be uncommented for shorter code.
.var EXOD_LITERAL_SEQUENCES_NOT_USED = 1 // 0 wenn used, 1 wenn not used
// -------------------------------------------------------------------
// zero page addresses used
// -------------------------------------------------------------------
.var exod_zp_len_lo = $a7

.var exod_zp_src_lo  = $ae
.var exod_zp_src_hi  = exod_zp_src_lo + 1

.var exod_zp_bits_hi = $fc

.var exod_zp_bitbuf  = $fd
.var exod_zp_dest_lo = exod_zp_bitbuf + 1	// dest addr lo
.var exod_zp_dest_hi = exod_zp_bitbuf + 2	// dest addr hi

.var exod_tabl_bi = exod_decrunch_table
.var exod_tabl_lo = exod_decrunch_table + 52
.var exod_tabl_hi = exod_decrunch_table + 104

// -------------------------------------------------------------------
// no code below this comment has to be modified in order to generate
// a working decruncher of this source file.
// However, you may want to relocate the tables last in the file to a
// more suitable address.
// -------------------------------------------------------------------

// -------------------------------------------------------------------
// jsr this label to decrunch, it will in turn init the tables and
// call the decruncher
// no constraints on register content, however the
// decimal flag has to be #0 (it almost always is, otherwise do a cld)
exod_decrunch:
// -------------------------------------------------------------------
// init zeropage, x and y regs. (12 bytes)
//
	ldy #0
	ldx #3
exod_init_zp:
	jsr exod_get_crunched_byte
	sta exod_zp_bitbuf - 1,x
	dex
	bne exod_init_zp
// -------------------------------------------------------------------
// calculate tables (50 bytes)
// x and y must be #0 when entering
//
exod_nextone:
	inx
	tya
	and #$0f
	beq exod_shortcut		// starta på ny sekvens

	txa			// this clears reg a
	lsr			// and sets the carry flag
	ldx exod_tabl_bi-1,y
exod_rolle:
	rol
	rol exod_zp_bits_hi
	dex
	bpl exod_rolle		// c = 0 after this (rol exod_zp_bits_hi)

	adc exod_tabl_lo-1,y
	tax

	lda exod_zp_bits_hi
	adc exod_tabl_hi-1,y
exod_shortcut:
	sta exod_tabl_hi,y
	txa
	sta exod_tabl_lo,y

	ldx #4
	jsr exod_get_bits		// clears x-reg.
	sta exod_tabl_bi,y
	iny
	cpy #52
	bne exod_nextone
	ldy #0
	beq exod_begin
exod_get_bits:
	lda #$00
	sta exod_zp_bits_hi
	cpx #$01
	bcc exod_bits_done
exod_bits_next:
	lsr exod_zp_bitbuf
	bne exod_ok
	pha
exod_literal_get_byte:
	jsr exod_get_crunched_byte
	bcc exod_literal_byte_gotten
	ror
	sta exod_zp_bitbuf
	pla
exod_ok:
	rol
	rol exod_zp_bits_hi
	dex
	bne exod_bits_next
exod_bits_done:
	rts
// -------------------------------------------------------------------
// main copy loop (18(16) bytes)
//
exod_copy_next_hi:
	dex
	dec exod_zp_dest_hi
	dec exod_zp_src_hi
exod_copy_next:
	dey
	bcc exod_literal_get_byte
	lda (exod_zp_src_lo),y
exod_literal_byte_gotten:
	sta (exod_zp_dest_lo),y
exod_copy_start:
	tya
	bne exod_copy_next
exod_begin:
	txa
	bne exod_copy_next_hi
// -------------------------------------------------------------------
// decruncher entry point, needs calculated tables (21(13) bytes)
// x and y must be #0 when entering
//
	inx
	jsr exod_get_bits
	tay
	bne exod_literal_start1
exod_begin2:
	inx
	jsr exod_bits_next
	lsr
	iny
	bcc exod_begin2
	cpy #$11
	bcc exod_sequence_start
	beq exod_bits_done
// -------------------------------------------------------------------
// literal sequence handling (13(2) bytes)
//
	ldx #$10
	jsr exod_get_bits
exod_literal_start1:
	sta <exod_zp_len_lo
	ldx <exod_zp_bits_hi
	ldy #0
	bcc exod_literal_start
exod_sequence_start:
// -------------------------------------------------------------------
// calulate length of sequence (exod_zp_len) (11 bytes)
//
	ldx exod_tabl_bi - 1,y
	jsr exod_get_bits
	adc exod_tabl_lo - 1,y	// we have now calculated exod_zp_len_lo
	sta exod_zp_len_lo
// -------------------------------------------------------------------
// now do the hibyte of the sequence length calculation (6 bytes)
	lda exod_zp_bits_hi
	adc exod_tabl_hi - 1,y	// c = 0 after this.
	pha
// -------------------------------------------------------------------
// here we decide what offset table to use (20 bytes)
// x is 0 here
//
	bne exod_nots123
	ldy exod_zp_len_lo
	cpy #$04
	bcc exod_size123
exod_nots123:
	ldy #$03
exod_size123:
	ldx exod_tabl_bit - 1,y
	jsr exod_get_bits
	adc exod_tabl_off - 1,y	// c = 0 after this.
	tay			// 1 <= y <= 52 here
// -------------------------------------------------------------------
// Here we do the dest_lo -= len_lo subtraction to prepare exod_zp_dest
// but we do it backwards:	a - b == (b - a - 1) ^ ~0 (C-syntax)
// (16(16) bytes)
	lda exod_zp_len_lo
exod_literal_start:			// literal enters here with y = 0, c = 1
	sbc exod_zp_dest_lo
	bcc exod_noborrow
	dec exod_zp_dest_hi
exod_noborrow:
	eor #$ff
	sta exod_zp_dest_lo
	cpy #$01		// y < 1 then literal
	bcc exod_pre_copy
// -------------------------------------------------------------------
// calulate absolute offset (exod_zp_src) (27 bytes)
//
	ldx exod_tabl_bi,y
	jsr exod_get_bits//
	adc exod_tabl_lo,y
	bcc exod_skipcarry
	inc exod_zp_bits_hi
	clc
exod_skipcarry:
	adc exod_zp_dest_lo
	sta exod_zp_src_lo
	lda exod_zp_bits_hi
	adc exod_tabl_hi,y
	adc exod_zp_dest_hi
	sta exod_zp_src_hi
// -------------------------------------------------------------------
// prepare for copy loop (8(6) bytes)
//
	pla
	tax
	sec
exod_pre_copy: ldy <exod_zp_len_lo
	jmp exod_copy_start
// -------------------------------------------------------------------
// two small static tables (6(6) bytes)
//
exod_tabl_bit:
	.byte 2,4,4
exod_tabl_off:
	.byte 48,32,16
// -------------------------------------------------------------------
// end of decruncher
// -------------------------------------------------------------------

// -------------------------------------------------------------------
// this 156 byte table area may be relocated. It may also be clobbered
// by other data between decrunches.
// -------------------------------------------------------------------
exod_decrunch_table:
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0
// -------------------------------------------------------------------
// end of decruncher
// -------------------------------------------------------------------
