; -- Covert Ops IRQ Loader --
;
; Platform: C64
; Code: Eddie[FAF]
;

;!src "../stdlib/stdlib.a"


        *=$8000

start:
        jsr initloader

        lda #$00
        sta $d020
        sta $d021

        ldx #<filename
        ldy #>filename
        jsr loadfile_exomizer
		bcc ok
		sta $d020
ok:
        rts

filename:
        !text "01"
        !byte $00


        !src "cfg_exom.s"
        !src "loader.s"

