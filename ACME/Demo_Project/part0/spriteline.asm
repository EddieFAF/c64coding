; ********************************
; ** Sprite Linie - Routinen    **
; ********************************

sprite_line_init:
; ********************************
; ** Spritedaten initialisieren **
; ********************************
            ldx #0
-           lda sprite_line_data,X
            sta $0340,X
            inx
            cpx #63
            bne -
            rts

sprite_line_set:
; ********************************
; ** Sprites einrichten         **
; ********************************
            lda #%11111111          ; Sprite 0+1 aktivieren
            sta VIC2SpriteEnable
            ldx #$01
            stx VIC2Sprite0Colour        ; Farbe Sprite 0
            stx VIC2Sprite1Colour        ; Farbe Sprite 1
            stx VIC2Sprite2Colour        ; Farbe Sprite 2
            stx VIC2Sprite3Colour
            stx VIC2Sprite4Colour
            stx VIC2Sprite5Colour
            stx VIC2Sprite6Colour
            stx VIC2Sprite7Colour

            lda #$0D
            sta $07f8               ; Sprite 0
            sta $07f9               ; Sprite 1
            sta $07fa               ; Sprite 2
            sta $07fb
            sta $07fc
            sta $07fd
            sta $07fe
            sta $07ff
            ldx #$00
-           lda spr_pos,x
            sta VIC2Sprite0X,x
            inx
            cpx #$10
            bne -
            lda #%00000000
            sta VIC2SpriteDoubleWidth
            lda #%00000000
            sta VIC2SpriteXMSB
            rts

spr_pos     !byte 158, 141
            !byte 158, 141
            !byte 158, 141
            !byte 158, 141

            !byte 182, 141
            !byte 182, 141
            !byte 182, 141
            !byte 182, 141
        
sprite_line_space:
; **********************
; ** Warten auf Space
; **********************
            lda #$7f       ; detect space bar
            sta $dc00
            lda $dc01
            and #$10
            beq sprite_line_move
            rts

sprite_line_move:
            ldx VIC2Sprite4X
            inx
            stx VIC2Sprite4X
            cpx #$00
            bne +
            lda VIC2SpriteXMSB
            ora #%10010000
            sta VIC2SpriteXMSB
+           ldx VIC2Sprite0X
            dex
            stx VIC2Sprite0X
            cpx #134
            beq sprite1
            cpx #110
            beq sprite2
            cpx #86
            beq sprite3
            cpx #62
            beq sprite4
            cpx #38
            beq sprite5
            cpx #20
            beq sprite_line_stop
            rts

sprite1     ldx #134
            stx VIC2Sprite0X
            lda VIC2SpriteDoubleWidth
            ora #%00100010
            sta VIC2SpriteDoubleWidth
            lda #134
            sta VIC2Sprite1X
            lda #182
            sta VIC2Sprite5X
            rts

sprite2     ldx #110
            stx VIC2Sprite2X
            ldx #230
            stx VIC2Sprite6X
            rts
            
sprite3     ldx #86
            stx VIC2Sprite2X
            lda VIC2SpriteDoubleWidth
            ora #%01000100
            sta VIC2SpriteDoubleWidth
            rts
            
sprite4     ldx #62
            stx VIC2Sprite3X
            ldx #022
            stx VIC2Sprite7X
            rts

sprite5     ldx #38
            stx VIC2Sprite3X
            lda VIC2SpriteDoubleWidth
            ora #%10001000
            sta VIC2SpriteDoubleWidth
            rts

sprite_line_stop:
; **********************
; ** stoppe Bewegung
; **********************
            lda #<color_move
            sta irq_jump_target+1
            lda #>color_move
            sta irq_jump_target+2
            lda #$00
            sta VIC2SpriteEnable
            rts

sprite_line_set_rts:
; **********************
; ** setze RTS
; **********************
            lda #<sprite_line_set_rts_exit
            sta irq_jump_target+1
            lda #>sprite_line_set_rts_exit
            sta irq_jump_target+2
sprite_line_set_rts_exit
            rts


