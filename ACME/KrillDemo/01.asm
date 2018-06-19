;***********************************************************************
;**  Project Name: Demo Project                                       **
;**  ---------------------------------------------------------------  **
;**  Filename: 01.asm                                                 **
;**  ---------------------------------------------------------------  **
;**  Author (c): [FAF]Eddie                                           **
;**  File Date: 2018-06-05                                            **
;***********************************************************************
.RELEASE = 0
.SOUNDDISPLAY = 0

!src "../../stdlib/stdlib.a"
!src "../../stdlib/macros.asm"
!src "loader.inc"

; ScreenRAM $4400-$47e8
; Logo Charset und Screen $7000-$7700
; Charset, Fadetabelle $7800-$7C13
; Code $8000-$8687
; Music $8E00-$9C9C

                        ; constants
C_SCREEN_RAM             = $4400
C_COLOUR_RAM             = $d800

.scroll_line             = $4568
.scroll_line_4x4         = $4720

C_CHARSET                = $7800
C_CHARSET_HIGH           = $7900
REG_ZERO_FE              = $fe
REG_ZERO_FD              = $fd

.soundinit               = $8E00
.soundplay               = $8E03

!if .RELEASE { !set C_EXIT_PART = exit_intro_part } else { !set C_EXIT_PART = $fce2 }
!if .RELEASE { !set C_APPLY_INTERRUPT = apply_interrupt } else { !set C_APPLY_INTERRUPT = .APPLY_INTERRUPT }

*=$8000

!if .RELEASE=0 {
        jmp .sync_intro
.APPLY_INTERRUPT:
        sta $D012
        stx $0314
        sty $0315
        jmp $ea81
}

; main function / subroutine
.sync_intro:
        inc VIC2InteruptStatus
        jsr $e544
        +ClearScreen $4400, $00
        jsr .gfx_init
        lda #%00000010 ;<- Bank 1
        sta $dd00
        jsr .scroll_init
        lda #$00
        jsr .soundinit

!if .RELEASE=0 {
        sei
        lda #$7f
        sta $dc0d     ; turn off the CIA interrupts
        sta $dd0d
        and $d011     ; clear high bit of raster line
        sta $d011
        +irqEnd $2C, .p1irq1

        lda #$01      ; enable raster interrupts
        sta $d01a
        cli
        jmp *
}
        lda #$2C
        ldx #<.p1irq1
        ldy #>.p1irq1
        jmp C_APPLY_INTERRUPT


; ************************************************
; ** Initialisierungen                          **
; ************************************************
.gfx_init:
        ldx #$00
-       lda .screen_top,x
        sta $4400,x
        lda .screen_top+24,x
        sta $4400+24,x
        lda #$08 ;color_top,x
        sta $d800,x
        lda #$08 ;color_top+24,x
        sta $d800+24,x
        inx
        bne -
        rts

.scroll_init:
        ldx #$00
-       lda #$00
        sta $4720,x
        sta $db20,x
        inx
        cpx #$A0
        bne -
        lda #<.scroll1x1_text
        ldy #>.scroll1x1_text
        sta .read+1
        sty .read+2
        rts

; ************************************************
; ** IRQ Routinen                               **
; ************************************************
.p1irq1:
        inc VIC2InteruptStatus     ; acknowledge interrupt
        +SetScreenAndCharLocation $0400, $3000
        lda #$18
        sta $d016
        lda #$1b
        sta $d011
        lda #$31
        cmp $d012
        bne *-3
        ldx #$0B
        dex
        bne *-1
.c10:   lda #$00
        sta VIC2BorderColour
        sta VIC2ScreenColour
        ldx #$09            ; delay
        dex
        bne *-1
.c11:   lda #$00
        sta VIC2BorderColour

.m10:   lda #$00
        sta VIC2ScreenColour
.m11:   lda #$00
        sta $d022
.m12:   lda #$00
        sta $d023

        !if .SOUNDDISPLAY=1 {
          dec VIC2BorderColour
          jsr .soundplay
          inc VIC2BorderColour
        } else {
          jsr .soundplay
        }

        lda #$69
        cmp $d012
        bne *-3
        ldx #$09
        dex
        bne *-1
        +SetScreenAndCharLocation $0400, $3800
        lda #$C8
        sta $d016
        lda #$00
        sta VIC2ScreenColour

        lda #$6F
        ldx #<.p1irq2
        ldy #>.p1irq2
        jmp C_APPLY_INTERRUPT


.p1irq2:
        inc VIC2InteruptStatus     ; acknowledge interrupt
        ldx #$03
        dex
        bne *-1
.c20:   lda #$00
        sta VIC2BorderColour
        sta VIC2ScreenColour
        +SetScreenAndCharLocation $0400, $3800
        lda #$C8
        sta $d016

        lda #$8F
        ldx #<.p1irq3
        ldy #>.p1irq3
        jmp C_APPLY_INTERRUPT

.p1irq3:
        inc VIC2InteruptStatus     ; acknowledge interrupt
        ldx #$03
        dex
        bne *-1
.c30:   lda #$00
        sta VIC2BorderColour
        sta VIC2ScreenColour

.p1subroutine:
        jsr .fade_in_all
        ldx .counter
        cpx #$40          ; delay
        bne .p1update_irq
        lda #$D0
        ldx #<.p2irq3b
        ldy #>.p2irq3b
        jmp C_APPLY_INTERRUPT

.p1update_irq:
        lda #$D0
        ldx #<.p1irq3b
        ldy #>.p1irq3b
        jmp C_APPLY_INTERRUPT

.p1irq3b:
        inc VIC2InteruptStatus     ; acknowledge interrupt
        lda #$D1
        cmp $D012
        bne *-3
        ldx #$0b
        dex
        bne *-1
        +SetScreenAndCharLocation $0400, $3800
.c40:   lda #$00
        sta VIC2BorderColour
        sta VIC2ScreenColour

        lda #$F2
        cmp $D012
        bne *-3
        ldx #$0B
        dex
        bne *-1
.c41:   lda #$00
        sta VIC2BorderColour
        sta VIC2ScreenColour
        +SetScreenAndCharLocation $0400, $3800

        lda #$F6
        ldx #<.p1irq4
        ldy #>.p1irq4
        jmp C_APPLY_INTERRUPT

.p1irq4:
        inc VIC2InteruptStatus     ; acknowledge interrupt
        lda #$F8
        cmp $d012
        bne *-3
        ldx #$0B
        dex
        bne *-1
.c50:   lda #$00
        sta VIC2BorderColour
        sta VIC2ScreenColour
        ldx #$09            ; delay
        dex
        bne *-1
.c51:   lda #$00
        sta VIC2ScreenColour
        sta VIC2BorderColour

        lda #$2C
        ldx #<.p1irq1
        ldy #>.p1irq1
        jmp C_APPLY_INTERRUPT



; ************************************************
; ** IRQ Part 2                                 **
; ************************************************
.p2irq1:
        inc VIC2InteruptStatus     ; acknowledge interrupt
        +SetScreenAndCharLocation $0400, $3000 ; Logo
        lda #$18
        sta $d016
        lda #$1b
        sta $d011
        lda #$31
        cmp $d012
        bne *-3
        ldx #$0B
        dex
        bne *-1
.p2C30: lda #$01
        sta VIC2BorderColour
        sta VIC2ScreenColour
        ldx #$09            ; delay
        dex
        bne *-1
        lda #$00
        sta VIC2BorderColour
.mc0:   lda #$06
        sta VIC2ScreenColour
.mc1:   lda #$0e
        sta $d022
.mc2:   lda #$03
        sta $d023

        !if .SOUNDDISPLAY=1 {
          dec VIC2BorderColour
          jsr .soundplay
          inc $VIC2BorderColour
        } else {
          jsr .soundplay
        }

        lda #$69
        cmp $d012
        bne *-3
        ldx #$09
        dex
        bne *-1
        +SetScreenAndCharLocation $0400, $3800
        lda #$C8
        sta $d016
        lda #$00
        sta VIC2ScreenColour

        lda #$6F
        ldx #<.p2irq2
        ldy #>.p2irq2
        jmp C_APPLY_INTERRUPT

.p2irq2:
        inc VIC2InteruptStatus     ; acknowledge interrupt
        ldx #$03
        dex
        bne *-1
.p2C20: lda #$06
        sta VIC2BorderColour
        sta VIC2ScreenColour
        +SetScreenAndCharLocation $0400, $3800 ; 1x1 Scroll
        lda #$C8
        sta $d016

        lda .scroll_delay
        sta $d016

        lda #$7f               ; detect space bar
        sta CIA1KeyboardColumnJoystickA
        lda CIA1KeyboardRowsJoystickB
        and #$10
        bne .update_irq
        jsr .exit_part
;        jmp C_EXIT_PART

.update_irq:
        lda #$8F
        ldx #<.p2irq3
        ldy #>.p2irq3
        jmp C_APPLY_INTERRUPT

.p2irq3:
        inc VIC2InteruptStatus     ; acknowledge interrupt
        ldx #$03
        dex
        bne *-1
        lda #$00
        sta VIC2BorderColour
        sta VIC2ScreenColour

        jsr .scroll4x4
.subroutine:
        jsr .rt

        lda #$D0
        ldx #<.p2irq3b
        ldy #>.p2irq3b
        jmp C_APPLY_INTERRUPT

.p2irq3b:
        inc VIC2InteruptStatus     ; acknowledge interrupt
        lda #$D1
        cmp $D012
        bne *-3
        ldx #$0b
        dex
        bne *-1
        +SetScreenAndCharLocation $0400, $3800
.p2C10: lda #$05    ; green
        sta VIC2BorderColour
        sta VIC2ScreenColour

        lda .scroll_delay_4x4
        sta $d016

        jsr .scroll1x1

        lda #$F2
        cmp $D012
        bne *-3
        ldx #$0A
        dex
        bne *-1
        nop
        lda #$00
        sta VIC2BorderColour
        sta VIC2ScreenColour
        lda #200
        sta $d016

        lda #$F6
        ldx #<.p2irq4
        ldy #>.p2irq4
        jmp C_APPLY_INTERRUPT

.p2irq4:
        inc VIC2InteruptStatus     ; acknowledge interrupt
        lda #$F8
        cmp $d012
        bne *-3
        ldx #$0B
        dex
        bne *-1
.p2C40:  lda #$01
        sta VIC2BorderColour
        sta VIC2ScreenColour
        ldx #$0B            ; delay
        dex
        bne *-1
        lda #$00
        sta VIC2BorderColour
        sta VIC2ScreenColour

        lda #$2C
        ldx #<.p2irq1
        ldy #>.p2irq1
        jmp C_APPLY_INTERRUPT

; ************************************************
; ** Subroutinen                                **
; ************************************************
.countdown:
        ldx .counter
        inx
        stx .counter
        rts

; --------------------------------------
; -- Fade Routine                     --
; --------------------------------------
.fade_in_all:
        ldx .delay
        inx
        stx .delay
        cpx #$08
        bne +
        jmp .fade_in
+       rts

; **** fade in ** **
.fade_in:
        ldx #$00
        stx .delay
        ldx .fade_count
        inx
        cpx #$10
        bne +
        lda #<.countdown
        sta .p1subroutine+1
        lda #>.countdown
        sta .p1subroutine+2
        ldx #$00
+       stx .fade_count
        
        lda .original_C10 ;hole zielfarbe
        asl ;und schiebe ins high nibble
        asl
        asl
        asl
        sta .fade_in_offset ;setze offset
        lda .c10+1 ;hole aktuelle farbe
        and #$0f ;und isoliere low nibble (muss gemacht werden sonst kommt mist raus)
        ora .fade_in_offset ; "addiere" offset
        ;akku zeigt jetzt auf tabelle targetcolor*16+currentcolor
        ;also auf die richtige zeile und spalte in der tabelle!
        tax ;akku nach X
        lda .fade_table,x ;hole fade value
        sta .c10+1

        lda .original_C11
        asl
        asl
        asl
        asl
        sta .fade_in_offset 
        lda .c11+1 
        and #$0f
        ora .fade_in_offset
        tax
        lda .fade_table,x
        sta .c11+1

        lda .original_C20
        asl
        asl
        asl
        asl
        sta .fade_in_offset 
        lda .c20+1
        and #$0f
        ora .fade_in_offset
        tax
        lda .fade_table,x
        sta .c20+1

        lda .original_C30
        asl
        asl
        asl
        asl
        sta .fade_in_offset 
        lda .c30+1
        and #$0f
        ora .fade_in_offset
        tax
        lda .fade_table,x
        sta .c30+1

        lda .original_C40
        asl
        asl
        asl
        asl
        sta .fade_in_offset 
        lda .c40+1
        and #$0f
        ora .fade_in_offset
        tax
        lda .fade_table,x
        sta .c40+1

        lda .original_C41
        asl
        asl
        asl
        asl
        sta .fade_in_offset 
        lda .c41+1
        and #$0f
        ora .fade_in_offset
        tax
        lda .fade_table,x
        sta .c41+1

        lda .original_C50
        asl
        asl
        asl
        asl
        sta .fade_in_offset 
        lda .c50+1
        and #$0f
        ora .fade_in_offset
        tax
        lda .fade_table,x
        sta .c50+1

        lda .original_C51
        asl
        asl
        asl
        asl
        sta .fade_in_offset 
        lda .c51+1
        and #$0f
        ora .fade_in_offset
        tax
        lda .fade_table,x
        sta .c51+1

; -----------------------
        lda .original_M0
        asl
        asl
        asl
        asl
        sta .fade_in_offset 
        lda .m10+1
        and #$0f
        ora .fade_in_offset
        tax
        lda .fade_table,x
        sta .m10+1

        lda .original_M1
        asl
        asl
        asl
        asl
        sta .fade_in_offset 
        lda .m11+1
        and #$0f
        ora .fade_in_offset
        tax
        lda .fade_table,x
        sta .m11+1

        lda .original_M2
        asl
        asl
        asl
        asl
        sta .fade_in_offset
        lda .m12+1
        and #$0f
        ora .fade_in_offset
        tax
        lda .fade_table,x
        sta .m12+1

; --------------------------------
.rt:    rts

; --------------------------------------
; -- 1x1 Scroller                     --
; --------------------------------------
.scroll1x1:
        lda .scroll_delay
        sec
        sbc .scroll_speed
        bcc +
        sta .scroll_delay
        rts
+       and #$07
        sta .scroll_delay
        jsr .scroll1x1_hardscroll
        rts

.scroll1x1_hardscroll:
        ldy #0
-       lda .scroll_line+1,y
        sta .scroll_line,y
        iny
        cpy #$28
        bne -

.read:  lda .scroll_line+$27
        cmp #$00
        bne +
        lda #<.scroll1x1_text
        ldy #>.scroll1x1_text
        sta .read+1
        sty .read+2
        jmp .read
+       sta .scroll_line+$27
        inc .read+1
        lda .read+1
        cmp #$00
        bne +
        inc .read+2
+       rts

; --------------------------------------
; -- 4x4 Scroller                     --
; --------------------------------------
.scroll4x4:
        lda .scroll_delay_4x4
        sec
        sbc .scroll_speed_4x4
        bcc +
        sta .scroll_delay_4x4
        rts
+       and #$07
        sta .scroll_delay_4x4
        jsr .scroll4x4_hardscroll
        rts

.scroll4x4_hardscroll:
        ldy #0
-       lda .scroll_line_4x4+1,y
        sta .scroll_line_4x4,y
        lda .scroll_line_4x4+41,y
        sta .scroll_line_4x4+40,y
        lda .scroll_line_4x4+81,y
        sta .scroll_line_4x4+80,y
        lda .scroll_line_4x4+121,y
        sta .scroll_line_4x4+120,y
        iny
        cpy #$28
        bne -

        ldx .scroller_char_step + 1
        bpl .render_next_scroll_colm

.scroller_message_index:
        ldx #000
.read_next_scroller_char:
        lda .scroll4x4_text, x
        bpl .advance_scroller_index
        lda .scroll4x4_text
        ldx #001
        ldy #>.scroll4x4_text
        sty .read_next_scroller_char + 2
        jmp .save_scroller_index

.advance_scroller_index:
        inx
        bne .save_scroller_index
        inc .read_next_scroller_char + 2
.save_scroller_index:
        stx .scroller_message_index + 1

        ldy #>CHARSET
        cmp #031
        bcc .calc_scrollchar_src_low
        ldy #>CHARSET+$100

.calc_scrollchar_src_low:
        and #031
        asl
        asl
        asl

        sty .render_scroller_column + 2
        sty .render_scroller_column2 + 2
        sta .render_scroller_column + 1
        sta .render_scroller_column2 + 1

        lda #192
        sta .scroller_character_mask + 1
        lda #003
        sta .scroller_char_step + 1

.render_next_scroll_colm:
        ldx #000
        stx REG_ZERO_FD
.render_scroller_column:
        lda CHARSET, x
.scroller_character_mask:
        and #192
.scroller_char_step:
        ldy #255
        beq .skip_shift_1
.shift_scroll_mask_loop1:
        lsr
        lsr
        dey
        bne .shift_scroll_mask_loop1
.skip_shift_1:
        asl
        asl
        sta REG_ZERO_FE

        inx
.render_scroller_column2:
        lda CHARSET, x
        and .scroller_character_mask + 1
        ldy .scroller_char_step + 1
        beq .skip_shift_2
.shift_scroll_mask_loop2:
        lsr
        lsr
        dey
        bne .shift_scroll_mask_loop2

.skip_shift_2:
        clc
        adc REG_ZERO_FE
        adc #064

.scroller_render_offset: 
        ldy REG_ZERO_FD
        sta .scroll_line_4x4 + $27, y
        tya
        adc #040
        sta REG_ZERO_FD
        inx
        cpx #008
        bne .render_scroller_column
        dec .scroller_char_step + 1
        lda .scroller_character_mask + 1
        lsr
        lsr
        sta .scroller_character_mask + 1
.update_scroller_done:
        rts

.exit_part:
        ldx #<.fade_out_all
        ldy #>.fade_out_all
        stx .subroutine+1
        sty .subroutine+2
        rts

.fade_out_all:
        ldx .delay
        inx
        stx .delay
        cpx #$08
        bne +
        jmp .fade_out
+       rts

; **** fade out ****
.fade_out:
        ldx #$00
        stx .delay
        ldx .fade_count
        inx
        cpx #$08
        bne +
        lda #<.fade_out_all_2
        sta .subroutine+1
        lda #>.fade_out_all_2
        sta .subroutine+2
        ldx #$00
+       stx .fade_count

        lda #$00 ;hole zielfarbe
        sta .fade_in_offset ;setze offset
        lda .p2C10+1 ;hole aktuelle farbe
        and #$0f ;und isoliere low nibble (muss gemacht werden sonst kommt mist raus)
        ora .fade_in_offset ; "addiere" offset
        ;akku zeigt jetzt auf tabelle targetcolor*16+currentcolor
        ;also auf die richtige zeile und spalte in der tabelle!
        tax ;akku nach X
        lda .fade_table,x ;hole fade value
        sta .p2C10+1
        rts

.fade_out_all_2:
        ldx .delay
        inx
        stx .delay
        cpx #$08
        bne +
        jmp .fade_out_2
+       rts

; **** fade out ****
.fade_out_2:
        ldx #$00
        stx .delay
        ldx .fade_count
        inx
        cpx #$08
        bne +
        lda #<.fade_out_all_3
        sta .subroutine+1
        lda #>.fade_out_all_3
        sta .subroutine+2

        ldx #$00
+       stx .fade_count

        lda #$00
        asl
        asl
        asl
        asl
        sta .fade_in_offset
        lda .p2C20+1
        and #$0f
        ora .fade_in_offset
        tax
        lda .fade_table,x
        sta .p2C20+1

        ldy #$00
-       lda $D968,y
        and #$0f
        ora .fade_in_offset
        tax
        lda .fade_table,x
        sta $D968,y
        iny
        cpy #$28
        bne -
        rts

.fade_out_all_3:
        ldx .delay
        inx
        stx .delay
        cpx #$04
        bne +
        jmp .fade_out_3
+       rts

; **** fade out ****
.fade_out_3:
        ldx #$00
        stx .delay
        ldx .fade_count
        inx
        cpx #$10
        bne +
        lda #$00
        sta .scroll_speed
        sta .scroll_speed_4x4

        lda #<.fade_out_all_4
        sta .subroutine+1
        lda #>.fade_out_all_4
        sta .subroutine+2

        ldx #$00
+       stx .fade_count

        lda #$00 ;hole zielfarbe
        sta .fade_in_offset
        lda .p2C30+1
        and #$0f
        ora .fade_in_offset
        tax
        lda .fade_table,x
        sta .p2C30+1

        lda .p2C40+1
        and #$0f
        ora .fade_in_offset
        tax
        lda .fade_table,x
        sta .p2C40+1

        rts

; Fade Out Top Charset 
.fade_out_all_4:
        ldx .delay
        inx
        stx .delay
        cpx #$08
        bne +
        jmp .fade_out_4
+       rts

; **** fade out ****
.fade_out_4:
        ldx #$00
        stx .delay
        ldx .fade_count
        inx
        cpx #$10
        bne +
        jmp C_EXIT_PART
        ldx #$00
+       stx .fade_count
        
        lda #$00 ;hole zielfarbe
        sta .fade_in_offset
        lda .mc0+1
        and #$0f
        ora .fade_in_offset
        tax
        lda .fade_table,x
        sta .mc0+1

        lda .mc1+1
        and #$0f
        ora .fade_in_offset
        tax
        lda .fade_table,x
        sta .mc1+1

        lda .mc2+1
        and #$0f
        ora .fade_in_offset
        tax
        lda .fade_table,x
        sta .mc2+1

        rts


.counter:           !byte $00

.scroll_speed:      !byte $01
.scroll_speed_4x4:  !byte $02

.fade_in_offset:    !byte 0
.fade_in_offset_low: !byte $00
.delay:              !byte $00
.fade_count:         !byte $00

.original_M0:        !byte $06
.original_M1:        !byte $0e
.original_M2:        !byte $03
.original_C10:       !byte $01
.original_C11:       !byte $00
.original_C20:       !byte $06 ;$0e
.original_C30:       !byte $00
.original_C40:       !byte $05 ;$08
.original_C41:       !byte $00
.original_C50:       !byte $01
.original_C51:       !byte $00


;***********************************************************************
;**  Included Labled Data (Resources from external files) which the   **
;**  compiler automatically (reserves and) assignes (a) (starting)    **
;**  memory (address) for.                                            **
;***********************************************************************
!ct scr
.scroll1x1_text:    !text "dies ist ein scrolltext, der sehr lang ist und"
                    !text " daher auch ueber viele zeilen gehen kann.... "
                    !byte $00

*=$7800
CHARSET: !bin "aeg_collection_09.64c",,2
; 4x4 dot matrix
        !byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0e,$0e,$0e,$00
        !byte $00,$00,$00,$00,$e0,$e0,$e0,$00,$00,$00,$00,$00,$ee,$ee,$ee,$00
        !byte $0e,$0e,$0e,$00,$00,$00,$00,$00,$0e,$0e,$0e,$00,$0e,$0e,$0e,$00
        !byte $0e,$0e,$0e,$00,$e0,$e0,$e0,$00,$0e,$0e,$0e,$00,$ee,$ee,$ee,$00
        !byte $e0,$e0,$e0,$00,$00,$00,$00,$00,$e0,$e0,$e0,$00,$0e,$0e,$0e,$00
        !byte $e0,$e0,$e0,$00,$e0,$e0,$e0,$00,$e0,$e0,$e0,$00,$ee,$ee,$ee,$00
        !byte $ee,$ee,$ee,$00,$00,$00,$00,$00,$ee,$ee,$ee,$00,$0e,$0e,$0e,$00
        !byte $ee,$ee,$ee,$00,$e0,$e0,$e0,$00,$ee,$ee,$ee,$00,$ee,$ee,$ee,$00

; ************************************************************************** ;
.fade_table:
    ;high nibble=target color
    ;low nibble =source color
    ;      0   1   2   3   4   5   6   7   8   9   a   b   c   d   e   f
    !byte $00,$0d,$09,$0c,$02,$08,$02,$0f,$02,$00,$08,$09,$04,$03,$04,$05    ;0
    !byte $06,$01,$08,$0d,$0c,$03,$0b,$01,$0a,$02,$0f,$04,$03,$01,$03,$07    ;1
    !byte $09,$07,$02,$0c,$02,$04,$0b,$0f,$02,$02,$08,$02,$04,$03,$04,$0a    ;2
    !byte $06,$0d,$04,$03,$0c,$03,$0b,$0f,$0c,$02,$0c,$04,$03,$03,$03,$03    ;3
    !byte $06,$0d,$04,$0c,$04,$0c,$0b,$0f,$04,$02,$04,$04,$04,$03,$04,$0a    ;4
    !byte $06,$0d,$08,$05,$0c,$05,$0b,$0f,$0c,$02,$0c,$04,$05,$03,$05,$05    ;5
    !byte $06,$0d,$0b,$0e,$0b,$08,$06,$0f,$0b,$0b,$08,$06,$04,$03,$04,$0c    ;6
    !byte $09,$07,$08,$0f,$0a,$0f,$0b,$07,$0a,$02,$0f,$04,$0f,$07,$0f,$07    ;7
    !byte $09,$07,$08,$0c,$08,$0c,$0b,$0f,$08,$02,$08,$08,$08,$03,$04,$0c    ;8
    !byte $09,$07,$09,$0c,$02,$08,$0b,$0f,$02,$09,$08,$09,$08,$03,$04,$0c    ;9
    !byte $09,$07,$04,$0c,$0a,$0c,$0b,$0f,$0a,$02,$0a,$04,$0a,$0f,$0c,$0a    ;a
    !byte $06,$0d,$0b,$0e,$0b,$08,$0b,$0f,$0b,$0b,$08,$0b,$04,$03,$04,$0c    ;b
    !byte $06,$0d,$08,$0c,$0c,$0c,$0b,$0f,$0c,$02,$0c,$04,$0c,$03,$0c,$0c    ;c
    !byte $09,$0d,$08,$0d,$0c,$03,$0b,$0d,$0c,$02,$0f,$04,$0f,$0d,$03,$0d    ;d
    !byte $06,$0d,$04,$0e,$0c,$0e,$0b,$0f,$0c,$02,$0c,$04,$0e,$03,$0e,$0e    ;e
    !byte $06,$0d,$08,$0f,$0c,$0f,$0b,$0f,$0c,$02,$0f,$04,$0f,$0f,$0f,$0f    ;f

.scroll_delay:      !byte $00
.scroll_delay_4x4:  !byte $00
.scroll4x4_text:    !text "hier kommt ein weiterer scrolltext, der auch sehr"
                    !text " lang ist. auch hier kann so viel text stehen und"
                    !text " daher auch ueber viele zeilen gehen kann.... "
                    !byte $FF

*=$7000
!src "logo_top2.inc"

*=$8E00
!bin "END.sid",,$7e

;*********************************************************************72
;** File END                                                          **
;***********************************************************************
