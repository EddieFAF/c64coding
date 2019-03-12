; -- intro part loader example --
;
; Platform: C64
; Code: Jesder / 0xc64
; Site: http:;www.0xc64.com
;
!sl "loader.inc"
!src "loadersymbols-c64.inc"
!src "../../stdlib/stdlib.a"


RELEASE = 1

STARTZEILE              = 10
      *= $0801
!byte $0c,$08,$0a,$00,$9e,$32,$30,$36,$34,$00,$00,$00,$00

*=$0810

                        ; program start
                        ;*= $080D "Main"                 ; begin (2049)

start:

        ;jsr install
        jsr $e544
        ldx #$00
-       lda char,x
        sta $2000,x
        lda char+$100,x
        sta $2100,x
        lda char+$200,x
        sta $2200,x
        lda char+$300,x
        sta $2300,x
        lda char+$400,x
        sta $2400,x
        lda char+$500,x
        sta $2500,x
        inx
        bne -

        ldx #$00
-       lda .screen_char1,x
        sta $0400+STARTZEILE*40,x
        lda .screen_color1,x
        sta $d800+STARTZEILE*40,x
        inx
        cpx #200
        bne -

        jmp load_next_part

        ; apply interrupt routine -------------------------------------------]
        ; -------------------------------------------------------------------]
apply_interrupt:
        sta VIC2Raster
        stx IRQServiceRoutineLo
        sty IRQServiceRoutineHi
        jmp $ea81


        ; exit part routine -------------------------------------------------]
        ; -------------------------------------------------------------------]
exit_intro_part:
        lda #<clean_current_part        ; reset main loop to load next part
        sta forever + 1
        lda #>clean_current_part
        sta forever + 2

        lda #000                        ; switch off interrupts
        sta CIA1InterruptControl
        sta CIA2InterruptControl

        lda #240                        ; disable raster interrupts
        sta VIC2InteruptControl

        ldx #$31                        ; restore interrupt pointer
        ldy #$ea
        jmp apply_interrupt


        ; clean up routine --------------------------------------------------]
        ; -------------------------------------------------------------------]
clean_up_intro_part:
        sei

        jsr $fda3

        ldy #31                        ; restore irq registers
irq_reset:
        lda $fd30,y
        sta $0314,y
        dey
        bpl irq_reset

        lda #3                        ; restore input / output device numbers
        sta ZPCurrentOutputDevice
        lda #000
        sta ZPCurrentInputDevice

        ldx #$1f                        ; restore sprites
sprite_reset:
        lda $ecb8,x
        sta $cfff,x
        dex
        bne sprite_reset

        jsr $e51b
        jsr $ff5e

        ldy #32                        ; restore sid chip
sid_reset:
        lda #31
        sta SID,y
        lda #000
        sta SID,y
        dey
        bpl sid_reset
        lda #15
        sta SIDVolumeFilter

        jsr $e544

        cli

        rts


        ; loader variables --------------------------------------------------]
        ; -------------------------------------------------------------------]

loader_part_name_low:   !byte <loader_part_name_1, <loader_part_name_2, <loader_part_name_3
loader_part_name_high:  !byte >loader_part_name_1, >loader_part_name_2, >loader_part_name_3
loader_part_index:      !byte 0

load_part_name_length:  !byte 2, 2, 2

loader_part_name_1:     !text "01"
loader_part_name_2:     !text "02"
loader_part_name_3:     !text "03"


        ; loader main -------------------------------------------------------]
        ; -------------------------------------------------------------------]

clean_current_part: jsr clean_up_intro_part ; clean up existing part
load_next_part:     jsr load_part           ; load next intro part
;                   jsr sync_part           ; start fadeout routine
-                   lda fertig
                    cmp #$01
                    bne -
;                   jsr sync_part2          ; execute set up routine
forever:            jmp forever             ; idle until next part required


        ; load part routine -------------------------------------------------]
        ; -------------------------------------------------------------------]

load_part:
        sei                             ; hook loader "interrupt"
        lda #$7f
        sta CIA1InterruptControl
        sta CIA2InterruptControl
        and VIC2ScreenControlV          ; $d011
        sta VIC2ScreenControlV

        ldy #58
        sty VIC2Raster

        lda #<loader_interrupt
        ldx #>loader_interrupt
        sta IRQServiceRoutineLo
        stx IRQServiceRoutineHi

        lda #$01
        sta VIC2InteruptControl
        cli

        clc
        ldx loader_part_index
set_filename_high:
        lda loader_part_name_high,x
        tay
set_filename_low:
        lda loader_part_name_low,x
        tax
;        jsr loadcompd

;                        inc set_filename_low + 1
;                        inc set_filename_high + 1

        inc loader_part_index


        lda #<forever                   ; restore eternal loop
        sta forever + 1
        lda #>forever
        sta forever + 2
        rts

        ; loader interrupt --------------------------------------------------]
        ; -------------------------------------------------------------------]

loader_interrupt:
        lda #STARTZEILE*8+44
        cmp $d012
        bne *-3
        ldx #$0B
        dex
        bne *-1
c1:     lda #$0B ; Linie oben
        sta $d020
        sta $d021
        ldx #$0B
        dex
        bne *-1
        lda #$00
        sta $d020
        sta $d021
        lda #<irq1
        ldx #>irq1
        sta IRQServiceRoutineLo
        stx IRQServiceRoutineHi

        lda #STARTZEILE*8+48
        sta VIC2Raster
        asl VIC2InteruptStatus
        jmp $ea81

irq1:
        lda #STARTZEILE*8+49
        cmp VIC2Raster
        bne *-3
        ldx #$0A
        dex
        bne *-1
        lda #$00
        sta VIC2BorderColour
        ldx #$0B
        dex
        bne *-1
        lda #$18
        sta VIC2MemorySetup
        lda #$18
        sta VIC2ScreenControlH
bg:     lda #$06
        sta VIC2ScreenColour
mc1:    lda #$0e
        sta VIC2ExtraBackgroundColour1
mc2:    lda #$03
        sta VIC2ExtraBackgroundColour2

        lda #<irq2
        ldx #>irq2
        sta IRQServiceRoutineLo
        stx IRQServiceRoutineHi
        lda #(STARTZEILE+5)*8+45
        sta VIC2Raster
        asl VIC2InteruptStatus
        jmp $ea81

irq2:   lda #(STARTZEILE+5)*8+50
        cmp VIC2Raster
        bne *-3
        ldx #$09
        dex
        bne *-1
        lda #$15
        sta VIC2MemorySetup
        lda #200
        sta VIC2ScreenControlH
        lda #$00
        sta $d021

        lda #<irq3
        ldx #>irq3
        sta IRQServiceRoutineLo
        stx IRQServiceRoutineHi

        ldy #(STARTZEILE+5)*8+54
        sty VIC2Raster

        asl VIC2InteruptStatus
        jmp $ea81

irq3:   lda #(STARTZEILE+5)*8+56
        cmp VIC2Raster
        bne *-3
        ldx #$0B
        dex
        bne *-1
c2:     lda #$0f ; Linie unten
        sta $d020
        sta $d021
        ldx #$0B
        dex
        bne *-1
c3:     lda #$0C ; Hintergund
        sta VIC2BorderColour
        sta VIC2ScreenColour

; ------------------] Detect Space Bar [-------------------------
        lda #$7f               ; detect space bar
        sta CIA1KeyboardColumnJoystickA
        lda CIA1KeyboardRowsJoystickB
        and #$10
        bne subroutine
        ldx #<fade_out1 ; Fadeout Routine starten
        ldy #>fade_out1
        stx subroutine+1
        sty subroutine+2
; ------------------] Detect Space Bar [-------------------------

subroutine:
        jsr rt
        lda #<loader_interrupt
        ldx #>loader_interrupt
        sta IRQServiceRoutineLo
        stx IRQServiceRoutineHi

        ldy #STARTZEILE*8+40
        sty VIC2Raster

        asl VIC2InteruptStatus
        jmp $ea31

; ---------------------------------------------------------------
fade_out1:
        ldx delay
        inx
        stx delay
        cpx #$04
        bne rt
        jmp fade_out
rt:     rts

;**** fade out ****
fade_out:
        ldx #$00
        stx delay
        ldx fade_count
        inx
        cpx #$10
        bne +
        lda #<fade_out_screen1
        sta subroutine+1
        lda #>fade_out_screen1
        sta subroutine+2
        ldx #$00
+       stx fade_count

        lda mc1+1 ;hole aktuelle farbe
        and #$0f
        tax
        lda fade_table,x   ;hole fade value
        sta mc1+1          ;schreibe neuen wert

        lda mc2+1 ;hole aktuelle farbe
        and #$0f
        tax
        lda fade_table,x   ;hole fade value
        sta mc2+1          ;schreibe neuen wert

        lda bg+1 ;hole aktuelle farbe
        and #$0f
        tax
        lda fade_table,x   ;hole fade value
        sta bg+1          ;schreibe neuen wert
        rts

; ---------------------------------------------------------------------------
fade_out_screen1:
        ldx delay
        inx
        stx delay
        cpx #$04
        bne +
        jmp fade_out_screen
+:      rts

;**** fade out ****
fade_out_screen:
        ldx #$00
        stx delay
        ldx fade_count
        inx
        cpx #$10
        bne +
        lda #<rt
        sta subroutine+1
        lda #>rt
        sta subroutine+2
        lda #$01
        sta fertig
        ldx #$00
+       stx fade_count

        lda c1+1 ;hole aktuelle farbe
        and #$0f
        tax
        lda fade_table,x   ;hole fade value
        sta c1+1          ;schreibe neuen wert

        lda c2+1 ;hole aktuelle farbe
        and #$0f
        tax
        lda fade_table,x   ;hole fade value
        sta c2+1          ;schreibe neuen wert

        lda c3+1 ;hole aktuelle farbe
        and #$0f
        tax
        lda fade_table,x   ;hole fade value
        sta c3+1          ;schreibe neuen wert

        rts
; ---------------------------------------------------------------------------
fade_count:
    !byte 0
; ----
delay: !byte $00
fertig: !byte $00

        ; sync new part -----------------------------------------------------]
        ; -------------------------------------------------------------------]

sync_part:
        lda #<fade_out1
        sta subroutine+1
        lda #>fade_out1
        sta subroutine+2
        rts
sync_part2:
        sei
        lda #$7f
        sta CIA1InterruptControl             ; turn off the CIA interrupts
        sta CIA2InterruptControl
        and VIC2ScreenControlV             ; clear high bit of raster line
        sta VIC2ScreenControlV

        ldy #000
        sty VIC2Raster

        lda #$00                       ; load interrupt address
        ldx #$80
        sta IRQServiceRoutineLo
        stx IRQServiceRoutineHi

        lda #$01                        ; enable raster interrupts
        sta VIC2InteruptControl
        cli

        rts


        ; loader interrupt --------------------------------------------------]
        ; -------------------------------------------------------------------]

loader:
         *= $0B00
;!bin "loader-c64.prg",,2


         *= $2000
;!bin "install-c64.prg",,2

*=$1000
char:
!src "charset1.inc"
!src "screen1.inc"

*=$1600
!src "../../stdlib/tables.asm"
