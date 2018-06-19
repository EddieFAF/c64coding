BANK=   $96     ; The value of the video bank register (CIA2) in the tech-area
ZP=     $FB     ; Zero page for indirect addressing
START=  $4400   ; Start of the charsets (we use inverted chars)
SCREEN= $4000   ; Position of the video matrix
SHIFTL= $CF00   ; x-shift, lowest 3 bits
SHIFTH= $CE00   ; x-shift, highest 3 bittid (multiplied with two)
POINTER= $033C  ; Pointer to shift-table
VALUE=  $033D   ; Shift now
SPEED=  $033E   ; Shift change

*= $C000  ; Start address

INIT:   SEI             ; Disable interrupts
        LDA #$7F
        STA $DC0D       ; Disable timer interrupt
        LDA #$81
        STA $D01A       ; Enable raster interrupt
        LDA #<IRQ
        STA $0314       ; Our own interrupt handler
        LDA #>IRQ
        STA $0315
        LDA #49         ; The interrupt to the line before the first bad line
        STA $D012
        LDA #$1B
        STA $D011       ; 9th bit of the raster compare

        LDY #0
        LDX #$40
        STX ZP+1
        STY ZP
        TYA
LOOP0   STA (ZP),Y      ; Clear the whole video bank ($4000-7FFF)
        INY
        BNE LOOP0
        INC ZP+1
        BPL LOOP0

        LDA #>START
        STA ZP+1
        LDA #$32        ; Character ROM to address space ($D000-)
        STA $01
LOOP1   TYA             ; (Y-register is zero initially)
        LSR
        LSR
        LSR
        TAX
        LDA TEXT,X      ; Which char to plot ?
        ASL             ; Source
        ASL
        ASL
        TAX             ; low byte to X
        LDA #$D0
        ADC #0          ; high byte (one bit) taken into account
        STA LOOP2+2 ; Self-modifying again..
LOOP2   LDA $D000,X
        STA (ZP),Y
        INX
        INY
        TXA
        AND #7
        BNE LOOP2       ; Copy one char
        CPY #0
        BNE LOOP1       ; Copy 32 chars (256 bytes)
        LDA #$37        ; Memory configuration back to normal
        STA $01

LOOP3   LDA START,Y       ; Copy the data to each charset, shifted by one
        STA START+2056,Y  ;  position to the right
        STA START+4112,Y
        STA START+6168,Y
        STA START+8224,Y
        STA START+10280,Y
        STA START+12336,Y
        STA START+14392,Y
        INY
        BNE LOOP3
        LDA #0          ; Clear the pointer, value and speed
        STA POINTER
        STA VALUE
        STA SPEED

LOOP4   TYA             ; (Y was zero)
        ORA #$80        ; Use the inverted chars
        STA SCREEN,Y    ; Set the character codes to video matrix
        STA SCREEN+40,Y
        STA SCREEN+80,Y
        STA SCREEN+120,Y
        STA SCREEN+160,Y
        STA SCREEN+200,Y
        STA SCREEN+240,Y
        STA SCREEN+280,Y
        LDA #239        ; leave the last line empty
        STA SCREEN+320,Y
        INY
        CPY #40
        BNE LOOP4       ; Loop until the whole area is filled
        CLI             ; Enable interrupts
        RTS

IRQ     LDA #BANK       ; Change the video bank, some timing
        STA $DD00
        NOP
        NOP

        LDY POINTER     ; Y-register will point to x-shift
        JMP BAD         ; next line is a bad line
LOOP5   NOP
LOOP6   LDA SHIFTL,Y    ; Do the shift
        STA $D016       ; 3 lowest bits
        LDA SHIFTH,Y
        STA $D018       ; another 3 bits
        NOP : NOP : NOP : NOP : NOP : NOP ; waste some time
        NOP : NOP : NOP : NOP : NOP : NOP
        NOP : NOP : NOP
        LDA $D012       ; check if it is time to stop
        CMP #$78
        BPL OVER
        INY   ; next position in table
        DEX
        BNE LOOP5       ; No bad line, loop
BAD     LDA SHIFTL,Y    ; This is a bad line, a bit more hurry
        STA $D016
        LDA SHIFTH,Y
        STA $D018
        INY
        LDX #7          ; New bad line coming up
        JMP LOOP6

OVER    LDA #$97        ; Video bank to 'normal'
        STA $DD00
        LDA #22         ; Same with the charset
        STA $D018
        LDA #8          ; and the horizontal scroll register
        STA $D016

        LDA $DC00       ; Let's check the joysticks
        AND $DC01
        TAX
        LDY SPEED
        AND #8          ; Turned right, add speed
        BNE EIP
        INY
        CPY #4          ; Don't store, too much speed
        BPL EIP
        STY SPEED
EIP     TXA
        AND #4          ; Turned left
        BNE ULOS
        DEY
        CPY #$FC        ; Too much ?
        BMI ULOS
        STY SPEED
ULOS    LDA VALUE       ; Add speed to value (signed)
        CLC
        ADC SPEED
        BPL OK
        LDA SPEED       ; Banged to the side ?
        EOR #$FF
        CLC
        ADC #1
        STA SPEED
        LDA VALUE
OK      STA VALUE
        LSR             ; Value is twice the shift
        TAX             ; Remember the shift
        AND #7          ; lowest 3 bits
        ORA #8          ; (screen 40 chars wide)
        LDY POINTER
        STA SHIFTL,Y
        TXA
        LSR
        LSR
        LSR             ; highest 3 bits too
        ASL             ;  multiplicated by two
        STA SHIFTH,Y
        DEC POINTER

        LDA #1          ; Ack the interrupt
        STA $D019
        JMP $EA31       ; The normal interrupt routine

TEXT    !text "THIS IS TECH-TECH FOR C=64 BY ME" ; Test text
                        ; SCR converts to screen codes
