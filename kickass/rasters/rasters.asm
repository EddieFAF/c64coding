// 10 SYS2064

*=$0801

      .byte $0B, $08, $0A, $00, $9E, $32, $30, $36, $34, $00, $00, $00

*=$0810

start:      sei                             // Set Interrupt disabled flag to prevent interrupts
            lda #$7F                        // C64 has system set to recieve interrupts from CIA #1
            sta $DC0D                       // so we need to disable those 
            lda #<irq1                      // Set lo-.byte of 
            sta $0314                       // IRQ Vector
            lda #>irq1                      // Set hi-.byte on
            sta $0315                       // IRQ Vector

            lda #$32                        // Set rasterline 
            sta $D012                       // where we want interrupt
            lda #$1B                        // Set default value of $D011 with highest bit clear because 
            sta $D011                       // it serve as highest bit of rasterline (for lines 256 - ...)

            lda $DC0D                       // Acknowledge CIA #1 interrupts incase they happened
            lda #$FF                        // Acknowledge all VIC II
            sta $D019                       // interrupts

            lda #$01                        // And as LAST thing, enable raster interrupts on
            sta $D01A                       // VIC II 

            cli                             // Clear Interrupt disabled flag to allow interrupts again

@waitSpace: lda $DC01                       // Check if 
            and #$10                        // space is pressed?
            bne @waitSpace                  // If not, keep waiting...

            jmp $FCE2                       // Reset C64...

irq1:       ldy $D012                       // Load current rasterline to Y register
            ldx #$3F                        // We want 64 lines of rasters, so load X with value 63 ($3f hex)
@rasterColorLoop:
            lda rasterColors,X              // Load color from table of raster colors with offset of X
@rasterWait:
            cpy $D012                       // is rasterline still same?
            beq @rasterWait                 // loop as long as it is
            sta $D021                       // set color if it's new rasterline

            iny                             // increment Y register by 1 for next line comparison
            dex                             // decrement X so we get different color for next line
            bpl @rasterColorLoop            // result is still positive we have lines left so loop...

            ldx #$08                        // Small delay loop
@dummyDelay:
            dex                             // to time next color change
            bne @dummyDelay                 // 
            lda #$06                        // change background                     
            sta $D021                       // color to dark blue

            // nop                          // EXTRA DELAY for NTSC version of C64...
                                            // Correct timings are depending how many cycles there are
                                            // on each raster line... 

            lda #$FF                        // Acknowledge all VIC II
            sta $D019                       // interrupts

            jsr moveRasters

            jmp $EA81                       // Jump to last part of KERNALs regular interrupt service routine

moveRasters:
            ldx #$37                        // Loop
@moveLoop:  lda rasterColors,x              // to move
            sta rasterColors+8,x            // raster colors 8 pixels different position
            dex                             // to create one character high line of moving
            bpl @moveLoop                   // colorblocks

            ldx #$07                        // We need 8 more colors (0..7 = 8)...
            ldy colorPos                    // Read position in our colorbar table to Y
@newColorsLoop:
            tya                             // Transfer Y to A
            and #$3F                        // 64 colors in colorBars, so limit value to $00..$3f (0..64 dec) 
            tay                             // Transfer A back to Y
            lda colorBars,Y                 // load color from colorBars with offset of Y
            sta rasterColors,X              // store new color to rasterColors with offset of X
            iny                             // Increment Y
            dex                             // Decrement X
            bpl @newColorsLoop              // result is still positive we have lines left so loop...
            inc colorPos                    // Increase value of colorPosition with 1
            rts                             // We're done so RETURN

colorPos:   .byte $00                       // Position in our colorBar table

rasterColors: .fill $40, $00                // Define Constant .bytes, $40 .bytes (64 dec) of value $00

colorBars:  .byte $00, $00, $09, $09, $08, $08, $0A, $0A     // brown orange
            .byte $07, $07, $01, $01, $07, $07, $0A, $0A     // yellowish
            .byte $08, $08, $09, $09, $00, $00, $00, $00     // color bar
            .byte $00, $00, $00, $00, $00, $00, $00, $00     // 

            .byte $00, $00, $0B, $0B, $05, $05, $03, $03     // greenish
            .byte $0D, $0D, $01, $01, $0D, $0D, $03, $03     // color bar
            .byte $05, $05, $0B, $0B, $00, $00, $00, $00     // 
            .byte $00, $00, $00, $00, $00, $00, $00, $00     // 

