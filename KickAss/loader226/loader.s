//------------------------------------------------------------------------------
// COVERT BITOPS Autoconfiguring Loader/Depacker V2.26
// with 1541/1571/1581/CMD FD/CMD HD/IDE64/Fastdrive-emu autodetection & support
//
// EXOMIZER 2 depack by Magnus Lind & Krill
// PUCRUNCH depack by Pasi Ojala
// 1581/CMD FD/CMD HD information from Ninja & DocBacardi /The Dreams
// 2MHz 2-bit transfer delay code by MagerValp
// Rest by Lasse Öörni
//
// Thanks to K.M/TABOO for inspiration on badline detection and 1-bit transfer,
// and Marko Mäkelä for his original irqloader.s (huge inspiration)
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Include your loader configuration file at this point!
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Defines derived from the compile options (need not be changed)
//------------------------------------------------------------------------------

.var loadtempreg     = zpbase2+0      //Temp variables for the loader
.var bufferstatus    = zpbase2+1      //Bytes in fastload buffer
.var fileopen        = zpbase2+2      //File open indicator
.var fastloadstatus  = zpbase2+3      //Fastloader active indicator

.var destlo          = zpbase+0
.var desthi          = zpbase+1

//------------------------------------------------------------------------------
// Other defines
//------------------------------------------------------------------------------

.var MW_LENGTH       = 32            //Bytes in one M-W command

.var status          = $90           //Kernal zeropage variables
.var messages        = $9d
.var fa              = $ba

.var acsbf           = $01           //Diskdrive variables: Buffer 1 command
.var trkbf           = $08           //Buffer 1 track
.var sctbf           = $09           //Buffer 1 sector
.var iddrv0          = $12           //Disk drive ID
.var drvtemp         = $06           //Temp variable
.var id              = $16           //Disk ID
.var buf             = $0400         //Sector data buffer
.var drvstart        = $0500         //Start of drivecode
.var initialize      = $d005         //Initialize routine in 1541 ROM

.var ciout           = $ffa8         //Kernal routines
.var listen          = $ffb1
.var second          = $ff93
.var unlsn           = $ffae
.var talk            = $ffb4
.var tksa            = $ff96
.var untlk           = $ffab
.var acptr           = $ffa5
.var chkin           = $ffc6
.var chkout          = $ffc9
.var chrin           = $ffcf
.var chrout          = $ffd2
.var close           = $ffc3
.var open            = $ffc0
.var setmsg          = $ff90
.var setnam          = $ffbd
.var setlfs          = $ffba
.var clrchn          = $ffcc
.var getin           = $ffe4
.var load            = $ffd5
.var save            = $ffd8

//------------------------------------------------------------------------------
// Resident portion of loader (routines that you're going to use at runtime)
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// LOADFILE
//
// Loads an unpacked file
//
// Parameters: X (low),Y (high): Address of null-terminated filename
// Returns: C=0 OK, C=1 error (A holds errorcode)
// Modifies: A,X,Y
//------------------------------------------------------------------------------

loadfile:       jsr openfile
                jsr getbyte             //Get startaddress lowbyte
                bcs loadfile_fail       //If EOF at first byte, error
                sta destlo
                jsr getbyte             //Get startaddress highbyte
                sta desthi
                ldy #$00
loadfile_loop:  jsr getbyte
                bcs loadfile_eof
                .if (LOAD_UNDER_IO) {
                jsr disableio           //Allow loading under I/O area
                }
                sta (destlo),y
                .if (LOAD_UNDER_IO) {
                jsr enableio
                }
                iny
                bne loadfile_loop
                inc desthi
                jmp loadfile_loop
loadfile_eof:   cmp #$01                //Returncode 0 = OK, others error
loadfile_fail:  rts

//------------------------------------------------------------------------------
// OPENFILE
//
// Opens a file either with slow or fast loader. If a file is already open, does
// nothing!
//
// Parameters: X (low),Y (high): Address of null-terminated filename
// Returns: -
// Modifies: A,X,Y
//------------------------------------------------------------------------------

openfile:       lda fileopen            //A file already open?
                beq open_ok
                rts
open_ok:        .if (LONG_NAMES) {
                stx destlo
                sty desthi
                } else {
                stx filename
                sty filename+1
                }
                inc fileopen            //File opened
                lda usefastload
                bne fastopen

//------------------------------------------------------------------------------
// SLOWOPEN
//
// Opens a file without fastloader.
//
// Parameters: A:0 (it always is at this point)
// Returns: -
// Modifies: A,X,Y
//------------------------------------------------------------------------------

slowopen:       .if (LONG_NAMES) {
                tay
                }
                jsr kernalon
                .if (LONG_NAMES) {
slowopen_nameloop:
                iny
                lda (destlo),y
                bne slowopen_nameloop
                tya
                ldx destlo
                ldy desthi
                } else {
                lda #$03
                ldx #<filename
                ldy #>filename
                }
                jsr setnam
                lda #$02
                ldy #$00
                jsr setlfsdevice
                jsr open
                ldx #$02                //File number
                jmp chkin

//------------------------------------------------------------------------------
// FASTOPEN
//
// Opens a file with fastloader. Uses an asynchronous protocol inspired by
// Marko Mäkelä's work when sending the filename.
//
// Parameters: -
// Returns: -
// Modifies: A,X,Y
//------------------------------------------------------------------------------

fastopen:       jsr initfastload        //If fastloader is not yet initted,
                                        //init it now
                ldy #$00
fastload_sendouter:
                lda (destlo),y
                sta loadtempreg
                pha
                ldx #$08                //Bit counter
fastload_sendinner:
                bit $dd00               //Wait for both DATA & CLK to go high
                bpl fastload_sendinner
                bvc fastload_sendinner
                .if (LONG_NAMES==0) {
                lsr filename,x
                } else {
                lsr loadtempreg
                }
                lda #$10
                ora $dd00
                bcc fastload_zerobit
                eor #$30
fastload_zerobit:
                sta $dd00
                lda #$c0                //Wait for CLK & DATA low (answer from
fastload_sendack:                       //the diskdrive)
                bit $dd00
                bne fastload_sendack
                lda #$ff-$30            //Set DATA and CLK high
                and $dd00
                sta $dd00
                .if (LONG_NAMES) {
                dex
                bne fastload_sendinner
                iny
                pla
                bne fastload_sendouter
                } else {
                dey
                bne fastload_sendinner
                dex
                bpl fastload_sendouter
                }
                sta $d07a               //SCPU to slow mode
fastload_predelay:
                dex                     //Delay to make sure the 1541 has
                bne fastload_predelay   //set DATA high before we continue

fastload_fillbuffer:
                sta $d07a               //SCPU to slow mode
                
                .if (TWOBIT_PROTOCOL) {

                ldx #$00
fastload_fbwait:
                bit $dd00               //Wait for 1541 to signal data ready by
                bmi fastload_fbwait     //setting DATA low
fastload_fbloop:
                sei
fastload_waitbadline:
                lda $d011               //Check that a badline won't disturb
                clc                     //the timing
                sbc $d012
                and #$07
                beq fastload_waitbadline
                lda $dd00
                ora #$10
                sta $dd00               //Set CLK low
fastload_delay: bit $00                 //Delay (NTSC version, will be modified for PAL)
                nop
                and #$03
                sta fastload_eor+1
                sta $dd00               //Set CLK high
                lda $dd00
                lsr
                lsr
                eor $dd00
                lsr
                lsr
                eor $dd00
                lsr
                lsr
fastload_eor:   eor #$00
                eor $dd00
                cli
                sta loadbuffer,x
                inx
                bne fastload_fbloop

                } else {

fastload_fbwait:bit $dd00                 //Wait for 1541 to signal data ready by
                bvc fastload_fbwait       //setting CLK high
                pha                       //Some delay before beginning
                pla
                pha
                pla
                ldx #$00
fastload_fillbufferloop:                  //1bit receive code
                nop
                nop
                nop
                ldy #$08                  //Bit counter
fastload_bitloop:
                nop
                lda #$10
                eor $dd00                 //Take databit
                sta $dd00                 //Store reversed clockbit
                asl
                ror loadbuffer,x
                dey
                bne fastload_bitloop
                .if (BORDER_FLASHING) {
                dec $d020
                inc $d020
                }
                inx
                bne fastload_fillbufferloop
                
                }

fillbuffer_common:
                stx bufferstatus                //X is 0 here
                ldx #$fe
                lda loadbuffer                  //Full 254 bytes?
                bne fastload_fullbuffer
                ldx loadbuffer+1                //End of load?
                bne fastload_noloadend
                stx fileopen                    //Clear fileopen indicator
                lda loadbuffer+2                //Read the return/error code
                sta fileclosed+1
fastload_noloadend:
                dex
fastload_fullbuffer:
                stx fastload_endcomp+1
fileclosed:     lda #$00
                sec
                rts

//------------------------------------------------------------------------------
// GETBYTE
//
// Gets a byte from an opened file.
//
// Parameters: -
// Returns: C=0 OK, A contains byte
//          C=1 File stream ended. A contains the error code:
//              $00 - OK, end of file
//              $01 - Sector read error (only with fastloading)
//              $02 - File not found
// Modifies: A
//------------------------------------------------------------------------------

getbyte:        lda fileopen
                beq fileclosed
                stx getbyte_restx+1
                sty getbyte_resty+1
getbyte_fileopen:
                lda usefastload
                beq slowload_getbyte
fastload_getbyte:
                ldx bufferstatus
                lda loadbuffer+2,x
                inx
fastload_endcomp:cpx #$00                       //Reach end of buffer?
                stx bufferstatus
                bcc getbyte_restx
                pha
                jsr fastload_fillbuffer
                pla
getbyte_done:   clc
getbyte_restx:  ldx #$00
getbyte_resty:  ldy #$00
                rts

slowload_getbyte:
                jsr chrin
                ldx status
                beq getbyte_done
                pha
                txa
                and #$03
                sta fileclosed+1        //EOF - store return code
                dec fileopen
                jsr close_kernaloff
                pla
                ldx fileclosed+1        //Check return code, if nonzero,
                cpx #$01                //return with carry set and return
                bcc getbyte_restx       //code in A
                txa
                bcs getbyte_restx

//------------------------------------------------------------------------------
// DISABLEIO
//
// Stores $01 status, disables interrupts & IO area.
//
// Parameters: -
// Returns: -
// Modifies: -
//------------------------------------------------------------------------------

disableio:      pha
                lda $01
                sta enableio_01+1
                lda #$34
                sei
                sta $01
                pla
                rts

//------------------------------------------------------------------------------
// ENABLEIO
//
// Restores $01 status and enables interrupts
//
// Parameters: -
// Returns: -
// Modifies: -
//------------------------------------------------------------------------------

enableio:       pha
enableio_01:    lda #$36
                sta $01
                cli
                pla
                rts

//------------------------------------------------------------------------------
// SETLFSDEVICE
//
// Gets the last used device number and performs a SETLFS.
//
// Parameters: -
// Returns: -
// Modifies: X
//------------------------------------------------------------------------------

setlfsdevice:   ldx fa
                jmp setlfs

//------------------------------------------------------------------------------
// KERNALON
//
// Switches KERNAL on to prepare for slow loading. Saves state of $01.
//
// Parameters: -
// Returns: -
// Modifies: A,X
//------------------------------------------------------------------------------

kernalon:       lda $01
                sta kernaloff+1
                lda useserial
                sta slowirq
                lda #$36
                sta $01
                rts

//------------------------------------------------------------------------------
// CLOSE_KERNALOFF
//
// Closes file 2 and then restores state of $01.
//
// Parameters: -
// Returns: -
// Modifies: A
//------------------------------------------------------------------------------

close_kernaloff:lda #$02
                jsr close
                jsr clrchn

//------------------------------------------------------------------------------
// KERNALOFF
//
// Restores state of $01.
//
// Parameters: -
// Returns: -
// Modifies: A
//------------------------------------------------------------------------------

kernaloff:      lda #$36
                sta $01
                lda #$00
                sta slowirq
il_ok:          rts

//------------------------------------------------------------------------------
// INITFASTLOAD
//
// Uploads the fastloader to disk drive memory and starts it.
//
// Parameters: -
// Returns: -
// Modifies: A,X,Y
//------------------------------------------------------------------------------

initfastload:   lda usefastload         //If fastloader not needed, do nothing
                beq il_ok
                lda fastloadstatus      //If fastloader already initted,
                bne il_ok               //do nothing
                inc fastloadstatus
                lda #<drivecode
                ldx #>drivecode
                ldy #(drvend-drvstart+MW_LENGTH-1)/MW_LENGTH

ifl_begin:      sta ifl_senddata+1
                stx ifl_senddata+2
                sty loadtempreg         //Number of "packets" to send
                jsr kernalon
                lda #>drvstart
                sta ifl_mwstring+1
                ldy #$00
                sty ifl_mwstring+2      //Drivecode starts at lowbyte 0
                beq ifl_nextpacket
ifl_sendmw:     lda ifl_mwstring,x      //Send M-W command (backwards)
                jsr ciout
                dex
                bpl ifl_sendmw
                ldx #MW_LENGTH
ifl_senddata:   lda drivecode,y         //Send one byte of drivecode
                jsr ciout
                iny
                bne ifl_notover
                inc ifl_senddata+2
ifl_notover:    inc ifl_mwstring+2      //Also, move the M-W pointer forward
                bne ifl_notover2
                inc ifl_mwstring+1
ifl_notover2:   dex
                bne ifl_senddata
                jsr unlsn               //Unlisten to perform the command
ifl_nextpacket: lda fa                  //Set drive to listen
                jsr listen
                lda status
                cmp #$c0
                beq ifl_error           //Abort if serial error (IDE64!)
                lda #$6f
                jsr second
                ldx #$05
                dec loadtempreg         //All "packets" sent?
                bpl ifl_sendmw
ifl_sendme:     lda ifl_mestring-1,x    //Send M-E command (backwards)
                jsr ciout
                dex
                bne ifl_sendme
                jsr unlsn
ifl_error:      jmp kernaloff

//------------------------------------------------------------------------------
// DRIVECODE - Code executed in the disk drive.
//------------------------------------------------------------------------------

drivecode:                              //Address in C64's memory
                rorg drvstart           //Address in diskdrive's memory

drvmain:        cli                     //File loop: Get filename first
                lda #$00                //Set DATA & CLK high
drv_1800ac0:    sta $1800
                .if (LONG_NAMES) {
                ldx #$00
                } else {
                ldx #$01
                }
drv_nameloop:   ldy #$08                //Bit counter
drv_namebitloop:
drv_1800ac1:    lda $1800
                bpl drv_noquit          //Quit if ATN is low
                jmp drv_quit
drv_noquit:     and #$05                //Wait for CLK or DATA going low
                beq drv_namebitloop
                lsr                     //Read the data bit
                lda #$02                //Pull the other line low to acknowledge
                bcc drv_namezero //the bit being received
                lda #$08
drv_namezero:   ror drv_filename,x      //Store the data bit
drv_1800ac2:    sta $1800
drv_namewait:
drv_1800ac3:    lda $1800               //Wait for either line going high
                and #$05
                cmp #$05
                beq drv_namewait
                lda #$00
drv_1800ac4:    sta $1800               //Set both lines high
                dey
                bne drv_namebitloop     //Loop until all bits have been received
                sei                     //Disable interrupts after first byte
                .if (LONG_NAMES) {
                inx
                lda drv_filename-1,x    //End of filename?
                bne drv_nameloop
                } else {
                dex
                bpl drv_nameloop
                }

                .if (TWOBIT_PROTOCOL==0) {
                lda #$08                //CLK low, data isn't available
drv_1800ac5:    sta $1800
                }

drv_dirtrk:     ldx $1000
drv_dirsct:     ldy $1000               //Read disk directory
drv_dirloop:    jsr drv_readsector      //Read sector
                bcs drv_loadend         //If failed, return error code
                ldy #$02
drv_nextfile:   lda buf,y               //File type must be PRG
                and #$83
                cmp #$82
                bne drv_notfound
                .if (LONG_NAMES) {
                ldx #$03
                sty drv_namelda+1
                lda #$a0                //Make an endmark at the 16th letter
                sta buf+19,y
drv_namecmploop:lda drv_filename-3,x    //Check for wildcard first
                cmp #$2a
                beq drv_found
drv_namelda:    lda buf,x               //Check against each letter of filename,
                cmp drv_filename-3,x    //break on mismatch
                bne drv_namedone
                inx
                bne drv_namecmploop
drv_namedone:   cmp #$a0                //If got endmark in both filenames, found
                bne drv_notfound
                lda drv_filename-3,x
                beq drv_found
                } else {
                lda buf+3,y
                cmp drv_filename
                bne drv_notfound
                lda buf+4,y
                cmp drv_filename+1
                beq drv_found
                }
drv_notfound:   tya
                clc
                adc #$20
                tay
                bcc drv_nextfile
                ldy buf+1               //Go to next directory block, go on until no
                ldx buf                 //more directory blocks
                bne drv_dirloop
drv_filenotfound:
                ldx #$02                //Return code $02 = File not found
drv_loadend:    stx buf+2
                lda #$00
                sta buf
                sta buf+1
                beq drv_sendblk

drv_quit:                               //If ATN, exit to drive ROM code
drv_drivetype:  ldx #$00
                bne drv_not1541
                jmp initialize

drv_not1541:    rts

drv_found:      iny
drv_nextsect:   ldx buf,y       //File found, get starting track & sector
                beq drv_loadend //At file's end? (return code $00 = OK)
                lda buf+1,y
                tay
                jsr drv_readsector      //Read the data sector
                bcs drv_loadend
                
                .if (TWOBIT_PROTOCOL) {

drv_sendblk:    ldy #$00
                ldx #$02
drv_sendloop:   lda buf,y
                lsr
                lsr
                lsr
                lsr
drv_1800ac5:    stx $1800               //Set DATA=low for first byte, high for
                tax                     //subsequent bytes
                lda drv_sendtbl,x
                pha
                lda buf,y
                and #$0f
                tax
                lda #$04
drv_1800ac6:    bit $1800               //Wait for CLK=low
                beq drv_1800ac6
                lda drv_sendtbl,x
drv_1800ac7:    sta $1800
drv_2mhzsend:   jsr drv_delay18
                nop
                asl
                and #$0f
drv_2mhz1800ac8:sta $1800
                cmp ($00,x)
                nop
                pla
drv_2mhz1800ac9:sta $1800
                cmp ($00,x)
                nop
                asl
                and #$0f
drv_2mhz1800ac10:
                sta $1800
                ldx #$00
                iny
                bne drv_sendloop
                jsr drv_delay12
drv_2mhz1800ac11:
                stx $1800               //Finish send: DATA & CLK both high

                } else {

drv_sendblk:    lda #$04                //Bitpair counter/
                ldx #$00                //compare-value for CLK-line
drv_1800ac6:    stx $1800               //CLK & DATA high -> ready to go
drv_sendloop:   ldx buf
drv_zpac1:      stx drvtemp
                tay                     //Bitpair counter
drv_sendloop_bitpair:
                ldx #$00
drv_zpac2:      lsr drvtemp
                bcs drv_sendloop_wait1
                ldx #$02
drv_sendloop_wait1:
drv_1800ac7:    bit $1800               //Wait until CLK high
                bne drv_sendloop_wait1
drv_1800ac8:    stx $1800
                ldx #$00
drv_zpac3:      lsr drvtemp
                bcs drv_sendloop_wait2
                ldx #$02
drv_sendloop_wait2:
drv_1800ac9:    bit $1800
                beq drv_sendloop_wait2  //Wait until CLK low
drv_1800ac10:   stx $1800
                dey
                bne drv_sendloop_bitpair
                inc drv_sendloop+1
                bne drv_sendloop
drv_sendloop_endwait:
drv_1800ac11:   bit $1800               //Wait for CLK high
                bne drv_sendloop_endwait
                asl                     //Set CLK low, DATA high
drv_1800ac12:   sta $1800               //(more data yet not ready)

                }

drv_senddone:   lda buf                 //First 2 bytes zero marks end of loading
                ora buf+1               //(3rd byte is the return code)
                bne drv_nextsect
                jmp drvmain

drv_readsector: jsr drv_led
drv_readtrk:    stx $1000
drv_readsct:    sty $1000
                ldy #RETRIES            //Retry counter
drv_retry:      lda #$80
                ldx #1
drv_execjsr:    jsr drv_1541exec        //Exec buffer 1 job
                cmp #$02                //Error?
                bcc drv_success
drv_skipid:     dey                     //Decrease retry counter
                bne drv_retry
drv_failure:    ldx #$01                //Return code $01 - Read error
drv_success:    sei                     //Make sure interrupts now disabled
drv_led:        lda #$08                //Flash the drive LED
drv_ledac1:     eor $1c00
drv_ledac2:     sta $1c00
                rts

drv_1541exec:   sta $01
                cli                     //Allow interrupts & execute command
drv_1541execwait:
                lda $01
                bmi drv_1541execwait
                pha
                lda id                  //Handle disk ID change
                sta iddrv0
                lda id+1
                sta iddrv0+1
                pla
                rts

drv_fdexec:     jsr $ff54               //FD2000 fix by Ninja
                lda $03
                rts

                .if (TWOBIT_PROTOCOL) {
drv_delay18:    cmp ($00,x)
drv_delay12:    rts

drv_sendtbl:    .byte $0f,$07,$0d,$05
                .byte $0b,$03,$09,$01
                .byte $0e,$06,$0c,$04
                .byte $0a,$02,$08,$00
                }

drv_1541dirtrk: .byte 18
drv_1541dirsct: .byte 1
drv_1581dirsct: .byte 3
drv_filename:

drvend:
                rend

//------------------------------------------------------------------------------
// M-W and M-E command strings
//------------------------------------------------------------------------------

ifl_mwstring:   .byte MW_LENGTH,$00,$00,"W-M"

ifl_mestring:   .byte >drvstart, <drvstart, "E-M"

//------------------------------------------------------------------------------
// Loader variables, if not on zeropage
//------------------------------------------------------------------------------

                .if (ADDITIONAL_ZEROPAGE==0) {
loadtempreg:    .byte 0          //Temp variable for the loader
bufferstatus:   .byte 0          //Bytes in fastload buffer
fileopen:       .byte 0          //File open indicator
fastloadstatus: .byte 0          //Fastloader active indicator
                }

//------------------------------------------------------------------------------
// Filename (in short name mode)
//------------------------------------------------------------------------------

                .if (LONG_NAMES==0) {
filename:       .byte "00*"
                }

//------------------------------------------------------------------------------
// Loader configuration
//------------------------------------------------------------------------------

usefastload:    .byte 0                          //If nonzero, fastloading will
                                                //be used (autoconfigured)
useserial:      .byte 1                          //If nonzero, serial protocol
                                                //is in use and IRQs can't be
                                                //used reliably while Kernal
                                                //file I/O is in progress
slowirq:        .byte 0                          //Indicator of whether IRQs are
                                                //currently delayed

//------------------------------------------------------------------------------
// Disposable portion of loader (routines only needed when initializing)
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// INITLOADER
//
// Inits the loadersystem. Must only be called only once in the beginning.
//
// Parameters: -
// Returns: -
// Modifies: A,X,Y
//------------------------------------------------------------------------------

initloader:     sta $d07f                       //Disable SCPU hardware regs
                lda #$00
                sta messages                    //Disable KERNAL messages
                sta fastloadstatus              //Initial fastload status = off
                sta fileopen                    //No file initially open

                .if (TWOBIT_PROTOCOL) {
                tay
                sei                             //Detect PAL/NTSC by measuring
il_detectntsc1: ldx $d011                       //greatest rasterline number
                bmi il_detectntsc1
il_detectntsc2: ldx $d011
                bpl il_detectntsc2
il_detectntsc3: cpy $d012
                bcs il_detectntsc4
                ldy $d012
il_detectntsc4: ldx $d011
                bmi il_detectntsc3
                cli
                cpy #$20
                bcc il_isntsc
                lda #$2c                        //Adjust 2-bit fastload transfer
                sta fastload_delay              //delay
il_isntsc:
                }


il_detectdrive: lda #$aa
                sta $a5
                lda #<il_drivecode
                ldx #>il_drivecode
                ldy #(il_driveend-il_drivecode+MW_LENGTH-1)/MW_LENGTH
                jsr ifl_begin                   //Upload test-drivecode
                lda status                      //If serial error here, not a
                cmp #$c0                        //serial device
                beq il_noserial
                ldx #$00
                ldy #$00
il_delay:       inx                             //Delay to make sure the test-
                bne il_delay                    //drivecode executed to the end
                iny
                bpl il_delay
                lda fa                          //Set drive to listen
                jsr listen
                lda #$6f
                jsr second
                ldx #$05
il_ddsendmr:    lda il_mrstring,x               //Send M-R command (backwards)
                jsr ciout
                dex
                bpl il_ddsendmr
                jsr unlsn
                lda fa
                jsr talk
                lda #$6f
                jsr tksa
                lda #$00
                jsr acptr                       //First byte: test value
                pha
                jsr acptr                       //Second byte: drive type
                tax
                jsr untlk
                pla
                cmp #$aa                        //Drive can execute code, so can
                beq il_fastloadok               //use fastloader
                lda $a5                         //If serial bus delay counter
                cmp #$aa                        //untouched, not a serial device
                bne il_nofastload
il_noserial:    dec useserial                   //Serial bus not used: switch to
il_nofastload:  rts                             //"fake" IRQ-loading mode

il_fastloadok:  inc usefastload
                stx il_drivetype+1
                stx drv_drivetype+1-drvstart+drivecode
                lda il_1800lo,x                 //Perform patching of drivecode
                sta il_patch1800lo+1
                lda il_1800hi,x
                sta il_patch1800hi+1
                .if (TWOBIT_PROTOCOL) {
                txa                             //For 1MHz drives, need to copy
                bne il_2mhzdrive                //the 1MHz transfer code
                ldy #drv_1mhzsenddone-drv_1mhzsend-1
il_copy1mhzcode:lda il_drv1mhzsend,y
                sta drv_2mhzsend-drvstart+drivecode,y
                dey
                bpl il_copy1mhzcode
                ldy #3
il_copy1mhz1800offsets:
                lda il_1mhz1800ofs,y            //Also copy the serial port
                sta il_2mhz1800ofs,y            //access offsets
                dey
                bpl il_copy1mhz1800offsets
il_2mhzdrive:   ldy #11
                } else {
                ldy #12
                }
il_patchloop:   ldx il_1800ofs,y
il_patch1800lo: lda #$00                        //Patch all $1800 accesses
                sta drvmain+1-drvstart+drivecode,x
il_patch1800hi: lda #$00
                sta drvmain+2-drvstart+drivecode,x
                dey
                bpl il_patchloop
il_drivetype:   ldx #$00
                lda il_dirtrklo,x               //Patch directory
                sta drv_dirtrk+1-drvstart+drivecode
                lda il_dirtrkhi,x
                sta drv_dirtrk+2-drvstart+drivecode
                lda il_dirsctlo,x
                sta drv_dirsct+1-drvstart+drivecode
                lda il_dirscthi,x
                sta drv_dirsct+2-drvstart+drivecode
                lda il_execlo,x                 //Patch job exec address
                sta drv_execjsr+1-drvstart+drivecode
                lda il_exechi,x
                sta drv_execjsr+2-drvstart+drivecode
                lda il_jobtrklo,x               //Patch job track/sector
                sta drv_readtrk+1-drvstart+drivecode
                clc
                adc #$01
                sta drv_readsct+1-drvstart+drivecode
                lda il_jobtrkhi,x
                sta drv_readtrk+2-drvstart+drivecode
                adc #$00
                sta drv_readsct+2-drvstart+drivecode    
                .if (TWOBIT_PROTOCOL==0) {
                lda il_zp,x                     //Patch zeropage temp usage
                sta drv_zpac1+1-drvstart+drivecode
                sta drv_zpac2+1-drvstart+drivecode
                sta drv_zpac3+1-drvstart+drivecode
                }
                lda il_ledenabled,x             //Patch LED flashing
                sta drv_led-drvstart+drivecode
                lda il_ledbit,x
                sta drv_led+1-drvstart+drivecode
                lda il_ledadrhi,x
                sta drv_ledac1+2-drvstart+drivecode
                sta drv_ledac2+2-drvstart+drivecode
                rts

//------------------------------------------------------------------------------
// IL_DRIVECODE - Drivecode used to detect drive type & test if drivecode
// execution works OK
//------------------------------------------------------------------------------

il_drivecode:
                rorg drvstart

                asl ild_return1         //Modify first returnvalue to prove
                                        //we've executed something :)
                lda $fea0               //Recognize drive family
                ldx #3                  //(from Dreamload)
ild_floop:      cmp ild_family-1,x
                beq ild_ffound
                dex                     //If unrecognized, assume 1541
                bne ild_floop
                beq ild_idfound
ild_ffound:     lda ild_idloclo-1,x
                sta ild_idlda+1
                lda ild_idlochi-1,x
                sta ild_idlda+2
ild_idlda:      lda $fea4               //Recognize drive type
                ldx #3                  //3 = CMD HD
ild_idloop:     cmp ild_id-1,x          //2 = CMD FD
                beq ild_idfound         //1 = 1581
                dex                     //0 = 1541
                bne ild_idloop
ild_idfound:    stx ild_return2
                rts

ild_family:     .byte $43,$0d,$ff
ild_idloclo:    .byte $a4,$c6,$e9
ild_idlochi:    .byte $fe,$e5,$a6
ild_id:         .byte "8","F","H"

ild_return1:    .byte $55
ild_return2:    .byte 0

                rend

il_driveend:

//------------------------------------------------------------------------------
// IL_DRV1MHZSEND - 2-bit protocol send code for 1MHz drives
//------------------------------------------------------------------------------

                .if (TWOBIT_PROTOCOL) {
il_drv1mhzsend:
                rorg drv_2mhzsend

drv_1mhzsend:   asl
                and #$0f
drv_1mhz1800ac8:sta $1800
                pla
drv_1mhz1800ac9:sta $1800
                asl
                and #$0f
drv_1mhz1800ac10:
                sta $1800
                ldx #$00
                iny
                bne drv_sendloop
                nop
drv_1mhz1800ac11:
                stx $1800               //Finish send: DATA & CLK both high
                beq drv_senddone
drv_1mhzsenddone:

                rend
                }

il_mrstring:    .byte 2,>ild_return1,<ild_return1,"R-M"

                .if (TWOBIT_PROTOCOL) {

il_1800ofs:     .byte drv_1800ac0-drvmain
                .byte drv_1800ac1-drvmain
                .byte drv_1800ac2-drvmain
                .byte drv_1800ac3-drvmain
                .byte drv_1800ac4-drvmain
                .byte drv_1800ac5-drvmain
                .byte drv_1800ac6-drvmain
                .byte drv_1800ac7-drvmain
il_2mhz1800ofs: .byte drv_2mhz1800ac8-drvmain
                .byte drv_2mhz1800ac9-drvmain
                .byte drv_2mhz1800ac10-drvmain
                .byte drv_2mhz1800ac11-drvmain

il_1mhz1800ofs: .byte drv_1mhz1800ac8-drvmain
                .byte drv_1mhz1800ac9-drvmain
                .byte drv_1mhz1800ac10-drvmain
                .byte drv_1mhz1800ac11-drvmain

                } else {

il_1800ofs:     .byte drv_1800ac0-drvmain
                .byte drv_1800ac1-drvmain
                .byte drv_1800ac2-drvmain
                .byte drv_1800ac3-drvmain
                .byte drv_1800ac4-drvmain
                .byte drv_1800ac5-drvmain
                .byte drv_1800ac6-drvmain
                .byte drv_1800ac7-drvmain
                .byte drv_1800ac8-drvmain
                .byte drv_1800ac9-drvmain
                .byte drv_1800ac10-drvmain
                .byte drv_1800ac11-drvmain
                .byte drv_1800ac12-drvmain

                }

il_1800lo:      .byte <$1800,<$4001,<$4001,<$8000
il_1800hi:      .byte >$1800,>$4001,>$4001,>$8000

il_dirtrklo:    .byte <drv_1541dirtrk,<$022b,<$54,<$2ba7
il_dirtrkhi:    .byte >drv_1541dirtrk,>$022b,>$54,>$2ba7
il_dirsctlo:    .byte <drv_1541dirsct,<drv_1581dirsct,<$56,<$2ba9
il_dirscthi:    .byte >drv_1541dirsct,>drv_1581dirsct,>$56,>$2ba9

il_execlo:      .byte <drv_1541exec,<$ff54,<drv_fdexec,<$ff4e
il_exechi:      .byte >drv_1541exec,>$ff54,>drv_fdexec,>$ff4e

il_jobtrklo:    .byte <$0008,<$000d,<$000d,<$2802
il_jobtrkhi:    .byte >$0008,>$000d,>$000d,>$2802

                .if (TWOBIT_PROTOCOL==0) {
il_zp:          .byte $06,$0b,$0b,$06
                }

il_ledenabled:  .byte $a9,$a9,$a9,$60
il_ledbit:      .byte $08,$40,$40,$00
il_ledadrhi:    .byte $1c,$40,$40,$40
