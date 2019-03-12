//-------------------------------------------------------------------------------
// Unpacked loading only -configuration
//-------------------------------------------------------------------------------

.const TWOBIT_PROTOCOL   = false      //Nonzero to use 2-bit protocol which may delay
                                //interrupts and does not allow sprites, but is
                                //faster. Zero to use 1-bit protocol which is
                                //the opposite.
.const LONG_NAMES      = true        //Set to nonzero to use long names (pointer in
                                //X,Y) or zero to use 2-letter names (letters
                                //in X,Y)
.const BORDER_FLASHING = false        //Set to nonzero to enable border flashing
                                //when fastloading :)
.const ADDITIONAL_ZEROPAGE = true    //Set to nonzero to use additional zeropage
                                //variables to shorten loader code
.const LOAD_UNDER_IO   = true        //Set to nonzero to enable possibility to load
                                //under I/O areas, and to load packed data
                                //under the Kernal ROM.
.const LOADFILE_UNPACKED = true      //Set to nonzero to include unpacked loading
.const LOADFILE_EXOMIZER = false      //Set to nonzero to include EXOMIZER loading
.const LOADFILE_PUCRUNCH = false      //Set to nonzero to include PUCRUNCH loading

.const LITERAL_SEQUENCES_NOT_USED = false  //(EXOMIZER only): set to nonzero for shorter
                                //depacker, if you use -c switch to disable
                                //literal sequences in Exomizer 2, or if you
                                //use Exomizer 1.
.const FORWARD_DECRUNCHING = false    //(EXOMIZER only): set to nonzero if you use -f
                                //switch in Exomizer 2, zero for Exomizer 1.

.const RETRIES         = 5        //Retries when reading a sector

.var loadbuffer      = $0400    //256 byte table used by fastloader

.var depackbuffer    = $0500    //156 bytes for EXOMIZER tables, 31 for
                                //PUCRUNCH.

.var zpbase          = $74      //Zeropage base address. Loader needs 2
                                //addresses with unpacked, 3 with PUCRUNCH
                                //and 8 with EXOMIZER loading.

.var zpbase2         = $7c      //Additional 4 zeropage addresses for shortening
                                //the loader code (optional)
