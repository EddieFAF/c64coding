#importonce
                        // zero page registers
.var REG_ZERO_I_DEV_NUM      = $99
.var REG_ZERO_O_DEV_NUM      = $9a
.var REG_ZERO_DEVICE_NO      = $ba


                        // common registers
.var REG_INTSERVICE_LOW      = $0314              // interrupt service routine low byte
.var REG_INTSERVICE_HIGH     = $0315              // interrupt service routine high byte
.var REG_STOP_LOW            = $0328
.var REG_STOP_HIGH           = $0329
.var REG_SCREENCTL_1         = $d011              // screen control register #1
.var REG_RASTERLINE          = $d012              // raster line position 
.var REG_SCREENCTL_2         = $d016    // screen control register #2
.var REG_MEMSETUP            = $d018    // memory setup register
.var REG_INTFLAG             = $d019    // interrupt flag register
.var REG_INTCONTROL          = $d01a    // interrupt control register
.var REG_BORCOLOUR           = $d020    // border colour register
.var REG_BGCOLOUR            = $d021    // background colour register
.var REG_SID                 = $d400
.var REG_SID_VOLUME          = $d418              // sid volume & filter register
.var REG_INTSTATUS_1         = $dc0d              // interrupt control and status register #1
.var REG_INTSTATUS_2         = $dd0d              // interrupt control and status register #2


                        // kernal routines
.var K_SETLFS                = $ffba
.var K_SETNAME               = $ffbd
.var K_CLOSE_FILE            = $ffc3
.var K_CLOSE_CHANNEL         = $ffcc
.var K_LOAD_FILE             = $ffd5
