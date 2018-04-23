;*******************************************************************************
;*** Zero Page Registers                                                     ***
;*******************************************************************************
REG_ZERO_I_DEV_NUM      = $99
REG_ZERO_O_DEV_NUM      = $9a
REG_ZERO_DEVICE_NO      = $ba

;*******************************************************************************
;*** Common Registers                                                        ***
;*******************************************************************************
REG_INTSERVICE_LOW      = $0314     ; interrupt service routine low byte
REG_INTSERVICE_HIGH     = $0315     ; interrupt service routine high byte
REG_STOP_LOW            = $0328
REG_STOP_HIGH           = $0329
REG_SCREENCTL_1         = $d011     ; screen control register #1
REG_RASTERLINE          = $d012     ; raster line position 
REG_SCREENCTL_2         = $d016     ; screen control register #2
REG_MEMSETUP            = $d018     ; memory setup register
REG_INTFLAG             = $d019     ; interrupt flag register
REG_INTCONTROL          = $d01a     ; interrupt control register
REG_BORCOLOUR           = $d020     ; border colour register
REG_BGCOLOUR            = $d021     ; background colour register
REG_SID                 = $d400
REG_SID_VOLUME          = $d418     ; sid volume & filter register
REG_INTSTATUS_1         = $dc0d     ; interrupt control and status register #1
REG_INTSTATUS_2         = $dd0d     ; interrupt control and status register #2

;*******************************************************************************
;*** Kernal Routines                                                         ***
;*******************************************************************************
K_SETLFS                = $ffba
K_SETNAME               = $ffbd
K_CLOSE_FILE            = $ffc3
K_CLOSE_CHANNEL         = $ffcc
K_LOAD_FILE             = $ffd5

;*******************************************************************************
;*** Farben                                                                  ***
;*******************************************************************************
COLOR_BLACK         = $00           ;schwarz
COLOR_WHITE         = $01           ;weiß
COLOR_RED           = $02           ;rot
COLOR_CYAN          = $03           ;türkis
COLOR_PURPLE        = $04           ;lila
COLOR_GREEN         = $05           ;grün
COLOR_BLUE          = $06           ;blau
COLOR_YELLOW        = $07           ;gelb
COLOR_ORANGE        = $08           ;orange
COLOR_BROWN         = $09           ;braun
COLOR_PINK          = $0a           ;rosa
COLOR_DARKGREY      = $0b           ;dunkelgrau
COLOR_GREY          = $0c           ;grau
COLOR_LIGHTGREEN    = $0d           ;hellgrün
COLOR_LIGHTBLUE     = $0e           ;hellblau
COLOR_LIGHTGREY     = $0f           ;hellgrau
 
;*******************************************************************************
;*** Die VIC II Register  -  ANFANG                                          ***
;*******************************************************************************
VICBASE             = $d000         ;(RG) = Register-Nr.
SPRITE0X            = $d000         ;(00) X-Position von Sprite 0
SPRITE0Y            = $d001         ;(01) Y-Position von Sprite 0
SPRITE1X            = $d002         ;(02) X-Position von Sprite 1
SPRITE1Y            = $d003         ;(03) Y-Position von Sprite 1
SPRITE2X            = $d004         ;(04) X-Position von Sprite 2
SPRITE2Y            = $d005         ;(05) Y-Position von Sprite 2
SPRITE3X            = $d006         ;(06) X-Position von Sprite 3
SPRITE3Y            = $d007         ;(07) Y-Position von Sprite 3
SPRITE4X            = $d008         ;(08) X-Position von Sprite 4
SPRITE4Y            = $d009         ;(09) Y-Position von Sprite 4
SPRITE5X            = $d00a         ;(10) X-Position von Sprite 5
SPRITE5Y            = $d00b         ;(11) Y-Position von Sprite 5
SPRITE6X            = $d00c         ;(12) X-Position von Sprite 6
SPRITE6Y            = $d00d         ;(13) Y-Position von Sprite 6
SPRITE7X            = $d00e         ;(14) X-Position von Sprite 7
SPRITE7Y            = $d00f         ;(15) Y-Position von Sprite 7
SPRITESMAXX         = $d010         ;(16) Höhstes BIT der jeweiligen X-Position
                                    ;        da der BS 320 Punkte breit ist reicht
                                    ;        ein BYTE für die X-Position nicht aus!
                                    ;        Daher wird hier das 9. Bit der X-Pos
                                    ;        gespeichert. BIT-Nr. (0-7) = Sprite-Nr.
SPRITEACTIV         = $d015         ;(21) Bestimmt welche Sprites sichtbar sind
                                    ;        Bit-Nr. = Sprite-Nr.
SPRITEDOUBLEHEIGHT  = $d017         ;(23) Doppelte Höhe der Sprites
                                    ;        Bit-Nr. = Sprite-Nr.
SPRITEDEEP          = $d01b         ;(27) Legt fest ob ein Sprite vor oder hinter
                                    ;        dem Hintergrund erscheinen soll.
                                    ;        Bit = 1: Hintergrund vor dem Sprite
                                    ;        Bit-Nr. = Sprite-Nr.
SPRITEMULTICOLOR    = $d01c         ;(28) Bit = 1: Multicolor Sprite 
                                    ;        Bit-Nr. = Sprite-Nr.
SPRITEDOUBLEWIDTH   = $d01d         ;(29) Bit = 1: Doppelte Breite des Sprites
                                    ;        Bit-Nr. = Sprite-Nr.
SPRITESPRITECOLL    = $d01e         ;(30) Bit = 1: Kollision zweier Sprites
                                    ;        Bit-Nr. = Sprite-Nr.
                                    ;        Der Inhalt wird beim Lesen gelöscht!!
SPRITEBACKGROUNDCOLL= $d01f         ;(31) Bit = 1: Sprite / Hintergrund Kollision
                                    ;        Bit-Nr. = Sprite-Nr.
                                    ;        Der Inhalt wird beim Lesen gelöscht!
SPRITEMULTICOLOR0   = $d025         ;(37) Spritefarbe 0 im Multicolormodus
SPRITEMULTICOLOR1   = $d026         ;(38) Spritefarbe 1 im Multicolormodus
SPRITE0COLOR        = $d027         ;(39) Farbe von Sprite 0
SPRITE1COLOR        = $d028         ;(40) Farbe von Sprite 1
SPRITE2COLOR        = $d029         ;(41) Farbe von Sprite 2
SPRITE3COLOR        = $d02a         ;(42) Farbe von Sprite 3
SPRITE4COLOR        = $d02b         ;(43) Farbe von Sprite 4
SPRITE5COLOR        = $d02c         ;(44) Farbe von Sprite 5
SPRITE6COLOR        = $d02d         ;(45) Farbe von Sprite 6
SPRITE7COLOR        = $d02e         ;(46) Farbe von Sprite 7
 
;*******************************************************************************
;*** Die VIC II Register  -  ENDE                                            ***
;*******************************************************************************
