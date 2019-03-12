#importonce         // <-- This is a library, we only want it included once
//.filenamespace MyMacros   // <-- Sets up a 'MyMacros' namespace for this file

.macro SetBorderColor(color) {
  lda #color
  sta $d020
}

.macro SetBackgroundColor(color) {
  lda #color
  sta $d021
}

.macro SetMultiColor1(color) {
  lda #color
  sta $d022
}

.macro SetMultiColor2(color) {
  lda #color
  sta $d023
}

.macro SetMultiColorMode() {
  lda $d016
  ora #16
  sta $d016 
}

.macro SetScrollMode() {
  lda $D016
  eor #%00001000
  sta $D016
}

// Set VIC banks
// $DD00 = %xxxxxx11 -> bank0: $0000-$3fff
// $DD00 = %xxxxxx10 -> bank1: $4000-$7fff
// $DD00 = %xxxxxx01 -> bank2: $8000-$bfff
// $DD00 = %xxxxxx00 -> bank3: $c000-$ffff
.macro SetVICBank0() {
  lda $DD00
  and #%11111100
  ora #%00000011
  sta $DD00
}

.macro SetVICBank1() {
  lda $DD00
  and #%11111100
  ora #%00000010
  sta $DD00
}

.macro SetVICBank2() {
  lda $DD00
  and #%11111100
  ora #%00000001
  sta $DD00
}

.macro SetVICBank3() {
  lda $DD00
  and #%11111100
  ora #%00000000
  sta $DD00
}

// Clear Color RAM
.macro ClearColorRam(clearByte) {
  lda #clearByte
  ldx #0
!loop:
  sta $D800, x
  sta $D800 + $100, x
  sta $D800 + $200, x
  sta $D800 + $300, x
  inx
  bne !loop-
}

// Clear Screen
.macro ClearScreen(screen, clearByte) {
  lda #clearByte
  ldx #0
!loop:
  sta screen, x
  sta screen + $100, x
  sta screen + $200, x
  sta screen + $300, x
  inx
  bne !loop-
}

// Set screen and char location
.macro SetScreenAndCharLocation(screen, charset) {
  lda #[[screen & $3FFF] / 64] | [[charset & $3FFF] / 1024]
  sta $D018
}

// screenChars  = $0400 ; the 40x25 buffer
// screenPixels = $1000 ; the pixel data for font or bitmap ($1000 or $9000 are always charrom)
.macro SetScreen(screenChars, screenPixels) {
// Select VIC bank
 lda # ((screenChars ^ $ffff) >> 14)
 sta $dd00

// Set VIC screen and font pointers
 lda # (((screenChars & $3fff) / $0400) << 4) + (((screenPixels & $3fff) / $0800) << 1)
 sta $d018
}
 
//----------------------------------------------------------
// Code for creating the breakpoint file sent to Vice.
//----------------------------------------------------------
.var _useBinFolderForBreakpoints = cmdLineVars.get("usebin") == "true"
.var _createDebugFiles = cmdLineVars.get("afo") == "true"
.print "File creation " + [_createDebugFiles
 ? "enabled (creating breakpoint file)"
 : "disabled (no breakpoint file created)"]
.var brkFile
.if(_createDebugFiles) {
 .if(_useBinFolderForBreakpoints)
 .eval brkFile = createFile("bin/breakpoints.txt")
 else
 .eval brkFile = createFile("breakpoints.txt")
}
.macro break() {
.if(_createDebugFiles) {
 .eval brkFile.writeln("break " + toHexString(*))
 }
}
//------------------------------------------------------

.macro equalCharPack(filename, screenAdr, charsetAdr) {
	.var charMap = Hashtable()
	.var charNo = 0
	.var screenData = List()
	.var charsetData = List()
	.var pic = LoadPicture(filename)

	// Graphics should fit in 8x8 Single collor / 4 x 8 Multi collor blocks
	.var PictureSizeX = pic.width/8
	.var PictureSizeY = pic.height/8

	.for (var charY=0; charY<PictureSizeY; charY++) {
		.for (var charX=0; charX<PictureSizeX; charX++) {
			.var currentCharBytes = List()
			.var key = ""
			.for (var i=0; i<8; i++) {
				.var byteVal = pic.getSinglecolorByte(charX, charY*8 + i)
				.eval key = key + toHexString(byteVal) + ","
				.eval currentCharBytes.add(byteVal)
			}
			.var currentChar = charMap.get(key)
			.if (currentChar == null) {
				.eval currentChar = charNo
				.eval charMap.put(key, charNo)
				.eval charNo++
				.for (var i=0; i<8; i++) {
					.eval charsetData.add(currentCharBytes.get(i))
				}
			}
			.eval screenData.add(currentChar)
		}
	}
	.pc = screenAdr "screen"
	.fill screenData.size(), screenData.get(i)
	.pc = charsetAdr "charset"
	.fill charsetData.size(), charsetData.get(i)
}
