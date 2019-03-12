JAVA=java
KICKASS=$(JAVA) -jar c:\Users\DKX80DG\Downloads\C64\Tools\\Assembler\KickAssembler\KickAss.jar
TASS64=64tass
EXOMIZER=c:\Users\DKX80DG\Downloads\C64\Tools\Exomizer\win32\exomizer.exe
EXOMIZERFLAGS=sfx basic -n
CC1541=/cygdrive/c/Users/DKX80DG/Downloads/C64/Tools/WinVICE-3.1-x86/c1541.exe
BITMAP2SPR=bitmap2spr.py
VICE=/cygdrive/c/Users/DKX80DG/Downloads/C64/Tools/WinVICE-3.1-x86/x64.exe
VICEFLAGS=-keybuf "\88"
SOURCEFILES=loader.s
OBJECTS=loader.prg.exo
 
%.prg: %.s
	$(KICKASS) $< -o $@
 
%.prg: %.asm
	$(TASS64) $< -o $@
 
%.spr: %.bmp
	$(BITMAP2SPR) $< $@
 
%.prg.exo: %.prg
	$(EXOMIZER) $(EXOMIZERFLAGS) $< -o $@
 
all: demo.d64
 
vice: demo.d64
	$(VICE) $(VICEFLAGS) $<
 
loader.prg: Nightshift.sid
 
part1.prg: lib.s sid/part1.sid gfx/picture.prg
 
gfx/picture.prg: lib.s gfx/picture.png
 
part2.prg: lib.s sid/part2.sid gfx/spritelogo.spr
 
demo.d64: $(OBJECTS)
	$(CC1541) -format "demo,id" d64 demo.d64
	$(CC1541) -attach demo.d64 -write loader.prg.exo "loader"

 
clean:
	rm -f demo.d64
	rm -f *.prg
	rm -f *.exo
	rm -f gfx/*.spr
	rm -f gfx/*.prg
	rm -f gfx/*.exo
	