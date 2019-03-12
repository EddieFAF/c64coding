JAVA=java
KICKASS=$(JAVA) -jar /usr/share/java/KickAss.jar
TASS64=64tass
EXOMIZER=exomizer
EXOMIZERFLAGS=sfx basic -n
CC1541=cc1541
BITMAP2SPR=bitmap2spr.py
VICE=x64
VICEFLAGS=-sidenginemodel 1803 -keybuf "\88"
SOURCEFILES=intro.s part1.s part2.s part3.asm gfx/picture.s
OBJECTS=intro.prg.exo part1.prg.exo part2.prg.exo part3.prg.exo
 
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
	$(X64) $(X64FLAGS) $<
 
intro.prg: lib.s sid/introtune.sid gfx/spritelogo.spr
 
part1.prg: lib.s sid/part1.sid gfx/picture.prg
 
gfx/picture.prg: lib.s gfx/picture.png
 
part2.prg: lib.s sid/part2.sid gfx/spritelogo.spr
 
demo.d64: $(OBJECTS)
	$(CC1541) -f "DEMO" -w intro.prg.exo \
	-f "PART1" -w part1.prg.exo \
	-f "PART2" -w part2.prg.exo \
	-f "PART3" -w part3.prg.exo \
	$@
 
clean:
	rm -f demo.d64
	rm -f *.prg
	rm -f *.exo
	rm -f gfx/*.spr
	rm -f gfx/*.prg
	rm -f gfx/*.exo
	