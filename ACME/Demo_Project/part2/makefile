ACME=c:\Users\DKX80DG\Downloads\C64\Tools\Assembler\ACME\acme.exe
EXOMIZER=c:\Users\DKX80DG\Downloads\C64\Tools\Exomizer\win32\exomizer.exe
EXOMIZERFLAGS=sfx basic -n
CC1541=cc1541
SOURCEFILES=intro.s part1.s part2.s
OBJECTS=loader.prg.exo part1.prg.exo part2.prg.exo
PACKEDOBJECTS=part2.prg.exo
 
%.prg: %.asm
	$(ACME) -v3 -f cbm -o $@ $<
 
%.prg.exo: %.prg
	$(EXOMIZER) $(EXOMIZERFLAGS) $< -o $@
 
all: $(PACKEDOBJECTS)

part2.prg: lazy_jones.64c
demo.d64: $(OBJECTS)
	$(CC1541) -f "DEMO" -w intro.prg.exo \
	-f "PART1" -w part1.prg.exo \
	-f "PART2" -w part2.prg.exo \
	-f "PART3" -w part3.prg.exo \
	$@

clean:
	rm -f *.exo
	rm -f *.prg
	rm demo.d64
