ACME=c:\Users\DKX80DG\Downloads\C64\Tools\Assembler\ACME\acme.exe
EXOMIZER=c:\Users\DKX80DG\Downloads\C64\Tools\Exomizer\win32\exomizer.exe
EXOMIZERFLAGS=sfx basic -n
CC1541=cc1541
VICE="c:\\Users\\DKX80DG\\Downloads\\C64\\Tools\\WinVICE-3.1-x86\\x64.exe"
VICEFLAGS=-sidenginemodel 1803
SOURCEFILES=part1.asm
OBJECTS=part1.prg.exo
PACKEDOBJECTS=part1.prg.exo
 
%.prg: %.asm
	$(ACME) -v3 -f cbm -o $@ $<
 
%.prg.exo: %.prg
	$(EXOMIZER) $(EXOMIZERFLAGS) $< -o $@
 
all: $(PACKEDOBJECTS)

vice: part1.prg.exo
	$(VICE) $(VICEFLAGS) $<

part1.prg: ode_to_64.bin

demo.d64: $(OBJECTS)
	$(CC1541) -f "DEMO" -w intro.prg.exo \
	-f "PART1" -w part1.prg.exo \
	-f "PART2" -w part2.prg.exo \
	-f "PART3" -w part3.prg.exo \
	$@

clean:
	rm -f *.exo
	rm -f *.prg
	rm -f *.lst
	rm demo.d64
