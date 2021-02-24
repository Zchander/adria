CL65=/usr/bin/cl65
CA65=/usr/bin/ca65
LD65=/usr/bin/ld65

CLFLAGS=-t none -C adria.cfg
LDFLAGS=-t none -C adria.cfg

SRC = main.asm
OBJ = main.o

.PHONY: clean default

default: bios.bin

bios.bin: *.asm
	$(CL65) $(CLFLAGS) -l bios.lst -o main.bin $^ 
	cp main.bin bios.bin
	cat main.bin >> bios.bin
	
clean:
	rm -f *.lst *.bin *.o *.map *.vice
