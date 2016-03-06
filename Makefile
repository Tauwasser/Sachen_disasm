MD5 := md5sum -c

.SUFFIXES: 
.PHONY = all clean compare

roms := 16in1.gb

all: $(roms)

clean:
	rm -rf *.o
	rm -rf *.map
	rm -rf *.sym
	rm -f $(roms)

compare: 16in1.gb
	@$(MD5) roms.md5

%.asm: ;

%.gb : %.o
	rgblink -n $(basename $@).sym -m $(basename $@).map -p 0xFF -o $@ $<

%.o : %.asm
	rgbasm -h -o $@ $<
