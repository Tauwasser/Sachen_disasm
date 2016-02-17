.PHONY = all clean

all: 16in1.gb

clean:
	rm -rf *.o
	rm -rf *.map
	rm -rf *.sym

16in1.gb : 16in1.o
	rgblink -n 16in1.sym -m 16in1.map -p 0xFF -o 16in1.gb 16in1.o

16in1.o : 16in1.asm
	rgbasm -h -o 16in1.o 16in1.asm