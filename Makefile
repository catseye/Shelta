YASM?=yasm

bin/sheltan.com: src/shelta86.s
	$(YASM) src/shelta86.s -o bin/sheltan.com

all: bin/sheltan.com

