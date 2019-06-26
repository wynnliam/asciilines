SRC=./src/*.hs
CMP=ghc
FLG=-odir ./bin -hidir ./bin -o
OUT=./bin/asciilines

.phony: clean

all: $(SRC)
	$(CMP) $(FLG) $(OUT) $(SRC)

run: $(OUT)
	$(OUT)

clean:
	rm ./bin/*
