.POSIX:

CSC ?= chicken-csc
BIN=corpus
SRCS=corpus.scm
LINKS=corpus.link

all: $(BIN)

$(BIN): $(SRCS)
	$(CSC) -static -O3 -d0 -o $(BIN) $(SRCS)

clean:
	rm -f $(BIN) $(LINKS)

.PHONY: all clean
