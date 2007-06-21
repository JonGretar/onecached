
ERLC=erlc
ERLC_FLAGS=

INCLUDES = $(wildcard *.hrl)
SOURCES = $(wildcard *.erl)
BEAMS = $(SOURCES:.erl=.beam)

all: $(BEAMS)

%.beam: %.erl $(INCLUDES)
	$(ERLC)	-W $(ERLC_FLAGS) $<

clean:
	rm -f *.beam *~