all: $(patsubst %.msc,%.eps, $(wildcard *.msc))

%.png: %.msc
	mscgen -T png $<

%.eps: %.msc
	mscgen -T eps $<
