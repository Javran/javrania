.PHONY: clean-site clean-bin clean

javrania: ./javrania.hs
	ghc $<

site: javrania
	./javrania build

clean-site: javrania
	./javrania clean

clean-bin:
	rm -f ./javrania{,.hi,.o}

clean: clean-site clean-bin
