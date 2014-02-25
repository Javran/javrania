.PHONY: clean

javrania: ./javrania.hs
	ghc $^

clean:
	rm -f ./javrania{,.hi,.o}

