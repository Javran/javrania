.PHONY: clean-site clean-bin clean all

all: javrania site

javrania: ./javrania.hs
	ghc $<
	./javrania clean

site: javrania
	./javrania build

clean-site: javrania
	./javrania clean

clean-bin:
	rm -f ./javrania{,.hi,.o}

clean: clean-site clean-bin

deploy:
	/bin/bash ./do-deploy.sh
