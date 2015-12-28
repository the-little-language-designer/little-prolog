.ONESHELL:

help:
	@
	echo -e "\e[33;1m"
	echo "* makefile operations"
	echo "  * clean"
	echo "* I wish you happy making ^-^"
	echo "  please read the makefile for more informations"
	echo -e "\e[0m"

tangle:
	@
	./tangle.el little-prolog.org

run:
	racket little-prolog.rkt

dev:
	make clean
	make tangle
	make run

bin:
	raco exe little-prolog.rkt

run-bin:
	time ./little-prolog

test:
	raco test little-prolog.rkt

clean:
	@
	echo -e "\e[33;1m"
	echo "* clean"
	echo -e "\e[0m"
	rm -f *~ */*~ */*/*~ */*/*/*~ */*/*/*/*~  */*/*/*/*/*~
	rm -rf compiled
	rm little-prolog
