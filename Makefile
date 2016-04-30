# Project: DKA-2-MKA (FLP 15/16 FUN)
# Author: Mark Birger (xbirge00)
# Date: 7.4.2016
# Makefile

all:
	ghc AutomataMain.hs -o dka-2-mka

clean:
	rm ./dka-2-mka ./*.hi ./*.o