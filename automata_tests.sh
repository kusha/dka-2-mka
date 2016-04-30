# Project: DKA-2-MKA (FLP 15/16 FUN)
# Author: Mark Birger (xbirge00)
# Date: 7.4.2016
# Automatic tests

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

make
for i in "automatas/input"/*
do
	f=$(basename $i)
	printf "$f"

   	./dka-2-mka -t "automatas/input/$f" > "automatas/output/$f"  2> /dev/null

   	hash1=`cat automatas/output/$f | md5sum`
	hash2=`cat automatas/valid/$f | md5sum`
	if [ "$hash1" = "$hash2" ]
	then
	    printf " ${GREEN}passed${NC}\n"
	else
	    printf " ${RED}failed${NC}\n"
	fi
done
make clean
