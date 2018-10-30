#!/bin/bash
for var in "$@"
do
	s=${var##*/}
	base=${s%.md}
  echo 'converting' $var
  pandoc -o $base.pdf --template=preamble.txt $var
	echo 'pdf done!'
done

# ./pandoc.sh Akel_fig* oder  ./pandoc.sh *.svg
