#!/bin/bash

fweave  -C3  $1.web
ftangle -TD  $1.web

latex    $1.tex
dvipdf   $1.dvi

echo Removing erroneous line from FORTRAN file
sed '8d' $1.f > $1.temp
mv $1.temp $1.f

# remove unnecessary files
for ext in dvi 
do
   rm $1.$ext
done

#rm INDEX.tex
#rm MODULES.tex

