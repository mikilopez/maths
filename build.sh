#!/bin/bash
entrada="src/"
salida="bin-linux/"
fpc -vt0wn -O4 -XX -CX -Xs ${entrada}maths.pas -o${salida}maths
rm ${salida}*.o ${salida}*.ppu
