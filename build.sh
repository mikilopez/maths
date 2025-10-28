#!/bin/bash
entrada="src/"
salida="bin-linux/"
fpc -vt0w -O4 -XXs ${entrada}maths.pas -o${salida}maths
rm ${salida}*.o ${salida}*.ppu
