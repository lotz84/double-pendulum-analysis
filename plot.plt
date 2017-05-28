set terminal png
set view map
set dgrid3d
set pm3d interpolate 10, 10
splot "source.dat" using 1:2:3 with pm3d
