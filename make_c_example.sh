#/bin/sh

gprclean -Pspark_norx_c.gpr

gprbuild -Pspark_norx_c.gpr -Xload_store=explicit -Xmode=debug -j0

gcc -o ./exec/norx_c_debug -I./src/c/ -L./lib/ -lspark_norx_c ./src/c_tests/norx_c_debug.c

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:./lib/

./exec/norx_c_debug
