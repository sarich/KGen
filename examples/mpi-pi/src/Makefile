FC = mpif90
FFLAGS = -g
build: demo

clean:
	rm -f pi3.o demo pi3_mod.mod

run:
	mpirun -np 8 ./demo

pi3.o: pi3.F90
	${FC} ${FFLAGS} -c pi3.F90

demo: demo.F90 pi3.o
	${FC} ${FFLAGS} pi3.o demo.F90 -o demo
