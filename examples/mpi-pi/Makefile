KGEN_HOME := ../..
KGEN := ${KGEN_HOME}/bin/kgen

#PAPI_DIR := /path/to/PAPI

SRC_DIR := ${PWD}/src
SRC := ${SRC_DIR}/pi3.F90

test:
	${KGEN} \
		--mpi enable \
		--cmd-clean "cd ${SRC_DIR}; make -f Makefile clean" \
		--cmd-build "cd ${SRC_DIR}; make -f Makefile build" \
        --cmd-run "cd ${SRC_DIR}; make -f Makefile run" \
		${SRC}

clean:
	${MAKE} -f Makefile clean -C src
	rm -rf kernel state kgen.log _kgen_compflag_cmdwrapper.sh strace.log include.ini model model.ini elapsedtime coverage papi
