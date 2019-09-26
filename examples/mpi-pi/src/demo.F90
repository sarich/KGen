PROGRAM demo
    USE pi3_mod, only : pi3
    IMPLICIT NONE
    INCLUDE 'mpif.h'
    INTEGER t, rank, size, err, N

    CALL mpi_init(err)
    CALL mpi_comm_size(MPI_COMM_WORLD,size,err)
    CALL mpi_comm_rank(MPI_COMM_WORLD,rank,err)
    N=400
    DO t=1,10
        CALL pi3(rank, size, N)
    END DO

    CALL mpi_finalize(err)
END PROGRAM
