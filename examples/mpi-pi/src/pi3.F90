MODULE pi3_mod
  PUBLIC pi3
CONTAINS
  double precision function f(x)
    implicit none
    double precision, intent(in) :: x
    f = 4.d0 / (1.d0 + x*x)
  end function f
  SUBROUTINE pi3(rank, mympisize, N)
    implicit none
    INTEGER, INTENT(IN) :: rank, mympisize, N
#include "mpif.h"

    double precision  PI25DT
    parameter        (PI25DT = 3.141592653589793238462643d0)

    double precision  mypi, sumpi, h, tmpsum, x, a
    integer i, rc, myierr
!                                 function to integrate
 
 
    h = 1.0d0/n
    sumpi = 0.0
    myierr = 0
    tmpsum  = 0.0d0
    !$kgen begin_callsite pi
    do i = rank+1, n, mympisize
       x = h * (dble(i) - 0.5d0)
       tmpsum = tmpsum + f(x)
       myierr = i
    enddo
    mypi = h * tmpsum
    sumpi=mypi

!                                 collect all the partial sums
    !$kgen exclude
    call MPI_REDUCE(mypi,sumpi,1,MPI_DOUBLE_PRECISION,MPI_SUM,0, &
                    MPI_COMM_WORLD,myierr)

!                                 node 0 prints the answer.
    !$kgen end_callsite pi
    if (rank .eq. 0) then
        write(6, 97) sumpi, abs(sumpi - PI25DT)
 97     format('  pi is approximately: ', F18.16, &
               '  Error is: ', F18.16)
    endif
  END SUBROUTINE PI3
END MODULE pi3_mod




