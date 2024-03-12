module inputs_m
use iso_fortran_env, only : real64
implicit none

type inputs_t
    real :: Ap_x, Bp_x, Cp_x
    real :: alpha
    real :: N1, N2, N3 
    real :: P_Ap, P_Bp, P_Cp, j_Cp
    real(kind=real64), allocatable :: inp_data(:,:)
    real :: Patmo
    integer :: last
end type


end module inputs_m