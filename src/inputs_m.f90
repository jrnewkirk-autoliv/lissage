module inputs_m
use iso_fortran_env, only : real64
use data_in_m
implicit none

type pressure_info_t
    real :: PressValue
    integer :: PressIndex
end type
type inputs_t
    integer :: Ap_x, Bp_x, Cp_x
    real :: alpha
    real :: N1, N2, N3
    real :: press
    real :: times
    contains
        procedure :: FindMax
end type 

interface
    module function FindMax(self) result(Peak)
        class(inputs_t), intent(in) :: self
        type(pressure_info_t) :: Peak
    end function
end interface
end module inputs_m 