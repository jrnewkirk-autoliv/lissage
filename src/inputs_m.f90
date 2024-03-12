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
contains
    procedure, public :: ConvertToPa, GetPressure, GetTime
end type

interface
    module function ConvertToPa(self, n_)
        class(inputs_t), intent(in) :: self
        integer, intent(in) :: n_
        integer :: s
        real(kind=real64), allocatable :: pressure(:)        
        real(kind=real64), allocatable :: ConvertToPa(:)
    end function

    module function GetPressure(self)
        class(inputs_t), intent(in) :: self
        real(kind=real64), allocatable :: GetPressure(:)        
    end function

    module function GetTime(self)
        class(inputs_t), intent(in) :: self
        real(kind=real64), allocatable :: GetTime(:)        
    end function

end interface

end module inputs_m