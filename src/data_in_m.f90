module data_in_m
use iso_fortran_env, only : real64
implicit none

type :: input_data_t
      real(kind = real64) :: time
      real(kind = real64) :: pressure
    contains
        procedure, public :: ConvertToPa
end type input_data_t

interface
elemental module function ConvertToPa(self) result(ConvertToPa2)
    class(input_data_t), intent(in) :: self    
    real(kind=real64):: ConvertToPa2
end function

end interface

end module