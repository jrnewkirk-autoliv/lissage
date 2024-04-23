module data_in_m
  use iso_fortran_env, only : real64
  implicit none
  ! private

type targets_t
    real(kind = real64) :: time
    real(kind = real64) :: pressure  
    end type 

type :: input_data_t
      real(kind=real64), allocatable :: time(:)
      real(kind=real64), allocatable :: pressure(:)
    contains
        procedure, public :: read_data, ConvertToPa, find_and_interpolate
        procedure, public :: define_intermediate_points_1, define_intermediate_points_2_3
        procedure, public :: build_spline_segment
end type input_data_t

interface

module function read_data(self, filename)
  class(input_data_t), intent(in) :: self
  character(len=60), intent(in) :: filename
  type(input_data_t) :: read_data
end function

elemental module function ConvertToPa(self) result(ConvertToPa2)
    class(input_data_t), intent(in) :: self    
    type(input_data_t) :: ConvertToPa2
end function

module function find_and_interpolate(self, p_x_) result(targets_out)
  class(input_data_t), intent(in) :: self
  real(kind = real64), intent(in) :: p_x_
  type(targets_t) :: targets_out
end function

module function define_intermediate_points_1(self, Pstart, Pstop, divisions, iteration) result(targets_out)
  class(input_data_t), intent(in) :: self
  real(kind = real64), intent(in) :: Pstart, Pstop
  integer, intent(in) :: divisions
  integer,intent(in) :: iteration
  type(targets_t) :: targets_out
end function

module function define_intermediate_points_2_3(self, Tstart, Tstop, divisions, iteration) result(targets_out)
  class(input_data_t), intent(in) :: self
  real(kind = real64), intent(in) :: Tstart, Tstop
  integer, intent(in) :: divisions, iteration
  type(targets_t) :: targets_out

end function

module function build_spline_segment(self, A, B, C, N1, N2, N3, tt0) result(spline_out)
  class(input_data_t), intent(in) :: self
  class(targets_t), intent(in) :: A, B, C
  integer, intent(in) :: N1, N2, N3
  real(kind = real64), intent(in) :: tt0
  type(targets_t) :: spline_out(N1 + N2 + N3)
end function

end interface

end module data_in_m