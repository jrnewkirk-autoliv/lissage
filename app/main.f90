program main
  use iso_fortran_env, only : real64
  use data_in_m, only : input_data_t, targets_t
  use spline_interp_m
  implicit none

  character(len=60) :: filename = 'D:\Fortran\Lissage\output2.txt'
  type(input_data_t) :: curves_in, curves_adjusted
  real(kind = real64) :: Ap_x = 5, Bp_x = 10, Cp_x = 50, tt0 = 0.00005 !!tt0 intended to be passed from Scilab to Fortran
  integer, parameter :: N1 = 10, N2 = 10, N3= 20
  type(targets_t) :: A,B,C
  type(targets_t), allocatable, dimension(:) :: SplineSeg
  type(spline_t) :: spl 
  integer :: unit, status, i
  real :: startTime, stopTime
  call cpu_time(startTime)
  !!!!! This function intended to be replaced by variable passing from Scilab
  curves_in = curves_in%read_data(filename)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  curves_adjusted = curves_in%ConvertToPa()
  A = curves_adjusted%find_and_interpolate(Ap_x)
  B = curves_adjusted%find_and_interpolate(Bp_x)
  C = curves_adjusted%find_and_interpolate(Cp_x)
  !allocate(SplineSeg(N1+N2+N3))
  SplineSeg = curves_adjusted%build_spline_segment(A, B, C, N1, N2, N3, tt0)
  call spline_set_coeffs(SplineSeg%time, SplineSeg%pressure, N1+N2+N3, spl)  
  !!!!!!!!! This section intended to be passed back to Scilab rather than write to file
  open(newunit=unit, file="FortranInterpolatedSplineOutput.txt", status='REPLACE')
    do i = 1, size(curves_adjusted%time, dim = 1)
      write(unit,'(2f30.13)') curves_adjusted%time(i), spline_evaluate(curves_adjusted%time(i), spl)
    end do
  close(unit)
  call cpu_time(stopTime)
  print *, stopTime
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end program main
