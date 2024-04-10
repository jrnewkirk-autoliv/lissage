program main
  use iso_fortran_env, only : real64
  use data_in_m, only : input_data_t

  ! implicit none
    real(kind = real64) :: Ap_x = 5, Bp_x = 10, Cp_x = 50
    real(kind = real64) :: Apt, Bpt, Cpt
    integer :: PTx_loc
    real(kind = real64) :: Ptx
    real(kind = real64) :: alpha = 0
    integer :: N1 = 10, N2 = 10, N3= 20
    integer :: i, n=0, stat
    character(len=100) :: line
    character(len=60) :: filename
    ! type(inputs_t) :: setup_conditions
    integer :: j
    type(input_data_t), allocatable, dimension(:) :: curves_in
    real(kind = real64), allocatable, dimension(:) :: pressures
    real(kind = real64), allocatable , dimension(:) :: times 
    type targets_t
    real(kind = real64) :: pressure
    real(kind = real64) :: time
    end type 

    type(targets_t) :: A,B,C

    filename = 'D:\Fortran\Lissage\output.txt'  ! replace with your filename
    open(unit=10, file=filename, status='old', action='read')
    do
        read(10, '(a)', iostat=stat) line
        if (stat /= 0) exit
        n = n + 1
    end do
    rewind(10)    
    allocate(curves_in(n))
    allocate(pressures(n))
    allocate(times(n))
    read(10,*) curves_in
    close(10)
    ! write(*,'(2f20.13)') curves_in
    pressures = (curves_in%pressure * 1.0d06) + 86570
    ! print *, pressures(1:5)
    pressures(:) = pressures(:)! + 86570d0
    ! print *, pressures(1:5)
    ! pressures = Add_Atmo(pressures)
    ! write(*,'(f20.13)'), pressures
    PTx_loc = maxloc(pressures, DIM=1)
    Ptx = pressures(PTx_loc)
    ! print *, Ptx, PTx_loc   
    A = find_and_interpolate(curves_in%time, pressures, Ap_x)
    B = find_and_interpolate(curves_in%time, pressures, Bp_x)
    C = find_and_interpolate(curves_in%time, pressures, Cp_x)
    print *, "PTx: ", Ptx, "A: ", A, "B: ", B, "C: ", C

    ! print *, Add_Atmo(pressures)

                         
contains

function find_and_interpolate(times_,pressures_,p_x_) result(targets_out)
  real(kind = real64), intent(in), dimension(:) :: times_
  real(kind = real64), intent(in), dimension(:) :: pressures_
  real(kind = real64), intent(in) :: p_x_
  real(kind = real64) :: interp_time
  type(targets_t) :: targets_out
  integer :: m, n_
  real(kind = real64) :: p_target

  p_target = (p_x_ / 100) * (maxval(pressures_, DIM = 1) - 86570) + 86570
  ! print *, maxval(pressures_)
  print *, p_target
  ! print *, pressures_(1:10)
  targets_out%pressure = p_target
  m = minloc(pressures_, mask=(pressures_>p_target),DIM = 1)
  n_ = m-1
  ! print *, m, n_
  print *, times_(1:10)

  targets_out%time = times_(n_) + (  &
                                  (times_(m) - times_(n_))              &
                                  /                                   &
                                  (pressures_(m) - pressures_(n_))    &
                                 )                                    &
                                 * (p_target - pressures_(n_))

end function

! elemental function Add_Atmo(press_in) result(press_out)
!   real(kind = real64), intent(in):: press_in
!   real(kind = real64), dimension(:) :: press_out

!   press_out = press_in + 86570
! end function

end program main
