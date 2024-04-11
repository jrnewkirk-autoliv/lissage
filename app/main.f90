program main
  
  use iso_fortran_env, only : real64
  use data_in_m, only : input_data_t
  implicit none
  ! implicit none
    real(kind = real64) :: Ap_x = 5, Bp_x = 10, Cp_x = 50
    real(kind = real64) :: Apt, Bpt, Cpt, diffAC
    integer :: PTx_loc, FinLoc
    real(kind = real64) :: Ptx, Pfin, Tfin
    real(kind = real64) :: alpha = 0
    integer, parameter :: N1 = 10, N2 = 10, N3= 20
    integer :: i, n=0, stat
    character(len=100) :: line
    character(len=60) :: filename
    integer :: N1_matrix(N1), N2_matrix(N2), N3_matrix(N3)
    real(kind = real64) :: A_line, B_line, tt0, t_milieu

    ! type(inputs_t) :: setup_conditions
    integer :: j
    type(input_data_t), allocatable, dimension(:) :: curves_in
    real(kind = real64), allocatable, dimension(:) :: pressures
    real(kind = real64), allocatable , dimension(:) :: times
    
    type targets_t
    real(kind = real64) :: time
    real(kind = real64) :: pressure    
    end type 
    type(targets_t), allocatable, dimension(:) :: SplineSeg
    type(targets_t) :: A,B,C

    filename = 'D:\Fortran\Lissage\output2.txt'  ! replace with your filename
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
    ! write(*,'(2f20.13)') curves_in(1:5)
    pressures = (curves_in%pressure * 1.0d06) + 86570
    pressures(:) = pressures(:)
    PTx_loc = maxloc(pressures, DIM=1)
    Ptx = pressures(PTx_loc)
    Tfin = curves_in(n-1)%time
    Pfin = curves_in(n-1)%pressure
    A = find_and_interpolate(curves_in%time, pressures, Ap_x)
    B = find_and_interpolate(curves_in%time, pressures, Bp_x)
    C = find_and_interpolate(curves_in%time, pressures, Cp_x)
    print *, "PTx: ", Ptx, "A: ", A, "B: ", B, "C: ", C
    print *, "Tfin", Tfin, "Tx", curves_in(PTx_loc)%time

    allocate(SplineSeg(N1+N2+N3))

    do i = 1, N1
      SplineSeg(i) =  define_intermediate_points_1(curves_in%time, pressures  &
      , A%pressure, C%pressure, N1, i)
    end do
    i = 1
    do i =1, N2
      print *, i
      SplineSeg(i + N1) =  define_intermediate_points_2_3(curves_in%time, pressures  &
      , C%time, curves_in(PTx_loc)%time, N2, i)
    end do
    i = 1
    do i =1, N3
      print *, i
      SplineSeg(i + N1 + N2) =  define_intermediate_points_2_3(curves_in%time, pressures  &
      , curves_in(PTx_loc)%time, Tfin, N3, i)
    end do

    A_line = (B%pressure - A%pressure) / (B%time - A%time)
    B_line = (A%pressure - 86570) - A_line * A%time
    t_milieu = -B_line / A_line;
    tt0 = t_milieu - alpha * t_milieu;
    SplineSeg(1)%pressure = 86570
    SplineSeg(1)%time = tt0
    SplineSeg(N1+N2+N3)%time = curves_in(n-2)%time
    SplineSeg(N1+N2+N3)%pressure = pressures(n-2)

    write(*,'(2f30.13)') SplineSeg
    write(*, *) "Smoothing points defined - Let's spline!"
contains




function define_intermediate_points_1(times_, pressures_, Pstart, Pstop, divisions, iteration) result(targets_out)
  real(kind = real64), intent(in), dimension(:) :: times_
  real(kind = real64), intent(in), dimension(:) :: pressures_
  real(kind = real64), intent(in) :: Pstart, Pstop
  integer, intent(in) :: divisions
  integer,intent(in) :: iteration
  integer :: m_, n_
  type(targets_t) :: targets_out

  
  targets_out%pressure = Pstart + iteration * (Pstop - Pstart) / divisions
  m_ = minloc(pressures_, mask=(pressures_>targets_out%pressure),DIM = 1)
  n_ = m_-1
  targets_out%time = (targets_out%pressure - pressures_(m_))                 &
                      *                                                     &
                      (times_(m_) - times_(n_))                             &
                      /                                                     &                      
                      (pressures_(m_) - pressures_(n_)) + times_(m_)
end function

function define_intermediate_points_2_3(times_, pressures_, Tstart, Tstop, divisions, iteration) result(targets_out)
  real(kind = real64), intent(in), dimension(:) :: times_
  real(kind = real64), intent(in), dimension(:) :: pressures_
  real(kind = real64), intent(in) :: Tstart, Tstop
  integer, intent(in) :: divisions
  integer,intent(in) :: iteration
  integer :: m_, n_
  type(targets_t) :: targets_out

  targets_out%time = Tstart + (iteration * (Tstop - Tstart) / divisions)
  m_ = minloc(times_, mask=(times_>targets_out%time),DIM = 1)
  n_ = m_ - 1
  targets_out%pressure = (pressures_(m_) - pressures_(n_))                  &
                          /                                                 &
                          (times_(m_) - times_(n_))                           &
                          *                                                 & 
                          (targets_out%time - times_(m_))                   &
                          + pressures_(m_)                                                                             
end function


function find_and_interpolate(times_,pressures_,p_x_) result(targets_out)
  real(kind = real64), intent(in), dimension(:) :: times_
  real(kind = real64), intent(in), dimension(:) :: pressures_
  real(kind = real64), intent(in) :: p_x_
  real(kind = real64) :: interp_time
  type(targets_t) :: targets_out
  integer :: m, n_
  real(kind = real64) :: p_target

  p_target = (p_x_ / 100) * (maxval(pressures_, DIM = 1) - 86570) + 86570
  targets_out%pressure = p_target
  m = minloc(pressures_, mask=(pressures_>p_target),DIM = 1)
  n_ = m-1

  targets_out%time = times_(n_) + (  &
                                  (times_(m) - times_(n_))              &
                                  /                                   &
                                  (pressures_(m) - pressures_(n_))    &
                                 )                                    &
                                 * (p_target - pressures_(n_))

end function

end program main
