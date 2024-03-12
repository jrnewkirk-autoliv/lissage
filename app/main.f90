program main
  use iso_fortran_env, only : real64
  use inputs_m!, only : inputs_t!, GetPressure, GetTime

  ! implicit none

  
    real(kind=real64), allocatable :: all_data(:,:)
    real(kind=real64), allocatable :: b(:)
    integer :: i, n=0, stat
    character(len=100) :: line
    character(len=60) :: filename
    type(inputs_t) :: setup_conditions
    integer :: j
    

    type :: input_data_t
      real(kind = real64) :: time
      real(kind = real64) :: pressure
    end type input_data_t

    type(input_data_t), dimension(1001) :: curves_in

    filename = 'D:\Fortran\Lissage\output.txt'  ! replace with your filename

    open(unit=10, file=filename, status='old', action='read')
    do
        read(10, '(a)', iostat=stat) line
        if (stat /= 0) exit
        n = n + 1
    end do
    rewind(10)    

    do i = 1, n
      read(10,*) curves_in(i)
    end do 


    ! allocate(all_data(n,2))

    ! read(10,*,iostat=stat) all_data
    close(10)
    write(*, '(f30.7)') curves_in%time
    ! write(*, '(i11)')  size(all_data,2)
    ! print *, fstat(filename)

    setup_conditions = inputs_t(Ap_x = 0.1, Bp_x = 0.3, Cp_x = 0.09, &
                        alpha = 1, N1 = 1, N2 = 2, N3 = 3, P_Ap = 4, &
                        P_Bp = 5, P_Cp = 7, j_Cp = 4, inp_data = all_data, &
                        Patmo = 69.4, last = n)


end program main
