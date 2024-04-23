submodule(data_in_m) data_in_s
implicit none

contains 

module procedure ConvertToPa
    ConvertToPa2%time = self%time
    ConvertToPa2%pressure = (self%pressure * 1.0e06) + 86570
end procedure

module procedure read_data
    character(len=100) :: line
    integer :: i, n=0, stat

    open(unit=10, file=filename, status='old', action='read')
    do
        read(10, '(a)', iostat=stat) line
        if (stat /= 0) exit
        n = n + 1
    end do
    rewind(10)   
    allocate(read_data%time(n))
    allocate(read_data%pressure(n)) 
    ! read(10, *) read_data

    do i = 1, n
        read(10, *) read_data%time(i), read_data%pressure(i)
    end do
    close(10)


end procedure

module procedure find_and_interpolate
    integer :: m,n_
    real(kind = real64) :: p_target
    p_target = (p_x_ / 100) * (maxval(self%pressure, DIM = 1) - 86570) + 86570
    targets_out%pressure = p_target
    m = minloc(self%pressure, mask=(self%pressure>p_target),DIM = 1)
    n_ = m-1
  
    targets_out%time = self%time(n_) + (                                          &
                                    (self%time(m) - self%time(n_))                &
                                    /                                             &
                                    (self%pressure(m) - self%pressure(n_))        &
                                   )                                              &
                                   * (p_target - self%pressure(n_))
end procedure

module procedure define_intermediate_points_1
    integer :: m_, n_

  associate(pressures_ => self%pressure)
    associate(times_ => self%time)

  targets_out%pressure = Pstart + iteration * (Pstop - Pstart) / divisions
  m_ = minloc(pressures_, mask=(pressures_>targets_out%pressure),DIM = 1)
  n_ = m_-1
  targets_out%time = (targets_out%pressure - pressures_(m_))                 &
                      *                                                     &
                      (times_(m_) - times_(n_))                             &
                      /                                                     &                      
                      (pressures_(m_) - pressures_(n_)) + times_(m_)

    end associate
end associate
end procedure

module procedure define_intermediate_points_2_3
    integer :: m_, n_
    associate(pressures_ => self%pressure)
        associate(times_ => self%time)
            targets_out%time = Tstart + iteration * (Tstop - Tstart) / divisions
                m_ = minloc(times_, mask=(times_>targets_out%time),DIM = 1)
                n_ = m_ - 1
                targets_out%pressure = (pressures_(m_) - pressures_(n_))                  &
                                        /                                                 &
                                      (times_(m_) - times_(n_))                           &
                                        *                                                 & 
                                        (targets_out%time - times_(m_))                   &
                                        + pressures_(m_)       
        end associate
    end associate
            
end procedure

module procedure build_spline_segment
    integer :: i, Ptx_loc, n
    real(kind = real64) :: Ptx, Tfin, Pfin
    PTx_loc = maxloc(self%pressure, DIM=1)
    Ptx = self%pressure(PTx_loc)
    n = size(self%time, dim = 1)
    Tfin = self%time(n-1)
    Pfin = self%pressure(n-1)
    

    do i = 1, N1
        spline_out(i) =  define_intermediate_points_1(self &
      , A%pressure, C%pressure, N1, i)
    end do

    do i =1, N2
      spline_out(i + N1) =  define_intermediate_points_2_3(self  &
      , C%time, self%time(PTx_loc), N2, i)
    end do

    do i =1, N3
      spline_out(i + N1 + N2) =  define_intermediate_points_2_3(self  &
      , self%time(PTx_loc), Tfin, N3, i)
    end do

    spline_out(1)%pressure = 86570
    spline_out(1)%time = tt0
    spline_out(N1+N2+N3)%time = self%time(n-2)
    spline_out(N1+N2+N3)%pressure = self%pressure(n-2)
end procedure

end submodule