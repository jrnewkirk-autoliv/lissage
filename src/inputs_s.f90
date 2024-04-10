submodule(inputs_m) inputs_s
implicit none

contains

    module procedure FindMax
        ! press_data => self%input_curve%pressure
        Peak%PressIndex = maxloc(self%press)
        print *, Peak%PressIndex
        ! sPeak%PressValue = press_data(Peak%PressIndex)
    end procedure

end submodule