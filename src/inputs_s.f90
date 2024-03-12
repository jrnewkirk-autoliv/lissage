submodule(inputs_m) inputs_s
implicit none

contains

    module procedure GetPressure 
        GetPressure = self%inp_data(1:self%last,2)*1.0e6
    end procedure

    module procedure GetTime 
        GetTime = self%inp_data(1:self%last,1)
    end procedure
end submodule