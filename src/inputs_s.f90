submodule(inputs_m) inputs_s
implicit none

contains
    module procedure ConvertToPa
        ConvertToPa = self%inp_data(1:size(self%inp_data,1),1) * 1.0e06
    end procedure

    module procedure GetPressure 
        GetPressure = self%inp_data(1:self%last,2)*1.0e6
    end procedure

    module procedure GetTime 
        GetTime = self%inp_data(1:self%last,1)
    end procedure
end submodule