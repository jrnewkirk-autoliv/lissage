submodule(data_in_m) data_in_s
implicit none

contains 

module procedure ConvertToPa
    ConvertToPa2 = self%pressure * 1.0e06
end procedure

end submodule