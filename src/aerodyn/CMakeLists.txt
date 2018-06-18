
set(AD_LIBS_SOURCES
        AeroDyn.f90
        AeroDyn_IO.f90
        AirfoilInfo.f90
        BEMT.f90
        BEMTUncoupled.f90
        UnsteadyAero.f90
        fmin_fcn.f90
        mod_root1dim.f90
        AeroDyn_Types.f90
        AirfoilInfo_Types.f90
        BEMT_Types.f90
        UnsteadyAero_Types.f90
        )

add_library(aerodyn SHARED ${AD_LIBS_SOURCES})
target_link_libraries(aerodyn nwtclibs)

#set(AD_DRIVER_SOURCES
#        AeroDyn_Driver.f90
#        AeroDyn_Driver_Subs.f90
#        AeroDyn_Driver_Types.f90)
#
#add_executable(aerodyn ${AD_DRIVER_SOURCES})
#target_link_libraries(aerodyn aerodynlib nwtclibs ${CMAKE_DL_LIBS})
