set(AD_WR_SOURCES
        aerodyn_wrapper.f90
        aerodyn_wrapper_subs.f90
        aerodyn_wrapper_types.f90
        )

        

add_library(AeroDynWrapper SHARED ${AD_WR_SOURCES} )
target_link_libraries(AeroDynWrapper aerodyn)
