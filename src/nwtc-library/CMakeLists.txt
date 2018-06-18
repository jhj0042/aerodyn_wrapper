set(NWTCLIBS_SOURCES
        ModMesh.f90
        ModMesh_Mapping.f90
        ModMesh_Types.f90
        NWTC_Base.f90
        NWTC_IO.f90
        NWTC_Library.f90
        NWTC_Num.f90
        SingPrec.f90
        NWTC_Library_Types.f90

        # RanLux sources
        ranlux/RANLUX.f90

        # NetLib sources
        NetLib/Dierckx_FitPack/dierckx_fitpack.f
        NetLib/fftpack/fftpack4.1.f
        NetLib/scalapack/dlasrt2.f
        NetLib/scalapack/slasrt2.f
        NetLib/Dierckx_FitPack/NWTC_FitPack.f90
        NetLib/fftpack/NWTC_FFTPACK.f90
        NetLib/lapack/NWTC_LAPACK.f90
        NetLib/scalapack/NWTC_ScaLAPACK.f90
        )

get_filename_component(FCNAME ${CMAKE_Fortran_COMPILER} NAME)

if (${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
    if (APPLE OR UNIX)
        set(NWTC_SYS_FILE SysGnuLinux.f90)
    elseif (WIN32)
        set(NWTC_SYS_FILE SysGnuWin.f90)
    endif (APPLE OR UNIX)
elseif (${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
    if (APPLE OR UNIX)
        set(NWTC_SYS_FILE SysIFL.f90)
    elseif (WIN32)
        set(NWTC_SYS_FILE SysIVF.f90)
    endif (APPLE OR UNIX)
endif ()

if (NWTC_SYS_FILE)
    message("-- Setting system file as: ${NWTC_SYS_FILE}")
    list(APPEND NWTCLIBS_SOURCES ${NWTC_SYS_FILE})
else (NWTC_SYS_FILE)
    message(FATAL_ERROR "Cannot determine system file used with NWTC_Library")
endif (NWTC_SYS_FILE)

add_library(nwtclibs ${NWTCLIBS_SOURCES})
target_link_libraries(nwtclibs ${LAPACK_LIBRARIES} ${CMAKE_DL_LIBS})

