! Created by ${USER_NAME} on 04/05/18.

program test_libaerodyn

    use aerodyn_wrapper


    ! local variables
    character(1000)    :: input_file
    real, dimension(6) :: velocity
    real, dimension(6) :: force

    ! Specify the name of the aerodyn input file here
    input_file = "NRELOffshrBsline5MW_Onshore_AeroDyn15_Test03.dat"


    ! Aerodyn module initialization
    call Initialize(input_file)


    !! Emulate a time forloop here
    call Update(velocity, force)


    ! Aerodyn module finalization (deallocations)
    call Finalize()


end program test_libaerodyn
