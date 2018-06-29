! Created by ${USER_NAME} on 04/05/18.

module aerodyn_wrapper

    use AeroDyn
    use aerodyn_wrapper_subs   
    
    implicit none


    ! Program variables
    type(Dvr_SimData)                              :: DvrData              ! The data required for running the AD driver
    type(AeroDyn_Data)                             :: AD                   ! AeroDyn data
    integer(IntKi)                                 :: iCase                ! loop counter (for driver case)
    integer(IntKi)                                 :: nt                   ! loop counter (for time step)
    integer(IntKi)                                 :: j                    ! loop counter (for array of inputs)
    integer(IntKi)                                 :: numSteps             ! number of time steps in the simulation
    logical                                        :: AD_Initialized
    real(DbKi)                                     :: dT_Dvr               !< copy of DT, to make sure AD didn't change it
    real(DbKi)                                     :: time                 !< Variable for storing time, in seconds
    integer(IntKi)                                 :: errStat              ! Status of error message
    character(ErrMsgLen)                           :: errMsg               ! Error message if ErrStat /= ErrID_None

    public :: Initialize
    public :: Update
    public :: Finalize


contains


    !! ======================================================================
    ! This is intended to read from aerodyn fast configuration file to get
    subroutine Initialize(filename)


        ! call here appropriate aerodyn subroutines to get the configuration
        ! you may have to populate some fortran type that has to be declared somewhere
   character(*)						 , intent(in) :: filename


   ! Local variables
   character(1024)                                :: PriPath
   character(1024)                                :: inpVersion                               ! String containing the input-version information.
   character(1024)                                :: line                                     ! String containing a line of input.
   integer                                        :: unIn, unEc
   integer                                        :: Sttus
   character( 11)                                 :: DateNow                                  ! Date shortly after the start of execution.
   character(  8)                                 :: TimeNow                                  ! Time of day shortly after the start of execution.
   integer, parameter                             :: NumCols = 7                              ! number of columns to be read from the input file
   real(ReKi)                                     :: InpCase(NumCols)                         ! Temporary array to hold combined-case input parameters.
   logical                                        :: TabDel
   logical                                        :: echo
   INTEGER(IntKi)                                 :: ErrStat2                                 ! Temporary Error status
   CHARACTER(ErrMsgLen)                           :: ErrMsg2                                  ! Temporary Err msg
   CHARACTER(*), PARAMETER                        :: routinename = 'AeroDyn_Wrapper_Initialization'
   CHARACTER(200)                                 :: git_commit    ! String containing the current git commit hash

   errStat     = ErrID_None
   errMsg      = ''
   AD_Initialized = .false.
   
   time        = 0.0 ! seconds
      
            
      ! Get the current time
   !call date_and_time ( Values=StrtTime )                               ! Let's time the whole simulation
   !call cpu_time ( UsrTime1 )                                           ! Initial time (this zeros the start time when used as a MATLAB function)

   ! initialize this driver:
!   call Dvr_Init( DvrData, ErrStat, ErrMsg, filename)
!      call CheckError()

!======================================================================
   ! since skipped Dvr_Init we have to now set DvrData and validate these inputs

   DvrData%OutFileData%unOutFile   = -1

   CALL NWTC_Init()
   ! Display the copyright notice
   CALL DispCopyrightLicense( version )
   ! Obtain OpenFAST git commit hash
   git_commit = QueryGitVersion()
   ! Tell our users what they're running
   CALL WrScr( ' Running '//GetNVD( version )//' modified from a part of OpenFAST - '//TRIM(git_Commit)//NewLine//' linked with '//TRIM( GetNVD( NWTC_Ver ))//NewLine )


   ! provide DvrData here:

   CALL GetPath( filename, PriPath )     ! Input files will be relative to the path where the primary input file is located.

   call GetNewUnit( unIn )


   call WrScr1 ( ' '//DvrData%OutFileData%runTitle )

   DvrData%AD_InputFile = filename
   DvrData%numBlades = 3
   DvrData%HubRad = 1.5
   DvrData%HubHt = 90
   DvrData%Overhang = -5.0191
   DvrData%ShftTilt = -5
   DvrData%precone = -2.5
   DvrData%OutFileData%Root = "test"

   if (len_trim(DvrData%OutFileData%Root) == 0) then
      call getroot(fileName,DvrData%OutFileData%Root)
   end if

   TabDel = .True.

   if (TabDel) then
      DvrData%OutFileData%delim = TAB
   else
      DvrData%OutFileData%delim = " "
   end if

   DvrData%OutFileData%OutFmt = "ES15.6E3"
   DvrData%NumCases = 3

   if ( DvrData%NumCases < 1 )  then
      call setErrStat( ErrID_Fatal,'Variable "NumCases" must be > 0.' ,errstat,errmsg,routinename)
      call cleanup()
      return
   end if

   allocate ( DvrData%Cases(DvrData%NumCases) , STAT=Sttus )
   if ( Sttus /= 0 )  then
      call setErrStat( ErrID_Fatal,'Error allocating memory for the Cases array.',errstat,errmsg,routinename)
      call cleanup()
      return
   end if

   do ICase=1,DvrData%NumCases

      DvrData%Cases(iCase)%WndSpeed        = 0.0000000E+00
      DvrData%Cases(ICase)%ShearExp        = 0.0000000E+00
      DvrData%Cases(ICase)%RotSpeed        = 12.1*RPM2RPS
      DvrData%Cases(ICase)%Pitch           = 0.0000000E+00*D2R
      DvrData%Cases(ICase)%Yaw             = 2.0000000E+01*D2R
      DvrData%Cases(iCase)%dT              = 0.138
      DvrData%Cases(iCase)%Tmax            = 12.7935

   end do ! ICase

   DvrData%Cases(2)%dT              = 0.0138
   DvrData%Cases(3)%dT              = 0.00138


   ! validate the inputs
   call ValidateInputs(DvrData, errStat2, errMsg2)
      call SetErrStat(errStat2, errMsg2, ErrStat, ErrMsg, RoutineName)
         call CheckError()


    contains

        subroutine cleanup()
            if (UnIn>0) close(UnIn)
            if (UnEc>0) close(UnEc)
        end subroutine cleanup

        subroutine CheckError()

            if (ErrStat /= ErrID_None) then
                call WrScr(TRIM(ErrMsg))

                if (ErrStat >= AbortErrLev) then
                    call Dvr_End()
                end if
            end if

        end subroutine CheckError

        subroutine Dvr_End()

            ! Local variables
            character(ErrMsgLen)                          :: errMsg2                 ! temporary Error message if ErrStat /= ErrID_None
            integer(IntKi)                                :: errStat2                ! temporary Error status of the operation

            character(*), parameter                       :: RoutineName = 'Dvr_End'
            ! Close the output file
            if (DvrData%OutFileData%unOutFile > 0) close(DvrData%OutFileData%unOutFile)

            if ( AD_Initialized ) then
                call AD_End( AD%u(1), AD%p, AD%x, AD%xd, AD%z, AD%OtherState, AD%y, AD%m, errStat2, errMsg2 )
                call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
            end if

            call AD_Dvr_DestroyDvr_SimData( DvrData, ErrStat2, ErrMsg2 )
            call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )

            call AD_Dvr_DestroyAeroDyn_Data( AD, ErrStat2, ErrMsg2 )
            call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )

            if (ErrStat >= AbortErrLev) then
                CALL ProgAbort( 'AeroDyn Wrapper encountered simulation error level: '&
                        //TRIM(GetErrStr(ErrStat)), TrapErrors=.FALSE., TimeWait=3._ReKi )  ! wait 3 seconds (in case they double-clicked and got an error)
            else
                call NormStop()
            end if


        end subroutine Dvr_End

    end subroutine Initialize




    !! ======================================================================
    ! This is intended to be called during a time loop to exchange information
    ! between the simulation code and aerodyn package
    ! in = velocities
    ! out = forces
    subroutine Update(velocity, force)
        real, dimension(6), intent(in)  :: velocity  ! input  = velocity (to be explicited with Jean-Christophe)
        real, dimension(6), intent(out) :: force     ! output = wrench in 6 dof (to be explicited with Jean-Christophe)

        !======================================================================

        do iCase = 1, DvrData%NumCases
            call WrScr( NewLine//'Running case '//trim(num2lstr(iCase))//' of '//trim(num2lstr(DvrData%NumCases))//'.' )


            !dT = TwoPi/DvrData%Cases(iCase)%RotSpeed / DvrData%NumSect ! sec

            numSteps = ceiling( DvrData%Cases(iCase)%TMax / DvrData%Cases(iCase)%dT)
            dT_Dvr   = DvrData%Cases(iCase)%dT

            call WrScr ('   WndSpeed='//trim(num2lstr(DvrData%Cases(iCase)%WndSpeed))//&
                    ' m/s; ShearExp='//trim(num2lstr(DvrData%Cases(iCase)%ShearExp))//&
                    '; RotSpeed='//trim(num2lstr(DvrData%Cases(iCase)%RotSpeed*RPS2RPM))//&
                    ' rpm; Pitch='//trim(num2lstr(DvrData%Cases(iCase)%Pitch*R2D))//&
                    ' deg; Yaw='//trim(num2lstr(DvrData%Cases(iCase)%Yaw*R2D))//&
                    ' deg; dT='//trim(num2lstr(DvrData%Cases(iCase)%dT))//&
                    ' s; Tmax='//trim(num2lstr(DvrData%Cases(iCase)%Tmax))//&
                    ' s; numSteps='//trim(num2lstr(numSteps)) )


            ! Set the Initialization input data for AeroDyn based on the Driver input file data, and initialize AD
            ! (this also initializes inputs to AD for first time step)
            call Init_AeroDyn(iCase, DvrData, AD, dT_Dvr, errStat, errMsg)
            call CheckError()
            AD_Initialized = .true.

            if (.not. EqualRealNos( dT_Dvr, DvrData%Cases(iCase)%dT ) ) then
                ErrStat = ErrID_Fatal
                ErrMsg = 'AeroDyn changed the time step for case '//trim(num2lstr(iCase))//'. Change DTAero to "default".'
                call CheckError()
            end if


            call Dvr_InitializeOutputFile( iCase, DvrData%Cases(iCase), DvrData%OutFileData, errStat, errMsg)
            call CheckError()


            do nt = 1, numSteps

                !...............................
                ! set AD inputs for nt (and keep values at nt-1 as well)
                !...............................

                call Set_AD_Inputs(iCase,nt,DvrData,AD,errStat,errMsg)
                call CheckError()

                time = AD%InputTime(2)

                ! Calculate outputs at nt - 1

                call AD_CalcOutput( time, AD%u(2), AD%p, AD%x, AD%xd, AD%z, AD%OtherState, AD%y, AD%m, errStat, errMsg )
                call CheckError()

                call Dvr_WriteOutputLine(DvrData%OutFileData, time, AD%y%WriteOutput, errStat, errMsg)
                call CheckError()




                ! Get state variables at next step: INPUT at step nt - 1, OUTPUT at step nt

                call AD_UpdateStates( time, nt-1, AD%u, AD%InputTime, AD%p, AD%x, AD%xd, AD%z, AD%OtherState, AD%m, errStat, errMsg )
                call CheckError()


            end do !nt=1,numSteps

            call AD_End( AD%u(1), AD%p, AD%x, AD%xd, AD%z, AD%OtherState, AD%y, AD%m, errStat, errMsg )
            AD_Initialized = .false.
            call CheckError()
            close( DvrData%OutFileData%unOutFile )



            do j = 2, numInp
                call AD_DestroyInput (AD%u(j),  errStat, errMsg)
                call CheckError()
            end do

        end do !iCase = 1, DvrData%NumCases

    contains

        subroutine CheckError()

            if (ErrStat /= ErrID_None) then
                call WrScr(TRIM(ErrMsg))

                if (ErrStat >= AbortErrLev) then
                    call Dvr_End()
                end if
            end if

        end subroutine CheckError

        subroutine Dvr_End()

            ! Local variables
            character(ErrMsgLen)                          :: errMsg2                 ! temporary Error message if ErrStat /= ErrID_None
            integer(IntKi)                                :: errStat2                ! temporary Error status of the operation

            character(*), parameter                       :: RoutineName = 'Dvr_End'
            ! Close the output file
            if (DvrData%OutFileData%unOutFile > 0) close(DvrData%OutFileData%unOutFile)

            if ( AD_Initialized ) then
                call AD_End( AD%u(1), AD%p, AD%x, AD%xd, AD%z, AD%OtherState, AD%y, AD%m, errStat2, errMsg2 )
                call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
            end if

            call AD_Dvr_DestroyDvr_SimData( DvrData, ErrStat2, ErrMsg2 )
            call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )

            call AD_Dvr_DestroyAeroDyn_Data( AD, ErrStat2, ErrMsg2 )
            call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )

            if (ErrStat >= AbortErrLev) then
                CALL ProgAbort( 'AeroDyn Wrapper encountered simulation error level: '&
                        //TRIM(GetErrStr(ErrStat)), TrapErrors=.FALSE., TimeWait=3._ReKi )  ! wait 3 seconds (in case they double-clicked and got an error)
            else
                call NormStop()
            end if


        end subroutine Dvr_End

    end subroutine Update

    !! ======================================================================
    ! This is intended to properly clean the aerodyn module
    ! Basically, you should call aerodyn subroutines that deallocate data that
    ! were allocated during initialization
    subroutine Finalize()

        call Dvr_End()

        contains
            subroutine Dvr_End()

                ! Local variables
                character(ErrMsgLen)                          :: errMsg2                 ! temporary Error message if ErrStat /= ErrID_None
                integer(IntKi)                                :: errStat2                ! temporary Error status of the operation

                character(*), parameter                       :: RoutineName = 'Dvr_End'
                ! Close the output file
                if (DvrData%OutFileData%unOutFile > 0) close(DvrData%OutFileData%unOutFile)

                if ( AD_Initialized ) then
                    call AD_End( AD%u(1), AD%p, AD%x, AD%xd, AD%z, AD%OtherState, AD%y, AD%m, errStat2, errMsg2 )
                    call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
                end if

                call AD_Dvr_DestroyDvr_SimData( DvrData, ErrStat2, ErrMsg2 )
                call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )

                call AD_Dvr_DestroyAeroDyn_Data( AD, ErrStat2, ErrMsg2 )
                call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )

                if (ErrStat >= AbortErrLev) then
                    CALL ProgAbort( 'AeroDyn Wrapper encountered simulation error level: '&
                            //TRIM(GetErrStr(ErrStat)), TrapErrors=.FALSE., TimeWait=3._ReKi )  ! wait 3 seconds (in case they double-clicked and got an error)
                else
                    call NormStop()
                end if


            end subroutine Dvr_End

    end subroutine Finalize




end module aerodyn_wrapper