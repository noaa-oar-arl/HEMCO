!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: HCOI_NUOPCStandaloneDriverMod.F90
!
! !DESCRIPTION: Module HCOI\_NUOPCStandaloneDriverMod provides a NUOPC-based
! standalone driver for the HEMCO component. This driver allows running HEMCO
! with the NUOPC interface without requiring integration into a larger Earth
! system model, providing a testing and development environment for the HEMCO
! NUOPC interface.
!\\
!\\
! !INTERFACE:
!
MODULE HCOI_NUOPCStandaloneDriverMod
!
! !USES:
!
  USE ESMF
  USE NUOPC
  USE NUOPC_Driver, &
       driver_routine_SS      => SetServices, &
       driver_label_SetModelServices => label_SetModelServices

  ! Import HcoiSetServices to differentiate from the local SetServices
  USE HCOI_NUOPCGridCompMod, ONLY : SetServices => HcoiSetServices
  USE HCO_ERROR_MOD,         ONLY : HCO_SUCCESS, HCO_FAIL

  IMPLICIT NONE
  PRIVATE

! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: DriverSetServices

! !PRIVATE MEMBER FUNCTIONS:
!
  PRIVATE :: SetModelServices
  PRIVATE :: SetRunSequence

! !REVISION HISTORY:
!  23 Apr 2025 - Initial version for NUOPC standalone driver
!EOP
!------------------------------------------------------------------------------
!BOC
CONTAINS

  !-----------------------------------------------------------------------------
  !BOP
  !
  ! !IROUTINE: DriverSetServices
  !
  ! !DESCRIPTION: Sets the NUOPC driver services
  !\\
  !\\
  ! !INTERFACE:
  !
  SUBROUTINE DriverSetServices(driver, rc)
    TYPE(ESMF_GridComp)  :: driver
    INTEGER, INTENT(OUT) :: rc
!
! !LOCAL VARIABLES:
!
    INTEGER :: localrc

    ! Begin...
    rc = ESMF_SUCCESS

    ! Register standard NUOPC driver services
    CALL NUOPC_CompDerive(driver, driver_routine_SS, rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    ! Add an entry point for initializing the model services
    CALL ESMF_GridCompSetEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeModelServices, phase=0, rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    ! Log successful registration
    CALL ESMF_LogWrite("HEMCO NUOPC Standalone Driver registered", &
                      ESMF_LOGMSG_INFO, rc=localrc)

  END SUBROUTINE DriverSetServices

  !-----------------------------------------------------------------------------
  !BOP
  !
  ! !IROUTINE: SetModelServices
  !
  ! !DESCRIPTION: Sets up the HEMCO component as a model in the NUOPC driver
  !\\
  !\\
  ! !INTERFACE:
  !
  SUBROUTINE SetModelServices(driver, rc)
    TYPE(ESMF_GridComp)  :: driver
    INTEGER, INTENT(OUT) :: rc
!
! !LOCAL VARIABLES:
!
    TYPE(ESMF_GridComp) :: hemcoComp
    TYPE(ESMF_Config)   :: config
    TYPE(ESMF_Clock)    :: clock
    TYPE(ESMF_Time)     :: startTime, stopTime
    TYPE(ESMF_TimeInterval) :: timeStep
    INTEGER :: localrc, petCount
    CHARACTER(LEN=ESMF_MAXSTR) :: configFile
    CHARACTER(LEN=ESMF_MAXSTR) :: message

    ! Begin...
    rc = ESMF_SUCCESS

    ! Create the HEMCO component using ESMF_GridCompCreate instead of NUOPC_DriverAddComp
    hemcoComp = ESMF_GridCompCreate(name="HEMCO", rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    ! Register the component's services with HcoiSetServices (aliased as SetServices)
    CALL ESMF_GridCompSetServices(hemcoComp, SetServices, rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    ! Add component to driver (use simpler form without keyword arguments)
    CALL ESMF_DriverAddComp(driver, hemcoComp, localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    ! Get the driver's config
    CALL ESMF_GridCompGet(driver, config=config, rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    ! Get the HEMCO configuration file name from the driver config
    CALL ESMF_ConfigGetAttribute(config, configFile, &
                               label="HEMCO_CONFIG:", &
                               default="HEMCO_Config.rc", rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    ! Log which config file we're using
    message = "Using HEMCO configuration file: " // TRIM(configFile)
    CALL ESMF_LogWrite(message, ESMF_LOGMSG_INFO, rc=localrc)

    ! Set up the component's config
    CALL NUOPC_CompAttributeSet(hemcoComp, name="HEMCO_CONFIG", &
                               value=TRIM(configFile), rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    ! Set standalone mode attribute
    CALL NUOPC_CompAttributeSet(hemcoComp, name="HEMCO_STANDALONE", &
                               value="true", rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    ! Set up clock for the driver
    ! Set default time parameters - can be overridden by config file
    CALL ESMF_TimeSet(startTime, yy=2019, mm=1, dd=1, h=0, m=0, s=0, rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    CALL ESMF_TimeSet(stopTime, yy=2019, mm=1, dd=2, h=0, m=0, s=0, rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    ! One hour timestep
    CALL ESMF_TimeIntervalSet(timeStep, h=1, m=0, s=0, rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    ! Create the clock with the default parameters
    clock = ESMF_ClockCreate(name="HEMCO Driver Clock", &
                            timeStep=timeStep, &
                            startTime=startTime, &
                            stopTime=stopTime, &
                            rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    ! Attach the clock to the driver
    CALL ESMF_GridCompSet(driver, clock=clock, rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    ! Set the run sequence
    CALL SetRunSequence(driver, rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    CALL ESMF_LogWrite("HEMCO NUOPC Standalone Driver model services set", &
                      ESMF_LOGMSG_INFO, rc=localrc)

  END SUBROUTINE SetModelServices

  !-----------------------------------------------------------------------------
  !BOP
  !
  ! !IROUTINE: SetRunSequence
  !
  ! !DESCRIPTION: Sets up the run sequence for the driver
  !\\
  !\\
  ! !INTERFACE:
  !
  SUBROUTINE SetRunSequence(driver, rc)
    TYPE(ESMF_GridComp)  :: driver
    INTEGER, INTENT(OUT) :: rc
!
! !LOCAL VARIABLES:
!
    TYPE(ESMF_Config) :: config
    INTEGER :: localrc

    ! Begin...
    rc = ESMF_SUCCESS

    ! Get the driver's config
    CALL ESMF_GridCompGet(driver, config=config, rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    ! Use a simpler approach with ESMF_AttributeSet instead of NUOPC_DriverAddRunElement
    CALL ESMF_AttributeSet(driver, name="RunSequence", &
                          value="@HEMCO", rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

  END SUBROUTINE SetRunSequence

  !-----------------------------------------------------------------------------
  !BOP
  !
  ! !IROUTINE: InitializeModelServices
  !
  ! !DESCRIPTION: Wrapper routine with ESMF interface to set up model services
  !\\
  !\\
  ! !INTERFACE:
  !
  SUBROUTINE InitializeModelServices(gcomp, importState, exportState, clock, rc)
    TYPE(ESMF_GridComp)  :: gcomp
    TYPE(ESMF_State)     :: importState
    TYPE(ESMF_State)     :: exportState
    TYPE(ESMF_Clock)     :: clock
    INTEGER, INTENT(OUT) :: rc

    ! Call our internal setup routine
    ! We don't need importState, exportState, or clock for this initialization
    CALL SetModelServices(gcomp, rc)

  END SUBROUTINE InitializeModelServices

END MODULE HCOI_NUOPCStandaloneDriverMod
!EOC