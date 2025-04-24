!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !PROGRAM: hemco_nuopc_standalone - HEMCO NUOPC standalone driver
!
! !DESCRIPTION: This program provides a standalone driver for HEMCO using the
! NUOPC interface. It allows HEMCO to be run in standalone mode but with all the
! benefits of the NUOPC framework.
!\\
!\\
! !INTERFACE:
!
PROGRAM hemco_nuopc_standalone
!
! !USES:
!
  USE ESMF
  USE NUOPC
  USE HCOI_NUOPCStandaloneDriverMod, ONLY : DriverSetServices

  IMPLICIT NONE
!
! !LOCAL VARIABLES:
!
  TYPE(ESMF_GridComp) :: driver
  INTEGER :: rc, userRC
  CHARACTER(LEN=ESMF_MAXSTR) :: configFile
  INTEGER :: i, argCount
  LOGICAL :: fileExists

  !---------------------------------------------------------------------------
  ! Initialize ESMF
  !---------------------------------------------------------------------------
  CALL ESMF_Initialize(defaultLogFileName="HEMCO_NUOPC.log", &
                      logkindflag=ESMF_LOGKIND_MULTI, &
                      rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  CALL ESMF_LogWrite("HEMCO NUOPC standalone driver starting", &
                    ESMF_LOGMSG_INFO, rc=rc)

  !---------------------------------------------------------------------------
  ! Create the top-level driver component
  !---------------------------------------------------------------------------
  driver = ESMF_GridCompCreate(name="HEMCO_NUOPC_Driver", rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  !---------------------------------------------------------------------------
  ! Process command line arguments for configuration file
  !---------------------------------------------------------------------------
  ! Set default configuration file
  configFile = "HEMCO_Config.rc"

  ! Check command line arguments
  argCount = COMMAND_ARGUMENT_COUNT()
  i = 1
  DO WHILE (i <= argCount)
    CALL GET_COMMAND_ARGUMENT(i, value=configFile)

    ! Check for --config or -c flag
    IF (configFile == "--config" .OR. configFile == "-c") THEN
      ! Get the next argument as the config file name
      IF (i + 1 <= argCount) THEN
        i = i + 1
        CALL GET_COMMAND_ARGUMENT(i, value=configFile)
      ELSE
        ! Missing config file argument
        CALL ESMF_LogWrite("ERROR: Missing configuration file after --config/-c flag", &
                         ESMF_LOGMSG_ERROR, rc=rc)
        CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
      END IF
    END IF

    i = i + 1
  END DO

  ! Check if the configuration file exists
  INQUIRE(FILE=TRIM(configFile), EXIST=fileExists)
  IF (.NOT. fileExists) THEN
    CALL ESMF_LogWrite("ERROR: Configuration file not found: "//TRIM(configFile), &
                     ESMF_LOGMSG_ERROR, rc=rc)
    CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  END IF

  !---------------------------------------------------------------------------
  ! Create HEMCO driver configuration
  !---------------------------------------------------------------------------
  CALL ConfigureDriver(driver, TRIM(configFile), rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  !---------------------------------------------------------------------------
  ! Register the driver's services
  !---------------------------------------------------------------------------
  CALL ESMF_GridCompSetServices(driver, DriverSetServices, userRC=userRC, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  IF (ESMF_LogFoundError(rcToCheck=userRC, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  !---------------------------------------------------------------------------
  ! Initialize the driver component
  !---------------------------------------------------------------------------
  CALL ESMF_GridCompInitialize(driver, userRC=userRC, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  IF (ESMF_LogFoundError(rcToCheck=userRC, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  !---------------------------------------------------------------------------
  ! Run the driver component
  !---------------------------------------------------------------------------
  CALL ESMF_GridCompRun(driver, userRC=userRC, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  IF (ESMF_LogFoundError(rcToCheck=userRC, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  !---------------------------------------------------------------------------
  ! Finalize the driver component
  !---------------------------------------------------------------------------
  CALL ESMF_GridCompFinalize(driver, userRC=userRC, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  IF (ESMF_LogFoundError(rcToCheck=userRC, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  !---------------------------------------------------------------------------
  ! Destroy the driver component
  !---------------------------------------------------------------------------
  CALL ESMF_GridCompDestroy(driver, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  !---------------------------------------------------------------------------
  ! Finalize ESMF
  !---------------------------------------------------------------------------
  CALL ESMF_LogWrite("HEMCO NUOPC standalone driver completed", &
                    ESMF_LOGMSG_INFO, rc=rc)
  CALL ESMF_Finalize(rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) STOP 1

CONTAINS

  !---------------------------------------------------------------------------
  ! Helper subroutine to configure the driver
  !---------------------------------------------------------------------------
  SUBROUTINE ConfigureDriver(driver, configFile, rc)
    TYPE(ESMF_GridComp)  :: driver
    CHARACTER(LEN=*), INTENT(IN) :: configFile
    INTEGER, INTENT(OUT) :: rc

    TYPE(ESMF_Config) :: config

    ! Begin
    rc = ESMF_SUCCESS

    ! Create the config
    config = ESMF_ConfigCreate(rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) RETURN

    ! Set the HEMCO configuration file attribute
    CALL ESMF_ConfigSetAttribute(config, configFile, &
                               label="HEMCO_CONFIG:", rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) RETURN

    ! Attach the config to the driver
    CALL ESMF_GridCompSet(driver, config=config, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) RETURN

    CALL ESMF_LogWrite("HEMCO NUOPC driver configured with: "//TRIM(configFile), &
                      ESMF_LOGMSG_INFO, rc=rc)

  END SUBROUTINE ConfigureDriver

END PROGRAM hemco_nuopc_standalone
!EOC