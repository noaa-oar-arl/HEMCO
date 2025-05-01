#ifdef ESMF_
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hemco_nuopc_main.F90
!
! !DESCRIPTION: Main program for HEMCO standalone NUOPC application.
! This file provides a complete standalone ESMF/NUOPC application that runs
! HEMCO as an ESMF/NUOPC component. It initializes the ESMF framework, sets up
! the HEMCO component, executes the run sequence, and finalizes the application.
!\\
!\\
! !INTERFACE:
!
PROGRAM HEMCO_NUOPC_Main
!
! !USES:
!
  USE ESMF
  USE NUOPC
  USE HCOI_ESMF_NUOPC_MOD, ONLY : HCOI_NUOPC_SS

  IMPLICIT NONE
!
! !REVISION HISTORY:
!  29 Apr 2025 - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC

  ! Local variables
  INTEGER                      :: rc, userRc
  TYPE(ESMF_GridComp)          :: hemcoComp
  TYPE(ESMF_State)             :: importState, exportState
  TYPE(ESMF_Clock)             :: clock
  TYPE(ESMF_TimeInterval)      :: timeStep
  TYPE(ESMF_Time)              :: startTime, stopTime
  CHARACTER(LEN=ESMF_MAXSTR)   :: configFile, diagFile, logFile
  CHARACTER(LEN=ESMF_MAXSTR)   :: hco_datadir
  LOGICAL                      :: useRestart
  CHARACTER(LEN=ESMF_MAXSTR)   :: restartFile
  INTEGER                      :: timeStepSeconds, nSteps
  INTEGER                      :: petCount, localPet
  TYPE(ESMF_VM)                :: vm
  CHARACTER(LEN=ESMF_MAXSTR)   :: msg
  INTEGER                      :: yearStart, monthStart, dayStart
  INTEGER                      :: yearEnd, monthEnd, dayEnd
  INTEGER                      :: hourStart, minStart, secStart
  INTEGER                      :: hourEnd, minEnd, secEnd
  LOGICAL                      :: exist
  TYPE(ESMF_Config)            :: CONFIG

  ! Initialize ESMF
  CALL ESMF_Initialize(logkindflag=ESMF_LOGKIND_MULTI, &
                      defaultCalkind=ESMF_CALKIND_GREGORIAN, &
                      rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Get VM info
  CALL ESMF_VMGetGlobal(vm, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  CALL ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! -------------------------------------------------------------------
  ! Read configuration from HEMCO_NUOPC.rc file
  ! -------------------------------------------------------------------

  ! Check for HEMCO NUOPC config file
  configFile = 'HEMCO_NUOPC.rc'
  IF (localPet == 0) THEN
    INQUIRE(FILE=TRIM(configFile), EXIST=exist)
    IF (.NOT. exist) THEN
      WRITE(*,*) "ERROR: HEMCO_NUOPC.rc file not found!"
      CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
      STOP
    ENDIF
  ENDIF

  ! Create config object from the resource file
  CONFIG = ESMF_ConfigCreate(rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  CALL ESMF_ConfigLoadFile(CONFIG, TRIM(configFile), rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! Get configuration from resource file or use defaults
  CALL ESMF_ConfigGetAttribute(CONFIG, configFile, label="ESMF.HEMCO.CONFIG_FILE:", &
                               default="HEMCO_Config.rc", rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  CALL ESMF_ConfigGetAttribute(CONFIG, diagFile, label="ESMF.HEMCO.DIAGN_FILE:", &
                               default="HEMCO_Diagn.rc", rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  CALL ESMF_ConfigGetAttribute(CONFIG, hco_datadir, label="ESMF.HEMCO.DATA_ROOT:", &
                               default="./", rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  CALL ESMF_ConfigGetAttribute(CONFIG, logFile, label="ESMF.HEMCO.LOG_FILE:", &
                               default="HEMCO.log", rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! Get restart options
  CALL ESMF_ConfigGetAttribute(CONFIG, useRestart, label="ESMF.HEMCO.USE_RESTART:", &
                               default=.FALSE., rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  CALL ESMF_ConfigGetAttribute(CONFIG, restartFile, label="ESMF.HEMCO.RESTART_FILE:", &
                               default="HEMCO_restart.nc", rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! Get time step and simulation length
  CALL ESMF_ConfigGetAttribute(CONFIG, timeStepSeconds, label="ESMF.HEMCO.TIMESTEP_SECONDS:", &
                               default=3600, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  CALL ESMF_ConfigGetAttribute(CONFIG, nSteps, label="ESMF.HEMCO.RUN_STEPS:", &
                               default=24, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! Get start date and time (defaults to 2016-01-01)
  CALL ESMF_ConfigGetAttribute(CONFIG, yearStart, label="ESMF.HEMCO.START_YEAR:", &
                               default=2016, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  CALL ESMF_ConfigGetAttribute(CONFIG, monthStart, label="ESMF.HEMCO.START_MONTH:", &
                               default=1, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  CALL ESMF_ConfigGetAttribute(CONFIG, dayStart, label="ESMF.HEMCO.START_DAY:", &
                               default=1, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  CALL ESMF_ConfigGetAttribute(CONFIG, hourStart, label="ESMF.HEMCO.START_HOUR:", &
                               default=0, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  CALL ESMF_ConfigGetAttribute(CONFIG, minStart, label="ESMF.HEMCO.START_MINUTE:", &
                               default=0, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  CALL ESMF_ConfigGetAttribute(CONFIG, secStart, label="ESMF.HEMCO.START_SECOND:", &
                               default=0, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! Get end date and time (defaults to 1 day after start)
  CALL ESMF_ConfigGetAttribute(CONFIG, yearEnd, label="ESMF.HEMCO.END_YEAR:", &
                               default=yearStart, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  CALL ESMF_ConfigGetAttribute(CONFIG, monthEnd, label="ESMF.HEMCO.END_MONTH:", &
                               default=monthStart, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  CALL ESMF_ConfigGetAttribute(CONFIG, dayEnd, label="ESMF.HEMCO.END_DAY:", &
                               default=dayStart+1, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  CALL ESMF_ConfigGetAttribute(CONFIG, hourEnd, label="ESMF.HEMCO.END_HOUR:", &
                               default=hourStart, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  CALL ESMF_ConfigGetAttribute(CONFIG, minEnd, label="ESMF.HEMCO.END_MINUTE:", &
                               default=minStart, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  CALL ESMF_ConfigGetAttribute(CONFIG, secEnd, label="ESMF.HEMCO.END_SECOND:", &
                               default=secStart, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! -------------------------------------------------------------------
  ! Create component, states, and clock
  ! -------------------------------------------------------------------

  ! Create the HEMCO component
  hemcoComp = ESMF_GridCompCreate(name="HEMCO", rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Set component attributes from environment or config
  CALL ESMF_AttributeSet(hemcoComp, name='HEMCO_CONFIG', value=configFile, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  CALL ESMF_AttributeSet(hemcoComp, name='HEMCO_DIAGN', value=diagFile, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  CALL ESMF_AttributeSet(hemcoComp, name='HEMCO_LOGFILE', value=logFile, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  CALL ESMF_AttributeSet(hemcoComp, name='HEMCO_DATA_ROOT', value=hco_datadir, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Register the HEMCO component
  CALL HCOI_NUOPC_SS(hemcoComp, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Create import and export states
  importState = ESMF_StateCreate(name="HEMCO Import", stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  exportState = ESMF_StateCreate(name="HEMCO Export", stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Create a time interval for stepping the model forward
  CALL ESMF_TimeIntervalSet(timeStep, s=timeStepSeconds, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Set the start time and end time
  CALL ESMF_TimeSet(startTime, yy=yearStart, mm=monthStart, dd=dayStart, &
                    h=hourStart, m=minStart, s=secStart, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  CALL ESMF_TimeSet(stopTime, yy=yearEnd, mm=monthEnd, dd=dayEnd, &
                   h=hourEnd, m=minEnd, s=secEnd, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Create the clock
  clock = ESMF_ClockCreate(name="HEMCO Clock", &
                          timeStep=timeStep, &
                          startTime=startTime, &
                          stopTime=stopTime, &
                          rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! -------------------------------------------------------------------
  ! Initialize, Run, and Finalize Component
  ! -------------------------------------------------------------------

  ! Initialize HEMCO
  IF (localPet == 0) WRITE(*,*) "HEMCO-NUOPC: Initializing HEMCO..."
  CALL ESMF_GridCompInitialize(hemcoComp, importState=importState, &
                              exportState=exportState, clock=clock, &
                              userRc=userRc, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  IF (ESMF_LogFoundError(rcToCheck=userRc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Run the component for a set amount of time
  timeLoop : DO WHILE (.NOT. ESMF_ClockIsStopTime(clock, rc=rc))
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    IF (localPet == 0) THEN
      CALL ESMF_ClockPrint(clock, options="currtime string", &
                          preString="HEMCO-NUOPC: Current time: ", rc=rc)
      IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    ENDIF

    ! Run HEMCO for one time step
    CALL ESMF_GridCompRun(hemcoComp, importState=importState, &
                         exportState=exportState, clock=clock, &
                         userRc=userRc, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    IF (ESMF_LogFoundError(rcToCheck=userRc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Advance the clock
    CALL ESMF_ClockAdvance(clock, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  ENDDO timeLoop

  ! Finalize HEMCO
  IF (localPet == 0) WRITE(*,*) "HEMCO-NUOPC: Finalizing HEMCO..."
  CALL ESMF_GridCompFinalize(hemcoComp, importState=importState, &
                            exportState=exportState, clock=clock, &
                            userRc=userRc, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  IF (ESMF_LogFoundError(rcToCheck=userRc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Clean up
  CALL ESMF_GridCompDestroy(hemcoComp, rc=rc)
  CALL ESMF_StateDestroy(importState, rc=rc)
  CALL ESMF_StateDestroy(exportState, rc=rc)
  CALL ESMF_ClockDestroy(clock, rc=rc)

  ! Finalize ESMF
  CALL ESMF_Finalize(rc=rc)
  IF (localPet == 0) WRITE(*,*) "HEMCO-NUOPC: Execution completed successfully"

CONTAINS

  !-----------------------------------------------------------------------
  ! Subroutine to get environment variables with defaults for strings
  !-----------------------------------------------------------------------
  SUBROUTINE Get_Env_Var(varName, value, default)
    CHARACTER(LEN=*), INTENT(IN)  :: varName
    CHARACTER(LEN=*), INTENT(OUT) :: value
    CHARACTER(LEN=*), INTENT(IN)  :: default

    ! Local variables
    INTEGER :: status

    ! Try to get the environment variable
    CALL GetEnvironmentVariable(varName, value, status=status)

    ! If not found or empty, use the default
    IF (status /= 0 .OR. LEN_TRIM(value) == 0) THEN
      value = default
    ENDIF
  END SUBROUTINE Get_Env_Var

  !-----------------------------------------------------------------------
  ! Subroutine to get environment variables with defaults for integers
  !-----------------------------------------------------------------------
  SUBROUTINE Get_Env_Var(varName, value, default)
    CHARACTER(LEN=*), INTENT(IN)  :: varName
    INTEGER, INTENT(OUT) :: value
    INTEGER, INTENT(IN)  :: default

    ! Local variables
    CHARACTER(LEN=32) :: strValue
    INTEGER :: status, readStatus

    ! Try to get the environment variable
    CALL GetEnvironmentVariable(varName, strValue, status=status)

    ! If not found or empty, use the default
    IF (status /= 0 .OR. LEN_TRIM(strValue) == 0) THEN
      value = default
    ELSE
      ! Try to convert to integer
      READ(strValue, *, IOSTAT=readStatus) value
      IF (readStatus /= 0) value = default
    ENDIF
  END SUBROUTINE Get_Env_Var

  !-----------------------------------------------------------------------
  ! Subroutine to get environment variables with defaults for logicals
  !-----------------------------------------------------------------------
  SUBROUTINE Get_Env_Var(varName, value, default)
    CHARACTER(LEN=*), INTENT(IN)  :: varName
    LOGICAL, INTENT(OUT) :: value
    LOGICAL, INTENT(IN)  :: default

    ! Local variables
    CHARACTER(LEN=32) :: strValue
    INTEGER :: status

    ! Try to get the environment variable
    CALL GetEnvironmentVariable(varName, strValue, status=status)

    ! If not found or empty, use the default
    IF (status /= 0 .OR. LEN_TRIM(strValue) == 0) THEN
      value = default
    ELSE
      ! Convert to logical based on string value
      strValue = ADJUSTL(strValue)
      IF (strValue(1:1) == 'T' .OR. strValue(1:1) == 't' .OR. &
          strValue(1:1) == 'Y' .OR. strValue(1:1) == 'y' .OR. &
          strValue == '1' .OR. strValue == '.TRUE.') THEN
        value = .TRUE.
      ELSE
        value = .FALSE.
      ENDIF
    ENDIF
  END SUBROUTINE Get_Env_Var

END PROGRAM HEMCO_NUOPC_Main
#endif