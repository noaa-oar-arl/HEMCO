!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: HCO_NUOPCGridCompMod.F90
!
! !DESCRIPTION: Module HCO\_NUOPCGridCompMod contains the NUOPC interface
! implementation for the HEMCO component. This module defines the NUOPC component
! specializations that integrate HEMCO into the NUOPC framework without requiring
! MAPL. It provides methods to import and export data through the NUOPC interface.
!
! The module supports two operation modes:
!  1. Coupled mode: HEMCO operates as a coupled component in a NUOPC-based
!     Earth system model, exchanging data with other components.
!  2. Standalone mode: HEMCO runs independently, managing its own I/O operations
!     similar to the HEMCO standalone executable.
!\\
!\\
! !INTERFACE:
!
MODULE HCOI_NUOPCGridCompMod
!
! !USES:
!
  USE ESMF
  USE NUOPC
  USE NUOPC_Model, &
       model_routine_SS    => SetServices, &
       model_label_SetClock => label_SetClock, &
       model_label_Advance => label_Advance

  USE HCO_Error_Mod,       ONLY : HCO_SUCCESS, HCO_FAIL
  USE HCO_Error_Mod,       ONLY : HCO_ERROR, HCO_WARNING
  USE HCO_State_Mod,       ONLY : HCO_State
  USE HCO_TYPES_MOD,       ONLY : HcoClock
  USE HCO_CLOCK_MOD,       ONLY : HcoClock_Init, HcoClock_Set
  USE HCO_Driver_Mod,      ONLY : HCO_Init, HCO_Run, HCO_Final
  USE HCO_Config_Mod,      ONLY : Config_ReadFile
  USE HCO_ExtList_Mod,     ONLY : GetExtOpt
  USE HCO_Diagn_Mod,       ONLY : DiagnFileOpen, DiagnFileGetNext, DiagnFileClose
  USE HCOIO_DIAGN_MOD,     ONLY : HcoDiagn_Write

  IMPLICIT NONE
  PRIVATE

! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: SetServices
  PUBLIC :: HcoiSetServices       ! Alias for use in other modules
  PUBLIC :: InitializeP0
  PUBLIC :: InitializeP1
  PUBLIC :: InitializeP2
  PUBLIC :: InitializeP3
  PUBLIC :: InitializeP4
  PUBLIC :: ModelAdvance
  PUBLIC :: SetClock
  PUBLIC :: Finalize

!
! !PRIVATE MEMBER FUNCTIONS:
!
  ! Helper routines
  PRIVATE :: CreateGrid
  PRIVATE :: UpdateHemcoClock
  PRIVATE :: AdvertiseFields
  PRIVATE :: RealizeFields
  PRIVATE :: ExportHEMCOData
  PRIVATE :: ImportHEMCOData

! !PRIVATE DATA MEMBERS:
!
  ! Component name
  CHARACTER(LEN=ESMF_MAXSTR) :: CompName = "HEMCO"

  ! Define the HEMCO NUOPC interface structure
  TYPE HCOI_TYPE
     ! HEMCO specific variables
     TYPE(HCO_State), POINTER :: HcoState     => NULL()
     TYPE(HcoClock), POINTER :: HcoClock     => NULL()

     ! NUOPC interface variables
     TYPE(ESMF_State)         :: importState
     TYPE(ESMF_State)         :: exportState
     TYPE(ESMF_Clock)         :: clock
     TYPE(ESMF_Grid)          :: grid

     ! Configuration
     CHARACTER(LEN=ESMF_MAXPATHLEN) :: configFile = ""

     ! Mode of operation
     LOGICAL                  :: standaloneMode = .FALSE.

     ! Status flags
     LOGICAL                  :: initialized  = .FALSE.
  END TYPE HCOI_TYPE

  ! Global HEMCO NUOPC interface instance
  TYPE(HCOI_TYPE), SAVE :: HcoiState

! !REVISION HISTORY:
!  22 Apr 2025 - Initial version
!  22 Apr 2025 - Added support for standalone mode
!EOP
!------------------------------------------------------------------------------
!BOC
CONTAINS

  !-----------------------------------------------------------------------------
  !BOP
  !
  ! !ROUTINE: SetServices
  !
  ! !DESCRIPTION: Sets the NUOPC services for the HEMCO component
  !\\
  !\\
  ! !INTERFACE:
  !
  SUBROUTINE SetServices(gcomp, rc)
    TYPE(ESMF_GridComp)  :: gcomp  ! Grid component
    INTEGER, INTENT(OUT) :: rc     ! Return code

    ! Begin
    rc = ESMF_SUCCESS

    ! Register standard NUOPC model component services
    CALL NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) RETURN

    ! Set entry points for Initialize, Run, and Finalize phases
    ! - Initialize Phase
    CALL NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p1"/), userRoutine=InitializeP0, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) RETURN

    CALL NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p2"/), userRoutine=InitializeP1, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) RETURN

    CALL NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p3"/), userRoutine=InitializeP2, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) RETURN

    CALL NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p4"/), userRoutine=InitializeP3, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) RETURN

    CALL NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p5"/), userRoutine=InitializeP4, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) RETURN

    ! - Run Phase
    CALL NUOPC_CompSpecialize(gcomp, specLabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) RETURN

    ! - Set Clock Phase (special)
    CALL NUOPC_CompSpecialize(gcomp, specLabel=model_label_SetClock, &
      specRoutine=SetClock, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) RETURN

    ! - Finalize Phase
    CALL ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, &
      userRoutine=Finalize, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) RETURN

    ! Create internal HEMCO state if needed
    IF (.NOT. ASSOCIATED(HcoiState%HcoState)) THEN
       ALLOCATE(HcoiState%HcoState)
       ALLOCATE(HcoiState%HcoClock)
    ENDIF

  END SUBROUTINE SetServices

  !-----------------------------------------------------------------------------
  ! Alias for the SetServices routine to avoid naming conflicts in other modules
  !-----------------------------------------------------------------------------
  SUBROUTINE HcoiSetServices(gcomp, rc)
    TYPE(ESMF_GridComp)  :: gcomp
    INTEGER, INTENT(OUT) :: rc

    CALL SetServices(gcomp, rc)
  END SUBROUTINE HcoiSetServices

  !-----------------------------------------------------------------------------
  !BOP
  !
  ! !IROUTINE: InitializeP0
  !
  ! !DESCRIPTION: First initialization phase - Basic setup and configuration
  !\\
  !\\
  ! !INTERFACE:
  !
  SUBROUTINE InitializeP0(gcomp, importState, exportState, clock, rc)
    TYPE(ESMF_GridComp)  :: gcomp
    TYPE(ESMF_State)     :: importState, exportState
    TYPE(ESMF_Clock)     :: clock
    INTEGER, INTENT(OUT) :: rc

    ! Local variables
    TYPE(ESMF_Config)           :: config
    CHARACTER(LEN=ESMF_MAXSTR)  :: msgString
    LOGICAL                     :: isPresent, am_I_Root
    INTEGER                     :: status, localPet

    ! Begin
    rc = ESMF_SUCCESS

    ! Store states and clock
    HcoiState%importState = importState
    HcoiState%exportState = exportState
    HcoiState%clock = clock

    ! Get component name
    CALL ESMF_GridCompGet(gcomp, name=CompName, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

    ! Determine if we're on the root PE
    CALL ESMF_GridCompGet(gcomp, localPet=localPet, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN
    am_I_Root = (localPet == 0)

    ! Get the component's configuration
    CALL ESMF_GridCompGet(gcomp, config=config, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

    ! Read in the HEMCO configuration file name
    CALL ESMF_ConfigGetAttribute(config, HcoiState%configFile, &
         label="HEMCO_CONFIG:", rc=status)

    IF (status == ESMF_SUCCESS) THEN
       WRITE(msgString, '(A,A)') "Using HEMCO configuration file: ", &
            TRIM(HcoiState%configFile)
       CALL ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    ELSE
       ! If not in config, use default
       HcoiState%configFile = "HEMCO_Config.rc"
       WRITE(msgString, '(A,A)') "Using default HEMCO configuration file: ", &
            TRIM(HcoiState%configFile)
       CALL ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    ENDIF

    ! Check for standalone mode setting
    CALL ESMF_ConfigGetAttribute(config, HcoiState%standaloneMode, &
         label="HEMCO_STANDALONE:", default=.FALSE., rc=status)

    IF (HcoiState%standaloneMode) THEN
       WRITE(msgString, '(A)') "HEMCO running in standalone mode"
       CALL ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    ELSE
       WRITE(msgString, '(A)') "HEMCO running in coupled mode"
       CALL ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    ENDIF

    ! Check if configuration file exists
    INQUIRE(FILE=TRIM(HcoiState%configFile), EXIST=isPresent)
    IF (.NOT. isPresent) THEN
       WRITE(msgString, '(A,A)') "HEMCO configuration file not found: ", &
            TRIM(HcoiState%configFile)
       CALL ESMF_LogWrite(msgString, ESMF_LOGMSG_ERROR, rc=rc)
       rc = ESMF_FAILURE
       RETURN
    ENDIF

    ! Initialize HEMCO Clock
    CALL HcoClock_Init(HcoiState%HcoState, RC=status)
    IF (status /= HCO_SUCCESS) THEN
        CALL ESMF_LogWrite("Failed to initialize HEMCO clock", &
                         ESMF_LOGMSG_ERROR, rc=rc)
        rc = ESMF_FAILURE
        RETURN
    ENDIF

    ! Read HEMCO configuration file
    CALL Config_ReadFile(am_I_Root, HcoiState%HcoState%Config, &
                         TRIM(HcoiState%configFile), 0, status)
    IF (status /= HCO_SUCCESS) THEN
        CALL ESMF_LogWrite("Failed to read HEMCO configuration file", &
                         ESMF_LOGMSG_ERROR, rc=rc)
        rc = ESMF_FAILURE
        RETURN
    ENDIF

    ! Mark not initialized yet - will be set after complete initialization
    HcoiState%initialized = .FALSE.

  END SUBROUTINE InitializeP0

  !-----------------------------------------------------------------------------
  !BOP
  !
  ! !IROUTINE: InitializeP1
  !
  ! !DESCRIPTION: Advertise import/export fields in the NUOPC initialize phase
  !\\
  !\\
  ! !INTERFACE:
  !
  SUBROUTINE InitializeP1(gcomp, importState, exportState, clock, rc)
    TYPE(ESMF_GridComp)  :: gcomp
    TYPE(ESMF_State)     :: importState, exportState
    TYPE(ESMF_Clock)     :: clock
    INTEGER, INTENT(OUT) :: rc

    ! Begin
    rc = ESMF_SUCCESS

    ! Advertise the fields that HEMCO can provide
    CALL AdvertiseFields(gcomp, exportState, rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

    ! In standalone mode we don't need to advertise import fields
    ! as HEMCO will handle its own input data

  END SUBROUTINE InitializeP1

  !-----------------------------------------------------------------------------
  !BOP
  !
  ! !IROUTINE: InitializeP2
  !
  ! !DESCRIPTION: Second initialization phase - Create grid and realize fields
  !\\
  !\\
  ! !INTERFACE:
  !
  SUBROUTINE InitializeP2(gcomp, importState, exportState, clock, rc)
    TYPE(ESMF_GridComp)  :: gcomp
    TYPE(ESMF_State)     :: importState, exportState
    TYPE(ESMF_Clock)     :: clock
    INTEGER, INTENT(OUT) :: rc

    ! Local variables
    LOGICAL :: am_I_Root
    INTEGER :: status, localPet

    ! Begin
    rc = ESMF_SUCCESS

    ! Determine if we're on the root PE
    CALL ESMF_GridCompGet(gcomp, localPet=localPet, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN
    am_I_Root = (localPet == 0)

    ! Create grid based on HEMCO configuration
    CALL CreateGrid(gcomp, HcoiState%grid, rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

    ! Configure standalone vs. coupled mode for HEMCO
    IF (HcoiState%standaloneMode) THEN
       ! In standalone mode, HEMCO should handle its own I/O operations
       HcoiState%HcoState%Options%HcoWritesDiagn = .TRUE.
       HcoiState%HcoState%Options%isESMF = .FALSE.
    ELSE
       ! In coupled mode, HEMCO data is handled through NUOPC
       HcoiState%HcoState%Options%HcoWritesDiagn = .FALSE.
       HcoiState%HcoState%Options%isESMF = .TRUE.
    ENDIF

    ! Field to diagnostics is redundant in ESMF coupled mode but
    ! useful for standalone operation
    HcoiState%HcoState%Options%Field2Diagn = HcoiState%standaloneMode

    ! Initialize HEMCO with proper grid information
    CALL HCO_Init(HcoiState%HcoState, RC=status)
    IF (status /= HCO_SUCCESS) THEN
        CALL ESMF_LogWrite("Failed to initialize HEMCO", &
                         ESMF_LOGMSG_ERROR, rc=rc)
        rc = ESMF_FAILURE
        RETURN
    ENDIF

    ! Realize the fields (allocate memory and associate with grid)
    CALL RealizeFields(exportState, HcoiState%grid, rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

  END SUBROUTINE InitializeP2

  !-----------------------------------------------------------------------------
  !BOP
  !
  ! !IROUTINE: InitializeP3
  !
  ! !DESCRIPTION: Third initialization phase
  !\\
  !\\
  ! !INTERFACE:
  !
  SUBROUTINE InitializeP3(gcomp, importState, exportState, clock, rc)
    TYPE(ESMF_GridComp)  :: gcomp
    TYPE(ESMF_State)     :: importState, exportState
    TYPE(ESMF_Clock)     :: clock
    INTEGER, INTENT(OUT) :: rc

    ! Begin
    rc = ESMF_SUCCESS

    ! Nothing to do in this phase for now
    ! Could implement data initialization or other setup here

  END SUBROUTINE InitializeP3

  !-----------------------------------------------------------------------------
  !BOP
  !
  ! !IROUTINE: InitializeP4
  !
  ! !DESCRIPTION: Final initialization phase - Mark as initialized
  !\\
  !\\
  ! !INTERFACE:
  !
  SUBROUTINE InitializeP4(gcomp, importState, exportState, clock, rc)
    TYPE(ESMF_GridComp)  :: gcomp
    TYPE(ESMF_State)     :: importState, exportState
    TYPE(ESMF_Clock)     :: clock
    INTEGER, INTENT(OUT) :: rc

    ! Begin
    rc = ESMF_SUCCESS

    ! Set the HEMCO State as initialized
    HcoiState%initialized = .TRUE.

    ! Export initial data
    CALL ExportHEMCOData(gcomp, exportState, rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

    ! Log success
    CALL ESMF_LogWrite("HEMCO NUOPC Interface successfully initialized", &
                      ESMF_LOGMSG_INFO, rc=rc)

  END SUBROUTINE InitializeP4

  !-----------------------------------------------------------------------------
  !BOP
  !
  ! !ROUTINE: SetClock
  !
  ! !DESCRIPTION: Sets the component clock from the parent clock
  !\\
  !\\
  ! !INTERFACE:
  !
  SUBROUTINE SetClock(gcomp, rc)
    TYPE(ESMF_GridComp)  :: gcomp
    INTEGER, INTENT(OUT) :: rc

    ! Local variables
    TYPE(ESMF_Clock)        :: parentClock, clock
    TYPE(ESMF_Time)         :: currTime, startTime, stopTime
    TYPE(ESMF_TimeInterval) :: timeStep
    CHARACTER(LEN=ESMF_MAXSTR) :: message
    INTEGER                  :: yy, mm, dd, h, m, s

    ! Begin
    rc = ESMF_SUCCESS

    ! Get the parent clock
    CALL NUOPC_ModelGet(gcomp, driverClock=parentClock, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

    ! Get times from parent clock
    CALL ESMF_ClockGet(parentClock, currTime=currTime, &
                     startTime=startTime, stopTime=stopTime, &
                     timeStep=timeStep, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

    ! Create component clock
    clock = ESMF_ClockCreate(name="HEMCO Clock", &
                           timeStep=timeStep, &
                           startTime=startTime, &
                           stopTime=stopTime, &
                           rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

    ! Advance to current time
    CALL ESMF_ClockSet(clock, currTime=currTime, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

    ! Set the component clock
    CALL ESMF_GridCompSet(gcomp, clock=clock, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

    ! Save clock in our module state
    HcoiState%clock = clock

    ! Get current time info and update HEMCO clock
    CALL UpdateHemcoClock(clock, rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

    ! Get time for logging
    CALL ESMF_TimeGet(currTime, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

    ! Log message about the clock
    WRITE(message,'(A,I4.4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2)') &
         "HEMCO NUOPC Interface clock set to: ", &
         yy, "-", mm, "-", dd, " ", h, ":", m, ":", s
    CALL ESMF_LogWrite(message, ESMF_LOGMSG_INFO, rc=rc)

  END SUBROUTINE SetClock

  !-----------------------------------------------------------------------------
  !BOP
  !
  ! !ROUTINE: ModelAdvance
  !
  ! !DESCRIPTION: Advances the model (HEMCO) one timestep
  !\\
  !\\
  ! !INTERFACE:
  !
  SUBROUTINE ModelAdvance(gcomp, rc)
    TYPE(ESMF_GridComp)  :: gcomp
    INTEGER, INTENT(OUT) :: rc

    ! Local variables
    TYPE(ESMF_Clock)     :: clock
    TYPE(ESMF_Time)      :: currTime
    TYPE(ESMF_State)     :: importState, exportState
    INTEGER              :: yy, mm, dd, h, m, s, status, localPet
    LOGICAL              :: am_I_Root
    CHARACTER(LEN=ESMF_MAXSTR) :: message

    ! Begin
    rc = ESMF_SUCCESS

    ! Get the clock and states
    CALL ESMF_GridCompGet(gcomp, clock=clock, &
                        importState=importState, &
                        exportState=exportState, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

    ! Determine if we're on the root PE
    CALL ESMF_GridCompGet(gcomp, localPet=localPet, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN
    am_I_Root = (localPet == 0)

    ! Get current time for logging
    CALL ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

    CALL ESMF_TimeGet(currTime, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

    ! Update HEMCO clock from ESMF clock
    CALL UpdateHemcoClock(clock, rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

    ! Log the time
    WRITE(message,'(A,I4.4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2)') &
         "Running HEMCO at: ", yy, "/", mm, "/", dd, " ", h, ":", m, ":", s
    CALL ESMF_LogWrite(message, ESMF_LOGMSG_INFO, rc=rc)

    ! In coupled mode, we need to import data from other components
    IF (.NOT. HcoiState%standaloneMode) THEN
       ! Import data from other components via NUOPC
       CALL ImportHEMCOData(gcomp, importState, rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) RETURN
    ENDIF

    ! Run HEMCO with updated clock
    CALL HCO_Run(HcoiState%HcoState, Phase=1, RC=status)
    IF (status /= HCO_SUCCESS) THEN
        CALL ESMF_LogWrite("HEMCO_Run failed", ESMF_LOGMSG_ERROR, rc=rc)
        rc = ESMF_FAILURE
        RETURN
    ENDIF

    ! In standalone mode, HEMCO writes its own diagnostics output
    IF (HcoiState%standaloneMode) THEN
       ! Write diagnostics using HEMCO's internal I/O system
       CALL HcoDiagn_Write(HcoiState%HcoState, .FALSE., status)
       IF (status /= HCO_SUCCESS) THEN
          CALL ESMF_LogWrite("HEMCO diagnostic write failed", &
                           ESMF_LOGMSG_WARNING, rc=rc)
       ENDIF
    ENDIF

    ! Export HEMCO data to ESMF export state (even in standalone mode,
    ! this ensures the data is available to other components if needed)
    CALL ExportHEMCOData(gcomp, exportState, rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

  END SUBROUTINE ModelAdvance

  !-----------------------------------------------------------------------------
  !BOP
  !
  ! !ROUTINE: Finalize
  !
  ! !DESCRIPTION: Finalize the component
  !\\
  !\\
  ! !INTERFACE:
  !
  SUBROUTINE Finalize(gcomp, importState, exportState, clock, rc)
    TYPE(ESMF_GridComp)  :: gcomp
    TYPE(ESMF_State)     :: importState    ! Import state
    TYPE(ESMF_State)     :: exportState    ! Export state
    TYPE(ESMF_Clock)     :: clock          ! Clock
    INTEGER, INTENT(OUT) :: rc

    ! Local variables
    INTEGER :: status, localPet
    LOGICAL :: am_I_Root
    CHARACTER(LEN=ESMF_MAXSTR) :: message

    ! Begin
    rc = ESMF_SUCCESS

    ! Determine if we're on the root PE
    CALL ESMF_GridCompGet(gcomp, localPet=localPet, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN
    am_I_Root = (localPet == 0)

    ! Clean up HEMCO
    IF (HcoiState%initialized) THEN
       CALL ESMF_LogWrite("Finalizing HEMCO NUOPC Interface", &
                        ESMF_LOGMSG_INFO, rc=rc)

       ! Finalize HEMCO
       CALL HCO_Final(HcoiState%HcoState, am_I_Root, status)
       IF (status /= HCO_SUCCESS) THEN
          CALL ESMF_LogWrite("Error shutting down HEMCO", &
                           ESMF_LOGMSG_WARNING, rc=rc)
          ! Continue with cleanup even if shutdown fails
       ENDIF
    ENDIF

    ! Clean up HEMCO state
    IF (ASSOCIATED(HcoiState%HcoState)) THEN
       DEALLOCATE(HcoiState%HcoState)
       NULLIFY(HcoiState%HcoState)
    ENDIF

    ! Clean up HEMCO clock
    IF (ASSOCIATED(HcoiState%HcoClock)) THEN
       DEALLOCATE(HcoiState%HcoClock)
       NULLIFY(HcoiState%HcoClock)
    ENDIF

    ! Mark as not initialized
    HcoiState%initialized = .FALSE.

    ! Log completion
    CALL ESMF_LogWrite("HEMCO NUOPC Interface finalized", &
                     ESMF_LOGMSG_INFO, rc=rc)

  END SUBROUTINE Finalize

  !-----------------------------------------------------------------------------
  !BOP
  !
  ! !ROUTINE: UpdateHemcoClock
  !
  ! !DESCRIPTION: Updates the HEMCO clock from an ESMF clock
  !\\
  !\\
  ! !INTERFACE:
  !
  SUBROUTINE UpdateHemcoClock(clock, rc)
    TYPE(ESMF_Clock)     :: clock
    INTEGER, INTENT(OUT) :: rc

    ! Local variables
    TYPE(ESMF_Time)         :: currTime
    TYPE(ESMF_TimeInterval) :: timeStep
    INTEGER                 :: yy, mm, dd, h, m, s
    INTEGER                 :: days, seconds
    INTEGER                 :: hcoRC

    ! Begin
    rc = ESMF_SUCCESS

    ! Get current time from ESMF clock
    CALL ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

    ! Get year, month, day, hour, minute, second from currTime
    CALL ESMF_TimeGet(currTime, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

    ! Get timestep in seconds
    CALL ESMF_TimeIntervalGet(timeStep, d=days, s=seconds, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

    ! Calculate total seconds in timestep
    seconds = days*86400 + seconds

    ! Update the HEMCO clock
    CALL HcoClock_Set(HcoiState%HcoState, yy, mm, dd, h, m, s, &
                     IsEmisTime=.TRUE., RC=hcoRC)
    IF (hcoRC /= HCO_SUCCESS) THEN
       rc = ESMF_FAILURE
       RETURN
    ENDIF

  END SUBROUTINE UpdateHemcoClock

  !-----------------------------------------------------------------------------
  !BOP
  !
  ! !ROUTINE: CreateGrid
  !
  ! !DESCRIPTION: Creates an ESMF grid from HEMCO configuration
  !\\
  !\\
  ! !INTERFACE:
  !
  SUBROUTINE CreateGrid(gcomp, grid, rc)
    TYPE(ESMF_GridComp)  :: gcomp
    TYPE(ESMF_Grid)      :: grid
    INTEGER, INTENT(OUT) :: rc

    ! Local variables
    INTEGER              :: im, jm, km
    INTEGER              :: i, j
    REAL(ESMF_KIND_R8), ALLOCATABLE :: centerLon(:,:), centerLat(:,:)
    REAL(ESMF_KIND_R8), ALLOCATABLE :: cornerLon(:,:), cornerLat(:,:)
    LOGICAL              :: am_I_Root
    CHARACTER(LEN=ESMF_MAXSTR) :: msgString
    INTEGER              :: localPet

    ! Begin
    rc = ESMF_SUCCESS

    ! Determine if we're on the root PE
    CALL ESMF_GridCompGet(gcomp, localPet=localPet, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN
    am_I_Root = (localPet == 0)

    ! Get grid dimensions from HEMCO config
    ! For now, we'll use a simple global grid - this should be enhanced
    ! to read the actual grid definition from HEMCO config
    im = 72  ! 5 degree longitude
    jm = 46  ! 4 degree latitude
    km = 1   ! Single layer

    ! Log grid creation
    WRITE(msgString,'(A,I0,A,I0)') "Creating HEMCO grid: ", im, "x", jm
    CALL ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

    ! Create 2D grid
    grid = ESMF_GridCreateNoPeriDim( &
           minIndex=(/1,1/), &
           maxIndex=(/im,jm/), &
           coordDep1=(/1,2/), &
           coordDep2=(/1,2/), &
           coordSys=ESMF_COORDSYS_SPH_DEG, &
           name="HEMCO_Grid", &
           rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

    ! Allocate coordinate arrays
    ALLOCATE(centerLon(im,jm), centerLat(im,jm))
    ALLOCATE(cornerLon(im+1,jm+1), cornerLat(im+1,jm+1))

    ! Get the coordinate arrays by reference and set the values
    ! First create the coordinate arrays
    DO j = 1, jm
       DO i = 1, im
          centerLon(i,j) = -180.0 + (i-0.5) * (360.0/im)
          centerLat(i,j) = -90.0 + (j-0.5) * (180.0/jm)
       ENDDO
    ENDDO

    DO j = 1, jm+1
       DO i = 1, im+1
          cornerLon(i,j) = -180.0 + (i-1) * (360.0/im)
          cornerLat(i,j) = -90.0 + (j-1) * (180.0/jm)
       ENDDO
    ENDDO

    ! Create the grid with coordinates
    grid = ESMF_GridCreate(minIndex=(/1,1/), &
                          maxIndex=(/im,jm/), &
                          coordDep1=(/1,2/), &
                          coordDep2=(/1,2/), &
                          coordSys=ESMF_COORDSYS_SPH_DEG, &
                          name="HEMCO_Grid", &
                          rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

    ! Add grid dimensions to HEMCO state
    HcoiState%HcoState%NX = im
    HcoiState%HcoState%NY = jm
    HcoiState%HcoState%NZ = km

    ! Add metadata to grid
    CALL ESMF_AttributeSet(grid, name="GridType", value="HEMCO_Grid", rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

  END SUBROUTINE CreateGrid

  !-----------------------------------------------------------------------------
  !BOP
  !
  ! !ROUTINE: AdvertiseFields
  !
  ! !DESCRIPTION: Advertises the fields that HEMCO will provide
  !\\
  !\\
  ! !INTERFACE:
  !
  SUBROUTINE AdvertiseFields(gcomp, exportState, rc)
    TYPE(ESMF_GridComp)  :: gcomp
    TYPE(ESMF_State)     :: exportState
    INTEGER, INTENT(OUT) :: rc

    ! Local variables
    INTEGER               :: LUN, ExtNr, Cat, Hier, SpaceDim
    INTEGER               :: I, FLAG, status
    LOGICAL               :: EOF
    CHARACTER(LEN=31)     :: cName, SpcName, OutUnit
    CHARACTER(LEN=63)     :: UnitName
    CHARACTER(LEN=127)    :: lName
    TYPE(ESMF_Field)      :: field

    ! Begin
    rc = ESMF_SUCCESS

    ! Try to open diagnostics definition file from HEMCO configuration
    CALL DiagnFileOpen(HcoiState%HcoState%Config, LUN, status)
    IF (status /= HCO_SUCCESS) THEN
        CALL ESMF_LogWrite("Error opening HEMCO diagnostics file", &
                         ESMF_LOGMSG_ERROR, rc=rc)
        rc = ESMF_FAILURE
        RETURN
    ENDIF

    ! If DiagnFile is found, advertise diagnostics for every entry
    IF (LUN > 0) THEN
       CALL ESMF_LogWrite("Reading HEMCO diagnostics from configuration file", &
                        ESMF_LOGMSG_INFO, rc=rc)

       DO
          ! Get next line
          CALL DiagnFileGetNext(HcoiState%HcoState%Config, LUN, cName, &
                              SpcName, ExtNr, Cat, Hier, &
                              SpaceDim, OutUnit, EOF, status, &
                              lName=lName, UnitName=UnitName)
          IF (status /= HCO_SUCCESS) THEN
             CALL ESMF_LogWrite("Error reading HEMCO diagnostics file", &
                              ESMF_LOGMSG_ERROR, rc=rc)
             rc = ESMF_FAILURE
             RETURN
          ENDIF

          ! Leave here if end of file
          IF (EOF) EXIT

          ! Remove any underscores in unit name with spaces
          DO I = 1, LEN(TRIM(ADJUSTL(UnitName)))
             IF (UnitName(I:I) == '_') UnitName(I:I) = ' '
          ENDDO

          ! Remove any underscores in long name with spaces
          DO I = 1, LEN(TRIM(ADJUSTL(lName)))
             IF (lName(I:I) == '_') lName(I:I) = ' '
          ENDDO

          ! Advertise this field
          field = ESMF_FieldEmptyCreate(name=TRIM(cName), rc=rc)
          IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, file=__FILE__)) RETURN

          ! Add field info as attributes
          CALL ESMF_AttributeSet(field, "StandardName", TRIM(cName), rc=rc)
          IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, file=__FILE__)) RETURN

          CALL ESMF_AttributeSet(field, "LongName", TRIM(lName), rc=rc)
          IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, file=__FILE__)) RETURN

          CALL ESMF_AttributeSet(field, "Units", TRIM(UnitName), rc=rc)
          IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, file=__FILE__)) RETURN

          ! Add field to export state
          CALL ESMF_StateAdd(exportState, (/field/), rc=rc)
          IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, file=__FILE__)) RETURN

          ! Log the field we're advertising
          CALL ESMF_LogWrite("Advertising HEMCO export: " // TRIM(cName), &
                           ESMF_LOGMSG_INFO, rc=rc)
       ENDDO

       ! Close file
       CALL DiagnFileClose(LUN)
    ELSE
       ! No diagnostics file found
       CALL ESMF_LogWrite("No HEMCO diagnostics file found, " // &
                        "no fields will be exported", &
                        ESMF_LOGMSG_WARNING, rc=rc)
    ENDIF

  END SUBROUTINE AdvertiseFields

  !-----------------------------------------------------------------------------
  !BOP
  !
  ! !ROUTINE: RealizeFields
  !
  ! !DESCRIPTION: Creates fields in export state and assigns grid
  !\\
  !\\
  ! !INTERFACE:
  !
  SUBROUTINE RealizeFields(exportState, grid, rc)
    TYPE(ESMF_State)     :: exportState
    TYPE(ESMF_Grid)      :: grid
    INTEGER, INTENT(OUT) :: rc

    ! Local variables
    INTEGER :: itemCount, i
    CHARACTER(LEN=ESMF_MAXSTR), ALLOCATABLE :: fieldNames(:)
    TYPE(ESMF_Field) :: field

    ! Begin
    rc = ESMF_SUCCESS

    ! Get field count
    CALL ESMF_StateGet(exportState, itemCount=itemCount, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

    ! If no fields, return
    IF (itemCount == 0) THEN
       CALL ESMF_LogWrite("No fields to realize in export state", &
                        ESMF_LOGMSG_INFO, rc=rc)
       RETURN
    ENDIF

    ! Get field names
    ALLOCATE(fieldNames(itemCount))
    CALL ESMF_StateGet(exportState, itemNameList=fieldNames, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

    ! Create fields with grid
    DO i = 1, itemCount
       ! Create field
       field = ESMF_FieldCreate(name=TRIM(fieldNames(i)), &
                              grid=grid, &
                              typekind=ESMF_TYPEKIND_R8, &
                              staggerloc=ESMF_STAGGERLOC_CENTER, &
                              rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) RETURN

       ! Add field to export state, replacing placeholder
       CALL ESMF_StateRemove(exportState, (/ TRIM(fieldNames(i)) /), rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) RETURN

       CALL ESMF_StateAdd(exportState, (/ field /), rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) RETURN

       ! Log
       CALL ESMF_LogWrite("Realized field: " // TRIM(fieldNames(i)), &
                        ESMF_LOGMSG_INFO, rc=rc)
    ENDDO

    ! Clean up
    DEALLOCATE(fieldNames)

  END SUBROUTINE RealizeFields

  !-----------------------------------------------------------------------------
  !BOP
  !
  ! !ROUTINE: ImportHEMCOData
  !
  ! !DESCRIPTION: Imports required data from ESMF import state to HEMCO
  !\\
  !\\
  ! !INTERFACE:
  !
  SUBROUTINE ImportHEMCOData(gcomp, importState, rc)
    TYPE(ESMF_GridComp)  :: gcomp
    TYPE(ESMF_State)     :: importState
    INTEGER, INTENT(OUT) :: rc

    ! Local variables
    INTEGER :: itemCount, i
    CHARACTER(LEN=ESMF_MAXSTR), ALLOCATABLE :: fieldNames(:)
    TYPE(ESMF_Field) :: field
    CHARACTER(LEN=ESMF_MAXSTR) :: message

    ! Begin
    rc = ESMF_SUCCESS

    ! Only needed in coupled mode
    IF (HcoiState%standaloneMode) THEN
       RETURN
    ENDIF

    ! In coupled mode, we would typically process each field in the import state
    ! and map it to the corresponding HEMCO ExtState field
    !
    ! For now, we'll just log that this is not fully implemented
    CALL ESMF_LogWrite("ImportHEMCOData: Import from coupled model not fully implemented", &
                     ESMF_LOGMSG_INFO, rc=rc)

    ! This would be the place to import meteorology and other fields from
    ! the coupled model and pass them to HEMCO's ExtState

  END SUBROUTINE ImportHEMCOData

  !-----------------------------------------------------------------------------
  !BOP
  !
  ! !ROUTINE: ExportHEMCOData
  !
  ! !DESCRIPTION: Exports HEMCO data to ESMF export state
  !\\
  !\\
  ! !INTERFACE:
  !
  SUBROUTINE ExportHEMCOData(gcomp, exportState, rc)
    TYPE(ESMF_GridComp)  :: gcomp
    TYPE(ESMF_State)     :: exportState
    INTEGER, INTENT(OUT) :: rc

    ! Local variables
    INTEGER :: itemCount, i, j, idx
    CHARACTER(LEN=ESMF_MAXSTR), ALLOCATABLE :: fieldNames(:)
    TYPE(ESMF_Field) :: field
    REAL(ESMF_KIND_R8), POINTER :: dataPtr(:,:)
    REAL(ESMF_KIND_R8), ALLOCATABLE :: tmpData(:,:)
    LOGICAL :: found
    INTEGER :: hcoRC, dims(3)

    ! Begin
    rc = ESMF_SUCCESS

    ! Get field count
    CALL ESMF_StateGet(exportState, itemCount=itemCount, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

    ! If no fields, return
    IF (itemCount == 0) THEN
       RETURN
    ENDIF

    ! Get field names
    ALLOCATE(fieldNames(itemCount))
    CALL ESMF_StateGet(exportState, itemNameList=fieldNames, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) RETURN

    ! Process each field
    DO i = 1, itemCount
       ! Get the field from export state
       CALL ESMF_StateGet(exportState, itemName=TRIM(fieldNames(i)), &
                        field=field, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) RETURN

       ! Get a pointer to the data
       CALL ESMF_FieldGet(field, farrayPtr=dataPtr, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) RETURN

       ! Get diagnostic data from HEMCO
       ! Note: In a full implementation, you would look up the diagnostic by name
       ! and retrieve its data. For this example, we'll just fill with sample data.
       DO j = 1, SIZE(dataPtr,2)
          DO idx = 1, SIZE(dataPtr,1)
             ! Sample data - in real implementation would get from HEMCO
             dataPtr(idx,j) = REAL(idx + j, ESMF_KIND_R8)
          ENDDO
       ENDDO
    ENDDO

    ! Clean up
    DEALLOCATE(fieldNames)

  END SUBROUTINE ExportHEMCOData

END MODULE HCOI_NUOPCGridCompMod
!EOC