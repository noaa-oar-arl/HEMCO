!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: HCOI_ESMxGridCompMod.F90
!
! !DESCRIPTION: Module HCO\_ESMxGridCompMod implements the HEMCO grid component
! for ESMX. It leverages the existing NUOPC interface and adapts it for use
! with ESMX applications.
!\\
!\\
! !INTERFACE:
!
MODULE HCOI_ESMxGridCompMod
!
! !USES:
!
  USE ESMF
  USE NUOPC
  USE NUOPC_Model, &
       model_routine_SS      => SetServices, &
       model_label_Advance   => label_Advance, &
       model_label_Initialize => label_SetClock, &
       model_label_Finalize  => label_Finalize

  ! Import HEMCO modules
  USE HCO_ERROR_MOD
  USE HCOI_NUOPCGridCompMod, ONLY : NUOPC_SetServices => SetServices, &
                                   InitializeP0, InitializeP1, InitializeP2, &
                                   InitializeP3, InitializeP4, &
                                   ModelAdvance, SetClock, Finalize

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: SetServices
!
! !PRIVATE MEMBER FUNCTIONS:
!
  PRIVATE :: Initialize
  PRIVATE :: Run
  PRIVATE :: Finalize_ESMx
!
! !REVISION HISTORY:
!  22 Apr 2025 - Initial version for ESMX support
!EOP
!------------------------------------------------------------------------------
!BOC
CONTAINS

  !-----------------------------------------------------------------------------
  !BOP
  !
  ! !IROUTINE: SetServices
  !
  ! !DESCRIPTION: Sets the NUOPC services for this component
  !\\
  !\\
  ! !INTERFACE:
  !
  SUBROUTINE SetServices(gridComp, rc)
    TYPE(ESMF_GridComp)  :: gridComp  ! Grid component
    INTEGER, INTENT(OUT) :: rc        ! Return code
!
! !LOCAL VARIABLES:
!
    TYPE(ESMF_Config) :: config
    CHARACTER(LEN=ESMF_MAXSTR) :: compType
    CHARACTER(LEN=ESMF_MAXSTR) :: name
    INTEGER :: localrc

    !---------------------------------------------------------------------------
    !  Begin...
    !---------------------------------------------------------------------------
    rc = ESMF_SUCCESS

    ! Get component name for logging
    CALL ESMF_GridCompGet(gridComp, name=name, rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) THEN
      rc = localrc
      RETURN
    ENDIF

    ! Log start of service setup
    CALL ESMF_LogWrite("HEMCO_ESMx: Setting up services for "//TRIM(name), &
                      ESMF_LOGMSG_INFO, rc=localrc)

    ! Register standard NUOPC model component
    CALL NUOPC_CompDerive(gridComp, model_routine_SS, rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    ! We need to use a different approach for all the specialization methods

    ! For the Run phase, use ModelAdvance directly instead of Run
    CALL NUOPC_CompSetEntryPoint(gridComp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"RunPhase1"/), userRoutine=Run, rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    ! For Initialize, use NUOPC_CompSetEntryPoint
    CALL NUOPC_CompSetEntryPoint(gridComp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p1"/), userRoutine=Initialize, rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    ! For Finalize, use ESMF_GridCompSetEntryPoint
    CALL ESMF_GridCompSetEntryPoint(gridComp, ESMF_METHOD_FINALIZE, &
      userRoutine=Finalize_ESMx, rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    ! Get component configuration to determine type
    CALL ESMF_GridCompGet(gridComp, config=config, rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    ! Check if component type is explicitly set to ESMX
    CALL ESMF_ConfigGetAttribute(config, compType, label="COMPONENT_TYPE:", &
                               default="ESMX", rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    ! Log end of service setup
    CALL ESMF_LogWrite("HEMCO_ESMx: Finished setting up services for "//TRIM(name), &
                      ESMF_LOGMSG_INFO, rc=localrc)

  END SUBROUTINE SetServices

  !-----------------------------------------------------------------------------
  !BOP
  !
  ! !IROUTINE: Initialize
  !
  ! !DESCRIPTION: Initialize the HEMCO ESMX component
  !\\
  !\\
  ! !INTERFACE:
  !
  SUBROUTINE Initialize(gridComp, importState, exportState, clock, rc)
    TYPE(ESMF_GridComp)  :: gridComp    ! Grid component
    TYPE(ESMF_State)     :: importState ! Import state
    TYPE(ESMF_State)     :: exportState ! Export state
    TYPE(ESMF_Clock)     :: clock       ! Clock
    INTEGER, INTENT(OUT) :: rc          ! Return code

    ! Local variables
    TYPE(ESMF_Config) :: config
    CHARACTER(LEN=ESMF_MAXSTR) :: configFileName
    CHARACTER(LEN=ESMF_MAXSTR) :: diagFileName
    CHARACTER(LEN=ESMF_MAXSTR) :: name
    LOGICAL :: isPresent
    INTEGER :: localrc

    ! Begin...
    rc = ESMF_SUCCESS

    ! Get component information
    CALL ESMF_GridCompGet(gridComp, name=name, config=config, rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    ! Log start of initialization
    CALL ESMF_LogWrite("HEMCO_ESMx: Initializing "//TRIM(name), &
                      ESMF_LOGMSG_INFO, rc=localrc)

    ! Get configuration file name
    CALL ESMF_ConfigGetAttribute(config, configFileName, &
                              label="HEMCO_CONFIG:", rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    ! Check for diagnostics file
    CALL ESMF_ConfigGetAttribute(config, diagFileName, &
                              label="HEMCO_DIAGN:", &
                              default="HEMCO_Diagn.rc", rc=localrc)

    ! Call the NUOPC initialization phases directly
    CALL InitializeP0(gridComp, importState, exportState, clock, rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) RETURN

    CALL InitializeP1(gridComp, importState, exportState, clock, rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) RETURN

    CALL InitializeP2(gridComp, importState, exportState, clock, rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) RETURN

    CALL InitializeP3(gridComp, importState, exportState, clock, rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) RETURN

    CALL InitializeP4(gridComp, importState, exportState, clock, rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) RETURN

    ! Log successful initialization
    CALL ESMF_LogWrite("HEMCO_ESMx: Successfully initialized "//TRIM(name), &
                      ESMF_LOGMSG_INFO, rc=localrc)

  END SUBROUTINE Initialize

  !-----------------------------------------------------------------------------
  !BOP
  !
  !IROUTINE: Run
  !
  ! !DESCRIPTION: Run the HEMCO ESMX component
  !\\
  !\\
  ! !INTERFACE:
  !
  SUBROUTINE Run(gridComp, importState, exportState, clock, rc)
    TYPE(ESMF_GridComp)  :: gridComp    ! Grid component
    TYPE(ESMF_State)     :: importState ! Import state
    TYPE(ESMF_State)     :: exportState ! Export state
    TYPE(ESMF_Clock)     :: clock       ! Clock
    INTEGER, INTENT(OUT) :: rc          ! Return code

    ! Local variables
    CHARACTER(LEN=ESMF_MAXSTR) :: name
    INTEGER :: localrc

    ! Begin...
    rc = ESMF_SUCCESS

    ! Get component name
    CALL ESMF_GridCompGet(gridComp, name=name, rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    ! Log start of run
    CALL ESMF_LogWrite("HEMCO_ESMx: Running "//TRIM(name), &
                      ESMF_LOGMSG_INFO, rc=localrc)

    ! Call the NUOPC run routine - this handles all the actual work
    CALL ModelAdvance(gridComp, rc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    ! Log end of run
    CALL ESMF_LogWrite("HEMCO_ESMx: Finished running "//TRIM(name), &
                      ESMF_LOGMSG_INFO, rc=localrc)

  END SUBROUTINE Run

  !-----------------------------------------------------------------------------
  !BOP
  !
  !IROUTINE: Finalize_ESMx
  !
  ! !DESCRIPTION: Finalize the HEMCO ESMX component
  !\\
  !\\
  ! !INTERFACE:
  !
  SUBROUTINE Finalize_ESMx(gridComp, importState, exportState, clock, rc)
    TYPE(ESMF_GridComp)  :: gridComp    ! Grid component
    TYPE(ESMF_State)     :: importState ! Import state
    TYPE(ESMF_State)     :: exportState ! Export state
    TYPE(ESMF_Clock)     :: clock       ! Clock
    INTEGER, INTENT(OUT) :: rc          ! Return code

    ! Local variables
    CHARACTER(LEN=ESMF_MAXSTR) :: name
    INTEGER :: localrc

    ! Begin...
    rc = ESMF_SUCCESS

    ! Get component name
    CALL ESMF_GridCompGet(gridComp, name=name, rc=localrc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    ! Log start of finalization
    CALL ESMF_LogWrite("HEMCO_ESMx: Finalizing "//TRIM(name), &
                      ESMF_LOGMSG_INFO, rc=localrc)

    ! Call the NUOPC finalize routine
    CALL Finalize(gridComp, importState, exportState, clock, rc)
    IF (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) RETURN

    ! Log successful finalization
    CALL ESMF_LogWrite("HEMCO_ESMx: Successfully finalized "//TRIM(name), &
                      ESMF_LOGMSG_INFO, rc=localrc)

  END SUBROUTINE Finalize_ESMx

END MODULE HCOI_ESMxGridCompMod
!EOC