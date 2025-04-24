!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: HCO_ESMxInterfaceMod.F90
!
! !DESCRIPTION: Module HCO\_ESMxInterfaceMod provides a high-level interface
! for using HEMCO within ESMX applications. It provides functions to create
! and initialize a HEMCO component that can be used in an ESMX application.
! This interface leverages the existing NUOPC functionality to minimize
! code duplication.
!\\
!\\
! !INTERFACE:
!
MODULE HCO_ESMxInterfaceMod
!
! !USES:
!
  USE ESMF
  USE NUOPC

  USE HCOI_ESMxGridCompMod, ONLY : SetServices
  USE HCO_Error_Mod,        ONLY : HCO_SUCCESS, HCO_FAIL
  USE HCO_Error_Mod,        ONLY : HCO_ERROR, HCO_WARNING

  IMPLICIT NONE
  PRIVATE

! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: HCO_ESMxInit
  PUBLIC :: HCO_ESMxCompCreate
  PUBLIC :: HCO_ESMxSetConfig

! !REVISION HISTORY:
!  22 Apr 2025 - Initial version for ESMX support
!  23 Apr 2025 - Updated to leverage NUOPC interface
!EOP
!------------------------------------------------------------------------------
!BOC
CONTAINS

  !-----------------------------------------------------------------------------
  !BOP
  !
  ! !ROUTINE: HCO_ESMxInit
  !
  ! !DESCRIPTION: Initialize the HEMCO ESMX interface
  !\\
  !\\
  ! !INTERFACE:
  !
  SUBROUTINE HCO_ESMxInit(rc)
    INTEGER, INTENT(OUT) :: rc ! Return code

    ! Begin
    rc = ESMF_SUCCESS

    ! Initialize ESMF
    CALL ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, &
                        defaultLogFileName="HCO_ESMx.log", &
                        logkindflag=ESMF_LOGKIND_MULTI, &
                        rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) RETURN

    ! Log successful initialization
    CALL ESMF_LogWrite("HEMCO ESMX Interface initialized", &
                      ESMF_LOGMSG_INFO, rc=rc)

  END SUBROUTINE HCO_ESMxInit

  !-----------------------------------------------------------------------------
  !BOP
  !
  ! !ROUTINE: HCO_ESMxCompCreate
  !
  ! !DESCRIPTION: Create a HEMCO component for ESMX
  !\\
  !\\
  ! !INTERFACE:
  !
  FUNCTION HCO_ESMxCompCreate(name, configFileName, petList, rc) RESULT(comp)
    CHARACTER(LEN=*),     INTENT(IN)  :: name           ! Component name
    CHARACTER(LEN=*),     INTENT(IN)  :: configFileName ! HEMCO configuration file
    INTEGER, OPTIONAL,    INTENT(IN)  :: petList(:)     ! List of PETs
    INTEGER,              INTENT(OUT) :: rc             ! Return code
    TYPE(ESMF_GridComp)              :: comp           ! HEMCO component

    ! Local variables
    TYPE(ESMF_Config) :: config
    CHARACTER(LEN=ESMF_MAXSTR) :: msgString

    ! Begin
    rc = ESMF_SUCCESS

    ! Create a new component
    comp = ESMF_GridCompCreate(name=name, petList=petList, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) RETURN

    ! Set the services
    CALL ESMF_GridCompSetServices(comp, SetServices, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) RETURN

    ! Set the HEMCO configuration file
    CALL HCO_ESMxSetConfig(comp, configFileName, rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) RETURN

    ! Log component creation
    WRITE(msgString, '(A,A,A)') "Created HEMCO ESMX component: ", TRIM(name)
    CALL ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

  END FUNCTION HCO_ESMxCompCreate

  !-----------------------------------------------------------------------------
  !BOP
  !
  ! !ROUTINE: HCO_ESMxSetConfig
  !
  ! !DESCRIPTION: Set configuration options for the HEMCO ESMX component
  !\\
  !\\
  ! !INTERFACE:
  !
  SUBROUTINE HCO_ESMxSetConfig(comp, configFileName, rc)
    TYPE(ESMF_GridComp)  :: comp           ! HEMCO component
    CHARACTER(LEN=*),     INTENT(IN)    :: configFileName ! Configuration file
    INTEGER,              INTENT(OUT)   :: rc             ! Return code

    ! Local variables
    TYPE(ESMF_Config) :: config

    ! Begin
    rc = ESMF_SUCCESS

    ! Get existing component config if any
    CALL ESMF_GridCompGet(comp, config=config, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) RETURN

    ! If no config exists, create one
    IF (.NOT. ESMF_ConfigIsCreated(config, rc=rc)) THEN
      config = ESMF_ConfigCreate(rc=rc)
      IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN
    ENDIF

    ! Set the configuration file
    CALL ESMF_ConfigSetAttribute(config, configFileName, label="HEMCO_CONFIG:", rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) RETURN

    ! Set the component type to ESMX
    CALL ESMF_ConfigSetAttribute(config, "ESMX", label="COMPONENT_TYPE:", rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) RETURN

    ! Set the config back in the component
    CALL ESMF_GridCompSet(comp, config=config, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) RETURN

  END SUBROUTINE HCO_ESMxSetConfig

END MODULE HCO_ESMxInterfaceMod
!EOC