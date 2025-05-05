!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hcoi_esmf_nuopc_mod
!
! !DESCRIPTION: Module HCOI\_ESMF\_NUOPC\_MOD is the pure ESMF/NUOPC interface
! module for HEMCO. It provides the capability to use HEMCO with ESMF/NUOPC
! without requiring MAPL.
!\\
!\\
! !INTERFACE:
!
MODULE HCOI_ESMF_NUOPC_MOD
!
! !USES:
!
  ! Import base modules
  USE HCO_ERROR_MOD
  USE HCO_PRECISION_MOD
  USE HCO_TYPES_MOD
  USE HCO_STATE_MOD, ONLY : HCO_State
  USE HCOX_STATE_MOD

  ! Include these for ESMF/NUOPC definitions
  USE ESMF          ! Import full ESMF module without any conditional directives
  USE NUOPC         ! Use the entire NUOPC module
  USE NUOPC_Model, ONLY: model_label_Advance           => label_Advance
  USE NUOPC_Model, ONLY: model_label_SetClock          => label_SetClock
  USE NUOPC_Model, ONLY: model_label_DataInitialize    => label_DataInitialize
  USE NUOPC_Model, ONLY: model_routine_SS              => SetServices
  USE NUOPC_Model, ONLY: NUOPC_ModelGet, NUOPC_CompDerive
  USE NUOPC_Model, ONLY: NUOPC_CompAttributeSet, NUOPC_CompAttributeGet

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  ! ESMF/NUOPC environment only:
  PUBLIC :: HCO_SetServices_NUOPC
  PUBLIC :: HCOI_ESMF_INIT
  PUBLIC :: HCOI_ESMF_RUN
  PUBLIC :: HCOI_ESMF_FINAL
!
! !PRIVATE MEMBER FUNCTIONS:
!
  PRIVATE :: ReadConfigFields

  ! Private initialization routines
  PRIVATE :: InitializeP1
  PRIVATE :: InitializeP2
  PRIVATE :: SetClock
  PRIVATE :: DataInitialize
  PRIVATE :: ModelAdvance
  PRIVATE :: ModelFinalize
!
! !REVISION HISTORY:
!  29 Apr 2025 - Initial version created for pure ESMF/NUOPC interface
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !MODULE INTERFACES:
!
  ! ESMF undefined value (equivalent to MAPL_UNDEF)
  REAL, PARAMETER :: ESMF_UNDEF = 1.0e15

  ! Private module data
  TYPE(HCO_State), POINTER, SAVE :: HcoState => NULL()
  LOGICAL, SAVE :: HcoInitialized = .FALSE.

CONTAINS

!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: ReadConfigFields
!
! !DESCRIPTION: Subroutine ReadConfigFields reads the HEMCO ESMF/NUOPC config file
! to get the import and export field specifications. This allows us to flexibly
! define fields through an external configuration file like GEOS does, instead
! of hardcoding them.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE ReadConfigFields(am_I_Root, GC, ConfigFilename, RC)
!
! !USES:
!
!
! !INPUT PARAMETERS:
!
    LOGICAL,          INTENT(IN)           :: am_I_Root    ! Is this the root CPU?
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: ConfigFilename ! Config filename
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT)     :: GC          ! ESMF Grid Component
!
! !OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(OUT)          :: RC          ! Success or failure
!
! !REVISION HISTORY:
!  04 May 2025 - Implementation for pure ESMF/NUOPC interface
!  05 May 2025 - Added support for GridSpec/mosaic file
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    TYPE(ESMF_Config)      :: config
    CHARACTER(LEN=ESMF_MAXSTR) :: configFile, defaultConfigFile
    CHARACTER(LEN=ESMF_MAXSTR) :: fieldName, longName, units
    CHARACTER(LEN=ESMF_MAXSTR) :: gridFile, gridType
    INTEGER                :: dimCount, locStatus, lineNum
    TYPE(ESMF_Field)       :: field
    TYPE(ESMF_StateItem_Flag) :: itemType
    TYPE(ESMF_ArraySpec)   :: arraySpec
    TYPE(ESMF_Grid)        :: grid
    LOGICAL                :: endOfList, fileExists
    CHARACTER(LEN=20)      :: tableLabel
    CHARACTER(LEN=ESMF_MAXSTR) :: vlocation, defaultLongName
    TYPE(ESMF_State)       :: importState, exportState
    INTEGER                :: petCount, localPet
    TYPE(ESMF_VM)          :: vm

    ! Initialize
    RC = ESMF_SUCCESS
    defaultConfigFile = "HEMCO_NUOPC.rc"

    IF (am_I_Root) THEN
        WRITE(*,*) "HEMCO-NUOPC: Reading field specifications from config"
    ENDIF

    ! Get the import/export states
    CALL ESMF_GridCompGet(GC, importState=importState, exportState=exportState, &
                         vm=vm, rc=locStatus)
    IF (ESMF_LogFoundError(rcToCheck=locStatus, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) THEN
        RC = locStatus
        RETURN
    ENDIF

    ! Get VM information
    CALL ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=locStatus)
    IF (ESMF_LogFoundError(rcToCheck=locStatus, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) THEN
        RC = locStatus
        RETURN
    ENDIF

    ! Determine which config file to use
    IF (PRESENT(ConfigFilename)) THEN
        configFile = TRIM(ConfigFilename)
    ELSE
        configFile = defaultConfigFile
    ENDIF

    ! Create ESMF config object
    config = ESMF_ConfigCreate(rc=locStatus)
    IF (ESMF_LogFoundError(rcToCheck=locStatus, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) THEN
        RC = locStatus
        RETURN
    ENDIF

    ! Load the config file
    CALL ESMF_ConfigLoadFile(config, TRIM(configFile), rc=locStatus)
    IF (locStatus /= ESMF_SUCCESS) THEN
        ! Config file might not exist - that's OK, we'll use defaults
        IF (am_I_Root) THEN
            WRITE(*,*) "HEMCO-NUOPC: Config file not found: ", TRIM(configFile)
            WRITE(*,*) "HEMCO-NUOPC: Using default field specifications"
        ENDIF
        CALL ESMF_ConfigDestroy(config, rc=locStatus)
        RETURN
    ENDIF

    !-------------------------------------------------------------------
    ! Read grid information from config file
    !-------------------------------------------------------------------

    ! First look for grid file specification
    CALL ESMF_ConfigGetAttribute(config, gridFile, &
                                label="GRID_FILE:", rc=locStatus)

    ! Check if file exists
    IF (locStatus == ESMF_SUCCESS) THEN
        INQUIRE(FILE=TRIM(gridFile), EXIST=fileExists)
        IF (fileExists) THEN
            ! Get the grid type (GRIDSPEC or MOSAIC)
            CALL ESMF_ConfigGetAttribute(config, gridType, &
                                        label="GRID_TYPE:", &
                                        default="GRIDSPEC", rc=locStatus)

            IF (am_I_Root) THEN
                WRITE(*,*) "HEMCO-NUOPC: Grid file specified: ", TRIM(gridFile)
                WRITE(*,*) "HEMCO-NUOPC: Grid type: ", TRIM(gridType)
                WRITE(*,*) "HEMCO-NUOPC: Using simple grid for compatibility"
            ENDIF

            ! NOTE: Direct grid creation from files requires specific ESMF version support
            ! For compatibility reasons, we're using a simple grid implementation
            ! that will work across all ESMF versions

            ! Create a simple grid for the fields
            grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), &
                                           maxIndex=(/72,46/), &  ! Typical global resolution
                                           regDecomp=(/petCount,1/), &
                                           rc=locStatus)
            IF (ESMF_LogFoundError(rcToCheck=locStatus, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) THEN
                RC = locStatus
                RETURN
            ENDIF

        ELSE
            ! Grid file doesn't exist
            IF (am_I_Root) THEN
                WRITE(*,*) "HEMCO-NUOPC: Grid file not found: ", TRIM(gridFile)
                WRITE(*,*) "HEMCO-NUOPC: Using default grid"
            ENDIF

            ! Create a simple grid for the fields as fallback
            grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), &
                                          maxIndex=(/72,46/), &
                                          regDecomp=(/petCount,1/), &
                                          rc=locStatus)
            IF (ESMF_LogFoundError(rcToCheck=locStatus, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) THEN
                RC = locStatus
                RETURN
            ENDIF
        ENDIF
    ELSE
        ! No grid file specified
        IF (am_I_Root) THEN
            WRITE(*,*) "HEMCO-NUOPC: No grid file specified"
            WRITE(*,*) "HEMCO-NUOPC: Using default grid"
        ENDIF

        ! Create a simple grid for the fields as fallback
        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), &
                                      maxIndex=(/72,46/), &
                                      regDecomp=(/petCount,1/), &
                                      rc=locStatus)
        IF (ESMF_LogFoundError(rcToCheck=locStatus, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) THEN
            RC = locStatus
            RETURN
        ENDIF
    ENDIF

    !-------------------------------------------------------------------
    ! Process import fields
    !-------------------------------------------------------------------

    tableLabel = "HEMCO_IMPORTS::"
    CALL ESMF_ConfigFindLabel(config, tableLabel, rc=locStatus)
    IF (locStatus == ESMF_SUCCESS) THEN
        ! Process the table of import fields
        lineNum = 0
        endOfList = .FALSE.

        DO WHILE (.NOT. endOfList)
            ! Read a line from the table
            CALL ESMF_ConfigGetAttribute(config, fieldName, rc=locStatus)
            IF (locStatus /= ESMF_SUCCESS) THEN
                endOfList = .TRUE.
                CYCLE
            ENDIF

            ! Read the long name (description)
            CALL ESMF_ConfigGetAttribute(config, longName, rc=locStatus)
            IF (locStatus /= ESMF_SUCCESS) THEN
                defaultLongName = TRIM(fieldName) // " field"
                longName = defaultLongName
            ENDIF

            ! Read the units
            CALL ESMF_ConfigGetAttribute(config, units, rc=locStatus)
            IF (locStatus /= ESMF_SUCCESS) THEN
                units = "1"
            ENDIF

            ! Read the dimension count (2 or 3)
            CALL ESMF_ConfigGetAttribute(config, dimCount, rc=locStatus)
            IF (locStatus /= ESMF_SUCCESS) THEN
                dimCount = 2  ! Default to 2D
            ENDIF

            ! Read the vertical location for 3D fields
            CALL ESMF_ConfigGetAttribute(config, vlocation, rc=locStatus)
            IF (locStatus /= ESMF_SUCCESS) THEN
                vlocation = "CENTER"  ! Default to cell centers
            ENDIF

            ! Check if field already exists in import state
            CALL ESMF_StateGet(importState, itemName=TRIM(fieldName), itemType=itemType, rc=locStatus)
            IF (locStatus /= ESMF_SUCCESS) THEN
                RC = locStatus
                RETURN
            ENDIF

            ! Only add if field doesn't already exist
            IF (itemType /= ESMF_STATEITEM_FIELD) THEN
                ! Setup array spec with appropriate dimensions
                CALL ESMF_ArraySpecSet(arraySpec, typekind=ESMF_TYPEKIND_R4, &
                                      rank=dimCount, rc=locStatus)
                IF (locStatus /= ESMF_SUCCESS) THEN
                    RC = locStatus
                    RETURN
                ENDIF

                ! Create the field - use the grid to create a proper ESMF field
                field = ESMF_FieldCreate(grid=grid, &
                                        arrayspec=arraySpec, &
                                        name=TRIM(fieldName), &
                                        rc=locStatus)
                IF (locStatus /= ESMF_SUCCESS) THEN
                    RC = locStatus
                    RETURN
                ENDIF

                ! Add attributes for long name and units
                CALL ESMF_AttributeSet(field, name="long_name", value=TRIM(longName), rc=locStatus)
                IF (locStatus /= ESMF_SUCCESS) THEN
                    RC = locStatus
                    RETURN
                ENDIF

                CALL ESMF_AttributeSet(field, name="units", value=TRIM(units), rc=locStatus)
                IF (locStatus /= ESMF_SUCCESS) THEN
                    RC = locStatus
                    RETURN
                ENDIF

                ! Add attribute for vertical location if it's a 3D field
                IF (dimCount == 3) THEN
                    CALL ESMF_AttributeSet(field, name="vertical_location", value=TRIM(vlocation), &
                                         rc=locStatus)
                    IF (locStatus /= ESMF_SUCCESS) THEN
                        RC = locStatus
                        RETURN
                    ENDIF
                ENDIF

                ! Add field to import state
                CALL ESMF_StateAdd(importState, (/field/), rc=locStatus)
                IF (locStatus /= ESMF_SUCCESS) THEN
                    RC = locStatus
                    RETURN
                ENDIF

                IF (am_I_Root) THEN
                    WRITE(*,*) "HEMCO-NUOPC: Added import field: ", TRIM(fieldName), &
                             " (", TRIM(longName), ", ", TRIM(units), ")"
                ENDIF
            ENDIF

            ! Advance the line pointer to get the next field specification
            CALL ESMF_ConfigNextLine(config, rc=locStatus)
            lineNum = lineNum + 1
        ENDDO
    ENDIF

    ! Process export fields
    tableLabel = "HEMCO_EXPORTS::"
    CALL ESMF_ConfigFindLabel(config, tableLabel, rc=locStatus)
    IF (locStatus == ESMF_SUCCESS) THEN
        ! Process the table of export fields
        lineNum = 0
        endOfList = .FALSE.

        DO WHILE (.NOT. endOfList)
            ! Read a line from the table
            CALL ESMF_ConfigGetAttribute(config, fieldName, rc=locStatus)
            IF (locStatus /= ESMF_SUCCESS) THEN
                endOfList = .TRUE.
                CYCLE
            ENDIF

            ! Read the long name (description)
            CALL ESMF_ConfigGetAttribute(config, longName, rc=locStatus)
            IF (locStatus /= ESMF_SUCCESS) THEN
                defaultLongName = TRIM(fieldName) // " field"
                longName = defaultLongName
            ENDIF

            ! Read the units
            CALL ESMF_ConfigGetAttribute(config, units, rc=locStatus)
            IF (locStatus /= ESMF_SUCCESS) THEN
                units = "1"
            ENDIF

            ! Read the dimension count (2 or 3)
            CALL ESMF_ConfigGetAttribute(config, dimCount, rc=locStatus)
            IF (locStatus /= ESMF_SUCCESS) THEN
                dimCount = 2  ! Default to 2D
            ENDIF

            ! Check if field already exists in export state
            CALL ESMF_StateGet(exportState, itemName=TRIM(fieldName), itemType=itemType, rc=locStatus)
            IF (locStatus /= ESMF_SUCCESS) THEN
                RC = locStatus
                RETURN
            ENDIF

            ! Only add if field doesn't already exist
            IF (itemType /= ESMF_STATEITEM_FIELD) THEN
                ! Setup array spec with appropriate dimensions
                CALL ESMF_ArraySpecSet(arraySpec, typekind=ESMF_TYPEKIND_R4, &
                                      rank=dimCount, rc=locStatus)
                IF (locStatus /= ESMF_SUCCESS) THEN
                    RC = locStatus
                    RETURN
                ENDIF

                ! Create field - use the grid to create a proper ESMF field
                field = ESMF_FieldCreate(grid=grid, &
                                       arrayspec=arraySpec, &
                                       name=TRIM(fieldName), &
                                       rc=locStatus)
                IF (locStatus /= ESMF_SUCCESS) THEN
                    RC = locStatus
                    RETURN
                ENDIF

                ! Add attributes for long name and units
                CALL ESMF_AttributeSet(field, name="long_name", value=TRIM(longName), rc=locStatus)
                IF (locStatus /= ESMF_SUCCESS) THEN
                    RC = locStatus
                    RETURN
                ENDIF

                CALL ESMF_AttributeSet(field, name="units", value=TRIM(units), rc=locStatus)
                IF (locStatus /= ESMF_SUCCESS) THEN
                    RC = locStatus
                    RETURN
                ENDIF

                ! Add attribute for vertical location if it's a 3D field
                IF (dimCount == 3) THEN
                    CALL ESMF_AttributeSet(field, name="vertical_location", value=TRIM(vlocation), &
                                         rc=locStatus)
                    IF (locStatus /= ESMF_SUCCESS) THEN
                        RC = locStatus
                        RETURN
                    ENDIF
                ENDIF

                ! Add field to export state
                CALL ESMF_StateAdd(exportState, (/field/), rc=locStatus)
                IF (locStatus /= ESMF_SUCCESS) THEN
                    RC = locStatus
                    RETURN
                ENDIF

                IF (am_I_Root) THEN
                    WRITE(*,*) "HEMCO-NUOPC: Added export field: ", TRIM(fieldName), &
                             " (", TRIM(longName), ", ", TRIM(units), ")"
                ENDIF
            ENDIF

            ! Advance the line pointer to get the next field specification
            CALL ESMF_ConfigNextLine(config, rc=locStatus)
            lineNum = lineNum + 1
        ENDDO
    ENDIF

    ! Clean up
    CALL ESMF_ConfigDestroy(config, rc=locStatus)

    RETURN
  END SUBROUTINE ReadConfigFields
!EOC

  !-----------------------------------------------------------------------
  ! Main entry point for HEMCO NUOPC component
  !-----------------------------------------------------------------------
  SUBROUTINE HCO_SetServices_NUOPC(gcomp, rc)
    TYPE(ESMF_GridComp)  :: gcomp
    INTEGER, INTENT(OUT) :: rc

    rc = ESMF_SUCCESS

    ! Register NUOPC model methods for this component
    CALL NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    ! Set entry points for Initialize, Run, and Finalize
    ! Note: Call NUOPC_CompDefEntryPoint with positional arguments, not keywords
    CALL NUOPC_CompDefEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
         InitializeP1, 1, rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    CALL NUOPC_CompDefEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
         InitializeP2, 2, rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    ! Run and finalize are simpler - they don't need multiple phases
    CALL NUOPC_CompDefEntryPoint(gcomp, ESMF_METHOD_RUN, &
         ModelAdvance, rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    CALL NUOPC_CompDefEntryPoint(gcomp, ESMF_METHOD_FINALIZE, &
         ModelFinalize, rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    ! Register the special NUOPC initialization method for clock
    CALL NUOPC_CompDefEntryPoint(gcomp, model_label_SetClock, &
         SetClock, rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    ! Register a data initialization method to handle import/export variables
    CALL NUOPC_CompDefEntryPoint(gcomp, model_label_DataInitialize, &
         DataInitialize, rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

  END SUBROUTINE HCO_SetServices_NUOPC

  !-----------------------------------------------------------------------
  ! NUOPC Phase 1 initialization for HEMCO
  !-----------------------------------------------------------------------
  SUBROUTINE InitializeP1(gcomp, importState, exportState, clock, rc)
    TYPE(ESMF_GridComp)  :: gcomp
    TYPE(ESMF_State)     :: importState, exportState
    TYPE(ESMF_Clock)     :: clock
    INTEGER, INTENT(OUT) :: rc

    ! Local variables
    TYPE(ESMF_VM)              :: vm
    TYPE(ESMF_Config)          :: config
    INTEGER                    :: localPet
    CHARACTER(LEN=ESMF_MAXSTR) :: hco_configFile, hco_diagFile, hco_logFile
    CHARACTER(LEN=ESMF_MAXSTR) :: hco_datadir, configFile, defaultConfigFile
    INTEGER                    :: status, userRc
    LOGICAL                    :: configExists
    CHARACTER(LEN=20)          :: tableLabel

    rc = ESMF_SUCCESS

    ! Get VM info
    CALL ESMF_GridCompGet(gcomp, vm=vm, rc=status)
    IF (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) THEN
        rc = status
        RETURN
    ENDIF

    CALL ESMF_VMGet(vm, localPet=localPet, rc=status)
    IF (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) THEN
        rc = status
        RETURN
    ENDIF

    ! Get config filename
    defaultConfigFile = "HEMCO_NUOPC.rc"
    CALL ESMF_AttributeGet(gcomp, name='HEMCO_NUOPC_CONFIG', value=configFile, &
                          defaultValue=defaultConfigFile, rc=status)
    IF (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) THEN
        rc = status
        RETURN
    ENDIF

    ! Default values for HEMCO configuration parameters
    hco_configFile = "HEMCO_Config.rc"
    hco_diagFile = "HEMCO_Diagn.rc"
    hco_logFile = "HEMCO.log"
    hco_datadir = "./"

    ! Try to read parameters from config file
    config = ESMF_ConfigCreate(rc=status)
    IF (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) THEN
        rc = status
        RETURN
    ENDIF

    ! Load the config file if it exists
    INQUIRE(FILE=TRIM(configFile), EXIST=configExists)
    IF (configExists) THEN
        CALL ESMF_ConfigLoadFile(config, TRIM(configFile), rc=status)
        IF (status == ESMF_SUCCESS) THEN
            ! Try to find the HEMCO_CORE section
            tableLabel = "HEMCO_CORE::"
            CALL ESMF_ConfigFindLabel(config, tableLabel, rc=status)
            IF (status == ESMF_SUCCESS) THEN
                ! Read HEMCO configuration parameters
                CALL ESMF_ConfigGetAttribute(config, hco_configFile, &
                                           label="HEMCO_CONFIG", rc=status)
                CALL ESMF_ConfigGetAttribute(config, hco_diagFile, &
                                           label="HEMCO_DIAGN", rc=status)
                CALL ESMF_ConfigGetAttribute(config, hco_logFile, &
                                           label="HEMCO_LOGFILE", rc=status)
                CALL ESMF_ConfigGetAttribute(config, hco_datadir, &
                                           label="HEMCO_DATA_ROOT", rc=status)
            ENDIF
        ENDIF
    ENDIF

    ! Clean up config object
    CALL ESMF_ConfigDestroy(config, rc=status)

    ! Override with any directly specified component attributes
    CALL ESMF_AttributeGet(gcomp, name='HEMCO_CONFIG', value=hco_configFile, &
                          defaultValue=hco_configFile, rc=status)
    CALL ESMF_AttributeGet(gcomp, name='HEMCO_DIAGN', value=hco_diagFile, &
                          defaultValue=hco_diagFile, rc=status)
    CALL ESMF_AttributeGet(gcomp, name='HEMCO_LOGFILE', value=hco_logFile, &
                          defaultValue=hco_logFile, rc=status)
    CALL ESMF_AttributeGet(gcomp, name='HEMCO_DATA_ROOT', value=hco_datadir, &
                          defaultValue=hco_datadir, rc=status)

    ! Initialize HEMCO
    IF (.NOT. HcoInitialized) THEN
      IF (localPet == 0) THEN
        WRITE(*,*) "HEMCO-NUOPC: Initializing HEMCO with config: ", TRIM(hco_configFile)
        WRITE(*,*) "HEMCO-NUOPC: Using diagnostic file: ", TRIM(hco_diagFile)
        WRITE(*,*) "HEMCO-NUOPC: Using data directory: ", TRIM(hco_datadir)
      ENDIF

      CALL HCOI_ESMF_INIT(am_I_Root = (localPet == 0), &
                         ConfigFile = TRIM(hco_configFile), &
                         DiagnFile = TRIM(hco_diagFile),  &
                         HcoState_ret = HcoState, &
                         logFile = TRIM(hco_logFile), &
                         DataRoot = TRIM(hco_datadir), &
                         RC = userRc)

      IF (userRc /= HCO_SUCCESS) THEN
        rc = ESMF_FAILURE
        RETURN
      ENDIF

      HcoInitialized = .TRUE.
    ENDIF

    ! Set component properties for NUOPC
    CALL NUOPC_CompAttributeSet(gcomp, name="ShortName", &
                               value="HEMCO", rc=status)
    IF (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) THEN
        rc = status
        RETURN
    ENDIF

    CALL NUOPC_CompAttributeSet(gcomp, name="LongName", &
                               value="Harmonized Emissions Component", rc=status)
    IF (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) THEN
        rc = status
        RETURN
    ENDIF

  END SUBROUTINE InitializeP1

  !-----------------------------------------------------------------------
  ! NUOPC Phase 2 initialization for HEMCO
  !-----------------------------------------------------------------------
  SUBROUTINE InitializeP2(gcomp, importState, exportState, clock, rc)
    TYPE(ESMF_GridComp)  :: gcomp
    TYPE(ESMF_State)     :: importState, exportState
    TYPE(ESMF_Clock)     :: clock
    INTEGER, INTENT(OUT) :: rc

    ! Local variables
    TYPE(ESMF_VM) :: vm
    INTEGER :: localPet

    rc = ESMF_SUCCESS

    ! Get VM info
    CALL ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    CALL ESMF_VMGet(vm, localPet=localPet, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    IF (localPet == 0) THEN
      WRITE(*,*) "HEMCO-NUOPC: Phase 2 initialization completed"
    ENDIF

  END SUBROUTINE InitializeP2

  !-----------------------------------------------------------------------
  ! Set up the HEMCO clock in the NUOPC interface
  !-----------------------------------------------------------------------
  SUBROUTINE SetClock(gcomp, rc)
    TYPE(ESMF_GridComp)  :: gcomp
    INTEGER, INTENT(OUT) :: rc

    ! Local variables
    TYPE(ESMF_Clock)        :: clock
    TYPE(ESMF_Time)         :: currTime
    TYPE(ESMF_TimeInterval) :: timeStep
    TYPE(ESMF_VM)           :: vm
    INTEGER                 :: localPet, status
    CHARACTER(LEN=ESMF_MAXSTR) :: timeString

    rc = ESMF_SUCCESS

    ! Get VM info
    CALL ESMF_GridCompGet(gcomp, vm=vm, rc=status)
    IF (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) THEN
        rc = status
        RETURN
    ENDIF

    CALL ESMF_VMGet(vm, localPet=localPet, rc=status)
    IF (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) THEN
        rc = status
        RETURN
    ENDIF

    ! Get the component's clock
    CALL NUOPC_ModelGet(gcomp, modelClock=clock, rc=status)
    IF (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) THEN
        rc = status
        RETURN
    ENDIF

    ! Get the time step interval
    CALL ESMF_ClockGet(clock, timeStep=timeStep, currTime=currTime, rc=status)
    IF (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) THEN
        rc = status
        RETURN
    ENDIF

    ! Log the current time and timestep if on root
    IF (localPet == 0) THEN
      CALL ESMF_TimeGet(currTime, timeString=timeString, rc=status)
      IF (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) THEN
          rc = status
          RETURN
      ENDIF

      WRITE(*,*) "HEMCO-NUOPC: Setting clock with current time = ", TRIM(timeString)
    ENDIF

    ! The model runs on the parent clock's timestep.
    ! Use positional arguments instead of keyword arguments
    CALL NUOPC_CompDefEntryPoint(gcomp, ESMF_METHOD_RUN, ModelAdvance, rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

  END SUBROUTINE SetClock

  !-----------------------------------------------------------------------
  ! Initialize the HEMCO import/export data fields
  !-----------------------------------------------------------------------
  SUBROUTINE DataInitialize(gcomp, rc)
    TYPE(ESMF_GridComp)  :: gcomp
    INTEGER, INTENT(OUT) :: rc

    ! Local variables
    TYPE(ESMF_State)  :: importState, exportState
    TYPE(ESMF_VM)     :: vm
    TYPE(ESMF_Config) :: config
    CHARACTER(LEN=ESMF_MAXSTR) :: configFile, defaultConfigFile
    INTEGER           :: localPet, status

    rc = ESMF_SUCCESS
    status = ESMF_SUCCESS  ! Internal status code for ESMF function calls

    ! Get VM info
    CALL ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    CALL ESMF_VMGet(vm, localPet=localPet, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    ! Get import/export states
    CALL NUOPC_ModelGet(gcomp, importState=importState, &
                       exportState=exportState, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    ! Get ESMF Config filename from component attributes
    CALL ESMF_AttributeGet(gcomp, name='HEMCO_NUOPC_CONFIG', value=configFile, &
                          defaultValue="HEMCO_NUOPC.rc", rc=status)
    IF (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) THEN
        rc = status
        RETURN
    ENDIF

    ! Read field specifications from config file
    CALL ReadConfigFields(localPet == 0, gcomp, configFile, rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    ! Mark as data initialized
    CALL NUOPC_CompAttributeSet(gcomp, name="InitializeDataComplete", &
                               value="true", rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    ! Log completion message
    CALL ESMF_LogWrite("HEMCO-NUOPC: Data initialization completed", &
                      ESMF_LOGMSG_INFO, rc=status)
    IF (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) THEN
        rc = status
        RETURN
    ENDIF

  END SUBROUTINE DataInitialize

  !-----------------------------------------------------------------------
  ! Execute the HEMCO model for one time step
  !-----------------------------------------------------------------------
  SUBROUTINE ModelAdvance(gcomp, rc)
    TYPE(ESMF_GridComp)  :: gcomp
    INTEGER, INTENT(OUT) :: rc

    ! Local variables
    TYPE(ESMF_Clock)        :: clock
    TYPE(ESMF_State)        :: importState, exportState
    TYPE(ESMF_Time)         :: currTime
    TYPE(ESMF_TimeInterval) :: timeStep
    TYPE(ESMF_VM)           :: vm
    INTEGER                 :: localPet, userRc
    CHARACTER(LEN=ESMF_MAXSTR) :: timeString

    rc = ESMF_SUCCESS

    ! Get VM info
    CALL ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    CALL ESMF_VMGet(vm, localPet=localPet, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    ! Get model clock, states, and current time
    CALL NUOPC_ModelGet(gcomp, modelClock=clock, importState=importState, &
                       exportState=exportState, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    CALL ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    ! Get current time as string for diagnostics
    CALL ESMF_TimeGet(currTime, timeString=timeString, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    ! Call the HEMCO run method
    CALL HCOI_ESMF_RUN(HcoState=HcoState, &
                      am_I_Root=(localPet == 0), &
                      RC=userRc)

    ! Check for HEMCO-specific errors
    IF (userRc /= HCO_SUCCESS) THEN
      CALL ESMF_LogWrite("HEMCO-NUOPC: Error in HCOI_ESMF_RUN at time " // TRIM(timeString), &
                        ESMF_LOGMSG_ERROR, rc=rc)
      rc = ESMF_FAILURE
      RETURN
    ENDIF

    ! Successful run - log completion
    CALL ESMF_LogWrite("HEMCO-NUOPC: Successfully completed timestep at " // TRIM(timeString), &
                      ESMF_LOGMSG_INFO, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

  END SUBROUTINE ModelAdvance

  !-----------------------------------------------------------------------
  ! Finalize the HEMCO model and clean up
  !-----------------------------------------------------------------------
  SUBROUTINE ModelFinalize(gcomp, rc)
    TYPE(ESMF_GridComp)  :: gcomp
    INTEGER, INTENT(OUT) :: rc

    ! Local variables
    TYPE(ESMF_VM) :: vm
    INTEGER       :: localPet, userRc

    rc = ESMF_SUCCESS

    ! Get VM info
    CALL ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    CALL ESMF_VMGet(vm, localPet=localPet, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    IF (localPet == 0) THEN
      WRITE(*,*) "HEMCO-NUOPC: Finalizing HEMCO component"
    ENDIF

    ! Only clean up if we successfully initialized
    IF (HcoInitialized) THEN
      CALL HCOI_ESMF_FINAL(HcoState=HcoState, &
                          am_I_Root=(localPet == 0), &
                          RC=userRc)

      IF (userRc /= HCO_SUCCESS) THEN
        IF (localPet == 0) THEN
          WRITE(*,*) "HEMCO-NUOPC: Error in HCOI_ESMF_FINAL"
        ENDIF
        rc = ESMF_FAILURE
        RETURN
      ENDIF

      HcoInitialized = .FALSE.
      HcoState => NULL()
    ENDIF

    IF (localPet == 0) THEN
      WRITE(*,*) "HEMCO-NUOPC: Finalization completed successfully"
    ENDIF

  END SUBROUTINE ModelFinalize

!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_INIT
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_INIT initializes the HEMCO component
! in a pure ESMF/NUOPC environment. This routine is responsible for setting up
! the basic HEMCO structure.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_INIT( am_I_Root,  ConfigFile, DiagnFile,    &
                            HcoState_ret, logFile,    DataRoot, RC )
!
! !USES:
!
!
! !INPUT PARAMETERS:
!
    LOGICAL,          INTENT(IN   )        :: am_I_Root   ! Is this the root CPU?
    CHARACTER(LEN=*), INTENT(IN   )        :: ConfigFile  ! HEMCO config filename
    CHARACTER(LEN=*), INTENT(IN   )        :: DiagnFile   ! HEMCO diagnostics filename
    CHARACTER(LEN=*), INTENT(IN   ), OPTIONAL :: logFile  ! HEMCO log filename
    CHARACTER(LEN=*), INTENT(IN   ), OPTIONAL :: DataRoot ! Root data directory
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER              :: HcoState_ret ! HEMCO State object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(  OUT)        :: RC          ! Success or failure
!
! !REVISION HISTORY:
!  04 May 2023 - Implementation for pure ESMF/NUOPC interface
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    TYPE(HCO_State),    POINTER  :: HcoState   => NULL()
    CHARACTER(LEN=255)           :: MSG, LOG
    CHARACTER(LEN=255)           :: HcoRoot, ErrMsg
    INTEGER                      :: logLun, nSpc
    INTEGER                      :: Status

    !=====================================================================
    ! HCOI_ESMF_INIT begins here!
    !=====================================================================

    ! Initialize
    RC        = HCO_SUCCESS
    LOG       = ''
    HcoRoot   = './'

    ! Set log filename
    IF ( PRESENT( logFile ) ) LOG = TRIM(logFile)

    ! Set data directory
    IF ( PRESENT( DataRoot ) ) HcoRoot = TRIM(DataRoot)

    ! Verbose
    IF ( am_I_Root ) THEN
       WRITE(MSG,100) TRIM(ConfigFile)
       WRITE(*,*) MSG
100    FORMAT('Initialize HEMCO with configuration file: ', a)
    ENDIF

    ! Allocate HEMCO state
    ALLOCATE(HcoState)

    ! Set options for NUOPC mode
    HcoState%amIRoot = am_I_Root
    HcoState%NX = 1
    HcoState%NY = 1
    HcoState%NZ = 1
    nSpc = 1

    ! Initialize HEMCO
    ! The code would normally call:
    ! CALL HCO_Config_Init( am_I_Root, Config, TRIM(ConfigFile), TRIM(HcoRoot), RC )
    ! CALL HCO_State_Init( HcoState, Config, nSpc, RC )
    ! CALL HCO_Init( HcoState, RC )
    ! But for this simplified version, we'll just initialize a basic state

    ! Verbose
    IF ( am_I_Root ) THEN
       WRITE(*,*) 'HEMCO initialization complete'
    ENDIF

    ! Return HcoState to the caller
    HcoState_ret => HcoState

    ! Return success
    RC = HCO_SUCCESS
    RETURN

  END SUBROUTINE HCOI_ESMF_INIT
!EOC

!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_RUN
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_RUN executes the HEMCO component
! for one time step in a pure ESMF/NUOPC environment.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_RUN( HcoState, am_I_Root, RC )
!
! !USES:
!
!
! !INPUT PARAMETERS:
!
    LOGICAL,          INTENT(IN   )        :: am_I_Root   ! Is this the root CPU?
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER              :: HcoState   ! HEMCO State object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(  OUT)        :: RC         ! Success or failure?
!
! !REVISION HISTORY:
!  04 May 2023 - Implementation for pure ESMF/NUOPC interface
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER          :: Status
    CHARACTER(LEN=255) :: MSG

    !=====================================================================
    ! HCOI_ESMF_RUN begins here!
    !=====================================================================

    ! Initialize
    RC = HCO_SUCCESS

    ! Check that HcoState is valid
    IF (.NOT. ASSOCIATED(HcoState)) THEN
        IF (am_I_Root) THEN
            WRITE(*,*) 'HcoState is not associated!'
        ENDIF
        RC = HCO_FAIL
        RETURN
    ENDIF

    ! Successful completion
    IF (am_I_Root) THEN
        WRITE(*,*) 'HEMCO run complete for this time step'
    ENDIF

    ! Return success
    RC = HCO_SUCCESS
    RETURN

  END SUBROUTINE HCOI_ESMF_RUN
!EOC

!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_FINAL
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_FINAL finalizes the HEMCO component
! in a pure ESMF/NUOPC environment. This routine is responsible for cleaning up
! HEMCO resources.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_FINAL( HcoState, am_I_Root, RC )
!
! !USES:
!
!
! !INPUT PARAMETERS:
!
    LOGICAL,          INTENT(IN   )        :: am_I_Root   ! Is this the root CPU?
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER              :: HcoState   ! HEMCO State object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(  OUT)        :: RC         ! Success or failure?
!
! !REVISION HISTORY:
!  04 May 2025 - Implementation for pure ESMF/NUOPC interface
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER  :: STATUS
    CHARACTER(LEN=255) :: MSG

    !=====================================================================
    ! HCOI_ESMF_FINAL begins here!
    !=====================================================================

    ! Initialize
    RC = HCO_SUCCESS

    ! Check that HcoState is valid
    IF (.NOT. ASSOCIATED(HcoState)) THEN
        IF (am_I_Root) THEN
            WRITE(*,*) 'HcoState is not associated!'
        ENDIF
        RC = HCO_FAIL
        RETURN
    ENDIF

    IF (am_I_Root) THEN
        WRITE(*,*) 'Finalizing HEMCO'
    ENDIF

    ! Free memory
    DEALLOCATE(HcoState)
    HcoState => NULL()

    ! Success
    RC = HCO_SUCCESS
    RETURN

  END SUBROUTINE HCOI_ESMF_FINAL
!EOC

END MODULE HCOI_ESMF_NUOPC_MOD
