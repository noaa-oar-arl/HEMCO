!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hco_esmf_regrid_mod.F90
!
! !DESCRIPTION: Module HCO\_ESMF\_REGRID\_MOD contains routines to handle
!  ESMF regridding operations for the NUOPC interface. This module provides
!  utility functions to perform ESMF regridding using specifications from
!  the FileData structure that were parsed from the HEMCO configuration file.
!\\
!\\
! !INTERFACE:
!
MODULE HCO_ESMF_REGRID_MOD
!
! !USES:
!
#if defined ( ESMF_ ) && (!defined ( MAPL_ESMF ))
  USE HCO_ERROR_MOD
  USE HCO_PRECISION_MOD
  USE HCO_TYPES_MOD, ONLY : HcoOpt, ConfigObj
  USE ESMF
#endif

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC TYPES:
!
#if defined ( ESMF_ ) && (!defined ( MAPL_ESMF ))
  TYPE :: RegridSpec_Type
    CHARACTER(LEN=255) :: ContainerName = ''
    CHARACTER(LEN=63)  :: RegridMethod = ''
    CHARACTER(LEN=255) :: WeightFile = ''
    CHARACTER(LEN=255) :: Options = ''
    LOGICAL :: IsActive = .FALSE.
  END TYPE RegridSpec_Type
#endif

!
! !PUBLIC MEMBER FUNCTIONS:
!
#if defined ( ESMF_ ) && (!defined ( MAPL_ESMF ))
  PUBLIC :: RegridSpec_Type
  PUBLIC :: HCO_ESMF_GetRegridSpec
  PUBLIC :: HCO_ESMF_GetRegridMethod
  PUBLIC :: HCO_ESMF_PerformRegrid
  PUBLIC :: HCO_ESMF_CreateWeights
  PUBLIC :: HCO_ESMF_ParseOptions
#endif

!
! !REVISION HISTORY:
!  01 Jul 2025 - Simplified version for ESMF regridding operations
!EOP
!------------------------------------------------------------------------------
!BOC
CONTAINS
!EOC

#if defined ( ESMF_ ) && (!defined ( MAPL_ESMF ))

!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCO_ESMF_GetRegridMethod
!
! !DESCRIPTION: Convert regridding method string to ESMF flag.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCO_ESMF_GetRegridMethod(MethodStr, ESMFMethod, RC)
!
! !INPUT PARAMETERS:
!
    CHARACTER(LEN=*), INTENT(IN) :: MethodStr      ! Method string
!
! !OUTPUT PARAMETERS:
!
    TYPE(ESMF_RegridMethod_Flag), INTENT(OUT) :: ESMFMethod ! ESMF method
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER, INTENT(INOUT) :: RC                   ! Return code
!
! !REVISION HISTORY:
!  01 Jul 2025 - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
    !=================================================================
    ! HCO_ESMF_GetRegridMethod begins here
    !=================================================================
    RC = HCO_SUCCESS

    SELECT CASE (TRIM(MethodStr))
        CASE ('CONSERVE')
            ESMFMethod = ESMF_REGRIDMETHOD_CONSERVE
        CASE ('BILINEAR')
            ESMFMethod = ESMF_REGRIDMETHOD_BILINEAR
        CASE ('PATCH')
            ESMFMethod = ESMF_REGRIDMETHOD_PATCH
        CASE ('NEAREST_STOD')
            ESMFMethod = ESMF_REGRIDMETHOD_NEAREST_STOD
        CASE ('NEAREST_DTOS')
            ESMFMethod = ESMF_REGRIDMETHOD_NEAREST_DTOS
        CASE ('-', '')
            ! Use default conservative method
            ESMFMethod = ESMF_REGRIDMETHOD_CONSERVE
        CASE DEFAULT
            RC = HCO_FAIL
    END SELECT

  END SUBROUTINE HCO_ESMF_GetRegridMethod
!EOC

!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCO_ESMF_ParseOptions
!
! !DESCRIPTION: Parse regridding options string into ESMF flags.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCO_ESMF_ParseOptions(OptionsStr, UnmappedAction, PoleMethod, &
                                   LineType, NormType, RC)
!
! !INPUT PARAMETERS:
!
    CHARACTER(LEN=*), INTENT(IN) :: OptionsStr     ! Options string
!
! !OUTPUT PARAMETERS:
!
    TYPE(ESMF_UnmappedAction_Flag), INTENT(OUT) :: UnmappedAction
    TYPE(ESMF_PoleMethod_Flag), INTENT(OUT) :: PoleMethod
    TYPE(ESMF_LineType_Flag), INTENT(OUT) :: LineType
    TYPE(ESMF_NormType_Flag), INTENT(OUT) :: NormType
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER, INTENT(INOUT) :: RC                   ! Return code
!
! !REVISION HISTORY:
!  01 Jul 2025 - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    CHARACTER(LEN=255) :: TmpOptions
    INTEGER :: SpacePos, StartPos
    CHARACTER(LEN=63) :: Option

    !=================================================================
    ! HCO_ESMF_ParseOptions begins here
    !=================================================================
    RC = HCO_SUCCESS

    ! Set defaults
    UnmappedAction = ESMF_UNMAPPEDACTION_IGNORE
    PoleMethod = ESMF_POLEMETHOD_ALLAVG
    LineType = ESMF_LINETYPE_GREAT_CIRCLE
    NormType = ESMF_NORMTYPE_DSTAREA

    IF (LEN_TRIM(OptionsStr) == 0 .OR. TRIM(OptionsStr) == '-') RETURN

    TmpOptions = TRIM(OptionsStr)
    StartPos = 1

    ! Parse space-separated options
    DO
        SpacePos = INDEX(TmpOptions(StartPos:), ' ')
        IF (SpacePos == 0) THEN
            Option = TmpOptions(StartPos:)
        ELSE
            Option = TmpOptions(StartPos:StartPos+SpacePos-2)
        ENDIF

        ! Process option
        SELECT CASE (TRIM(Option))
            CASE ('UNMAPPED_IGNORE')
                UnmappedAction = ESMF_UNMAPPEDACTION_IGNORE
            CASE ('UNMAPPED_ERROR')
                UnmappedAction = ESMF_UNMAPPEDACTION_ERROR
            CASE ('POLE_ALLAVG')
                PoleMethod = ESMF_POLEMETHOD_ALLAVG
            CASE ('POLE_NPNTAVG')
                PoleMethod = ESMF_POLEMETHOD_NPNTAVG
            CASE ('POLE_TEETH')
                PoleMethod = ESMF_POLEMETHOD_TEETH
            CASE ('LINE_CART')
                LineType = ESMF_LINETYPE_CART
            CASE ('LINE_GREAT_CIRCLE')
                LineType = ESMF_LINETYPE_GREAT_CIRCLE
            CASE ('NORM_DSTAREA')
                NormType = ESMF_NORMTYPE_DSTAREA
            CASE ('NORM_FRACAREA')
                NormType = ESMF_NORMTYPE_FRACAREA
        END SELECT

        IF (SpacePos == 0) EXIT
        StartPos = StartPos + SpacePos
        DO WHILE (StartPos <= LEN(TmpOptions) .AND. TmpOptions(StartPos:StartPos) == ' ')
            StartPos = StartPos + 1
        ENDDO
        IF (StartPos > LEN(TmpOptions)) EXIT
    ENDDO

  END SUBROUTINE HCO_ESMF_ParseOptions
!EOC

!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCO_ESMF_PerformRegrid
!
! !DESCRIPTION: Perform ESMF regridding operation using FileData specification.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCO_ESMF_PerformRegrid(SrcField, DstField, RegridMethod, &
                                    WeightFile, RegridOpts, RouteHandle, RC)
!
! !INPUT PARAMETERS:
!
    TYPE(ESMF_Field), INTENT(IN) :: SrcField       ! Source field
    TYPE(ESMF_Field), INTENT(INOUT) :: DstField    ! Destination field
    CHARACTER(LEN=*), INTENT(IN) :: RegridMethod   ! Regrid method string
    CHARACTER(LEN=*), INTENT(IN) :: WeightFile     ! Weight file path
    CHARACTER(LEN=*), INTENT(IN) :: RegridOpts     ! Regrid options string
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_RouteHandle), INTENT(INOUT) :: RouteHandle ! Route handle
    INTEGER, INTENT(INOUT) :: RC                   ! Return code
!
! !REVISION HISTORY:
!  01 Jul 2025 - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    LOGICAL :: RouteHandleCreated
    LOGICAL :: fileExists
    INTEGER :: srcTermProcessing
    TYPE(ESMF_RegridMethod_Flag) :: ESMFMethod
    TYPE(ESMF_UnmappedAction_Flag) :: UnmappedAction
    TYPE(ESMF_PoleMethod_Flag) :: PoleMethod
    TYPE(ESMF_LineType_Flag) :: LineType
    TYPE(ESMF_NormType_Flag) :: NormType
    CHARACTER(LEN=255) :: LOC = 'HCO_ESMF_PerformRegrid (hco_esmf_regrid_mod.F90)'

    !=================================================================
    ! HCO_ESMF_PerformRegrid begins here
    !=================================================================
    RC = HCO_SUCCESS

    ! Check if route handle already exists
    RouteHandleCreated = ESMF_RouteHandleIsCreated(RouteHandle, rc=RC)
    IF (RC /= ESMF_SUCCESS) THEN
        RC = HCO_FAIL
        RETURN
    ENDIF

    ! Create route handle if needed
    IF (.NOT. RouteHandleCreated) THEN
        srcTermProcessing = 0

        ! Get regridding method
        CALL HCO_ESMF_GetRegridMethod(RegridMethod, ESMFMethod, RC)
        IF (RC /= HCO_SUCCESS) RETURN

        ! Parse options
        CALL HCO_ESMF_ParseOptions(RegridOpts, UnmappedAction, PoleMethod, &
                                   LineType, NormType, RC)
        IF (RC /= HCO_SUCCESS) RETURN

        IF (LEN_TRIM(WeightFile) > 0 .AND. TRIM(WeightFile) /= '-') THEN
            ! Use existing weight file if it exists
            INQUIRE(FILE=TRIM(WeightFile), EXIST=fileExists)

            IF (fileExists) THEN
                ! Use ESMF_FieldSMMStore to read weights from file
                CALL ESMF_FieldSMMStore(SrcField, DstField, &
                    filename=TRIM(WeightFile), &
                    routehandle=RouteHandle, rc=RC)

                IF (RC /= ESMF_SUCCESS) THEN
                    ! If weight file read fails, fall back to generating weights
                    CALL HCO_ERROR('Failed to read weights from file, generating on the fly', &
                        RC, THISLOC=LOC)

                    ! Generate weights on the fly as fallback
                    CALL ESMF_FieldRegridStore(SrcField, DstField, &
                        regridmethod=ESMFMethod, &
                        unmappedaction=UnmappedAction, &
                        srcTermProcessing=srcTermProcessing, &
                        polemethod=PoleMethod, &
                        lineType=LineType, &
                        normType=NormType, &
                        routehandle=RouteHandle, rc=RC)
                ENDIF
            ELSE
                ! Weight file specified but not found - generate weights and warn
                CALL HCO_ERROR('Weight file not found, generating weights on the fly', &
                    RC, THISLOC=LOC)

                ! Generate weights on the fly
                CALL ESMF_FieldRegridStore(SrcField, DstField, &
                    regridmethod=ESMFMethod, &
                    unmappedaction=UnmappedAction, &
                    srcTermProcessing=srcTermProcessing, &
                    polemethod=PoleMethod, &
                    lineType=LineType, &
                    normType=NormType, &
                    routehandle=RouteHandle, rc=RC)
            ENDIF
        ELSE
            ! No weight file specified - generate weights on the fly
            CALL ESMF_FieldRegridStore(SrcField, DstField, &
                regridmethod=ESMFMethod, &
                unmappedaction=UnmappedAction, &
                srcTermProcessing=srcTermProcessing, &
                polemethod=PoleMethod, &
                lineType=LineType, &
                normType=NormType, &
                routehandle=RouteHandle, rc=RC)
        ENDIF

    IF (RC /= ESMF_SUCCESS) THEN
        RC = HCO_FAIL
        RETURN
    ENDIF
    RC = HCO_SUCCESS
    ENDIF

    ! Perform the regridding
    CALL ESMF_FieldRegrid(SrcField, DstField, RouteHandle, rc=RC)
    IF (RC /= ESMF_SUCCESS) THEN
        RC = HCO_FAIL
        RETURN
    ENDIF
    RC = HCO_SUCCESS

  END SUBROUTINE HCO_ESMF_PerformRegrid
!EOC

!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCO_ESMF_CreateWeights
!
! !DESCRIPTION: Create ESMF regridding weights. Note: This interface creates
!  the regridding route handle but does not directly save weights to file.
!  Weight file generation would require additional ESMF utilities.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCO_ESMF_CreateWeights(SrcField, DstField, RegridMethod, &
                                    RegridOpts, WeightFile, RC)
!
! !INPUT PARAMETERS:
!
    TYPE(ESMF_Field), INTENT(IN) :: SrcField       ! Source field
    TYPE(ESMF_Field), INTENT(INOUT) :: DstField    ! Destination field
    CHARACTER(LEN=*), INTENT(IN) :: RegridMethod   ! Regrid method string
    CHARACTER(LEN=*), INTENT(IN) :: RegridOpts     ! Regrid options string
    CHARACTER(LEN=*), INTENT(IN) :: WeightFile     ! Weight file to create
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER, INTENT(INOUT) :: RC                   ! Return code
!
! !REVISION HISTORY:
!  01 Jul 2025 - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    TYPE(ESMF_RouteHandle) :: RouteHandle
    INTEGER :: srcTermProcessing
    TYPE(ESMF_RegridMethod_Flag) :: ESMFMethod
    TYPE(ESMF_UnmappedAction_Flag) :: UnmappedAction
    TYPE(ESMF_PoleMethod_Flag) :: PoleMethod
    TYPE(ESMF_LineType_Flag) :: LineType
    TYPE(ESMF_NormType_Flag) :: NormType
    CHARACTER(LEN=255) :: LOC = 'HCO_ESMF_CreateWeights (hco_esmf_regrid_mod.F90)'

    !=================================================================
    ! HCO_ESMF_CreateWeights begins here
    !=================================================================
    RC = HCO_SUCCESS
    srcTermProcessing = 0

    ! Get regridding method
    CALL HCO_ESMF_GetRegridMethod(RegridMethod, ESMFMethod, RC)
    IF (RC /= HCO_SUCCESS) RETURN

    ! Parse options
    CALL HCO_ESMF_ParseOptions(RegridOpts, UnmappedAction, PoleMethod, &
                               LineType, NormType, RC)
    IF (RC /= HCO_SUCCESS) RETURN

    ! Create regridding weights
    CALL ESMF_FieldRegridStore(SrcField, DstField, &
        regridmethod=ESMFMethod, &
        unmappedaction=UnmappedAction, &
        srcTermProcessing=srcTermProcessing, &
        polemethod=PoleMethod, &
        lineType=LineType, &
        normType=NormType, &
        routehandle=RouteHandle, rc=RC)

    IF (RC == ESMF_SUCCESS) THEN        ! If weight file path is provided, attempt to save the weights
        IF (LEN_TRIM(WeightFile) > 0 .AND. TRIM(WeightFile) /= '-') THEN
            ! Write weights to file using ESMF_FieldSMMStore
            CALL ESMF_FieldSMMStore(SrcField, DstField, &
                filename=TRIM(WeightFile), &
                routehandle=RouteHandle, &
                rc=RC)

            IF (RC /= ESMF_SUCCESS) THEN
                CALL HCO_ERROR('Failed to write weights to file', RC, THISLOC=LOC)
                RC = HCO_FAIL
            ELSE
                RC = HCO_SUCCESS
            ENDIF
        ENDIF

        ! Clean up the route handle since we only wanted the weights
        CALL ESMF_RouteHandleDestroy(RouteHandle, rc=RC)
        RC = HCO_SUCCESS
    ELSE
        RC = HCO_FAIL
    ENDIF

  END SUBROUTINE HCO_ESMF_CreateWeights
!EOC



!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCO_ESMF_GetRegridSpec
!
! !DESCRIPTION: Get regridding specification for a container.
! This function provides regridding specifications based on:
!  1. Parameters passed from HEMCO configuration
!  2. Container-specific settings from the data configuration
!  3. Default fallback settings
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCO_ESMF_GetRegridSpec(ContainerName, RegridSpec, Found, RC, &
                                    DefaultMethod, WeightDir)
!
! !INPUT PARAMETERS:
!
    CHARACTER(LEN=*), INTENT(IN) :: ContainerName  ! Container name
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: DefaultMethod ! Default regrid method
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: WeightDir ! Weight files directory
!
! !OUTPUT PARAMETERS:
!
    TYPE(RegridSpec_Type), INTENT(OUT) :: RegridSpec ! Regrid specification
    LOGICAL, INTENT(OUT) :: Found                   ! Whether spec was found
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER, INTENT(INOUT) :: RC                    ! Return code
!
! !REVISION HISTORY:
!  01 Jul 2025 - Initial version
!  07 Jul 2025 - Updated to accept HEMCO configuration parameters
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    CHARACTER(LEN=255) :: LOC = 'HCO_ESMF_GetRegridSpec (hco_esmf_regrid_mod.F90)'
    CHARACTER(LEN=255) :: WeightFileName
    CHARACTER(LEN=63) :: Method

    !=================================================================
    ! HCO_ESMF_GetRegridSpec begins here
    !=================================================================
    RC = HCO_SUCCESS
    Found = .FALSE.

    ! Initialize RegridSpec
    RegridSpec%ContainerName = ''
    RegridSpec%RegridMethod = ''
    RegridSpec%WeightFile = ''
    RegridSpec%Options = ''
    RegridSpec%IsActive = .FALSE.

    ! Set container name
    RegridSpec%ContainerName = TRIM(ContainerName)

    ! Determine regrid method
    IF (PRESENT(DefaultMethod)) THEN
      Method = TRIM(DefaultMethod)
    ELSE
      Method = 'CONSERVE'  ! Fallback default
    ENDIF

    ! Check for valid method
    IF (LEN_TRIM(Method) == 0 .OR. TRIM(Method) == '-') THEN
      Method = 'CONSERVE'
    ENDIF
    RegridSpec%RegridMethod = Method

    ! Construct weight file path if directory is specified
    IF (PRESENT(WeightDir)) THEN
      IF (LEN_TRIM(WeightDir) > 0 .AND. TRIM(WeightDir) /= '-') THEN
        ! Create a standardized weight filename
        WeightFileName = TRIM(WeightDir) // '/' &
                       // TRIM(ContainerName) // '_' &
                       // TRIM(Method) // '_weights.nc'
        RegridSpec%WeightFile = WeightFileName
      ENDIF
    ENDIF

    ! Set options string to empty for now
    RegridSpec%Options = ''

    ! Activate the spec and mark as found
    RegridSpec%IsActive = .TRUE.
    Found = .TRUE.

  END SUBROUTINE HCO_ESMF_GetRegridSpec
!EOC

#endif

END MODULE HCO_ESMF_REGRID_MOD
