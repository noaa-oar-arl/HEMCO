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
    INTEGER :: srcTermProcessing
    TYPE(ESMF_RegridMethod_Flag) :: ESMFMethod
    TYPE(ESMF_UnmappedAction_Flag) :: UnmappedAction
    TYPE(ESMF_PoleMethod_Flag) :: PoleMethod
    TYPE(ESMF_LineType_Flag) :: LineType
    TYPE(ESMF_NormType_Flag) :: NormType

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
            ! Use existing weight file - this would need to be implemented
            ! with ESMF_FieldSMMStore reading from file
            ! For now, generate weights on the fly
            CALL ESMF_FieldRegridStore(SrcField, DstField, &
                regridmethod=ESMFMethod, &
                unmappedaction=UnmappedAction, &
                srcTermProcessing=srcTermProcessing, &
                polemethod=PoleMethod, &
                lineType=LineType, &
                normType=NormType, &
                routehandle=RouteHandle, rc=RC)
        ELSE
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

        IF (RC /= ESMF_SUCCESS) RETURN
    ENDIF

    ! Perform the regridding
    CALL ESMF_FieldRegrid(SrcField, DstField, RouteHandle, rc=RC)

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
    ! Note: ESMF_FieldRegridStore doesn't directly save to file in this interface
    ! Weight files would need to be generated using other ESMF utilities
    CALL ESMF_FieldRegridStore(SrcField, DstField, &
        regridmethod=ESMFMethod, &
        unmappedaction=UnmappedAction, &
        srcTermProcessing=srcTermProcessing, &
        polemethod=PoleMethod, &
        lineType=LineType, &
        normType=NormType, &
        routehandle=RouteHandle, rc=RC)

    IF (RC == ESMF_SUCCESS) THEN
        ! Clean up the route handle since we only wanted the weights
        CALL ESMF_RouteHandleDestroy(RouteHandle, rc=RC)
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
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCO_ESMF_GetRegridSpec(ContainerName, RegridSpec, Found, RC)
!
! !INPUT PARAMETERS:
!
    CHARACTER(LEN=*), INTENT(IN) :: ContainerName  ! Container name
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
!EOP
!------------------------------------------------------------------------------
!BOC
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

    ! For now, this is a placeholder that doesn't find any specs
    ! In a full implementation, this would look up regrid specifications
    ! from configuration files or data structures

  END SUBROUTINE HCO_ESMF_GetRegridSpec
!EOC

#endif

END MODULE HCO_ESMF_REGRID_MOD
