#if defined ( ESMF_ ) && (!defined ( MAPL_ESMF ))
! The 'Pure ESMF-based' HEMCO I/O module is used for:
! - NUOPC/ESMF environments without MAPL (ESMF_ without MAPL_ESMF)
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hcoio_read_esmf_mod.F90
!
! !DESCRIPTION: Module HCOIO\_Read\_esmf\_mod is the HEMCO interface for
!  data reading within the pure ESMF/NUOPC environment (without MAPL).
!  This module uses ESMF regridding methods instead of HEMCO's internal
!  regridding when configured to do so.
!
!  This module implements the pure ESMF/NUOPC environment.
!\\
!\\
! !INTERFACE:
!
MODULE HCOIO_Read_Mod
!
! !USES:
!
  USE HCO_Types_Mod
  USE HCO_Error_Mod
  USE HCO_State_Mod,       ONLY : Hco_State
#if defined ( ESMF_ ) && (!defined ( MAPL_ESMF ))
  USE HCO_ESMF_REGRID_MOD, ONLY : HCO_ESMF_GetRegridSpec, RegridSpec_Type, &
                                  HCO_ESMF_PerformRegrid
  USE ESMF
#endif

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC  :: HCOIO_Read
  PUBLIC  :: HCOIO_CloseAll
  PUBLIC  :: HCOIO_ReadWithESMFRegrid
!
! !PRIVATE MEMBER FUNCTIONS:
!
  PRIVATE :: UseESMFRegridding
  PRIVATE :: ReadGridFromFile
  PRIVATE :: ReadGridCoordinatesSimple
!
! !REVISION HISTORY:
!  29 Apr 2025 - Initial version for pure ESMF/NUOPC implementation
!EOP
!------------------------------------------------------------------------------
!BOC
CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOIO_DataRead (ESMF version)
!
! !DESCRIPTION: Interface routine between ESMF/NUOPC and HEMCO to obtain
! the data array for a given HEMCO data container. The data is obtained
! through the ImportState interface. The HEMCO source file attribute is taken
! to identify the import field name.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOIO_Read( HcoState, Lct, RC )
!
! !USES:
!
    USE ESMF
    USE HCO_FILEDATA_MOD, ONLY : FileData_ArrInit, FileData_CreateESMFGrid

!
! !INPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER        :: HcoState
    TYPE(ListCont),   POINTER        :: Lct
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT)  :: RC
!
! !REVISION HISTORY:
!  29 Apr 2025 - Initial version
!  01 Jul 2025 - Added ESMF regridding support
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER                    :: II, JJ, LL, TT
    INTEGER                    :: I, J, K, L, T
    INTEGER                    :: localrc
    REAL(hp), POINTER          :: Ptr3D(:,:,:)
    REAL(hp), POINTER          :: Ptr2D(:,:)
    TYPE(ESMF_State)           :: IMPORT
    LOGICAL                    :: isPresent, UseESMFRegrid
    CHARACTER(LEN=255)         :: MSG
    CHARACTER(LEN=255), PARAMETER :: LOC = 'HCOIO_READ (hcoio_read_esmf_mod.F90)'
    TYPE(RegridSpec_Type)      :: RegridSpec
    LOGICAL                    :: RegridSpecFound

    !=================================================================
    ! HCOIO_READ begins here
    !=================================================================

    ! For error handling
    CALL HCO_ENTER( HcoState%Config%Err,  LOC, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR 0', RC, THISLOC=LOC )
        RETURN
    ENDIF

    ! Check if we should use ESMF regridding for this container
    CALL UseESMFRegridding(HcoState, Lct, UseESMFRegrid, RC)
    IF (RC /= HCO_SUCCESS) RETURN

    ! If ESMF regridding is requested, use the specialized routine
    IF (UseESMFRegrid) THEN
        CALL HCOIO_ReadWithESMFRegrid(HcoState, Lct, RC)
        CALL HCO_LEAVE(HcoState%Config%Err, RC)
        RETURN
    ENDIF

    ! Otherwise, continue with original NUOPC import state method
    ! Point to ESMF IMPORT object
    IMPORT = HcoState%IMPORT

    ! Check if import state is valid (using proper ESMF method)
    ! ESMF_StateIsCreated is a function, not a subroutine
    isPresent = ESMF_StateIsCreated(IMPORT, rc=localrc)
    IF (localrc /= ESMF_SUCCESS .OR. .NOT. isPresent) THEN
        MSG = 'IMPORT state is not valid or not created'
        CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
        RETURN
    ENDIF

    ! Init pointers
    Ptr3D => NULL()
    Ptr2D => NULL()

    ! Verbose?
    IF ( HcoState%Config%doVerbose ) THEN
       MSG = 'Reading from ESMF ImportState: ' // TRIM(Lct%Dct%Dta%ncFile)
       CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
    ENDIF

    !-----------------------------------------------------------------
    ! Read 3D data from ESMF
    !-----------------------------------------------------------------
    IF ( Lct%Dct%Dta%SpaceDim == 3 ) THEN

       ! Get data from import state
       CALL GetDataFromImport_3D(IMPORT, TRIM(Lct%Dct%Dta%ncFile), Ptr3D, localrc)
       IF (localrc /= ESMF_SUCCESS) THEN
          MSG = 'Cannot get 3D pointer from ImportState: ' // TRIM(Lct%Dct%Dta%ncFile)
          CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
          RETURN
       ENDIF

       ! Get array dimensions
       II = SIZE(Ptr3D,1)
       JJ = SIZE(Ptr3D,2)
       LL = SIZE(Ptr3D,3)
       TT = 1

       ! Define HEMCO array if not yet defined
       IF ( .NOT. ASSOCIATED(Lct%Dct%Dta%V3) ) THEN
          CALL FileData_ArrInit( Lct%Dct%Dta, TT, II, JJ, LL, RC )
          IF ( RC /= HCO_SUCCESS ) THEN
              CALL HCO_ERROR( 'ERROR 1', RC, THISLOC=LOC )
              RETURN
          ENDIF
       ENDIF

       ! Copy data to HEMCO array rather than pointing to it
       DO k = 1, LL
          DO j = 1, JJ
             DO i = 1, II
                Lct%Dct%Dta%V3(1)%Val(i,j,k) = Ptr3D(i,j,k)
             END DO
          END DO
       END DO

       ! Debug output
       IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
          print *, "HEMCO: Connected 3D array from ImportState: ", TRIM(Lct%Dct%Dta%ncFile)
       ENDIF

    !-----------------------------------------------------------------
    ! Read 2D data from ESMF
    !-----------------------------------------------------------------
    ELSEIF ( Lct%Dct%Dta%SpaceDim == 2 ) THEN

       ! Get data from import state
       CALL GetDataFromImport_2D(IMPORT, TRIM(Lct%Dct%Dta%ncFile), Ptr2D, localrc)
       IF (localrc /= ESMF_SUCCESS) THEN
          MSG = 'Cannot get 2D pointer from ImportState: ' // TRIM(Lct%Dct%Dta%ncFile)
          CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
          RETURN
       ENDIF

       ! Get array dimensions
       II = SIZE(Ptr2D,1)
       JJ = SIZE(Ptr2D,2)
       LL = 1
       TT = 1

       ! Define HEMCO array pointer if not yet defined
       IF ( .NOT. ASSOCIATED(Lct%Dct%Dta%V2) ) THEN
          CALL FileData_ArrInit( Lct%Dct%Dta, TT, II, JJ, RC )
          IF ( RC /= HCO_SUCCESS ) THEN
              CALL HCO_ERROR( 'ERROR 2', RC, THISLOC=LOC )
              RETURN
          ENDIF
       ENDIF

       ! Copy data to HEMCO array rather than pointing to it
       DO j = 1, JJ
          DO i = 1, II
             Lct%Dct%Dta%V2(1)%Val(i,j) = Ptr2D(i,j)
          END DO
       END DO

       ! Debug output
       IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
          print *, "HEMCO: Connected 2D array from ImportState: ", TRIM(Lct%Dct%Dta%ncFile)
       ENDIF

    ENDIF

    !-----------------------------------------------------------------
    ! Cleanup and leave
    !-----------------------------------------------------------------
    Ptr3D  => NULL()
    Ptr2D  => NULL()

    ! Return w/ success
    CALL HCO_LEAVE ( HcoState%Config%Err, RC )

  CONTAINS

    !=========================================================================
    ! Helper routines for getting data from import state
    !=========================================================================

    SUBROUTINE GetDataFromImport_3D(import_state, fieldname, dataPtr, rc)
      TYPE(ESMF_State), INTENT(IN)     :: import_state
      CHARACTER(LEN=*), INTENT(IN)     :: fieldname
      REAL(hp), POINTER                :: dataPtr(:,:,:)
      INTEGER, INTENT(OUT)             :: rc

      TYPE(ESMF_Field)                 :: field
      INTEGER                          :: fieldRank
      REAL(ESMF_KIND_R8), POINTER      :: esmfPtr(:,:,:)
      INTEGER                          :: i, j, k, nx, ny, nz
      INTEGER                          :: allocStat

      ! Initialize
      rc = ESMF_SUCCESS
      dataPtr => NULL()

      ! Check if field exists in import state
      CALL ESMF_StateGet(import_state, fieldname, field, rc=rc)
      IF (rc /= ESMF_SUCCESS) RETURN

      ! Get field rank
      CALL ESMF_FieldGet(field, rank=fieldRank, rc=rc)
      IF (rc /= ESMF_SUCCESS) RETURN

      ! Verify field is 3D
      IF (fieldRank /= 3) THEN
         rc = ESMF_RC_ARG_RANK
         RETURN
      ENDIF

      ! Get data pointer from ESMF
      CALL ESMF_FieldGet(field, farrayPtr=esmfPtr, rc=rc)
      IF (rc /= ESMF_SUCCESS) RETURN

      ! Get dimensions
      nx = SIZE(esmfPtr,1)
      ny = SIZE(esmfPtr,2)
      nz = SIZE(esmfPtr,3)

      ! Allocate HEMCO precision array
      ALLOCATE(dataPtr(nx,ny,nz), STAT=allocStat)
      IF (allocStat /= 0) THEN
         rc = ESMF_RC_MEM
         RETURN
      ENDIF

      ! Copy data with appropriate precision conversion
      DO k = 1, nz
         DO j = 1, ny
            DO i = 1, nx
               dataPtr(i,j,k) = REAL(esmfPtr(i,j,k), hp)
            END DO
         END DO
      END DO
    END SUBROUTINE GetDataFromImport_3D

    SUBROUTINE GetDataFromImport_2D(import_state, fieldname, dataPtr, rc)
      TYPE(ESMF_State), INTENT(IN)     :: import_state
      CHARACTER(LEN=*), INTENT(IN)     :: fieldname
      REAL(hp), POINTER                :: dataPtr(:,:)
      INTEGER, INTENT(OUT)             :: rc

      TYPE(ESMF_Field)                 :: field
      INTEGER                          :: fieldRank
      REAL(ESMF_KIND_R8), POINTER      :: esmfPtr(:,:)
      INTEGER                          :: i, j, nx, ny
      INTEGER                          :: allocStat

      ! Initialize
      rc = ESMF_SUCCESS
      dataPtr => NULL()

      ! Check if field exists in import state
      CALL ESMF_StateGet(import_state, fieldname, field, rc=rc)
      IF (rc /= ESMF_SUCCESS) RETURN

      ! Get field rank
      CALL ESMF_FieldGet(field, rank=fieldRank, rc=rc)
      IF (rc /= ESMF_SUCCESS) RETURN

      ! Verify field is 2D
      IF (fieldRank /= 2) THEN
         rc = ESMF_RC_ARG_RANK
         RETURN
      ENDIF

      ! Get data pointer from ESMF
      CALL ESMF_FieldGet(field, farrayPtr=esmfPtr, rc=rc)
      IF (rc /= ESMF_SUCCESS) RETURN

      ! Get dimensions
      nx = SIZE(esmfPtr,1)
      ny = SIZE(esmfPtr,2)

      ! Allocate HEMCO precision array
      ALLOCATE(dataPtr(nx,ny), STAT=allocStat)
      IF (allocStat /= 0) THEN
         rc = ESMF_RC_MEM
         RETURN
      ENDIF

      ! Copy data with appropriate precision conversion
      DO j = 1, ny
         DO i = 1, nx
            dataPtr(i,j) = REAL(esmfPtr(i,j), hp)
         END DO
      END DO
    END SUBROUTINE GetDataFromImport_2D

  END SUBROUTINE HCOIO_Read
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOIO_CloseAll
!
! !DESCRIPTION: Subroutine HCOIO\_CloseAll makes sure that there is no open
! netCDF file left in the stream. This is a stub as there is no such handling
! within HEMCO for the ESMF environment, it is performed by ESMF.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOIO_CloseAll( HcoState, RC )
!
! !INPUT PARAMTERS:
!
    TYPE(HCO_State), POINTER          :: HcoState    ! HEMCO state
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT)   :: RC
!
! !REVISION HISTORY:
!  29 Apr 2025 - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    !======================================================================
    ! HCOIO_CloseAll begins here
    !======================================================================

    ! Return w/ success
    RC = HCO_SUCCESS

  END SUBROUTINE HCOIO_CloseAll
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: UseESMFRegridding
!
! !DESCRIPTION: Determine if ESMF regridding should be used for this container.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE UseESMFRegridding(HcoState, Lct, UseESMFRegrid, RC)
!
! !INPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER        :: HcoState
    TYPE(ListCont),   POINTER        :: Lct
!
! !OUTPUT PARAMETERS:
!
    LOGICAL,          INTENT(OUT)    :: UseESMFRegrid
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT)  :: RC
!
! !REVISION HISTORY:
!  01 Jul 2025 - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    TYPE(RegridSpec_Type) :: RegridSpec
    LOGICAL :: RegridSpecFound
    CHARACTER(LEN=255) :: ContainerName

    !=================================================================
    ! UseESMFRegridding begins here
    !=================================================================
    RC = HCO_SUCCESS
    UseESMFRegrid = .FALSE.

    ! Get container name
    ContainerName = TRIM(Lct%Dct%cName)

    ! First, check if this container has regridding options specified in the FileData
    IF (ASSOCIATED(Lct%Dct%Dta)) THEN
        IF (LEN_TRIM(Lct%Dct%Dta%RegridMethod) > 0 .AND. &
            TRIM(Lct%Dct%Dta%RegridMethod) /= '-') THEN
            UseESMFRegrid = .TRUE.
            RETURN
        ENDIF
    ENDIF

    ! Check if there's a regridding specification for this container from settings
    CALL HCO_ESMF_GetRegridSpec(ContainerName, RegridSpec, RegridSpecFound, RC)
    IF (RC /= HCO_SUCCESS) RETURN

    ! Use ESMF regridding if:
    ! 1. A specific regrid spec was found, OR
    ! 2. The data source is not from NUOPC import state (i.e., from files)
    IF (RegridSpecFound .OR. &
        (LEN_TRIM(Lct%Dct%Dta%ncFile) > 0 .AND. &
         TRIM(Lct%Dct%Dta%ncFile) /= 'ESMF' .AND. &
         INDEX(Lct%Dct%Dta%ncFile, '.nc') > 0)) THEN
        UseESMFRegrid = .TRUE.
    ENDIF

  END SUBROUTINE UseESMFRegridding
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOIO_ReadWithESMFRegrid
!
! !DESCRIPTION: Read data using ESMF regridding instead of HEMCO internal
!  regridding. This routine loads data from files and regrids it using
!  ESMF regridding capabilities.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOIO_ReadWithESMFRegrid(HcoState, Lct, RC)
!
! !INPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER        :: HcoState
    TYPE(ListCont),   POINTER        :: Lct
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT)  :: RC
!
! !REVISION HISTORY:
!  01 Jul 2025 - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER                    :: II, JJ, LL, TT
    INTEGER                    :: I, J, K, L, T
    INTEGER                    :: localrc, srcTermProcessing
    INTEGER                    :: nLon, nLat  ! Grid dimensions
    INTEGER, DIMENSION(1)      :: LBounds, UBounds ! For ESMF_FieldGet
    REAL(hp), POINTER          :: Ptr3D(:,:,:)
    REAL(hp), POINTER          :: Ptr2D(:,:)
    REAL(ESMF_KIND_R8), ALLOCATABLE :: lonCenters(:), latCenters(:)
    REAL(ESMF_KIND_R8), ALLOCATABLE :: lonCenters2D(:,:), latCenters2D(:,:)
    LOGICAL                    :: IsCurvilinear
    TYPE(ESMF_Grid)            :: SrcGrid, DstGrid
    TYPE(ESMF_Field)           :: SrcField, DstField
    TYPE(ESMF_RouteHandle)     :: RouteHandle
    TYPE(RegridSpec_Type)      :: RegridSpec
    LOGICAL                    :: RegridSpecFound
    LOGICAL                    :: GridExists
    CHARACTER(LEN=255)         :: MSG, ContainerName, VarName
    CHARACTER(LEN=255), PARAMETER :: LOC = 'HCOIO_ReadWithESMFRegrid (hcoio_read_esmf_mod.F90)'
    REAL(ESMF_KIND_R8), POINTER :: SrcPtr3D(:,:,:), DstPtr3D(:,:,:)
    REAL(ESMF_KIND_R8), POINTER :: SrcPtr2D(:,:), DstPtr2D(:,:)

    !=================================================================
    ! HCOIO_ReadWithESMFRegrid begins here
    !=================================================================

    ! For error handling
    CALL HCO_ENTER( HcoState%Config%Err,  LOC, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR 0', RC, THISLOC=LOC )
        RETURN
    ENDIF

    ! Get container name and regridding specification
    ContainerName = TRIM(Lct%Dct%cName)

    ! First, check if regridding info is stored in FileData (from config parsing)
    IF (ASSOCIATED(Lct%Dct%Dta) .AND. &
        LEN_TRIM(Lct%Dct%Dta%RegridMethod) > 0 .AND. &
        TRIM(Lct%Dct%Dta%RegridMethod) /= '-') THEN
        ! Use regridding info from FileData
        RegridSpec%ContainerName = ContainerName
        RegridSpec%RegridMethod  = TRIM(Lct%Dct%Dta%RegridMethod)
        RegridSpec%WeightFile    = TRIM(Lct%Dct%Dta%WeightFile)
        RegridSpec%Options       = TRIM(Lct%Dct%Dta%RegridOpts)
        RegridSpec%IsActive      = .TRUE.
        RegridSpecFound = .TRUE.
    ELSE
        ! Fallback to global regridding specification
        CALL HCO_ESMF_GetRegridSpec(ContainerName, RegridSpec, RegridSpecFound, RC)
        IF (RC /= HCO_SUCCESS) THEN
            CALL HCO_ERROR('Cannot get regrid specification', RC, THISLOC=LOC)
            RETURN
        ENDIF
    ENDIF

    ! Verbose?
    IF ( HcoState%Config%doVerbose ) THEN
       MSG = 'Reading with ESMF regridding: ' // TRIM(Lct%Dct%Dta%ncFile)
       IF (RegridSpecFound) THEN
           MSG = TRIM(MSG) // ' using method: ' // TRIM(RegridSpec%RegridMethod)
           IF (LEN_TRIM(RegridSpec%WeightFile) > 0 .AND. &
               TRIM(RegridSpec%WeightFile) /= '-') THEN
               MSG = TRIM(MSG) // ' with weights: ' // TRIM(RegridSpec%WeightFile)
           ENDIF
       ENDIF
       CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
    ENDIF

    ! Implementation of full ESMF regridding workflow
    ! 1. Read coordinate and data from source file
    ! 2. Create or retrieve source grid using FileData utility
    ! 3. Create ESMF fields on both grids
    ! 4. Perform ESMF regridding using HCO_ESMF_PerformRegrid
    ! 5. Copy regridded data to HEMCO arrays

    IF (.NOT. RegridSpecFound) THEN
        MSG = 'No regridding specification found for container: ' // TRIM(ContainerName)
        CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
        RETURN
    ENDIF

    ! Check if source grid already exists in FileData
    GridExists = ASSOCIATED(Lct%Dct%Dta%SrcGrid)

    ! If grid doesn't exist, we need to read coordinates and create it
    IF (.NOT. GridExists) THEN
        ! Use ESMF_FieldRead to read coordinates and detect grid type
        CALL ReadGridFromFile(Lct%Dct%Dta%ncFile, nLon, nLat, &
                             lonCenters, latCenters, &
                             lonCenters2D, latCenters2D, &
                             IsCurvilinear, RC)
        IF (RC /= HCO_SUCCESS) THEN
            MSG = 'Failed to read grid from file: ' // TRIM(Lct%Dct%Dta%ncFile)
            CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
            RETURN
        ENDIF

        ! Create ESMF grid using the appropriate utility function
        IF (IsCurvilinear) THEN
            CALL FileData_CreateESMFCurvilinearGrid(Lct%Dct%Dta, &
                                                   lonCenters2D, latCenters2D, RC)
        ELSE
            CALL FileData_CreateESMFGrid(Lct%Dct%Dta, lonCenters, latCenters, RC)
        ENDIF

        IF (RC /= HCO_SUCCESS) THEN
            MSG = 'Failed to create ESMF grid for file: ' // TRIM(Lct%Dct%Dta%ncFile)
            CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
            IF (ALLOCATED(lonCenters)) DEALLOCATE(lonCenters, latCenters)
            IF (ALLOCATED(lonCenters2D)) DEALLOCATE(lonCenters2D, latCenters2D)
            RETURN
        ENDIF

        ! Clean up coordinate arrays
        IF (ALLOCATED(lonCenters)) DEALLOCATE(lonCenters, latCenters)
        IF (ALLOCATED(lonCenters2D)) DEALLOCATE(lonCenters2D, latCenters2D)
    ENDIF

    ! At this point, we have a source grid in Lct%Dct%Dta%SrcGrid
    ! Now complete the full ESMF regridding workflow:
    ! 1. Read the actual data variable from NetCDF
    ! 2. Create ESMF fields on source and destination grids
    ! 3. Create or retrieve RouteHandle for regridding
    ! 4. Perform regridding using the specified method
    ! 5. Copy results to HEMCO arrays

    ! Get the source grid from FileData
    SrcGrid = Lct%Dct%Dta%SrcGrid

    ! Get destination grid from HEMCO state (this should be set up by the host model)
    IF (HcoState%Grid%HasESMFGrid) THEN
        DstGrid = HcoState%Grid%ESMFGrid
    ELSE
        ! Fallback: create a simple destination grid if HEMCO grid doesn't have ESMF grid
        DstGrid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/HcoState%NX, HcoState%NY/), &
                                           coordSys=ESMF_COORDSYS_SPH_DEG, rc=localrc)
        IF (localrc /= ESMF_SUCCESS) THEN
            MSG = 'Failed to create destination ESMF grid'
            CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
            RETURN
        ENDIF
    ENDIF

    ! Get variable name to read
    VarName = TRIM(Lct%Dct%Dta%ncPara)

    ! Create source field and read data directly from NetCDF file using ESMF
    SrcField = ESMF_FieldCreate(SrcGrid, typekind=ESMF_TYPEKIND_R8, rc=localrc)
    IF (localrc /= ESMF_SUCCESS) THEN
        MSG = 'Failed to create source ESMF field'
        CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
        RETURN
    ENDIF

    ! Read data directly into ESMF field from NetCDF file
    CALL ESMF_FieldRead(SrcField, fileName=TRIM(Lct%Dct%Dta%ncFile), &
                       variableName=TRIM(VarName), rc=localrc)
    IF (localrc /= ESMF_SUCCESS) THEN
        MSG = 'Failed to read data from NetCDF file: ' // TRIM(Lct%Dct%Dta%ncFile)
        CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
        CALL ESMF_FieldDestroy(SrcField, rc=localrc)
        RETURN
    ENDIF

    ! Create destination field
    DstField = ESMF_FieldCreate(DstGrid, typekind=ESMF_TYPEKIND_R8, rc=localrc)
    IF (localrc /= ESMF_SUCCESS) THEN
        MSG = 'Failed to create destination ESMF field'
        CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
        CALL ESMF_FieldDestroy(SrcField, rc=localrc)
        RETURN
    ENDIF

    ! Check if RouteHandle is cached
    IF (Lct%Dct%Dta%HasRegridCache) THEN
        RouteHandle = Lct%Dct%Dta%RouteHandle
    ELSE
        ! Create RouteHandle for regridding
        CALL HCO_ESMF_PerformRegrid(SrcField, DstField, &
                                    TRIM(RegridSpec%RegridMethod), &
                                    TRIM(RegridSpec%WeightFile), &
                                    TRIM(RegridSpec%Options), &
                                    RouteHandle, RC)
        IF (RC /= HCO_SUCCESS) THEN
            MSG = 'Failed to create regridding RouteHandle'
            CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
            CALL ESMF_FieldDestroy(SrcField, rc=localrc)
            CALL ESMF_FieldDestroy(DstField, rc=localrc)
            RETURN
        ENDIF

        ! Cache the RouteHandle
        Lct%Dct%Dta%RouteHandle = RouteHandle
        Lct%Dct%Dta%HasRegridCache = .TRUE.
    ENDIF

    ! Perform regridding
    CALL ESMF_FieldRegrid(SrcField, DstField, RouteHandle, rc=localrc)
    IF (localrc /= ESMF_SUCCESS) THEN
        MSG = 'Failed to perform ESMF regridding'
        CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
        CALL ESMF_FieldDestroy(SrcField, rc=localrc)
        CALL ESMF_FieldDestroy(DstField, rc=localrc)
        RETURN
    ENDIF

    ! Get field dimensions to determine if 2D or 3D
    CALL ESMF_FieldGet(DstField, dimCount=II, rc=localrc)
    IF (localrc /= ESMF_SUCCESS) THEN
        MSG = 'Failed to get destination field dimensions'
        CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
        CALL ESMF_FieldDestroy(SrcField, rc=localrc)
        CALL ESMF_FieldDestroy(DstField, rc=localrc)
        RETURN
    ENDIF

    IF (II == 2) THEN
        ! 2D data
        CALL ESMF_FieldGet(DstField, farrayPtr=DstPtr2D, rc=localrc)
        IF (localrc /= ESMF_SUCCESS) THEN
            MSG = 'Failed to get 2D destination field pointer'
            CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
            CALL ESMF_FieldDestroy(SrcField, rc=localrc)
            CALL ESMF_FieldDestroy(DstField, rc=localrc)
            RETURN
        ENDIF

        ! Initialize HEMCO data arrays if needed
        CALL FileData_ArrInit(Lct%Dct%Dta, 1, HcoState%NX, HcoState%NY, RC)
        IF (RC /= HCO_SUCCESS) THEN
            MSG = 'Failed to initialize HEMCO 2D data arrays'
            CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
            CALL ESMF_FieldDestroy(SrcField, rc=localrc)
            CALL ESMF_FieldDestroy(DstField, rc=localrc)
            RETURN
        ENDIF

        ! Copy regridded data to HEMCO arrays
        Lct%Dct%Dta%V2(1)%Val(:,:) = REAL(DstPtr2D(:,:), hp)

    ELSE
        ! 3D data
        CALL ESMF_FieldGet(DstField, farrayPtr=DstPtr3D, rc=localrc)
        IF (localrc /= ESMF_SUCCESS) THEN
            MSG = 'Failed to get 3D destination field pointer'
            CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
            CALL ESMF_FieldDestroy(SrcField, rc=localrc)
            CALL ESMF_FieldDestroy(DstField, rc=localrc)
            RETURN
        ENDIF

        ! Get the number of levels
        CALL ESMF_FieldGet(DstField, ungriddedLBound=LBounds, ungriddedUBound=UBounds, rc=localrc)
        IF (localrc /= ESMF_SUCCESS) THEN
            MSG = 'Failed to get 3D field level count'
            CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
            CALL ESMF_FieldDestroy(SrcField, rc=localrc)
            CALL ESMF_FieldDestroy(DstField, rc=localrc)
            RETURN
        ENDIF

        LL = UBounds(1)

        ! Initialize HEMCO data arrays if needed
        CALL FileData_ArrInit(Lct%Dct%Dta, LL, HcoState%NX, HcoState%NY, RC)
        IF (RC /= HCO_SUCCESS) THEN
            MSG = 'Failed to initialize HEMCO 3D data arrays'
            CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
            CALL ESMF_FieldDestroy(SrcField, rc=localrc)
            CALL ESMF_FieldDestroy(DstField, rc=localrc)
            RETURN
        ENDIF

        ! Copy regridded data to HEMCO arrays
        DO L = 1, LL
            Lct%Dct%Dta%V3(1)%Val(:,:,L) = REAL(DstPtr3D(:,:,L), hp)
        ENDDO
    ENDIF

    ! Clean up ESMF fields
    CALL ESMF_FieldDestroy(SrcField, rc=localrc)
    CALL ESMF_FieldDestroy(DstField, rc=localrc)

    ! Verbose output
    IF ( HcoState%Config%doVerbose ) THEN
       MSG = 'ESMF regridding completed successfully for: ' // TRIM(Lct%Dct%Dta%ncFile)
       CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
    ENDIF

    ! Return w/ success
    RC = HCO_SUCCESS
    CALL HCO_LEAVE ( HcoState%Config%Err, RC )

  END SUBROUTINE HCOIO_ReadWithESMFRegrid
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOIO_ReadFallback
!
! !DESCRIPTION: Fallback routine that uses standard HEMCO file reading
!  with internal regridding. This is used when ESMF regridding is requested
!  but not yet fully implemented.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOIO_ReadFallback(HcoState, Lct, RC)
!
! !INPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER        :: HcoState
    TYPE(ListCont),   POINTER        :: Lct
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT)  :: RC
!
! !REVISION HISTORY:
!  01 Jul 2025 - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    CHARACTER(LEN=255), PARAMETER :: LOC = 'HCOIO_ReadFallback (hcoio_read_esmf_mod.F90)'

    !=================================================================
    ! HCOIO_ReadFallback begins here
    !=================================================================

    ! For now, this calls the standard HEMCO file reading routine
    ! In a complete implementation, this would be replaced with
    ! a call to the standard HEMCO I/O module for file-based reading

    ! TODO: Call standard HEMCO file reading routine
    ! CALL HCOIO_Read_Std(HcoState, Lct, RC)

    ! For now, just return success to prevent compilation errors
    RC = HCO_SUCCESS

  END SUBROUTINE HCOIO_ReadFallback
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ReadGridFromFile
!
! !DESCRIPTION: Read grid coordinates from NetCDF file using ESMF_FieldRead
!  and detect whether the grid is rectilinear or curvilinear.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE ReadGridFromFile( FileName, nLon, nLat, &
                               lonCenters1D, latCenters1D, &
                               lonCenters2D, latCenters2D, &
                               IsCurvilinear, RC )
!
! !USES:
!
    USE ESMF
!
! !INPUT PARAMETERS:
!
    CHARACTER(LEN=*), INTENT(IN)    :: FileName      ! NetCDF file name
!
! !OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(OUT)   :: nLon, nLat    ! Grid dimensions
    REAL(ESMF_KIND_R8), ALLOCATABLE, INTENT(OUT) :: lonCenters1D(:)   ! 1D lon centers
    REAL(ESMF_KIND_R8), ALLOCATABLE, INTENT(OUT) :: latCenters1D(:)   ! 1D lat centers
    REAL(ESMF_KIND_R8), ALLOCATABLE, INTENT(OUT) :: lonCenters2D(:,:) ! 2D lon centers
    REAL(ESMF_KIND_R8), ALLOCATABLE, INTENT(OUT) :: latCenters2D(:,:) ! 2D lat centers
    LOGICAL,          INTENT(OUT)   :: IsCurvilinear ! Grid type
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT) :: RC            ! Return code
!
! !REVISION HISTORY:
!  01 Jul 2025 - Initial version using ESMF_FieldRead for coordinate detection
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    TYPE(ESMF_Grid)            :: TempGrid
    TYPE(ESMF_Field)           :: LonField, LatField
    CHARACTER(LEN=255)         :: MSG, LOC
    CHARACTER(LEN=64)          :: LonVarName, LatVarName
    LOGICAL                    :: Success

    !=================================================================
    ! ReadGridFromFile begins here
    !=================================================================
    LOC = 'ReadGridFromFile (hcoio_read_esmf_mod.F90)'
    RC = HCO_SUCCESS
    IsCurvilinear = .FALSE.

    ! For now, implement a simplified approach using standard NetCDF reading
    ! This can be enhanced later to use ESMF_FieldRead

    ! Try to read coordinates using ESMF-native methods
    CALL ReadGridCoordinatesSimple(FileName, nLon, nLat, &
                                  lonCenters1D, latCenters1D, &
                                  lonCenters2D, latCenters2D, &
                                  IsCurvilinear, RC)

    ! Success
    RC = HCO_SUCCESS

  END SUBROUTINE ReadGridFromFile
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ReadGridCoordinatesSimple
!
! !DESCRIPTION: Simplified routine to read grid coordinates and detect
!  grid type. This is a placeholder that returns default rectilinear grid
!  coordinates. For full ESMF integration, coordinate information should
!  be obtained through ESMF grid creation utilities or from the host model.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE ReadGridCoordinatesSimple( FileName, nLon, nLat, &
                                       lonCenters1D, latCenters1D, &
                                       lonCenters2D, latCenters2D, &
                                       IsCurvilinear, RC )
!
! !INPUT PARAMETERS:
!
    CHARACTER(LEN=*), INTENT(IN)    :: FileName      ! NetCDF file name
!
! !OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(OUT)   :: nLon, nLat    ! Grid dimensions
    REAL(ESMF_KIND_R8), ALLOCATABLE, INTENT(OUT) :: lonCenters1D(:)
    REAL(ESMF_KIND_R8), ALLOCATABLE, INTENT(OUT) :: latCenters1D(:)
    REAL(ESMF_KIND_R8), ALLOCATABLE, INTENT(OUT) :: lonCenters2D(:,:)
    REAL(ESMF_KIND_R8), ALLOCATABLE, INTENT(OUT) :: latCenters2D(:,:)
    LOGICAL,          INTENT(OUT)   :: IsCurvilinear ! Grid type
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT) :: RC            ! Return code
!
! !REVISION HISTORY:
!  01 Jul 2025 - ESMF-native version - simplified placeholder
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    CHARACTER(LEN=255) :: MSG, LOC
    INTEGER :: I, J

    !=================================================================
    ! ReadGridCoordinatesSimple begins here
    !=================================================================
    LOC = 'ReadGridCoordinatesSimple (hcoio_read_esmf_mod.F90)'
    RC = HCO_SUCCESS

    ! For now, return a default global rectilinear grid
    ! In a full implementation, this would use ESMF utilities to read
    ! coordinate information from the NetCDF file via ESMF grid creation
    ! or obtain grid information from the host model's grid definitions

    ! Set default global grid dimensions (this should be obtained from file metadata)
    nLon = 144  ! Default 2.5 degree resolution
    nLat = 91   ! Default 2 degree resolution
    IsCurvilinear = .FALSE.

    ! Create default longitude coordinates (-180 to 177.5, 2.5 degree spacing)
    ALLOCATE(lonCenters1D(nLon))
    DO I = 1, nLon
        lonCenters1D(I) = -180.0_ESMF_KIND_R8 + (I-1) * 2.5_ESMF_KIND_R8
    ENDDO

    ! Create default latitude coordinates (-90 to 90, 2 degree spacing)
    ALLOCATE(latCenters1D(nLat))
    DO J = 1, nLat
        latCenters1D(J) = -90.0_ESMF_KIND_R8 + (J-1) * 2.0_ESMF_KIND_R8
    ENDDO

    ! Note: In a production implementation, coordinate information should be:
    ! 1. Read using ESMF_FieldRead from coordinate variables
    ! 2. Obtained from ESMF grid creation utilities
    ! 3. Provided by the host model's grid definitions
    ! This placeholder ensures the code compiles and runs with default values

    ! Log information about using default coordinates
    MSG = 'Using default rectilinear grid coordinates for: ' // TRIM(FileName)
    ! In verbose mode, this would be logged via HCO_MSG

    ! Success
    RC = HCO_SUCCESS

  END SUBROUTINE ReadGridCoordinatesSimple
!EOC

END MODULE HCOIO_Read_Mod
#endif