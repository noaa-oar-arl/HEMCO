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

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC  :: HCOIO_Read
  PUBLIC  :: HCOIO_CloseAll
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
    USE HCO_FILEDATA_MOD, ONLY : FileData_ArrInit

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
    LOGICAL                    :: isPresent
    CHARACTER(LEN=255)         :: MSG
    CHARACTER(LEN=255), PARAMETER :: LOC = 'HCOIO_READ (hcoio_read_esmf_mod.F90)'

    !=================================================================
    ! HCOIO_READ begins here
    !=================================================================

    ! For error handling
    CALL HCO_ENTER( HcoState%Config%Err,  LOC, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR 0', RC, THISLOC=LOC )
        RETURN
    ENDIF

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
END MODULE HCOIO_Read_Mod
#endif