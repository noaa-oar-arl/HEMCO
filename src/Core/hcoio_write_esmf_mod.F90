#if defined ( ESMF_ ) && (!defined ( MAPL_ESMF ))
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hcoio_write_esmf_mod.F90
!
! !DESCRIPTION: Module HCOIO\_Write\_esmf\_mod is the HEMCO interface for data
!  writing within the pure ESMF/NUOPC environment (without MAPL). It provides
!  routines to write diagnostic output via the ESMF/NUOPC export state.
!\\
!\\
! !INTERFACE:
!
MODULE HCOIO_Write_Mod
!
! !USES:
!
  USE HCO_Error_Mod
  USE HCO_Types_Mod
  USE HCO_Diagn_Mod
  USE HCO_State_Mod, ONLY : HCO_State

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: HCOIO_Write
  PUBLIC :: HCOIO_WriteOut
  PUBLIC :: HCOIO_WriteOutAsDiag
  PUBLIC :: HCOIO_GetDiagName
  PUBLIC :: HCOIO_WriteFieldESMF
!
! !PRIVATE MEMBER FUNCTIONS:
!
  PRIVATE :: ConstructTimeStamp
  PRIVATE :: WriteToFile
  PRIVATE :: CreateESMFFieldFromData
!
! !DEFINED PARAMETERS:
!
  ! Fill value used in HEMCO diagnostics netCDF files.
  REAL(sp), PARAMETER :: FillValue = HCO_MISSVAL
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
! !IROUTINE: HCOIO_Write
!
! !DESCRIPTION: Subroutine HCOIO\_Write is the connection between HEMCO and
!  the pure ESMF/NUOPC environment. It is called from hco_diagn_mod.F90 when
!  outputting data.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOIO_Write( HcoState, cID, cName, dat2D, dat3D, &
                         lev, vDiagn, CoordL, RC          )
!
! !USES:
!
    USE ESMF

!
! !INPUT PARAMETERS:
!
    TYPE(HCO_State), POINTER        :: HcoState     ! HcO State
    INTEGER,          INTENT(IN)    :: cID          ! Collection ID
    CHARACTER(LEN=*), INTENT(IN)    :: cName        ! Collection name
    REAL(sp),         POINTER       :: dat2D(:,:)   ! 2D data
    REAL(sp),         POINTER       :: dat3D(:,:,:) ! 3D data
    INTEGER,          INTENT(IN)    :: lev(2)       ! lev dim. (if 3D)
    CHARACTER(LEN=*), INTENT(IN)    :: vDiagn       ! Variable name
    INTEGER,          INTENT(IN)    :: CoordL       ! Coord lev (if 3D)
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT) :: RC           ! Return code
!
! !REVISION HISTORY:
!  29 Apr 2025 - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER                        :: II, JJ, LL
    INTEGER                        :: FLAG
    CHARACTER(LEN=255)             :: diagName, msg
    LOGICAL                        :: IsDiagDefined, isPresent
    TYPE(ESMF_State)               :: EXPORT
    INTEGER                        :: localrc
    CHARACTER(LEN=255), PARAMETER  :: LOC = 'HCOIO_WRITE (hcoio_write_esmf_mod.F90)'

    !=================================================================
    ! HCOIO_WRITE begins here!
    !=================================================================

    ! Enter
    CALL HCO_ENTER ( HcoState%Config%Err, LOC, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR ( 'ERROR 0', RC, THISLOC=LOC )
        RETURN
    ENDIF

    ! Check if the internal export state is valid
    EXPORT = HcoState%EXPORT
    ! Use ESMF_StateIsCreated instead of direct comparison with ESMF_StateEmpty
    isPresent = ESMF_StateIsCreated(EXPORT, rc=localrc)
    IF (.NOT. isPresent .OR. localrc /= ESMF_SUCCESS) THEN
        CALL HCO_WARNING('EXPORT state is empty or invalid - diagnostics will not be exported', THISLOC=LOC)

        ! Even if EXPORT is empty, we can still attempt to write to file
        ! Fall through and allow file-based output to work
    ENDIF

    ! Get the diagnostic name for export
    CALL HCOIO_GetDiagName ( HcoState, cID, cName, vDiagn, diagName, RC )
    IF ( RC /= HCO_SUCCESS ) RETURN

    ! If export state is not empty, check if diagnostic is defined
    IF (isPresent) THEN
        CALL IsDiagInExport(EXPORT, diagName, IsDiagDefined, localrc)
        IF (localrc /= ESMF_SUCCESS) THEN
            CALL HCO_ERROR('Failed to check if diagnostic exists in export state', RC, THISLOC=LOC)
            RETURN
        ENDIF

        ! If diagnostic is defined in export, write to export state
        IF (IsDiagDefined) THEN
            ! Verbose output
            IF ( HcoState%Config%doVerbose ) THEN
                MSG = 'Writing to ESMF ExportState: ' // TRIM(diagName)
                CALL HCO_MSG(MSG,LUN=HcoState%Config%hcoLogLUN)
            ENDIF

            !-----------------------------------------------------------------
            ! Write 3D data to ESMF export state
            !-----------------------------------------------------------------
            IF ( ASSOCIATED(dat3D) ) THEN
                II = SIZE(dat3D,1)
                JJ = SIZE(dat3D,2)
                LL = SIZE(dat3D,3)

                CALL WriteToExport_3D(EXPORT, diagName, dat3D, localrc)
                IF (localrc /= ESMF_SUCCESS) THEN
                    MSG = 'Failed to write 3D diagnostic to export state: ' // TRIM(diagName)
                    CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
                    RETURN
                ENDIF

                ! Debug output
                IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
                    MSG = 'Wrote 3D diagnostic to export state: ' // TRIM(diagName)
                    CALL HCO_MSG(MSG, LUN=HcoState%Config%hcoLogLUN)
                ENDIF

            !-----------------------------------------------------------------
            ! Write 2D data to ESMF export state
            !-----------------------------------------------------------------
            ELSEIF ( ASSOCIATED(dat2D) ) THEN
                II = SIZE(dat2D,1)
                JJ = SIZE(dat2D,2)

                CALL WriteToExport_2D(EXPORT, diagName, dat2D, localrc)
                IF (localrc /= ESMF_SUCCESS) THEN
                    MSG = 'Failed to write 2D diagnostic to export state: ' // TRIM(diagName)
                    CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
                    RETURN
                ENDIF

                ! Debug output
                IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
                    MSG = 'Wrote 2D diagnostic to export state: ' // TRIM(diagName)
                    CALL HCO_MSG(MSG, LUN=HcoState%Config%hcoLogLUN)
                ENDIF
            ENDIF
        ENDIF
    ENDIF

    !-----------------------------------------------------------------
    ! Return
    !-----------------------------------------------------------------

    ! Leave w/ success
    CALL HCO_LEAVE( HcoState%Config%Err, RC )

  CONTAINS

    !=========================================================================
    ! Helper routines for interfacing with ESMF export
    !=========================================================================

    SUBROUTINE IsDiagInExport(export_state, diag_name, is_defined, rc)
      TYPE(ESMF_State), INTENT(IN)       :: export_state
      CHARACTER(LEN=*), INTENT(IN)       :: diag_name
      LOGICAL, INTENT(OUT)               :: is_defined
      INTEGER, INTENT(OUT)               :: rc

      TYPE(ESMF_Field)                   :: field

      ! Initialize
      rc = ESMF_SUCCESS
      is_defined = .FALSE.

      ! Check if the field exists in the export state using standard ESMF_StateGet
      ! Handle errors explicitly to determine if field exists
      CALL ESMF_StateGet(export_state, itemName=diag_name, field=field, rc=rc)

      ! If rc is successful, the field exists
      IF (rc == ESMF_SUCCESS) THEN
          is_defined = .TRUE.
      ELSE
          ! This is not an error, just means field doesn't exist
          is_defined = .FALSE.
      ENDIF

      ! Reset rc to success since finding/not finding a field is not an error
      rc = ESMF_SUCCESS
    END SUBROUTINE IsDiagInExport

    SUBROUTINE WriteToExport_3D(export_state, fieldname, data, rc)
      TYPE(ESMF_State), INTENT(INOUT)    :: export_state
      CHARACTER(LEN=*), INTENT(IN)       :: fieldname
      REAL(sp), INTENT(IN)               :: data(:,:,:)
      INTEGER, INTENT(OUT)               :: rc

      TYPE(ESMF_Field)                   :: field
      REAL(ESMF_KIND_R8), POINTER        :: ptr(:,:,:)
      INTEGER                            :: i, j, k

      ! Initialize
      rc = ESMF_SUCCESS

      ! Get field from export state
      CALL ESMF_StateGet(export_state, fieldname, field, rc=rc)
      IF (rc /= ESMF_SUCCESS) RETURN

      ! Get pointer to field data
      CALL ESMF_FieldGet(field, farrayPtr=ptr, rc=rc)
      IF (rc /= ESMF_SUCCESS) RETURN

      ! Copy data to field
      DO k = 1, SIZE(data,3)
        DO j = 1, SIZE(data,2)
          DO i = 1, SIZE(data,1)
            ptr(i,j,k) = data(i,j,k)
          ENDDO
        ENDDO
      ENDDO
    END SUBROUTINE WriteToExport_3D

    SUBROUTINE WriteToExport_2D(export_state, fieldname, data, rc)
      TYPE(ESMF_State), INTENT(INOUT)    :: export_state
      CHARACTER(LEN=*), INTENT(IN)       :: fieldname
      REAL(sp), INTENT(IN)               :: data(:,:)
      INTEGER, INTENT(OUT)               :: rc

      TYPE(ESMF_Field)                   :: field
      REAL(ESMF_KIND_R8), POINTER        :: ptr(:,:)
      INTEGER                            :: i, j

      ! Initialize
      rc = ESMF_SUCCESS

      ! Get field from export state
      CALL ESMF_StateGet(export_state, fieldname, field, rc=rc)
      IF (rc /= ESMF_SUCCESS) RETURN

      ! Get pointer to field data
      CALL ESMF_FieldGet(field, farrayPtr=ptr, rc=rc)
      IF (rc /= ESMF_SUCCESS) RETURN

      ! Copy data to field
      DO j = 1, SIZE(data,2)
        DO i = 1, SIZE(data,1)
          ptr(i,j) = data(i,j)
        ENDDO
      ENDDO
    END SUBROUTINE WriteToExport_2D

  END SUBROUTINE HCOIO_Write
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOIO_WriteOut
!
! !DESCRIPTION: Routine HCOIO\_WriteOut writes the data stored in container
!  ContName to the specified netCDF file.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOIO_WriteOut( HcoState, cName, OutFile, RC )
!
! !USES:
!
    USE ESMF
!
! !INPUT PARAMETERS:
!
    TYPE(HCO_State), POINTER         :: HcoState    ! HEMCO state object
    CHARACTER(LEN=*), INTENT(IN   )  :: cName       ! container name
    CHARACTER(LEN=*), INTENT(IN   )  :: OutFile     ! Output file name
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT)  :: RC          ! Return code
!
! !REVISION HISTORY:
!  29 Apr 2025 - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    REAL(sp), POINTER        :: Arr2D(:,:)
    REAL(sp), POINTER        :: Arr3D(:,:,:)
    TYPE(DiagnCont), POINTER :: Diagn
    CHARACTER(LEN=255)       :: MSG, LOC
    INTEGER                  :: RC_TMP
    LOGICAL                  :: Found, FOUND_CONT

    !======================================================================
    ! HCOIO_WRITEOUT begins here
    !======================================================================

    ! Initialize
    RC = HCO_SUCCESS
    Diagn => NULL()
    Arr2D => NULL()
    Arr3D => NULL()
    LOC = 'HCOIO_WRITEOUT (hcoio_write_esmf_mod.F90)'

    ! Enter
    CALL HCO_ENTER( HcoState%Config%Err, LOC, RC )
    IF ( RC /= HCO_SUCCESS ) RETURN

    ! Verbose
    IF ( HcoState%Config%doVerbose ) THEN
       MSG = 'Write container ' // TRIM(cName) // ' to file ' // TRIM(OutFile)
       CALL HCO_MSG(MSG, LUN=HcoState%Config%hcoLogLUN)
    ENDIF

    ! Try to get content of diagnostics container cName.
    ! Pass integer flag instead of logical for FOUND_CONT
    CALL Diagn_Get( HcoState, .FALSE., Diagn, FLAG=RC_TMP, RC=RC, &
                    cName=cName )
    FOUND_CONT = (RC_TMP == HCO_SUCCESS)

    ! Container exists
    IF ( FOUND_CONT ) THEN
       IF ( Diagn%SpaceDim == 2 ) THEN
          Arr2D => Diagn%Arr2D%Val
          ! Use ESMF field writing for CF-compliant output
          CALL HCOIO_WriteFieldESMF(HcoState, cName, OutFile, &
                                   Arr2D, NULL(), &
                                   VarUnit='unknown', &
                                   VarLongName=cName, RC=RC)
       ELSE
          Arr3D => Diagn%Arr3D%Val
          ! Use ESMF field writing for CF-compliant output
          CALL HCOIO_WriteFieldESMF(HcoState, cName, OutFile, &
                                   NULL(), Arr3D, &
                                   VarUnit='unknown', &
                                   VarLongName=cName, RC=RC)
       ENDIF

       IF ( RC /= HCO_SUCCESS ) THEN
           CALL HCO_ERROR ( 'Error writing to file using ESMF', RC, THISLOC=LOC )
           RETURN
       ENDIF

    ! Container not found - error
    ELSE
       MSG = 'Cannot find container ' // TRIM(cName)
       CALL HCO_ERROR( MSG, RC )
       RETURN
    ENDIF

    ! Cleanup
    Diagn => NULL()
    Arr2D => NULL()
    Arr3D => NULL()

    ! Leave w/ success
    CALL HCO_LEAVE( HcoState%Config%Err, RC )

  END SUBROUTINE HCOIO_WriteOut
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOIO_WriteOutAsDiag
!
! !DESCRIPTION: Subroutine HCOIO\_WriteOutAsDiag is a wrapper routine to allow
!  writing out arrays. This allows handling arbitrary arrays in a standard way.
!  In the ESMF implementation, this writes to a file.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOIO_WriteOutAsDiag( HcoState, Arr, COUNT, RC )
!
! !USES:
!
    USE ESMF
!
! !INPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER         :: HcoState    ! HCO state
    REAL(sp),         POINTER         :: Arr(:,:,:)  ! Data to be written
    INTEGER,          INTENT(IN)      :: COUNT       ! Counter
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT)   :: RC          ! Return code
!
! !REVISION HISTORY:
!  29 Apr 2025 - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    CHARACTER(LEN=255) :: MSG, OutFile, LOC

    ! ================================================================
    ! HCOIO_WRITEOUTASDIAG begins here
    ! ================================================================

    ! Initialize
    RC = HCO_SUCCESS
    LOC = 'HCOIO_WRITEOUTASDIAG (hcoio_write_esmf_mod.F90)'

    ! Enter
    CALL HCO_ENTER( HcoState%Config%Err, LOC, RC )
    IF ( RC /= HCO_SUCCESS ) RETURN

    ! Construct output filename
    WRITE(OutFile, '(a,i5.5,a)') 'HEMCO_Diagn.', COUNT, '.nc'

    ! Write to file using ESMF field writing for CF-compliant output
    CALL HCOIO_WriteFieldESMF(HcoState, 'HEMCO_Diagn', OutFile, &
                             NULL(), Arr, &
                             VarUnit='unknown', &
                             VarLongName='HEMCO Diagnostics', RC=RC)
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR ( 'Error writing to diagnostics file using ESMF', RC, THISLOC=LOC )
        RETURN
    ENDIF

    ! Verbose mode
    IF ( HcoState%Config%doVerbose ) THEN
        MSG = 'Wrote diagnostics to ' // TRIM(OutFile)
        CALL HCO_MSG(MSG, LUN=HcoState%Config%hcoLogLUN)
    ENDIF

    ! Leave w/ success
    CALL HCO_LEAVE( HcoState%Config%Err, RC )

  END SUBROUTINE HCOIO_WriteOutAsDiag
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOIO_GetDiagName
!
! !DESCRIPTION: Routine HCOIO\_GetDiagName generates the name for a diagnostic
!  field in the GCHP/ESMF export state. The name will be composed of the
!  collection name followed by the diagnostic name. If the collection number
!  is 13 (restart collection) or the collection name is 'Restart', the restart
!  field name will just be the variable name.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOIO_GetDiagName ( HcoState, cID, cName, vName, DiagName, RC )
!
! !USES:
!
    USE HCO_CharTools_Mod
!
! !INPUT PARAMETERS:
!
    TYPE(HCO_State), POINTER        :: HcoState     ! HCO state
    INTEGER,          INTENT(IN)    :: cID          ! collection ID
    CHARACTER(LEN=*), INTENT(IN)    :: cName        ! collection name
    CHARACTER(LEN=*), INTENT(IN)    :: vName        ! variable name
!
! !OUTPUT PARAMETERS:
!
    CHARACTER(LEN=*), INTENT(OUT)   :: DiagName     ! diagnostic name
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT) :: RC           ! return code
!
! !REVISION HISTORY:
!  29 Apr 2025 - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
    !=================================================================
    ! HCOIO_GETDIACNAME begins here!
    !=================================================================

    ! Initialize
    RC = HCO_SUCCESS

    ! Restart collection needs special export name format
    IF ( cID == 13 .OR. TRIM(cName) == 'Restart' ) THEN
       DiagName = TRIM(vName)

    ! Default export name format: CollectionName_FieldName
    ELSE
       DiagName = TRIM(cName) // '_' // TRIM(vName)
    ENDIF

    ! Replace any whitespace in the diagnostic name with underscores
    CALL HCO_CharTools_ReplaceWhiteSpace( DiagName )

  END SUBROUTINE HCOIO_GetDiagName
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: WriteToFile
!
! !DESCRIPTION: Subroutine WriteToFile writes data to a netCDF file.
!  It's a simplified version of HCOIO_Write from the standard HEMCO I/O module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE WriteToFile( HcoState, DiagnName, FileName, Arr2D, Arr3D, COUNTER, RC )
!
! !USES:
!
    USE HCO_m_netCDF_io_define
    USE HCO_m_netcdf_io_open
    USE HCO_Ncdf_Mod,        ONLY : NC_Create
    USE HCO_Ncdf_Mod,        ONLY : NC_Close
    USE HCO_Ncdf_Mod,        ONLY : NC_Var_Def
    USE HCO_Ncdf_Mod,        ONLY : NC_Var_Write
    USE HCO_State_Mod,       ONLY : HCO_State
    USE HCO_JulDay_Mod,      ONLY : JulDay

    ! Parameters for netCDF routines
    include "netcdf.inc"
!
! !INPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER        :: HcoState     ! HEMCO state object
    CHARACTER(LEN=*), INTENT(IN)     :: DiagnName    ! Diagnostic name
    CHARACTER(LEN=*), INTENT(IN)     :: FileName     ! Output filename
    REAL(sp),         POINTER        :: Arr2D(:,:)   ! 2D data to write (optional)
    REAL(sp),         POINTER        :: Arr3D(:,:,:) ! 3D data to write (optional)
    INTEGER,          INTENT(IN)     :: COUNTER      ! Counter (for diagnostics)
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT)  :: RC           ! Return code
!
! !REVISION HISTORY:
!  29 Apr 2025 - Initial version based on HCOIO_Write from standard HEMCO I/O
!  30 Apr 2025 - Added support for curvilinear grids
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER                   :: I, J, levIdTmp
    REAL(dp)                  :: GMT, JD1, THISDAY, P0
    INTEGER                   :: YYYY, MM, DD, h, m, s, nLevTmp
    REAL(sp), POINTER         :: nctime(:)
    REAL(dp), POINTER         :: Arr1D(:)
    REAL(sp), POINTER         :: Arr2Dlon(:,:)       ! For curvilinear grid lon
    REAL(sp), POINTER         :: Arr2Dlat(:,:)       ! For curvilinear grid lat
    REAL(sp), POINTER         :: OutArr2D(:,:)
    REAL(sp), POINTER         :: OutArr3D(:,:,:)
    REAL(sp), POINTER         :: OutArr4D(:,:,:,:)
    REAL(hp), POINTER         :: hyam(:)
    REAL(hp), POINTER         :: hybm(:)
    CHARACTER(LEN=255)        :: MSG, LOC
    CHARACTER(LEN=31)         :: myLName, mySName, myFterm
    CHARACTER(LEN=63)         :: timeunit
    INTEGER                   :: fId, lonId, latId, levId, TimeId
    INTEGER                   :: VarCt
    INTEGER                   :: nLon, nLat, nLev, nTime
    INTEGER                   :: Prc, L
    LOGICAL                   :: NoLevDim, Is3D
    LOGICAL                   :: IsCurvilinear       ! Flag for curvilinear grid

    CHARACTER(LEN=255), PARAMETER :: DefContact = 'HEMCO Support (hemco_support@as.harvard.edu)'
    CHARACTER(LEN=255), PARAMETER :: DefReference = 'wiki.geos-chem.org/The_HEMCO_Users_Guide'

    !=================================================================
    ! WriteToFile begins here!
    !=================================================================

    ! Initialize
    LOC = 'WriteToFile (hcoio_write_esmf_mod.F90)'
    RC  = HCO_SUCCESS
    Arr1D      => NULL()
    Arr2Dlon   => NULL()
    Arr2Dlat   => NULL()
    OutArr2D   => NULL()
    OutArr3D   => NULL()
    OutArr4D   => NULL()
    hyam       => NULL()
    hybm       => NULL()
    nctime     => NULL()

    ! Check that we have data
    IF (.NOT. ASSOCIATED(Arr2D) .AND. .NOT. ASSOCIATED(Arr3D)) THEN
        MSG = 'Both Arr2D and Arr3D are NULL - nothing to write'
        CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
        RETURN
    ENDIF

    ! Determine if we have 3D data
    Is3D = ASSOCIATED(Arr3D)

    ! Check if this is a curvilinear grid by comparing dimensions of lon/lat arrays
    ! If XMID and YMID are 2D arrays with matching dimensions to the grid, it's curvilinear
    IsCurvilinear = .FALSE.
    IF (ASSOCIATED(HcoState%Grid%XMID%Val) .AND. ASSOCIATED(HcoState%Grid%YMID%Val)) THEN
        IF (SIZE(HcoState%Grid%XMID%Val, 2) > 1 .AND. SIZE(HcoState%Grid%YMID%Val, 1) > 1) THEN
            IsCurvilinear = .TRUE.

            IF ( HcoState%Config%doVerbose ) THEN
                MSG = 'Detected curvilinear grid - will write 2D coordinate variables'
                CALL HCO_MSG(MSG, LUN=HcoState%Config%hcoLogLUN)
            ENDIF
        ENDIF
    ENDIF

    ! Create current time stamps
    CALL HcoClock_Get(HcoState%Clock, YYYY, MM, DD, h, m, s, RC)
    IF (RC /= HCO_SUCCESS) THEN
        CALL HCO_ERROR('Failed to get clock time', RC, THISLOC=LOC)
        RETURN
    ENDIF

    ! Inherit precision from HEMCO
    Prc = HP

    ! Define grid dimensions
    nLon  = HcoState%NX
    nLat  = HcoState%NY
    nLev  = HcoState%NZ
    nTime = 1

    ! Don't define level dimension if this is 2D data
    NoLevDim = .NOT. Is3D

    ! Initialize output arrays
    IF (Is3D) THEN
        ALLOCATE(OutArr4D(nlon, nlat, nlev, ntime))
        OutArr4D = 0.0_sp

        ! Copy data
        OutArr4D(:,:,:,1) = Arr3D(:,:,:)
    ELSE
        ALLOCATE(OutArr2D(nlon, nlat))
        ALLOCATE(OutArr3D(nlon, nlat, ntime))
        OutArr2D = 0.0_sp
        OutArr3D = 0.0_sp

        ! Copy data
        OutArr2D(:,:) = Arr2D(:,:)
        OutArr3D(:,:,1) = Arr2D(:,:)
    ENDIF

    ! Verbose output
    IF (HcoState%Config%doVerbose) THEN
        MSG = 'Writing to file: ' // TRIM(FileName)
        CALL HCO_MSG(MSG, LUN=HcoState%Config%hcoLogLUN)
    ENDIF

    !-----------------------------------------------------------------
    ! Create output file
    !-----------------------------------------------------------------

    ! Define a variable for the number of levels, which will either be -1
    ! (if all 2D data) or the number of levels in the grid (for 3D data).
    IF (NoLevDim) THEN
        nLevTmp = -1
    ELSE
        nLevTmp = nLev
    ENDIF

    ! Create output file
    CALL NC_Create(NcFile     = FileName,                        &
                   Title      = DiagnName,                       &
                   Reference  = DefReference,                    &
                   Contact    = DefContact,                      &
                   nLon       = nLon,                            &
                   nLat       = nLat,                            &
                   nLev       = nLevTmp,                         &
                   nTime      = NF_UNLIMITED,                    &
                   fId        = fId,                             &
                   lonId      = lonId,                           &
                   latId      = latId,                           &
                   levId      = levId,                           &
                   timeId     = timeId,                          &
                   VarCt      = VarCt,                           &
                   CREATE_NC4 = .TRUE.                          )

    !-----------------------------------------------------------------
    ! Write grid dimensions
    !-----------------------------------------------------------------

    IF (IsCurvilinear) THEN
        !-----------------------------------------------------------------
        ! For curvilinear grids, write 2D coordinate variables
        !-----------------------------------------------------------------

        ! Allocate 2D arrays for longitude and latitude
        ALLOCATE(Arr2Dlon(nLon,nLat))
        ALLOCATE(Arr2Dlat(nLon,nLat))

        ! Copy data from the grid
        Arr2Dlon = HcoState%Grid%XMID%Val
        Arr2Dlat = HcoState%Grid%YMID%Val

        ! Write longitude as a 2D field
        CALL NC_Var_Def(fId         = fId,                          &
                        lonId       = lonId,                        &
                        latId       = latId,                        &
                        levId       = -1,                           &
                        timeId      = -1,                           &
                        VarName     = 'lon',                        &
                        VarLongName = 'Longitude',                  &
                        VarUnit     = 'degrees_east',               &
                        DataType    = sp,                           &
                        VarCt       = VarCt,                        &
                        Compress    = .TRUE.                       )
        CALL NC_Var_Write(fId, 'lon', Arr2D=Arr2Dlon)

        ! Write latitude as a 2D field
        CALL NC_Var_Def(fId         = fId,                          &
                        lonId       = lonId,                        &
                        latId       = latId,                        &
                        levId       = -1,                           &
                        timeId      = -1,                           &
                        VarName     = 'lat',                        &
                        VarLongName = 'Latitude',                   &
                        VarUnit     = 'degrees_north',              &
                        DataType    = sp,                           &
                        VarCt       = VarCt,                        &
                        Compress    = .TRUE.                       )
        CALL NC_Var_Write(fId, 'lat', Arr2D=Arr2Dlat)

        ! Deallocate temporary arrays
        DEALLOCATE(Arr2Dlon)
        DEALLOCATE(Arr2Dlat)
    ELSE
        !-----------------------------------------------------------------
        ! For rectilinear grids, write 1D coordinate variables
        !-----------------------------------------------------------------

        ! Write longitude axis
        CALL NC_Var_Def(fId         = fId,                          &
                        lonId       = lonId,                        &
                        latId       = -1,                           &
                        levId       = -1,                           &
                        timeId      = -1,                           &
                        VarName     = 'lon',                        &
                        VarLongName = 'Longitude',                  &
                        VarUnit     = 'degrees_east',               &
                        Axis        = 'X',                          &
                        DataType    = dp,                           &
                        VarCt       = VarCt,                        &
                        Compress    = .TRUE.                       )
        ALLOCATE(Arr1D(nLon))
        Arr1D = HcoState%Grid%XMID%Val(:,1)
        CALL NC_Var_Write(fId, 'lon', Arr1D=Arr1D)
        DEALLOCATE(Arr1D)

        ! Write latitude axis
        CALL NC_Var_Def(fId         = fId,                          &
                        lonId       = -1,                           &
                        latId       = latId,                        &
                        levId       = -1,                           &
                        timeId      = -1,                           &
                        VarName     = 'lat',                        &
                        VarLongName = 'Latitude',                   &
                        VarUnit     = 'degrees_north',              &
                        Axis        = 'Y',                          &
                        DataType    = dp,                           &
                        VarCt       = VarCt,                        &
                        Compress    = .TRUE.                       )
        ALLOCATE(Arr1D(nLat))
        Arr1D = HcoState%Grid%YMID%Val(1,:)
        CALL NC_Var_Write(fId, 'lat', Arr1D=Arr1D)
        DEALLOCATE(Arr1D)
    ENDIF

    ! Write vertical grid parameters (if 3D data)
    IF (.NOT. NoLevDim) THEN
        ! Reference pressure [Pa]
        P0 = 1.0e+05_dp

        ! Allocate vertical coordinate arrays
        ALLOCATE(Arr1D(nLev))
        ALLOCATE(hyam(nLev))
        ALLOCATE(hybm(nLev))

        ! Construct vertical level coordinates
        DO L = 1, nLev
            ! A parameter at grid midpoints
            hyam(L) = (HcoState%Grid%zGrid%Ap(L) + &
                       HcoState%Grid%zGrid%Ap(L+1)) * 0.5_dp

            ! B parameter at grid midpoints
            hybm(L) = (HcoState%Grid%zGrid%Bp(L) + &
                       HcoState%Grid%zGrid%Bp(L+1)) * 0.5_dp

            ! Vertical level coordinate
            Arr1d(L) = (hyam(L) / P0) + hybm(L)
        ENDDO

        ! Write level axis
        myLName = 'hybrid level at midpoints ((A/P0)+B)'
        mySName = 'atmosphere_hybrid_sigma_pressure_coordinate'
        myFTerm = 'a: hyai b: hybi p0: P0 ps: PS'
        CALL NC_Var_Def(fId          = fId,                     &
                       lonId        = -1,                       &
                       latId        = -1,                       &
                       levId        = levId,                    &
                       timeId       = -1,                       &
                       VarName      = 'lev',                    &
                       VarLongName  = myLName,                  &
                       StandardName = mySName,                  &
                       FormulaTerms = myFTerm,                  &
                       VarUnit      = 'level',                  &
                       Axis         = 'Z',                      &
                       Positive     = 'up',                     &
                       DataType     = dp,                       &
                       VarCt        = VarCt,                    &
                       Compress     = .TRUE.                   )
        CALL NC_Var_Write(fId, 'lev', Arr1D=Arr1D)

        ! Write hybrid A coordinate
        myLName = 'hybrid A coefficient at layer midpoints'
        CALL NC_Var_Def(fId          = fId,                     &
                       lonId        = -1,                       &
                       latId        = -1,                       &
                       levId        = levId,                    &
                       timeId       = -1,                       &
                       VarName      = 'hyam',                   &
                       VarLongName  = myLName,                  &
                       VarUnit      = 'Pa',                     &
                       DataType     = dp,                       &
                       VarCt        = VarCt,                    &
                       Compress     = .TRUE.                   )
        CALL NC_Var_Write(fId, 'hyam', Arr1D=hyam)

        ! Write hybrid B coordinate
        myLName = 'hybrid B coefficient at layer midpoints'
        CALL NC_Var_Def(fId          = fId,                     &
                       lonId        = -1,                       &
                       latId        = -1,                       &
                       levId        = levId,                    &
                       timeId       = -1,                       &
                       VarName      = 'hybm',                   &
                       VarLongName  = myLName,                  &
                       VarUnit      = '1',                      &
                       DataType     = dp,                       &
                       VarCt        = VarCt,                    &
                       Compress     = .TRUE.                   )
        CALL NC_Var_Write(fId, 'hybm', Arr1D=hybm)

        ! Write reference pressure
        CALL NC_Var_Def(fId         = fId,                      &
                       lonId       = -1,                        &
                       latId       = -1,                        &
                       levId       = -1,                        &
                       timeId      = -1,                        &
                       VarName     = 'P0',                      &
                       VarLongName = 'Reference pressure',      &
                       VarUnit     = 'Pa',                      &
                       DataType    = dp,                        &
                       VarCt       = VarCt,                     &
                       Compress    = .FALSE.                   )
        CALL NC_Var_Write(fId, 'P0', P0)

        ! Deallocate arrays
        DEALLOCATE(Arr1d)
        DEALLOCATE(hyam)
        DEALLOCATE(hybm)
    ENDIF

    !-----------------------------------------------------------------
    ! Write time dimension
    !-----------------------------------------------------------------

    ! Time formatting
    GMT     = REAL(h,dp) + (REAL(m,dp)/60.0_dp) + (REAL(s,dp)/3600.0_dp)
    THISDAY = DD + (GMT / 24.0_dp)
    JD1     = JULDAY(YYYY, MM, THISDAY)

    ! Standard time format
    WRITE(timeunit, 100) YYYY, MM, DD, h, m, s
100 FORMAT('hours since ',i4.4,'-',i2.2,'-',i2.2,' ',i2.2,':',i2.2,':',i2.2,' GMT')

    ALLOCATE(nctime(ntime))
    nctime(1) = 0.0_sp  ! Start at 0 since we use current time as reference

    ! Write time dimension
    CALL NC_Var_Def(fId         = fId,                          &
                   lonId       = -1,                            &
                   latId       = -1,                            &
                   levId       = -1,                            &
                   timeId      = timeId,                        &
                   VarName     = 'time',                        &
                   VarLongName = 'Time',                        &
                   VarUnit     = timeUnit,                      &
                   Axis        = 'T',                           &
                   Calendar    = 'gregorian',                   &
                   DataType    = 8,                             &
                   VarCt       = VarCt,                         &
                   Compress    = .TRUE.                        )
    CALL NC_VAR_WRITE(fId, 'time', Arr1D=nctime)
    DEALLOCATE(nctime)

    !-----------------------------------------------------------------
    ! Write grid box areas
    !-----------------------------------------------------------------
    CALL NC_Var_Def(fId         = fId,                          &
                   lonId       = lonId,                         &
                   latId       = latId,                         &
                   levId       = -1,                            &
                   timeId      = -1,                            &
                   VarName     = 'AREA',                        &
                   VarLongName = 'Grid box area',               &
                   VarUnit     = 'm2',                          &
                   DataType    = Prc,                           &
                   VarCt       = VarCt,                         &
                   Compress    = .TRUE.                        )
    CALL NC_Var_Write(fId, 'AREA', Arr2D=HcoState%Grid%Area_M2%Val)

    !-----------------------------------------------------------------
    ! Define and write data variable
    !-----------------------------------------------------------------

    ! Close define mode
    CALL NcEnd_Def(fId)

    ! Define level ID based on dimensionality
    IF (Is3D) THEN
        levIdTmp = levId
    ELSE
        levIdTmp = -1
    ENDIF

    ! Define variable
    IF (IsCurvilinear) THEN
        ! For curvilinear grid, use standard approach but add coordinate attribute later
        CALL NC_Var_Def(fId          = fId,                         &
                       lonId        = lonId,                        &
                       latId        = latId,                        &
                       levId        = levIdTmp,                     &
                       timeId       = timeId,                       &
                       VarName      = TRIM(DiagnName),              &
                       VarLongName  = TRIM(DiagnName),              &
                       VarUnit      = 'unknown',                    &
                       MissingValue = FillValue,                    &
                       DataType     = sp,                           &
                       VarCt        = VarCt,                        &
                       DefMode      = .FALSE.,                      &
                       Compress     = .TRUE.                       )

        ! For curvilinear grids, add coordinates attribute after defining the variable
        ! This needs to be done manually since the NC_Var_Def interface doesn't support it
        ! CALL NcDef_Var_Attributes(fId, TRIM(DiagnName), "coordinates", "lon lat")
    ELSE
        CALL NC_Var_DEF(fId          = fId,                         &
                       lonId        = lonId,                        &
                       latId        = latId,                        &
                       levId        = levIdTmp,                     &
                       timeId       = timeId,                       &
                       VarName      = TRIM(DiagnName),              &
                       VarLongName  = TRIM(DiagnName),              &
                       VarUnit      = 'unknown',                    &
                       MissingValue = FillValue,                    &
                       DataType     = sp,                           &
                       VarCt        = VarCt,                        &
                       DefMode      = .FALSE.,                      &
                       Compress     = .TRUE.                       )
    ENDIF

    ! Write data
    IF (Is3D) THEN
        CALL NC_VAR_WRITE(fId, TRIM(DiagnName), Arr4D=OutArr4D)
    ELSE
        CALL NC_VAR_WRITE(fId, TRIM(DiagnName), Arr3D=OutArr3D)
    ENDIF

    ! Add counter variable if needed (for diagnostics output)
    IF (COUNTER > 0) THEN
        CALL NC_Var_Def(fId         = fId,                      &
                       lonId       = -1,                        &
                       latId       = -1,                        &
                       levId       = -1,                        &
                       timeId      = -1,                        &
                       VarName     = 'COUNTER',                 &
                       VarLongName = 'Diagnostics counter',     &
                       VarUnit     = '1',                       &
                       DataType    = 4,                         &
                       VarCt       = VarCt,                     &
                       DefMode     = .FALSE.,                   &
                       Compress    = .TRUE.                    )
        CALL NC_VAR_WRITE(fId, 'COUNTER', COUNTER)
    ENDIF

    !-----------------------------------------------------------------
    ! Cleanup
    !-----------------------------------------------------------------

    ! Close file
    CALL NC_CLOSE(fId)

    ! Free memory
    IF (ASSOCIATED(OutArr2D)) DEALLOCATE(OutArr2D)
    IF (ASSOCIATED(OutArr3D)) DEALLOCATE(OutArr3D)
    IF (ASSOCIATED(OutArr4D)) DEALLOCATE(OutArr4D)

    ! Success
    RC = HCO_SUCCESS

  END SUBROUTINE WriteToFile

  !=========================================================================
  ! Implementation of NcDef_Var_Attributes for setting variable attributes
  !=========================================================================
  SUBROUTINE NcDef_Var_Attributes(ncid, varname, attr_name, attr_value)
    USE HCO_m_netCDF_io_handle_err
    include "netcdf.inc"

    INTEGER, INTENT(IN)          :: ncid
    CHARACTER(LEN=*), INTENT(IN) :: varname
    CHARACTER(LEN=*), INTENT(IN) :: attr_name
    CHARACTER(LEN=*), INTENT(IN) :: attr_value

    INTEGER :: varid, status

    ! Get the variable ID
    status = nf_inq_varid(ncid, varname, varid)
    IF (status /= NF_NOERR) CALL handle_err(status)

    ! Add the attribute
    status = nf_put_att_text(ncid, varid, attr_name, LEN_TRIM(attr_value), attr_value)
    IF (status /= NF_NOERR) CALL handle_err(status)
  END SUBROUTINE NcDef_Var_Attributes
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOIO_WriteFieldESMF
!
! !DESCRIPTION: Subroutine HCOIO\_WriteFieldESMF writes HEMCO diagnostic data
!  to NetCDF files using ESMF_FieldWrite, which automatically provides
!  CF-compliant output with proper coordinate variables and metadata.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOIO_WriteFieldESMF( HcoState, FieldName, FileName, &
                                   Arr2D, Arr3D, VarUnit, VarLongName, RC )
!
! !USES:
!
    USE ESMF
    USE HCO_FileData_Mod, ONLY : FileData_CreateESMFGrid
!
! !INPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER        :: HcoState     ! HEMCO state object
    CHARACTER(LEN=*), INTENT(IN)     :: FieldName    ! Field name
    CHARACTER(LEN=*), INTENT(IN)     :: FileName     ! Output filename
    REAL(sp),         POINTER        :: Arr2D(:,:)   ! 2D data (optional)
    REAL(sp),         POINTER        :: Arr3D(:,:,:) ! 3D data (optional)
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: VarUnit      ! Variable units
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: VarLongName  ! Variable long name
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT)  :: RC           ! Return code
!
! !REVISION HISTORY:
!  01 Jul 2025 - Initial version using ESMF_FieldWrite
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    TYPE(ESMF_Grid)              :: Grid
    TYPE(ESMF_Field)             :: Field
    REAL(ESMF_KIND_R8), POINTER  :: FieldPtr2D(:,:)
    REAL(ESMF_KIND_R8), POINTER  :: FieldPtr3D(:,:,:)
    REAL(ESMF_KIND_R8), ALLOCATABLE :: lonCenters(:), latCenters(:)
    CHARACTER(LEN=255)           :: MSG, LOC
    CHARACTER(LEN=255)           :: Units, LongName
    INTEGER                      :: nLon, nLat, nLev
    INTEGER                      :: I, J, K
    LOGICAL                      :: Is3D

    !=================================================================
    ! HCOIO_WriteFieldESMF begins here!
    !=================================================================

    LOC = 'HCOIO_WriteFieldESMF (hcoio_write_esmf_mod.F90)'
    RC = HCO_SUCCESS

    ! Check input data
    IF (.NOT. ASSOCIATED(Arr2D) .AND. .NOT. ASSOCIATED(Arr3D)) THEN
        MSG = 'Both Arr2D and Arr3D are NULL - nothing to write'
        CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
        RETURN
    ENDIF

    Is3D = ASSOCIATED(Arr3D)

    ! Get grid dimensions
    nLon = HcoState%NX
    nLat = HcoState%NY
    IF (Is3D) THEN
        nLev = HcoState%NZ
    ELSE
        nLev = 1
    ENDIF

    ! Set default metadata
    IF (PRESENT(VarUnit)) THEN
        Units = TRIM(VarUnit)
    ELSE
        Units = 'unknown'
    ENDIF

    IF (PRESENT(VarLongName)) THEN
        LongName = TRIM(VarLongName)
    ELSE
        LongName = TRIM(FieldName)
    ENDIF

    ! Create coordinate arrays for ESMF grid
    ALLOCATE(lonCenters(nLon), latCenters(nLat))

    ! Extract coordinate centers from HEMCO grid
    DO I = 1, nLon
        lonCenters(I) = HcoState%Grid%XMID%Val(I,1)
    ENDDO
    DO J = 1, nLat
        latCenters(J) = HcoState%Grid%YMID%Val(1,J)
    ENDDO

    ! Create ESMF grid
    IF (Is3D) THEN
        Grid = ESMF_GridCreateNoPeriDim( &
            maxIndex=(/nLon, nLat, nLev/), &
            coordSys=ESMF_COORDSYS_SPH_DEG, &
            indexflag=ESMF_INDEX_GLOBAL, &
            rc=RC)
    ELSE
        Grid = ESMF_GridCreateNoPeriDim( &
            maxIndex=(/nLon, nLat/), &
            coordSys=ESMF_COORDSYS_SPH_DEG, &
            indexflag=ESMF_INDEX_GLOBAL, &
            rc=RC)
    ENDIF

    IF (RC /= ESMF_SUCCESS) THEN
        MSG = 'Failed to create ESMF grid'
        CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
        DEALLOCATE(lonCenters, latCenters)
        RETURN
    ENDIF

    ! Add coordinates to grid
    CALL ESMF_GridAddCoord(Grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=RC)
    IF (RC /= ESMF_SUCCESS) THEN
        MSG = 'Failed to add coordinates to grid'
        CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
        CALL ESMF_GridDestroy(Grid, rc=RC)
        DEALLOCATE(lonCenters, latCenters)
        RETURN
    ENDIF

    ! Create ESMF field
    Field = ESMF_FieldCreate(Grid, ESMF_TYPEKIND_R8, &
                            staggerloc=ESMF_STAGGERLOC_CENTER, &
                            name=TRIM(FieldName), rc=RC)
    IF (RC /= ESMF_SUCCESS) THEN
        MSG = 'Failed to create ESMF field'
        CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
        CALL ESMF_GridDestroy(Grid, rc=RC)
        DEALLOCATE(lonCenters, latCenters)
        RETURN
    ENDIF

    ! Set field attributes
    CALL ESMF_AttributeSet(Field, 'units', TRIM(Units), rc=RC)
    IF (RC /= ESMF_SUCCESS) THEN
        MSG = 'Failed to set units attribute'
        CALL HCO_WARNING(MSG, THISLOC=LOC)
    ENDIF

    CALL ESMF_AttributeSet(Field, 'long_name', TRIM(LongName), rc=RC)
    IF (RC /= ESMF_SUCCESS) THEN
        MSG = 'Failed to set long_name attribute'
        CALL HCO_WARNING(MSG, THISLOC=LOC)
    ENDIF

    ! Get pointer to field data and copy HEMCO data
    IF (Is3D) THEN
        CALL ESMF_FieldGet(Field, farrayPtr=FieldPtr3D, rc=RC)
        IF (RC /= ESMF_SUCCESS) THEN
            MSG = 'Failed to get 3D field pointer'
            CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
            CALL ESMF_FieldDestroy(Field, rc=RC)
            CALL ESMF_GridDestroy(Grid, rc=RC)
            DEALLOCATE(lonCenters, latCenters)
            RETURN
        ENDIF

        ! Copy data
        DO K = 1, SIZE(Arr3D,3)
            DO J = 1, SIZE(Arr3D,2)
                DO I = 1, SIZE(Arr3D,1)
                    FieldPtr3D(I,J,K) = REAL(Arr3D(I,J,K), ESMF_KIND_R8)
                ENDDO
            ENDDO
        ENDDO
    ELSE
        CALL ESMF_FieldGet(Field, farrayPtr=FieldPtr2D, rc=RC)
        IF (RC /= ESMF_SUCCESS) THEN
            MSG = 'Failed to get 2D field pointer'
            CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
            CALL ESMF_FieldDestroy(Field, rc=RC)
            CALL ESMF_GridDestroy(Grid, rc=RC)
            DEALLOCATE(lonCenters, latCenters)
            RETURN
        ENDIF

        ! Copy data
        DO J = 1, SIZE(Arr2D,2)
            DO I = 1, SIZE(Arr2D,1)
                FieldPtr2D(I,J) = REAL(Arr2D(I,J), ESMF_KIND_R8)
            ENDDO
        ENDDO
    ENDIF

    ! Write field to NetCDF file using ESMF
    ! This automatically creates CF-compliant output
    CALL ESMF_FieldWrite(Field, TRIM(FileName), rc=RC)
    IF (RC /= ESMF_SUCCESS) THEN
        MSG = 'Failed to write field to file: ' // TRIM(FileName)
        CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
        CALL ESMF_FieldDestroy(Field, rc=RC)
        CALL ESMF_GridDestroy(Grid, rc=RC)
        DEALLOCATE(lonCenters, latCenters)
        RETURN
    ENDIF

    ! Verbose output
    IF (HcoState%Config%doVerbose) THEN
        MSG = 'Successfully wrote ESMF field to: ' // TRIM(FileName)
        CALL HCO_MSG(MSG, LUN=HcoState%Config%hcoLogLUN)
    ENDIF

    ! Cleanup
    CALL ESMF_FieldDestroy(Field, rc=RC)
    CALL ESMF_GridDestroy(Grid, rc=RC)
    DEALLOCATE(lonCenters, latCenters)

    RC = HCO_SUCCESS

  END SUBROUTINE HCOIO_WriteFieldESMF
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: CreateESMFFieldFromData
!
! !DESCRIPTION: Helper routine to create an ESMF field from HEMCO data arrays.
!  This routine handles the grid creation and field initialization.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE CreateESMFFieldFromData( HcoState, FieldName, Arr2D, Arr3D, &
                                      Field, RC )
!
! !USES:
!
    USE ESMF
!
! !INPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER        :: HcoState     ! HEMCO state object
    CHARACTER(LEN=*), INTENT(IN)     :: FieldName    ! Field name
    REAL(sp),         POINTER        :: Arr2D(:,:)   ! 2D data (optional)
    REAL(sp),         POINTER        :: Arr3D(:,:,:) ! 3D data (optional)
!
! !OUTPUT PARAMETERS:
!
    TYPE(ESMF_Field), INTENT(OUT)    :: Field        ! ESMF field
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT)  :: RC           ! Return code
!
! !REVISION HISTORY:
!  01 Jul 2025 - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    TYPE(ESMF_Grid)              :: Grid
    REAL(ESMF_KIND_R8), POINTER  :: FieldPtr2D(:,:)
    REAL(ESMF_KIND_R8), POINTER  :: FieldPtr3D(:,:,:)
    CHARACTER(LEN=255)           :: MSG, LOC
    INTEGER                      :: nLon, nLat, nLev
    INTEGER                      :: I, J, K
    LOGICAL                      :: Is3D

    !=================================================================
    ! CreateESMFFieldFromData begins here!
    !=================================================================

    LOC = 'CreateESMFFieldFromData (hcoio_write_esmf_mod.F90)'
    RC = HCO_SUCCESS

    ! Check input data
    IF (.NOT. ASSOCIATED(Arr2D) .AND. .NOT. ASSOCIATED(Arr3D)) THEN
        MSG = 'Both Arr2D and Arr3D are NULL'
        CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
        RETURN
    ENDIF

    Is3D = ASSOCIATED(Arr3D)

    ! Get grid dimensions
    nLon = HcoState%NX
    nLat = HcoState%NY
    IF (Is3D) THEN
        nLev = HcoState%NZ
    ENDIF

    ! Create ESMF grid based on HEMCO grid
    ! For now, create a simple rectilinear grid
    IF (Is3D) THEN
        Grid = ESMF_GridCreateNoPeriDim( &
            maxIndex=(/nLon, nLat, nLev/), &
            coordSys=ESMF_COORDSYS_SPH_DEG, &
            indexflag=ESMF_INDEX_GLOBAL, &
            rc=RC)
    ELSE
        Grid = ESMF_GridCreateNoPeriDim( &
            maxIndex=(/nLon, nLat/), &
            coordSys=ESMF_COORDSYS_SPH_DEG, &
            indexflag=ESMF_INDEX_GLOBAL, &
            rc=RC)
    ENDIF

    IF (RC /= ESMF_SUCCESS) THEN
        MSG = 'Failed to create ESMF grid'
        CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
        RETURN
    ENDIF

    ! Create ESMF field
    Field = ESMF_FieldCreate(Grid, ESMF_TYPEKIND_R8, &
                            staggerloc=ESMF_STAGGERLOC_CENTER, &
                            name=TRIM(FieldName), rc=RC)
    IF (RC /= ESMF_SUCCESS) THEN
        MSG = 'Failed to create ESMF field'
        CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
        CALL ESMF_GridDestroy(Grid, rc=RC)
        RETURN
    ENDIF

    ! Get pointer to field data and copy HEMCO data
    IF (Is3D) THEN
        CALL ESMF_FieldGet(Field, farrayPtr=FieldPtr3D, rc=RC)
        IF (RC /= ESMF_SUCCESS) THEN
            MSG = 'Failed to get 3D field pointer'
            CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
            CALL ESMF_FieldDestroy(Field, rc=RC)
            CALL ESMF_GridDestroy(Grid, rc=RC)
            RETURN
        ENDIF

        ! Copy data
        DO K = 1, SIZE(Arr3D,3)
            DO J = 1, SIZE(Arr3D,2)
                DO I = 1, SIZE(Arr3D,1)
                    FieldPtr3D(I,J,K) = REAL(Arr3D(I,J,K), ESMF_KIND_R8)
                ENDDO
            ENDDO
        ENDDO
    ELSE
        CALL ESMF_FieldGet(Field, farrayPtr=FieldPtr2D, rc=RC)
        IF (RC /= ESMF_SUCCESS) THEN
            MSG = 'Failed to get 2D field pointer'
            CALL HCO_ERROR(MSG, RC, THISLOC=LOC)
            CALL ESMF_FieldDestroy(Field, rc=RC)
            CALL ESMF_GridDestroy(Grid, rc=RC)
            RETURN
        ENDIF

        ! Copy data
        DO J = 1, SIZE(Arr2D,2)
            DO I = 1, SIZE(Arr2D,1)
                FieldPtr2D(I,J) = REAL(Arr2D(I,J), ESMF_KIND_R8)
            ENDDO
        ENDDO
    ENDIF

    RC = HCO_SUCCESS

  END SUBROUTINE CreateESMFFieldFromData
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ConstructTimeStamp
!
! !DESCRIPTION: Subroutine ConstructTimeStamp is a helper routine to construct
! the time stamp of a given diagnostics collection.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE ConstructTimeStamp ( HcoState, PS, PrevTime, Yr, Mt, Dy, hr, mn, RC )
!
! !USES:
!
    USE HCO_State_Mod,       ONLY : HCO_State
    USE HCO_Clock_Mod
    USE HCO_JULDAY_MOD
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(HCO_State), POINTER          :: HcoState     ! HEMCO state obj
    INTEGER,         INTENT(IN   )    :: PS           ! collecion ID
    LOGICAL,         INTENT(IN   )    :: PrevTime     ! Use previous time?
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,         INTENT(INOUT)    :: RC           ! Return code
!
! !OUTPUT PARAMETERS:
!
    INTEGER,         INTENT(  OUT)    :: Yr
    INTEGER,         INTENT(  OUT)    :: Mt
    INTEGER,         INTENT(  OUT)    :: Dy
    INTEGER,         INTENT(  OUT)    :: hr
    INTEGER,         INTENT(  OUT)    :: mn
!
! !REVISION HISTORY:
!  06 Nov 2015 - C. Keller   - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER            :: Y2, M2, D2, h2, n2, s2
    INTEGER            :: Y1, M1, D1, h1, n1, s1
    INTEGER            :: LastYMD, LastHMS
    INTEGER            :: YYYYMMDD, HHMMSS
    INTEGER            :: OutTimeStamp
    REAL(dp)           :: DAY, UTC, JD1, JD2, JDMID
    CHARACTER(LEN=255) :: MSG
    CHARACTER(LEN=255) :: LOC = 'ConstuctTimeStamp (hcoio_write_esmf_mod.F90)'

    !=================================================================
    ! ConstructTimeStamp begins here!
    !=================================================================

    ! Use HEMCO clock to create timestamp used in filename. Use previous
    ! time step if this option is selected.
    IF ( .NOT. PrevTime ) THEN
       ! Use current time - just pass the parameters we need
       ! The HcoClock_Get routine has optional parameters, so we only need to include
       ! the ones we actually want to retrieve
       CALL HcoClock_Get(HcoState%Clock, cYYYY=Y2, cMM=M2, cDD=D2, &
                         cH=h2, cM=n2, cS=s2, RC=RC)
       IF ( RC /= HCO_SUCCESS ) THEN
           CALL HCO_ERROR( 'ERROR 8', RC, THISLOC=LOC )
           RETURN
       ENDIF
    ELSE
       ! Use previous time
       ! Get parameters for the previous timestamp instead of current
       CALL HcoClock_Get(HcoState%Clock, pYYYY=Y2, pMM=M2, pDD=D2, &
                         pH=h2, pM=n2, pS=s2, RC=RC)
       IF ( RC /= HCO_SUCCESS ) THEN
           CALL HCO_ERROR( 'ERROR 9', RC, THISLOC=LOC )
           RETURN
       ENDIF
    ENDIF

    ! Get timestamp location for this collection
    CALL DiagnCollection_Get( HcoState%Diagn, PS, OutTimeStamp=OutTimeStamp, &
                              LastYMD=LastYMD, LastHMS=LastHMS, RC=RC )
    IF ( RC /= HCO_SUCCESS ) THEN
        CALL HCO_ERROR( 'ERROR 10', RC, THISLOC=LOC )
        RETURN
    ENDIF

    ! Determine dates to be used:

    ! To use start date
    IF ( OutTimeStamp == HcoDiagnStart ) THEN
       Yr = FLOOR( MOD(LastYMD*1.d0, 100000000.d0 ) / 1.0d4 )
       Mt = FLOOR( MOD(LastYMD*1.d0, 10000.d0     ) / 1.0d2 )
       Dy = FLOOR( MOD(LastYMD*1.d0, 100.d0       ) / 1.0d0 )
       Hr = FLOOR( MOD(LastHMS*1.d0, 1000000.d0   ) / 1.0d4 )
       Mn = FLOOR( MOD(LastHMS*1.d0, 10000.d0     ) / 1.0d2 )

    ! Use mid point
    ELSEIF ( OutTimeStamp == HcoDiagnMid ) THEN

       ! Julian day of start interval:
       Y1 = FLOOR( MOD(LastYMD*1.d0, 100000000.d0 ) / 1.0d4 )
       M1 = FLOOR( MOD(LastYMD*1.d0, 10000.d0     ) / 1.0d2 )
       D1 = FLOOR( MOD(LastYMD*1.d0, 100.d0       ) / 1.0d0 )
       h1 = FLOOR( MOD(LastHMS*1.d0, 1000000.d0   ) / 1.0d4 )
       n1 = FLOOR( MOD(LastHMS*1.d0, 10000.d0     ) / 1.0d2 )
       s1 = FLOOR( MOD(LastHMS*1.d0, 100.d0       ) / 1.0d0 )

       UTC = ( REAL(h1,dp) / 24.0_dp    ) + &
             ( REAL(n1,dp) / 1440.0_dp  ) + &
             ( REAL(s1,dp) / 86400.0_dp )
       DAY = REAL(D1,dp) + UTC
       JD1 = JULDAY( Y1, M1, DAY )

       ! Julian day of end interval:
       UTC = ( REAL(h2,dp) / 24.0_dp    ) + &
             ( REAL(n2,dp) / 1440.0_dp  ) + &
             ( REAL(s2,dp) / 86400.0_dp )
       DAY = REAL(D2,dp) + UTC
       JD2 = JULDAY( Y2, M2, DAY )

       ! Julian day in the middle
       JDMID = ( JD1 + JD2 ) / 2.0_dp

       ! Tranlate back into dates
       CALL CALDATE( JDMID, YYYYMMDD, HHMMSS )
       Yr = FLOOR ( MOD( YYYYMMDD, 100000000) / 1.0e4_dp )
       Mt = FLOOR ( MOD( YYYYMMDD, 10000    ) / 1.0e2_dp )
       Dy = FLOOR ( MOD( YYYYMMDD, 100      ) / 1.0e0_dp )
       Hr = FLOOR ( MOD(   HHMMSS, 1000000  ) / 1.0e4_dp )
       Mn = FLOOR ( MOD(   HHMMSS, 10000    ) / 1.0e2_dp )

    ! Otherwise, use end date
    ELSE
       Yr = Y2
       Mt = M2
       Dy = D2
       Hr = h2
       Mn = n2
    ENDIF

    ! Return w/ success
    RC = HCO_SUCCESS

  END SUBROUTINE ConstructTimeStamp
!EOC
END MODULE HCOIO_Write_Mod
#endif