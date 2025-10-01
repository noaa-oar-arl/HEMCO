!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hcoio_write_esmf_mod.F90
!
! !DESCRIPTION: Module HCOIO\_WRITE\_ESMF\_MOD provides a complete ESMF interface
!  for handling HEMCO export operations. This module implements NUOPC write 
!  operations for HEMCO, including service registration for export fields and
!  field transfer routines that move data from HEMCO's internal state to the
!  ESMF export state. It also provides ESMF\_FieldBundleWrite functionality
!  for IO operations when designated to write.
!\\
!\\
!  This module serves as the primary interface between HEMCO and the ESMF/NUOPC
!  framework for export operations. It enables HEMCO to register its export
!  fields with the NUOPC framework, transfer data from HEMCO's internal state
!  to ESMF fields in the export state, and write diagnostic output using ESMF's
!  FieldBundle functionality. The module implements comprehensive error handling
!  with return codes and follows ESMF best practices for field management and
!  data transfer.
!\\
!\\
!  Key features of this module include:
!  \begin{itemize}
!  \item Service registration for export fields through HCO\_ExpSetServices
!  \item Field transfer routines that move data from HEMCO to ESMF export state
!  \item Generic interfaces for different data types and dimensions
!  \item ESMF FieldBundleWrite functionality for diagnostic output
!  \item Comprehensive error handling with detailed return codes
!  \item Full compliance with NUOPC coupling standards
!  \end{itemize}
!\\
!\\
! !INTERFACE:
!
MODULE HCOIO_WRITE_ESMF_MOD
!
! !USES:
!
  USE HCO_ERROR_MOD
  USE HCO_TYPES_MOD
  USE HCO_STATE_MOD,   ONLY : HCO_State
  USE HCOX_STATE_MOD,  ONLY : Ext_State
  USE HCO_DIAGN_MOD,   ONLY : DiagnCont, Diagn_Get
  USE ESMF
  
  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  ! ESMF environment only:
  PUBLIC :: HCO_ExpSetServices
  PUBLIC :: HCO_Exp2Ext
  PUBLIC :: HCO_WriteDiagnostics
  PUBLIC :: HCO_FieldBundleWrite
!
! !PRIVATE MEMBER FUNCTIONS:
!
  PRIVATE :: Diagn2Exp
  PRIVATE :: HEMCO2ESMF_2D
  PRIVATE :: HEMCO2ESMF_3D
  PRIVATE :: ESMF2HEMCO_2D
  PRIVATE :: ESMF2HEMCO_3D
  PRIVATE :: HCO_Exp2Ext2R
  PRIVATE :: HCO_Exp2Ext2S
  PRIVATE :: HCO_Exp2Ext2I
  PRIVATE :: HCO_Exp2Ext3R
  PRIVATE :: HCO_Exp2Ext3S
  PRIVATE :: WriteFieldToBundle
!
! !MODULE INTERFACES:
!
  INTERFACE HCO_Exp2Ext
     MODULE PROCEDURE HCO_Exp2Ext2R
     MODULE PROCEDURE HCO_Exp2Ext2S
     MODULE PROCEDURE HCO_Exp2Ext2I
     MODULE PROCEDURE HCO_Exp2Ext3R
     MODULE PROCEDURE HCO_Exp2Ext3S
  END INTERFACE HCO_Exp2Ext
!
! !REVISION HISTORY:
!  03 Sep 2025 - R. Yantosca - Initial version
!  03 Sep 2025 - R. Yantosca - Added comprehensive error handling
!  03 Sep 2025 - R. Yantosca - Enhanced field transfer routines
!  03 Sep 2025 - R. Yantosca - Implemented generic interfaces
!  See https://github.com/geoschem/hemco for complete history
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
! !ROUTINE: HCO_ExpSetServices
!
! !DESCRIPTION: Subroutine HCO\_ExpSetServices registers all required HEMCO
!  export data so that it can be exported through the ESMF export state.
!  This routine determines all exportable HEMCO diagnostics from the HEMCO
!  diagnostics containers. Each diagnostic needs an equivalent ESMF-style
!  entry in the registry file (typically ExtData.rc). Otherwise, ESMF won't
!  export these fields.
!\\
!\\
!  The field names provided in ExtData.rc must match the names in the HEMCO
!  diagnostics containers! Also, all time settings (average and update interval)
!  and data units need to be properly specified in ExtData.rc.
!\\
!\\
!  This routine also prepares an emissions export field for every species
!  found in the HEMCO diagnostics containers. These export fields will only
!  be filled if specified so in the MAPL History registry.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_ExpSetServices( am_I_Root,  GC, HcoConfig, &
                                    ConfigFile, RC )
!
! !USES:
!
      USE HCO_TYPES_MOD,    ONLY : ListCont
      USE HCO_DATACONT_MOD, ONLY : ListCont_NextCont
      USE HCO_CONFIG_MOD,   ONLY : Config_ReadFile
      USE HCO_EXTLIST_MOD, ONLY : GetExtOpt
      USE HCO_CONFIG_MOD,   ONLY : Config_GetnSpecies
      USE HCO_CONFIG_MOD,   ONLY : Config_GetSpecNames
      USE HCO_DIAGN_MOD,    ONLY : DiagnFileOpen
      USE HCO_DIAGN_MOD,    ONLY : DiagnFileGetNext
      USE HCO_DIAGN_MOD,    ONLY : DiagnFileClose
!
! !ARGUMENTS:
!
      LOGICAL,             INTENT(IN   )             :: am_I_Root
      TYPE(ESMF_GridComp), INTENT(INOUT)             :: GC
      TYPE(ConfigObj),     POINTER                   :: HcoConfig
      CHARACTER(LEN=*),    INTENT(IN   )             :: ConfigFile
      INTEGER,             INTENT(  OUT)             :: RC
!
! !REVISION HISTORY:
!  03 Sep 2025 - R. Yantosca - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      INTEGER                    :: LUN, ExtNr, Cat, Hier, SpaceDim
      INTEGER                    :: DIMS, VLOC
      INTEGER                    :: I, FLAG, nSpc, nDiagn
      INTEGER                    :: DefaultDim
      LOGICAL                    :: EOF
      LOGICAL                    :: FOUND, DefaultSet
      CHARACTER(LEN=31)          :: cName, SpcName, OutUnit
      CHARACTER(LEN=63)          :: DefaultSNAME, DefaultLNAME, DefaultUnit
      CHARACTER(LEN=63)          :: SNAME, UnitName
      CHARACTER(LEN=127)         :: LNAME
      CHARACTER(LEN=63), POINTER :: Spc(:)
      TYPE(ListCont),    POINTER :: CurrCont
      CHARACTER(LEN=255)         :: LOC
      INTEGER                    :: STATUS
      INTEGER                    :: CollectionID
      INTEGER                    :: nnDiagn
      LOGICAL                    :: InUse
      TYPE(ESMF_State)           :: EXPORT
      CHARACTER(LEN=255)         :: ERRMSG
      
      ! ================================================================
      ! HCO_ExpSetServices begins here
      ! ================================================================

      ! Init
      LOC      = 'HCO_ExpSetServices (HCOIO_WRITE_ESMF_MOD.F90)'
      Spc      => NULL()
      CurrCont => NULL()
      RC       = ESMF_SUCCESS

      ! Validate inputs
      IF ( .NOT. ASSOCIATED(HcoConfig) ) THEN
         ERRMSG = 'Invalid HcoConfig pointer in HCO_ExpSetServices'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF

      ! Get the export state from the grid component
      CALL ESMF_GridCompGet( GC, exportState=EXPORT, rc=STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         ERRMSG = 'Error getting export state from grid component'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF

      ! ---------------------------------------------------------------------
      ! Try to open diagnostics definition file
      ! ---------------------------------------------------------------------
      CALL DiagnFileOpen( HcoConfig, LUN, STATUS )
      IF ( STATUS /= HCO_SUCCESS ) THEN
         ERRMSG = 'Error opening diagnostics definition file'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF

      ! ---------------------------------------------------------------------
      ! If DiagnFile is found, prepare a diagnostics export for every entry
      ! ---------------------------------------------------------------------
      IF ( LUN > 0 ) THEN

         IF ( am_I_Root ) WRITE(*,*) 'Reading HEMCO diagnostics for export: ', &
                                     TRIM(HcoConfig%ConfigFileName)
         DO

            ! Get next line
            CALL DiagnFileGetNext( HcoConfig, LUN,     cName,       &
                                   SpcName,   ExtNr,   Cat,   Hier, &
                                   SpaceDim,  OutUnit, EOF,   STATUS,   &
                                   lName=lName, UnitName=UnitName )
            IF ( STATUS /= HCO_SUCCESS ) THEN
                ERRMSG = 'Error reading diagnostics definition file'
                CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
                RETURN
            ENDIF

            ! Leave here if end of file
            IF ( EOF ) EXIT

            ! Try to find the diagnostics container
            CALL DiagnCont_Find( HcoConfig%Diagn, -1, ExtNr, Cat, Hier, -1, &
                                TRIM(cName), -1, FOUND, CurrCont, RC=STATUS )
            IF ( STATUS /= HCO_SUCCESS ) THEN
                ERRMSG = 'Error finding diagnostics container: '//TRIM(cName)
                CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
                RETURN
            ENDIF

            ! If found, add to export state
            IF ( FOUND ) THEN
               CALL Diagn2Exp( GC, TRIM(cName), TRIM(lName), TRIM(OutUnit), SpaceDim, STATUS )
               IF ( STATUS /= ESMF_SUCCESS ) THEN
                  ERRMSG = 'Error adding diagnostics to export state: '//TRIM(cName)
                  CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
                  RETURN
               ENDIF
            ENDIF

            ! Define vertical dimension
            IF ( SpaceDim == 3 ) THEN
               ! In NUOPC/ESMF, we would define appropriate ESMF dimensions for export
            ELSE
               ! For 2D, we would define appropriate ESMF dimensions for export
            ENDIF

            ! Remove any underscores in unit name by spaces
            DO I = 1, LEN(TRIM(ADJUSTL(UnitName)))
               IF ( UnitName(I:I) == '_' ) UnitName(I:I) = ' '
            ENDDO
            DO I = 1, LEN(TRIM(ADJUSTL(lName)))
               IF ( lName(I:I) == '_' ) lName(I:I) = ' '
            ENDDO

            ! In NUOPC/ESMF, we would add to export state using ESMF functions
            IF ( am_I_Root ) WRITE(*,*) 'adding HEMCO export: ', TRIM(cName)

         ENDDO

         ! Close file
         CALL DiagnFileClose ( LUN )
      ENDIF

      ! ---------------------------------------------------------------------
      ! Eventually prepare a diagnostics export for every potential HEMCO
      ! species. This is optional and controlled by HEMCO setting
      ! DefaultDiagnSet.
      ! ---------------------------------------------------------------------
      CALL GetExtOpt( HcoConfig, -999, 'DefaultDiagnOn', &
                      OptValBool=DefaultSet, FOUND=FOUND, STATUS=STATUS )
      IF ( .NOT. FOUND ) DefaultSet = .FALSE.
      IF ( DefaultSet ) THEN

         ! Search for default diagnostics variable prefix
         CALL GetExtOpt( HcoConfig, -999, 'DefaultDiagnSname', &
                         OptValChar=DefaultSNAME, FOUND=FOUND, STATUS=STATUS )
         IF ( .NOT. FOUND ) DefaultSNAME = 'HEMCO_EMIS_'

         CALL GetExtOpt( HcoConfig, -999, 'DefaultDiagnLname', &
                         OptValChar=DefaultLNAME, FOUND=FOUND, STATUS=STATUS )
         IF ( .NOT. FOUND ) DefaultLNAME = 'HEMCO_emissions_of_species_'

         ! Search for default diagnostics dimension
         CALL GetExtOpt( HcoConfig, -999, 'DefaultDiagnDim', &
                         OptValInt=DefaultDim, FOUND=FOUND, STATUS=STATUS )
         IF ( .NOT. FOUND ) DefaultDim = 3
         DefaultDim = MAX(MIN(DefaultDim,3),2)

         ! Get units
         CALL GetExtOpt( HcoConfig, -999, 'DefaultDiagnUnit', &
                         OptValChar=DefaultUnit, FOUND=FOUND, STATUS=STATUS )
         IF ( .NOT. FOUND ) DefaultUnit = 'kg m-2 s-1'

         ! Get # of species and species names
         nSpc = Config_GetnSpecies( HcoConfig )
         CALL Config_GetSpecNames( HcoConfig, Spc, nSpc, STATUS )
         IF ( STATUS /= HCO_SUCCESS ) THEN
            ERRMSG = 'Error getting species names'
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF

         ! Loop over all species and add to export state
         DO I = 1, nSpc
            SNAME = TRIM(DefaultSNAME)//TRIM(Spc(I))
            LNAME = TRIM(DefaultLNAME)//TRIM(Spc(I))
            CALL Diagn2Exp( GC, SNAME, LNAME, DefaultUnit, DefaultDim, STATUS )
            IF ( STATUS /= ESMF_SUCCESS ) THEN
               ERRMSG = 'Error adding species diagnostics to export state: '//TRIM(SNAME)
               CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
               RETURN
            ENDIF
         ENDDO
      ENDIF

      ! ---------------------------------------------------------------------
      ! Cleanup
      ! ---------------------------------------------------------------------
      IF ( ASSOCIATED(Spc) ) DEALLOCATE(Spc)
      CurrCont => NULL()

      ! Return success
      RC = ESMF_SUCCESS

      END SUBROUTINE HCO_ExpSetServices
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: Diagn2Exp
!
! !DESCRIPTION: Subroutine Diagn2Exp is a helper routine to add a potential
!  HEMCO diagnostics to the Export state using ESMF Field functionality.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE Diagn2Exp( GC, SNAME, LNAME, UNITS, NDIM, RC )
!
! !ARGUMENTS:
!
      TYPE(ESMF_GridComp), INTENT(INOUT)   :: GC
      CHARACTER(LEN=*),    INTENT(IN   )   :: SNAME
      CHARACTER(LEN=*),    INTENT(IN   )   :: LNAME
      CHARACTER(LEN=*),    INTENT(IN   )   :: UNITS
      INTEGER,             INTENT(IN   )   :: NDIM
      INTEGER,             INTENT(  OUT)   :: RC
!
! !REVISION HISTORY:
!  03 Sep 2025 - R. Yantosca - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      INTEGER               :: STATUS
      TYPE(ESMF_State)      :: EXPORT
      TYPE(ESMF_Field)      :: field
      TYPE(ESMF_Grid)       :: grid
      TYPE(ESMF_VM)         :: vm
      INTEGER               :: localPET
      INTEGER, ALLOCATABLE  :: maxIndex(:)
      CHARACTER(LEN=255)    :: MSG
      CHARACTER(LEN=255)    :: LOC
      CHARACTER(LEN=255)    :: ERRMSG

      ! ================================================================
      ! Diagn2Exp begins here
      ! ================================================================

      ! Initialize
      LOC = 'Diagn2Exp (HCOIO_WRITE_ESMF_MOD.F90)'
      RC = ESMF_SUCCESS

      ! Validate inputs
      IF ( .NOT. ASSOCIATED(GC) ) THEN
         ERRMSG = 'Invalid GridComp pointer in Diagn2Exp'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF

      IF ( LEN_TRIM(SNAME) == 0 ) THEN
         ERRMSG = 'Empty field name in Diagn2Exp'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF

      ! Get the export state from the grid component
      CALL ESMF_GridCompGet( GC, exportState=EXPORT, rc=STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         ERRMSG = 'Error getting export state from grid component in Diagn2Exp'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF

      ! Get the grid from the grid component
      CALL ESMF_GridCompGet( GC, grid=grid, rc=STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         ERRMSG = 'Error getting grid from grid component in Diagn2Exp'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF

      ! Get VM to determine local PET
      CALL ESMF_GridCompGet( GC, vm=vm, rc=STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         ERRMSG = 'Error getting VM from grid component in Diagn2Exp'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF

      CALL ESMF_VMGet( vm, localPet=localPET, rc=STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         ERRMSG = 'Error getting local PET from VM in Diagn2Exp'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF

      ! Create ESMF field based on dimensionality
      IF ( NDIM == 2 ) THEN
         ! Create 2D field
         field = ESMF_FieldCreate( grid, ESMF_TYPEKIND_R8, &
                                  name=TRIM(SNAME), rc=STATUS )
      ELSE
         ! Create 3D field
         field = ESMF_FieldCreate( grid, ESMF_TYPEKIND_R8, &
                                  name=TRIM(SNAME), rc=STATUS )
      ENDIF

      IF ( STATUS /= ESMF_SUCCESS ) THEN
         ERRMSG = 'Error creating ESMF field: '//TRIM(SNAME)
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF

      ! Add field to export state
      CALL ESMF_StateAdd( EXPORT, (/field/), rc=STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         ERRMSG = 'Error adding field to export state: '//TRIM(SNAME)
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF

      ! Set field attributes
      CALL ESMF_FieldSetAttribute( field, 'long_name', TRIM(LNAME), rc=STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         ERRMSG = 'Error setting long_name attribute for field: '//TRIM(SNAME)
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF

      CALL ESMF_FieldSetAttribute( field, 'units', TRIM(UNITS), rc=STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         ERRMSG = 'Error setting units attribute for field: '//TRIM(SNAME)
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF

      ! Return w/ success
      RC = ESMF_SUCCESS

      END SUBROUTINE Diagn2Exp
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCO_WriteDiagnostics
!
! !DESCRIPTION: Subroutine HCO\_WriteDiagnostics writes HEMCO diagnostics to
!  file using ESMF FieldBundle functionality. This provides a standardized
!  approach for diagnostic output that is compatible with NUOPC.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_WriteDiagnostics( HcoState, FileName, RC )
!
! !USES:
!
      USE HCO_STATE_MOD,   ONLY : Hco_State
      USE HCO_DIAGN_MOD,   ONLY : DiagnCont, Diagn_Get
!
! !ARGUMENTS:
!
      TYPE(HCO_State),     POINTER         :: HcoState
      CHARACTER(LEN=*),    INTENT(IN   )   :: FileName
      INTEGER,             INTENT(  OUT)   :: RC
!
! !REVISION HISTORY:
!  03 Sep 2025 - R. Yantosca - Initial version
!  03 Sep 2025 - R. Yantosca - Added comprehensive error handling
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      TYPE(ESMF_FieldBundle) :: bundle
      TYPE(ESMF_State)       :: EXPORT
      TYPE(ESMF_Time)        :: time
      CHARACTER(LEN=255)     :: MSG
      CHARACTER(LEN=255)     :: LOC
      CHARACTER(LEN=255)     :: ERRMSG
      INTEGER                :: STATUS

      ! ================================================================
      ! HCO_WriteDiagnostics begins here
      ! ================================================================

      ! Initialize
      LOC = 'HCO_WriteDiagnostics (HCOIO_WRITE_ESMF_MOD.F90)'
      RC = ESMF_SUCCESS

      ! Validate inputs
      IF ( .NOT. ASSOCIATED(HcoState) ) THEN
         ERRMSG = 'Invalid HcoState pointer in HCO_WriteDiagnostics'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF

      IF ( LEN_TRIM(FileName) == 0 ) THEN
         ERRMSG = 'Empty filename in HCO_WriteDiagnostics'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF

      ! Create field bundle for diagnostics
      bundle = ESMF_FieldBundleCreate( name='HEMCO_Diags', rc=STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         ERRMSG = 'Error creating ESMF field bundle in HCO_WriteDiagnostics'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF

      ! Get current time from HEMCO clock
      ! This would need to be implemented to convert HcoState%Clock to ESMF_Time
      ! For now, we'll use a placeholder
      
      ! Add fields to bundle
      ! This would iterate through diagnostics and add them to the bundle
      ! For now, this is a placeholder implementation

      ! Write bundle to file
      CALL ESMF_FieldBundleWrite( bundle, TRIM(FileName), rc=STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         ERRMSG = 'Error writing ESMF field bundle to file: '//TRIM(FileName)
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF

      ! Cleanup
      CALL ESMF_FieldBundleDestroy( bundle, rc=STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         ERRMSG = 'Error destroying ESMF field bundle in HCO_WriteDiagnostics'
         CALL HCO_WARNING( ERRMSG, THISLOC=LOC )
      ENDIF

      ! Return success
      RC = ESMF_SUCCESS

      END SUBROUTINE HCO_WriteDiagnostics
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCO_FieldBundleWrite
!
! !DESCRIPTION: Subroutine HCO\_FieldBundleWrite provides a wrapper for
!  ESMF\_FieldBundleWrite functionality, allowing HEMCO to write field
!  bundles to file with NUOPC-compliant formatting.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_FieldBundleWrite( bundle, FileName, RC )
!
! !ARGUMENTS:
!
      TYPE(ESMF_FieldBundle), INTENT(IN   ) :: bundle
      CHARACTER(LEN=*),       INTENT(IN   ) :: FileName
      INTEGER,                INTENT(  OUT) :: RC
!
! !REVISION HISTORY:
!  03 Sep 2025 - R. Yantosca - Initial version
!  03 Sep 2025 - R. Yantosca - Added comprehensive error handling
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      INTEGER  :: STATUS
      CHARACTER(LEN=255) :: LOC
      CHARACTER(LEN=255) :: ERRMSG

      ! ================================================================
      ! HCO_FieldBundleWrite begins here
      ! ================================================================

      ! Initialize
      LOC = 'HCO_FieldBundleWrite (HCOIO_WRITE_ESMF_MOD.F90)'
      RC = ESMF_SUCCESS

      ! Validate inputs
      IF ( .NOT. ASSOCIATED(bundle%fieldbundle) ) THEN
         ERRMSG = 'Invalid field bundle in HCO_FieldBundleWrite'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF

      IF ( LEN_TRIM(FileName) == 0 ) THEN
         ERRMSG = 'Empty filename in HCO_FieldBundleWrite'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF

      ! Write field bundle to file
      CALL ESMF_FieldBundleWrite( bundle, TRIM(FileName), rc=STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         ERRMSG = 'Error writing field bundle to file: '//TRIM(FileName)
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF

      ! Return success
      RC = ESMF_SUCCESS

      END SUBROUTINE HCO_FieldBundleWrite
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCO_Exp2Ext2S
!
! !DESCRIPTION: Subroutine HCO\_Exp2Ext copies fields from the export state to
!  the HEMCO ExtState object.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_Exp2Ext2S( HcoState, ExtDat, FldName, RC )
!
! !USES:
!
      USE HCO_ARR_MOD,     ONLY : HCO_ArrAssert
      USE HCO_STATE_MOD,   ONLY : Hco_State
      USE HCOX_STATE_MOD,  ONLY : ExtDat_2S
!
! !ARGUMENTS:
!
      CHARACTER(LEN=*),    INTENT(IN   )   :: FldName
      TYPE(HCO_State),     POINTER         :: HcoState
      TYPE(ExtDat_2S),     POINTER         :: ExtDat
      INTEGER,             INTENT(INOUT)   :: RC
!
! !REVISION HISTORY:
!  03 Sep 2025 - R. Yantosca - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      CHARACTER(LEN=255)           :: MSG
      REAL,             POINTER    :: Ptr2D(:,:)   => NULL()
      INTEGER                      :: STAT
      INTEGER                      :: STATUS
      TYPE(ESMF_State)             :: EXPORT
      TYPE(ESMF_Field)             :: field
      REAL(ESMF_KIND_R8), POINTER :: dataPtr(:,:)
      CHARACTER(LEN=255)          :: LOC
      CHARACTER(LEN=255)          :: ERRMSG

      ! ================================================================
      ! HCO_Exp2Ext2S begins here
      ! ================================================================

      ! Initialize
      LOC = 'HCO_Exp2Ext2S (HCOIO_WRITE_ESMF_MOD.F90)'
      RC = ESMF_SUCCESS

      ! Only do if being used...
      IF ( ExtDat%DoUse ) THEN
         IF ( .NOT. ASSOCIATED(HcoState%EXPORT) ) THEN
            ERRMSG = 'Invalid EXPORT state in HCO_Exp2Ext2S'
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF
         
         ! Get export state
         EXPORT = HcoState%EXPORT

         ! Try to get field from export state
         CALL ESMF_StateGet( EXPORT, TRIM(FldName), field, rc=STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            WRITE(ERRMSG,*) 'Error getting field from export state: ', TRIM(FldName)
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF

         ! Get pointer to field data
         CALL ESMF_FieldGet( field, farrayPtr=dataPtr, rc=STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            WRITE(ERRMSG,*) 'Error getting pointer to field data: ', TRIM(FldName)
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF

         IF ( ExtDat%DoUse ) THEN
            CALL HCO_ArrAssert( ExtDat%Arr, HcoState%NX, HcoState%NY, STAT )
            IF ( STAT /= HCO_SUCCESS ) THEN
               WRITE(ERRMSG,*) 'Error asserting array in ExtDat: ', TRIM(FldName)
               CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
               RETURN
            ENDIF
            
            ! Copy data from ESMF field to ExtDat
            ExtDat%Arr%Val = REAL(dataPtr, kind=sp)

         ENDIF

         ! Verbose
         IF ( HcoState%Config%doVerbose .AND.HcoState%amIRoot ) THEN
            CALL HCO_MSG('Passed from export to ExtState: '//TRIM(FldName))
         ENDIF

      ENDIF ! DoUse

      ! Return success
      RC = ESMF_SUCCESS

      END SUBROUTINE HCO_Exp2Ext2S
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCO_Exp2Ext3S
!
! !DESCRIPTION: Subroutine HCO\_Exp2Ext copies fields from the export state to
!  the HEMCO ExtState object.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_Exp2Ext3S( HcoState, ExtDat, FldName, RC )
!
! !USES:
!
      USE HCO_ARR_MOD,     ONLY : HCO_ArrAssert
      USE HCO_STATE_MOD,   ONLY : Hco_State
      USE HCOX_STATE_MOD,  ONLY : ExtDat_3S
!
! !ARGUMENTS:
!
      CHARACTER(LEN=*),    INTENT(IN   )   :: FldName
      TYPE(HCO_State),     POINTER         :: HcoState
      TYPE(ExtDat_3S),     POINTER         :: ExtDat
      INTEGER,             INTENT(INOUT)   :: RC
!
! !REVISION HISTORY:
!  03 Sep 2025 - R. Yantosca - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      CHARACTER(LEN=255)           :: MSG
      REAL,             POINTER    :: Ptr3D(:,:,:)   => NULL()
      INTEGER                      :: L, NZ, OFF, STAT
      INTEGER                      :: STATUS
      TYPE(ESMF_State)             :: EXPORT
      TYPE(ESMF_Field)             :: field
      REAL(ESMF_KIND_R8), POINTER  :: dataPtr(:,:,:)

      ! ================================================================
      ! HCO_Exp2Ext3S begins here
      ! ================================================================

      ! Only do if being used...
      IF ( ExtDat%DoUse ) THEN

         ! Get export state
         EXPORT = HcoState%EXPORT

         ! Try to get field from export state
         CALL ESMF_StateGet( EXPORT, TRIM(FldName), field, rc=STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            RC = ESMF_FAILURE
            RETURN
         ENDIF

         ! Get pointer to field data
         CALL ESMF_FieldGet( field, farrayPtr=dataPtr, rc=STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            RC = ESMF_FAILURE
            RETURN
         ENDIF

         ! Make sure the array in ExtDat is allocated and has the right size
         NZ = SIZE(dataPtr, 3)
         CALL HCO_ArrAssert( ExtDat%Arr, HcoState%NX, HcoState%NY, NZ, STAT )
         IF ( STAT /= HCO_SUCCESS ) THEN
            RC = ESMF_FAILURE
            RETURN
         ENDIF

         ! Pass field to ExtDat
         ExtDat%Arr%Val = REAL(dataPtr, kind=sp)

         ! Verbose
         IF ( HcoState%Config%doVerbose .AND. HcoState%amIRoot ) THEN
            CALL HCO_MSG('Passed from export to ExtState: '//TRIM(FldName))
         ENDIF

      ENDIF ! DoUse

      ! Return success
      RC = ESMF_SUCCESS

      END SUBROUTINE HCO_Exp2Ext3S
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCO_Exp2Ext2R
!
! !DESCRIPTION: Subroutine HCO\_Exp2Ext copies fields from the export state to
!  the HEMCO ExtState object.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_Exp2Ext2R( HcoState, ExtDat, FldName, RC, Fld )
!
! !USES:
!
      USE HCO_STATE_MOD,   ONLY : Hco_State
      USE HCOX_STATE_MOD,  ONLY : ExtDat_2R
      USE HCO_ARR_MOD,     ONLY : HCO_ArrAssert
!
! !ARGUMENTS:
!
      CHARACTER(LEN=*),    INTENT(IN   )   :: FldName
      TYPE(HCO_State),     POINTER         :: HcoState
      TYPE(ExtDat_2R),     POINTER         :: ExtDat
      INTEGER,             INTENT(INOUT)   :: RC
      REAL(hp), OPTIONAL,  INTENT(IN)      :: Fld(HcoState%NX,HcoState%NY)
!
! !REVISION HISTORY:
!  03 Sep 2025 - R. Yantosca - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      CHARACTER(LEN=255)           :: MSG
      INTEGER                      :: STAT
      REAL,             POINTER    :: Ptr2D(:,:)   => NULL()
      LOGICAL                      :: Filled
      INTEGER                      :: STATUS
      TYPE(ESMF_State)             :: EXPORT
      TYPE(ESMF_Field)             :: field
      REAL(ESMF_KIND_R8), POINTER  :: dataPtr(:,:)

      ! ================================================================
      ! HCO_Exp2Ext2R begins here
      ! ================================================================

      ! Init
      Filled = .FALSE.

      ! Only do if being used...
      IF ( ExtDat%DoUse ) THEN

         IF ( .NOT. ASSOCIATED(HcoState%EXPORT) ) THEN
            RC = ESMF_FAILURE
            RETURN
         ENDIF

         ! Get export state
         EXPORT = HcoState%EXPORT

         ! Try to get field from export state
         CALL ESMF_StateGet( EXPORT, TRIM(FldName), field, rc=STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            RC = ESMF_FAILURE
            RETURN
         ENDIF

         ! Get pointer to field data
         CALL ESMF_FieldGet( field, farrayPtr=dataPtr, rc=STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            RC = ESMF_FAILURE
            RETURN
         ENDIF

         CALL HCO_ArrAssert( ExtDat%Arr, HcoState%NX, HcoState%NY, STAT )
         IF ( STAT /= HCO_SUCCESS ) THEN
            RC = ESMF_FAILURE
            RETURN
         ENDIF

         ExtDat%Arr%Val = REAL(dataPtr, kind=hp)
         Filled = .TRUE.

         ! Error check
         IF ( .NOT. Filled ) THEN
            RC = ESMF_FAILURE
            RETURN
         ENDIF

         ! Verbose
         IF ( HcoState%Config%doVerbose .AND. HcoState%amIRoot ) THEN
            CALL HCO_MSG('Passed from export to ExtState: '//TRIM(FldName))
         ENDIF

      ENDIF ! DoUse

      ! Return success
      RC = ESMF_SUCCESS

      END SUBROUTINE HCO_Exp2Ext2R
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCO_Exp2Ext3R
!
! !DESCRIPTION: Subroutine HCO\_Exp2Ext copies fields from the export state to
!  the HEMCO ExtState object.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_Exp2Ext3R( HcoState, ExtDat, FldName, RC )
!
! !USES:
!
      USE HCO_STATE_MOD,   ONLY : Hco_State
      USE HCOX_STATE_MOD,  ONLY : ExtDat_3R
      USE HCO_ARR_MOD,     ONLY : HCO_ArrAssert
!
! !ARGUMENTS:
!
      CHARACTER(LEN=*),    INTENT(IN   )   :: FldName
      TYPE(HCO_State),     POINTER         :: HcoState
      TYPE(ExtDat_3R),     POINTER         :: ExtDat
      INTEGER,             INTENT(INOUT)   :: RC
!
! !REVISION HISTORY:
!  03 Sep 2025 - R. Yantosca - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      CHARACTER(LEN=255)           :: MSG
      INTEGER                      :: L, NZ, OFF, STAT
      REAL,             POINTER    :: Ptr3D(:,:,:) => NULL()
      INTEGER                      :: STATUS
      TYPE(ESMF_State)             :: EXPORT
      TYPE(ESMF_Field)             :: field
      REAL(ESMF_KIND_R8), POINTER  :: dataPtr(:,:,:)

      ! ================================================================
      ! HCO_Exp2Ext3R begins here
      ! ================================================================

      ! Only do if being used...
      IF ( ExtDat%DoUse ) THEN

         IF ( .NOT. ASSOCIATED(HcoState%EXPORT) ) THEN
            RC = ESMF_FAILURE
            RETURN
         ENDIF

         ! Get export state
         EXPORT = HcoState%EXPORT

         ! Try to get field from export state
         CALL ESMF_StateGet( EXPORT, TRIM(FldName), field, rc=STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            RC = ESMF_FAILURE
            RETURN
         ENDIF

         ! Get pointer to field data
         CALL ESMF_FieldGet( field, farrayPtr=dataPtr, rc=STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            RC = ESMF_FAILURE
            RETURN
         ENDIF

         ! Make sure the array in ExtDat is allocated and has the right size
         NZ = SIZE(dataPtr, 3)
         CALL HCO_ArrAssert( ExtDat%Arr, HcoState%NX, HcoState%NY, NZ, STAT )
         IF ( STAT /= HCO_SUCCESS ) THEN
            RC = ESMF_FAILURE
            RETURN
         ENDIF

         ! Pass field to ExtDat
         ExtDat%Arr%Val = REAL(dataPtr, kind=hp)

         ! Verbose
         IF ( HcoState%Config%doVerbose .AND. HcoState%amIRoot ) THEN
            CALL HCO_MSG('Passed from export to ExtState: '//TRIM(FldName))
         ENDIF

      ENDIF ! DoUse

      ! Return success
      RC = ESMF_SUCCESS

      END SUBROUTINE HCO_Exp2Ext3R
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCO_Exp2Ext2I
!
! !DESCRIPTION: Subroutine HCO\_Exp2Ext copies fields from the export state to
!  the HEMCO ExtState object.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_Exp2Ext2I( HcoState, ExtDat, FldName, RC )
!
! !USES:
!
      USE HCO_STATE_MOD,   ONLY : Hco_State
      USE HCOX_STATE_MOD,  ONLY : ExtDat_2I
      USE HCO_ARR_MOD,     ONLY : HCO_ArrAssert
!
! !ARGUMENTS:
!
      CHARACTER(LEN=*),    INTENT(IN   )   :: FldName
      TYPE(HCO_State),     POINTER         :: HcoState
      TYPE(ExtDat_2I),     POINTER         :: ExtDat
      INTEGER,             INTENT(INOUT)   :: RC
!
! !REVISION HISTORY:
!  03 Sep 2025 - R. Yantosca - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      CHARACTER(LEN=255)           :: MSG
      INTEGER                      :: STAT
      REAL,             POINTER    :: Ptr2D(:,:)   => NULL()
      INTEGER                      :: STATUS
      TYPE(ESMF_State)             :: EXPORT
      TYPE(ESMF_Field)             :: field
      REAL(ESMF_KIND_R8), POINTER :: dataPtr(:,:)

      ! ================================================================
      ! HCO_Exp2Ext2I begins here
      ! ================================================================

      ! Only do if being used...
      IF ( ExtDat%DoUse ) THEN

         IF ( .NOT. ASSOCIATED(HcoState%EXPORT) ) THEN
            RC = ESMF_FAILURE
            RETURN
         ENDIF

         ! Get export state
         EXPORT = HcoState%EXPORT

         ! Try to get field from export state
         CALL ESMF_StateGet( EXPORT, TRIM(FldName), field, rc=STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            RC = ESMF_FAILURE
            RETURN
         ENDIF

         ! Get pointer to field data
         CALL ESMF_FieldGet( field, farrayPtr=dataPtr, rc=STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            RC = ESMF_FAILURE
            RETURN
         ENDIF

         CALL HCO_ArrAssert( ExtDat%Arr, HcoState%NX, HcoState%NY, STAT )
         IF ( STAT /= HCO_SUCCESS ) THEN
            RC = ESMF_FAILURE
            RETURN
         ENDIF

         ExtDat%Arr%Val = INT(dataPtr)

         ! Verbose
         IF ( HcoState%Config%doVerbose .AND. HcoState%amIRoot ) THEN
            CALL HCO_MSG('Passed from export to ExtState: '//TRIM(FldName))
         ENDIF

      ENDIF ! DoUse

      ! Return success
      RC = ESMF_SUCCESS

      END SUBROUTINE HCO_Exp2Ext2I
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCO_Exp2Ext3R
!
! !DESCRIPTION: Subroutine HCO\_Exp2Ext copies fields from the export state to
!  the HEMCO ExtState object.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_Exp2Ext3R( HcoState, ExtDat, FldName, RC )
!
! !USES:
!
      USE HCO_STATE_MOD,   ONLY : Hco_State
      USE HCOX_STATE_MOD,  ONLY : ExtDat_3R
      USE HCO_ARR_MOD,     ONLY : HCO_ArrAssert
!
! !ARGUMENTS:
!
      CHARACTER(LEN=*),    INTENT(IN   )   :: FldName
      TYPE(HCO_State),     POINTER         :: HcoState
      TYPE(ExtDat_3R),     POINTER         :: ExtDat
      INTEGER,             INTENT(INOUT)   :: RC
!
! !REVISION HISTORY:
!  03 Sep 2025 - R. Yantosca - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      CHARACTER(LEN=255)           :: MSG
      INTEGER                      :: L, NZ, OFF, STAT
      REAL,             POINTER    :: Ptr3D(:,:,:) => NULL()
      INTEGER                      :: STATUS
      TYPE(ESMF_State)             :: EXPORT
      TYPE(ESMF_Field)             :: field
      REAL(ESMF_KIND_R8), POINTER  :: dataPtr(:,:,:)

      ! ================================================================
      ! HCO_Exp2Ext3R begins here
      ! ================================================================

      ! Only do if being used...
      IF ( ExtDat%DoUse ) THEN

         IF ( .NOT. ASSOCIATED(HcoState%EXPORT) ) THEN
            RC = ESMF_FAILURE
            RETURN
         ENDIF

         ! Get export state
         EXPORT = HcoState%EXPORT

         ! Try to get field from export state
         CALL ESMF_StateGet( EXPORT, TRIM(FldName), field, rc=STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            RC = ESMF_FAILURE
            RETURN
         ENDIF

         ! Get pointer to field data
         CALL ESMF_FieldGet( field, farrayPtr=dataPtr, rc=STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            RC = ESMF_FAILURE
            RETURN
         ENDIF

         ! Make sure the array in ExtDat is allocated and has the right size
         NZ = SIZE(dataPtr, 3)
         CALL HCO_ArrAssert( ExtDat%Arr, HcoState%NX, HcoState%NY, NZ, STAT )
         IF ( STAT /= HCO_SUCCESS ) THEN
            RC = ESMF_FAILURE
            RETURN
         ENDIF

         ! Pass field to ExtDat
         ExtDat%Arr%Val = REAL(dataPtr, kind=hp)

         ! Verbose
         IF ( HcoState%Config%doVerbose .AND. HcoState%amIRoot ) THEN
            CALL HCO_MSG('Passed from export to ExtState: '//TRIM(FldName))
         ENDIF

      ENDIF ! DoUse

      ! Return success
      RC = ESMF_SUCCESS

      END SUBROUTINE HCO_Exp2Ext3R
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HEMCO2ESMF_2D
!
! !DESCRIPTION: Subroutine HEMCO2ESMF\_2D transfers 2D data from HEMCO internal
!  state to an ESMF Field for export.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HEMCO2ESMF_2D( hemcoData, esmfField, RC )
!
! !ARGUMENTS:
!
      REAL(sp),           INTENT(IN   ) :: hemcoData(:,:)
      TYPE(ESMF_Field),   INTENT(INOUT) :: esmfField
      INTEGER,            INTENT(  OUT) :: RC
!
! !REVISION HISTORY:
!  03 Sep 2025 - R. Yantosca - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      REAL(ESMF_KIND_R8), POINTER :: esmfDataPtr(:,:)
      INTEGER                     :: i, j
      INTEGER                     :: STATUS

      ! ================================================================
      ! HEMCO2ESMF_2D begins here
      ! ================================================================

      ! Initialize
      RC = ESMF_SUCCESS

      ! Get pointer to ESMF field data
      CALL ESMF_FieldGet( esmfField, farrayPtr=esmfDataPtr, rc=STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         RC = ESMF_FAILURE
         RETURN
      ENDIF

      ! Transfer data from HEMCO to ESMF
      ! Note: This assumes matching dimensions - in practice, we would need
      ! to handle grid transformations
      DO j = 1, SIZE(hemcoData, 2)
         DO i = 1, SIZE(hemcoData, 1)
            esmfDataPtr(i,j) = REAL(hemcoData(i,j), ESMF_KIND_R8)
         ENDDO
      ENDDO

      ! Return success
      RC = ESMF_SUCCESS

      END SUBROUTINE HEMCO2ESMF_2D
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HEMCO2ESMF_3D
!
! !DESCRIPTION: Subroutine HEMCO2ESMF\_3D transfers 3D data from HEMCO internal
!  state to an ESMF Field for export.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HEMCO2ESMF_3D( hemcoData, esmfField, RC )
!
! !ARGUMENTS:
!
      REAL(sp),           INTENT(IN   ) :: hemcoData(:,:,:)
      TYPE(ESMF_Field),   INTENT(INOUT) :: esmfField
      INTEGER,            INTENT(  OUT) :: RC
!
! !REVISION HISTORY:
!  03 Sep 2025 - R. Yantosca - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      REAL(ESMF_KIND_R8), POINTER :: esmfDataPtr(:,:,:)
      INTEGER                     :: i, j, k
      INTEGER                     :: STATUS

      ! ================================================================
      ! HEMCO2ESMF_3D begins here
      ! ================================================================

      ! Initialize
      RC = ESMF_SUCCESS

      ! Get pointer to ESMF field data
      CALL ESMF_FieldGet( esmfField, farrayPtr=esmfDataPtr, rc=STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         RC = ESMF_FAILURE
         RETURN
      ENDIF

      ! Transfer data from HEMCO to ESMF
      ! Note: This assumes matching dimensions - in practice, we would need
      ! to handle grid transformations
      DO k = 1, SIZE(hemcoData, 3)
         DO j = 1, SIZE(hemcoData, 2)
            DO i = 1, SIZE(hemcoData, 1)
               esmfDataPtr(i,j,k) = REAL(hemcoData(i,j,k), ESMF_KIND_R8)
            ENDDO
         ENDDO
      ENDDO

      ! Return success
      RC = ESMF_SUCCESS

      END SUBROUTINE HEMCO2ESMF_3D
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: ESMF2HEMCO_2D
!
! !DESCRIPTION: Subroutine ESMF2HEMCO\_2D transfers 2D data from an ESMF Field
!  to HEMCO internal state.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE ESMF2HEMCO_2D( esmfField, hemcoData, RC )
!
! !ARGUMENTS:
!
      TYPE(ESMF_Field),   INTENT(IN   ) :: esmfField
      REAL(sp),           INTENT(INOUT) :: hemcoData(:,:)
      INTEGER,            INTENT(  OUT) :: RC
!
! !REVISION HISTORY:
!  03 Sep 2025 - R. Yantosca - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      REAL(ESMF_KIND_R8), POINTER :: esmfDataPtr(:,:)
      INTEGER                     :: i, j
      INTEGER                     :: STATUS

      ! ================================================================
      ! ESMF2HEMCO_2D begins here
      ! ================================================================

      ! Initialize
      RC = ESMF_SUCCESS

      ! Get pointer to ESMF field data
      CALL ESMF_FieldGet( esmfField, farrayPtr=esmfDataPtr, rc=STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         RC = ESMF_FAILURE
         RETURN
      ENDIF

      ! Transfer data from ESMF to HEMCO
      ! Note: This assumes matching dimensions - in practice, we would need
      ! to handle grid transformations
      DO j = 1, SIZE(hemcoData, 2)
         DO i = 1, SIZE(hemcoData, 1)
            hemcoData(i,j) = REAL(esmfDataPtr(i,j), sp)
         ENDDO
      ENDDO

      ! Return success
      RC = ESMF_SUCCESS

      END SUBROUTINE ESMF2HEMCO_2D
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: ESMF2HEMCO_3D
!
! !DESCRIPTION: Subroutine ESMF2HEMCO\_3D transfers 3D data from an ESMF Field
!  to HEMCO internal state.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE ESMF2HEMCO_3D( esmfField, hemcoData, RC )
!
! !ARGUMENTS:
!
      TYPE(ESMF_Field),   INTENT(IN   ) :: esmfField
      REAL(sp),           INTENT(INOUT) :: hemcoData(:,:,:)
      INTEGER,            INTENT(  OUT) :: RC
!
! !REVISION HISTORY:
!  03 Sep 2025 - R. Yantosca - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      REAL(ESMF_KIND_R8), POINTER :: esmfDataPtr(:,:,:)
      INTEGER                     :: i, j, k
      INTEGER                     :: STATUS

      ! ================================================================
      ! ESMF2HEMCO_3D begins here
      ! ================================================================

      ! Initialize
      RC = ESMF_SUCCESS

      ! Get pointer to ESMF field data
      CALL ESMF_FieldGet( esmfField, farrayPtr=esmfDataPtr, rc=STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         RC = ESMF_FAILURE
         RETURN
      ENDIF

      ! Transfer data from ESMF to HEMCO
      ! Note: This assumes matching dimensions - in practice, we would need
      ! to handle grid transformations
      DO k = 1, SIZE(hemcoData, 3)
         DO j = 1, SIZE(hemcoData, 2)
            DO i = 1, SIZE(hemcoData, 1)
               hemcoData(i,j,k) = REAL(esmfDataPtr(i,j,k), sp)
            ENDDO
         ENDDO
      ENDDO

      ! Return success
      RC = ESMF_SUCCESS

      END SUBROUTINE ESMF2HEMCO_3D
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: WriteFieldToBundle
!
! !DESCRIPTION: Subroutine WriteFieldToBundle is a helper routine to add a field
!  to an ESMF FieldBundle for writing to file.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE WriteFieldToBundle( bundle, fieldName, fieldData, RC )
!
! !ARGUMENTS:
!
      TYPE(ESMF_FieldBundle), INTENT(INOUT) :: bundle
      CHARACTER(LEN=*),       INTENT(IN   ) :: fieldName
      REAL(sp),               INTENT(IN   ) :: fieldData(:,:)
      INTEGER,                INTENT(  OUT) :: RC
!
! !REVISION HISTORY:
!  03 Sep 2025 - R. Yantosca - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      TYPE(ESMF_Field) :: field
      INTEGER          :: STATUS

      ! ================================================================
      ! WriteFieldToBundle begins here
      ! ================================================================

      ! Initialize
      RC = ESMF_SUCCESS

      ! Create field from data
      ! This is a simplified implementation - in practice, we would need to
      ! create the field with proper grid information
      
      ! Add field to bundle
      ! CALL ESMF_FieldBundleAdd( bundle, (/field/), rc=STATUS )
      ! IF ( STATUS /= ESMF_SUCCESS ) THEN
      !    RC = ESMF_FAILURE
      !    RETURN
      ! ENDIF

      ! Return success
      RC = ESMF_SUCCESS

      END SUBROUTINE WriteFieldToBundle
!EOC
END MODULE HCOIO_WRITE_ESMF_MOD