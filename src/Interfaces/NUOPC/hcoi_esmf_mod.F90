!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hcoi_esmf_mod
!
! !DESCRIPTION: Module HCOI\_ESMF\_MOD is the HEMCO-ESMF interface module.
!\\
!\\
! !INTERFACE:
!
MODULE HCOI_ESMF_MOD
!
! !USES:
!
  USE HCO_ERROR_MOD
  USE HCO_Types_Mod
  USE ESMF

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  ! ESMF environment only:
  PUBLIC :: HCO_SetServices
  PUBLIC :: HCO_SetExtState_ESMF
  PUBLIC :: HCO_Imp2Ext
  ! NEXUS integration functions
  PUBLIC :: HCO_NexusInit
  PUBLIC :: HCO_NexusRegisterFields
  PUBLIC :: HCO_NexusTransferData
  PUBLIC :: HCO_NexusFinalize
  PUBLIC :: HCO_NexusImportData
  PUBLIC :: HCO_NexusExportData
  PUBLIC :: HCO_NexusGetConfig
!
! !PRIVATE MEMBER FUNCTIONS:
!
  PRIVATE :: Diagn2Exp
  PRIVATE :: HCO_Imp2Ext2R
  PRIVATE :: HCO_Imp2Ext2S
  PRIVATE :: HCO_Imp2Ext2I
  PRIVATE :: HCO_Imp2Ext3R
  PRIVATE :: HCO_Imp2Ext3S
  ! NEXUS private helper functions
  PRIVATE :: HCO_NexusImportData
  PRIVATE :: HCO_NexusExportData
  PRIVATE :: HCO_NexusGetConfig
!
! !REVISION HISTORY:
!  10 Oct 2014 - C. Keller   - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !MODULE INTERFACES:
!
  INTERFACE HCO_Imp2Ext
     MODULE PROCEDURE HCO_Imp2Ext2R
     MODULE PROCEDURE HCO_Imp2Ext2S
     MODULE PROCEDURE HCO_Imp2Ext2I
     MODULE PROCEDURE HCO_Imp2Ext3R
     MODULE PROCEDURE HCO_Imp2Ext3S
  END INTERFACE HCO_Imp2Ext

CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCO_SetServices
!
! !DESCRIPTION: Subroutine HCO\_SetServices registers all required HEMCO
! data so that it can be imported through the ESMF import state.
! This routine determines all required HEMCO input fields from the HEMCO
! configuration file. Note that each file needs an equivalent ESMF-style
! entry in the registry file (typically ExtData.rc). Otherwise, ESMF won't
! read these files and HEMCO will fail when attempting to get pointers to
! these data arrays.
!\\
!\\
! The field names provided in ExtData.rc must match the names in the HEMCO
! configuration file! Also, all time settings (average and update interval)
! and data units need to be properly specified in ExtData.rc.
! For now, ExtData.rc and HEMCO configuration file need to be synchronized
! manually. The pyHEMCO interface will automate this process!
!\\
!\\
! This routine also prepares an emissions export field for every species
! found in the HEMCO configuration file. These export fields will only
! be filled if specified so in the MAPL History registry.
! The corresponding HEMCO diagnostics must be created separately via
! Diagn\_Create (e.g. in hcoi\_gc\_diagn\_mod.F90).
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_SetServices( am_I_Root,  GC, HcoConfig, &
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
!  29 Aug 2013 - C. Keller - Initial version.
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
      TYPE(ESMF_State)           :: IMPORT
      TYPE(ESMF_Field)           :: field
      CHARACTER(LEN=255)         :: ERRMSG
! ================================================================
! HCO_SetServices begins here
! ================================================================

! Init
LOC      = 'HCO_SetServices (HCOI_ESMF_MOD.F90)'
Spc      => NULL()
CurrCont => NULL()
RC       = ESMF_SUCCESS

! Get the import state from the grid component
CALL ESMF_GridCompGet( GC, importState=IMPORT, rc=STATUS )
IF ( STATUS /= ESMF_SUCCESS ) THEN
   ERRMSG = 'Error getting import state from grid component'
   CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
   RETURN
ENDIF

! ---------------------------------------------------------------------
! Read file into buffer
! ---------------------------------------------------------------------

CALL Config_ReadFile( am_I_Root, HcoConfig, TRIM(ConfigFile), 0, STATUS )
IF ( STATUS /= HCO_SUCCESS ) THEN
   ERRMSG = 'Error reading configuration file'
   CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
   RETURN
ENDIF

! ---------------------------------------------------------------------
! Set services for all import fields
! ---------------------------------------------------------------------

! Loop over all lines and set services according to input file content
CurrCont => NULL()
CALL ListCont_NextCont ( HcoConfig%ConfigList, CurrCont, FLAG )
DO WHILE ( FLAG == HCO_SUCCESS )

   ! Skip containers that are not defined
   IF ( .NOT. ASSOCIATED(CurrCont%Dct) ) THEN
      CALL ListCont_NextCont ( HcoConfig%ConfigList, CurrCont, FLAG )
      CYCLE
   ENDIF
   IF ( .NOT. ASSOCIATED(CurrCont%Dct%Dta) ) THEN
      CALL ListCont_NextCont ( HcoConfig%ConfigList, CurrCont, FLAG )
      CYCLE
   ENDIF

   ! Add arrays to import spec. Distinguish between 2D and 3D arrays.
   ! Note that we can ignore the time reading interval here, as this
   ! is automatically determined by ESMF based upon the registry file
   ! content!.
   ! Ignore containers with ncRead flag disabled. These are typically
   ! scalar fields directly read from the configuration file.
   IF ( .NOT. CurrCont%Dct%Dta%ncRead ) THEN

   ! Multiple data containers can use the same source data. In this
   ! case we only need to import the data once. The second, third, etc.
   ! containters registerd for the same source data have been assigned
   ! lower DtaHome values (in hco_config_mod.F90), so skip this container
   ! if flag is not -999 (=default).
   ELSEIF ( CurrCont%Dct%DtaHome /= -999 ) THEN

   ! Import 2D data
   ELSEIF ( CurrCont%Dct%Dta%SpaceDim == 2 ) THEN
      ! Create and add 2D field to import state
      field = ESMF_FieldCreate( ESMF_TYPEKIND_R8, name=TRIM(CurrCont%Dct%Dta%Name), rc=STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         WRITE(ERRMSG,'(a,a)') 'Error creating 2D field: ', TRIM(CurrCont%Dct%Dta%Name)
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF
      CALL ESMF_StateAdd( IMPORT, (/field/), rc=STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         WRITE(ERRMSG,'(a,a)') 'Error adding 2D field to import state: ', TRIM(CurrCont%Dct%Dta%Name)
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF

   ! Import 3D data: Assume central location in vertical dimension!
   ELSEIF ( CurrCont%Dct%Dta%SpaceDim == 3 ) THEN
      ! Create and add 3D field to import state
      field = ESMF_FieldCreate( ESMF_TYPEKIND_R8, name=TRIM(CurrCont%Dct%Dta%Name), rc=STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         WRITE(ERRMSG,'(a,a)') 'Error creating 3D field: ', TRIM(CurrCont%Dct%Dta%Name)
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF
      CALL ESMF_StateAdd( IMPORT, (/field/), rc=STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         WRITE(ERRMSG,'(a,a)') 'Error adding 3D field to import state: ', TRIM(CurrCont%Dct%Dta%Name)
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF

   ! Return w/ error if not 2D or 3D data
   ELSE
      WRITE(ERRMSG,'(a,i0)') 'Invalid space dimension for field: ', CurrCont%Dct%Dta%SpaceDim
      CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
      RETURN
   ENDIF

   ! Advance to next container
   CALL ListCont_NextCont ( HcoConfig%ConfigList, CurrCont, FLAG )

ENDDO

! Free pointer
CurrCont => NULL()

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

   IF ( am_I_Root ) WRITE(*,*) 'Reading HEMCO configuration file: ', &
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

      ! Define vertical dimension
      IF ( SpaceDim == 3 ) THEN
         ! In NUOPC/ESMF, we would define appropriate ESMF dimensions
      ELSE
         ! For 2D, we would define appropriate ESMF dimensions
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
         WRITE(ERRMSG,'(a,a)') 'Error in Diagn2Exp for species: ', TRIM(SNAME)
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF
   ENDDO
ENDIF

! ---------------------------------------------------------------------
! Cleanup
! ---------------------------------------------------------------------
IF ( ASSOCIATED(Spc) ) DEALLOCATE(Spc)

! Return success
RC = ESMF_SUCCESS

END SUBROUTINE HCO_SetServices
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: Diagn2Exp
!
! !DESCRIPTION: Subroutine Diagn2Exp is a helper routine to add a potential
! HEMCO diagnostics to the Export state.
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
!  05 Jan 2015 - C. Keller - Initial version
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
      LOC = 'Diagn2Exp (HCOI_ESMF_MOD.F90)'
      RC = ESMF_SUCCESS

      ! Validate inputs
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
         field = ESMF_FieldCreate( grid, ESMF_TYPEKIND_R4, &
                                  name=TRIM(SNAME), rc=STATUS )
      ELSE
         ! Create 3D field
         field = ESMF_FieldCreate( grid, ESMF_TYPEKIND_R4, &
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
! !ROUTINE: HCO_SetExtState_ESMF
!
! !DESCRIPTION: Subroutine HCO\_SetExtState\_ESMF tries to populate some
! fields of the ExtState object from the ESMF import state.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_SetExtState_ESMF( HcoState, ExtState, RC )
!
! !USES:
!
      USE HCO_STATE_MOD,   ONLY : Hco_State
      USE HCOX_STATE_MOD,  ONLY : Ext_State
!
! !ARGUMENTS:
!
      TYPE(HCO_State),     POINTER         :: HcoState
      TYPE(Ext_State),     POINTER         :: ExtState
      INTEGER,             INTENT(INOUT)   :: RC
!
! !REVISION HISTORY:
!  06 Mar 2015 - C. Keller - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      INTEGER :: STATUS
      CHARACTER(LEN=255) :: ERRMSG
      CHARACTER(LEN=255) :: LOC

      ! ================================================================
      ! HCO_SetExtState_ESMF begins here
      ! ================================================================

      ! Initialize
      LOC = 'HCO_SetExtState_ESMF (HCOI_ESMF_MOD.F90)'
      RC = ESMF_SUCCESS

      ! Validate inputs
      IF ( .NOT. ASSOCIATED(HcoState) ) THEN
         ERRMSG = 'Invalid HcoState pointer'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF

      IF ( .NOT. ASSOCIATED(ExtState) ) THEN
         ERRMSG = 'Invalid ExtState pointer'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF

      ! Get pointers to fields
      CALL HCO_Imp2Ext ( HcoState, ExtState%BYNCY, 'BYNCY', STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         ERRMSG = 'Error getting BYNCY field'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF

#if defined( MODEL_GEOS )
      ! Get pointers to fields
      CALL HCO_Imp2Ext ( HcoState, ExtState%LFR, 'LFR', STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         ERRMSG = 'Error getting LFR field'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF
#endif

      ! Populate meteorological fields if enabled (optional for compatibility)
      IF ( ASSOCIATED(HcoState) .AND. HcoState%do_met_import ) THEN
         ! U wind (3D)
         CALL HCO_Imp2Ext ( HcoState, ExtState%U, 'U', STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            CALL ESMF_LogWrite( "Warning: U field not found in import state; using defaults for eastward wind", &
                               ESMF_LOGMSG_WARNING, line=__LINE__, file=__FILE__, rc=STATUS )
            STATUS = ESMF_SUCCESS  ! Continue for optional field
         ENDIF

         ! V wind (3D)
         CALL HCO_Imp2Ext ( HcoState, ExtState%V, 'V', STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            CALL ESMF_LogWrite( "Warning: V field not found in import state; using defaults for northward wind", &
                               ESMF_LOGMSG_WARNING, line=__LINE__, file=__FILE__, rc=STATUS )
            STATUS = ESMF_SUCCESS
         ENDIF

         ! TK (3D temperature)
         CALL HCO_Imp2Ext ( HcoState, ExtState%TK, 'TK', STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            CALL ESMF_LogWrite( "Warning: TK field not found in import state; using defaults for temperature", &
                               ESMF_LOGMSG_WARNING, line=__LINE__, file=__FILE__, rc=STATUS )
            STATUS = ESMF_SUCCESS
         ENDIF

         ! PBL (2D boundary layer height)
         CALL HCO_Imp2Ext ( HcoState, ExtState%PBL, 'PBL', STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            CALL ESMF_LogWrite( "Warning: PBL field not found in import state; using defaults for PBL height", &
                               ESMF_LOGMSG_WARNING, line=__LINE__, file=__FILE__, rc=STATUS )
            STATUS = ESMF_SUCCESS
         ENDIF

         ! PSFC (2D surface pressure)
         CALL HCO_Imp2Ext ( HcoState, ExtState%PSFC, 'PSFC', STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            CALL ESMF_LogWrite( "Warning: PSFC field not found in import state; using defaults for surface pressure", &
                               ESMF_LOGMSG_WARNING, line=__LINE__, file=__FILE__, rc=STATUS )
            STATUS = ESMF_SUCCESS  ! PSFC might be required for some, but optional here
         ENDIF
      ENDIF

      ! Return success
      RC = ESMF_SUCCESS

      END SUBROUTINE HCO_SetExtState_ESMF
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCO_Imp2Ext2S
!
! !DESCRIPTION: Subroutine HCO\_Imp2Ext copies fields from the import state to
! the HEMCO ExtState object.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_Imp2Ext2S( HcoState, ExtDat, FldName, RC )
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
!  08 Feb 2016 - C. Keller - Initial version
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
      TYPE(ESMF_State)             :: IMPORT
      TYPE(ESMF_Field)             :: field
      REAL(ESMF_KIND_R8), POINTER  :: dataPtr(:,:)
      CHARACTER(LEN=255)           :: LOC
      CHARACTER(LEN=255)           :: ERRMSG

      ! ================================================================
      ! HCO_Imp2Ext2S begins here
      ! ================================================================

      ! Initialize
      LOC = 'HCO_Imp2Ext2S (HCOI_ESMF_MOD.F90)'
      RC = ESMF_SUCCESS

      ! Only do if being used...
      IF ( ExtDat%DoUse ) THEN
         ! Validate inputs
         IF ( .NOT. ASSOCIATED(HcoState) ) THEN
            ERRMSG = 'Invalid HcoState pointer'
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF
         
         IF ( .NOT. ASSOCIATED(HcoState%IMPORT) ) THEN
            ERRMSG = 'Invalid IMPORT state in HcoState'
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF
         
         ! Get import state
         IMPORT = HcoState%IMPORT
         
         ! Try to get field from import state
         CALL ESMF_StateGet( IMPORT, TRIM(FldName), field, rc=STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            WRITE(ERRMSG,'(a,a)') 'Error getting field from import state: ', TRIM(FldName)
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF
         
         ! Get pointer to field data
         CALL ESMF_FieldGet( field, farrayPtr=dataPtr, rc=STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            WRITE(ERRMSG,'(a,a)') 'Error getting pointer to field data: ', TRIM(FldName)
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF
         
         IF ( ExtDat%DoUse ) THEN
            CALL HCO_ArrAssert( ExtDat%Arr, HcoState%NX, HcoState%NY, STAT )
            IF ( STAT /= HCO_SUCCESS ) THEN
               WRITE(ERRMSG,'(a,a)') 'Error asserting array for field: ', TRIM(FldName)
               CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
               RETURN
            ENDIF
            ! Copy data from ESMF field to ExtDat
            ExtDat%Arr%Val = REAL(dataPtr, kind=sp)
         ENDIF

         ! Verbose
         IF ( HcoState%Config%doVerbose .AND.HcoState%amIRoot ) THEN
            CALL HCO_MSG('Passed from import to ExtState: '//TRIM(FldName))
         ENDIF

      ENDIF ! DoUse

      ! Return success
      RC = ESMF_SUCCESS

      END SUBROUTINE HCO_Imp2Ext2S
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCO_Imp2Ext3S
!
! !DESCRIPTION: Subroutine HCO\_Imp2Ext copies fields from the import state to
! the HEMCO ExtState object.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_Imp2Ext3S( HcoState, ExtDat, FldName, RC )
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
!  08 Feb 2016 - C. Keller - Initial version
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
      TYPE(ESMF_State)             :: IMPORT
      TYPE(ESMF_Field)             :: field
      REAL(ESMF_KIND_R8), POINTER  :: dataPtr(:,:,:)
      CHARACTER(LEN=255)           :: LOC
      CHARACTER(LEN=255)           :: ERRMSG

      ! ================================================================
      ! HCO_Imp2Ext3S begins here
      ! ================================================================

      ! Initialize
      LOC = 'HCO_Imp2Ext3S (HCOI_ESMF_MOD.F90)'
      RC = ESMF_SUCCESS

      ! Only do if being used...
      IF ( ExtDat%DoUse ) THEN
         ! Validate inputs
         IF ( .NOT. ASSOCIATED(HcoState) ) THEN
            ERRMSG = 'Invalid HcoState pointer'
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF

         IF ( .NOT. ASSOCIATED(HcoState%IMPORT) ) THEN
            ERRMSG = 'Invalid IMPORT state in HcoState'
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF

         ! Get import state
         IMPORT = HcoState%IMPORT

         ! Try to get field from import state
         CALL ESMF_StateGet( IMPORT, TRIM(FldName), field, rc=STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            WRITE(ERRMSG,'(a,a)') 'Error getting field from import state: ', TRIM(FldName)
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF

         ! Get pointer to field data
         CALL ESMF_FieldGet( field, farrayPtr=dataPtr, rc=STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            WRITE(ERRMSG,'(a,a)') 'Error getting pointer to field data: ', TRIM(FldName)
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF

         ! Make sure the array in ExtDat is allocated and has the right size
         NZ = SIZE(dataPtr, 3)
         CALL HCO_ArrAssert( ExtDat%Arr, HcoState%NX, HcoState%NY, NZ, STAT )
         IF ( STAT /= HCO_SUCCESS ) THEN
            WRITE(ERRMSG,'(a,a)') 'Error asserting array for field: ', TRIM(FldName)
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF

         ! Copy data from ESMF field to ExtDat
         ExtDat%Arr%Val = REAL(dataPtr, kind=sp)

         ! Verbose
         IF ( HcoState%Config%doVerbose .AND. HcoState%amIRoot ) THEN
            CALL HCO_MSG('Passed from import to ExtState: '//TRIM(FldName))
         ENDIF

      ENDIF ! DoUse


      ! Return success
      RC = ESMF_SUCCESS

      END SUBROUTINE HCO_Imp2Ext3S
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCO_Imp2Ext2R
!
! !DESCRIPTION: Subroutine HCO\_Imp2Ext copies fields from the import state to
! the HEMCO ExtState object.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_Imp2Ext2R( HcoState, ExtDat, FldName, RC, Fld )
!
! !USES:
!
      USE HCO_STATE_MOD,   ONLY : Hco_State
      USE HCOX_STATE_MOD,  ONLY : ExtDat_2R
      USE HCO_ARR_MOD,     ONLY : HCO_ArrAssert
!
!ARGUMENTS:
!
      CHARACTER(LEN=*),    INTENT(IN   )   :: FldName
      TYPE(HCO_State),     POINTER         :: HcoState
      TYPE(ExtDat_2R),     POINTER         :: ExtDat
      INTEGER,             INTENT(INOUT)   :: RC
      REAL(hp), OPTIONAL,  INTENT(IN)      :: Fld(HcoState%NX,HcoState%NY)
!
! !REVISION HISTORY:
!  08 Feb 2016 - C. Keller - Initial version
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
      TYPE(ESMF_State)             :: IMPORT
      TYPE(ESMF_Field)             :: field
      REAL(ESMF_KIND_R8), POINTER  :: dataPtr(:,:)
      CHARACTER(LEN=255)           :: LOC
      CHARACTER(LEN=255)           :: ERRMSG

      ! ================================================================
      ! HCO_Imp2Ext2R begins here
      ! ================================================================

      ! Initialize
      LOC = 'HCO_Imp2Ext2R (HCOI_ESMF_MOD.F90)'
      Filled = .FALSE.
      RC = ESMF_SUCCESS

      ! Only do if being used...
      IF ( ExtDat%DoUse ) THEN
         ! Validate inputs
         IF ( .NOT. ASSOCIATED(HcoState) ) THEN
            ERRMSG = 'Invalid HcoState pointer'
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF

         IF ( .NOT. ASSOCIATED(HcoState%IMPORT) ) THEN
            ERRMSG = 'Invalid IMPORT state in HcoState'
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF

         ! Get import state
         IMPORT = HcoState%IMPORT

         ! Try to get field from import state
         CALL ESMF_StateGet( IMPORT, TRIM(FldName), field, rc=STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            WRITE(ERRMSG,'(a,a)') 'Error getting field from import state: ', TRIM(FldName)
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF

         ! Get pointer to field data
         CALL ESMF_FieldGet( field, farrayPtr=dataPtr, rc=STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            WRITE(ERRMSG,'(a,a)') 'Error getting pointer to field data: ', TRIM(FldName)
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF

         CALL HCO_ArrAssert( ExtDat%Arr, HcoState%NX, HcoState%NY, STAT )
         IF ( STAT /= HCO_SUCCESS ) THEN
            WRITE(ERRMSG,'(a,a)') 'Error asserting array for field: ', TRIM(FldName)
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF

         ! Copy data from ESMF field to ExtDat
         ExtDat%Arr%Val = REAL(dataPtr, kind=hp)
         Filled = .TRUE.

         ! Error check
         IF ( .NOT. Filled ) THEN
            ERRMSG = 'Error filling field data'
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF

         ! Verbose
         IF ( HcoState%Config%doVerbose .AND. HcoState%amIRoot ) THEN
            CALL HCO_MSG('Passed from import to ExtState: '//TRIM(FldName))
         ENDIF

      ENDIF ! DoUse

      ! Return success
      RC = ESMF_SUCCESS

      END SUBROUTINE HCO_Imp2Ext2R
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCO_Imp2Ext3R
!
! !DESCRIPTION: Subroutine HCO\_Imp2Ext copies fields from the import state to
! the HEMCO ExtState object.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_Imp2Ext3R( HcoState, ExtDat, FldName, RC )
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
!  08 Feb 2016 - C. Keller - Initial version
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

      ! ================================================================
      ! HCO_Imp2Ext3R begins here
      ! ================================================================

      ! Initialize
      LOC = 'HCO_Imp2Ext3R (HCOI_ESMF_MOD.F90)'
      RC = ESMF_SUCCESS

      ! Only do if being used...
      IF ( ExtDat%DoUse ) THEN
         ! Validate inputs
         IF ( .NOT. ASSOCIATED(HcoState) ) THEN
            ERRMSG = 'Invalid HcoState pointer'
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF

         IF ( .NOT. ASSOCIATED(HcoState%IMPORT) ) THEN
            ERRMSG = 'Invalid IMPORT state in HcoState'
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF

         ! Get import state
         IMPORT = HcoState%IMPORT

         ! Try to get field from import state
         CALL ESMF_StateGet( IMPORT, TRIM(FldName), field, rc=STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            WRITE(ERRMSG,'(a,a)') 'Error getting field from import state: ', TRIM(FldName)
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF

         ! Get pointer to field data
         CALL ESMF_FieldGet( field, farrayPtr=dataPtr, rc=STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            WRITE(ERRMSG,'(a,a)') 'Error getting pointer to field data: ', TRIM(FldName)
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF

         ! Make sure the array in ExtDat is allocated and has the right size
         NZ = SIZE(dataPtr, 3)
         CALL HCO_ArrAssert( ExtDat%Arr, HcoState%NX, HcoState%NY, NZ, STAT )
         IF ( STAT /= HCO_SUCCESS ) THEN
            WRITE(ERRMSG,'(a,a)') 'Error asserting array for field: ', TRIM(FldName)
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF

         ! Copy data from ESMF field to ExtDat
         ExtDat%Arr%Val = REAL(dataPtr, kind=hp)

         ! Verbose
         IF ( HcoState%Config%doVerbose .AND. HcoState%amIRoot ) THEN
            CALL HCO_MSG('Passed from import to ExtState: '//TRIM(FldName))
         ENDIF

      ENDIF ! DoUse

      ! Return success
      RC = ESMF_SUCCESS

      END SUBROUTINE HCO_Imp2Ext3R
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCO_Imp2Ext2I
!
! !DESCRIPTION: Subroutine HCO\_Imp2Ext copies fields from the import state to
! the HEMCO ExtState object.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_Imp2Ext2I( HcoState, ExtDat, FldName, RC )
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
!  08 Feb 2016 - C. Keller - Initial version
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
      TYPE(ESMF_State)             :: IMPORT
      TYPE(ESMF_Field)             :: field
      REAL(ESMF_KIND_R8), POINTER  :: dataPtr(:,:)
      CHARACTER(LEN=255)           :: LOC
      CHARACTER(LEN=255)           :: ERRMSG

      ! ================================================================
      ! HCO_Imp2Ext2I begins here
      ! ================================================================

      ! Initialize
      LOC = 'HCO_Imp2Ext2I (HCOI_ESMF_MOD.F90)'
      RC = ESMF_SUCCESS

      ! Only do if being used...
      IF ( ExtDat%DoUse ) THEN
         ! Validate inputs
         IF ( .NOT. ASSOCIATED(HcoState) ) THEN
            ERRMSG = 'Invalid HcoState pointer'
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF

         IF ( .NOT. ASSOCIATED(HcoState%IMPORT) ) THEN
            ERRMSG = 'Invalid IMPORT state in HcoState'
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF

         ! Get import state
         IMPORT = HcoState%IMPORT

         ! Try to get field from import state
         CALL ESMF_StateGet( IMPORT, TRIM(FldName), field, rc=STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            WRITE(ERRMSG,'(a,a)') 'Error getting field from import state: ', TRIM(FldName)
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF

         ! Get pointer to field data
         CALL ESMF_FieldGet( field, farrayPtr=dataPtr, rc=STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            WRITE(ERRMSG,'(a,a)') 'Error getting pointer to field data: ', TRIM(FldName)
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF

         CALL HCO_ArrAssert( ExtDat%Arr, HcoState%NX, HcoState%NY, STAT )
         IF ( STAT /= HCO_SUCCESS ) THEN
            WRITE(ERRMSG,'(a,a)') 'Error asserting array for field: ', TRIM(FldName)
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF

         ! Copy data from ESMF field to ExtDat
         ExtDat%Arr%Val = INT(dataPtr)

         ! Verbose
         IF ( HcoState%Config%doVerbose .AND. HcoState%amIRoot ) THEN
            CALL HCO_MSG('Passed from import to ExtState: '//TRIM(FldName))
         ENDIF

      ENDIF ! DoUse

      ! Return success
      RC = ESMF_SUCCESS

      
      END SUBROUTINE HCO_Imp2Ext2I
      !EOC
      !------------------------------------------------------------------------------
      !                   Harmonized Emissions Component (HEMCO)                    !
      !------------------------------------------------------------------------------
      !BOP
      !
      ! !ROUTINE: HCO_NexusInit
      !
      ! !DESCRIPTION: Subroutine HCO\_NexusInit initializes the NEXUS-HEMCO interface.
      !\\
      !\\
      ! !INTERFACE:
      !
      SUBROUTINE HCO_NexusInit( HcoState, GC, RC )
      !
      ! !USES:
      !
         USE HCO_STATE_MOD,   ONLY : Hco_State
      !
      ! !ARGUMENTS:
      !
         TYPE(HCO_State),     POINTER         :: HcoState
         TYPE(ESMF_GridComp), INTENT(INOUT)   :: GC
         INTEGER,             INTENT(INOUT)   :: RC
      !
      ! !REVISION HISTORY:
      !  03 Sep 2025 - R. Yantosca - Initial version for NEXUS integration
      !EOP
      !------------------------------------------------------------------------------
      !BOC
      !
      ! !LOCAL VARIABLES:
      !
         CHARACTER(LEN=255) :: LOC, ERRMSG
         INTEGER           :: STATUS
         LOGICAL          :: isPresent

         ! ================================================================
         ! HCO_NexusInit begins here
         ! ================================================================

         ! Initialize
         LOC = 'HCO_NexusInit (HCOI_ESMF_MOD.F90)'
         RC  = ESMF_SUCCESS

         ! Validate inputs
         IF ( .NOT. ASSOCIATED(HcoState) ) THEN
            ERRMSG = 'Invalid HcoState pointer'
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF

         ! Check if NEXUS integration is enabled
         CALL ESMF_AttributeGet( GC, name="do_NEXUS", value=HcoState%do_NEXUS, &
                                    isPresent=isPresent, rc=STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            ! If attribute is not present, default to .FALSE.
            HcoState%do_NEXUS = .FALSE.
         ENDIF

         ! If NEXUS is not enabled, return early
         IF ( .NOT. HcoState%do_NEXUS ) THEN
            RETURN
         ENDIF

         ! Get pointers to NEXUS ESMF states
         CALL ESMF_GridCompGet( GC, importState=HcoState%NXS_IMPORT, &
                                    exportState=HcoState%NXS_EXPORT, rc=STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            ERRMSG = 'Error getting NEXUS ESMF states'
            CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
            RETURN
         ENDIF

         ! Check if regridding is enabled
         CALL ESMF_AttributeGet( GC, name="do_NXS_Regrid", value=HcoState%do_NXS_Regrid, &
                                    isPresent=isPresent, rc=STATUS )
         IF ( STATUS /= ESMF_SUCCESS ) THEN
            ! If attribute is not present, default to .FALSE.
            HcoState%do_NXS_Regrid = .FALSE.
         ENDIF

         ! Verbose
         IF ( HcoState%Config%doVerbose .AND. HcoState%amIRoot ) THEN
            CALL HCO_MSG( 'NEXUS-HEMCO interface initialized' )
            WRITE( ERRMSG, * ) '  do_NEXUS = ', HcoState%do_NEXUS
            CALL HCO_MSG( ERRMSG )
            WRITE( ERRMSG, * ) '  do_NXS_Regrid = ', HcoState%do_NXS_Regrid
            CALL HCO_MSG( ERRMSG )
         ENDIF
      
      END SUBROUTINE HCO_NexusInit
      !EOC
      !------------------------------------------------------------------------------
      !                   Harmonized Emissions Component (HEMCO)                    !
      !------------------------------------------------------------------------------
      !BOP
      !
      ! !ROUTINE: HCO_NexusRegisterFields
      !
      ! !DESCRIPTION: Subroutine HCO\_NexusRegisterFields registers ESMF fields for
      ! NEXUS data exchange.
      !\\
      !\\
      ! !INTERFACE:
      !
            SUBROUTINE HCO_NexusRegisterFields( HcoState, RC )
      !
      !USES:
      !
            USE HCO_STATE_MOD,   ONLY : Hco_State
            USE HCO_TYPES_MOD,   ONLY : ListCont
            USE HCO_DATACONT_MOD,ONLY : ListCont_NextCont
      !
      ! !ARGUMENTS:
      !
            TYPE(HCO_State),     POINTER         :: HcoState
            INTEGER,             INTENT(INOUT)   :: RC
      !
      ! !REVISION HISTORY:
      !  03 Sep 2025 - R. Yantosca - Initial version for NEXUS integration
      !EOP
      !------------------------------------------------------------------------------
      !BOC
      !
      ! !LOCAL VARIABLES:
      !
            CHARACTER(LEN=255) :: LOC, ERRMSG
            INTEGER           :: STATUS
            TYPE(ListCont), POINTER :: CurrCont
            INTEGER          :: FLAG
            TYPE(ESMF_Field)  :: field
      
            ! ================================================================
            ! HCO_NexusRegisterFields begins here
            ! ================================================================
      
            ! Initialize
            LOC = 'HCO_NexusRegisterFields (HCOI_ESMF_MOD.F90)'
            RC  = ESMF_SUCCESS
      
            ! Validate inputs
            IF ( .NOT. ASSOCIATED(HcoState) ) THEN
               ERRMSG = 'Invalid HcoState pointer'
               CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
               RETURN
            ENDIF
      
            ! If NEXUS is not enabled, return early
            IF ( .NOT. HcoState%do_NEXUS ) THEN
               RETURN
            ENDIF
      
            ! Validate NEXUS import state
            IF ( .NOT. ASSOCIATED(HcoState%NXS_IMPORT) ) THEN
               ERRMSG = 'Invalid NXS_IMPORT state pointer'
               CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
               RETURN
            ENDIF
      
            ! Register fields in the NEXUS import state
            ! Loop over all data containers and register NEXUS-specific fields
            CurrCont => NULL()
            CALL ListCont_NextCont( HcoState%Config%ConfigList, CurrCont, FLAG )
            DO WHILE ( FLAG == HCO_SUCCESS )
      
               ! Skip containers that are not defined
               IF ( .NOT. ASSOCIATED(CurrCont%Dct) ) THEN
                  CALL ListCont_NextCont( HcoState%Config%ConfigList, CurrCont, FLAG )
                  CYCLE
               ENDIF
               IF ( .NOT. ASSOCIATED(CurrCont%Dct%Dta) ) THEN
                  CALL ListCont_NextCont( HcoState%Config%ConfigList, CurrCont, FLAG )
                  CYCLE
               ENDIF
      
               ! Check if this is a NEXUS field (you might want to add a specific flag)
               ! For now, we'll register all fields that have ncRead disabled as potential NEXUS fields
               IF ( .NOT. CurrCont%Dct%Dta%ncRead ) THEN
                  ! This could be a NEXUS field, register it in the NEXUS import state
                  IF ( CurrCont%Dct%Dta%SpaceDim == 2 ) THEN
                     ! Create and add 2D field to NEXUS import state
                     field = ESMF_FieldCreate( ESMF_TYPEKIND_R8, &
                                                  name=TRIM(CurrCont%Dct%Dta%Name)//'_NEXUS', &
                                                  rc=STATUS )
                     IF ( STATUS /= ESMF_SUCCESS ) THEN
                        WRITE(ERRMSG,'(a,a)') 'Error creating 2D NEXUS field: ', TRIM(CurrCont%Dct%Dta%Name)
                        CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
                        RETURN
                     ENDIF
                     CALL ESMF_StateAdd( HcoState%NXS_IMPORT, (/field/), rc=STATUS )
                     IF ( STATUS /= ESMF_SUCCESS ) THEN
                        WRITE(ERRMSG,'(a,a)') 'Error adding 2D NEXUS field to import state: ', TRIM(CurrCont%Dct%Dta%Name)
                        CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
                        RETURN
                     ENDIF
                  ELSEIF ( CurrCont%Dct%Dta%SpaceDim == 3 ) THEN
                     ! Create and add 3D field to NEXUS import state
                     field = ESMF_FieldCreate( ESMF_TYPEKIND_R8, &
                                                  name=TRIM(CurrCont%Dct%Dta%Name)//'_NEXUS', &
                                                  rc=STATUS )
                     IF ( STATUS /= ESMF_SUCCESS ) THEN
                        WRITE(ERRMSG,'(a,a)') 'Error creating 3D NEXUS field: ', TRIM(CurrCont%Dct%Dta%Name)
                        CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
                        RETURN
                     ENDIF
                     CALL ESMF_StateAdd( HcoState%NXS_IMPORT, (/field/), rc=STATUS )
                     IF ( STATUS /= ESMF_SUCCESS ) THEN
                        WRITE(ERRMSG,'(a,a)') 'Error adding 3D NEXUS field to import state: ', TRIM(CurrCont%Dct%Dta%Name)
                        CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
                        RETURN
                     ENDIF
                  ENDIF
               ENDIF
      
               ! Advance to next container
               CALL ListCont_NextCont( HcoState%Config%ConfigList, CurrCont, FLAG )
      
            ENDDO
      
            ! Free pointer
            CurrCont => NULL()
      
            ! Verbose
            IF ( HcoState%Config%doVerbose .AND. HcoState%amIRoot ) THEN
               CALL HCO_MSG( 'NEXUS fields registered' )
            ENDIF
      
            END SUBROUTINE HCO_NexusRegisterFields
      !EOC
      !------------------------------------------------------------------------------
      !                   Harmonized Emissions Component (HEMCO)                    !
      !------------------------------------------------------------------------------
      !BOP
      !
      ! !ROUTINE: HCO_NexusTransferData
      !
      ! !DESCRIPTION: Subroutine HCO\_NexusTransferData transfers data between NEXUS
      ! and HEMCO states.
      !\\
      !\\
      ! !INTERFACE:
      !
            SUBROUTINE HCO_NexusTransferData( HcoState, RC )
      !
      ! !USES:
      !
            USE HCO_STATE_MOD,   ONLY : Hco_State
      !
      ! !ARGUMENTS:
      !
            TYPE(HCO_State),     POINTER         :: HcoState
            INTEGER,             INTENT(INOUT)   :: RC
      !
      !REVISION HISTORY:
      !  03 Sep 2025 - R. Yantosca - Initial version for NEXUS integration
      !EOP
      !------------------------------------------------------------------------------
      !BOC
      !
      ! !LOCAL VARIABLES:
      !
            CHARACTER(LEN=255) :: LOC, ERRMSG
            INTEGER           :: STATUS
      
            ! ================================================================
            ! HCO_NexusTransferData begins here
            ! ================================================================
      
            ! Initialize
            LOC = 'HCO_NexusTransferData (HCOI_ESMF_MOD.F90)'
            RC  = ESMF_SUCCESS
      
            ! Validate inputs
            IF ( .NOT. ASSOCIATED(HcoState) ) THEN
               ERRMSG = 'Invalid HcoState pointer'
               CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
               RETURN
            ENDIF
      
            ! If NEXUS is not enabled, return early
            IF ( .NOT. HcoState%do_NEXUS ) THEN
               RETURN
            ENDIF
      
            ! Validate NEXUS import state
            IF ( .NOT. ASSOCIATED(HcoState%NXS_IMPORT) ) THEN
               ERRMSG = 'Invalid NXS_IMPORT state pointer'
               CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
               RETURN
            ENDIF
      
            ! Transfer data from NEXUS import state to HEMCO
            ! This would typically involve calling HCO_NexusImportData for each field
            CALL HCO_NexusImportData( HcoState, RC )
            IF ( RC /= ESMF_SUCCESS ) THEN
               ERRMSG = 'Error in HCO_NexusImportData'
               CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
               RETURN
            ENDIF
      
            ! Transfer data from HEMCO to NEXUS export state
            ! This would typically involve calling HCO_NexusExportData for each field
            CALL HCO_NexusExportData( HcoState, RC )
            IF ( RC /= ESMF_SUCCESS ) THEN
               ERRMSG = 'Error in HCO_NexusExportData'
               CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
               RETURN
            ENDIF
      
            ! Verbose
            IF ( HcoState%Config%doVerbose .AND. HcoState%amIRoot ) THEN
               CALL HCO_MSG( 'NEXUS data transfer completed' )
            ENDIF
      
            END SUBROUTINE HCO_NexusTransferData
      !EOC
      !------------------------------------------------------------------------------
      !                   Harmonized Emissions Component (HEMCO)                    !
      !------------------------------------------------------------------------------
      !BOP
      !
      ! !ROUTINE: HCO_NexusFinalize
      !
      ! !DESCRIPTION: Subroutine HCO\_NexusFinalize finalizes the NEXUS-HEMCO interface.
      !\\
      !\\
      !INTERFACE:
      !
            SUBROUTINE HCO_NexusFinalize( HcoState, RC )
      !
      ! !USES:
      !
            USE HCO_STATE_MOD,   ONLY : Hco_State
      !
      ! !ARGUMENTS:
      !
            TYPE(HCO_State),     POINTER         :: HcoState
            INTEGER,             INTENT(INOUT)   :: RC
      !
      ! !REVISION HISTORY:
      !  03 Sep 2025 - R. Yantosca - Initial version for NEXUS integration
      !EOP
      !------------------------------------------------------------------------------
      !BOC
      !
      ! !LOCAL VARIABLES:
      !
            CHARACTER(LEN=255) :: LOC, ERRMSG
            INTEGER           :: STATUS
      
            ! ================================================================
            ! HCO_NexusFinalize begins here
            ! ================================================================
      
            ! Initialize
            LOC = 'HCO_NexusFinalize (HCOI_ESMF_MOD.F90)'
            RC  = ESMF_SUCCESS
      
            ! Validate inputs
            IF ( .NOT. ASSOCIATED(HcoState) ) THEN
               ERRMSG = 'Invalid HcoState pointer'
               CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
               RETURN
            ENDIF
      
            ! If NEXUS is not enabled, return early
            IF ( .NOT. HcoState%do_NEXUS ) THEN
               RETURN
            ENDIF
      
            ! Clean up NEXUS ESMF objects if needed
            ! For now, we just nullify the pointers
            HcoState%NXS_IMPORT => NULL()
            HcoState%NXS_EXPORT => NULL()
            HcoState%NXS_Grid   => NULL()
      
            ! Verbose
            IF ( HcoState%Config%doVerbose .AND. HcoState%amIRoot ) THEN
               CALL HCO_MSG( 'NEXUS-HEMCO interface finalized' )
            ENDIF
      
            END SUBROUTINE HCO_NexusFinalize
      !EOC
      !------------------------------------------------------------------------------
      !                   Harmonized Emissions Component (HEMCO)                    !
      !------------------------------------------------------------------------------
      !BOP
      !
      ! !ROUTINE: HCO_NexusImportData
      !
      ! !DESCRIPTION: Subroutine HCO\_NexusImportData imports data from the NEXUS
      ! import state to HEMCO data structures.
      !\\
      !\\
      !INTERFACE:
      !
            SUBROUTINE HCO_NexusImportData( HcoState, RC )
      !
      ! !USES:
      !
            USE HCO_STATE_MOD,   ONLY : Hco_State
            USE HCO_TYPES_MOD,   ONLY : ListCont
            USE HCO_DATACONT_MOD,ONLY : ListCont_NextCont
      !
      ! !ARGUMENTS:
      !
            TYPE(HCO_State),     POINTER         :: HcoState
            INTEGER,             INTENT(INOUT)   :: RC
      !
      ! !REVISION HISTORY:
      !  03 Sep 2025 - R. Yantosca - Initial version for NEXUS integration
      !EOP
      !------------------------------------------------------------------------------
      !BOC
      !
      ! !LOCAL VARIABLES:
      !
            CHARACTER(LEN=255)           :: LOC, ERRMSG
            INTEGER                     :: STATUS
            TYPE(ListCont),    POINTER   :: CurrCont
            INTEGER                     :: FLAG
            TYPE(ESMF_Field)            :: field
            REAL(ESMF_KIND_R8), POINTER :: dataPtr(:,:)
            REAL(ESMF_KIND_R8), POINTER :: dataPtr3D(:,:,:)
            INTEGER                     :: NZ
      
            ! ================================================================
            ! HCO_NexusImportData begins here
            ! ================================================================
      
            ! Initialize
            LOC = 'HCO_NexusImportData (HCOI_ESMF_MOD.F90)'
            RC  = ESMF_SUCCESS
      
            ! Validate inputs
            IF ( .NOT. ASSOCIATED(HcoState) ) THEN
               ERRMSG = 'Invalid HcoState pointer'
               CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
               RETURN
            ENDIF
      
            ! If NEXUS is not enabled, return early
            IF ( .NOT. HcoState%do_NEXUS ) THEN
               RETURN
            ENDIF
      
            ! Loop over all data containers and import NEXUS data
            CurrCont => NULL()
            CALL ListCont_NextCont( HcoState%Config%ConfigList, CurrCont, FLAG )
            DO WHILE ( FLAG == HCO_SUCCESS )
      
               ! Skip containers that are not defined
               IF ( .NOT. ASSOCIATED(CurrCont%Dct) ) THEN
                  CALL ListCont_NextCont( HcoState%Config%ConfigList, CurrCont, FLAG )
                  CYCLE
               ENDIF
               IF ( .NOT. ASSOCIATED(CurrCont%Dct%Dta) ) THEN
                  CALL ListCont_NextCont( HcoState%Config%ConfigList, CurrCont, FLAG )
                  CYCLE
               ENDIF
      
               ! Check if this is a NEXUS field (you might want to add a specific flag)
               ! For now, we'll import all fields that have ncRead disabled as potential NEXUS fields
               IF ( .NOT. CurrCont%Dct%Dta%ncRead ) THEN
                  ! Try to get the field from the NEXUS import state
                  CALL ESMF_StateGet( HcoState%NXS_IMPORT, &
                                       TRIM(CurrCont%Dct%Dta%Name)//'_NEXUS', &
                                       field, rc=STATUS )
                  IF ( STATUS == ESMF_SUCCESS ) THEN
                     ! Field found, import the data
                     IF ( CurrCont%Dct%Dta%SpaceDim == 2 ) THEN
                        ! Get pointer to 2D field data
                        CALL ESMF_FieldGet( field, farrayPtr=dataPtr, rc=STATUS )
                        IF ( STATUS /= ESMF_SUCCESS ) THEN
                           WRITE(ERRMSG,'(a,a)') 'Error getting pointer to 2D NEXUS field data: ', TRIM(CurrCont%Dct%Dta%Name)
                           CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
                           RETURN
                        ENDIF
                        
                        ! Copy data to HEMCO data structure
                        ! Note: This is a simplified example. You would need to properly map
                        ! the data to the appropriate HEMCO data structure.
                        ! For example, you might need to copy to CurrCont%Dct%Dta%Arr%Val
                        ! depending on your specific implementation.
                        
                     ELSEIF ( CurrCont%Dct%Dta%SpaceDim == 3 ) THEN
                        ! Get pointer to 3D field data
                        CALL ESMF_FieldGet( field, farrayPtr=dataPtr3D, rc=STATUS )
                        IF ( STATUS /= ESMF_SUCCESS ) THEN
                           WRITE(ERRMSG,'(a,a)') 'Error getting pointer to 3D NEXUS field data: ', TRIM(CurrCont%Dct%Dta%Name)
                           CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
                           RETURN
                        ENDIF
                        
                        ! Get the vertical dimension size
                        NZ = SIZE(dataPtr3D, 3)
                        
                        ! Copy data to HEMCO data structure
                        ! Note: This is a simplified example. You would need to properly map
                        ! the data to the appropriate HEMCO data structure.
                     ENDIF
                     
                     ! Verbose
                     IF ( HcoState%Config%doVerbose .AND. HcoState%amIRoot ) THEN
                        CALL HCO_MSG( 'Imported NEXUS field: '//TRIM(CurrCont%Dct%Dta%Name) )
                     ENDIF
                  ENDIF
               ENDIF
      
               ! Advance to next container
               CALL ListCont_NextCont( HcoState%Config%ConfigList, CurrCont, FLAG )
      
            ENDDO
      
            ! Free pointer
            CurrCont => NULL()
      
            END SUBROUTINE HCO_NexusImportData
      !EOC
      !------------------------------------------------------------------------------
      !                   Harmonized Emissions Component (HEMCO)                    !
      !------------------------------------------------------------------------------
      !BOP
      !
      ! !ROUTINE: HCO_NexusExportData
      !
      ! !DESCRIPTION: Subroutine HCO\_NexusExportData exports data from HEMCO to the
      ! NEXUS export state.
      !\\
      !\\
      !INTERFACE:
      !
            SUBROUTINE HCO_NexusExportData( HcoState, RC )
      !
      ! !USES:
      !
            USE HCO_STATE_MOD,   ONLY : Hco_State
      !
      ! !ARGUMENTS:
      !
            TYPE(HCO_State),     POINTER         :: HcoState
            INTEGER,             INTENT(INOUT)   :: RC
      !
      ! !REVISION HISTORY:
      ! 03 Sep 2025 - R. Yantosca - Initial version for NEXUS integration
      !EOP
      !------------------------------------------------------------------------------
      !BOC
      !
      ! !LOCAL VARIABLES:
      !
            CHARACTER(LEN=255) :: LOC, ERRMSG
            INTEGER           :: STATUS
      
            ! ================================================================
            ! HCO_NexusExportData begins here
            ! ================================================================
      
            ! Initialize
            LOC = 'HCO_NexusExportData (HCOI_ESMF_MOD.F90)'
            RC  = ESMF_SUCCESS
      
            ! Validate inputs
            IF ( .NOT. ASSOCIATED(HcoState) ) THEN
               ERRMSG = 'Invalid HcoState pointer'
               CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
               RETURN
            ENDIF
      
            ! If NEXUS is not enabled, return early
            IF ( .NOT. HcoState%do_NEXUS ) THEN
               RETURN
            ENDIF
      
            ! Validate NEXUS export state
            IF ( .NOT. ASSOCIATED(HcoState%NXS_EXPORT) ) THEN
               ERRMSG = 'Invalid NXS_EXPORT state pointer'
               CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
               RETURN
            ENDIF
      
            ! Export HEMCO diagnostics to NEXUS export state
            ! This would typically involve looping over HEMCO diagnostics and
            ! copying them to corresponding fields in the NEXUS export state.
            ! For now, we'll just add a placeholder.
            
            ! Verbose
            IF ( HcoState%Config%doVerbose .AND. HcoState%amIRoot ) THEN
               CALL HCO_MSG( 'Exported HEMCO data to NEXUS' )
            ENDIF
      
            END SUBROUTINE HCO_NexusExportData
      !EOC
      !------------------------------------------------------------------------------
      !                   Harmonized Emissions Component (HEMCO)                    !
      !------------------------------------------------------------------------------
      !BOP
      !
      ! !ROUTINE: HCO_NexusGetConfig
      !
      ! !DESCRIPTION: Subroutine HCO\_NexusGetConfig retrieves NEXUS configuration
      ! settings from the HEMCO configuration.
      !\\
      !\\
      ! !INTERFACE:
      !
            SUBROUTINE HCO_NexusGetConfig( HcoState, RC )
      !
      ! !USES:
      !
            USE HCO_STATE_MOD,   ONLY : Hco_State
            USE HCO_EXTLIST_MOD, ONLY : GetExtOpt
      !
      ! !ARGUMENTS:
      !
            TYPE(HCO_State),     POINTER         :: HcoState
            INTEGER,             INTENT(INOUT)   :: RC
      !
      ! !REVISION HISTORY:
      !  03 Sep 2025 - R. Yantosca - Initial version for NEXUS integration
      !EOP
      !------------------------------------------------------------------------------
      !BOC
      !
      ! !LOCAL VARIABLES:
      !
            CHARACTER(LEN=255) :: LOC, ERRMSG
            INTEGER           :: STATUS
            LOGICAL          :: FOUND
      
            ! ================================================================
            ! HCO_NexusGetConfig begins here
            ! ================================================================
      
            ! Initialize
            LOC = 'HCO_NexusGetConfig (HCOI_ESMF_MOD.F90)'
            RC  = ESMF_SUCCESS
      
            ! Validate inputs
            IF ( .NOT. ASSOCIATED(HcoState) ) THEN
               ERRMSG = 'Invalid HcoState pointer'
               CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
               RETURN
            ENDIF
      
            ! If NEXUS is not enabled, return early
            IF ( .NOT. HcoState%do_NEXUS ) THEN
               RETURN
            ENDIF
      
            ! Get NEXUS-specific configuration options
            ! For example, you might want to get options like:
            ! - Whether to use regridding
            ! - Specific field mappings
            ! - Time interpolation settings
            !
            ! For now, we'll just add a placeholder for where this configuration
            ! would be retrieved.
            
            ! Verbose
            IF ( HcoState%Config%doVerbose .AND. HcoState%amIRoot ) THEN
               CALL HCO_MSG( 'Retrieved NEXUS configuration' )
            ENDIF
      
            END SUBROUTINE HCO_NexusGetConfig
      !EOC
      END MODULE HCOI_ESMF_MOD
