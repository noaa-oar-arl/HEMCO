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
  USE HCO_ERROR_MOD
  USE HCO_Types_Mod

  USE ESMF


  USE NUOPC, ONLY: NUOPC_CompAttributeSet, NUOPC_CompDerive, NUOPC_ModelGet, &
                NUOPC_CompSetEntryPoint, NUOPC_CompSetTimeStep


  USE NUOPC_Model, &
       ONLY: model_routine_SS           => SetServices, &
             model_label_Advance        => label_Advance, &
             model_label_DataInitialize => label_DataInitialize, &
             model_label_SetClock       => label_SetClock

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  ! ESMF environment only:
  PUBLIC :: HCO_SetServices_NUOPC
  PUBLIC :: HCO_SetExtState_NUOPC
  PUBLIC :: HCO_Imp2Ext_NUOPC
  PUBLIC :: HCOI_NUOPC_SS  ! Set services for this component
!
! !PRIVATE MEMBER FUNCTIONS:
!
  PRIVATE :: Diagn2Exp_NUOPC
  PRIVATE :: HCO_Imp2Ext2R_NUOPC
  PRIVATE :: HCO_Imp2Ext2S_NUOPC
  PRIVATE :: HCO_Imp2Ext2I_NUOPC
  PRIVATE :: HCO_Imp2Ext3R_NUOPC
  PRIVATE :: HCO_Imp2Ext3S_NUOPC
!
! !REVISION HISTORY:
!  29 Apr 2025 - Initial version created for pure ESMF/NUOPC interface
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !MODULE INTERFACES:
!
  INTERFACE HCO_Imp2Ext_NUOPC
     MODULE PROCEDURE HCO_Imp2Ext2R_NUOPC
     MODULE PROCEDURE HCO_Imp2Ext2S_NUOPC
     MODULE PROCEDURE HCO_Imp2Ext2I_NUOPC
     MODULE PROCEDURE HCO_Imp2Ext3R_NUOPC
     MODULE PROCEDURE HCO_Imp2Ext3S_NUOPC
  END INTERFACE HCO_Imp2Ext_NUOPC

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
! !ROUTINE: HCO_SetServices_NUOPC
!
! !DESCRIPTION: Subroutine HCO\_SetServices\_NUOPC registers all required HEMCO
! data so that it can be imported through the ESMF import state.
! This routine determines all required HEMCO input fields from the HEMCO
! configuration file.
!\\
!\\
! The field names provided in the configuration must match the names in the
! HEMCO configuration file! Also, all time settings (average and update interval)
! and data units need to be properly specified.
!\\
!\\
! This routine also prepares an emissions export field for every species
! found in the HEMCO configuration file.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_SetServices_NUOPC( am_I_Root, GC, HcoConfig, &
                                      ConfigFile, RC )
!
! !USES:
!
      USE HCO_TYPES_MOD,    ONLY : ListCont
      USE HCO_DATACONT_MOD, ONLY : ListCont_NextCont
      USE HCO_CONFIG_MOD,   ONLY : Config_ReadFile
      USE HCO_EXTLIST_MOD,  ONLY : GetExtOpt
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
!  29 Apr 2025 - Initial version for pure ESMF/NUOPC interface
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      INTEGER                    :: LUN, ExtNr, Cat, Hier, SpaceDim
      INTEGER                    :: I, FLAG, nSpc, nDiagn
      INTEGER                    :: dimCount
      INTEGER                    :: localrc
      LOGICAL                    :: EOF
      LOGICAL                    :: FOUND, DefaultSet
      CHARACTER(LEN=31)          :: cName, SpcName, OutUnit
      CHARACTER(LEN=63)          :: DefaultSNAME, DefaultLNAME, DefaultUnit
      CHARACTER(LEN=63)          :: SNAME, UnitName
      CHARACTER(LEN=127)         :: LNAME
      CHARACTER(LEN=63), POINTER :: Spc(:)
      TYPE(ListCont),    POINTER :: CurrCont
      CHARACTER(LEN=255)         :: LOC
      TYPE(ESMF_Field)           :: field
      TYPE(ESMF_StateItem_Flag)  :: itemType
      TYPE(ESMF_ArraySpec)       :: arraySpec

      ! ================================================================
      ! HCO_SetServices_NUOPC begins here
      ! ================================================================

      ! Init
      LOC      = 'HCO_SetServices_NUOPC (HCOI_ESMF_NUOPC_MOD.F90)'
      Spc      => NULL()
      CurrCont => NULL()
      RC = ESMF_SUCCESS

      ! ---------------------------------------------------------------------
      ! Read file into buffer
      ! ---------------------------------------------------------------------

      CALL Config_ReadFile( am_I_Root, HcoConfig, TRIM(ConfigFile), 0, RC )
      IF (RC /= HCO_SUCCESS) THEN
         CALL HCO_ERROR('Error reading configuration file', RC, THISLOC=LOC)
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
         ! Ignore containers with ncRead flag disabled. These are typically
         ! scalar fields directly read from the configuration file.
         IF ( .NOT. CurrCont%Dct%Dta%ncRead ) THEN

         ! Multiple data containers can use the same source data. In this
         ! case we only need to import the data once. The second, third, etc.
         ! containters registered for the same source data have been assigned
         ! lower DtaHome values (in hco_config_mod.F90), so skip this container
         ! if flag is not -999 (=default).
         ELSEIF ( CurrCont%Dct%DtaHome /= -999 ) THEN

         ! Set up ESMF_ArraySpec with the appropriate dimensions
         ELSEIF ( CurrCont%Dct%Dta%SpaceDim == 2 .OR. CurrCont%Dct%Dta%SpaceDim == 3 ) THEN

            ! Create field for import state
            CALL ESMF_StateIsField(GC%ImportState, &
                                  TRIM(CurrCont%Dct%Dta%ncFile), &
                                  itemType, RC=localrc)

            ! Only add if the field isn't already in the import state
            IF (itemType /= ESMF_STATEITEM_FIELD) THEN
               ! Set up array spec
               CALL ESMF_ArraySpecSet(arraySpec, typekind=ESMF_TYPEKIND_R4, &
                                     rank=CurrCont%Dct%Dta%SpaceDim, RC=localrc)
               IF (localrc /= ESMF_SUCCESS) THEN
                  RC = localrc
                  CALL HCO_ERROR('Error setting up ArraySpec', RC, THISLOC=LOC)
                  RETURN
               ENDIF

               ! Create the field
               field = ESMF_FieldCreate(name=TRIM(CurrCont%Dct%Dta%ncFile), &
                                       arraySpec=arraySpec, &
                                       gridToFieldMap=(/1,2/), &
                                       ungriddedLBound=(/1/), &
                                       ungriddedUBound=(/1/), &
                                       RC=localrc)
               IF (localrc /= ESMF_SUCCESS) THEN
                  RC = localrc
                  CALL HCO_ERROR('Error creating field', RC, THISLOC=LOC)
                  RETURN
               ENDIF

               ! Add field to import state
               CALL ESMF_StateAdd(GC%ImportState, (/field/), RC=localrc)
               IF (localrc /= ESMF_SUCCESS) THEN
                  RC = localrc
                  CALL HCO_ERROR('Error adding field to import state', RC, THISLOC=LOC)
                  RETURN
               ENDIF
            ENDIF

         ! Return w/ error if not 2D or 3D data
         ELSE
            RC = HCO_FAIL
            CALL HCO_ERROR('Only 2D or 3D data arrays are supported', RC, THISLOC=LOC)
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
      CALL DiagnFileOpen( HcoConfig, LUN, RC )
      IF ( RC /= HCO_SUCCESS ) THEN
         CALL HCO_ERROR('Error opening diagnostics file', RC, THISLOC=LOC)
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
                                   SpaceDim,  OutUnit, EOF,   RC,   &
                                   lName=lName, UnitName=UnitName )
            IF ( RC /= HCO_SUCCESS ) THEN
                CALL HCO_ERROR( 'Error reading diagnostics file', RC, THISLOC=LOC )
                RETURN
            ENDIF

            ! Leave here if end of file
            IF ( EOF ) EXIT

            ! Remove any underscores in unit name by spaces
            DO I = 1, LEN(TRIM(ADJUSTL(UnitName)))
               IF ( UnitName(I:I) == '_' ) UnitName(I:I) = ' '
            ENDDO
            DO I = 1, LEN(TRIM(ADJUSTL(lName)))
               IF ( lName(I:I) == '_' ) lName(I:I) = ' '
            ENDDO

            ! Add to export state - create field
            CALL Diagn2Exp_NUOPC(GC, cName, lName, UnitName, SpaceDim, RC)
            IF (RC /= ESMF_SUCCESS) THEN
               CALL HCO_ERROR('Error adding diagnostics to export state', RC, THISLOC=LOC)
               RETURN
            ENDIF

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
                      OptValBool=DefaultSet, FOUND=FOUND, RC=RC )
      IF ( .NOT. FOUND ) DefaultSet = .FALSE.
      IF ( DefaultSet ) THEN

         ! Search for default diagnostics variable prefix
         CALL GetExtOpt( HcoConfig, -999, 'DefaultDiagnSname', &
                         OptValChar=DefaultSNAME, FOUND=FOUND, RC=RC )
         IF ( .NOT. FOUND ) DefaultSNAME = 'HEMCO_EMIS_'

         CALL GetExtOpt( HcoConfig, -999, 'DefaultDiagnLname', &
                         OptValChar=DefaultLNAME, FOUND=FOUND, RC=RC )
         IF ( .NOT. FOUND ) DefaultLNAME = 'HEMCO_emissions_of_species_'

         ! Search for default diagnostics dimension
         CALL GetExtOpt( HcoConfig, -999, 'DefaultDiagnDim', &
                         OptValInt=SpaceDim, FOUND=FOUND, RC=RC )
         IF ( .NOT. FOUND ) SpaceDim = 3
         SpaceDim = MAX(MIN(SpaceDim,3),2)

         ! Get units
         CALL GetExtOpt( HcoConfig, -999, 'DefaultDiagnUnit', &
                         OptValChar=DefaultUnit, FOUND=FOUND, RC=RC )
         IF ( .NOT. FOUND ) DefaultUnit = 'kg m-2 s-1'

         ! Get # of species and species names
         nSpc = Config_GetnSpecies( HcoConfig )
         CALL Config_GetSpecNames( HcoConfig, Spc, nSpc, RC )
         IF ( RC /= HCO_SUCCESS ) THEN
            CALL HCO_ERROR('Error getting species names', RC, THISLOC=LOC)
            RETURN
         ENDIF

         ! Loop over all species and add to export state
         DO I = 1, nSpc
            SNAME = TRIM(DefaultSNAME)//TRIM(Spc(I))
            LNAME = TRIM(DefaultLNAME)//TRIM(Spc(I))
            CALL Diagn2Exp_NUOPC( GC, SNAME, LNAME, DefaultUnit, SpaceDim, RC )
            IF ( RC /= ESMF_SUCCESS ) THEN
               CALL HCO_ERROR('Error adding default diagnostics to export state', RC, THISLOC=LOC)
               RETURN
            ENDIF
         ENDDO
      ENDIF

      ! ---------------------------------------------------------------------
      ! Cleanup
      ! ---------------------------------------------------------------------
      IF ( ASSOCIATED(Spc) ) DEALLOCATE(Spc)

      ! Return success
      RETURN

      END SUBROUTINE HCO_SetServices_NUOPC
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: Diagn2Exp_NUOPC
!
! !DESCRIPTION: Subroutine Diagn2Exp\_NUOPC is a helper routine to add a potential
! HEMCO diagnostics to the Export state using pure ESMF/NUOPC.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE Diagn2Exp_NUOPC( GC, SNAME, LNAME, UNITS, NDIM, RC )
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
!  29 Apr 2025 - Initial version for pure ESMF/NUOPC interface
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      TYPE(ESMF_Field)      :: field
      TYPE(ESMF_ArraySpec)  :: arraySpec
      TYPE(ESMF_StateItem_Flag) :: itemType
      INTEGER               :: localrc

      ! ================================================================
      ! Diagn2Exp_NUOPC begins here
      ! ================================================================

      RC = ESMF_SUCCESS

      ! Check if field already exists
      CALL ESMF_StateIsField(GC%ExportState, TRIM(SNAME), itemType, RC=localrc)
      IF (localrc /= ESMF_SUCCESS) THEN
         RC = localrc
         RETURN
      ENDIF

      ! Only add if field doesn't already exist
      IF (itemType /= ESMF_STATEITEM_FIELD) THEN
         ! Set up array spec with appropriate dimensions
         CALL ESMF_ArraySpecSet(arraySpec, typekind=ESMF_TYPEKIND_R4, &
                               rank=NDIM, RC=localrc)
         IF (localrc /= ESMF_SUCCESS) THEN
            RC = localrc
            RETURN
         ENDIF

         ! Create field
         field = ESMF_FieldCreate(name=TRIM(SNAME), &
                                 arraySpec=arraySpec, &
                                 gridToFieldMap=(/1,2/), &
                                 ungriddedLBound=(/1/), &
                                 ungriddedUBound=(/1/), &
                                 RC=localrc)
         IF (localrc /= ESMF_SUCCESS) THEN
            RC = localrc
            RETURN
         ENDIF

         ! Add attributes for long name and units
         CALL ESMF_AttributeSet(field, name="long_name", value=TRIM(LNAME), RC=localrc)
         IF (localrc /= ESMF_SUCCESS) THEN
            RC = localrc
            RETURN
         ENDIF

         CALL ESMF_AttributeSet(field, name="units", value=TRIM(UNITS), RC=localrc)
         IF (localrc /= ESMF_SUCCESS) THEN
            RC = localrc
            RETURN
         ENDIF

         ! Add field to export state
         CALL ESMF_StateAdd(GC%ExportState, (/field/), RC=localrc)
         IF (localrc /= ESMF_SUCCESS) THEN
            RC = localrc
            RETURN
         ENDIF
      ENDIF

      ! Return success
      RC = ESMF_SUCCESS
      RETURN

      END SUBROUTINE Diagn2Exp_NUOPC
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCO_SetExtState_NUOPC
!
! !DESCRIPTION: Subroutine HCO\_SetExtState\_NUOPC tries to populate some
! fields of the ExtState object from the ESMF import state.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_SetExtState_NUOPC( HcoState, ExtState, RC )
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
!  29 Apr 2025 - Initial version for pure ESMF/NUOPC interface
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!

      ! ================================================================
      ! HCO_SetExtState_NUOPC begins here
      ! ================================================================

      ! Get pointers to fields for the External State
      CALL HCO_Imp2Ext_NUOPC ( HcoState, ExtState%BYNCY, 'BYNCY', RC )

      ! Add more fields as needed...

      ! Return success
      RC = HCO_SUCCESS

      END SUBROUTINE HCO_SetExtState_NUOPC
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCO_Imp2Ext2S_NUOPC
!
! !DESCRIPTION: Subroutine HCO\_Imp2Ext2S\_NUOPC copies fields from the import
! state to the HEMCO ExtState object for 2D single precision arrays.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_Imp2Ext2S_NUOPC( HcoState, ExtDat, FldName, RC )
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
!  29 Apr 2025 - Initial version for pure ESMF/NUOPC interface
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      TYPE(ESMF_Field)           :: field
      REAL, POINTER              :: Ptr2D(:,:) => NULL()
      INTEGER                    :: STAT, localrc

      ! ================================================================
      ! HCO_Imp2Ext2S_NUOPC begins here
      ! ================================================================

      RC = ESMF_SUCCESS

      ! Only do if being used...
      IF ( ExtDat%DoUse ) THEN
         ! Get field from import state
         CALL ESMF_StateGet(HcoState%IMPORT, TRIM(FldName), field, RC=localrc)
         IF (localrc /= ESMF_SUCCESS) THEN
            ! Field not found, return success but don't update ExtDat
            RC = ESMF_SUCCESS
            RETURN
         ENDIF

         ! Get pointer to data
         CALL ESMF_FieldGet(field, farrayPtr=Ptr2D, RC=localrc)
         IF (localrc /= ESMF_SUCCESS) THEN
            RC = localrc
            RETURN
         ENDIF

         ! Make sure ExtDat array is allocated and has the right size
         CALL HCO_ArrAssert(ExtDat%Arr, HcoState%NX, HcoState%NY, STAT)
         IF (STAT /= HCO_SUCCESS) THEN
            RC = STAT
            RETURN
         ENDIF

         ! Copy data from field to ExtDat, handling undefined values
         ExtDat%Arr%Val = 0.0
         WHERE (Ptr2D /= ESMF_UNDEF)
            ExtDat%Arr%Val = Ptr2D
         END WHERE

         ! Cleanup
         NULLIFY(Ptr2D)

         ! Verbose output
         IF (HcoState%Config%doVerbose .AND. HcoState%amIRoot) THEN
            CALL HCO_MSG('Passed from import to ExtState: '//TRIM(FldName))
         ENDIF
      ENDIF

      ! Return with success
      RC = ESMF_SUCCESS
      RETURN

      END SUBROUTINE HCO_Imp2Ext2S_NUOPC
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCO_Imp2Ext3S_NUOPC
!
! !DESCRIPTION: Subroutine HCO\_Imp2Ext3S\_NUOPC copies fields from the import
! state to the HEMCO ExtState object for 3D single precision arrays.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_Imp2Ext3S_NUOPC( HcoState, ExtDat, FldName, RC )
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
!  29 Apr 2025 - Initial version for pure ESMF/NUOPC interface
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      TYPE(ESMF_Field)           :: field
      REAL, POINTER              :: Ptr3D(:,:,:) => NULL()
      INTEGER                    :: L, NZ, OFF, STAT, localrc

      ! ================================================================
      ! HCO_Imp2Ext3S_NUOPC begins here
      ! ================================================================

      RC = ESMF_SUCCESS

      ! Only do if being used...
      IF ( ExtDat%DoUse ) THEN
         ! Get field from import state
         CALL ESMF_StateGet(HcoState%IMPORT, TRIM(FldName), field, RC=localrc)
         IF (localrc /= ESMF_SUCCESS) THEN
            ! Field not found, return success but don't update ExtDat
            RC = ESMF_SUCCESS
            RETURN
         ENDIF

         ! Get pointer to data
         CALL ESMF_FieldGet(field, farrayPtr=Ptr3D, RC=localrc)
         IF (localrc /= ESMF_SUCCESS) THEN
            RC = localrc
            RETURN
         ENDIF

         ! Make sure the array in ExtDat is allocated and has the right size
         NZ = SIZE(Ptr3D, 3)
         CALL HCO_ArrAssert(ExtDat%Arr, HcoState%NX, HcoState%NY, NZ, STAT)
         IF (STAT /= HCO_SUCCESS) THEN
            RC = STAT
            RETURN
         ENDIF

         ! Pass field to ExtDat, handling undefined values
         OFF = LBOUND(Ptr3D, 3)
         DO L = 1, NZ
            WHERE (Ptr3D(:,:,NZ-L+OFF) == ESMF_UNDEF)
               ExtDat%Arr%Val(:,:,L) = 0.0
            ELSEWHERE
               ExtDat%Arr%Val(:,:,L) = Ptr3D(:,:,NZ-L+OFF)
            END WHERE
         ENDDO

         ! Cleanup
         NULLIFY(Ptr3D)

         ! Verbose output
         IF (HcoState%Config%doVerbose .AND. HcoState%amIRoot) THEN
            CALL HCO_MSG('Passed from import to ExtState: '//TRIM(FldName))
         ENDIF
      ENDIF

      ! Return with success
      RC = ESMF_SUCCESS
      RETURN

      END SUBROUTINE HCO_Imp2Ext3S_NUOPC
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCO_Imp2Ext2R_NUOPC
!
! !DESCRIPTION: Subroutine HCO\_Imp2Ext2R\_NUOPC copies fields from the import
! state to the HEMCO ExtState object for 2D real*8 arrays.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_Imp2Ext2R_NUOPC( HcoState, ExtDat, FldName, RC, Fld )
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
!  29 Apr 2025 - Initial version for pure ESMF/NUOPC interface
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      TYPE(ESMF_Field)           :: field
      REAL, POINTER              :: Ptr2D(:,:) => NULL()
      INTEGER                    :: STAT, localrc
      LOGICAL                    :: Filled

      ! ================================================================
      ! HCO_Imp2Ext2R_NUOPC begins here
      ! ================================================================

      RC = ESMF_SUCCESS
      Filled = .FALSE.

      ! Only do if being used...
      IF ( ExtDat%DoUse ) THEN
         ! Make sure the array in ExtDat is allocated and has the right size
         CALL HCO_ArrAssert(ExtDat%Arr, HcoState%NX, HcoState%NY, STAT)
         IF (STAT /= HCO_SUCCESS) THEN
            RC = STAT
            RETURN
         ENDIF

         ! Initialize with zeros
         ExtDat%Arr%Val = 0.0

         ! Try to get field from import state
         CALL ESMF_StateGet(HcoState%IMPORT, TRIM(FldName), field, RC=localrc)
         IF (localrc == ESMF_SUCCESS) THEN
            ! Field found, get pointer to data
            CALL ESMF_FieldGet(field, farrayPtr=Ptr2D, RC=localrc)
            IF (localrc == ESMF_SUCCESS) THEN
               ! Copy data, handling undefined values
               WHERE (Ptr2D /= ESMF_UNDEF)
                  ExtDat%Arr%Val = Ptr2D
               END WHERE
               Filled = .TRUE.
               NULLIFY(Ptr2D)
            ENDIF
         ENDIF

         ! If not filled from import state, try using provided field
         IF (.NOT. Filled .AND. PRESENT(Fld)) THEN
            ExtDat%Arr%Val = Fld
            Filled = .TRUE.
         ENDIF

         ! Error check
         IF (.NOT. Filled) THEN
            CALL HCO_ERROR('Cannot fill '//TRIM(FldName), RC)
            RC = HCO_FAIL
            RETURN
         ENDIF

         ! Verbose output
         IF (HcoState%Config%doVerbose .AND. HcoState%amIRoot) THEN
            CALL HCO_MSG('Passed from import to ExtState: '//TRIM(FldName))
         ENDIF
      ENDIF

      ! Return with success
      RC = ESMF_SUCCESS
      RETURN

      END SUBROUTINE HCO_Imp2Ext2R_NUOPC
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCO_Imp2Ext3R_NUOPC
!
! !DESCRIPTION: Subroutine HCO\_Imp2Ext3R\_NUOPC copies fields from the import
! state to the HEMCO ExtState object for 3D real*8 arrays.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_Imp2Ext3R_NUOPC( HcoState, ExtDat, FldName, RC )
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
!  29 Apr 2025 - Initial version for pure ESMF/NUOPC interface
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      TYPE(ESMF_Field)           :: field
      REAL, POINTER              :: Ptr3D(:,:,:) => NULL()
      INTEGER                    :: L, NZ, OFF, STAT, localrc

      ! ================================================================
      ! HCO_Imp2Ext3R_NUOPC begins here
      ! ================================================================

      RC = ESMF_SUCCESS

      ! Only do if being used...
      IF ( ExtDat%DoUse ) THEN
         ! Get field from import state
         CALL ESMF_StateGet(HcoState%IMPORT, TRIM(FldName), field, RC=localrc)
         IF (localrc /= ESMF_SUCCESS) THEN
            ! Field not found, return success but don't update ExtDat
            RC = ESMF_SUCCESS
            RETURN
         ENDIF

         ! Get pointer to data
         CALL ESMF_FieldGet(field, farrayPtr=Ptr3D, RC=localrc)
         IF (localrc /= ESMF_SUCCESS) THEN
            RC = localrc
            RETURN
         ENDIF

         ! Make sure the array in ExtDat is allocated and has the right size
         NZ = SIZE(Ptr3D, 3)
         CALL HCO_ArrAssert(ExtDat%Arr, HcoState%NX, HcoState%NY, NZ, STAT)
         IF (STAT /= HCO_SUCCESS) THEN
            RC = STAT
            RETURN
         ENDIF

         ! Pass field to ExtDat, handling undefined values
         OFF = LBOUND(Ptr3D, 3)
         DO L = 1, NZ
            WHERE (Ptr3D(:,:,NZ-L+OFF) == ESMF_UNDEF)
               ExtDat%Arr%Val(:,:,L) = 0.0
            ELSEWHERE
               ExtDat%Arr%Val(:,:,L) = Ptr3D(:,:,NZ-L+OFF)
            END WHERE
         ENDDO

         ! Cleanup
         NULLIFY(Ptr3D)

         ! Verbose output
         IF (HcoState%Config%doVerbose .AND. HcoState%amIRoot) THEN
            CALL HCO_MSG('Passed from import to ExtState: '//TRIM(FldName))
         ENDIF
      ENDIF

      ! Return with success
      RC = ESMF_SUCCESS
      RETURN

      END SUBROUTINE HCO_Imp2Ext3R_NUOPC
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCO_Imp2Ext2I_NUOPC
!
! !DESCRIPTION: Subroutine HCO\_Imp2Ext2I\_NUOPC copies fields from the import
! state to the HEMCO ExtState object for 2D integer arrays.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_Imp2Ext2I_NUOPC( HcoState, ExtDat, FldName, RC )
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
!  29 Apr 2025 - Initial version for pure ESMF/NUOPC interface
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      TYPE(ESMF_Field)           :: field
      REAL, POINTER              :: Ptr2D(:,:) => NULL()
      INTEGER                    :: STAT, localrc

      ! ================================================================
      ! HCO_Imp2Ext2I_NUOPC begins here
      ! ================================================================

      RC = ESMF_SUCCESS

      ! Only do if being used...
      IF ( ExtDat%DoUse ) THEN
         ! Get field from import state
         CALL ESMF_StateGet(HcoState%IMPORT, TRIM(FldName), field, RC=localrc)
         IF (localrc /= ESMF_SUCCESS) THEN
            ! Field not found, return success but don't update ExtDat
            RC = ESMF_SUCCESS
            RETURN
         ENDIF

         ! Get pointer to data
         CALL ESMF_FieldGet(field, farrayPtr=Ptr2D, RC=localrc)
         IF (localrc /= ESMF_SUCCESS) THEN
            RC = localrc
            RETURN
         ENDIF

         ! Make sure the array in ExtDat is allocated and has the right size
         CALL HCO_ArrAssert(ExtDat%Arr, HcoState%NX, HcoState%NY, STAT)
         IF (STAT /= HCO_SUCCESS) THEN
            RC = STAT
            RETURN
         ENDIF

         ! Copy data, handling undefined values
         ExtDat%Arr%Val = 0
         WHERE (Ptr2D /= ESMF_UNDEF)
            ExtDat%Arr%Val = INT(Ptr2D)
         END WHERE

         ! Cleanup
         NULLIFY(Ptr2D)

         ! Verbose output
         IF (HcoState%Config%doVerbose .AND. HcoState%amIRoot) THEN
            CALL HCO_MSG('Passed from import to ExtState: '//TRIM(FldName))
         ENDIF
      ENDIF

      ! Return with success
      RC = ESMF_SUCCESS
      RETURN

      END SUBROUTINE HCO_Imp2Ext2I_NUOPC
!EOC

!-----------------------------------------------------------------------
  ! NUOPC SetServices registration for HEMCO
  !-----------------------------------------------------------------------
  SUBROUTINE HCOI_NUOPC_SS(gcomp, rc)
    TYPE(ESMF_GridComp)  :: gcomp
    INTEGER, INTENT(OUT) :: rc

    rc = ESMF_SUCCESS

    ! Register NUOPC model methods for this component
    CALL NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    ! Set entry points for Initialize, Run, and Finalize
    CALL ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
         InitializeP1, phase=1, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    CALL ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
         InitializeP2, phase=2, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    ! Run and finalize are simpler - they don't need multiple phases
    CALL ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
         ModelAdvance, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    CALL ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, &
         ModelFinalize, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    ! Register the special NUOPC initialization method for clock
    CALL NUOPC_CompSetEntryPoint(gcomp, model_label_SetClock, &
         SetClock, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    ! Register a data initialization method to handle import/export variables
    CALL NUOPC_CompSetEntryPoint(gcomp, model_label_DataInitialize, &
         DataInitialize, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

  END SUBROUTINE HCOI_NUOPC_SS

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
    INTEGER                    :: localPet
    CHARACTER(LEN=ESMF_MAXSTR) :: hco_configFile, hco_diagFile, hco_logFile
    CHARACTER(LEN=ESMF_MAXSTR) :: hco_datadir
    INTEGER                    :: status, userRc

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

    ! Get HEMCO configuration attributes from component
    CALL ESMF_AttributeGet(gcomp, name='HEMCO_CONFIG', value=hco_configFile, &
                          defaultValue="HEMCO_Config.rc", rc=status)
    IF (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) THEN
        rc = status
        RETURN
    ENDIF

    CALL ESMF_AttributeGet(gcomp, name='HEMCO_DIAGN', value=hco_diagFile, &
                          defaultValue="HEMCO_Diagn.rc", rc=status)
    IF (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) THEN
        rc = status
        RETURN
    ENDIF

    CALL ESMF_AttributeGet(gcomp, name='HEMCO_LOGFILE', value=hco_logFile, &
                          defaultValue="HEMCO.log", rc=status)
    IF (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) THEN
        rc = status
        RETURN
    ENDIF

    CALL ESMF_AttributeGet(gcomp, name='HEMCO_DATA_ROOT', value=hco_datadir, &
                          defaultValue="./", rc=status)
    IF (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) THEN
        rc = status
        RETURN
    ENDIF

    ! Initialize HEMCO
    IF (.NOT. HcoInitialized) THEN
      IF (localPet == 0) THEN
        PRINT *, "HEMCO-NUOPC: Initializing HEMCO with config: ", TRIM(hco_configFile)
        PRINT *, "HEMCO-NUOPC: Using diagnostic file: ", TRIM(hco_diagFile)
        PRINT *, "HEMCO-NUOPC: Using data directory: ", TRIM(hco_datadir)
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
      PRINT *, "HEMCO-NUOPC: Phase 2 initialization completed"
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

      PRINT *, "HEMCO-NUOPC: Setting clock with current time = ", TRIM(timeString)
    ENDIF

    ! The model runs on the parent clock's timestep.
    CALL NUOPC_CompSetTimeStep(gcomp, timeStep, rc=status)
    IF (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) THEN
        rc = status
        RETURN
    ENDIF

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
    INTEGER           :: localPet

    rc = ESMF_SUCCESS

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

    ! Here we would advertise available import/export fields
    ! For a standalone HEMCO model, we don't have any required imports,
    ! but in a coupled system we might need to define them

    ! Mark as data initialized
    CALL NUOPC_CompAttributeSet(gcomp, name="InitializeDataComplete", &
                               value="true", rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) RETURN

    IF (localPet == 0) THEN
      PRINT *, "HEMCO-NUOPC: Data initialization completed"
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
    IF (localPet == 0) THEN
      CALL ESMF_TimeGet(currTime, timeString=timeString, rc=rc)
      IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) RETURN
    ENDIF

    ! Call the HEMCO run method
    CALL HCOI_ESMF_RUN(HcoState=HcoState, &
                      am_I_Root=(localPet == 0), &
                      RC=userRc)

    ! Check for HEMCO-specific errors
    IF (userRc /= HCO_SUCCESS) THEN
      IF (localPet == 0) THEN
        PRINT *, "HEMCO-NUOPC: Error in HCOI_ESMF_RUN at time ", TRIM(timeString)
      ENDIF
      rc = ESMF_FAILURE
      RETURN
    ENDIF

    ! Successful run
    IF (localPet == 0) THEN
      PRINT *, "HEMCO-NUOPC: Successfully completed timestep at ", TRIM(timeString)
    ENDIF

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
      PRINT *, "HEMCO-NUOPC: Finalizing HEMCO component"
    ENDIF

    ! Only clean up if we successfully initialized
    IF (HcoInitialized) THEN
      CALL HCOI_ESMF_FINAL(HcoState=HcoState, &
                          am_I_Root=(localPet == 0), &
                          RC=userRc)

      IF (userRc /= HCO_SUCCESS) THEN
        IF (localPet == 0) THEN
          PRINT *, "HEMCO-NUOPC: Error in HCOI_ESMF_FINAL"
        ENDIF
        rc = ESMF_FAILURE
        RETURN
      ENDIF

      HcoInitialized = .FALSE.
      HcoState => NULL()
    ENDIF

    IF (localPet == 0) THEN
      PRINT *, "HEMCO-NUOPC: Finalization completed successfully"
    ENDIF

  END SUBROUTINE ModelFinalize

END MODULE HCOI_ESMF_NUOPC_MOD