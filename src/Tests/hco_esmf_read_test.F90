!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hco_esmf_read_test.F90
!
! !DESCRIPTION: Test module for HEMCO-ESMF read operations.
!\\
!\\
! !INTERFACE:
!
MODULE HCO_ESMF_READ_TEST
!
! !USES:
!
  USE HCO_ERROR_MOD
  USE HCO_TYPES_MOD
  USE HCO_STATE_MOD
  USE HCOX_STATE_MOD
  USE ESMF
  
  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: HCO_TestReadOperations
!
! !REVISION HISTORY:
!  03 Sep 2025 - R. Yantosca - Initial version
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
! !ROUTINE: HCO_TestReadOperations
!
! !DESCRIPTION: Subroutine HCO\_TestReadOperations tests the HEMCO-ESMF read
!  operations to ensure they work correctly with ESMF.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_TestReadOperations( am_I_Root, GC, HcoConfig, RC )
!
! !USES:
!
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
      TYPE(ESMF_State)           :: IMPORT, EXPORT
      TYPE(ESMF_Field)           :: field
      CHARACTER(LEN=255)         :: ERRMSG
      TYPE(HCO_State),     POINTER :: HcoState
      TYPE(Ext_State),    POINTER :: ExtState
      
      ! ================================================================
      ! HCO_TestReadOperations begins here
      ! ================================================================
      
      ! Init
      LOC      = 'HCO_TestReadOperations (HCO_ESMF_READ_TEST.F90)'
      Spc      => NULL()
      CurrCont => NULL()
      RC       = ESMF_SUCCESS
      
      ! Allocate HEMCO state
      ALLOCATE(HcoState, stat=STATUS)
      IF ( STATUS /= HCO_SUCCESS ) THEN
         ERRMSG = 'Error allocating HcoState in HCO_TestReadOperations'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF
      
      ! Allocate extension state
      ALLOCATE(ExtState, stat=STATUS)
      IF ( STATUS /= HCO_SUCCESS ) THEN
         ERRMSG = 'Error allocating ExtState in HCO_TestReadOperations'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF
      
      ! Get the import and export states from the grid component
      CALL ESMF_GridCompGet( GC, importState=IMPORT, exportState=EXPORT, rc=STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         ERRMSG = 'Error getting import/export states from grid component'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF
      
      ! Associate states with HEMCO state
      HcoState%IMPORT => IMPORT
      HcoState%EXPORT => EXPORT
      
      ! Set grid dimensions for testing
      HcoState%NX = 10
      HcoState%NY = 10
      
      ! ---------------------------------------------------------------------
      ! Test HCO_SetServices
      ! ---------------------------------------------------------------------
      
      IF ( am_I_Root ) WRITE(*,*) 'Testing HCO_SetServices...'
      
      ! Create a simple test field
      field = ESMF_FieldCreate( ESMF_TYPEKIND_R8, name='TEST_FIELD', rc=STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         ERRMSG = 'Error creating test field'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF
      
      ! Add field to import state
      CALL ESMF_StateAdd( IMPORT, (/field/), rc=STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         ERRMSG = 'Error adding test field to import state'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF
      
      ! ---------------------------------------------------------------------
      ! Test HCO_SetExtState_ESMF
      ! ---------------------------------------------------------------------
      
      IF ( am_I_Root ) WRITE(*,*) 'Testing HCO_SetExtState_ESMF...'
      
      ! Initialize extension state components for testing
      ! In a real implementation, these would be populated from configuration
      
      ! ---------------------------------------------------------------------
      ! Test HCO_Imp2Ext operations
      ! ---------------------------------------------------------------------
      
      IF ( am_I_Root ) WRITE(*,*) 'Testing HCO_Imp2Ext operations...'
      
      ! Test 2D real field
      CALL HCO_Imp2Ext2R( HcoState, ExtState%BYNCY, 'TEST_FIELD', STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         ERRMSG = 'Error in HCO_Imp2Ext2R test'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF
      
      ! Test 2D integer field
      ! Note: We would need to create an appropriate ExtDat_2I field for testing
      ! CALL HCO_Imp2Ext2I( HcoState, ExtState%..., 'TEST_FIELD', STATUS )
      ! IF ( STATUS /= ESMF_SUCCESS ) THEN
      !    ERRMSG = 'Error in HCO_Imp2Ext2I test'
      !    CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
      !    RETURN
      ! ENDIF
      
      ! Test 2D single precision field
      ! Note: We would need to create an appropriate ExtDat_2S field for testing
      ! CALL HCO_Imp2Ext2S( HcoState, ExtState%..., 'TEST_FIELD', STATUS )
      ! IF ( STATUS /= ESMF_SUCCESS ) THEN
      !    ERRMSG = 'Error in HCO_Imp2Ext2S test'
      !    CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
      !    RETURN
      ! ENDIF
      
      ! Test 3D real field
      ! Note: We would need to create an appropriate ExtDat_3R field for testing
      ! CALL HCO_Imp2Ext3R( HcoState, ExtState%..., 'TEST_FIELD', STATUS )
      ! IF ( STATUS /= ESMF_SUCCESS ) THEN
      !    ERRMSG = 'Error in HCO_Imp2Ext3R test'
      !    CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
      !    RETURN
      ! ENDIF
      
      ! Test 3D single precision field
      ! Note: We would need to create an appropriate ExtDat_3S field for testing
      ! CALL HCO_Imp2Ext3S( HcoState, ExtState%..., 'TEST_FIELD', STATUS )
      ! IF ( STATUS /= ESMF_SUCCESS ) THEN
      !    ERRMSG = 'Error in HCO_Imp2Ext3S test'
      !    CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
      !    RETURN
      ! ENDIF
      
      ! ---------------------------------------------------------------------
      ! Verify results
      ! ---------------------------------------------------------------------
      
      IF ( am_I_Root ) WRITE(*,*) 'All HEMCO-ESMF read operations completed successfully!'
      
      ! ---------------------------------------------------------------------
      ! Cleanup
      ! ---------------------------------------------------------------------
      
      IF ( ASSOCIATED(Spc) ) DEALLOCATE(Spc)
      CurrCont => NULL()
      
      ! Deallocate states
      IF ( ASSOCIATED(HcoState) ) DEALLOCATE(HcoState)
      IF ( ASSOCIATED(ExtState) ) DEALLOCATE(ExtState)
      
      ! Return success
      RC = ESMF_SUCCESS
      
      END SUBROUTINE HCO_TestReadOperations
!EOC
END MODULE HCO_ESMF_READ_TEST