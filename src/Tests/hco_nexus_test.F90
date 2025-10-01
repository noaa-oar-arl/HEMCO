!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hco_nexus_test.F90
!
! !DESCRIPTION: Test module for HEMCO-NEXUS integration functions.
!\\
!\\
! !INTERFACE:
!
MODULE HCO_NEXUS_TEST
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
  PUBLIC :: HCO_TestNexusIntegration
!
! !REVISION HISTORY:
!  04 Sep 2025 - R. Yantosca - Initial version for NEXUS integration testing
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
! !ROUTINE: HCO_TestNexusIntegration
!
! !DESCRIPTION: Subroutine HCO\_TestNexusIntegration tests the HEMCO-NEXUS
!  integration functions to ensure they work correctly with ESMF.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE HCO_TestNexusIntegration( am_I_Root,  GC, HcoConfig, &
                                          ConfigFile, RC )
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
      CHARACTER(LEN=*),    INTENT(IN   )             :: ConfigFile
      INTEGER,             INTENT(  OUT)             :: RC
!
! !REVISION HISTORY:
!  04 Sep 2025 - R. Yantosca - Initial version for NEXUS integration testing
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
      ! HCO_TestNexusIntegration begins here
      ! ================================================================
      
      ! Init
      LOC      = 'HCO_TestNexusIntegration (HCO_NEXUS_TEST.F90)'
      Spc      => NULL()
      CurrCont => NULL()
      RC       = ESMF_SUCCESS
      
      ! Allocate HEMCO state
      ALLOCATE(HcoState, stat=STATUS)
      IF ( STATUS /= HCO_SUCCESS ) THEN
         ERRMSG = 'Error allocating HcoState in HCO_TestNexusIntegration'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF
      
      ! Allocate extension state
      ALLOCATE(ExtState, stat=STATUS)
      IF ( STATUS /= HCO_SUCCESS ) THEN
         ERRMSG = 'Error allocating ExtState in HCO_TestNexusIntegration'
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
      ! Test HCO_NexusInit
      ! ---------------------------------------------------------------------
      
      IF ( am_I_Root ) WRITE(*,*) 'Testing HCO_NexusInit...'
      
      ! Test the NEXUS initialization function
      CALL HCO_NexusInit( HcoState, GC, STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         ERRMSG = 'Error in HCO_NexusInit'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF
      
      ! ---------------------------------------------------------------------
      ! Test HCO_NexusRegisterFields
      ! ---------------------------------------------------------------------
      
      IF ( am_I_Root ) WRITE(*,*) 'Testing HCO_NexusRegisterFields...'
      
      ! Test the NEXUS field registration function
      CALL HCO_NexusRegisterFields( HcoState, STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         ERRMSG = 'Error in HCO_NexusRegisterFields'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF
      
      ! ---------------------------------------------------------------------
      ! Test HCO_NexusTransferData
      ! ---------------------------------------------------------------------
      
      IF ( am_I_Root ) WRITE(*,*) 'Testing HCO_NexusTransferData...'
      
      ! Test the NEXUS data transfer function
      CALL HCO_NexusTransferData( HcoState, STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         ERRMSG = 'Error in HCO_NexusTransferData'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF
      
      ! ---------------------------------------------------------------------
      ! Test HCO_NexusImportData
      ! ---------------------------------------------------------------------
      
      IF ( am_I_Root ) WRITE(*,*) 'Testing HCO_NexusImportData...'
      
      ! Test the NEXUS import data function
      CALL HCO_NexusImportData( HcoState, STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         ERRMSG = 'Error in HCO_NexusImportData'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF
      
      ! ---------------------------------------------------------------------
      ! Test HCO_NexusExportData
      ! ---------------------------------------------------------------------
      
      IF ( am_I_Root ) WRITE(*,*) 'Testing HCO_NexusExportData...'
      
      ! Test the NEXUS export data function
      CALL HCO_NexusExportData( HcoState, STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         ERRMSG = 'Error in HCO_NexusExportData'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF
      
      ! ---------------------------------------------------------------------
      ! Test HCO_NexusGetConfig
      ! ---------------------------------------------------------------------
      
      IF ( am_I_Root ) WRITE(*,*) 'Testing HCO_NexusGetConfig...'
      
      ! Test the NEXUS configuration retrieval function
      CALL HCO_NexusGetConfig( HcoState, STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         ERRMSG = 'Error in HCO_NexusGetConfig'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF
      
      ! ---------------------------------------------------------------------
      ! Test HCO_NexusFinalize
      ! ---------------------------------------------------------------------
      
      IF ( am_I_Root ) WRITE(*,*) 'Testing HCO_NexusFinalize...'
      
      ! Test the NEXUS finalization function
      CALL HCO_NexusFinalize( HcoState, STATUS )
      IF ( STATUS /= ESMF_SUCCESS ) THEN
         ERRMSG = 'Error in HCO_NexusFinalize'
         CALL HCO_ERROR( ERRMSG, RC, THISLOC=LOC )
         RETURN
      ENDIF
      
      ! ---------------------------------------------------------------------
      ! Verify results
      ! ---------------------------------------------------------------------
      
      IF ( am_I_Root ) WRITE(*,*) 'All HEMCO-NEXUS integration tests completed successfully!'
      
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
      
      END SUBROUTINE HCO_TestNexusIntegration
!EOC
END MODULE HCO_NEXUS_TEST