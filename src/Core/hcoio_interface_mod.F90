!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hcoio_interface_mod.F90
!
! !DESCRIPTION: Module HCOIO\_Interface\_Mod provides a unified interface for
! HEMCO I/O operations. It dispatches to the appropriate implementation based on
! the environment (ESMF with MAPL, ESMF without MAPL, or NetCDF).
!\\
!\\
! !INTERFACE:
!
MODULE HCOIO_Interface_Mod
!
! !USES:
!
  USE HCO_Error_Mod
  USE HCO_Types_Mod
  USE HCO_State_Mod,     ONLY : HCO_State

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC  :: HCOIO_Read
  PUBLIC  :: HCOIO_Write
  PUBLIC  :: HCOIO_CloseAll
!
! !REVISION HISTORY:
!  29 Apr 2025 - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOIO_Read
!
! !DESCRIPTION: Subroutine HCOIO\_Read is the central routine for
! reading data into HEMCO. It dispatches to the appropriate implementation
! based on the environment.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOIO_Read( HcoState, Lct, RC )
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
    !======================================================================
    ! HCOIO_Read begins here
    !======================================================================

#if defined(ESMF_) && defined(MAPL_ESMF)
    ! ESMF environment with MAPL
    USE HCOIO_Read_Mod, ONLY : HCOIO_Read_MAPL => HCOIO_Read
    CALL HCOIO_Read_MAPL(HcoState, Lct, RC)
#elif defined(ESMF_)
    ! Pure ESMF environment (no MAPL)
    USE HCOIO_Read_Mod, ONLY : HCOIO_Read_ESMF => HCOIO_Read
    CALL HCOIO_Read_ESMF(HcoState, Lct, RC)
#else
    ! Default NetCDF implementation
    USE HCOIO_Read_NetCDF_Mod, ONLY : HCOIO_Read_NetCDF => HCOIO_Read
    CALL HCOIO_Read_NetCDF(HcoState, Lct, RC)
#endif

  END SUBROUTINE HCOIO_Read
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOIO_Write
!
! !DESCRIPTION: Subroutine HCOIO\_Write is the central routine for
! writing data from HEMCO. It dispatches to the appropriate implementation
! based on the environment.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOIO_Write( HcoState, RC, OnlyIfFirst, COL )
!
! !INPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER                 :: HcoState    ! HEMCO state object
    LOGICAL,          OPTIONAL, INTENT(IN   ) :: OnlyIfFirst !
    INTEGER,          OPTIONAL, INTENT(IN   ) :: COL         ! Collection Nr.
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,                    INTENT(INOUT) :: RC          ! Failure or success
!
! !REVISION HISTORY:
!  29 Apr 2025 - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
    !======================================================================
    ! HCOIO_Write begins here
    !======================================================================

#if defined(ESMF_) && defined(MAPL_ESMF)
    ! ESMF environment with MAPL
    USE HCOIO_Write_Mod, ONLY : HCOIO_Write_MAPL => HCOIO_Write
    CALL HCOIO_Write_MAPL(HcoState, RC, OnlyIfFirst, COL)
#elif defined(ESMF_)
    ! Pure ESMF environment (no MAPL)
    USE HCOIO_Write_Mod, ONLY : HCOIO_Write_ESMF => HCOIO_Write
    CALL HCOIO_Write_ESMF(HcoState, RC, OnlyIfFirst, COL)
#else
    ! Default NetCDF implementation
    USE HCOIO_Write_NetCDF_Mod, ONLY : HCOIO_Write_NetCDF => HCOIO_Write
    CALL HCOIO_Write_NetCDF(HcoState, RC, OnlyIfFirst, COL)
#endif

  END SUBROUTINE HCOIO_Write
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOIO_CloseAll
!
! !DESCRIPTION: Subroutine HCOIO\_CloseAll ensures that all open file
! connections are properly closed.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOIO_CloseAll( HcoState, RC )
!
! !INPUT PARAMETERS:
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
    !======================================================================
    ! HCOIO_CloseAll begins here
    !======================================================================

#if defined(ESMF_) && defined(MAPL_ESMF)
    ! ESMF environment with MAPL
    USE HCOIO_Read_Mod, ONLY : HCOIO_CloseAll_MAPL => HCOIO_CloseAll
    CALL HCOIO_CloseAll_MAPL(HcoState, RC)
#elif defined(ESMF_)
    ! Pure ESMF environment (no MAPL)
    USE HCOIO_Read_Mod, ONLY : HCOIO_CloseAll_ESMF => HCOIO_CloseAll
    CALL HCOIO_CloseAll_ESMF(HcoState, RC)
#else
    ! Default NetCDF implementation
    USE HCOIO_Read_NetCDF_Mod, ONLY : HCOIO_CloseAll_NetCDF => HCOIO_CloseAll
    CALL HCOIO_CloseAll_NetCDF(HcoState, RC)
#endif

  END SUBROUTINE HCOIO_CloseAll
!EOC
END MODULE HCOIO_Interface_Mod