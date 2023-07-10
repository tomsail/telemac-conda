module m_serafin2_def
!-----------------------------------------------------------------------
! This file is part of ESTEL3D
! Copyright (c) EDF 2007
! contact : regina.nebauer@edf.fr
!-----------------------------------------------------------------------
!
! Function: Defines common parameters and structures for the
!           modules reading and writing serafin files.
!-----------------------------------------------------------------------

 USE M_ESTEL3D_UTIL

 implicit none

 PUBLIC


! The data types available (part of the file header)
  INTEGER, PARAMETER, PUBLIC :: DATA_FE    = 1
  INTEGER, PARAMETER, PUBLIC :: DATA_MHFE  = 2
  INTEGER, PARAMETER, PUBLIC :: DATA_POINT = 3

! The discretization of variables to write (part of the file header)
  INTEGER, PARAMETER, PUBLIC :: DISC_P0 = 0
  INTEGER, PARAMETER, PUBLIC :: DISC_P1 = 1
  INTEGER, PARAMETER, PUBLIC :: DISC_P2 = 2

! The different discretisations for finite element data :
! TODO : define this kind of variables in the BIEF !!!!

  INTEGER, PARAMETER, PUBLIC :: POINTS   = 0
  INTEGER, PARAMETER, PUBLIC :: FE_TETRA = 31
  INTEGER, PARAMETER, PUBLIC :: FE_TRI   = 11


  INTEGER, PARAMETER, PUBLIC :: FULL_INTERLACE  = 200
  INTEGER, PARAMETER, PUBLIC :: NO_INTERLACE    = 201
!-----------------------------------------------------------------------
!  Description of a data set in a serafin file :
!-----------------------------------------------------------------------

  TYPE DataSet_S
      ! The number of zones (read or written) in the current data set
      INTEGER                                     :: Num_Zones
      ! The data type in this data set (DATA_FE, DATA_POINT ...)
      INTEGER                                     :: DataType
      ! Number of variables in each zone
      INTEGER                                     :: NBVar
      ! number of aux variables in each zone
      INTEGER                                     :: NBVarAux
      ! Date and time
      INTEGER, DIMENSION(3)                       :: DATE
      INTEGER, DIMENSION(3)                       :: TIME
      ! variable names
      CHARACTER(len=30),DIMENSION(:), POINTER :: VARNAMES
      ! variable locations (?? should move to Zone_s ???)
      INTEGER, DIMENSION(:), POINTER              :: VarLocation 
      ! variable units
      CHARACTER(len=10),DIMENSION(:), POINTER :: VARUNITS
      CHARACTER(len=4),DIMENSION(2)               :: UNITS
      ! title 
      CHARACTER(len=80)                           :: TITLE
  END TYPE DataSet_S

!-----------------------------------------------------------------------
! Description of a zone in a serafin file
! These variables are initialized at the zone creation and are used 
! to check the process of writing data values.
!-----------------------------------------------------------------------
  INTEGER, PARAMETER :: IPARAM_MAX = 20

  TYPE ZoneArg_S      

      character(len=80)                       :: Title
      ! Time 
      Double precision                        :: Time
      
      ! Table of aux data values. First aux data value is the 
      ! time.
      Double precision, DIMENSION(:), POINTER :: AuxVarValues

      ! List of variables to be shared and with which zone.
      ! shared variables should not be written for the current zone.
      INTEGER,          DIMENSION(:), POINTER :: VarShareList

      ! List of variables declared as "passive" for this zone.
      ! passive variables has no expected value for this zone.
      INTEGER,          DIMENSION(:), POINTER :: PassiveVarList

      ! Integer tab for zone description 
      INTEGER, DIMENSION(IPARAM_MAX)          :: IParam

  END TYPE ZoneArg_S           


! Max number of files to be opened or closed :
  INTEGER, PARAMETER :: NMAX_FILES = 5

! The base for the logical unit. 
  INTEGER, PARAMETER :: LU_SERAFIN_READ_BASE = 60 
  INTEGER, PARAMETER :: LU_SERAFIN_WRITE_BASE = &
                        LU_SERAFIN_READ_BASE+NMAX_FILES

!----------------------------------------------------------------------
!  The different parameters in the zone descriptor are assigned by
! identifier. Here the list of the identifiers :
!----------------------------------------------------------------------
  ! Identifiers for the different zone descriptors
  ! ID from 1 - 20 => Integer values
  ! The IDs corresponds to the entry in the IPARAM
  ! section of the zone descriptor.
  INTEGER, PARAMETER, PUBLIC :: ID_NELEM         = 1  ! ID_NELEM + P0
  INTEGER, PARAMETER, PUBLIC :: ID_NPOIN         = 2  ! ID_NELEM + P1
  INTEGER, PARAMETER, PUBLIC :: ID_NFAC          = 3  ! ID_NELEM + P2
  INTEGER, PARAMETER, PUBLIC :: ID_NPLAN         = 4  ! Number of prism layers
  INTEGER, PARAMETER, PUBLIC :: ID_NPTFR         = 5  ! Number of border nodes
  INTEGER, PARAMETER, PUBLIC :: ID_NPTIR         = 6  ! ?? //
  INTEGER, PARAMETER, PUBLIC :: ID_DISC          = 7  ! FE_TETRA, FE_TRI ...
  INTEGER, PARAMETER, PUBLIC :: ID_NDP           = 8  ! No of nodes per element



  INTEGER, PARAMETER, PUBLIC :: ID_Share_Connect = 20    ! Zone to share connect
                                                 ! with.

  ! Other zone parameters :
  INTEGER, PARAMETER, PUBLIC :: ID_Var_Share_List   = 51
  INTEGER, PARAMETER, PUBLIC :: ID_Passive_Var_List = 52
  INTEGER, PARAMETER, PUBLIC :: ID_Aux_Data         = 53
  INTEGER, PARAMETER, PUBLIC :: ID_TITLE            = 54
  INTEGER, PARAMETER, PUBLIC :: ID_SOLUTIONTIME     = 55


! status of the current zone :

  TYPE ZoneCheck_S      
      ! Internal control structures : 
      ! check if variables ar written / have to be written
      LOGICAL,          DIMENSION(:), POINTER :: VarCheck
      ! check if zone is completed (received all data values)
      LOGICAL                                 :: Completed
      ! current zone number
      INTEGER                                 :: IZone
      LOGICAL, DIMENSION(IPARAM_MAX)          :: ICheck

      ! Checking for writing of expected data structures : 
      LOGICAL                                 :: ExpectIkle 
      LOGICAL                                 :: ExpectIkleFac 
      LOGICAL                                 :: ExpectIPOBO
      LOGICAL                                 :: ExpectKnolg
      LOGICAL                                 :: CheckIkle
      LOGICAL                                 :: CheckIPOBO
      LOGICAL                                 :: CheckKNolg
  END TYPE ZoneCheck_S           

!-----------------------------------------------------------------------
! Descriptor for a file : the dataset (file header) and the current
! zone descriptor. the physical description is given by the file name
! and the Logical Unit.
!-----------------------------------------------------------------------

  TYPE File_S      
      TYPE(DataSet_S),  POINTER :: dataset   ! dataset descriptor
      TYPE(ZoneArg_S),  POINTER :: ZonePara  ! current zone parameters
      TYPE(ZoneCheck_S),POINTER :: ZoneCheck ! current zone status
      INTEGER           :: LU
      CHARACTER(len=80) :: file_name
  END TYPE


! a structure store all data for the current zone
! in a ZoneData structure ( connectivity and data )
! tables for the data :
  type :: real_ptr
      double precision, dimension(:), pointer :: r
  end type real_ptr

  type ZoneData_S
      TYPE(real_ptr), dimension(:), pointer :: DataArray
      double precision, dimension(:),pointer:: auxdata
      integer, dimension(:), pointer        :: ikle
      integer, dimension(:), pointer        :: knolg
      ! to be completed ...
  end type ZoneData_S


 TYPE(ZoneData_S), DIMENSION(NMAX_FILES), TARGET :: ZoneDataList


 contains

!-----------------------------------------------------------------------
subroutine clear_dataset_d(dataset)
!-----------------------------------------------------------------------
! Function : Initialize a dataset descriptor ( free memory and 
!            set all pointers to zero)
! Status : DONE.
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Arguments

  TYPE(DataSet_S), intent(inout) :: dataset
  
!-----------------------------------------------------------------------
! Local declarations
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
  if (debug) call proc_begin('clear_dataset_d')
!-----------------------------------------------------------------------

  if(associated(dataset%varnames)) deallocate(dataset%varnames)
  if(associated(dataset%varunits)) deallocate(dataset%varunits)
  if(associated(dataset%varlocation)) deallocate(dataset%varlocation)
  dataset%num_zones = 0
  dataset%NbVar     = 0
  dataset%NbVarAux  = 0
  dataset%date(:)   = 0
  dataset%time(:)   = 0
  dataset%units(:)  = ' '
  dataset%title     = ' '

!-----------------------------------------------------------------------
  if (debug) call proc_end('clear_dataset_d')
!-----------------------------------------------------------------------
  return
end subroutine clear_dataset_d 




!-----------------------------------------------------------------------
subroutine clear_zonepara_d(zonep)
!-----------------------------------------------------------------------
! Function : Initialize a dataset descriptor ( free memory and 
!            set all pointers to zero)
! Status : 
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Arguments

  TYPE(ZoneArg_S), intent(inout) :: zonep
  
!-----------------------------------------------------------------------
! Local declarations
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
  if (debug) call proc_begin('clear_zonepar_d')
!-----------------------------------------------------------------------

write(*,*)'step1'
  if(associated(zonep%varsharelist)) deallocate(zonep%varsharelist)
write(*,*)'step2', associated(zonep%passivevarlist)
  if(associated(zonep%passivevarlist)) deallocate(zonep%passivevarlist)
write(*,*)'step3'
  if(associated(zonep%auxvarvalues)) deallocate(zonep%auxvarvalues)
write(*,*)'step4'
  nullify(zonep%varsharelist)
  nullify(zonep%passivevarlist)
  nullify(zonep%auxvarvalues)
  zonep%iparam(:) = 0
  zonep%time = 0.d0
  zonep%title(:) = ' '


!-----------------------------------------------------------------------
  if (debug) call proc_end('clear_zonepar_d')
!-----------------------------------------------------------------------
  return
end subroutine clear_zonepara_d 


!-----------------------------------------------------------------------
subroutine clear_zonedata_d(zoned)
!-----------------------------------------------------------------------
! Function : Initialize a zonedata descriptor ( free memory and 
!            set all pointers to zero)
! Status : TODO
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Arguments

  TYPE(ZoneDATA_S), intent(inout) :: zoned
  
!-----------------------------------------------------------------------
! Local declarations
!-----------------------------------------------------------------------

  integer :: ndat, i

!-----------------------------------------------------------------------
  if (debug) call proc_begin('clear_zonedata_d')
!-----------------------------------------------------------------------

  if(associated(zoned%DataArray)) then
    ndat = size(zoned%dataarray)
    do i = 1, ndat
        if ( associated(zoned%dataarray(i)%r))then
            deallocate(zoned%dataarray(i)%r)
        end if
    end do
!   deallocate(zoned%dataarray)
  end if
! nullify(zoned%dataarray)
  if(associated(zoned%auxdata)) deallocate(zoned%auxdata)
  nullify(zoned%auxdata)
  if(associated(zoned%ikle)) deallocate(zoned%ikle)
  nullify(zoned%ikle)
  if(associated(zoned%knolg)) deallocate(zoned%knolg)
  nullify(zoned%knolg)

!-----------------------------------------------------------------------
  if (debug) call proc_end('clear_zonedata_d')
!-----------------------------------------------------------------------
  return
end subroutine clear_zonedata_d 




end module m_serafin2_def
