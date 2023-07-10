module m_read_serafin2
! TODO : check the IOSTAT value of the read() statements
!-----------------------------------------------------------------------
! This file is part of ESTEL3D
! Copyright (c) EDF 2007
! contact : regina.nebauer@edf.fr
!-----------------------------------------------------------------------
!
! Function: This module defines all functions for reading the serafin2
!           File format.
!           It is designed to access randomly the information and data
!           for a given zone (=time step).
!           
!***********************************************************************
! Module functions :
!  . serafin_open_file() - Open a serafin2 file. If success, this will
!                          return a file Id. This procedure will read 
!                          the file header and a zone.
!  . serafin_get_fileinfo() - recover at any moment informations about 
!                          the data set (variable names and units, data
!                          type ....)
!  . serafin_get_zoneinfo() - for the active zone, get all header
!                          informations  (solutiontime, title, number 
!                          of nodes and elements ....)
!  . serafin_get_auxdata() - for the active zone, get the values for the
!                          auxiliary data.
!  . serafin_get_data_array() - for the active zone, get the data array
!                         for a iven variable.
!  . serafin_get_ikle() - Recover the connectivity table for finite
!                         element data.
!
!***********************************************************************
! Serafin_2 file format : 
!***********************************************************************
! See the M_WRITE_SERAFIN module for details.
! 
!
!***********************************************************************
! Reading Serafin2 files
!***********************************************************************
! Reminder serafin2 file format (see M_WRITE_SERAFIN for details)
!=======================================================================
! Global file informations : 
!---------------------------
! . the title of the data file
! . data format informations (point data, FE data ...)
! . Number of variables
! . number of aux variables
! . variable names and units (aux data and volume data)
! . Discretisation of the variables (P0, P1 ... )
! . data and time informations.
!
! Zone header 
!-------------
! The zone header contains the following informations : 
! . The zone title
! . The solution time
! . The variable share list
! . The passive variable list
! . The auxiliary data
! . the IPARAM section :
!   . number of nodes
!   . number of elements
!   . ....
!   . number of prism layers for sigma mesh
!   . number of border nodes
!   . discretisation : triangles, tetra ...
!   . the number of points per element
!   . informations about parallel computing options
!   . informations about connectivity sharing
!
! Zone data section
!------------------
! . if connectivity is not shared, and the data are of FE like type,
!   the first data entry is the connectivity.
! . if the data are not shared with another zone, follows the data
!   for the variables.
! Data sharing is only possible in the following case :
! Zone n - write the data
! Zone n+1 - share with zone n
! Zone n+2 - share with zone n (or n+1)
! Zone m - write all data
! Zone m+1 - share with zone m
! Zone m+2 - share with zone m (or m+1)
! It is not possible to do the following :
! Zone n - write data 
! Zone n+1 - share with zone n
! Zone n+m - write data
! Zone n+m+k - share data with zone n
! this is not possible because we cannot keep track of ALL data written
! at any place of the file. We can only keep track of data written, 
! and NOT replaced by other data, prior to the data qhare query.
! So finally, the number of the zone to share data with is rather a
! flag "share or not share" with the previous record of these data.
!
! How this module works :
!========================
!
! Opening a serafin file :
!-------------------------
! The user should provide the name of the file to open. The module will
! return a file ID, which is different from the logical unit.
! call serafin_open_file(name,IdFile). 
! A negative file Id indicates an error.
! The user may open the file in two different modes :
! RECOVER - open the file and go directly to the last zone. This mode
!           will be use to recover or continue calculations.
! READALL - go to the first zone and read the data zone by zone.
!
!   call serafin_open_file(name,IdFile,mode)
!
! where name is the name of the file to open on disk, IdFile is the
! returned file ID (this is NOT a logical unit) and mode is one of
! RECOVER or READALL.
!
! The informations about the file header are stored and may be 
! recovered by the user as long as the file Id is valid (as long as
! the file is opened). The file header information are recovered by
! the means of a generic procedure, by indicating the parameter ID :
! 
!   call serafin_get_fileinfo(IdFile,IdPara,parameter,[dim])
!
! where IdFile is a valid file ID in the m_serafin_read module,
! IdPara is one of the following parameters, parameter is the value
! recovered. If the parameter is a table, the user should provide the
! dimension of the table (ie the number of variables => this information
! should be recovered BEFORE reading the tables... for fixed size tables
! like for the date and time informations, this dimension is needed too.
! )
!
! the parameters may be the following ones :
!----------------------------------------------------------------------
! ID_FILE_TITLE
!----------------------------------------------------------------------
! Type : character(72)
!----------------------------------------------------------------------
! Description : The file title.
!----------------------------------------------------------------------
! ID_FILE_NVAR
!----------------------------------------------------------------------
! Type : integer
!----------------------------------------------------------------------
! Description : The number of variables
!----------------------------------------------------------------------
! ID_FILE_NVARAUX
!----------------------------------------------------------------------
! Type : integer 
!----------------------------------------------------------------------
! Description : The number of Auxiliary variables
!----------------------------------------------------------------------
! ID_FILE_DATE
!----------------------------------------------------------------------
! Type : two integer tables of dimension 3
!----------------------------------------------------------------------
! Description :  Date and time informations
!----------------------------------------------------------------------
! ID_VARNAMES
!----------------------------------------------------------------------
! Type : character(len=30), dimension(NVAR+NVARAUX)
!----------------------------------------------------------------------
! Description :  The variable names (volume and aux data).
!                Each variable name is 30 characters long.
!----------------------------------------------------------------------
! ID_VARUNITS
!----------------------------------------------------------------------
! Type : character(len=10), dimension(NVAR+NVARAUX)
!----------------------------------------------------------------------
! Description :  The variable units (volume and aux data).
!                Each variable unit is 10 characters long.
!----------------------------------------------------------------------
! ID_FILE_UNITS
!----------------------------------------------------------------------
! Type : character(len=4), dimension(2)
!----------------------------------------------------------------------
! Description :  The units of the calculations for length and time.
!    TODO : add mass....
!----------------------------------------------------------------------
! ID_DATATYPE
!----------------------------------------------------------------------
! Type : integer (parameter)
!----------------------------------------------------------------------
! Description :  The type of the data in the file : pointlike data
!                (DATA_POINT) or FE data (DATA_FE) ....
!----------------------------------------------------------------------
! ID_FILE_VARLOCATIONS
!----------------------------------------------------------------------
! Type : integer(NVAR)
!----------------------------------------------------------------------
! Description :  For finite element data, the discretisation of the 
!                variables (DISC_P0, DISC_P1). For point like data,
!                the table can be ignored. For FE like data, it will
!                determine the number of data points to read.
!----------------------------------------------------------------------
!
! Reading a zone :
!=====================================================================
! Depending on the opening mode of the file, the user may ask to read
! one zone after the other or only the last zone.
! Zone informations are available only for the current active zone. It 
! is not possible to rewind the file, access to the file is only 
! sequential. 
! The active zone is changed by a call to 
!
!     call serafin_get_nextzone(IdFile,IdZone)
!
! where IdFIle is the unique id of the file to read and IdZone is the
! returned zone id. This will be now the active zone, until the next
! call to serafin_get_next_zone() or to serafin_close_file()
!
! Zone header informations are stored in memory and are available as
! long as the zone is active via a generic procedure :
!
!     call serafin_get_zoneinfo(IdFile,IdParameter,value[,dim],ret)
!
! where IdFile is a valid file identifier,IdParameter one of the 
! parameter id's as described in M_WRITE_SERAFIN for writing the zone 
! arguments, value is the returned value of the zone argument and
! dim is the dimension of the table to be provided by the user.
! ret is 0 if success, -1 if error.
! 
! The data and the connectivity of the current zone can be recovered by
! the user in any order, as long as the zone is active.
! 
!     call serafin_getikle(IdFIle,d1,d2,ikle(d1,d2),ret)
!     call serafin_getdata(IdFile,NumVar,ndat,value(ndat),ret)
!
! The dimensions of the tables ikle and value should be provided by the
! user, NumVar is the number of the variable to be recovered.
! There may be more than one call for reading connectivity or data in
! the same zone.
!
!Closing a serafin file :
!------------------------
! At any moment, a file may be closed. The file Id becomes invalid, 
! all ressources are freed (data tables, zone and file descriptors)
!
!-----------------------------------------------------------------------

! Module dependencies :

  USE M_ESTEL3D_UTIL
  USE M_SERAFIN2_DEF

!-----------------------------------------------------------------------
! All declarations are private, if not stated otherwise.

  PRIVATE

!-----------------------------------------------------------------------
! PUBLIC DECLARATIONS
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! List of public subroutines
  PUBLIC :: serafin2_open_file
  PUBLIC :: serafin2_get_fileinfo
  PUBLIC :: serafin2_get_nextzone
  PUBLIC :: serafin2_get_zoneinfo
  PUBLIC :: serafin2_get_auxdata
  PUBLIC :: serafin2_get_data_array
  PUBLIC :: serafin2_get_ikle
      public :: serafin_get_fileinfo_str
      public :: serafin_get_fileinfo_str_tab
      public :: serafin_get_fileinfo_int
      public :: serafin_get_fileinfo_int_tab


! public declarations for user parameter id's
! (see comment for the same section in m_write_serafin ...)
  PUBLIC :: DATA_FE  
  PUBLIC :: DATA_MHFE 
  PUBLIC :: DATA_POINT
  PUBLIC :: DISC_P0
  PUBLIC :: DISC_P1
  PUBLIC :: DISC_P2 
  PUBLIC :: POINTS  
  PUBLIC :: FE_TETRA
  PUBLIC :: FE_TRI  
  PUBLIC :: ID_NELEM 
  PUBLIC :: ID_NPOIN 
  PUBLIC :: ID_NFAC  
  PUBLIC :: ID_NPLAN 
  PUBLIC :: ID_NPTFR  
  PUBLIC :: ID_NPTIR    
  PUBLIC :: ID_DISC      
  PUBLIC :: ID_NDP         
  PUBLIC :: ID_Share_Connect
  PUBLIC :: ID_Var_Share_List 
  PUBLIC :: ID_Passive_Var_List
  PUBLIC :: ID_Aux_Data  
  PUBLIC :: ID_TITLE      
  PUBLIC :: ID_SOLUTIONTIME
  PUBLIC :: NO_INTERLACE
  PUBLIC :: FULL_INTERLACE

  INTEGER, PARAMETER, PUBLIC :: ID_NVAR           = 60
  INTEGER, PARAMETER, PUBLIC :: ID_NVAR_AUX       = 61
  INTEGER, PARAMETER, PUBLIC :: ID_VARLOCATIONS   = 62
  INTEGER, PARAMETER, PUBLIC :: ID_DATA_TYPE      = 63
  INTEGER, PARAMETER, PUBLIC :: ID_FILE_DATE      = 64
  INTEGER, PARAMETER, PUBLIC :: ID_FILE_TIME      = 65
  INTEGER, PARAMETER, PUBLIC :: ID_VARNAMES       = 66
  INTEGER, PARAMETER, PUBLIC :: ID_VARUNITS       = 67


  INTEGER, PARAMETER, PUBLIC :: RECOVER           = 500
  INTEGER, PARAMETER, PUBLIC :: READALL           = 501


!-----------------------------------------------------------------------
! PRIVATE DECLARATIONS
!-----------------------------------------------------------------------

  LOGICAL :: INIT_SERAFIN = .FALSE. ! Initialisation status of module

  TYPE(File_S), DIMENSION(NMAX_FILES), TARGET :: FileList

  TYPE(ZoneData_S), DIMENSION(NMAX_FILES), TARGET :: ZoneData

! For checking the IOSTAT of the read statement :
  INTEGER, PARAMETER :: EOF_OK = 1
  INTEGER, PARAMETER :: EOF_KO = 2

! generic procedure interface to get the file informations

  interface serafin2_get_fileinfo
      module procedure serafin_get_fileinfo_str
      module procedure serafin_get_fileinfo_str_tab
      module procedure serafin_get_fileinfo_int
      module procedure serafin_get_fileinfo_int_tab
  end interface


! generic procedure interface to get the zone informations

  interface serafin2_get_zoneinfo
      module procedure serafin_get_zoneinfo_int
      module procedure serafin_get_zoneinfo_int_tab
      module procedure serafin_get_zoneinfo_str
      module procedure serafin_get_zoneinfo_real
  end interface




  contains

!-----------------------------------------------------------------------
subroutine init_read_serafin()
!-----------------------------------------------------------------------
! Function: Initialization of this module.
!
! Status : TODO
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Arguments
  
!-----------------------------------------------------------------------
! Local declarations
!-----------------------------------------------------------------------

  TYPE(File_S)    ,POINTER :: FileD
  integer :: i

!-----------------------------------------------------------------------
  if (debug) call proc_begin('init_read_serafin')
!-----------------------------------------------------------------------

  if ( INIT_SERAFIN ) then
    message_1(1) = 'M_READ_SERAFIN deja initialise. Rien a faire.'
    message_1(2) = 'M_READ_SERAFIN already initialized. Nothing to do.'
  else
      ! Loop over all file descriptors and initalize all connected 
      ! data descriptors :

      do i=1, NMAX_FILES

        ! Pointer to the file descriptor
        FileD => FileList(i)

        FileD%LU = -1
        Allocate(FileD%ZonePara)
        Allocate(FileD%dataset)
        ! no need for the zonecheck here...
        nullify(FileD%zonecheck)
        ! init pointers for dataset descriptor
!       call clear_dataset_d(filed%dataset)
        nullify(filed%dataset%varnames)
        nullify(filed%dataset%varunits)
        nullify(filed%dataset%varlocation)
        ! init pointers for zonepara descriptor
!       call clear_zonepara_d(filed%zonepara)
        nullify(fileD%zonepara%varsharelist)
        nullify(fileD%zonepara%passivevarlist)
        nullify(fileD%zonepara%auxvarvalues)
        ! init pointers for zonedata descriptor
!       call clear_zonedata_d(zonedataList(i))
        nullify(zonedatalist(i)%DataArray)
        nullify(zonedatalist(i)%ikle)
        nullify(zonedatalist(i)%auxdata)
        nullify(zonedatalist(i)%knolg)

      end do

  end if


!-----------------------------------------------------------------------
  if (debug) call proc_end('init_read_serafin')
!-----------------------------------------------------------------------
  return
end subroutine init_read_serafin

!-----------------------------------------------------------------------
subroutine serafin2_open_file(l,fname, mode, IdFile, IdZone, last)
!-----------------------------------------------------------------------
! Function: Open the file named fname. If ok, IdFile will be a positive
!           integer. If failed, IdFile is negative.
!           The file may be openend in mode READALL (read one zone after
!           the other, starting at zone 1) or RECOVER (go to the last 
!           complete zone write).
!           The current zone after this call depends on the opening
!           mode.
!           The user may directly access the data, a call to
!           serafin_get_nextzone is NOT necessary.
!           IdZone is the id of the current zone after opening the file.
! Status : TODO
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Arguments
  integer         , intent(in) :: l
  character(len=l), intent(in) :: fname
  integer, intent(in)          :: mode
  integer,          intent(out):: IdFile
  integer,          intent(out):: IdZone
  logical,          intent(out) :: last
  
!-----------------------------------------------------------------------
! Local declarations
!-----------------------------------------------------------------------

  character(len=80)         :: title
  TYPE(DataSet_S), pointer  :: datad
  type(File_S), pointer     :: filed
  type(zonearg_s), pointer  :: zonepara
  type(zonedata_s), pointer :: zonedata
  integer                   :: i
  integer                   :: ok
  integer                   :: ntot

  integer :: i1,i2

!-----------------------------------------------------------------------
  if (debug) call proc_begin('serafin2_open_file')
!-----------------------------------------------------------------------
write(*,*)'HERE ! START SERAFIN2'

  if ( .NOT. init_serafin ) call init_read_serafin()

  IdFile = -1
  write(*,*)'Length of fname : ',l
  write(*,*)'File name '
  write(*,*)'<',fname(1:len(fname)),'>'
  if ( debug ) then
    call write_line('-')
    message_1(1) = 'M_READ_SERAFIN : Lecture du fichier : '
    message_1(2) = 'M_READ_SERAFIN : Try to read the file : '
    call write_listing(message_1,fname)
    call write_line('-')
  end if

    write(*,*)'step6'
! Try to allocate a new file Id.
  do i = 1, NMAX_FILES
      ! A file descriptor is free if the associated LU equals -1
      if ( FileList(i)%LU .EQ. -1 ) then
          ! save file id and exit loop
          IdFile = i
          exit
      end if
  end do
  FileD => FileList(IdFile)
  write(*,*)'File Id : ',IdFile

! If no more unique file Id's available, error and stop.
  if ( IdFIle .EQ. -1 ) then
      message_1(1) = 'Tous les fichiers sont en cours d''utilisation' &
                     // ' (M_READ_SERAFIN)'
      message_1(2) = 'All files are in use. (M_READ_SERAFIN)'
      call write_listing(message_1)
  else
    write(*,*)'try to open file'
    ! Try to open the file. If error, exit.
    FileD%LU = 50 ! LU_SERAFIN_READ_BASE + IdFile
    write(*,*)'lu = ',FileD%LU
    open(UNIT=FILED%LU,&
         FILE=fname(1:l),&
!        STATUS='OLD',&
         FORM='UNFORMATTED',&
!        ACTION='READ',&
         IOSTAT=ok)
   write(*,*)'open passed. check iostat : '
    ! error on opening the file :
    if ( OK > 0 ) then

      message_1(1) = 'Erreur a l''ouverture du fichier '
      message_1(2) = 'Error while opening the file '
      call write_listing(message_1,fname)

    ! if file opening ok, try to read
    else
      write(*,*)'iostat ok. read file'
      ! Try to read the file header. The first entry is a 80 character
      ! string. if error, exit.
      read(filed%LU,IOSTAT=ok) title(1:80)
      write(*,*)'Title of Data set :',title
      ! the last characters of the title should be the string
      ! 'SERAFIN2'.
      ! If this is not the case, this is not the right file format :
      if ( title(73:80) .NE. 'SERAFIN2' .OR. ok .NE. 0 ) then
        close(FileD%LU)
        FileD%LU = -1
        message_1(1) = 'Mauvais format de fichier M_READ_SERAFIN : '
        message_1(2) = 'bad file format for M_READ_SERAFIN : '
        call write_listing(message_1,fname)

      else ! Title and format ok
        ! this seems to be the right file. allocate the data set 
        ! descriptor and read the complete file header. Fill in directly
        ! the informations into the data set descriptor.
        ! Allocate the data set descriptor !
        write(*,*)'reading title ok.'

        allocate(FileD%DataSet)
        DataD => FileD%DataSet
        DataD%Title = title
        write(*,*)'(',DataD%Title,')'
        ! read the number of variables and aux variables
        read(FileD%LU,IOSTAT=ok)i1,i2 ! DataD%NbVar,DataD%NbVarAux
        if(ok .NE. 0) then
          write(*,*)'error reading number of variables '
          return
        end if
        DataD%NbVar = i1
        DataD%NbVarAux = i2

        ! Allocate the tables for the dataset descriptor :
        ! the variable names and units and the varlocation table
        write(*,*)'Number of variables : ',DataD%NbVar
        write(*,*)'Number of aux variables : ',DataD%NbVarAux
        ntot = DataD%NbVar+DataD%NbVarAux
        dataD%num_zones = 0

        allocate(DataD%VarNames(ntot))
        allocate(DataD%VarUnits(ntot))
        allocate(DataD%VarLocation(DataD%NbVar))

        ! Read the units used by the calculation

        read(FileD%LU,IOSTAT=OK)(DataD%Units(i),i=1,2)

        ! Read the variable names

        read(FileD%LU,IOSTAT=OK)(DataD%VarNames(i),i=1,ntot)
        read(FileD%LU,IOSTAT=OK)(DataD%VarUnits(i),i=1,ntot)

        ! Read the date and time information

        read(filed%lu,IOSTAT=ok)(datad%date(i),i=1,3)
        read(filed%lu,IOSTAT=ok)(datad%time(i),i=1,3)

        ! Read the data type in the file (point,fe ..)

        read(filed%lu,IOSTAT=ok) datad%datatype

        ! Read the variable locations :

        read(filed%lu,IOSTAT=ok)(datad%varlocation(i),i=1,datad%nbvar)

        ! Listing output for the variable names and units
        call write_line('-')
        message_1(1) = 'Liste des noms et unites des variables dans '&
                       // 'le fichier'
        message_1(2) = 'List of variable names and units in file '
        call write_listing(message_1)
        call write_line('-')
        message_1(1) = 'Liste des variables domaine : '
        message_1(1) = 'List of domain variables : '
        call write_listing(message_1)
        do i = 1, datad%nbvar
          message_1(1) = 'Nom et unite de la variable '
          message_1(2) = 'Variable name and unit'
          call write_listing(message_1,trim(datad%varnames(i))//'['//&
                                     trim(datad%varunits(i))// '] (P'// &
                                     trim(I2Char(datad%varlocation(i)))&
                                     //')')
        end do


        message_1(1) = 'Liste des variables auxiliaires : '
        message_1(1) = 'List of auxiliary variables : '
        call write_listing(message_1)
        do i = datad%nbvar+1,ntot
          message_1(1) = 'Nom et unite de la variable '
          message_1(2) = 'Variable name and unit'
          call write_listing(message_1,trim(datad%varnames(i))//'['//&
                                     trim(datad%varunits(i))//']')
        end do
        

        if ( debug ) then
           call write_line('-')
           message_1(1) = 'Resumee entete du fichier '
           message_1(2) = 'Resuming file header : '
           call write_listing(message_1)
           call write_line('-')
           message_1(1) = 'DataD%NVar = '
           message_1(2) = 'DataD%NVar = '
           call write_listing(message_1,DataD%Nbvar)
           message_1(1) = 'DataD%NVarAux = '
           message_1(2) = 'DataD%NVarAux = '
           call write_listing(message_1,DataD%NbvarAux)
           message_1(1) = 'DataD%datatype = '
           message_1(2) = 'DataD%datatype = '
           call write_listing(message_1,DataD%datatype)
           message_1(1) = 'DataD%date = '//i2char(datad%date(1))//'-'&
                                         //i2char(datad%date(2))//'-'&
                                         //i2char(datad%date(3))
           message_1(2) = message_1(1)
           call write_listing(message_1)
           message_1(1) = 'DataD%time = '//i2char(datad%time(1))//'-'&
                                         //i2char(datad%time(2))//'-'&
                                         //i2char(datad%time(3))
           message_1(2) = message_1(1)
           call write_listing(message_1)
           message_1(1) = 'DataD%units= ['//datad%units(1)// &
                                        ']-['//datad%units(2)//']'
           message_1(2) = message_1(1)
           call write_listing(message_1)
           

        end if


        ! TODO : check if this is valid.
        select case (datad%datatype)
          case(DATA_FE) 
            message_1(1) = 'Fichier de donnees type Elements Finis'
            message_1(2) = 'File contains finite element like data'
            call write_listing(message_1)
          case(DATA_POINT)
            message_1(1) = 'Fichier de donnees type Point'
            message_1(2) = 'File contains point like data'
            call write_listing(message_1)
          case default
            message_1(1) = 'Type de donnees inconnues.'
            message_1(2) = 'Unknown data kind in file.'
            call write_listing(message_1)
            call clear_dataset_d(datad)
            filed%lu = -1
            IdFile = -1
        end select

        if ( IdFile .GT. 0 ) then

          ! END OF DATA SET DESCRIPTION 

          ! BEGIN OF ZONE DATA DESCRIPTION

          ! Allocate the arrays for the ZoneData descriptor :
          ! allocate the auxdata array (size nvaraux)
          ZonePara => FileD%ZonePara
          allocate(ZonePara%AuxVarValues(DataD%NbVarAux))
          ! allocate the variable share list
          allocate(ZonePara%VarShareList(DataD%NbVar))
          ! allocate the passive variable list
          allocate(ZonePara%PassiveVarList(DataD%NbVar))

          ! allocate the data array pointer (size nvar)
          ZoneData => ZoneDataList(IdFile)
          allocate(ZoneData%DataArray(datad%nbvar))
          ! allocate the table for the aux data values
          allocate(ZoneData%auxdata(datad%nbvaraux))

          ! Read the zone required by the mode :
          ! if mode READALL, read the first zone only.
          ! return IdZone=1 

          call serafin2_get_nextzone(IdFile,IdZone,last)
          if ( IdZone .NE. 1 ) then
             message_1(1) = 'Fichier serafin2 errone'
             message_1(2) = 'Corrupted Serafin2 file'
             call write_listing(message_1)
             last = .TRUE. ! do not enter the loop if RECOVER
             IdZone = -1
          end if
          write(*,*)'serafin file is ok'
          ! if mode RECOVER, go to the last zone.
          ! uncompleted zones are NOT admitted here. A seperated
          ! "cleanup"  procedure should be used to make a file clean
          ! (the last zone is suppressed, if not completed).
          ! Return the number of the last zone.
          if ( mode .EQ. RECOVER ) then
          write(*,*)'mode recover'
            do while (.NOT. last )
              write(*,*)'read next zone ..'
              call serafin2_get_nextzone(IdFile,IdZone,last)

            end do
          end if ! mode recover
        end if  ! file id ok
      end if ! serafin title ok
    end if ! ok for open the file
  end if ! found a free file descriptor
  write(*,*)'end of serafin2 open file'

!-----------------------------------------------------------------------
  if (debug) call proc_end('serafin2_open_file')
!-----------------------------------------------------------------------
  return
end subroutine serafin2_open_file


!-----------------------------------------------------------------------
subroutine serafin_get_fileinfo_int(IdFile,IdFilePara,value,ret)
!-----------------------------------------------------------------------
! Function : Get a file info parameter with the IdFilePara. The 
!            parameter is an integer
!            If the parameter id is unknown or the file id invalid,
!            ret will be negative. if operation succeeded, ret will be
!            zero.
!            Integer file infos are :
!            ID_NVAR     - the number of domain variables
!            ID_NVARAUX  - the number of auxiliary variables
!            ID_DATATYPE - the Id of the data type : 
!                          DATA_POINT,DATA_FE
! Status : TODO
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Arguments
  integer, intent(in)    :: IdFile
  integer, intent(in)    :: IdFilePara
  integer, intent(inout) :: value
  integer, intent(out)   :: ret
  
!-----------------------------------------------------------------------
! Local declarations
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
  if (debug) call proc_begin('serafin_get_fileinfo_int')
!-----------------------------------------------------------------------

  ret = -1
! check if file Id is valid.
  if ( validFile(IdFile)) then
      select case (IdFilePara) 
        case ( Id_NVar )
        write(*,*)'number of variables'
          value = FileList(IdFile)%Dataset%NbVar
          ret = 0
        case ( Id_NVar_Aux )
        write(*,*)'number of aux variables'
          value = FileList(IdFile)%Dataset%NbVarAux
          ret = 0
        case ( Id_Data_Type )
        write(*,*)' data type '
          value = FileList(IdFile)%Dataset%DataType
          ret = 0
        case default 
          message_1(1)='Mauvais identifiant IdFilePara de type entier' 
          message_1(2)='Bad integer type identifier for IdFilePara'
          call write_listing(message_1)
      end select

  end if

!-----------------------------------------------------------------------
  if (debug) call proc_end('serafin_get_fileinfo_int')
!-----------------------------------------------------------------------
  return
end subroutine serafin_get_fileinfo_int


!-----------------------------------------------------------------------
subroutine serafin_get_fileinfo_int_tab(IdFile,IdFilePara, &
                                        ndat,value,ret)
!-----------------------------------------------------------------------
! Function : Get a file info parameter with the IdFilePara. The 
!            parameter is an integer table. The user should provide the
!            dimension of the table.
!            If the parameter id is unknown or the file id invalid,
!            ret will be negative. if operation succeeded, ret will be
!            zero.
!            Integer table informations are
!            ID_FILE_DATE    - date as a dimension 3 table
!            ID_FILE_TIME    - time as a dimension 3 table
!            ID_VARLOCATIONS - discretisation type for the values :
!                              DISC_P0, DISC_P1
!
! Status : TODO
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Arguments
  integer, intent(in)  :: IdFile
  integer, intent(in)  :: IdFilePara
  integer, intent(in)  :: ndat
  integer,dimension(ndat), intent(inout) :: value
  integer, intent(out) :: ret
  
!-----------------------------------------------------------------------
! Local declarations
!-----------------------------------------------------------------------
integer :: i

!-----------------------------------------------------------------------
  if (debug) call proc_begin('serafin_get_fileinfo_int_tab')
!-----------------------------------------------------------------------

  ret = -1
  if ( validFile(IdFile)) then
      select case (IdFilePara) 
        case ( ID_FILE_DATE )
          ! Read the date information. The date is an integer 
          ! table of dimension 3
          if ( ndat .EQ. 3 ) then
            value(:) = FileList(IdFile)%Dataset%date(:)
            ret = 0
          else
             message_1(1) = 'Dimension de date = 3'
             message_1(2) = 'Dimension of date = 3'
             call write_listing(message_1)
          end if
        case ( ID_FILE_TIME )
          ! Read the time information. The time is an integer 
          ! table of dimension 3
          if ( ndat .EQ. 3 ) then
            value(:) = FileList(IdFile)%Dataset%time(:)
            ret = 0
          else
             message_1(1) = 'Dimension de time = 3'
             message_1(2) = 'Dimension of time = 3'
             call write_listing(message_1)
          end if
        case ( ID_VARLOCATIONS )
          if ( ndat .EQ. fileList(idfile)%dataset%nbvar ) then
             value(:) = FileList(IdFile)%Dataset%VarLocation(:)
             write(*,*)"variable locations : ",(value(i),i=1,ndat)
             ret = 0
          else
             message_1(1) = 'Mauvaise dimension pour VarLocation ' //&
                            'on attend : '
             message_1(2) = 'Bad dimension for VarLocation. expected :'
             call write_listing(message_1, &
                                fileList(idfile)%dataSet%NbVar)
           end if
        case default 
          message_1(1)='Mauvais identifiant IdFilePara de type '//&
                       'tableau d''entier' 
          message_1(2)='Bad integer table identifier for IdFilePara'
          call write_listing(message_1)
      end select

  end if

!-----------------------------------------------------------------------
  if (debug) call proc_end('serafin_get_fileinfo_int_tab')
!-----------------------------------------------------------------------
  return
end subroutine serafin_get_fileinfo_int_tab


!-----------------------------------------------------------------------
subroutine serafin_get_fileinfo_str(IdFile,IdFilePara,value,ret)
!-----------------------------------------------------------------------
! Function : Get a file info parameter with the IdFilePara. The 
!            parameter is of character string type.
!            If the parameter id is unknown or the file id invalid,
!            ret will be negative. if operation succeeded, ret will be
!            zero.
!            character string informations are :
!            ID_TITLE - a 72 character string for the title of the 
!                       data set
! Status : TODO
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Arguments
  integer, intent(in)  :: IdFile
  integer, intent(in)  :: IdFilePara
  character(len=80), intent(inout) :: value
  integer, intent(out) :: ret
  
!-----------------------------------------------------------------------
! Local declarations
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
  if (debug) call proc_begin('serafin_get_fileinfo_str')
!-----------------------------------------------------------------------

  ret = -1
  if ( validFile(IdFile) ) then
     if ( IdFilePara .EQ. ID_TITLE ) then
         value = fileList(IdFile)%DataSet%Title
         ret = 0
     else
         message_1(1) = 'Mauvais identifiant pour parametre fichier '//&
                        ' de type chaine '
         message_1(2) = 'Bad identifier of string type for file '//&
                        'parameters'
         call write_listing(message_1)
     end if

  end if
  

!-----------------------------------------------------------------------
  if (debug) call proc_end('serafin_get_fileinfo_str')
!-----------------------------------------------------------------------
  return
end subroutine serafin_get_fileinfo_str


!-----------------------------------------------------------------------
subroutine serafin_get_fileinfo_str_tab(IdFile,IdFilePara, &
                                        l,ndat,value,ret)
!-----------------------------------------------------------------------
! Function : Get a file info parameter with the IdFilePara. The 
!            parameter is of character string type.
!            If the parameter id is unknown or the file id invalid,
!            ret will be negative. if operation succeeded, ret will be
!            zero.
!            character string table informations are :
!            ID_VARNAMES - the names of the variables as a 30 charcter
!                          string, dimension NVAR+NVARAUX.
!            ID_VARUNITS - the units of the variables as a 10 charcter
!                          string, dimension NVAR+NVARAUX.
!            ID_UNITS    - the units of base calculation as a 4 charcter
!                          string, dimension 2
!
! Status : TODO
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Arguments
  integer, intent(in)  :: IdFile
  integer, intent(in)  :: IdFilePara
  integer, intent(in)  :: ndat
  integer, intent(in)  :: l
  character(len=l),dimension(ndat), intent(inout) :: value
  integer, intent(out) :: ret
  
!-----------------------------------------------------------------------
! Local declarations
!-----------------------------------------------------------------------

  TYPE(DATASET_S), pointer :: DataD

!-----------------------------------------------------------------------
  if (debug) call proc_begin('serafin_get_fileinfo_str_tab')
!-----------------------------------------------------------------------


       write(*,*)'step1'
  ret = -1
       write(*,*)'step2'
  if ( validFile(IdFile))  then
       write(*,*)'step2'
     DataD => FileList(IdFIle)%DataSet

       write(*,*)'step3'
     select case(IdFilePara)
       case ( ID_VARNAMES ) 
       write(*,*)'here ...'
          if ( ndat .EQ. DataD%NbVar+DataD%NbVarAux ) then
            value(:) = DataD%VarNames(:)
            ret = 0
          else
            message_1(1) = 'Mauvaise dimension pour les noms des '//&
                            'variables (NbVar+NbVarAux = )' 
            message_1(2) = 'Bad Dimension for variable names : ' //&
                           'expected (NbVar+NbVarAux ) '
            call write_listing(message_1,DataD%NbVar+DataD%NbVarAux)
          end if
       case ( ID_VARUNITS ) 
          if ( ndat .EQ. DataD%NbVar+DataD%NbVarAux ) then
            value(:) = DataD%VarUnits(:)
            ret = 0
          else
            message_1(1) = 'Mauvaise dimension pour les unites des '//&
                            'variables (NbVar+NbVarAux = )' 
            message_1(2) = 'Bad Dimension for variable units : ' //&
                           'expected (NbVar+NbVarAux ) '
            call write_listing(message_1,DataD%NbVar+DataD%NbVarAux)
          end if
       case default 
          message_1(1)='Mauvais Identifiant pour IdFileArg de type '//&
                       'tableau de chaines'
          message_1(2)='Bad identifier for IdFileArg of type string '//&
                       'table'
          call write_listing(message_1)
     end select

  end if

!-----------------------------------------------------------------------
  if (debug) call proc_end('serafin_get_fileinfo_str_tab')
!-----------------------------------------------------------------------
  return
end subroutine serafin_get_fileinfo_str_tab



!-----------------------------------------------------------------------
subroutine serafin2_close_file(IdFile,ret)
!-----------------------------------------------------------------------
! Function : Get a file info parameter with the IdFilePara. The 
!            parameter is of character string type.
!            If the parameter id is unknown or the file id invalid,
!            ret will be negative. if operation succeeded, ret will be
!            zero.
! Status : TODO
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Arguments
  integer, intent(inout) :: IdFile
  integer, intent(out)   :: ret
  
!-----------------------------------------------------------------------
! Local declarations
!-----------------------------------------------------------------------

 TYPE(File_S), pointer :: FileD

!-----------------------------------------------------------------------
  if (debug) call proc_begin('serafin2_close_file')
!-----------------------------------------------------------------------

  if ( validFile(IdFile) ) then
     FileD => FileList(IdFile)
     ! close the file
     close(FileD%LU)
     FileD%LU = -1
     ! clear all associated descriptors (free all memory)
     call clear_zonedata_d(ZoneDataList(IdFile))
     deallocate(ZoneDataList(IdFile)%DataArray)
     call clear_zonepara_d(FileD%ZonePara)
     call clear_dataset_d(FileD%DataSet)
     IdFile = -1
  end if

!-----------------------------------------------------------------------
  if (debug) call proc_end('serafin2_close_file')
!-----------------------------------------------------------------------
  return
end subroutine serafin2_close_file



!-----------------------------------------------------------------------
subroutine serafin2_get_nextzone(IdFile,IdZone,last)
!-----------------------------------------------------------------------
! Function : For the file IdFile, get the next zone in the file. 
!            the returned value is the number of the new active zone. 
!            If the returned zone id is negative, an error occured.
!            or the end of file is reached.
!            If the "last" flag is set to true, the current zone is the
!            last one in the file.
!            All zone informations are stored in the zonepara section
!            of the file descriptor. Data are read (connectivity as well
!            as the data arrays) and are available for the user until
!            reading the next zone or closing the file.
! Status : TODO
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Arguments
  integer, intent(in)  :: IdFile
  integer, intent(out) :: IdZone
  logical, intent(out) :: last
  
!-----------------------------------------------------------------------
! Local declarations
!-----------------------------------------------------------------------

  type(File_S), pointer     :: FileD
  type(DataSet_S),pointer  :: DataD
  type(ZoneData_S),pointer :: ZoneData
  type(ZoneArg_S),pointer  :: ZonePara
  integer, dimension(20) :: iparam_temp
  integer :: nelem, npoin, ndp,ndat
  integer :: i,j
  integer :: ok
  character(len=80) :: next_title
  logical :: eq_nelem, eq_ndp, eq_npoin

!-----------------------------------------------------------------------
  if (debug) call proc_begin('serafin2_get_nextzone')
!-----------------------------------------------------------------------
  ! Set the zone id to something invalid
  IdZone = -1
  ! The next zone can be read only if the file ID is valid.
  if ( ValidFile(IdFile) ) then
     ! get the pointer shortcuts to the descriptors of the file contents
     ! dataset, zoneparameter and the zone data set.
     FileD    => FileList(IdFile)
     DataD    => FileD%DataSet
     ZonePara => FileD%ZonePara
     ZoneData => ZoneDataList(IdFile)

     ! One more zone read for this data set.
     DataD%Num_Zones = DataD%Num_Zones + 1
     ! The returned zone Id is the number of zones read in the data set.
     IdZone          = DataD%Num_Zones

     ! Read the zone header :
     !------------------------------------------------------------------
     ! Z1 : the title is a 80 character string
     read(FileD%LU,IOSTAT=ok) ZonePara%Title(1:80)
     if ( debug ) then
         message_1(1) = 'Titre zone : <' // zonepara%title // '>'
         message_1(2) = 'Zone title : <' // zonepara%title // '>'
         call write_listing(message_1)
     end if
     ! Z2 : The solution time for this zone
     read(FileD%LU,IOSTAT=ok) ZonePara%time
     if ( debug ) then
         message_1(1) = 'Solution time : '
         message_1(2) = 'Solution time : '
         call write_listing(message_1,zonepara%time)
     end if
     
     ! Z3 : The IPARAM section
     ! read this section into a temporary array. This is done in order
     ! to compare the dimensions NELEM, NPOIN ... with the previous
     ! ones. if there is a difference, we should Reallocate 
     ! the data arrays and connectivity tables ...
     read(FileD%LU,IOSTAT=ok) IPARAM_temp
     nelem = iparam_temp(ID_NELEM)
     npoin = iparam_temp(ID_NPOIN)
     ndp   = iparam_temp(ID_NDP)   
     if ( debug ) then
         message_1(1) = 'NELEM : '
         message_1(2) = 'NELEM : '
         call write_listing(message_1, nelem)
         message_1(1) = 'NPOIN : '
         message_1(2) = 'NPOIN : '
         call write_listing(message_1, npoin)
         message_1(1) = 'NDP   : '
         message_1(2) = 'NDP   : '
         call write_listing(message_1, ndp  )
     end if

     ! Z4 : Variable share list 
     read(FileD%LU,IOSTAT=ok) &
                 (ZonePara%VarShareList(i),i=1,DataD%NbVar)
     
     ! Z5 : passive variable list
     read(FileD%LU,IOSTAT=ok) &
                 (ZonePara%PassiveVarList(i),i=1,DataD%NbVar)
     
     ! Z6 aux data table 
     read(FileD%LU,IOSTAT=ok) &
                 (ZonePara%AuxVarValues(i),i=1,DataD%NbVarAux)

     ! (RE-) ALLOCATION OF THE ZONE DATA ARRAYS
     !------------------------------------------------------------------
     if (DataD%Num_Zones .EQ. 1 ) then 
       ! if we are the first time here, ie DataD%Num_Zones equals one,
       ! we should allocate ALL data arrays :
       call alloc_zonedata(DataD%NbVar,DataD%VarLocation, &
                           nelem,npoin,ndp,ZoneData)
     else
       ! If the zone data arrays are already allocated, check the 
       ! sizes of the arrays.
       eq_nelem = NELEM .EQ. ZonePara%IPARAM(ID_NELEM)
       eq_npoin = NPOIN .EQ. ZonePara%IPARAM(ID_NPOIN)
       eq_ndp   = NDP   .EQ. ZonePara%IPARAM(ID_NDP)
       ! if the number of elements is not the same, reallocate
       ! all P0 arrays.
       if ( .NOT. eq_nelem .OR. .NOT. eq_npoin .OR. .NOT. eq_ndp ) then
          
         call clear_zonedata_d(zonedata)
         if ( iparam_temp(ID_SHARE_CONNECT) .NE. 0 ) then
            message_1(1)='Impossible de partager la connectivite : '//&
                         'nelem/npoin/ndp ont change'
            message_1(2)='It is not possible to share the connectivity'&
                         //'nelem/npoin/ndp changed.'
            call write_listing(message_1)
            IdZone = -1
            ! TODO : check for variable sharing : not possible if the
            ! number of data points changed!!!!
         else

            call alloc_zonedata(DataD%NbVar,DataD%VarLocation, &
                             NELEM,NPOIN,NDP,ZoneData)
         end if ! check for sharing connectivity
       end if
           
      end if ! (re-)allocation of zone data descriptor

      ! Put the iparam section read into the zone parameter 
      ! descriptor.
      ZonePara%Iparam(:) = iparam_temp(:)

      if ( IdZone .NE. -1 ) then
      ! Check if we should read the connectivity. This is the case
      ! if the data set contains finite element like data and if the
      ! connectivity is not shared. If we are in the first zone,
      ! connectivity can not be shared.

        if ( DataD%DataType .EQ. DATA_FE ) then
        write(*,*)'Finite element data '
          ! If connectivity is shared, no need to read it again.
          ! Sharing connectivity is NOT possible for the first zone
          ! in a file (no connectivity existing prior to the 
          ! zone read / write)
          if ( ZonePara%IPARAM(ID_Share_Connect) .NE. 0 ) then
            if ( DataD%Num_Zones .EQ. 1 ) then
              message_1(1) ='Impossible de partager la connectivite '&
                            //'dans la premiere zone.'
              message_1(2) ='It is not possible to share the '//&
                             'connectivity in the first zone'
              call write_listing(message_1)
              IdZone = -1
            end if
          else 
          ! If connectivity is not shared, we should read it.
          write(*,*)'read connectivity'
          read(FileD%LU,IOSTAT=ok)(ZoneData%Ikle(i),i=1,nelem*ndp)
          write(*,*)'connectivity end.'
          ! TODO : check for knolg, ....
          end if
        end if

        ! Now read the data into the arrays :

        ! loop over all variables in the data set :

        do i = 1, dataD%NbVar

          ! get the number of data points via the varlocations 
          ! array. this should be valid even for point like data
          if ( DataD%VarLocation(i) .EQ. DISC_P0) then
             ndat = nelem
          else
             ndat = npoin
          end if
          
          if ( ZonePara%PassiveVarList(i) .NE. 0 ) then
            ! If the variable is declared to be passive, the
            ! value is set to 0
            ZoneData%DataArray(i)%r(:)= 0.D0
          else
            ! if the variable is not shared, nor passive, get its 
            ! values from the file.
            if ( ZonePara%VarShareList(i) .EQ. 0 ) then
              read(FileD%LU,IOSTAT=ok) &
                            (ZoneData%DataArray(i)%r(j),j=1,ndat)
            end if
          end if
           
        end do ! loop over all variables in the data set

     end if ! IdZone > 0

     IdZone = DataD%Num_Zones

     ! Try to Read the title of the next zone, in order to check for
     ! the EOF. If EOF, set last to TRUE. if not, do a Backspace to 
     ! repositioning to the beginning of the zone.

     read(FileD%LU,IOSTAT=ok) next_title(1:80)
     if ( ok .LT. 0 ) then
       last = .TRUE.
     else 
       BACKSPACE(FileD%LU)
       last = .FALSE.
     end if

  end if
  

!-----------------------------------------------------------------------
  if (debug) call proc_end('serafin2_get_nextzone')
!-----------------------------------------------------------------------
  return
end subroutine serafin2_get_nextzone


!-----------------------------------------------------------------------
subroutine serafin_get_zoneinfo_int(IdFile,IdParameter,value,ret)
!-----------------------------------------------------------------------
! Function : Get a zone argument of integer type.
!            Possible integer type parameters are :
!            ID_NELEM
!            ID_NPOIN
!            ID_NFAC
!            ID_NPLAN
!            ID_NPTFR
!            ID_NPTIR
!            ID_DISC
!            ID_NDP
!            ID_SHARE_CONNECT
!            these values are stored in the IPARAM section of the file,
!            the parameter id equals the entry in the IPARAM table.
! Status : TODO
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Arguments
  integer, intent(in) :: IdFile
  integer, intent(in) :: IdParameter
  integer,intent(inout) :: value
  integer, intent(out) :: ret
  
!-----------------------------------------------------------------------
! Local declarations
!-----------------------------------------------------------------------

  integer,dimension(:), pointer :: iparam

!-----------------------------------------------------------------------
  if (debug) call proc_begin('serafin_get_zoneinfo_int')
!-----------------------------------------------------------------------

! Set return value to failed.
  ret = -1

! Check if the file ID is valid 
  if ( ValidFile(IdFile) ) then
      iparam => FileList(IdFile)%ZonePara%IPARAM
      select case ( IdParameter ) 
        case (ID_NELEM )
          value = iparam(ID_NELEM)
          ret = 0
        case (ID_NPOIN )
          value = iparam(ID_NPOIN)
          ret = 0
        case (ID_NDP )
          value = iparam(ID_NDP)
          ret = 0
        case (ID_NFAC )
          value = iparam(ID_NFAC)
          ret = 0
        case (ID_NPLAN )
          value = iparam(ID_NPLAN)
          ret = 0
        case (ID_NPTFR )
          value = iparam(ID_NPTFR)
          ret = 0
        case (ID_DISC )
          value = iparam(ID_DISC)
          ret = 0
        case (ID_Share_Connect )
          value = iparam(ID_Share_Connect)
          ret = 0
        case default 
           message_1(1) = 'Parametre de type entier inconnu (ZonePara)'
           message_1(2) = 'Unknown zone parameter of integer type'
           call write_listing(message_1)
      end select
  end if
  

!-----------------------------------------------------------------------
  if (debug) call proc_end('serafin_get_zoneinfo_int')
!-----------------------------------------------------------------------
  return
end subroutine serafin_get_zoneinfo_int


!-----------------------------------------------------------------------
subroutine serafin_get_zoneinfo_real(IdFile,IdParameter,value,ret)
!-----------------------------------------------------------------------
! Function : Get a zone argument of real type.
!            The only possible real type zone info is
!            ID_SOLUTIONTIME
! Status : TODO
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Arguments
  integer, intent(in) :: IdFile
  integer, intent(in) :: IdParameter
  double precision,intent(inout) :: value
  integer, intent(out) :: ret
  
!-----------------------------------------------------------------------
! Local declarations
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
  if (debug) call proc_begin('serafin_get_zoneinfo_real')
!-----------------------------------------------------------------------
  ret = -1
! check if file id is valid
  
  if ( validfile(IdFile) ) then

      if ( IdParameter .EQ. ID_SOLUTIONTIME ) then
         value = FileList(IdFile)%ZonePara%time
         ret = 0
      else
         message_1(1)='Parametre de type Reel inconu (ZonePara)'
         message_1(2)='Unknown real type parameter for zone '
         call write_listing(message_1)
         ret = -1
      end if

  end if

!-----------------------------------------------------------------------
  if (debug) call proc_end('serafin_get_zoneinfo_real')
!-----------------------------------------------------------------------
  return
end subroutine serafin_get_zoneinfo_real

!-----------------------------------------------------------------------
subroutine serafin_get_zoneinfo_str(IdFile,IdParameter,value,ret)
!-----------------------------------------------------------------------
! Function : Get a zone argument of string type.
!            The possible arguments :
!            ID_TITLE : the title of the current zone
! Status : TODO
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Arguments
  integer, intent(in) :: IdFile
  integer, intent(in) :: IdParameter
  character(len=80),intent(inout) :: value
  integer, intent(out) :: ret
  
!-----------------------------------------------------------------------
! Local declarations
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
  if (debug) call proc_begin('serafin_get_zoneinfo_str')
!-----------------------------------------------------------------------

  if( ValidFile(IdFile) ) then
     if ( IdParameter .EQ. ID_TITLE ) then
         value(1:80) = FileList(IdFile)%ZonePara%Title 
         ret = 0
     else
         message_1(1)='Parametre de type chaine inconu (ZonePara)'
         message_1(2)='Unknown string type parameter for zone '
         call write_listing(message_1)
         ret = -1
     end if
 end if

!-----------------------------------------------------------------------
  if (debug) call proc_end('serafin_get_zoneinfo_str')
!-----------------------------------------------------------------------
  return
end subroutine serafin_get_zoneinfo_str


!-----------------------------------------------------------------------
subroutine serafin_get_zoneinfo_int_tab(IdFile,IdParameter, &
                                        ndat,value,ret)
!-----------------------------------------------------------------------
! Function : Get a zone argument of integer table type.
!            Possible zone parameters of integer table type :
!            ID_VAR_SHARE_LIST : the list of variable to be shared.
!                                shared variables are not read in the
!                                current zone 
!            ID_PASSIVE_VAR_LIST : passive variables have a value of 0.
!            the user should provide the dimension of the table, which
!            should be the number of domain variables in the current
!            file.
!          
! Status : TODO
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Arguments
  integer, intent(in) :: IdFile
  integer, intent(in) :: IdParameter
  integer, intent(in) :: ndat
  integer,dimension(ndat),intent(inout) :: value
  integer, intent(out) :: ret
  
!-----------------------------------------------------------------------
! Local declarations
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
  if (debug) call proc_begin('serafin_get_zoneinfo_int_tab')
!-----------------------------------------------------------------------

 ret = -1
 if ( ValidFile(IdFile) ) then
     select case ( IdParameter ) 
         case ( Id_VAR_SHARE_LIST ) 
           if ( ndat .NE. FileList(IdFile)%DataSet%NbVar ) then
             message_1(1) = 'Mauvaise dimension pour tableau VarShare'
             message_1(2) = 'Bad Dimension for variable share list '
             call write_listing(message_1)
             ret = -1
           else
             value(:) = FileList(IdFile)%ZonePara%VarShareList(:)
           end if
         case ( Id_Passive_Var_List ) 
           if ( ndat .NE. FileList(IdFile)%DataSet%NbVar ) then
             message_1(1) = 'Mauvaise dimension pour tableau Passive Vars'
             message_1(2) = 'Bad Dimension for passive variable list '
             call write_listing(message_1)
             ret = -1
           else
             value(:) = FileList(IdFile)%ZonePara%PassiveVarList(:)
           end if
         case default 
         message_1(1)='Parametre de type tab entier inconnu (ZonePara)'
         message_1(2)='Unknown Zone parameter of intger table type'
         call write_listing(message_1)
     end select
 end if

!-----------------------------------------------------------------------
  if (debug) call proc_end('serafin_get_zoneinfo_int_tab')
!-----------------------------------------------------------------------
  return
end subroutine serafin_get_zoneinfo_int_tab

!-----------------------------------------------------------------------
subroutine serafin2_get_auxdata(IdFile,ndat,value,ret)
!-----------------------------------------------------------------------
! Function : Get the auxiliary data list
! Status : TODO
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Arguments
  integer, intent(in) :: IdFile
  integer, intent(in) :: ndat
  double precision, dimension(ndat),intent(inout) :: value
  integer, intent(out) :: ret
  
!-----------------------------------------------------------------------
! Local declarations
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
  if (debug) call proc_begin('serafin2_get_auxdata')
!-----------------------------------------------------------------------
  ret = -1
  if ( ValidFile(IdFile) ) then
      if ( ndat .EQ. FileList(IdFile)%DataSet%NbVarAux ) then
          value(1:ndat) = FileList(IdFile)%ZonePara%AuxVarValues(1:ndat)
          ret = 0
      else
          message_1(1) ='Mauvaise dimension pour valeurs aux. attendu '
          message_1(2) ='Bad Dimension for aux data table. expected : '
          call write_listing(message_1, &
                    FileList(IdFile)%DataSet%NbVarAux)
          ret = -1
      end if
  end if

!-----------------------------------------------------------------------
  if (debug) call proc_end('serafin2_get_auxdata')
!-----------------------------------------------------------------------
  return
end subroutine serafin2_get_auxdata



!-----------------------------------------------------------------------
subroutine serafin2_get_data_array(IdFile,nvar,ndat,value,ret)
!-----------------------------------------------------------------------
! Function : Get the data array for the variable number nvar in the 
!            data file. The values are for the current zone (last zone
!            read by the user) 
!            The user should provide as an input the expected size of 
!            the data array, in order to check it.
! Status : TODO
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Arguments
  integer, intent(in) :: IdFile
  integer, intent(in) :: ndat
  integer, intent(in) :: nvar
  double precision, dimension(ndat),intent(inout) :: value
  integer, intent(out) :: ret
  
!-----------------------------------------------------------------------
! Local declarations
!-----------------------------------------------------------------------
  integer :: nval

!-----------------------------------------------------------------------
  if (debug) call proc_begin('serafin2_get_data_array')
!-----------------------------------------------------------------------
  ret = -1
  ! Check if the file id is valid.
  if ( ValidFile(IdFile) ) then
      if ( debug ) then
          message_1(1)='Lecture de la variable no '
          message_1(2)='Reading variable number '
          call write_listing(message_1,nvar)
      end if
      ! The variable id should be greater than zero and less or equal
      ! to the number of variables in the data set. 
      if(nvar.LE.0 .OR. nvar.GT.FileList(IdFile)%DataSet%NbVar ) then
          message_1(1) ='Numero de variable invalide'
          message_1(2) ='Invalid variable Id'
          call write_listing(message_1)
          ret = -1
      else
          ! If the variable id is valid, get the discretization.
          ! The discretization defines the number of data points to 
          ! read. The user should give the right size of the array (he
          ! knows what he is reading), otherwise this is an error. 
          if ( FileList(IdFile)%Dataset%VarLocation(nvar) .EQ. 0 ) then
              nval = FileList(IdFile)%ZonePara%IPARAM(Id_NELEM)
          else
              nval = FileList(IdFile)%ZonePara%IPARAM(Id_NPOIN)
          end if
          ! (don't check other options for VarLocation. (already done
          ! when reading the file header.

          if ( ndat .NE. nval ) then
              message_1(1)='Mauvaise dimension pour tableau. attendu :'
              message_1(2)='Bad dimension for data array. expected :'
              call write_listing(message_1,nval)
          else
              ! If the array has the right size, put in the data values.
              value(1:ndat) = &
                      ZoneDataList(IdFile)%DataArray(nvar)%r(1:ndat) 
          end if
      end if
  end if

!-----------------------------------------------------------------------
  if (debug) call proc_end('serafin2_get_data_array')
!-----------------------------------------------------------------------
  return
end subroutine serafin2_get_data_array

!-----------------------------------------------------------------------
subroutine serafin2_get_ikle(IdFile,mode,nelem,ndp,value,ret)
!-----------------------------------------------------------------------
! Function : Get the connectivity table in a given mode 
!            FULL_INTERLACE (les numeros de noeuds pour un element se
!            suivent (E1_N1,E1_N2,E1_N3;E2_N1,E2,N2,E2_N3;...)
!            NO_INTERLACE on stocke d'abord pour tous les elements le 
!            numero du premier noeud, ensuite pour tous les elements 
!            le deuxieme ..(E1_N1,E2_N1,E3_N1..;E1_N2,E2_N2,E3_N2 ...)
! Status : TODO
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Arguments
  integer, intent(in) :: IdFile
  integer, intent(in) :: mode
  integer, intent(in) :: nelem
  integer, intent(in) :: ndp
  integer, dimension(ndp*nelem),intent(inout) :: value
  integer, intent(out) :: ret
  
!-----------------------------------------------------------------------
! Local declarations
!-----------------------------------------------------------------------
  integer :: i,j
  integer, dimension(:), pointer :: ikle

!-----------------------------------------------------------------------
  if (debug) call proc_begin('serafin2_get_ikle')
!-----------------------------------------------------------------------
  ret = -1
  ! Check if the file id is valid.
  if ( ValidFile(IdFile) ) then
      ! Check the number of elements and the number of nodes per element
      if ( nelem .NE. FileList(IdFile)%ZonePara%IPARAM(ID_NELEM) ) then
          message_1(1) = 'Mauvais nombre d''elements. valeur attendue :'
          message_1(2) = 'Bad number of elements. expected : '
          call write_listing(message_1, &
                          FileList(IdFile)%ZonePara%IPARAM(ID_NELEM) ) 
      end if
      if ( ndp .NE. FileList(IdFile)%ZonePara%IPARAM(ID_NDP) ) then
          message_1(1) = 'Mauvais nombre pour ndp. valeur attendue :'
          message_1(2) = 'Bad number of nodes per element. expected : '
          call write_listing(message_1, &
                          FileList(IdFile)%ZonePara%IPARAM(ID_NDP) ) 
      end if
      ikle => ZoneDataList(IdFile)%ikle
      ! Check the mesh mode and fill in the array
      select case (mode)

        case ( NO_INTERLACE)
          if ( debug ) then
            message_1(1) = 'Recup maillage en mode NO_INTERLACE)'
            message_1(2) = 'Get mesh in mode NO_INTERLACE)'
            call write_listing(message_1)
          end if
          ! In the mode NO_INTERLACE, we store together the nodes 
          ! of an element, E1_N1, E1_N2, E1_N3 .... E2_N1,E2_N2 ...
          ! The storage in the serafin2 file is NO_INTERLACE, and we 
          ! can get directly the connectivity :
          value(:)=ikle(:)
          ret = 0
        case ( FULL_INTERLACE)
          if ( debug ) then
            message_1(1) = 'Recup maillage en mode FULL_INTERLACE)'
            message_1(2) = 'Get mesh in mode FULL_INTERLACE)'
            call write_listing(message_1)
          end if
          ! In the mode FULL_INTERLACE,  we store first the node number
          ! one of all elements, than the node number two for all
          ! elements ... E1_N1, E2_N1, E3_N1 ... E1_N2, E2_N2, E3_N2 ..
          do i = 1, ndp
            do j = 1, nelem
              value((i-1)*nelem+j)=ikle((j-1)*ndp+i)
            end do
          end do
          ret = 0
        case default
                 message_1(1) = 'Format stockage maillage inconnu'
                 message_1(2) = 'Unknown format for mesh storage'
                 call write_listing(message_1)
      end select

  end if

!-----------------------------------------------------------------------
  if (debug) call proc_end('serafin2_get_ikle')
!-----------------------------------------------------------------------
  return
end subroutine serafin2_get_ikle



!-----------------------------------------------------------------------
logical function ValidFile(IdFile )
!-----------------------------------------------------------------------
! Function: Does stuff
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Arguments
  Integer, intent(in) :: idfile
  
!-----------------------------------------------------------------------
! Local declarations

!-----------------------------------------------------------------------

  ! init to invalid file ID
  ValidFile = .FALSE.

  ! Check if file id is in valid range
  if (IdFile <= 0 .OR. IdFile .GT. NMax_files) then
    message_1(1) = 'Identifiant fichier invalide (M_READ_SERAFIN) : '
    message_1(2) = 'Invalid file id (M_READ_SERAFIN) : '
    call write_listing(message_1,IdFile)

  else
    ! check if file is allocated (has a valid LU associated)
    if ( FileList(IdFile)%LU .EQ. -1) then
        message_1(1) = 'Fichier pas ouver (M_READ_SERAFIN) : '
        message_1(1) = 'File not opened (M_READ_SERAFIN) : '
        call write_listing(message_1,IdFile)
    else
        ValidFile = .TRUE.
    end if
  end if

!-----------------------------------------------------------------------
end function validfile
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
subroutine alloc_zonedata(nbvar,VarLocations,nelem,npoin,ndp,ZoneData) 
!-----------------------------------------------------------------------
! Function : (Re-)Allocate all data structures for the ZoneData_S
!            structure. Data arrays are dimensioned by the number of
!            elements or the number of nodes in the mesh (for FE data).
!            In case of point like data, VarLocation was set to 1 by the
!            procedure creating the file.

! Status : TODO
!-----------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------
! Arguments
  integer, intent(in) :: nbvar
  integer, intent(in), dimension(nbvar) :: VarLocations
  integer, intent(in) :: nelem
  integer, intent(in) :: npoin
  integer, intent(in) :: ndp
  type(ZoneData_S), intent(inout) :: ZoneData
  
!-----------------------------------------------------------------------
! Local declarations
!-----------------------------------------------------------------------

  integer :: i, ndat

!-----------------------------------------------------------------------
  if (debug) call proc_begin('alloc_zonedata')
!-----------------------------------------------------------------------

! For all variables in the file, get the variable location.
       do i = 1, NbVar
           ! In case of P0 discretization, the size of the data array
           ! is the number of elements.
           if ( VarLocations(i) .EQ. 0 ) then 
             ndat = nelem
           ! For point like data or node centered data, the number of
           ! values in an array is given by the number of nodes /
           ! points.
           else if (VarLocations(i) .EQ. 1 ) then 
             ndat = npoin
           else
           ! No other values are possible for the variable location.
             message_1(1)='Discretisation inconnue de la variable no '
             message_1(2)='Unknown discretisation for the variable no '
             call write_listing(message_1,i)
           end if
           ! Allocate the array.
           allocate(ZoneData%DataArray(i)%r(ndat))
       end do
       ! in case of finite element data, we should allocate the 
       ! connectivity table. For finite element data, the number 
       ! elements and the number of nodes per element are not zero.
       if ( nelem .NE. 0 .AND. NDP .NE. 0 ) then
          allocate(ZoneData%Ikle(NELEM*NDP))
       end if

       ! TODO knolg

!-----------------------------------------------------------------------
  if (debug) call proc_end('alloc_zonedata')
!-----------------------------------------------------------------------
  return
!-----------------------------------------------------------------------
end subroutine alloc_zonedata
!-----------------------------------------------------------------------

end module m_read_serafin2
