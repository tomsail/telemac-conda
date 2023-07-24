!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  InitialiseNestor               !********************************************
!***                                              ********************************************
!***                                              ********************************************
!
     & (  ncsize_Sis, ipid_Sis, npoin_Sis, nSiCla_Sis
     &  , NodeArea_sis, x_sis, y_sis
     &  , SisStartDate, SisStartTime, SisMorpholFactor
     &  , npoin_SisGlobal, SisGraphicOutputPeriod
     &  , ZF
     &  , Lu_outp, LuAct_F, LuPol_F, LuRef_F, LuRst_F
     &  , calledby_t2d
     &  , ZRL    ! reference level [m+NN]
     & )
!
      USE m_TypeDefs_Nestor
      USE m_Nestor
      USE INTERFACE_PARALLEL, ONLY : P_DSUM
!
#ifndef  NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  ReadPolygons
     &                               , ErrMsgAndStop
     &                               , ReadDigActions
     &                               , inside_point_2d_d
#endif
!
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: ncsize_Sis, ipid_Sis, npoin_Sis
      INTEGER, INTENT(IN) :: nSiCla_Sis
!
      REAL (KIND=R8), INTENT(IN)
     &              , DIMENSION (npoin_Sis) ::   NodeArea_sis
     &                                         , x_sis
     &                                         , y_sis
!
      INTEGER, INTENT(IN)
     &       , DIMENSION (3)     ::  SisStartDate     ! year , month  , day
     &                             , SisStartTime     ! hours, minutes, seconds
      REAL (KIND=R8), INTENT(IN) ::  SisMorpholFactor ! morphological factor
!
      INTEGER, INTENT(IN) :: npoin_SisGlobal, SisGraphicOutputPeriod
!
      REAL (KIND=R8), INTENT(IN)
     &              , DIMENSION (npoin_Sis) ::   ZF     ! Bottom
!
      INTEGER, INTENT(IN)        ::  Lu_outp            ! Logical Unit for standard output
      INTEGER, INTENT(IN)        ::  LuAct_F, LuPol_F   ! Logical Unit of nestor input files:  ACTion, POLygon
     &                             , LuRef_F, LuRst_F   ! REFerence level, ReSTart  File
!
      LOGICAL                    ::  calledby_t2d
!
      REAL (KIND=R8), INTENT(IN)
     &              , DIMENSION (npoin_Sis) ::   ZRL
!
!
#ifndef NESTOR_INTERFACES
      !--------------------- local variables ---------------
!
!
      INTEGER  :: status
      INTEGER  :: i, n, m, sL
      INTEGER  :: iMesh         ! Mesh index of Field node
      INTEGER  :: nNodesInside
      INTEGER,ALLOCATABLE,DIMENSION (:) :: iTmp
      LOGICAL, PARAMETER :: lrinc = .TRUE.  ! Gehoert der Rand zum Polygongebiet dazu? Hier Ja
      LOGICAL  :: NodeInside                ! is grid node inside a Polygon?
      LOGICAL  :: NoDigFieldLinked, NoDumpFieldLinked   ! is Field linked to an action
!
      CHARACTER      (128)  :: str     ! to store a value as string
      TYPE(t_String_Length) :: SRname  ! subroutine where the error occured
!
!      dbug WRITE(6,*)'?>-------  SR InitialiseNestor ------------'
      SRname%s = "InitialiseNestor"   ! subroutine name
!
!
!
!     ====== copy value to the global Nestor-modul variable ============
      ipid          = ipid_Sis
      npoin         = npoin_Sis
      ncsize        = ncsize_Sis
      npoinGlobal   = npoin_SisGlobal
!
      nGrainClass   = nSiCla_Sis
      MorpholFactor = SisMorpholFactor
!
      Lu     = Lu_outp       ! file unit standard output
      LuActF = LuAct_F
      LuPolF = LuPol_F
      LuRefF = LuRef_F
      LuRstF = LuRst_F
!
      WRITE(Lu,*)'?>  -------- initialisation of Nestor --------------+'
      WRITE(Lu,*)'?>                                                  |'
      WRITE(Lu,*)'?> info:  max value of ZRL = ',MAXVAL(ZRL)
      WRITE(Lu,*)'?> info:  min value of ZRL = ',MINVAL(ZRL)
      WRITE(Lu,*)'?>'
      WRITE(Lu,*)'?> info: about nodes which were changed by Nestor.'
      WRITE(Lu,*)'?>       Each info line is marked with the Lable'
      WRITE(Lu,*)'?>       XdigX  or  XdumX  and followed by:'
      WRITE(Lu,*)"?>       TimeStart   ","Time   "
     & ,"delta-z  ","deltaVolume  ","Node  ","km   "
     & ,"X-coordinate  ","Y-coordinate  ","FieldNname  "
     & ,"ActionNumber"
      WRITE(Lu,*)'?>                                                  |'
      WRITE(Lu,*)'?>  ------------------------------------------------+'
!
!
!
      called_by_t2d = calledby_t2d
      IF(.NOT. called_by_t2d) morphodynamic = .TRUE.
!
      SisStart%year    = SisStartDate(1)   !> copy date values to
      SisStart%month   = SisStartDate(2)   !  the Nestor-modul
      SisStart%day     = SisStartDate(3)   !  DateTime structure
!                                            
      SisStart%hour    = SisStartTime(1)   !> copy time values to
      SisStart%minutes = SisStartTime(2)   !  the Nestor-modul
      SisStart%seconds = SisStartTime(3)   !  DateTime structure

      IF(ipid .EQ. 0) WRITE(Lu,'(A11,I5,5(I3))')"XstartDateX"
     &  ,SisStart%year, SisStart%month,   SisStart%day
     &  ,SisStart%hour, SisStart%minutes, SisStart%seconds
!
!
!     ==================================================================
!
!
      !Restart             = SisRestart
      !WRITE(6,*)'?> Restart = ', Restart
      GraphicOutputPeriod = SisGraphicOutputPeriod
!
      IF( ncsize  >   1 ) ParallelComputing = .TRUE.
!
!
!
      CALL ReadPolygons()
!
      !DO i=1, nPolys                                  ! debug
      !  WRITE(6,'("NAME:",A)')Poly(i)%name            ! debug
      !  WRITE(6,*)'numberPt = ',Poly(i)%nPoints       ! debug
      !  DO j=1, Poly(i)%nPoints                       ! debug
      !    WRITE(6,*)Poly(i)%Pt(j)%x, Poly(i)%Pt(j)%y  ! debug
      !  ENDDO                                         ! debug
      !ENDDO                                           ! debug
!
!
      nFields = nPolys
!
      ALLOCATE( F(nFields), stat=status )
      ALLOCATE( iTmp(npoin), stat=status )
      iTmp(:) = -1   ! initialisation with dummy value
!
!
!========================================================================
      DO n=1, nFields      !=============================================
                           !=============================================
!
      iTmp(:) = -1   !> initialise array iTmp
!
      F(n)%name    = Poly(n)%name
      READ(Poly(n)%name,'(I3)') F(n)%FieldID        !> read the (1:3) first string elements as integer
!
!
!      __________________________________________________________________
!     |________  find the Field-nodes    ________________________________|
      nNodesInside = 0                                             !> number of nodes inside
      DO i=1, npoin
        Call inside_point_2d_d ( y_sis(i)        , x_sis(i)        !> test if grid
     &                          ,Poly(n)                           !  nodes are inside
     &                          ,lrinc           , NodeInside    ) !  a polygon and
        IF(  NodeInside  ) THEN                                    !  store them in
          nNodesInside       = nNodesInside + 1                    !  array iTemp(:)
          iTmp(nNodesInside) = i
        ENDIF
      ENDDO
!
      !> now that we know the required size for the array F(n)%Node(:)
      ALLOCATE( F(n)%Node( nNodesInside ), stat=status)
      F(n)%Node(:) = iTmp(1:nNodesInside)
      F(n)%nNodes  = nNodesInside
!
      ALLOCATE( F(n)%NodeArea( nNodesInside ), stat=status)
      ALLOCATE( F(n)%X( nNodesInside )       , stat=status)
      ALLOCATE( F(n)%Y( nNodesInside )       , stat=status)
      ALLOCATE( F(n)%Z0( nNodesInside )      , stat=status)
      DO i=1, nNodesInside
        iMesh = F(n)%Node(i)
        F(n)%NodeArea(i) = NodeArea_sis( iMesh )
        F(n)%X(i)        = x_sis(        iMesh )
        F(n)%Y(i)        = y_sis(        iMesh )
        F(n)%Z0(i)       = ZF(           iMesh )
      ENDDO
!
      F(n)%Area = 0.0D0    !> set value of field area in the Field-Structur
      DO i=1, F(n)%nNodes                           !> Here for the current partition.
        F(n)%Area = F(n)%Area + F(n)%NodeArea(i)    !  For parallel processing we do the
      ENDDO                                         !  summation over all partitions later
      IF ( ParallelComputing ) THEN
!
        F(n)%Area = P_DSUM(F(n)%Area) !> this is the Field-area over all
                                      !  partitions which are "touched" by
                                      !  the Field
      ENDIF ! (ParallelComputing)
!
!
                               !=============================================
      ENDDO ! loop over Fields !=============================================
!============================================================================
!
      CALL ReadDigActions()
!
!
      DO m=1, nActions                                     !> adadaption due to the
        A(m)%TimeStart  = A(m)%TimeStart / MorpholFactor   !  compression of the timescale
        A(m)%TimeEnd    = A(m)%TimeEnd   / MorpholFactor   !  through the morholgical
        A(m)%TimeRepeat = A(m)%TimeRepeat/ MorpholFactor   !  factor
        A(m)%DigRate    = A(m)%DigRate   * MorpholFactor
        A(m)%DumpRate   = A(m)%DumpRate  * MorpholFactor
      ENDDO
!
      DO m=1, nActions
        IF( A(m)%DigVolume > 0.0D0 ) THEN           !> If DigVolume is not the default value (-0.1)
          A(m)%DigVolume  = -ABS( A(m)%DigVolume )  !  we change DigVolume to a negativ value
        ENDIF                                       !  because we remove the volume from the bottom
      ENDDO
!
!
      !--- link the Fields to the Action ---
      DO m=1, nActions  ! loop over Actions
        NoDigFieldLinked  = .TRUE.
        NoDumpFieldLinked = .TRUE.
        DO n=1, nFields
          IF( A(m)%FieldDigID  == F(n)%FieldID ) THEN
            A(m)%FieldDigID  = n   ! A(m)%FieldDigID  is now the index to element of F(:)
            NoDigFieldLinked = .FALSE.
          ENDIF
          IF( A(m)%FieldDumpID == F(n)%FieldID ) THEN
            A(m)%FieldDumpID = n   ! A(m)%FieldDumpID is now the index to element of F(:)
            NoDumpFieldLinked = .FALSE.
          ENDIF
        ENDDO ! end loop over Fields
!
        IF(       NoDigFieldLinked
     &     .AND. (A(m)%ActionType == 3 .OR. A(m)%ActionType == 1) )THEN! write error message and stop
          str = TRIM(A(m)%FieldDig)
          sL  = LEN_TRIM(str)
          Call ErrMsgAndStop( "while linking Action with Polygon  "
     &    ,"reason: The Action defines the FieldDig: "//str(1:sL)
     &    ,"        But the Polygon file contains no such Polygon "
     &    ,"occured in Action: ", m, SRname, ipid      )
        ENDIF
!
!
        IF( NoDumpFieldLinked ) THEN
         IF(   (        A(m)%ActionType  ==  1            ! 1=Dig_by_time
     &            .AND. A(m)%FieldDumpID >   0 )                                   ! Korrektur 07.05.2018
     &     .OR.(        A(m)%ActionType  ==  2 )          ! 2=Dump_by_time
     &     .OR.(        A(m)%ActionType  ==  3            ! 3=Dig_by_criterion
     &            .AND. A(m)%FieldDumpID >   0 )                                   ! Korrektur 07.05.2018
     &     .OR.(        A(m)%ActionType  ==  4 )          ! 4=Reset_bottom
     &     .OR.(        A(m)%ActionType  ==  6 ) ) THEN   ! 6=Backfill_to_level
          str = TRIM(A(m)%FieldDump)
          sL  = LEN_TRIM(str)
          Call ErrMsgAndStop( "while linking Action with Polygon  "
     &    ,"reason: The Action defines the FieldDump: "//str(1:sL)
     &    ,"        But the Polygon file contains no such Polygon "
     &    ,"occured in Action: ", m, SRname, ipid      )
         ENDIF
        ENDIF
!
!
      ENDDO  ! end loop over Actions
!
      !> initialise the status of the Actions:          0 = not yet      active
                                          !             1 = currently    active
      DO m=1, nActions                    !             2 = temporary  inactive
        A(m)%State = 0                    !             9 = for ever   inactive
        IF( A(m)%TimeStart  < 0.0 ) A(m)%State = 9     !> In case a action starts in the past
      ENDDO
!
!
      DO m=1, nActions         !> loop over Actions 
        n = A(m)%FieldDumpID
        IF( n <= 0 )  CYCLE    !> no dump field linked to the action
!
        IF(A(m)%ActionTypeStr(1:17) == 'Backfill_to_level') THEN
          A(m)%DumpPlanar = .TRUE.  !> default for Backfill_to_level
        ENDIF
!
        nNodesInside = F(n)%nNodes
        IF(A(m)%ActionTypeStr(1:12) == 'Reset_bottom') THEN
          !> allocate and set the initial bottom
          ALLOCATE( F(n)%Z0( nNodesInside ), stat=status)
          DO i=1, nNodesInside
            iMesh = F(n)%Node(i)
            F(n)%Z0(i) = ZF( iMesh )
          ENDDO
        ENDIF
!
      ENDDO  ! loop over Actions
!
!
      IF(      123456789.1D0 > MAXVAL(ZRL)           !> test if array ZRL is filled
     &   .AND.   MINVAL(ZRL) > 123456788.9D0 ) THEN  !  with default values
        DO m=1, nActions
         IF( calledby_t2d ) THEN
          IF( A(m)%ReferenceLevel(1:4) == 'GRID') Call ErrMsgAndStop
     &     ( "reason: You set 'ReferenceLevel = GRID' therefore Nestor"
     &      ,"        needs in the telemac2D geometry or restart file"
     &      ,"        values for the variable ZRL ('RFERENCE LEVEL')."
     &      ,"occured in Action: ", m, SRname, ipid      )
         ELSE    ! called by sisyphe
          IF( A(m)%ReferenceLevel(1:4) == 'GRID') Call ErrMsgAndStop
     &     ( "reason: You set 'ReferenceLevel = GRID' therefore Nestor"
     &      ,"      needs in the sisyphe/gaia geometry or restart file"
     &      ,"      values for the variable ZRL ('RFERENCE LEVEL')."
     &      ,"occured in Action: ", m, SRname, ipid      )
         ENDIF
        ENDDO
      ENDIF
!
!
!
      DO m=1, nActions      !> loop over Actions to set the methode of dumping
        IF(       A(m)%FieldDumpID > 0 ) THEN
          IF(       (.NOT. A(m)%DumpPlanar )
     &       .AND.  A(m)%DumpRate    < 0.0D0 ) THEN
            A(m)%DumpMode = 10  !> Dump_by_Time
          ENDIF
          IF(       A(m)%DumpPlanar
     &       .AND.  A(m)%DumpRate    < 0.0D0 ) THEN
            A(m)%DumpMode = 11  !> Dump_by_Time_Planar
          ENDIF
          IF(       (.NOT. A(m)%DumpPlanar )
     &       .AND.  A(m)%DumpRate    > 0.0D0 ) THEN
            A(m)%DumpMode = 20  !> Dump_by_Rate
          ENDIF
          IF(       A(m)%DumpPlanar
     &       .AND.  A(m)%DumpRate    > 0.0D0 ) THEN
            A(m)%DumpMode = 21  !> Dump_by_Rate_Planar
          ENDIF
        ELSE
          A(m)%DumpMode = -1    !> no dumping
        ENDIF
      ENDDO  ! loop over Actions
!
      DEALLOCATE( iTmp )
!
!      dbug WRITE(6,*)'?>-------  SR InitialiseNestor END --------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE InitialiseNestor            !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
