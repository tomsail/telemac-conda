!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  WRITE_Action_Visualisation     !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & (  )
!
      USE m_TypeDefs_Nestor
!
      USE m_Nestor, ONLY :  A, nActions, ipid, ParallelComputing
     &                       , SisStart
!
#ifndef NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY : open_File
#endif
!
      IMPLICIT NONE
!
#ifndef NESTOR_INTERFACES
      !--------------------- local variables ---------------
      INTEGER :: m, fu
      REAL (KIND=R8)  :: checkTime, x,y, tStart, tEnd, tRepeat
      REAL (KIND=R8)  :: min_tStart, max_tEnd
      REAL (KIND=R8)  :: x1,  y1,  x2,  y2, dy,  lit
      REAL (KIND=R8)  :: offset, scS, txS
      CHARACTER (128) :: myText1, str1, str2, filename
!
!      dbug WRITE(6,*)'?>-------  SR WRITE_Action_Visualisation ---'
!
      IF( ParallelComputing .AND. ipid.NE.0 ) RETURN
!
859   FORMAT(A6,'|',I4,2('.',I2.2),'-',2(I2.2,':'),I2.2,'|')    ! date
861   FORMAT(A6,2('|',G15.8),'|',A12,2('|',G11.4),'|')          ! axis or line
972   FORMAT(A6,2('|',G15.8),'|',A12,2('|',G11.4),2('|',A6) )   ! circle
983   FORMAT(A6,2('|',G15.8),'|',A12,'|',G11.4,'|',A,'|')       ! text
!
      fu = 476
      fileName = "../NestorGraphic_data.dat"
      OPEN( fu, FILE   = fileName
     &        , STATUS = 'REPLACE'
     &        , ACTION = 'WRITE'   )
!
!
!
      ! ---- date and time ----------------------
      WRITE(fu,859)'date', SisStart%year, SisStart%month, SisStart%day
     &              ,SisStart%hour, SisStart%minutes, SisStart%seconds
!
      liT    =   4.0     ! line Thickness
      scS    =  50.0     ! scatter Size
      txS    =   8.0     ! text Size
!
!
!     ==== WRITE legend =======================================================================
      x1     =   3.0
      y1     =   4.0
      x2     =   4.0
      dy     =   1.0     !  offset value -> line space in legend
!
      ! ---- legend Dig_by_time --------------
      y = y1           ! => top line
      WRITE(fu,861)'L-line', x1,     y, 'blue', liT, x2
      WRITE(fu,983)'L-text', x2+0.2, y,'black', txS,'Dig_by_time'
!
      ! ---- legend Dig_by_criterion ---------
      y = y1 - 1.0*dy  ! => new line
      WRITE(fu,861)'L-line', x1    , y, 'blue', liT, x2
      WRITE(fu,983)'L-text', x2+0.2, y,'black', txS,'Dig_by_criterion'
      x = x1
      DO m=1, 3
         WRITE(fu,972)'L-circ', x, y, 'black', liT, scS,'no','green'
         x = x + 0.5
      ENDDO
!
      ! ---- legend Dump_by_time -------------
      y = y1 - 2.0*dy  ! => new line
      WRITE(fu,861)'L-line', x1,     y,  'red', liT, x2
      WRITE(fu,983)'L-text', x2+0.2, y,'black', txS,'Dump_by_time'
!
      ! ---- legend Backfill_to_level ---------
      y = y1 - 3.0*dy  ! => new line
      WRITE(fu,972)'L-circ', x1,     y,'black', liT, scS,'no','green'
      WRITE(fu,861)'L-line', x1,     y,'green', liT, x2
      WRITE(fu,983)'L-text', x2+0.2, y,'black', txS,'Backfill_to_level'
!
      ! ---- legend Reset_bottom ---------
      y = y1 - 4.0*dy  ! => new line
      WRITE(fu,861)'L-line', x1,     y,'saddlebrown', liT, x2
      WRITE(fu,983)'L-text', x2+0.2, y,'black',txS,'Reset_bottom'
!
      ! ---- legend Save_water_level ---------
      y = y1           ! => top line
      x1= 7.0
      WRITE(fu,972)'L-circ', x1,     y,'blue' , liT, scS,'no','green'
      WRITE(fu,983)'L-text', x1+0.2, y,'black', txS,'Save_water_level'
!     ==== end WRITE legend =======================================================================
!
!
      DO m=1, nActions
        IF( A(m)%ActionType == 5 ) THEN        !>  Save_water_level
          A(m)%TimeEnd = A(m)%TimeStart + 1.D0 !>  A(m)%TimeEnd is not set
        ENDIF                                  !   in the action file but we need a
      ENDDO                                    !   proprer value for the next DO loop
!
      min_tStart =  90000000000.0D0
      max_tEnd   = -10000000000.0D0
      DO m=1, nActions  !> identify the total time interval over all actions
        tStart  = A(m)%TimeStart
        tEnd    = A(m)%TimeEnd
        IF( tStart < min_tStart)  min_tStart = tStart
        IF( tEnd   > max_tEnd  )  max_tEnd   = tEnd
      ENDDO
!
                                                         !> only to make a nice graphic
      x1 = min_tStart - 0.05*(max_tEnd - min_tStart)     !  extent the time interval about -5% (x-axis range min value)
      x2 = max_tEnd   + 0.05*(max_tEnd - min_tStart)     !                           about +5% (x-axis range max value)
      x2 = max_tEnd   + 0.30*(max_tEnd - min_tStart)     !                           about +5% (x-axis range max value)
      !x2 = max_tEnd   + 1.10*(max_tEnd - min_tStart)     !                           about +5% (x-axis range max value)
      y1 = -1.0D0                                        !  set    -1                          (y-axis range min value)
      y2 = real(nActions+1,kind=selected_real_kind( 10, 60 ))                            !  extent number of actions about +1  (y-axis range max value)
!
      !offset = (max_tEnd - min_tStart) / 300.0D0    !> offset of text in the graphic
      offset = (max_tEnd - min_tStart) / 100.0D0    !> offset of text in the graphic
!
      ! axis -------------------------
      WRITE(fu,861)'X-axis',x1,x2,'black',4,-999
      WRITE(fu,861)'Y-axis',y1,y2,'black',4,-999
!
!
!
      DO m=1, nActions   !==================================================
       y       = REAL(m,kind=selected_real_kind( 10, 60 ))
       tStart  = A(m)%TimeStart
       tEnd    = A(m)%TimeEnd
       tRepeat = A(m)%TimeRepeat
!
       SELECT CASE( A(m)%ActionType ) !=====================================
        CASE( 1 )    !------------- Dig_by_time --------------------------
           str1    = A(m)%FieldDig
           str2    = ''
           IF( A(m)%DumpPlanar ) str2 = A(m)%ReferenceLevel
           myText1 = TRIM(str1)//'   '//TRIM(str2)
           WRITE(fu,983)'  text', tEnd+offset, y, 'black'
     &                          , txS,  myText1
           WRITE(fu,861)'  line', tStart, y, 'blue', liT, tEnd
!
!
        CASE( 2 )    !------------- Dump_by_time -------------------------
           WRITE(fu,983)' text ', tEnd+offset, y, 'black'
     &                          , txS,  TRIM(A(m)%FieldDump)
           WRITE(fu,861)' line ', tStart, y, 'red', liT, tEnd
!
!
        CASE( 3 )    !------------- Dig_by_criterion --------------------
           str1    = A(m)%FieldDig
           str2    = ''
           IF( A(m)%DumpPlanar ) str2 = A(m)%ReferenceLevel
           myText1 = TRIM(str1)//'   '//TRIM(str2)
           WRITE(fu,983)'  text', tEnd+offset, y, 'black'
     &                          , txS,  myText1
           WRITE(fu,861)'line', tStart, y, 'blue', liT, tEnd
!
           checkTime = tStart
           DO WHILE( checkTime <= tEnd )
             WRITE(fu,972)'circle', checkTime, y, 'black', liT, scS
     &                            , 'no', 'green'
             checkTime = checkTime + tRepeat
           ENDDO
!
!
!
!
        CASE( 4 )    !------------- Reset_bottom -------------------------
           WRITE(fu,983)'  text', tEnd+offset, y, 'black'
     &                          , txS,  TRIM(A(m)%FieldDump)
           WRITE(fu,861)'  line', tStart, y, 'saddlebrown', liT, tEnd
!
!
        CASE( 5 )    !------------- Save_water_level ---------------------
           WRITE(fu,983)'  text', tEnd+offset, y, 'black'
     &                          , txS,  TRIM(A(m)%ReferenceLevel)
           WRITE(fu,972)'circle', tStart, y, 'blue', liT, scS
     &                          , 'no', 'red'
!
!
        CASE( 6 )    !------------- Backfill_to_level --------------------
           str1    = A(m)%FieldDump
           str2    = A(m)%ReferenceLevel
           myText1 = TRIM(str1)//'   '//TRIM(str2)
           WRITE(fu,983)'  text', tEnd+offset, y, 'black'
     &                          , txS,  myText1
!
           WRITE(fu,861)'  line', tStart, y, 'green', liT, tEnd
!
!
           WRITE(fu,972)'circle', tStart, y, 'black', liT, scS
     &                          , 'no', 'green'
!
        CASE DEFAULT !-- Dig_by_time -- Dump_by_time -- Reset_bottom ----
!
       END SELECT    !===================================================
      ENDDO    !===================================================
!
      CLOSE(fu)
!
!      dbug WRITE(6,*)'?>-------  SR WRITE_Action_Visualisation End '
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE WRITE_Action_Visualisation  !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
