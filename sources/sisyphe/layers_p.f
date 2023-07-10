!                   *******************
                    SUBROUTINE LAYERS_P
!                   *******************
!
     &(PATH_PRE,FILE_PRE,JG)
!
!***********************************************************************
! SISYPHE   V7P2                                   16/05/2017
!***********************************************************************
!
!BRIEF   .CSV-FILE OUTPUT OF A LAYER PROFILE IN POINT J
!
!HISTORY  UWE MERKEL
!+        2011-07-20
!
!history  Uwe Merkel (UHM) R. Kopmann (BAW)
!+        23/11/2016, 2017
!+        V6P3, V7P2
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| JG             |<--| GLOBAL POINT NUMBER
!| PATH_PRE       |<--| WHERE TO SAVE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SISYPHE
      USE BIEF
      USE BIEF_DEF
      USE CVSP_OUTPUTFILES

      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)    :: JG
      CHARACTER(*),     INTENT(IN )    :: PATH_PRE,FILE_PRE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=100) :: DEBUGFILE
      CHARACTER(LEN=7)   :: OCSTR
      INTEGER  I,K,J
      DOUBLE PRECISION DEPTH, AT, MYFRA, BSUM
      INTEGER :: ID
!
!----------------------------------------------------------------
!
      OUTPUTCOUNTER = OUTPUTCOUNTER + 1
!
      WRITE(UNIT=OCSTR, FMT='(I7)') OUTPUTCOUNTER
      DO I=1,7
        IF(OCSTR(I:I)==' ') OCSTR(I:I)='0'
      ENDDO
      AT = DT*LT/PERCOU
!
!     GLOBAL NUMBERS TO GLOBAL NUMBERS
      J = JG
      IF(NCSIZE>1) J = GLOBAL_TO_LOCAL_POINT(JG,MESH)
!
      WRITE(UNIT=DEBUGFILE, FMT='(A,A,A,A,I8,A,G15.8,A)')
     &     PATH_PRE,OCSTR,'_',FILE_PRE,
     &     JG,'_T_',AT,'.LAY.CSV'
      DO I=1,LEN_TRIM(DEBUGFILE)
        IF(DEBUGFILE(I:I)==' ') DEBUGFILE(I:I)='_'
      END DO
!
      IF(J > 0) THEN !0 IF NODE IS NOT ON THIS PARTITION
        CALL GET_FREE_ID(ID)
        OPEN(ID, FILE=DEBUGFILE , STATUS='UNKNOWN')
        REWIND ID
        WRITE(ID,*)"J K FD50(I) AT Z AVAIL(J,K,I) X Y D50 TAU H"
!
        DEPTH = ZF%R(J)
!
        !LAYER TOP
        DO K=1,NLAYER%I(J)
!
          BSUM = 0.D0
          DO I=1,NSICLA
            BSUM = FDM(I)*AVAIL(J,K,I) + BSUM
          ENDDO
!
          DO I=1,NSICLA
            WRITE (ID,'(I8,1X,I4,1X,7(G15.8,1X))')
     &      JG,(NLAYER%I(J)-K+1),FDM(I),AT,DEPTH,
     &      AVAIL(J,K,I),X(J),Y(J), BSUM, TOB%R(J), Z%R(J)
          ENDDO
            DEPTH = DEPTH - ES(J,K)

        ENDDO
!
!     RIGID BED
!
      DO I=1,NSICLA
        BSUM = FDM(I)*AVAIL(J,NLAYER%I(J),I) + BSUM
      ENDDO
!
      DO I=1,NSICLA
            MYFRA = 0.D0
            IF (I==1) MYFRA = 1.D0
            WRITE (ID,'(I8,1X,I4,1X,7(G15.8,1X))')
     &      JG,0,FDM(I),AT,DEPTH,MYFRA,X(J),Y(J),BSUM
      ENDDO
!
      CLOSE(ID)
      ENDIF
!
!----------------------------------------------------------------
!
      RETURN
      END SUBROUTINE LAYERS_P
