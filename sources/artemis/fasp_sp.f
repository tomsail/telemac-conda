!                   ******************
                    SUBROUTINE FASP_SP
!                   ******************
!
     &(XRELV,YRELV,ZRELV,NP,X,Y,Z,I)
!
!***********************************************************************
! BIEF   V7P3                                   15/09/2017
!***********************************************************************
!
!brief    INTERPOLATES VALUES FROM A SET OF POINTS ON AN ARTEMIS BOUNDARY NODE.
!
!history  N.DURAND (HRW)
!+        15/09/2017
!+        V7P3
!+   Adapted from FASP
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NP             |-->| NUMBER OF TOMAWAC SPECTRA
!| XRELV          |-->| ABCISSAE OF TOMAWAC SPECTRA
!| YRELV          |-->| ORDINATES OF TOMAWAC SPECTRA
!| ZRELV          |-->| VALUES IN TOMAWAC SPECTRA
!| I              |-->| ARTEMIS BOUNDARY NODE NUMBER
!| X,Y            |-->| MESH COORDINATES
!| Z              |<--| INTERPOLATED VALUES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_ARTEMIS, ONLY : NDIR, DEBUG
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NP
      DOUBLE PRECISION, INTENT(IN)  :: X,Y
      DOUBLE PRECISION, INTENT(IN)  :: XRELV(NP),YRELV(NP)
      DOUBLE PRECISION, INTENT(IN)  :: ZRELV(NP,NDIR+1)
      DOUBLE PRECISION, INTENT(OUT) :: Z(NDIR+1)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N,INUM,I,IDIR
      INTEGER N1,N2,N3,N4
!
      DOUBLE PRECISION DIST1,DIST2,DIST3,DIST4
      DOUBLE PRECISION DIFX,DIFY,DIST
      DOUBLE PRECISION ZNUM,ZDEN
!
      LOGICAL OK1,OK2,OK3,OK4
!
!-----------------------------------------------------------------------
!
!     INITIALISES:
!
      DIST1=1.D12
      DIST2=1.D12
      DIST3=1.D12
      DIST4=1.D12
!
      OK1 = .FALSE.
      OK2 = .FALSE.
      OK3 = .FALSE.
      OK4 = .FALSE.
!
      INUM = 0
!
!     LOOP ON THE SET OF TOMAWAC SPECTRA POINTS (THERE ARE NP)
!     TO IDENTIFY THOSE CLOSEST TO ARTEMIS LIQUID BOUNDARY NODE :
!
      DO N=1,NP
        DIFX = XRELV(N)-X
        DIFY = YRELV(N)-Y
        DIST = DIFX*DIFX + DIFY*DIFY
!
        IF ( DIST.LT.1.D-6 ) DIST=1.D-6
! ->QUADRANT 1 :
        IF( DIFX.LE.0.D0 .AND. DIFY.LE.0.D0) THEN
          IF(DIST.LE.DIST1)THEN
            DIST1=DIST
            N1=N
            OK1 = .TRUE.
            INUM = INUM + 1
          ENDIF
! ->QUADRANT 2 :
        ELSEIF( DIFX.GE.0.D0 .AND. DIFY.LE.0.D0) THEN
          IF(DIST.LE.DIST2)THEN
            DIST2=DIST
            N2=N
            OK2 = .TRUE.
            INUM = INUM + 1
          ENDIF
! ->QUADRANT 3 :
        ELSEIF( DIFX.GE.0.D0 .AND. DIFY.GE.0.D0) THEN
          IF(DIST.LE.DIST3)THEN
            DIST3=DIST
            N3=N
            OK3 = .TRUE.
            INUM = INUM + 1
          ENDIF
! ->QUADRANT 4 :
        ELSEIF( DIFX.LE.0.D0 .AND. DIFY.GE.0.D0) THEN
          IF(DIST.LE.DIST4)THEN
            DIST4=DIST
            N4=N
            OK4 = .TRUE.
            INUM = INUM + 1
          ENDIF
        ENDIF
      ENDDO ! N=1,NP
!
!     END OF LOOP ON THE SET OF TOMAWAC SPECTRA POINTS
!
      IF(INUM.EQ.0) THEN
        WRITE(LU,401) I
        CALL PLANTE(1)
        STOP
!
      ELSE
!
!       IF(DEBUG.GT.0) WRITE(LU,*) I,INUM,OK1,N1,OK2,N2,OK3,N3,OK4,N4
!
        DO IDIR = 1,NDIR+1
!
          ZNUM = 0.D0
          ZDEN = 0.D0
          IF(OK1) THEN
            ZNUM = ZNUM + ZRELV(N1,IDIR)/DIST1
            ZDEN = ZDEN + 1.D0/DIST1
          ENDIF
          IF(OK2) THEN
            ZNUM = ZNUM + ZRELV(N2,IDIR)/DIST2
            ZDEN = ZDEN + 1.D0/DIST2
          ENDIF
          IF(OK3) THEN
            ZNUM = ZNUM + ZRELV(N3,IDIR)/DIST3
            ZDEN = ZDEN + 1.D0/DIST3
          ENDIF
          IF(OK4) THEN
            ZNUM = ZNUM + ZRELV(N4,IDIR)/DIST4
            ZDEN = ZDEN + 1.D0/DIST4
          ENDIF
!
          Z(IDIR)=ZNUM/ZDEN
!        IF(DEBUG.GT.0) WRITE(LU,*) I,IDIR,Z(IDIR)
!
        ENDDO ! IDIR = 1,500*NDALE
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     PRINTOUT FORMATS:
!
401   FORMAT(/,1X,'FASDPA_SP : NOT ENOUGH DATA TO INTERPOLATE ',
     &       'AT ARTEMIS BOUNDARY NODE : ',I7)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
