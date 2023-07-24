!                   *****************
                    SUBROUTINE VERMOY
!                   *****************
!
     & (FINT1,FINT2,F1,F2,NFONC,Z,TRA01,TRA02,TRA03,
     &  IPLAN1,IPLAN2,NPOIN2,NPLAN,OPTBAN)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE AVERAGE OF A 3D VARIABLE ON THE VERTICAL.
!
!history  F LEPEINTRE (LNH)
!+        25/11/97
!+        V5P3
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F1             |-->| VARIABLES TO AVERAGE
!| F2             |-->| VARIABLES TO AVERAGE
!| FINT1          |<->| AVERAGED VARIABLES
!| FINT2          |<->| AVERAGED VARIABLES
!| IPLAN1         |-->| PLAN IN WHICH INTEGRATION IS DONE
!| IPLAN2         |-->| PLAN IN WHICH INTEGRATION IS DONE
!| NFONC          |-->| VARIABLES NUMBER
!| NPLAN          |-->| NUMBER OF HORIZONTAL PLANES
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| OPTBAN         |-->| OPTION FOR TIDAL FLATS, IF 1, FREE SURFACE
!|                |   | MODIFIED AND PIECE-WISE LINEAR
!| TRA01          |<->| WORK ARRAY
!| TRA02          |<->| WORK ARRAY
!| TRA03          |<->| WORK ARRAY
!| Z              |-->| ELEVATION OF 3D MESH NODES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN2,NPLAN,NFONC,IPLAN1,IPLAN2,OPTBAN
      DOUBLE PRECISION, INTENT(INOUT) :: FINT1(NPOIN2),FINT2(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: F1(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN)    :: F2(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN)    :: Z(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA02(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA03(NPOIN2,NPLAN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN2,IP,I
      DOUBLE PRECISION AUX
!
!***********************************************************************
!
! CHECKS THE PLANE NUMBERS IPLAN1 AND IPLAN2
!
      IF( IPLAN2.LE.IPLAN1 .OR.
     &    IPLAN1.LT.1      .OR. IPLAN1.GT.NPLAN .OR.
     &    IPLAN2.LT.1      .OR. IPLAN2.GT.NPLAN     ) THEN
        WRITE(LU,12) IPLAN1,IPLAN2
        CALL PLANTE(1)
        STOP
      ENDIF
!
! CHECKS NFONC
!
      IF(NFONC.NE.1.AND.NFONC.NE.2) THEN
        WRITE(LU,14) NFONC
        CALL PLANTE(1)
        STOP
      ENDIF
!
! CHECKS OPTBAN
!
      IF(OPTBAN.NE.0.AND.OPTBAN.NE.1.AND.OPTBAN.NE.2) THEN
        WRITE(LU,16) OPTBAN
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
! COMPUTES FINT = SUM OF 2*F*DZ FROM IPLAN1 TO IPLAN2
! THE INTEGRAL IS COMPUTED BY THE TRAPEZOID RULE
!
      IPOIN2 = NPOIN2*(IPLAN2-IPLAN1)
!
      IF(NFONC.GE.1) THEN
      CALL OV('X=Y+Z   ', X=TRA01, Y=F1(1,IPLAN1+1), Z=F1(1,IPLAN1),
     &        DIM1=IPOIN2)
      ENDIF
      IF(NFONC.EQ.2) THEN
      CALL OV('X=Y+Z   ', X=TRA02, Y=F2(1,IPLAN1+1), Z=F2(1,IPLAN1),
     &        DIM1=IPOIN2)
      ENDIF
      CALL OV('X=Y-Z   ', X=TRA03, Y=Z(1,IPLAN1+1), Z=Z(1,IPLAN1),
     &        DIM1=IPOIN2)
!
      IF(NFONC.GE.1) CALL OV('X=YZ    ', X=FINT1, Y=TRA01, Z=TRA03,
     &                       DIM1=NPOIN2)
      IF(NFONC.EQ.2) CALL OV('X=YZ    ', X=FINT2, Y=TRA02, Z=TRA03,
     &                       DIM1=NPOIN2)
!
      IF(IPLAN2-IPLAN1.GE.2) THEN
        IF(NFONC.GE.1) THEN
        DO IP = 2,IPLAN2-IPLAN1
          CALL OV('X=X+YZ  ', X=FINT1, Y=TRA01(1,IP), Z=TRA03(1,IP),
     &            DIM1=NPOIN2)
        END DO
        ENDIF
        IF(NFONC.EQ.2) THEN
        DO IP = 2,IPLAN2-IPLAN1
          CALL OV('X=X+YZ  ', X=FINT2, Y=TRA02(1,IP), Z=TRA03(1,IP),
     &            DIM1=NPOIN2)
        END DO
        ENDIF
      ENDIF
!
!     COMPUTES THE ELEVATION
!
      CALL OV('X=Y-Z   ', X=TRA03, Y=Z(1,IPLAN2), Z=Z(1,IPLAN1),
     &        DIM1=NPOIN2)
!
      IF(OPTBAN.EQ.0.OR.OPTBAN.EQ.2) THEN
!
!       DIVIDES BY 2 H (OR 0 IF H=0)
!
        IF(NFONC.GE.1) THEN
          CALL OVD('X=CY/Z  ',FINT1,FINT1,TRA03,0.5D0,NPOIN2,
     &                        2,0.D0,1.D-8)
        ENDIF
        IF(NFONC.EQ.2) THEN
          CALL OVD('X=CY/Z  ',FINT2,FINT2,TRA03,0.5D0,NPOIN2,
     &                        2,0.D0,1.D-8)
        ENDIF
!
      ELSEIF(OPTBAN.EQ.1) THEN
!
        AUX=1.D0/FLOAT(IPLAN2-IPLAN1+1)
!
!       DIVIDES BY 2 H OR ARITHMETIC MEAN IF THERE IS NO WATER
!
        IF(NFONC.GE.1) THEN
          DO I=1,NPOIN2
            IF(TRA03(I,1).GT.1.D-4) THEN
              FINT1(I)=FINT1(I)*0.5D0/TRA03(I,1)
            ELSE
              FINT1(I)=0.D0
              DO IP=IPLAN1,IPLAN2
                FINT1(I)=FINT1(I)+F1(I,IP)
              ENDDO
              FINT1(I)=FINT1(I)*AUX
            ENDIF
          ENDDO
        ENDIF
        IF(NFONC.EQ.2) THEN
          DO I=1,NPOIN2
            IF(TRA03(I,1).GT.1.D-4) THEN
              FINT2(I)=FINT2(I)*0.5D0/TRA03(I,1)
            ELSE
              FINT2(I)=0.D0
              DO IP=IPLAN1,IPLAN2
                FINT2(I)=FINT2(I)+F2(I,IP)
              ENDDO
              FINT2(I)=FINT2(I)*AUX
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
12    FORMAT('VERMOY: ERROR ON THE DATA IPLAN1 AND IPLAN2',2I3)
14    FORMAT('VERMOY: ERROR ON ARGUMENT NFONC',1I6)
16    FORMAT('VERMOY: ERROR ON ARGUMENT OPTBAN: ',1I6)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
