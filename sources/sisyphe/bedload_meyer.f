!                   ************************
                    SUBROUTINE BEDLOAD_MEYER
!                   ************************
!
     &(TETAP,HIDING,HIDFAC,DENS,GRAV,DM,AC,ACP,QSC,SLOPEFF,COEFPN)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/07/2011
!***********************************************************************
!
!brief    MEYER-PETER BEDLOAD TRANSPORT FORMULATION.
!
!history  E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!+        11/09/1995
!+        V5P1
!+
!
!history  C.VILLARET
!+        **/10/2003
!+        V5P4
!+
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
!!history  U.MERKEL R.KOPMAN
!+        15/03/2011
!+        V6P1
!+
!
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AC             |<->| CRITICAL SHIELDS PARAMETER
!| ACP            |<->| MODIFIED SHIELDS PARAMETER
!| COEFPN         |<->| CORRECTION OF TRANSORT FOR SLOPING BED EFFECT
!| DENS           |-->| RELATIVE DENSITY
!| DM             |-->| SEDIMENT GRAIN DIAMETER
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| HIDFAC         |-->| HIDING FACTOR FORMULAS
!| HIDING         |-->| HIDING FACTOR CORRECTION
!| QSC            |<->| BED LOAD TRANSPORT
!| SLOPEFF        |-->| LOGICAL, SLOPING BED EFFECT OR NOT
!| TETAP          |-->| ADIMENSIONAL SKIN FRICTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_SISYPHE, EX_BEDLOAD_MEYER => BEDLOAD_MEYER
      USE DECLARATIONS_SISYPHE, ONLY : MPM_ARAY
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TETAP, HIDING
      INTEGER,          INTENT(IN)    :: HIDFAC, SLOPEFF
      DOUBLE PRECISION, INTENT(IN)    :: DENS, GRAV, DM, AC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ACP ! WORK ARRAY T1
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC, COEFPN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION :: C2
!
!======================================================================!
!                               PROGRAM                                !
!=======================================================================
!
      CALL CPSTVC(QSC,ACP)
      CALL OS('X=C     ', X=ACP, C=AC)
!
!     SLOPE EFFECT: SOULBY FORMULATION
!
      IF(SLOPEFF.EQ.2) THEN
        CALL OS('X=XY    ', X=ACP, Y=COEFPN )
      ENDIF
!
!     BEDLOAD TRANSPORT CORRECTED FOR EXTENDED GRAIN SIZE
!     WITH VARIABLE MPM_COEFFICIENT
!
      C2 = SQRT(GRAV*DENS*DM**3)
!
      IF(HIDFAC.EQ.1.OR.HIDFAC.EQ.2) THEN
!       CALL OS('X=XY    ', X=ACP, Y=HIDING)
!       CALL OS('X=Y-Z   ', X=QSC, Y=TETAP, Z=ACP)
!       CALL OS('X=+(Y,C)', X=QSC, Y=QSC , C=0.D0)
!       CALL OS('X=Y**C  ', X=QSC, Y=QSC , C=1.5D0)
!       CALL OS('X=CX    ', X=QSC, C=C2)
!       CALL OS('X=XY    ', X=QSC, Y=MPM_ARAY)
        DO I=1,QSC%DIM1
          QSC%R(I)=C2*MPM_ARAY%R(I)
     &               *SQRT(MAX(TETAP%R(I)-ACP%R(I)*HIDING%R(I),0.D0))**3
        ENDDO
      ELSE
!       CALL OS('X=Y-Z   ', X=QSC, Y=TETAP, Z=ACP)
!       CALL OS('X=+(Y,C)', X=QSC, Y=QSC, C=0.D0)
!       CALL OS('X=Y**C  ', X=QSC, Y=QSC, C=1.5D0)
!       CALL OS('X=CX    ', X=QSC, C=C2)
!       CALL OS('X=XY    ', X=QSC, Y=HIDING)
!       CALL OS('X=XY    ', X=QSC, Y=MPM_ARAY)
        DO I=1,QSC%DIM1
          QSC%R(I)=C2*MPM_ARAY%R(I)*HIDING%R(I)*SQRT(
     &                                 MAX(TETAP%R(I)-ACP%R(I),0.D0))**3
        ENDDO
      ENDIF
!
!=======================================================================
!
      RETURN
      END
