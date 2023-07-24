!                   *******************
                    SUBROUTINE CALTETAP
!                   *******************
     &(TETA,XSGBOR,YSGBOR,ADIR,NPTFR)
!
!
!***********************************************************************
! ARTEMIS   V7P3                                     Aug 2017
!***********************************************************************
!
!brief    COMPUTES ANGLE TETAP ON THE BOUNDARY FROM THE WAVE INCIDENCE
!+        ON THE DOMAIN
!         TETAP is given in degrees, in the interval [0 ; 90]
!
!history  C. PEYRARD (LNHE)
!+        18/03/2014
!+        V7P0
!+        NEW SUBROUTINE CREATED / IMPLEMENTED
!
!history  N.DURAND (HRW)
!+        August 2017
!+        V7P3
!+   RADDEG now defined in DECLARATIONS_ARTEMIS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| TETA           |-->| ANGLE BETWEEN WAVE DIRECTION AND BOUNDARY NORMAL
!                       (TETAP)
!| XSGBOR         |-->| X COMPONENT OF THE NORMAL TO THE BOUNDARY SEGMENT
!| YSGBOR         |-->| Y COMPONENT OF THE NORMAL TO THE BOUNDARY SEGMENT
!| ADIR           |-->| INCIDENCE ANGLE OF WAVES
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_ARTEMIS, ONLY : RADDEG
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPTFR
      DOUBLE PRECISION, INTENT(INOUT) :: TETA(NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: ADIR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)  :: XSGBOR(NPTFR,4),YSGBOR(NPTFR,4)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
      DOUBLE PRECISION PSCALA,XNA,YNA,XI,YI,PSCALB,XNB,YNB
!
      INTRINSIC COS, SIN
!
!-----------------------------------------------------------------------
!
      DO I=1,NPTFR
!
!       STEP 1 : VECTOR NORMAL TO "THE POINT" (SEGMENT AFTER/BEFORE)
!       ======================================================
!       NORMAL TO SEGMENT AFTER
        XNA=XSGBOR(I,1)
        YNA=YSGBOR(I,1)
!       NORMAL TO SEGMENT BEFORE
        XNB=XSGBOR(I,2)
        YNB=YSGBOR(I,2)
!       NORMALIZATION NOT NECESSARY AS (XSGBOR(K,1),YSGBOR(K,1)) AND
!       (XSGBOR(K,2),YSGBOR(K,2)) ARE ALREADY OF NORM 1
!
!       STEP 2 : VECTOR INCIDENCE AT NODE
!       =========================
        XI=COS(ADIR(I))
        YI=SIN(ADIR(I))
!
!       STEP 3 : ANGLE BETWEEN NORMAL DIRECTION AND INCIDENCE DIRECTION
!       ===============================================================
        PSCALA=XNA*XI+YNA*YI
        PSCALB=XNB*XI+YNB*YI
!       SMALLEST ANGLE BETWEEN NORMAL AND INCIDENCE (0<TETAP<90)
        IF(PSCALA.LT.0D0) THEN
          PSCALA=-PSCALA
        ENDIF
        IF(PSCALB.LT.0D0) THEN
          PSCALB=-PSCALB
        ENDIF
!
!       STEP 4 = TETAP IS GIVEN IN DEGRES
!       =================================
!       CHOOSE OF THE LOWER VALUE OF TETA (SEEMS BETTER AT CORNER CONSIDERING
!       THE PHBOR ROUTINE RULES)
        TETA(I)=MIN(ACOS(PSCALA)*RADDEG,ACOS(PSCALB)*RADDEG)
      ENDDO
!=======================================================================
!
      RETURN
      END
