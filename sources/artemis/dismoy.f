!                   *****************
                    SUBROUTINE DISMOY
!                   *****************
!
     &(NPOIN,NELEM,X,Y,IKLE,K,LISHHO)
!
!***********************************************************************
! ARTEMIS   V7P3                                     Aug 2017
!***********************************************************************
!
!brief    CALCULATES THE NUMBER OF SMOOTHINGS ON THE WAVE
!+                HEIGHT (LISHHO), A PRIORI NECESSARY TO FILTER OUT
!+                THE PARASITIC OSCILLATIONS (REGULAR WAVES).
!+                ESTIMATED FROM THE AVERAGE DISTANCE BETWEEN
!+                NODES AND THE AVERAGE NUMBER OF NODES IN HALF A
!+                WAVELENGTH.
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH) ; P. THELLIER (LNH)
!+        04/06/1999
!+        V5P1
!+   THANK YOU PAUL
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
!history  N.DURAND (HRW)
!+        August 2017
!+        V7P3
!+   PI now defined in DECLARATIONS_ARTEMIS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IKLE           |---| CONNECTIVITY TABLE
!| K              |-->| WAVE NUMBER
!| LISHHO         |<--| NUMBER OF SMOOTHING FOR HHO
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINT
!| X,Y            |-->| MESH COORDINATES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_ARTEMIS, EX_DISMOY => DISMOY
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_ARTEMIS, ONLY : PI
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN,NELEM
      INTEGER, INTENT(INOUT) :: LISHHO
      INTEGER, INTENT(IN) :: IKLE(NELEM,*)
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN),Y(NPOIN),K(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! VARIABLES FOR THE COMPUTATION OF AVERAGE DISTANCES
!
      INTEGER NG, ISOM, IELEM
      DOUBLE PRECISION SOM, D1, D2, DMOY, SOMD
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE AVERAGE DISTANCE BETWEEN A NODE AND ITS NEIGHBOURS
!     -----------------------------------------------------------
!
      SOMD = 0.D0
      DO NG=1,NPOIN
        ISOM = 0
        SOM = 0.D0
!
        DO IELEM=1,NELEM
!
          IF (IKLE(IELEM,1).EQ.NG) THEN
!         --                       ---
!
!           PT 1 IS COMMON, COMPUTES THE DISTANCE TO PTS 2 AND 3
            D1 = SQRT ( (X(NG)-X(IKLE(IELEM,2))) **2.D0 +
     &                  (Y(NG)-Y(IKLE(IELEM,2))) **2.D0 )
            D2 = SQRT ( (X(NG)-X(IKLE(IELEM,3))) **2.D0 +
     &                  (Y(NG)-Y(IKLE(IELEM,3))) **2.D0 )
            SOM = SOM + D1 + D2
            ISOM = ISOM + 2
!
          ELSEIF (IKLE(IELEM,2).EQ.NG) THEN
!         ------                       ----
!
!           PT 2 IS COMMON, COMPUTES THE DISTANCE TO PTS 1 AND 3
            D1 = SQRT ( (X(NG)-X(IKLE(IELEM,1))) **2.D0 +
     &                  (Y(NG)-Y(IKLE(IELEM,1))) **2.D0 )
            D2 = SQRT ( (X(NG)-X(IKLE(IELEM,3))) **2.D0 +
     &                  (Y(NG)-Y(IKLE(IELEM,3))) **2.D0 )
            SOM = SOM + D1 + D2
            ISOM = ISOM + 2
!
          ELSEIF (IKLE(IELEM,3).EQ.NG) THEN
!         ------                       ----
!
!           PT 3 IS COMMON, COMPUTES THE DISTANCE TO PTS 1 AND 2
            D1 = SQRT ( (X(NG)-X(IKLE(IELEM,1))) **2.D0 +
     &                  (Y(NG)-Y(IKLE(IELEM,1))) **2.D0 )
            D2 = SQRT ( (X(NG)-X(IKLE(IELEM,2))) **2.D0 +
     &                  (Y(NG)-Y(IKLE(IELEM,2))) **2.D0 )
            SOM = SOM + D1 + D2
            ISOM = ISOM + 2
!
          ENDIF
!         -----
!
        ENDDO ! IELEM
!
        DMOY = SOM / FLOAT(ISOM)
        SOMD = SOMD + (PI / (K(NG)*DMOY))
!
      ENDDO ! NG
!
!     ESTIMATES THE NUMBER OF SMOOTHINGS FROM THE AVERAGE DISTANCE
!     -------------------------------------------------------------------
!
      LISHHO = INT((SOMD/FLOAT(NPOIN))) * 10
!
      RETURN
      END
