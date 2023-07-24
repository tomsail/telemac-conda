!                   ****************************
                    SUBROUTINE CVSP_ADD_FRACTION
!                   ****************************
!
     &(J, I, DZFCL)
!
!***********************************************************************
! SISYPHE   V6P3                                   12/03/2013
!***********************************************************************
!
!brief    ADDS A FRACTION TO THE TOPMOST VERTICAL SORTING PROFILE SECTION
!
!history  UWE MERKEL
!+        2011
!+        V6P2
!+
!
!history  P. A. TASSI (EDF R&D, LNHE)
!+        12/03/2013
!+        V6P3
!+   Cleaning, cosmetic
!
!
!history  R. KOPMANN (BAW)
!+        25/02/2019
!+        V7P2
!+  Considering a new case: only 1 fraction occurs, must have value 1
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| J              |<--| INDEX OF A POINT IN MESH
!| I              |<--| INDEX OF A FRACTION
!| DZFCL          |<--| EVOLUTION OF FRACTION I [M]
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SISYPHE
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN) :: J
      INTEGER,          INTENT(IN) :: I
      DOUBLE PRECISION, INTENT(IN) :: DZFCL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION STR_OLD, STR_NEW, TEMP1, TEMP2
      DOUBLE PRECISION SUMF, SUMF2
      INTEGER II
      LOGICAL RET, CVSP_CHECK_F
!
!-----------------------------------------------------------------------
!     MAKES SURE THAT THERE IS NO INFLUENCE ON THE PROFILE POINTS BELOW
!     BY INSERTING A SECTION WITH 0 STRENGTH IF IT DOESN'T EXIST ALREADY
!
!     CHECKS FOR BREAKPOINT (= 0 STRENGTH)
!-----------------------------------------------------------------------
!
      IF (PRO_MAX(J).GT.2) THEN
        IF (PRO_D(J,PRO_MAX(J)-1,1).GT.PRO_D(J,PRO_MAX(J)-2,1)) THEN
!
!-----------------------------------------------------------------------
!INSERT
!-----------------------------------------------------------------------
!
          PRO_MAX(J) = PRO_MAX(J) + 1
!
!-----------------------------------------------------------------------
!SHIFTS BREAKPOINT
!-----------------------------------------------------------------------
!
          DO II=1,NSICLA
            PRO_F(J,PRO_MAX(J),II) = PRO_F(J,PRO_MAX(J)-1,II)
            PRO_F(J,PRO_MAX(J)-1,II) = PRO_F(J,PRO_MAX(J)-2,II)
            PRO_D(J,PRO_MAX(J),II) = PRO_D(J,PRO_MAX(J)-1,II)
            PRO_D(J,PRO_MAX(J)-1,II) = PRO_D(J,PRO_MAX(J)-2,II)
          ENDDO

        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
! ADDS MATERIAL
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!STRENGTH OF FRACTION
!-----------------------------------------------------------------------
!
      STR_OLD = (PRO_D(J,PRO_MAX(J),I)-PRO_D(J,PRO_MAX(J)-1,I))
      STR_NEW = DZFCL + STR_OLD
!
!-----------------------------------------------------------------------
!NEW FRACTIONS
!TOP
!-----------------------------------------------------------------------
!
      PRO_F(J,PRO_MAX(J),I) =
     &     (DZFCL + PRO_F(J,PRO_MAX(J),I) * STR_OLD) / (STR_NEW)
!
!-----------------------------------------------------------------------
!BOTTOM
!-----------------------------------------------------------------------
!
      PRO_F(J,PRO_MAX(J)-1,I) =
     &     (DZFCL + PRO_F(J,PRO_MAX(J)-1,I) * STR_OLD) / (STR_NEW)
!
!-----------------------------------------------------------------------
!NEW DEPTH=Z OF FRACTION
!-----------------------------------------------------------------------
!
      PRO_D(J,PRO_MAX(J),I) = DZFCL + PRO_D(J,PRO_MAX(J),I)
!
!-----------------------------------------------------------------------
!SHIFTING PERCENTAGE FOR THE OTHER FRACTIONS
!-----------------------------------------------------------------------
!
      SUMF = 0.D0
      SUMF2 = 0.D0
      DO II=1,NSICLA
        IF (I /= II) THEN
!
!-----------------------------------------------------------------------
! SUM OF FRACTIONS AFTER SEDIMENTATION /= I
!-----------------------------------------------------------------------
!
          TEMP1 = PRO_F(J,PRO_MAX(J),II) * STR_OLD / STR_NEW
          TEMP2 = PRO_F(J,PRO_MAX(J)-1,II) * STR_OLD / STR_NEW
!
!-----------------------------------------------------------------------
! ASSIGN NEW THICKNESS & CORRECTED FRACTIONS
!-----------------------------------------------------------------------
!
          PRO_F(J,PRO_MAX(J),II) = TEMP1
          PRO_D(J,PRO_MAX(J),II) = DZFCL + PRO_D(J,PRO_MAX(J),II)
          PRO_F(J,PRO_MAX(J)-1,II) = TEMP2
          SUMF = SUMF + TEMP1
          SUMF2 = SUMF2 + TEMP2
        ENDIF
      ENDDO
      IF(SUMF.EQ.0.D0) THEN
        PRO_F(J,PRO_MAX(J),I) = 1.D0
      ENDIF

      IF(SUMF2.EQ.0.D0) THEN
        PRO_F(J,PRO_MAX(J)-1,I) = 1.D0
      ENDIF
!
!-----------------------------------------------------------------------
! REMOVES FLOATING POINT TRUCATIONS
!-----------------------------------------------------------------------
!
      RET = CVSP_CHECK_F(J,PRO_MAX(J),'ADF: MAX  ')
      RET = CVSP_CHECK_F(J,PRO_MAX(J)-1,'ADF: MAX+1')
      IF (PRO_MAX(J).GT.2) THEN
        RET =  CVSP_CHECK_F(J,PRO_MAX(J)-2,'ADF: MAX+2')
      ENDIF
      IF (PRO_MAX(J).GT.3) THEN
        RET = CVSP_CHECK_F(J,PRO_MAX(J)-3,'ADF: MAX+3')
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CVSP_ADD_FRACTION
