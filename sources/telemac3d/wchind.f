!                   *****************
                    SUBROUTINE WCHIND
!                   *****************
!
     &(WCHU,C,CINI,CGEL,NPOIN3,HIND_TYPE)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    MODELS HINDERED CONCENTRATION DEPENDENT SETTLING VELOCITY IN 3D
!+        ACCORDING TO EITHER 1. WHITEHOUSE ET AL. (2000):
!+
!+        WCHU = WCHU* (1 - C/CGEL)^5
!+
!+        OR 2. WINTERWERP (1999):
!+
!+        WCHU =
!+
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments in sediment merged on 25/02/2014.
!+
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| C              |-->| CONCENTRATION OF SED AT NODES
!|                |   | (ACTUALLY TRAV1 BIEF OBJECT WORK ARRAY)
!| HN             |-->| WATER DEPTH
!| NPOIN3         |-->| TOTAL NUMBER OF POINTS IN 3D MESH
!| WCHU           |<->| SEDIMENT SETTLING VELOCITY
!| CGEL           |<--| SEDIMENT CONCENTRATION AT WHICH SEDIMENT FORMS
!|                |   | A WEAK SOIL (KG/M3)
!| CINI           |-->| THRESHOLD CONCENTRATION FOR HINDERING TO START
!| HIND_TYPE      |-->| 1:WHITEHOUSE ET AL. (2000), 2:WINTERWERP (1999)
!|                |-->| (NOTE THAT OPTION 2 IS NOT WORKING YET)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: C
      INTEGER, INTENT(IN)           :: HIND_TYPE, NPOIN3
      DOUBLE PRECISION, INTENT(INOUT) :: WCHU(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)  :: CINI, CGEL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          :: I
      DOUBLE PRECISION :: QRWC,CORR,PHI,PHI_P,PHI_STAR
!
!-----------------------------------------------------------------------
!
      DO I=1,NPOIN3
!
!       ONLY APPLY HINDERING IF ABOVE THRESHOLD
!
        IF(C%R(I).GT.CINI) THEN
          IF(HIND_TYPE.EQ.1) THEN
!           WHITEHOUSE ET AL. (2000) HINDERED SETTLING (HIND_TYPE=1)
!           MODIFIED BY TBE TO ACCOUNT FOR TURBULENT SETTLING VELOCITY
!           MODEL (BECAUSE WCINI VARIES)
            PHI = (C%R(I)-CINI) / CGEL
            QRWC = 1.D0-PHI
!           N.B. WC IS POSITIVE
            WCHU(I) = MAX(WCHU(I)*QRWC**5,0.D0)
          ELSEIF(HIND_TYPE.EQ.2) THEN
            WRITE(LU,*) 'WINTERWERP HINDRED SETTLING DOES NOT WORK YET'
            CALL PLANTE(1)
            STOP
!           WINTERWERP (1999) HINDERED SETTLING (HIND_TYPE=2)
            PHI      = C%R(I)/CGEL
            PHI_P    = C%R(I)/2650.D0
            PHI_STAR = MIN(1.D0,PHI)
!           CV 0< CORRECTION <1
            CORR=(1.D0-PHI_STAR)*(1.D0-PHI_P)/(1.D0+2.5D0*PHI)
            CORR= MIN(MAX(CORR, 0.D0),1.D0)
            WCHU(I)  = WCHU(I)*CORR
          ELSE
            WRITE(LU,*) 'HINDERED SETTLING FORMULA MUST BE 1 OR 2'
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

