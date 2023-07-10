!                   **********************
                    SUBROUTINE USER_CORFON
!                   **********************
!
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!
!warning  USER SUBROUTINE
!
!history  Y AUDOUIN (LNHE)
!+        20/09/2018
!+        V8P0
!+
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IM,JM,I,J,POS_LOC
      DOUBLE PRECISION EIKON
!
!-----------------------------------------------------------------------
!
      IM = 47
      JM = 10
!
!  VARIANTE FOND EN PENTE RECTILIGNE + CUVETTE
!
      DO I=1,IM
        DO J=1,JM
!         PENTE RECTILIGNE
          POS_LOC = GLOBAL_TO_LOCAL_POINT(I+(J-1)*IM,MESH)
!
!         TODO: THIS IS VERY HEAVY, THERE SHOULD BE A
!               FORMULA FUNCTION OF X.
!
          IF(POS_LOC.GT.0) THEN
            ZF%R(POS_LOC)=-0.6D0+0.46D0*FLOAT(I-1)/FLOAT(IM-1)
!           BOSSE GAUSSIENNE
            IF(I.GT.9.AND.I.LT.29) THEN
              EIKON = -(I-19)**2/20.D0
              ZF%R(POS_LOC) = ZF%R(POS_LOC) + 0.1D0*EXP(EIKON)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      RETURN
      END
