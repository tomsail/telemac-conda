!                       *****************
                        SUBROUTINE LIMITE
!                       *****************
     &( F     , FREQ  , NPOIN2, NDIRE , NF    )
!
!***********************************************************************
! TOMAWAC   V7P0                                 30/07/2014
!***********************************************************************
!
!brief     EQUILIBRIUM RANGE SPECTRUM OF PHILLIPS APPLIED AS AN UPPER
!+         LIMIT FOR THE SPECTRUM : E(F)=ALFA*G**2/(2.PI)**4 F**(-5)
!
!
!history  E. GAGNAIRE-RENOU AND M.BENOIT (EDF/LNHE)
!+        09/2014
!+        V7P0
!+        NEW SUBROUTINE CREATED / IMPLEMENTED
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!| F              |-->| DIRECTIONAL SPECTRUM
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI,GRAVIT
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TOMAWAC, EX_LIMITE => LIMITE
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NF,NDIRE,NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: FREQ(NF)
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN2,NDIRE,NF)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          IP    , JP    , JF
      DOUBLE PRECISION EDEF  , COEF  , DTETAR, EMAX  , REDUC
!
!-----------------------------------------------------------------------
!
      DTETAR=DEUPI/DBLE(NDIRE)
      COEF=0.0081D0*GRAVIT**2/DEUPI**4
!
      DO IP=1, NPOIN2
        DO JF=1, NF
          EDEF=0.0D0
          DO JP=1, NDIRE
            EDEF=EDEF+F(IP,JP,JF)
          ENDDO
          EDEF=EDEF*DTETAR
          EMAX=COEF/FREQ(JF)**5
          IF (EDEF.GT.EMAX) THEN
            REDUC=EMAX/EDEF
            DO JP=1, NDIRE
              F(IP,JP,JF)=F(IP,JP,JF)*REDUC
            ENDDO
          ENDIF
        ENDDO
      ENDDO
!
      RETURN
      END
