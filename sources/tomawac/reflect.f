      SUBROUTINE REFLECT(NDIRE, NF, NPTFR, NPOIN2, FBOR, F, KREFL)
!
!**********************************************************************
!
!brief calculate the solution after a reflection on boundary 
!      
!| F              |-->| DIRECTIONAL SPECTRUM
!| FBOR           |<--| DIRECTIONAL SPECTRUM ON THE BOUNDARY
!| KREFL          |<--| INTEGER INDICATES THAT IT IS A REFLECTION POINT
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPTFR          |-->| NUMBER OF POINTS ON THE BOUNDARY
!**********************************************************************
      
      USE DECLARATIONS_TOMAWAC, ONLY : LIFBOR, NBOR, MESH, TETA, DEUPI,
     &     COEREF
      USE INTERFACE_TOMAWAC, EX_REFLECT => REFLECT
      IMPLICIT NONE
      
      INTEGER, INTENT(IN)            :: NPTFR,NDIRE,NF, NPOIN2, KREFL
      DOUBLE PRECISION, INTENT(INOUT):: FBOR(NPTFR,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)   :: F(NPOIN2,NDIRE,NF)
      
!-----------------------------------------------------------------------
! DECLARES LOCAL VARIABLES 
!-----------------------------------------------------------------------
      INTEGER IPTFR, DIREP, DIREPP1, DIRE, FREQ
      DOUBLE PRECISION TETAP, E1,E2,DIRX,DIRY,XREF,YREF
      DOUBLE PRECISION TETA1, TETA2, COETETA
      DO IPTFR=1,NPTFR
        IF (LIFBOR(IPTFR).EQ.KREFL) THEN
          E1 = MESH%XNEBOR%R(IPTFR)
          E2 = MESH%YNEBOR%R(IPTFR)
          DO DIRE=1,NDIRE
            DO FREQ=1,NF
              FBOR(IPTFR,DIRE,FREQ)=0.D0
            ENDDO
          ENDDO
          DO DIRE=1,NDIRE
            DIRX=SIN(TETA(DIRE))
            DIRY=COS(TETA(DIRE))
            XREF=DIRX*(E2**2-E1**2)-2*DIRY*E1*E2
            YREF=DIRY*(E1**2-E2**2)-2*DIRX*E1*E2
            TETAP=DEUPI/4-ATAN2(YREF,XREF)
            IF (TETAP.LT.0.0D0) TETAP=TETAP+DEUPI
            IF (TETAP.GE.DEUPI) TETAP=TETAP-DEUPI
            DIREP=INT(NDIRE*TETAP/DEUPI)+1
            DIREPP1=DIREP+1
            IF(DIREP.EQ.NDIRE) DIREPP1=1
            TETA1 = TETA(DIREP)
            TETA2 = TETA(DIREPP1)
            COETETA = (TETA2 - TETAP)/(TETA2-TETA1)
            DO FREQ=1,NF
              FBOR(IPTFR,DIREP,FREQ)=FBOR(IPTFR,DIREP,FREQ)+
     &              COETETA*COEREF*F(NBOR(IPTFR),DIRE,FREQ)
              FBOR(IPTFR,DIREPP1,FREQ)=FBOR(IPTFR,DIREPP1,FREQ)+
     &             (1-COETETA)*COEREF*F(NBOR(IPTFR),DIRE,FREQ)
            ENDDO
          ENDDO
        ENDIF
      ENDDO
      RETURN
      END
