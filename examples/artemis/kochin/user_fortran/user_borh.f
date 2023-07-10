!                   ********************
                    SUBROUTINE USER_BORH
!                   ********************
!
!
!***********************************************************************
! ARTEMIS
!***********************************************************************
!
!brief    TAKES INTO ACCOUNT USER-SPECIFIED BOUNDARY CONDITIONS.
!+        THEY ARE GIVEN BY SEGMENT.
!
!history  J-M HERVOUET (LNH)
!+
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION PHI_RE, PHI_IM
      DOUBLE PRECISION DDXPHI_RE, DDYPHI_RE, DDXPHI_IM, DDYPHI_IM
!
      INTEGER I, IG, JB
!
!-----------------------------------------------------------------------
!
! CONDITIONS AUX LIMITES
! UN SEGMENT EST SOLIDE SI IL EST DE TYPE KLOG.
! UN SEGMENT EST ONDE INCIDENTE SI IL EST DE TYPE KINC.
! UN SEGMENT EST UNE ENTREE SI IL EST DE TYPE KENT.
! UN SEGMENT EST UNE SORTIE SI IL EST DE TYPE KSORT.
!
! TOUS LES ANGLES SONT EN DEGRES
!                         ------
! ---------------------------------------
! INITIALISATION DES VARIABLES PAR DEFAUT
! ---------------------------------------
      TETAB%R(:) = 0.D0
      TETAP%R(:) = 0.D0
      ALFAP%R(:) = 0.D0
      RP%R(:)    = 0.D0
      HB%R(:)    = 0.D0

      PRB%R(:)   =0.D0
      PIB%R(:)   =0.D0
      DDXPRB%R(:)=0.D0
      DDYPRB%R(:)=0.D0
      DDXPIB%R(:)=0.D0
      DDYPIB%R(:)=0.D0

      DO I=1,NPTFR
        JB=BOUNDARY_COLOUR%I(I)

!       SORTIE
        IF(JB.GE.1.AND.JB.LE.72) THEN
          LIHBOR%I(I) = KSORT
          TETAP%R(I) = 0.D0
        ENDIF

!       Potentiel incident
        IF(JB.GE.73.AND.JB.LE.144) THEN
          LIHBOR%I(I) = KPOT
          TETAP%R(I)  = 0.D0
          IG   = MESH%NBOR%I(I)

          CALL FAR_FIELD_POTENTIAL
     &  ( X(IG)        , Y(IG)         , K%R(IG) , PHI_RE   , PHI_IM,
     &   DDXPHI_RE , DDYPHI_RE , DDXPHI_IM , DDYPHI_IM)
          PRB%R(I)   = PHI_RE
          PIB%R(I)   = PHI_IM
          DDXPRB%R(I)= DDXPHI_RE
          DDYPRB%R(I)= DDYPHI_RE
          DDXPIB%R(I)= DDXPHI_IM
          DDYPIB%R(I)= DDYPHI_IM
        ENDIF

      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
