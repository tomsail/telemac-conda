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
      INTEGER I,JB
!
!-----------------------------------------------------------------------
!
! RP     COEFFICIENTS DE REFLEXION DES PAROIS
!
! TETAP  ANGLE D'ATTAQUE DE LA HOULE SUR LES LIMITES
!        PAS SEULEMENT LES PAROIS, MAIS AUSSI LES
!        LES FRONTIERES LIQUIDES
!        (COMPTE PAR RAPPORT A LA NORMALE EXTERIEURE
!         DANS LE SENS DIRECT)
!
! ALFAP  DEPHASAGE INDUIT PAR LA PAROI ENTRE L'ONDE
!        REFLECHIE ET L'ONDE INCIDENTE (SI ALFAP EST
!        POSITIF, L'ONDE REFLECHIE EST EN RETARD)
!
! HB     HAUTEUR DE LA HOULE AUX FRONTIERES OUVERTES
!
! TETAB  ANGLE D'ATTAQUE DE LA HOULE (FRONT. OUV.)
!        (COMPTE PAR RAPPORT A L'AXE DES X DANS LE
!         SENS DIRECT)
!
! TETAH  ANGLE DE PROPAGATION DE LA HOULE
!
! NPTFR  NOMBRE DE POINTS FRONTIERE.
! INITIALIZATION OF VARIABLES TO DEFAULT VALUE
! ---------------------------------------
      TETAB%R(:) = TETAH
      HB%R(:)    = 0.D0
      TETAP%R(:) = 0.D0
      ALFAP%R(:) = 0.D0
      RP%R(:)    = 0.D0
!
      DO I=1,NPTFR
        JB=BOUNDARY_COLOUR%I(I)
!
!**************************************************
! Boundary conditions on solid boundaries
!**************************************************
! limite Nord du modele (h<5m): paroi absorbante
        IF(JB.GE.570.AND.JB.LE.641)THEN
          RP%R(I) = 0.0D0
          TETAP%R(I) = 0.D0
          ALFAP%R(I) = 0.D0
        ENDIF
!
!   plage au Nord Ouest
        IF(JB.GE.642.AND.JB.LE.703)THEN
          RP%R(I)     = 0.05D0
          TETAP%R(I)  = 0.D0
          ALFAP%R(I)  = 0.D0
        ENDIF
!
!   enrochements perpendiculaires a la plage
        IF(JB.GE.704.AND.JB.LE.784)THEN
          RP%R(I) = 0.15D0
          TETAP%R(I) = 45.D0
          ALFAP%R(I) = 0.D0
        ENDIF
!
!   plage et cote basse
        IF(JB.GE.785.AND.JB.LE.947)THEN
          RP%R(I) = 0.05D0
          TETAP%R(I) = 0.D0
          ALFAP%R(I) = 0.D0
        ENDIF
!
! bassins du port de Borme et capitainerie (ile)
        IF(JB.GE.948.AND.JB.LE.1029)THEN
          RP%R(I) = 1.D0
          TETAP%R(I) = 0.D0
          ALFAP%R(I) = 0.D0
        ENDIF
        IF(JB.GE.1.AND.JB.LE.267)THEN
          RP%R(I) = 1.D0
          TETAP%R(I) = 0.D0
          ALFAP%R(I) = 0.D0
        ENDIF
!
!   musoir et digue du port en enrochements
        IF(JB.GE.268.AND.JB.LE.331)THEN
          RP%R(I) = 0.15D0
          TETAP%R(I) = 0.D0
          ALFAP%R(I) = 0.D0
        ENDIF
!
!**************************************************
! Boundary condition on liquid boundaries
!**************************************************
! limite sud: Onde Incidente
        IF(JB.GE.332.AND.JB.LE.406)THEN
          HB%R(I) = 2.D0
          TETAB%R(I) = 180.D0
          TETAP%R(I) = 63.D0
        ENDIF
!
! limite Est : Onde Incidente
        IF(JB.GE.407.AND.JB.LE.497)THEN
          HB%R(I) = 2.D0
          TETAB%R(I) = 180.D0
          TETAP%R(I) = 0.D0
        ENDIF
!
! limite nord: Onde incidente tant que Prof>5m
        IF(JB.GE.498.AND.JB.LE.569)THEN
          HB%R(I) = 2.D0
          TETAB%R(I) = 180.D0
          TETAP%R(I) = 73.D0
        ENDIF

      ENDDO !I=1,NPTFR
!
!-----------------------------------------------------------------------
!
      RETURN
      END
