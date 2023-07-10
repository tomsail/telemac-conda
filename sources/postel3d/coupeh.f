!                       *****************
                        SUBROUTINE COUPEH
!                       *****************
!
     &(IREC,AT,Z,U,V,W,HREF,NPLREF,PLINF,NC2DH,NPOIN2,NPLAN,NCOU,
     & FFORMAT,VAR,SHZ,NVA3,TAB,TEXTELU)
!
!***********************************************************************
! POSTEL3D VERSION 6.0   01/09/99   T. DENOT (LNH) 01 30 87 74 89
! FORTRAN90
!***********************************************************************
!
!     FONCTION  : ECRIT POUR CHAQUE COUPE HORIZONTALES LES VARIABLES
!                      D'UN PAS DE TEMPS
!
!     ATTENTION : LORSQUE LE PLAN DE COUPE SE SITUE EN DEHORS DU DOMAINE
!                 (EN DESSOUS DU FOND OU AU DESSUS DE LA SURFACE) :
!
!                 ON FIXE LES VITESSES HORIZONTALES A ZERO
!                 CE QUI EST BIEN ADAPTE POUR TRACER DES VECTEURS
!
!                 ON EXTRAPOLE LES AUTRES VARIABLES A PARTIR DE LEURS
!                 VALEURS AU PREMIER ETAGE SI EN DESSOUS DU FOND
!                         AU DERNIER ETAGE SI AU DESSUS DE LA SURFACE
!                 CE QUI EST BIEN ADAPTE POUR TRACER DES ISOCOURBES
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! !      NOM       !MODE!                   ROLE                       !
! !________________!____!______________________________________________!
! !   IREC         ! -->! PAS TRAITE                                   !
! !   AT           ! -->! TEMPS CORRESPONDANT AU PAS TRAITE            !
! !   Z            ! -->! COTES DES NOEUDS                             !
! !   U,V,W        ! -->! COMPOSANTES 3D DE LA VITESSE                 !
! !   TA,TP        ! -->! CONCENTRATIONS DES TRACEURS                  !
! !   NUX,NUY,NUZ  ! -->! COEFFICIENTS DE VISCOSITE POUR LES VITESSES  !
! !   NAX,NAY,NAZ  ! -->! COEFFICIENTS DE VISCOSITE POUR LES TR.ACTIFS !
! !   NPX,NPY,NPZ  ! -->! COEFFICIENTS DE VISCOSITE POUR LES TR.PASSIFS!
! !   RI           ! -->! NOMBRE DE RICHARDSON                         !
! !   AK,EP        ! -->! VARIABLES DU MODELE K-EPSILON                !
! !   RHO          ! -->! ECARTS RELATIFS DE DENSITE                   !
! !   VAR          ! -->! TABLEAU DE TRAVAIL POUR PROJETER LES VARIABLES
! !   SHZ          ! -->! COORDONNEE BARYCENTRIQUE SUIVANT Z           !
! !   HREF         ! -->! DECALAGE PAR RAPPORT AU PLAN DE REFERENCE    !
! !   NPLREF       ! -->! PLAN DE REFERENCE                            !
! !   PLINF        ! -->! PLAN SUITE IMMEDIATEMENT SOUS LA COUPE       !
! !   NC2DH        ! -->! NOMBRE DE COUPES HORIZONTALES                !
! !   NPOIN2       ! -->! NOMBRE DE POINTS DU MAILLAGE 2D              !
! !   NCOU         ! -->! NUMERO DE CANAL - 1 DE LA PREMIERE COUPE     !
! !   NPLAN        ! -->! NOMBRE DE PLANS                              !
! !   NTRAC        ! -->! NOMBRE DE TRACEURS ACTIFS                    !
! !   NTRPA        ! -->! NOMBRE DE TRACEURS PASSIFS                   !
! !   SORG3D       ! -->! INDICATEUR DES VARIABLES ENREGISTREES        !
! !________________!____!______________________________________________!
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELE PAR : POSTEL3D
! SOUS-PROGRAMME APPELES : ECRI2
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!**********************************************************************
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_POSTEL3D, ONLY: PREZ => Z
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NC2DH,NPOIN2,NPLAN
      INTEGER, INTENT(IN) :: NCOU(NC2DH)
      INTEGER, INTENT(IN) :: IREC
      DOUBLE PRECISION, INTENT(IN) :: U(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: V(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: W(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: Z(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: AT
      DOUBLE PRECISION, INTENT(IN) :: HREF(9)
      INTEGER , INTENT(IN) :: NPLREF(9)
      INTEGER , INTENT(INOUT) :: PLINF(NPOIN2)
      TYPE (BIEF_OBJ), INTENT(INOUT) :: TAB
      INTEGER, INTENT(IN) :: NVA3
      CHARACTER(LEN=8) :: FFORMAT
      DOUBLE PRECISION,INTENT(INOUT) :: VAR(NPOIN2),SHZ(NPOIN2)
      CHARACTER(LEN=32), INTENT(IN) :: TEXTELU(100)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IC,I,J,CANAL
!
      INTEGER IERR
      CHARACTER(LEN=32) :: VAR_NAME
!
!***********************************************************************
!
!    POUR CHAQUE COUPE HORIZONTALE FAIRE :
!
      DO IC = 1,NC2DH
!
        CANAL = NCOU(IC)
!
        DO I = 1,NPOIN2
          VAR(I) = HREF(IC)
!          IF (NPLREF(IC).GE.1) VAR(I) = VAR(I) + Z(I,NPLREF(IC))
          IF (NPLREF(IC).GE.1) THEN
            VAR(I) = VAR(I) + Z(I,NPLREF(IC))
          ENDIF
          PLINF(I) = 1
        ENDDO
!
        IF (NPLAN.GE.3) THEN
          DO J = 2,NPLAN-1
            DO I = 1,NPOIN2
              IF (Z(I,J).LE.VAR(I)) PLINF(I) = J
            ENDDO
          ENDDO
        ENDIF
!
!
        DO I = 1,NPOIN2
!..01/2004
!  ATTENTION : Cas des bancs decouvrants (plans confondus)
          SHZ(I) = (          VAR(I)   -Z(I,PLINF(I)))
     &           / MAX((Z(I,PLINF(I)+1)-Z(I,PLINF(I))),1.D-6)
!..01/2004
        ENDDO
!
!-----------------------------------------------------------------------
!
!    INDICATEUR DU DOMAINE
!    ---------------------
        DO I = 1,NPOIN2
          VAR(I) = MIN(SHZ(I),1.D0-SHZ(I)) + 1.D-6
        ENDDO
        IF (LNG.EQ.LNG_FR) VAR_NAME = 'INDICATEUR DOM.                 '
        IF (LNG.EQ.LNG_EN) VAR_NAME = 'DOMAIN INDICATOR                '
        CALL ADD_DATA(FFORMAT,CANAL,VAR_NAME,AT,IREC,.TRUE.,VAR,
     &                NPOIN2,IERR)
        CALL CHECK_CALL(IERR,'COUPEH:ADD_DATA:DOM')
!
!       Adding z
!
        DO I = 1,NPOIN2
          VAR(I) = 0.D0
          IF (SHZ(I).GT.-1.D-6.AND.SHZ(I).LT.1.000001D0)
     &       VAR(I) =
     &     PREZ((PLINF(I)-1)*NPOIN2+I)*(1.-SHZ(I))
     &   + PREZ( PLINF(I)   *NPOIN2+I)*    SHZ(I)
        ENDDO
        CALL ADD_DATA(FFORMAT,CANAL,TEXTELU(1),AT,IREC,.FALSE.,VAR,
     &                NPOIN2,IERR)
        CALL CHECK_CALL(IERR,'COUPEH:ADD_DATA:J')
!
!
!    COMPOSANTE U DE LA VITESSE
!    --------------------------
        DO I = 1,NPOIN2
          VAR(I) = 0.D0
          IF (SHZ(I).GT.-1.D-6.AND.SHZ(I).LT.1.000001D0)
     &    VAR(I) = U(I,PLINF(I))*(1.-SHZ(I))+U(I,PLINF(I)+1)*SHZ(I)
        ENDDO
        VAR_NAME = TEXTELU(2)
        CALL ADD_DATA(FFORMAT,CANAL,VAR_NAME,AT,IREC,.FALSE.,VAR,
     &                NPOIN2,IERR)
        CALL CHECK_CALL(IERR,'COUPEH:ADD_DATA:U')
!
!
!    COMPOSANTE V DE LA VITESSE
!    --------------------------
        DO I = 1,NPOIN2
          VAR(I) = 0.D0
          IF (SHZ(I).GT.-1.D-6.AND.SHZ(I).LT.1.000001D0)
     &    VAR(I) = V(I,PLINF(I))*(1.-SHZ(I))+V(I,PLINF(I)+1)*SHZ(I)
        ENDDO
        VAR_NAME = TEXTELU(3)
        CALL ADD_DATA(FFORMAT,CANAL,VAR_NAME,AT,IREC,.FALSE.,VAR,
     &                NPOIN2,IERR)
        CALL CHECK_CALL(IERR,'COUPEH:ADD_DATA:V')
!
!    COMPOSANTE W DE LA VITESSE
!    --------------------------
        DO I = 1,NPOIN2
          VAR(I) = W(I,PLINF(I))*(1.-SHZ(I))+W(I,PLINF(I)+1)*SHZ(I)
        ENDDO
        VAR_NAME = TEXTELU(4)
        CALL ADD_DATA(FFORMAT,CANAL,VAR_NAME,AT,IREC,.FALSE.,VAR,
     &                NPOIN2,IERR)
        CALL CHECK_CALL(IERR,'COUPEH:ADD_DATA:W')
!
!       Other variables
!
        IF (NVA3.GT.4) THEN
          DO J=5,NVA3
            DO I = 1,NPOIN2
              VAR(I) = 0.D0
              IF (SHZ(I).GT.-1.D-6.AND.SHZ(I).LT.1.000001D0)
     &           VAR(I) =
     &         TAB%ADR(J-4)%P%R((PLINF(I)-1)*NPOIN2+I)*(1.-SHZ(I))
     &       + TAB%ADR(J-4)%P%R( PLINF(I)   *NPOIN2+I)*    SHZ(I)
            ENDDO
            CALL ADD_DATA(FFORMAT,CANAL,TEXTELU(J),AT,IREC,.FALSE.,VAR,
     &                    NPOIN2,IERR)
            CALL CHECK_CALL(IERR,'COUPEH:ADD_DATA:J')
          ENDDO
        ENDIF
!
!
      ENDDO !IC
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
