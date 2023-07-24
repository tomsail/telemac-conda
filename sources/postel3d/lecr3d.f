!                       *****************
                        SUBROUTINE LECR3D
!                       *****************
!
     &(IREC,AT,Z,U,V,W,NPOIN3,NPOIN2,NPLAN,NRES,FFORMAT,NVA3,TAB)
!
!***********************************************************************
! POSTEL3D VERSION 5.1   01/09/99   T. DENOT (LNH) 01 30 87 74 89
! FORTRAN90
!***********************************************************************
!
!     FONCTION  : LIT LES ENREGISTREMENTS 3D D'UN PAS DE TEMPS
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! !      NOM       !MODE!                   ROLE                       !
! !________________!____!______________________________________________!
! !   IREC         !--> ! PAS DE TEMPS TRAITE                          !
! !   AT           !<-- ! TEMPS CORRESPONDANT AU PAS TRAITE            !
! !   U,V,W        !<-- ! COMPOSANTES 3D DE LA VITESSE                 !
! !   TA,TP        !<-- ! CONCENTRATIONS DES TRACEURS                  !
! !   NUX,NUY,NUZ  !<-- ! COEFFICIENTS DE VISCOSITE POUR LES VITESSES  !
! !   NAX,NAY,NAZ  !<-- ! COEFFICIENTS DE VISCOSITE POUR LES TR.ACTIFS !
! !   NPX,NPY,NPZ  !<-- ! COEFFICIENTS DE VISCOSITE POUR LES TR.PASSIFS!
! !   RI           !<-- ! NOMBRE DE RICHARDSON                         !
! !   AK,EP        !<-- ! VARIABLES DU MODELE K-EPSILON                !
! !   RHO          !<-- ! ECARTS RELATIFS DE DENSITE                   !
! !   H            !<-- ! HAUTEUR D'EAU                                !
! !   Z            !<-- ! COTES DES NOEUDS                             !
! !   ZSTAR        ! -->! COTES RELATIVES DES NOEUDS                   !
! !   NPOIN2       ! -->! NOMBRE DE POINTS DU MAILLAGE 2D              !
! !   NPOIN3       ! -->! NOMBRE DE POINTS DU MAILLAGE 3D              !
! !   NRES         ! -->! NUMERO DE CANAL DU FICHIER DE RESULTAT 3D    !
! !   NPLAN        ! -->! NOMBRE DE PLANS                              !
! !   NPLINT       ! -->! NUMERO DU PLAN DE CHANGEMENT DE TRANSFORMATION
! !   NTRAC        ! -->! NOMBRE DE TRACEURS ACTIFS                    !
! !   NTRPA        ! -->! NOMBRE DE TRACEURS PASSIFS                   !
! !   SORG3D       ! -->! INDICATEUR DES VARIABLES ENREGISTREES        !
! !   R4           ! -->! TABLEAU DE REELS SIMPLE PRECISION POUR LIT   !
! !________________!____!______________________________________________!
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
! SOUS-PROGRAMME APPELE PAR : POSTEL3D
! SOUS-PROGRAMME APPELES : LIT
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!**********************************************************************
!
      USE BIEF
      USE DECLARATIONS_POSTEL3D, ONLY: TEXTLU
      USE INTERFACE_HERMES
      USE INTERFACE_POSTEL3D, EX_LECR3D => LECR3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: IREC
      INTEGER, INTENT(IN) :: NPOIN3,NPOIN2,NPLAN,NRES
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(INOUT) :: NVA3
      TYPE (BIEF_OBJ) , INTENT(INOUT) :: TAB
      DOUBLE PRECISION , INTENT(INOUT) :: AT
      DOUBLE PRECISION , INTENT(INOUT) :: U(NPOIN3)
      DOUBLE PRECISION , INTENT(INOUT) :: V(NPOIN3)
      DOUBLE PRECISION , INTENT(INOUT) :: W(NPOIN3)
      DOUBLE PRECISION , INTENT(INOUT) :: Z(NPOIN2,NPLAN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      INTEGER IERR
!
!***********************************************************************
!
!
! LECTURE DU TEMPS DU DEBUT DU CALCUL
!
      CALL GET_DATA_TIME(FFORMAT,NRES,IREC,AT,IERR)
      CALL CHECK_CALL(IERR,'LECR3D:GET_DATA_TIME')
!
! LECTURE DES VITESSES U,V ET W
!
      CALL GET_DATA_VALUE(FFORMAT,NRES,IREC,TEXTLU(1),Z,NPOIN3,IERR)
      CALL CHECK_CALL(IERR,'LECR3D:GET_DATA_VALUE:Z')
      CALL GET_DATA_VALUE(FFORMAT,NRES,IREC,TEXTLU(2),U,NPOIN3,IERR)
      CALL CHECK_CALL(IERR,'LECR3D:GET_DATA_VALUE:U')
      CALL GET_DATA_VALUE(FFORMAT,NRES,IREC,TEXTLU(3),V,NPOIN3,IERR)
      CALL CHECK_CALL(IERR,'LECR3D:GET_DATA_VALUE:V')
      CALL GET_DATA_VALUE(FFORMAT,NRES,IREC,TEXTLU(4),W,NPOIN3,IERR)
      CALL CHECK_CALL(IERR,'LECR3D:GET_DATA_VALUE:W')
!
      IF (NVA3.GT.4) THEN
        DO I=1,NVA3-4
          CALL GET_DATA_VALUE(FFORMAT,NRES,IREC,TEXTLU(I+4),
     &                        TAB%ADR(I)%P%R,NPOIN3,IERR)
          CALL CHECK_CALL(IERR,'LECR3D:GET_DATA_VALUE:W')
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
