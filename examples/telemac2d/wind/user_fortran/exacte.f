!                       *****************
                        SUBROUTINE EXACTE
!                       *****************
!
     &(FEXA,X,Y,NPOIN,ZF)
!
!***********************************************************************
! PROGICIEL : EX-PROGRAMME DE F. LEPEINTRE
!***********************************************************************
!
!     FONCTION:
!     =========
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |                |    |                                              |
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!**********************************************************************
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC2D, ONLY: GRAV
      IMPLICIT NONE
!
      INTEGER NPOIN,I
!
      DOUBLE PRECISION FEXA(NPOIN),LCANAL,HINI,H0,FVENT
      DOUBLE PRECISION X(NPOIN),Y(NPOIN),ZF(NPOIN)
!
      EXTERNAL FUNC
      INTRINSIC SQRT
      DOUBLE PRECISION FUNC
!
      COMMON/FORFUN/FVENT,LCANAL,HINI
!
!-----------------------------------------------------------------------
!
!     HAUTEUR D'EAU EN X=0 A L'EQUILIBRE (POUR LA SOLUTION ANALYTIQUE)
!
      H0 = HINI
      CALL ZBRENT(FUNC,1.D-6,0.D0,H0,100)
!
!     CALCUL DE LA SOLUTION EXACTE
!
      DO I = 1 , NPOIN
!
        FEXA(I) = SQRT( 2.D0*FVENT * X(I) / GRAV + H0*H0)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

