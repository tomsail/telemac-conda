!                       ******************************
                        DOUBLE PRECISION FUNCTION FUNC
!                       ******************************
!
     &(X)
!
!***********************************************************************
! PROGICIEL : MITHRIDATE     01/06/90    PAINTER (LNH) 30 87 78 54
!***********************************************************************
!
!     FONCTION:
!     =========
!
!     FONCTION DONT LE ZERO CORRESPOND A LA HAUTEUR EN X=0
!     QUAND LA SURFACE LIBRE EQUILIBRE LA FORCE DUE AU VENT
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |                |    |                                              |
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!----------------------------------------------------------------------
!
      USE DECLARATIONS_TELEMAC2D, ONLY: GRAV
!
      DOUBLE PRECISION A1,A2,LCANAL,X,FVENT,HINI
!
      COMMON/FORFUN/FVENT,LCANAL,HINI
!
!----------------------------------------------------------------------
!
      A1 = 2.D0 * FVENT * LCANAL / GRAV
      A2 = 3.D0 * FVENT * HINI * LCANAL / GRAV
      FUNC = SQRT((A1+X*X)**3) - X**3 - A2
!
!-----------------------------------------------------------------------
!
      RETURN
      END
