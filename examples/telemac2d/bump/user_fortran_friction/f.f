!                       ***************************
                        DOUBLE PRECISION FUNCTION F
!                       ***************************
!
     &(Z,MAILLE,QFIXG,PRAD,ST)
!
!***********************************************************************
! PROGICIEL : TELEMAC        07/12/88    J-M HERVOUET (LNH) 30 71 80 18
!***********************************************************************
!
!  FONCTION  :
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   Z            | -->|
! |   MAILLE       | -->|
! |   QFIXG        | -->|
! |   PRAD         | -->|
! |   ST           | -->|
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC2D, ONLY: GRAV
      IMPLICIT NONE
!
      DOUBLE PRECISION PRAD(*),ANUM,DENO,QFIXG,ST,Z,YCRIT,Y
!
      INTEGER MAILLE
!
      INTRINSIC ABS
!
      COMMON/HCRIT/YCRIT
!
!-----------------------------------------------------------------------
!
!     SI ON EST A LA PROFONDEUR CRITIQUE (DENO=0), ON PREND LA VALEUR
!     DE LA PENTE SUR UN POINT PROCHE
!
      IF(ABS(Z-YCRIT).LT.1.D-5) THEN
        Y = Z+0.001D0
      ELSE
        Y = Z
      ENDIF
!
      ANUM = PRAD(MAILLE) - QFIXG**2/(ST**2*Y**(10.D0/3.D0))
      DENO = 1.D0 - QFIXG**2/(GRAV*Y**3)
!     IF (ABS(DENO).LT.1.D-6) DENO = SIGN(1.D-6,DENO)
!
      F = ANUM/DENO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
