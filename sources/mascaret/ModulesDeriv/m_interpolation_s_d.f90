MODULE M_INTERPOLATION_S_D
  IMPLICIT NONE
!***********************************************************************
! PROGICIEL : MASCARET
!                             A. LEBOSSE
!                             P. CHERUBINI
!                             S. PERON
!                             S. MANDELKERN
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!
!----------------------------------------------------------
!  Interface du sous systeme de protection PROTLIB
!  pour un code Fortran90.
!
!----------------------------------------------------------
!
CONTAINS
!  Differentiation of interpolation_s in forward (tangent) mode (with options noISIZE context):
!   variations   of useful results: yt
!   with respect to varying inputs: x xt yt
! Resultats
! Abscisse pour laquelle on veut YT
! Ordre d'interpolation
! Tableau des abscisses
! tableau des ordonnees
! dimension des tableaux X et Y
! Erreur
  SUBROUTINE INTERPOLATION_S_D(yt, ytd, xt, xtd, n, x, xd, y, ix, erreur&
& )
!***********************************************************************
!
!  FONCTION :
!  --------
!
!           INTERPOLATION DE LAGRANGE D'ORDRE N
!
!-----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ----------------------
!
!   SOUS PROGRAMMES APPELANTS :  - QCL
!   ---------------------------  - LEC_LIGNE
!
!
!
!   SOUS PROGRAMMES APPELES :    ---
!   -------------------------
!
!   COMMENTAIRES :        LE POLYNOME OPTIMAL D ORDRE N PASSANT PAR LES
!   ------------          POINTS (X ,Y ) EST DONNE PAR LA FORMULE:
!                                  I  I
!                         P(X) = SOMME(Y *PRODUIT( (X-X )/(X -X ) )
!                                J=1,N  J  I=1,N       I    J  I
!                                          I.NE.J
!                         EN EFFET P(X ) = Y QUELQUE SOIT J
!                                     J     J
!
!============================= Declarations =============================
    USE M_PRECISION
    USE M_PARAMETRE_C
    USE M_FICHIER_T
    USE M_ERREUR_T
    USE M_TRAITER_ERREUR_I
    USE M_MESSAGE_C
    IMPLICIT NONE
!.. Arguments ..
    DOUBLE PRECISION, INTENT(OUT) :: yt
    DOUBLE PRECISION, INTENT(OUT) :: ytd
    DOUBLE PRECISION, INTENT(IN) :: xt
    DOUBLE PRECISION, INTENT(IN) :: xtd
    INTEGER, INTENT(IN) :: n, ix
    DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: x, y
    DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: xd
    TYPE(ERREUR_T), INTENT(INOUT) :: erreur
!.. Scalairs locaux ..
    INTEGER :: i, imax, imin, imoy, ixm1, j
    DOUBLE PRECISION :: coeff, xmax, xmin
    DOUBLE PRECISION :: coeffd
!character(132) :: !arbredappel_old
!.. Fonctions intrinseques ..
    INTRINSIC ABS, INT, REAL
    INTRINSIC TRIM
    INTRINSIC DABS
    DOUBLE PRECISION :: dabs1
    DOUBLE PRECISION :: dabs0
!============================ Instructions =================
    erreur%numero = 0
!arbredappel_old = trim(Erreur%Arbredappel)
    erreur%arbredappel = TRIM(erreur%arbredappel)//'=>INTERPOLATION'
!     TESTS SUR LES DONNEES
!     --------------------
!---------------------------------
! test sur l'ordre d'interpolation
!---------------------------------
    IF (n .GT. ix) THEN
      erreur%numero = 17
      erreur%ft = err_17
      erreur%ft_c = err_17c
      CALL TRAITER_ERREUR(erreur, n, ix)
      RETURN
    ELSE IF (xt .LT. x(1) .OR. xt .GT. x(ix)) THEN
!-----------------------------------
! test sur l'appartenance au domaine
!-----------------------------------
      erreur%numero = 20
      erreur%ft = err_20
      erreur%ft_c = err_20c
      CALL TRAITER_ERREUR(erreur, xt, x(1), x(ix))
      RETURN
    ELSE
      DO i=1,ix
        IF (xt - x(i) .GE. 0.) THEN
          dabs0 = xt - x(i)
        ELSE
          dabs0 = -(xt-x(i))
        END IF
        IF (dabs0 .LE. eps10) GOTO 110
      END DO
!     SELECTION DES N POINTS SERVANT A L INTERPOLATION
!     ------------------------------------------------
      ixm1 = ix - 1
      DO i=1,ixm1
        IF (xt .GT. x(i) .AND. xt .LT. x(i+1)) GOTO 100
      END DO
 100  IF (y(i) - y(i+1) .GE. 0.) THEN
        dabs1 = y(i) - y(i+1)
      ELSE
        dabs1 = -(y(i)-y(i+1))
      END IF
      IF (dabs1 .LE. eps10) THEN
        yt = y(i)
        ytd = 0.D0
      ELSE
!     SINON INTERPOLER
        imoy = i
        xmin = REAL(imoy) - REAL(n)/2.
        xmax = REAL(imoy) + REAL(n)/2.
        imin = INT(xmin) + 1
        imax = INT(xmax) + 1
!     INTERPOLATION DE LAGRANGE
!     -------------------------
        yt = 0.
        ytd = 0.D0
        DO j=imin,imax
          coeff = 1.
          coeffd = 0.D0
          DO i=imin,imax
            IF (i .NE. j) THEN
              coeffd = ((coeffd*(xt-x(i))+coeff*(xtd-xd(i)))*(x(j)-x(i))&
&               -coeff*(xt-x(i))*(xd(j)-xd(i)))/(x(j)-x(i))**2
              coeff = coeff*(xt-x(i))/(x(j)-x(i))
            END IF
          END DO
          ytd = ytd + y(j)*coeffd
          yt = yt + y(j)*coeff
        END DO
      END IF
!Erreur%Arbredappel = !arbredappel_old
      RETURN
 110  yt = y(i)
      ytd = 0.D0
!Erreur%Arbredappel = !arbredappel_old
      RETURN
    END IF
  END SUBROUTINE INTERPOLATION_S_D
END MODULE M_INTERPOLATION_S_D

