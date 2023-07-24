MODULE M_INTERPOLATION_S_B
  IMPLICIT NONE


CONTAINS
!  Differentiation of interpolation_s in reverse (adjoint) mode (with options noISIZE context):
!   gradient     of useful results: x xt yt
!   with respect to varying inputs: x xt yt
! Resultats
! Abscisse pour laquelle on veut YT
! Ordre d'interpolation
! Tableau des abscisses
! tableau des ordonnees
! dimension des tableaux X et Y
! Erreur
  SUBROUTINE INTERPOLATION_S_B(yt, ytb, xt, xtb, n, x, xb, y, yb, ix, &
&   erreur)
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
    DOUBLE PRECISION :: yt
    DOUBLE PRECISION :: ytb
    DOUBLE PRECISION, INTENT(IN) :: xt
    DOUBLE PRECISION :: xtb
    INTEGER, INTENT(IN) :: n, ix
    DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: x, y
    DOUBLE PRECISION, DIMENSION(:) :: xb, yb
    TYPE(ERREUR_T), INTENT(INOUT) :: erreur
!.. Scalairs locaux ..
    INTEGER :: i, imax, imin, imoy, ixm1, j
    DOUBLE PRECISION :: coeff, xmax, xmin
    DOUBLE PRECISION :: coeffb
!character(132) :: !arbredappel_old
!.. Fonctions intrinseques ..
    INTRINSIC ABS, INT, REAL
    INTRINSIC TRIM
    INTRINSIC DABS
    INTEGER :: ad_count
    INTEGER :: i0
    INTEGER :: branch
    DOUBLE PRECISION :: tempb0
    DOUBLE PRECISION :: tempb
    DOUBLE PRECISION :: dabs1
    DOUBLE PRECISION :: dabs0
!============================ Instructions =================
!arbredappel_old = trim(Erreur%Arbredappel)
!     TESTS SUR LES DONNEES
!     --------------------
!---------------------------------
! test sur l'ordre d'interpolation
!---------------------------------
    IF (n .LE. ix) THEN
!-----------------------------------
! test sur l'appartenance au domaine
!-----------------------------------
      IF (.NOT.(xt .LT. x(1) .OR. xt .GT. x(ix))) THEN
        ad_count = 1
        DO i=1,ix
          IF (xt - x(i) .GE. 0.) THEN
            dabs0 = xt - x(i)
          ELSE
            dabs0 = -(xt-x(i))
          END IF
          IF (dabs0 .LE. eps10) THEN
            GOTO 110
          ELSE
            ad_count = ad_count + 1
          END IF
        END DO
        CALL PUSHCONTROL1B(0)
        CALL PUSHINTEGER4(ad_count)
!     SELECTION DES N POINTS SERVANT A L INTERPOLATION
!     ------------------------------------------------
        ixm1 = ix - 1
        DO i=1,ixm1
          IF (xt .GT. x(i) .AND. xt .LT. x(i+1)) GOTO 100
        END DO
 100    IF (y(i) - y(i+1) .GE. 0.) THEN
          dabs1 = y(i) - y(i+1)
        ELSE
          dabs1 = -(y(i)-y(i+1))
        END IF
        IF (dabs1 .GT. eps10) THEN
!     SINON INTERPOLER
          imoy = i
          xmin = REAL(imoy) - REAL(n)/2.
          xmax = REAL(imoy) + REAL(n)/2.
          imin = INT(xmin) + 1
          imax = INT(xmax) + 1
!     INTERPOLATION DE LAGRANGE
!     -------------------------
          DO j=imin,imax
            coeff = 1.
            DO i=imin,imax
              IF (i .NE. j) THEN
                CALL PUSHREAL8(coeff)
                coeff = coeff*(xt-x(i))/(x(j)-x(i))
                CALL PUSHCONTROL1B(1)
              ELSE
                CALL PUSHCONTROL1B(0)
              END IF
            END DO
          END DO
          DO j=imax,imin,-1
            coeffb = y(j)*ytb
            DO i=imax,imin,-1
              CALL POPCONTROL1B(branch)
              IF (branch .NE. 0) THEN
                CALL POPREAL8(coeff)
                tempb = coeffb/(x(j)-x(i))
                tempb0 = -(coeff*(xt-x(i))*tempb/(x(j)-x(i)))
                xtb = xtb + coeff*tempb
                xb(i) = xb(i) - tempb0 - coeff*tempb
                xb(j) = xb(j) + tempb0
                coeffb = (xt-x(i))*tempb
              END IF
            END DO
          END DO
        END IF
        GOTO 120
 110    CALL PUSHCONTROL1B(1)
        CALL PUSHINTEGER4(ad_count)
 120    CALL POPINTEGER4(ad_count)
        DO i0=1,ad_count
          IF (i0 .EQ. 1) CALL POPCONTROL1B(branch)
        END DO
        ytb = 0.D0
      END IF
    END IF
  END SUBROUTINE INTERPOLATION_S_B

END MODULE M_INTERPOLATION_S_B

