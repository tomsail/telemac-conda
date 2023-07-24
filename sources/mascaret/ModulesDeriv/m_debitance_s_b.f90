MODULE M_DEBITANCE_S_B
  IMPLICIT NONE

CONTAINS
!  Differentiation of debitance_s in reverse (adjoint) mode (with options noISIZE context):
!   gradient     of useful results: s1 deb1 cf1
!   with respect to varying inputs: s1 rh1 cf1
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
! Debitance
! Strickler mineur
! Rayon hydraulique mineur
! Section mouillee mineur
! Loi de frottement
! Coefficient de frottement mineur
  SUBROUTINE DEBITANCE_S_B(deb1, deb1b, st1, rh1, rh1b, s1, s1b, &
&   loifrottement, cf1, cf1b, erreur)
!============================= Declarations ===========================
    USE M_PRECISION
! LOI_FROTTEMENT_STRICKLER
    USE M_CONSTANTES_CALCUL_C
! GPES, W23,
    USE M_PARAMETRE_C
! Definition du type Erreur
    USE M_ERREUR_T
    IMPLICIT NONE
! Arguments
!----------
    DOUBLE PRECISION :: deb1
    DOUBLE PRECISION :: deb1b
    DOUBLE PRECISION :: st1
    DOUBLE PRECISION, INTENT(IN) :: rh1
    DOUBLE PRECISION :: rh1b
    DOUBLE PRECISION, INTENT(IN) :: s1
    DOUBLE PRECISION :: s1b
    INTEGER, INTENT(IN) :: loifrottement
    DOUBLE PRECISION, INTENT(IN) :: cf1
    DOUBLE PRECISION :: cf1b
    TYPE(ERREUR_T), INTENT(INOUT) :: erreur
! Variables locales
!------------------
    DOUBLE PRECISION :: chezy
    DOUBLE PRECISION :: chezyb
!character(132) :: !arbredappel_old ! arbre d'appel precedent
! Constantes
!-----------
    INTRINSIC DSQRT
    INTRINSIC DLOG10
    INTEGER :: branch
    DOUBLE PRECISION :: temp1
    DOUBLE PRECISION :: temp0
    DOUBLE PRECISION :: tempb2
    DOUBLE PRECISION :: tempb1
    DOUBLE PRECISION :: tempb0
    DOUBLE PRECISION :: st1b
    DOUBLE PRECISION :: tempb
    DOUBLE PRECISION :: temp
!============================= Instructions ===========================
! INITIALISATION
!===============
!arbredappel_old    = trim(!Erreur%arbredappel)
!Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>DEBITANCE'
    IF (rh1 .EQ. 0._DOUBLE) THEN
      rh1b = 0.D0
    ELSE
      SELECT CASE  (loifrottement)
      CASE (loi_frottement_strickler)
!----------------------------------------------------
! Strickler fixe
!----------------------------------------------------
        chezy = cf1*rh1**w16
        CALL PUSHCONTROL3B(1)
      CASE (loi_frottement_chezy)
!----------
! Chezy fixe
!----------
        chezy = cf1
        CALL PUSHCONTROL3B(2)
      CASE (loi_frottement_colebrook)
!----------------------------------------------------
! COLEBROOK   x(alpha[ks])
!----------------------------------------------------
        chezy = 2._DOUBLE*(0.190_DOUBLE*cf1+0.972_DOUBLE)*DSQRT(&
&         8._DOUBLE*gpes)*DLOG10(12._DOUBLE*rh1/cf1)
        CALL PUSHCONTROL3B(3)
      CASE (loi_frottement_bazin)
!----------------------------------------------------
! BAZIN   Chezy(i) en fonction de RH et mb (fixe)
!----------------------------------------------------
        chezy = 45._DOUBLE/(1._DOUBLE+cf1/rh1)
        CALL PUSHCONTROL3B(4)
      CASE DEFAULT
        CALL PUSHCONTROL3B(0)
      END SELECT
      st1 = chezy/rh1**w16
!------------------
! Fin du traitement
!------------------
!Erreur%arbredappel = !arbredappel_old
      tempb2 = rh1**w23*deb1b
      st1b = s1*tempb2
      s1b = s1b + st1*tempb2
      IF (rh1 .LE. 0.0 .AND. (w23 .EQ. 0.0 .OR. w23 .NE. INT(w23))) THEN
        rh1b = 0.0
      ELSE
        rh1b = st1*s1*w23*rh1**(w23-1)*deb1b
      END IF
      temp1 = rh1**w16
      chezyb = st1b/temp1
      IF (.NOT.(rh1 .LE. 0.0 .AND. (w16 .EQ. 0.0 .OR. w16 .NE. INT(w16))&
&         )) rh1b = rh1b - chezy*w16*rh1**(w16-1)*st1b/temp1**2
      CALL POPCONTROL3B(branch)
      IF (branch .LT. 2) THEN
        IF (branch .NE. 0) THEN
          cf1b = cf1b + rh1**w16*chezyb
          IF (.NOT.(rh1 .LE. 0.0 .AND. (w16 .EQ. 0.0 .OR. w16 .NE. INT(&
&             w16)))) rh1b = rh1b + cf1*w16*rh1**(w16-1)*chezyb
        END IF
      ELSE IF (branch .EQ. 2) THEN
        cf1b = cf1b + chezyb
      ELSE IF (branch .EQ. 3) THEN
        temp = rh1/cf1
        tempb = DSQRT(gpes*8._DOUBLE)*2._DOUBLE*chezyb
        tempb0 = (0.190_DOUBLE*cf1+0.972_DOUBLE)*tempb/(temp*DLOG(10.D0)&
&         *cf1)
        cf1b = cf1b + DLOG10(12._DOUBLE*temp)*0.190_DOUBLE*tempb - temp*&
&         tempb0
        rh1b = rh1b + tempb0
      ELSE
        temp0 = cf1/rh1
        tempb1 = -(45._DOUBLE*chezyb/((temp0+1._DOUBLE)**2*rh1))
        cf1b = cf1b + tempb1
        rh1b = rh1b - temp0*tempb1
      END IF
    END IF
  END SUBROUTINE DEBITANCE_S_B

END MODULE M_DEBITANCE_S_B

