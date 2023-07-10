MODULE M_DEBITANCE_S_D
 IMPLICIT NONE
 CONTAINS
  SUBROUTINE DEBITANCE_S_D(deb1, deb1d, st1, rh1, rh1d, s1, s1d, &
&   loifrottement, cf1, cf1d, erreur)
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
    DOUBLE PRECISION, INTENT(OUT) :: deb1
    DOUBLE PRECISION, INTENT(OUT) :: deb1d
    DOUBLE PRECISION, INTENT(OUT) :: st1
    DOUBLE PRECISION :: st1d
    DOUBLE PRECISION, INTENT(IN) :: rh1
    DOUBLE PRECISION, INTENT(IN) :: rh1d
    DOUBLE PRECISION, INTENT(IN) :: s1
    DOUBLE PRECISION, INTENT(IN) :: s1d
    INTEGER, INTENT(IN) :: loifrottement
    DOUBLE PRECISION, INTENT(IN) :: cf1
    DOUBLE PRECISION, INTENT(IN) :: cf1d
    TYPE(ERREUR_T), INTENT(INOUT) :: erreur
! Variables locales
!------------------
    DOUBLE PRECISION :: chezy
    DOUBLE PRECISION :: chezyd
!character(132) :: !arbredappel_old ! arbre d'appel precedent
! Constantes
!-----------
    INTRINSIC DSQRT
    INTRINSIC DLOG10
    DOUBLE PRECISION :: pwr1
    DOUBLE PRECISION :: pwr1d
    DOUBLE PRECISION :: result1
    DOUBLE PRECISION :: arg1
    DOUBLE PRECISION :: arg1d
!============================= Instructions ===========================
! INITIALISATION
!===============
    erreur%numero = 0
!arbredappel_old    = trim(!Erreur%arbredappel)
!Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>DEBITANCE'
    IF (rh1 .EQ. 0._DOUBLE) THEN
      st1 = cf1
      deb1 = 0._DOUBLE
      deb1d = 0.D0
      RETURN
    ELSE
      SELECT CASE  (loifrottement)
      CASE (loi_frottement_strickler)
!----------------------------------------------------
! Strickler fixe
!----------------------------------------------------
        IF (rh1 .GT. 0.0 .OR. (rh1 .LT. 0.0 .AND. w16 .EQ. INT(w16))) &
&       THEN
          pwr1d = w16*rh1**(w16-1)*rh1d
        ELSE IF (rh1 .EQ. 0.0 .AND. w16 .EQ. 1.0) THEN
          pwr1d = rh1d
        ELSE
          pwr1d = 0.0
        END IF
        pwr1 = rh1**w16
        chezyd = cf1d*pwr1 + cf1*pwr1d
        chezy = cf1*pwr1
      CASE (loi_frottement_chezy)
!----------
! Chezy fixe
!----------
        chezyd = cf1d
        chezy = cf1
      CASE (loi_frottement_colebrook)
!----------------------------------------------------
! COLEBROOK   x(alpha[ks])
!----------------------------------------------------
        result1 = DSQRT(8._DOUBLE*gpes)
        arg1d = (12._DOUBLE*rh1d*cf1-12._DOUBLE*rh1*cf1d)/cf1**2
        arg1 = 12._DOUBLE*rh1/cf1
        chezyd = 2._DOUBLE*result1*(0.190_DOUBLE*cf1d*DLOG10(arg1)+(&
&         0.190_DOUBLE*cf1+0.972_DOUBLE)*arg1d/(arg1*DLOG(10.D0)))
        chezy = 2._DOUBLE*(0.190_DOUBLE*cf1+0.972_DOUBLE)*result1*DLOG10&
&         (arg1)
      CASE (loi_frottement_bazin)
!----------------------------------------------------
! BAZIN   Chezy(i) en fonction de RH et mb (fixe)
!----------------------------------------------------
        chezyd = -(45._DOUBLE*(cf1d*rh1-cf1*rh1d)/rh1**2/(1._DOUBLE+cf1/&
&         rh1)**2)
        chezy = 45._DOUBLE/(1._DOUBLE+cf1/rh1)
      CASE DEFAULT
        chezyd = 0.D0
      END SELECT
      IF (rh1 .GT. 0.0 .OR. (rh1 .LT. 0.0 .AND. w16 .EQ. INT(w16))) THEN
        pwr1d = w16*rh1**(w16-1)*rh1d
      ELSE IF (rh1 .EQ. 0.0 .AND. w16 .EQ. 1.0) THEN
        pwr1d = rh1d
      ELSE
        pwr1d = 0.0
      END IF
      pwr1 = rh1**w16
      st1d = (chezyd*pwr1-chezy*pwr1d)/pwr1**2
      st1 = chezy/pwr1
      IF (rh1 .GT. 0.0 .OR. (rh1 .LT. 0.0 .AND. w23 .EQ. INT(w23))) THEN
        pwr1d = w23*rh1**(w23-1)*rh1d
      ELSE IF (rh1 .EQ. 0.0 .AND. w23 .EQ. 1.0) THEN
        pwr1d = rh1d
      ELSE
        pwr1d = 0.0
      END IF
      pwr1 = rh1**w23
      deb1d = (st1d*s1+st1*s1d)*pwr1 + st1*s1*pwr1d
      deb1 = st1*s1*pwr1
!------------------
! Fin du traitement
!------------------
!Erreur%arbredappel = !arbredappel_old
      RETURN
    END IF
  END SUBROUTINE DEBITANCE_S_D
END MODULE M_DEBITANCE_S_D

