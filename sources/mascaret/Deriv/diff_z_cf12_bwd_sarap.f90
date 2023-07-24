!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET.
!
!   MASCARET is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET.  If not, see <http://www.gnu.org/licenses/>

SUBROUTINE REPAR_B(deb, debb, vmoy, vmoyb, beta, betab, q1, q2, s1, s1b&
& , s2, s2b, rh1, rh1b, rh2, rh2b, p1, p1b, p2, p2b, q, qb, cf1, cf1b, &
& cf2, cf2b, modelelit, loifrottement, erreur)
! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P. CHERUBINI
!                             S. MANDELKERN
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
!
!   FONCTION :
!   --------
!   CALCUL DE LA REPARTITION DE DEBIT
!   MODELE DEBORD : REPARTITION LIT MINEUR/LIT MAJEUR , UTILISANT LE
!                   PARAMETRE DE MODIFICATION DES DEBITANCES A
!   MODELE CRUGOS : APPLICATION DE LA FORMULE DE COMPOSITION DES
!   ou Fond Berge   RUGOSITES D'EINSTEIN -  LE LIT EST ALORS CONSIDERE
!                   COMME UNIQUE, AVEC UN COEFFICIENT DE RUGOSITE
!                   FONCTION DES COEFFICIENTS INITIAUX, ET DES
!                   PARAMETRES GEOMETRIQUES
!   SI AUNCUN MODELE N'A ETE RETENU, LE MODELE DEBORD EST APPLIQUE EN
!   PRENANT LE PARAMETRE A EGAL A 1
!
! ----------------------------------------------------------------------
! ARGUMENTS
! .________________.____._______________________________________________
! !    NOM    !TYPE!MODE!                   ROLE
! !___________!____!____!______________________________________________
! !  DEB      ! R  !<-- ! DEBITANCE
! !  VMOY     ! R  !<-- ! VITESSE MOYENNE
! !  BETA     ! R  !<-- ! COEFFICIENT DE REPARTITION DE VITESSES MIN/MAJ
! !  Q1,Q2    ! R  !<-- ! DEBIT
! !  S1,S2    ! R  !<-->! SECTION MOUILLEE    ) INDICE 1 LIT MINEUR
! !  RH1,RH2  ! R  !<-->! RAYON HYDRAULIQUE   )        2 LIT MAJEUR
! !  Q        ! R  ! -->! DEBIT GLOBAL
! !  P1,P2    ! R  ! -->! PERIMETRE MOUILLE   )
! !  CF1,CF2  ! R  ! -->! COEF. DE FROTTEMENT MIN et MAJ
! !  ModeleLit! I  ! -->! Type du modele du lit
! !  Erreur   ! T  ! -->! Erreur
! !___________!____!____!______________________________________________
!  VARIABLES LOCALES
! .___________.____.____.______________________________________________
! !  RH       ! R  !<-- ! RAYON HYDRAULIQUE
! !  A        ! R  ! -- ! PARAMETRE DU MODELE DEBORD
! !  PUT      ! R  ! -- ! VALEUR SEUIL POUR LE CALCUL DE A
! !___________!____!____!______________________________________________
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
! ----------------------------------------------------------------------
!
!   FICHIERS  ENTREE/SORTIE :
!   -------------------------
!   SOUS PROGRAMMES APPELANTS :  REZO, CRITIQ, PERMAT, QREPAR, SARAP
!   ---------------------------
!   SOUS PROGRAMMES APPELES :
!   -------------------------
!   COMMENTAIRES :
!   --------------
!   POUR TRAITER UN LIT COMPOSE EN LIT UNIQUE, IL SUFFIT DE DEMANDER
!   LE MODELE CRUGOS, AVEC DES COEFFICIENTS DE RUGOSITE EGAUX
! ----------------------------------------------------------------------
!============================ Declarations ==============================
  USE M_PRECISION
  USE M_PARAMETRE_C
! Messages d'erreur
  USE M_MESSAGE_C
! MODELE_LIT
  USE M_CONSTANTES_CALCUL_C
! type ERREUR_T
  USE M_ERREUR_T
! Calcul de la debitance
  USE M_DEBITANCE_S_B
  USE M_DEBITANCE_S
! Traitement de l'erreur
  USE M_TRAITER_ERREUR_I
  IMPLICIT NONE
!Erreur%arbredappel = !arbredappel_old
!.. Formal Arguments ..
  DOUBLE PRECISION :: deb
  DOUBLE PRECISION :: debb
  DOUBLE PRECISION :: vmoy
  DOUBLE PRECISION :: vmoyb
  DOUBLE PRECISION :: beta
  DOUBLE PRECISION :: betab
  DOUBLE PRECISION, INTENT(OUT) :: q1
  DOUBLE PRECISION, INTENT(OUT) :: q2
  DOUBLE PRECISION, INTENT(INOUT) :: s1
  DOUBLE PRECISION :: s1b
  DOUBLE PRECISION, INTENT(INOUT) :: s2
  DOUBLE PRECISION :: s2b
  DOUBLE PRECISION, INTENT(INOUT) :: rh1
  DOUBLE PRECISION :: rh1b
  DOUBLE PRECISION, INTENT(INOUT) :: rh2
  DOUBLE PRECISION :: rh2b
  DOUBLE PRECISION, INTENT(IN) :: q
  DOUBLE PRECISION :: qb
  DOUBLE PRECISION, INTENT(IN) :: p1
  DOUBLE PRECISION :: p1b
  DOUBLE PRECISION, INTENT(IN) :: p2
  DOUBLE PRECISION :: p2b
  DOUBLE PRECISION, INTENT(IN) :: cf1
  DOUBLE PRECISION :: cf1b
  DOUBLE PRECISION, INTENT(IN) :: cf2
  DOUBLE PRECISION :: cf2b
  INTEGER, INTENT(IN) :: modelelit
  INTEGER, INTENT(IN) :: loifrottement
  TYPE(ERREUR_T), INTENT(INOUT) :: erreur
!.. Local Scalars ..
  DOUBLE PRECISION :: st1_temp
  DOUBLE PRECISION :: rh
  DOUBLE PRECISION :: rhb
  DOUBLE PRECISION :: a, a0, deb1, deb2, eta, fp1, fp2, r0, s&
& , stequi
  DOUBLE PRECISION :: ab, a0b, deb1b, deb2b, etab, fp1b, fp2b, fs1b, &
& fs2b, r0b, sb, stequib
!character(132) :: !arbredappel_old
! Les Constantes sont declares dans le module M_PARAMETRES
  DOUBLE PRECISION, SAVE :: put=0.3_DOUBLE
!.. Intrinsic Functions ..
  INTRINSIC DCOS, DSQRT
  INTEGER :: branch
  DOUBLE PRECISION :: temp3
  DOUBLE PRECISION :: temp2
  DOUBLE PRECISION :: temp1
  DOUBLE PRECISION :: temp0
  DOUBLE PRECISION :: tempb9
  DOUBLE PRECISION :: tempb8
  DOUBLE PRECISION :: tempb7
  DOUBLE PRECISION :: tempb6
  DOUBLE PRECISION :: tempb5
  DOUBLE PRECISION :: tempb4
  DOUBLE PRECISION :: tempb3
  DOUBLE PRECISION :: tempb2
  DOUBLE PRECISION :: tempb1
  DOUBLE PRECISION :: tempb0
  DOUBLE PRECISION :: tempb14
  DOUBLE PRECISION :: tempb13
  DOUBLE PRECISION :: tempb12
  DOUBLE PRECISION :: tempb11
  DOUBLE PRECISION :: tempb10
  DOUBLE PRECISION :: tempb
  DOUBLE PRECISION :: temp
  DOUBLE PRECISION :: temp9
  DOUBLE PRECISION :: temp8
  DOUBLE PRECISION :: temp7
  DOUBLE PRECISION :: temp6
  DOUBLE PRECISION :: temp5
  DOUBLE PRECISION :: temp4
!============================= Instructions =============================
! INITIALISATION
! --------------
!arbredappel_old    = trim(!Erreur%arbredappel)
!Erreur%Arbredappel = trim(!Erreur%arbredappel)//'=>REPAR'
! GRANDEURS INDEPENDANTES DU MODELE
! ---------------------------------
! Tests
  IF (p1 .LE. eps6) THEN
    rh1b = 0.D0
    rh2b = 0.D0
  ELSE IF (s1 .LE. eps6) THEN
    rh1b = 0.D0
    rh2b = 0.D0
  ELSE
    rh = (s1+s2)/(p1+p2)
! MODELE DE COMPOSITION DES RUGOSITES EN LIT UNIQUE
! -------------------------------------------------
    IF (modelelit .EQ. modele_lit_fond_berge) THEN
! MODELISATION FOND/BERGE
! CALCULS INTERNES DU MODELE
      s = s1 + s2
      fp1 = p1/cf1**w32
      fp2 = p2/cf2**w32
      stequi = ((p1+p2)/(fp1+fp2))**w23
! RESULTATS
! MODIFICATION DES VARIABLES GEOMETRIQUES
      temp = cf1**w32
      temp0 = cf2**w32
      fs2b = s2b
      fs1b = s1b
      tempb3 = rh**w23*debb
      stequib = s*tempb3
      IF (rh .LE. 0.0 .AND. (w23 .EQ. 0.0 .OR. w23 .NE. INT(w23))) THEN
        rhb = 0.0
      ELSE
        rhb = stequi*s*w23*rh**(w23-1)*debb
      END IF
      temp1 = (p1+p2)/(fp1+fp2)
      IF (temp1 .LE. 0.0 .AND. (w23 .EQ. 0.0 .OR. w23 .NE. INT(w23))) &
&     THEN
        tempb6 = 0.0
      ELSE
        tempb6 = w23*temp1**(w23-1)*stequib/(fp1+fp2)
      END IF
      tempb7 = -(temp1*tempb6)
      tempb4 = fs2b/(fp1+fp2)
      tempb8 = -(s*fp2*tempb4/(fp1+fp2))
      tempb5 = fs1b/(fp1+fp2)
      sb = fp2*tempb4 + fp1*tempb5 + stequi*tempb3
      tempb9 = -(s*fp1*tempb5/(fp1+fp2))
      fp1b = tempb8 + tempb9 + s*tempb5 + tempb7
      p1b = p1b + fp1b/temp + tempb6
      fp2b = s*tempb4 + tempb8 + tempb9 + tempb7
      p2b = p2b + fp2b/temp0 + tempb6
      IF (.NOT.(cf2 .LE. 0.0 .AND. (w32 .EQ. 0.0 .OR. w32 .NE. INT(w32))&
&         )) cf2b = cf2b - p2*w32*cf2**(w32-1)*fp2b/temp0**2
      IF (.NOT.(cf1 .LE. 0.0 .AND. (w32 .EQ. 0.0 .OR. w32 .NE. INT(w32))&
&         )) cf1b = cf1b - p1*w32*cf1**(w32-1)*fp1b/temp**2
      s1b = sb
      s2b = sb
      rh1b = 0.D0
      rh2b = 0.D0
      debb = 0.D0
      betab = 0.D0
    ELSE
! VALEUR DE BASE DU PARAMETRE DU MODELE DEBORD
! --------------------------------------------
! COMPOSITION DES RUGOSITE DEBORD (OPTIO2) OU NON DEFINIE (OPTIO3)
      a = 1._DOUBLE
! MODELE DEBORD
! -------------
      IF (modelelit .EQ. modele_lit_debord) THEN
        a0 = w09*(cf2/cf1)**w16
        r0 = rh2/rh1
        IF (r0 .GE. put) THEN
          a = a0
          CALL PUSHCONTROL2B(0)
        ELSE
          a = ((1._DOUBLE-a0)*DCOS(pi*r0/put)+1._DOUBLE+a0)/2._DOUBLE
          CALL PUSHCONTROL2B(1)
        END IF
      ELSE
        CALL PUSHCONTROL2B(2)
      END IF
      CALL DEBITANCE_S(deb1, st1_temp, rh1, s1, loifrottement, cf1, &
&                erreur)
      IF (erreur%numero .NE. 0) THEN
        rh2b = 0.D0
        deb1b = 0.D0
        ab = 0.D0
      ELSE
        CALL PUSHREAL8(deb1)
        deb1 = deb1*a
        deb2 = cf2*DSQRT(s2**2+s1*s2*(1._DOUBLE-a*a))*rh2**w23
        IF (s2 .LE. s1*eps4) THEN
          deb1b = 0.D0
          deb2b = 0.D0
        ELSE
          eta = deb1/deb2
          temp9 = (eta+1._DOUBLE)**2
          temp8 = (s1+s2)/temp9
          tempb13 = temp8*betab/s1
          temp7 = eta**2/s1
          tempb14 = (temp7+1.0/s2)*betab/temp9
          etab = 2*eta*tempb13 - temp8*2*(eta+1._DOUBLE)*tempb14
          s1b = s1b + tempb14 - temp7*tempb13
          s2b = s2b + tempb14 - temp8*betab/s2**2
          deb1b = etab/deb2
          deb2b = -(deb1*etab/deb2**2)
        END IF
        CALL POPREAL8(deb1)
        deb1b = deb1b + debb
        deb2b = deb2b + debb
        temp6 = rh2**w23
        temp5 = -(a**2) + 1._DOUBLE
        temp3 = s2**2 + s1*s2*temp5
        temp4 = DSQRT(temp3)
        IF (temp3 .EQ. 0.0) THEN
          tempb12 = 0.0
        ELSE
          tempb12 = cf2*temp6*deb2b/(2.D0*DSQRT(temp3))
        END IF
        s2b = s2b + (temp5*s1+2*s2)*tempb12
        s1b = s1b + temp5*s2*tempb12
        ab = deb1*deb1b - s1*s2*2*a*tempb12
        cf2b = cf2b + temp4*temp6*deb2b
        IF (rh2 .LE. 0.0 .AND. (w23 .EQ. 0.0 .OR. w23 .NE. INT(w23))) &
&       THEN
          rh2b = 0.0
        ELSE
          rh2b = cf2*temp4*w23*rh2**(w23-1)*deb2b
        END IF
        deb1b = a*deb1b
        debb = 0.D0
        betab = 0.D0
      END IF
      CALL DEBITANCE_S_B(deb1, deb1b, st1_temp, rh1, rh1b, s1, s1b, &
&                  loifrottement, cf1, cf1b, erreur)
      CALL POPCONTROL2B(branch)
      IF (branch .EQ. 0) THEN
        a0b = ab
        r0b = 0.D0
      ELSE IF (branch .EQ. 1) THEN
        tempb11 = ab/2._DOUBLE
        temp2 = pi*r0/put
        a0b = (1.D0-DCOS(temp2))*tempb11
        r0b = -(pi*DSIN(temp2)*(1._DOUBLE-a0)*tempb11/put)
      ELSE
        GOTO 100
      END IF
      rh2b = rh2b + r0b/rh1
      rh1b = rh1b - rh2*r0b/rh1**2
      IF (cf2/cf1 .LE. 0.0 .AND. (w16 .EQ. 0.0 .OR. w16 .NE. INT(w16))) &
&     THEN
        tempb10 = 0.0
      ELSE
        tempb10 = w16*(cf2/cf1)**(w16-1)*w09*a0b/cf1
      END IF
      cf2b = cf2b + tempb10
      cf1b = cf1b - cf2*tempb10/cf1
 100  rhb = 0.D0
    END IF
    tempb1 = rhb/(p1+p2)
    tempb = vmoyb/(s1+s2)
    tempb0 = -(q*tempb/(s1+s2))
    qb = qb + tempb
    s1b = s1b + tempb1 + tempb0
    s2b = s2b + tempb1 + tempb0
    tempb2 = -((s1+s2)*tempb1/(p1+p2))
    p1b = p1b + tempb2
    p2b = p2b + tempb2
    vmoyb = 0.D0
  END IF
END SUBROUTINE REPAR_B




SUBROUTINE PSING_B(zam, zamb, singularite, zref, zav, zavb&
& , qam, qamb, profil, b1plan, idt, xdt, section, temps, erreur)
! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             S. PERON
!                             S. MANDELKERN
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
!
!  FONCTION :
!  ----------
!
!             CALCUL DE LA COTE A L'AMONT D'UNE SINGULARITE
!             EN REGIME PERMANENT
!
!-----------------------------------------------------------------------
!
!  FICHIERS ENTREE/SORTIE :
!  ----------------------
!
!  SOUS PROGRAMME APPELANT :  PERMAT
!  -------------------------
!  SOUS PROGRAMMES APPELES :  INTERPOLATION_S
!  -------------------------
!
!  COMMENTAIRES :
!  ------------
!
!  . LES CALCULS DEPENDENT DU TYPE DE LA SINGULARITE
!  . SI LA SINGULARITE EST DEFINIE AU MOYEN D'UNE FAMILLE DE COURBES
!    (TYPE 1), LA COTE AMONT EST OBTENUE DIRECTEMENT AU MOYEN
!    D'INTERPOLATIONS SUR CES COURBES
!  . SI LA SINGULARITE EST DEFINIE AU MOYEN D'UNE LOI
!    Q = F ( ZAMONT , ZAVAL) (TYPE 2) , LA COTE AMONT EST
!    ESTIMEE INITIALEMENT EN SUPPOSANT LE REGIME DENOYE, PUIS ELLE
!    EST MODIFIEE LE  CAS ECHEANT JUSQU'A OBTENIR LE DEBIT CORRECT
!  . SI LA SINGULARITE EST DE TYPE 3, LA COTE AMONT EST ESTIMEE
!    DE MANIERE SEMBLABLE, EN ASSIMILANT LA CHARGE A LA HAUTEUR
!    AU DESSUS DU SEUIL
!  . SI LA SINGULARITE EST DE TYPE 4 OU 5 LA SOLUTION EST
!    IMMEDIATE
!  . LES TYPES SUIVANTS NE SONT PAS ADMIS EN PERMANENT
!
!    EN REGIME NOYE , LA CORRECTION EST DONNEE PAR LE COEFFICIENT C :
!    RH=(HAVAL-Singularite%CoteCrete)/(HAMONT-Singularite%CoteCrete)
!       ---          RH < 0.8   C= +1
!       ---   0.8  < RH < 1.0   C= C1*RH**3 + C2*RH**2 + C3*RH + C4
! **********************************************************************
!============================= Declarations ===========================
!
!.. Modules importes ..
!----------------------
  USE M_PRECISION
! EPS3, W23, W32
  USE M_PARAMETRE_C
! Messages d'erreur
  USE M_MESSAGE_C
! Type ERREUR_T
  USE M_ERREUR_T
! Type PROFIL_T
  USE M_PROFIL_T
! Type SINGULARITE_T
  USE M_SINGULARITE_T
! Sous-programme INTERPOLATION_S
  USE M_INTERPOLATION_S
  USE M_INTERPOLATION_S_B
! Sous programme RHSBP_SECTION_S
  USE M_RHSBP_S
! Traitement de l'erreur
  USE M_TRAITER_ERREUR_I
  IMPLICIT NONE
!.. Arguments ..
!----------------
  DOUBLE PRECISION, INTENT(INOUT) :: zam
  DOUBLE PRECISION :: zamb
  TYPE(SINGULARITE_T), INTENT(IN) :: singularite
  DOUBLE PRECISION, INTENT(IN) :: zref
  DOUBLE PRECISION, INTENT(IN) :: zav
  DOUBLE PRECISION :: zavb
  DOUBLE PRECISION, INTENT(IN) :: qam
  DOUBLE PRECISION :: qamb
  TYPE(PROFIL_T), DIMENSION(:), INTENT(IN) :: profil
  DOUBLE PRECISION, DIMENSION(:, :), INTENT(IN) :: b1plan
  INTEGER, DIMENSION(:), INTENT(IN) :: idt
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: xdt
  INTEGER, INTENT(IN) :: section
  DOUBLE PRECISION, INTENT(IN) :: temps
  TYPE(ERREUR_T), INTENT(INOUT) :: erreur
!.. Constantes ..
!----------------
! racine(2*G)
  DOUBLE PRECISION, PARAMETER :: rdg=4.429447_DOUBLE
! Seuil pour RH indicateur
  DOUBLE PRECISION, PARAMETER :: cdenoy=0.8_DOUBLE
! du regime noye/denoye
! Coefficients
  DOUBLE PRECISION, PARAMETER :: c1=0._DOUBLE
! de la loi
  DOUBLE PRECISION, PARAMETER :: c2=-25._DOUBLE
! en regime
  DOUBLE PRECISION, PARAMETER :: c3=40._DOUBLE
! noye
  DOUBLE PRECISION, PARAMETER :: c4=-15._DOUBLE
! Coefficients de la
  DOUBLE PRECISION, PARAMETER :: d1=0.385_DOUBLE
! loi en regime noye seuil mince
  DOUBLE PRECISION, PARAMETER :: d2=1.5_DOUBLE
! Pas pour la convergence
  INTEGER, PARAMETER :: pas=10
! Nombre d'iteration maximal
  INTEGER, PARAMETER :: nb_iter_max=1000
!.. Variables Locales ..
!-----------------------
  DOUBLE PRECISION :: alpha
  DOUBLE PRECISION :: alphab
  DOUBLE PRECISION :: dch
  DOUBLE PRECISION :: dx
  DOUBLE PRECISION :: dz
  DOUBLE PRECISION :: dzb
  DOUBLE PRECISION :: epsq
  DOUBLE PRECISION :: charge_amont, charge_aval
  DOUBLE PRECISION :: charge_amontb, charge_avalb
  DOUBLE PRECISION :: charge
  DOUBLE PRECISION :: largeur_seuil
  DOUBLE PRECISION :: q1, q2
  DOUBLE PRECISION :: qamont
  DOUBLE PRECISION :: rh
  DOUBLE PRECISION :: zam1, zam2
  DOUBLE PRECISION :: zam1b, zam2b
  DOUBLE PRECISION :: cote_crete
  INTEGER :: iq
  INTEGER :: ipoint
! numero d'iteration
  INTEGER :: num_iter
  INTEGER :: sens
  INTRINSIC SIZE
  INTRINSIC DMAX1
  INTRINSIC DABS
  INTRINSIC MAX
  INTEGER :: arg1
  INTEGER :: ad_count
  INTEGER :: i
  INTEGER :: branch
  INTEGER :: ad_count0
  INTEGER :: i0
  INTEGER :: ad_count1
  INTEGER :: i1
  DOUBLE PRECISION :: dummyzerodiffb0(SIZE(singularite%ptzamont(:, :), &
& 1))
  DOUBLE PRECISION :: dmax14b
  DOUBLE PRECISION :: temp0
  DOUBLE PRECISION :: dummyzerodiffb(SIZE(singularite%ptzaval(:), 1))
  DOUBLE PRECISION :: dmax10b
  DOUBLE PRECISION :: dmax13b
  DOUBLE PRECISION :: dmax12b
  DOUBLE PRECISION :: dabs2
  DOUBLE PRECISION :: dabs1
  DOUBLE PRECISION :: dabs0
  DOUBLE PRECISION :: dmax14
  DOUBLE PRECISION :: dmax13
  DOUBLE PRECISION :: dmax12
  DOUBLE PRECISION :: dmax11
  DOUBLE PRECISION :: temp
  DOUBLE PRECISION :: dmax10
  DOUBLE PRECISION :: dmax11b
  DOUBLE PRECISION :: dummyzerodiffb6(SIZE(singularite%ptz(:), 1))
  DOUBLE PRECISION :: dummyzerodiffb5(SIZE(singularite%ptq(:), 1))
  DOUBLE PRECISION :: dummyzerodiffb4(SIZE(singularite%ptz(:), 1))
  DOUBLE PRECISION :: dummyzerodiffb3(SIZE(singularite%ptq(:), 1))
  DOUBLE PRECISION :: dummyzerodiffb2(SIZE(singularite%ptzamont(:, :)&
& , 1))
  DOUBLE PRECISION :: dummyzerodiffb1(SIZE(singularite%ptzaval(:), 1))
!character(132) :: !arbredappel_old ! ancien arbre d'appel
!============================= Instructions ===========================
! INITIALISATIONS
! ---------------
!arbredappel_old    = trim(!Erreur%arbredappel)
!Erreur%Arbredappel = trim(!Erreur%arbredappel)//'=>PSING'
! CALCUL SUIVANT LE TYPE DE LA SINGULARITE
! ----------------------------------------
  SELECT CASE  (singularite%type)
  CASE (singularite_type_zamont_zaval_q)
! RECHERCHE DES DEBITS ENCADRANT LE DEBIT AMONT
    q1 = singularite%ptq(1)
    q2 = singularite%ptq(SIZE(singularite%ptq))
    IF (qam .LT. q1 .OR. qam .GT. q2) THEN
      erreur%numero = 42
      erreur%ft = err_42
      erreur%ft_c = err_42c
      CALL TRAITER_ERREUR(erreur, singularite%numero, temps, qam, q1, q2&
&                  )
      zavb = 0.D0
    ELSE
      DO iq=1,SIZE(singularite%ptq)-1
        q1 = singularite%ptq(iq)
        q2 = singularite%ptq(iq+1)
        IF (qam .GE. q1 .AND. qam .LE. q2) GOTO 100
      END DO
! INTERPOLATION DE LA COTE AMONT A PARTIR DES DEBITS DE REFERENCE
 100  arg1 = SIZE(singularite%ptzamont(1, :))
      CALL INTERPOLATION_S(zam1, zav, premier_ordre_interpolation, &
&                    singularite%ptzaval(:), singularite%ptzamont(iq, :)&
&                    , arg1, erreur)
      IF (erreur%numero .NE. 0) THEN
        zavb = 0.D0
        zam1b = 0.D0
      ELSE
        CALL PUSHINTEGER4(arg1)
        arg1 = SIZE(singularite%ptzamont(1, :))
        CALL INTERPOLATION_S(zam2, zav, premier_ordre_interpolation, &
&                      singularite%ptzaval(:), singularite%ptzamont(1+iq&
&                      , :), arg1, erreur)
        IF (erreur%numero .NE. 0) THEN
          zam1b = 0.D0
          zam2b = 0.D0
        ELSE
! RESULTAT
          alpha = (qam-q1)/(q2-q1)
          alphab = (zam2-zam1)*zamb
          zam1b = (1._DOUBLE-alpha)*zamb
          zam2b = alpha*zamb
          qamb = qamb + alphab/(q2-q1)
          zamb = 0.D0
        END IF
        dummyzerodiffb1 = 0.D0
        dummyzerodiffb2 = 0.D0
        zavb = 0.D0
        CALL INTERPOLATION_S_B(zam2, zam2b, zav, zavb, &
&                        premier_ordre_interpolation, singularite%&
&                        ptzaval(:), dummyzerodiffb1, singularite%&
&                        ptzamont(1+iq, :), dummyzerodiffb2, arg1, &
&                        erreur)
        CALL POPINTEGER4(arg1)
      END IF
      dummyzerodiffb = 0.D0
      dummyzerodiffb0 = 0.D0
      CALL INTERPOLATION_S_B(zam1, zam1b, zav, zavb, &
&                      premier_ordre_interpolation, singularite%ptzaval(&
&                      :), dummyzerodiffb, singularite%ptzamont(iq, :), &
&                      dummyzerodiffb0, arg1, erreur)
    END IF
  CASE (singularite_type_zamont_q)
! COTE AMONT EN REGIME DENOYE
    arg1 = SIZE(singularite%ptz)
    CALL INTERPOLATION_S(zam, qam, premier_ordre_interpolation, &
&                  singularite%ptq(:), singularite%ptz(:), arg1, erreur)
    IF (erreur%numero .NE. 0) THEN
      zavb = 0.D0
    ELSE
! TEST DU REGIME DENOYE
      charge_amont = zam - singularite%cotecrete
      charge_aval = zav - singularite%cotecrete
      IF (charge_amont .LE. 0._DOUBLE) THEN
        erreur%numero = 43
        erreur%ft = err_43
        erreur%ft_c = err_43c
        CALL TRAITER_ERREUR(erreur, singularite%numero, temps, zam, &
&                     singularite%cotecrete)
        zavb = 0.D0
        charge_amontb = 0.D0
        charge_avalb = 0.D0
      ELSE
        rh = charge_aval/charge_amont
        IF (rh .LE. cdenoy) THEN
          zavb = 0.D0
          charge_amontb = 0.D0
          charge_avalb = 0.D0
        ELSE
! ITERATIONS EN REGIME NOYE
          num_iter = 0
          epsq = eps3*qam
          IF (zam .LT. zav) THEN
            zam = zav
            CALL PUSHCONTROL1B(0)
          ELSE
            zam = zam
            CALL PUSHCONTROL1B(1)
          END IF
          sens = -1
          IF (charge_amont .LT. charge_aval) THEN
            dmax10 = charge_aval
            CALL PUSHCONTROL1B(0)
          ELSE
            dmax10 = charge_amont
            CALL PUSHCONTROL1B(1)
          END IF
          dz = dmax10/pas
          ad_count = 1
 30       zam = zam + dz
          CALL PUSHINTEGER4(arg1)
          arg1 = SIZE(singularite%ptq)
          CALL INTERPOLATION_S(qamont, zam, premier_ordre_interpolation&
&                        , singularite%ptz(:), singularite%ptq(:), arg1&
&                        , erreur)
          IF (erreur%numero .EQ. 0) THEN
            rh = charge_aval/(zam-singularite%cotecrete)
            IF (rh .GT. cdenoy) THEN
              IF (rh .LT. 1._DOUBLE) THEN
                qamont = (c1*rh**3+c2*rh**2+c3*rh+c4)*qamont
              ELSE
                qamont = 0._DOUBLE
              END IF
            END IF
            IF (qam - qamont .GE. 0.) THEN
              dabs0 = qam - qamont
            ELSE
              dabs0 = -(qam-qamont)
            END IF
            IF (dabs0 .LE. epsq) THEN
              GOTO 110
            ELSE
              num_iter = num_iter + 1
              IF (num_iter .GT. nb_iter_max) THEN
                erreur%numero = 44
                erreur%ft = err_44
                erreur%ft_c = err_44c
                CALL TRAITER_ERREUR(erreur, singularite%numero, &
&                             singularite%type, temps)
              END IF
              IF ((qamont .GT. qam .AND. sens .EQ. -1) .OR. (qamont .LT.&
&                 qam .AND. sens .EQ. 1)) THEN
                dz = -(dz/pas)
                sens = -sens
                CALL PUSHCONTROL1B(1)
              ELSE
                CALL PUSHCONTROL1B(0)
              END IF
              ad_count = ad_count + 1
              GOTO 30
            END IF
          END IF
          CALL PUSHCONTROL1B(0)
          CALL PUSHINTEGER4(ad_count)
          GOTO 120
 110      CALL PUSHCONTROL1B(1)
          CALL PUSHINTEGER4(ad_count)
 120      CALL POPINTEGER4(ad_count)
          DO i=1,ad_count
            IF (i .EQ. 1) THEN
              CALL POPCONTROL1B(branch)
              IF (branch .EQ. 0) THEN
                dzb = 0.D0
              ELSE
                dzb = 0.D0
              END IF
            ELSE
              CALL POPCONTROL1B(branch)
              IF (branch .NE. 0) dzb = -(dzb/pas)
            END IF
            CALL POPINTEGER4(arg1)
            dzb = dzb + zamb
          END DO
          dmax10b = dzb/pas
          CALL POPCONTROL1B(branch)
          IF (branch .EQ. 0) THEN
            charge_avalb = dmax10b
            charge_amontb = 0.D0
          ELSE
            charge_amontb = dmax10b
            charge_avalb = 0.D0
          END IF
          CALL POPCONTROL1B(branch)
          IF (branch .EQ. 0) THEN
            zavb = zamb
            zamb = 0.D0
          ELSE
            zavb = 0.D0
          END IF
        END IF
      END IF
      zavb = zavb + charge_avalb
      zamb = zamb + charge_amontb
    END IF
    dummyzerodiffb3 = 0.D0
    dummyzerodiffb4 = 0.D0
    CALL INTERPOLATION_S_B(zam, zamb, qam, qamb, &
&                    premier_ordre_interpolation, singularite%ptq(:), &
&                    dummyzerodiffb3, singularite%ptz(:), &
&                    dummyzerodiffb4, arg1, erreur)
  CASE (singularite_type_profil_crete)
    largeur_seuil = singularite%ptx(SIZE(singularite%ptx)) - singularite&
&     %ptx(1)
    IF (0._DOUBLE .LT. zav - singularite%cotecrete) THEN
      charge_aval = zav - singularite%cotecrete
      CALL PUSHCONTROL1B(0)
    ELSE
      CALL PUSHCONTROL1B(1)
      charge_aval = 0._DOUBLE
    END IF
! ESTIMATION INITIALE
    charge_amont = (qam/(singularite%coeffdebit*largeur_seuil*rdg))**w23
    zam = singularite%cotecrete + charge_amont
    dch = 0._DOUBLE
! RECHERCHE ITERATIVE
    num_iter = 0
    epsq = eps3*qam
    IF (charge_amont .LT. charge_aval) THEN
      dmax11 = charge_aval
      CALL PUSHCONTROL1B(0)
    ELSE
      dmax11 = charge_amont
      CALL PUSHCONTROL1B(1)
    END IF
    dz = dmax11/pas
    IF (zam .LT. zav) THEN
      dmax12 = zav
      CALL PUSHCONTROL1B(0)
    ELSE
      dmax12 = zam
      CALL PUSHCONTROL1B(1)
    END IF
    zam = dmax12 - dz
    ad_count0 = 1
 50 zam = zam + dz
    qamont = 0._DOUBLE
    DO ipoint=1,SIZE(singularite%pty)-1
      dx = singularite%ptx(ipoint+1) - singularite%ptx(ipoint)
      cote_crete = (singularite%pty(ipoint+1)+singularite%pty(ipoint))/&
&       2._DOUBLE
      IF (0._DOUBLE .LT. zam - cote_crete) THEN
        charge = zam - cote_crete
      ELSE
        charge = 0._DOUBLE
      END IF
      IF (charge .GT. 0._DOUBLE) qamont = qamont + singularite%&
&         coeffdebit*dx*rdg*(charge+dch)**w32
    END DO
    CALL PUSHINTEGER4(ipoint - 1)
    rh = charge_aval/(zam-singularite%cotecrete)
    IF (singularite%epaisseur_seuil .EQ. 1) THEN
      IF (rh .GT. cdenoy) THEN
        IF (rh .LT. 1._DOUBLE) THEN
          qamont = (c1*rh**3+c2*rh**2+c3*rh+c4)*qamont
        ELSE
          qamont = 0._DOUBLE
        END IF
      END IF
    ELSE IF (zav - singularite%cotecrete .GE. 0._DOUBLE) THEN
      IF (rh .LT. 1._DOUBLE) THEN
        qamont = (1._DOUBLE-rh**d2)**d1*qamont
      ELSE
        qamont = 0._DOUBLE
      END IF
    END IF
    IF (qam - qamont .GE. 0.) THEN
      dabs1 = qam - qamont
    ELSE
      dabs1 = -(qam-qamont)
    END IF
    IF (dabs1 .GT. epsq) THEN
      num_iter = num_iter + 1
      IF (num_iter .GT. nb_iter_max) THEN
        GOTO 130
      ELSE
        IF (num_iter .EQ. 1) THEN
          IF (qamont .LT. qam) THEN
            CALL PUSHCONTROL3B(4)
            sens = -1
          ELSE IF (qamont .GT. qam) THEN
            sens = 1
            dz = -dz
            CALL PUSHCONTROL3B(3)
          ELSE
            CALL PUSHCONTROL3B(2)
          END IF
        ELSE IF ((qamont .GT. qam .AND. sens .EQ. -1) .OR. (qamont .LT. &
&           qam .AND. sens .EQ. 1)) THEN
          dz = -(dz/pas)
          sens = -sens
          CALL PUSHCONTROL3B(1)
        ELSE
          CALL PUSHCONTROL3B(0)
        END IF
        ad_count0 = ad_count0 + 1
        GOTO 50
      END IF
    END IF
    CALL PUSHCONTROL1B(0)
    CALL PUSHINTEGER4(ad_count0)
    GOTO 140
 130 CALL PUSHCONTROL1B(1)
    CALL PUSHINTEGER4(ad_count0)
    erreur%numero = 44
    erreur%ft = err_44
    erreur%ft_c = err_44c
    CALL TRAITER_ERREUR(erreur, singularite%numero, singularite%type, &
&                 temps)
 140 CALL POPINTEGER4(ad_count0)
    DO i0=1,ad_count0
      IF (i0 .EQ. 1) THEN
        CALL POPCONTROL1B(branch)
        IF (branch .EQ. 0) THEN
          dzb = 0.D0
        ELSE
          dzb = 0.D0
        END IF
      ELSE
        CALL POPCONTROL3B(branch)
        IF (branch .LT. 2) THEN
          IF (branch .NE. 0) dzb = -(dzb/pas)
        ELSE IF (branch .NE. 2) THEN
          IF (branch .EQ. 3) dzb = -dzb
        END IF
      END IF
      dzb = dzb + zamb
    END DO
    dmax12b = zamb
    dzb = dzb - zamb
    CALL POPCONTROL1B(branch)
    IF (branch .EQ. 0) THEN
      zavb = dmax12b
      zamb = 0.D0
    ELSE
      zamb = dmax12b
      zavb = 0.D0
    END IF
    dmax11b = dzb/pas
    CALL POPCONTROL1B(branch)
    IF (branch .EQ. 0) THEN
      charge_avalb = dmax11b
      charge_amontb = 0.D0
    ELSE
      charge_amontb = dmax11b
      charge_avalb = 0.D0
    END IF
    charge_amontb = charge_amontb + zamb
    temp = singularite%coeffdebit*largeur_seuil*rdg
    IF (.NOT.(qam/temp .LE. 0.0 .AND. (w23 .EQ. 0.0 .OR. w23 .NE. INT(&
&       w23)))) qamb = qamb + w23*(qam/temp)**(w23-1)*charge_amontb/temp
    CALL POPCONTROL1B(branch)
    IF (branch .EQ. 0) zavb = zavb + charge_avalb
    zamb = 0.D0
  CASE (singularite_type_crete_coeff)
    CALL RHSBP_SECTION_S(largeur_seuil, zref, singularite%cotecrete, idt&
&                  (section), xdt(section), profil, b1plan, erreur)
    IF (0._DOUBLE .LT. zav - singularite%cotecrete) THEN
      charge_aval = zav - singularite%cotecrete
      CALL PUSHCONTROL1B(0)
    ELSE
      CALL PUSHCONTROL1B(1)
      charge_aval = 0._DOUBLE
    END IF
! ESTIMATION INITIALE
    charge_amont = (qam/(singularite%coeffdebit*largeur_seuil*rdg))**w23
    zam = singularite%cotecrete + charge_amont
    dch = 0._DOUBLE
! RECHERCHE ITERATIVE
    num_iter = 0
    epsq = eps3*qam
    IF (charge_amont .LT. charge_aval) THEN
      dmax13 = charge_aval
      CALL PUSHCONTROL1B(0)
    ELSE
      dmax13 = charge_amont
      CALL PUSHCONTROL1B(1)
    END IF
    dz = dmax13/pas
    IF (zam .LT. zav) THEN
      dmax14 = zav
      CALL PUSHCONTROL1B(0)
    ELSE
      dmax14 = zam
      CALL PUSHCONTROL1B(1)
    END IF
    zam = dmax14 - dz
    ad_count1 = 1
 60 zam = zam + dz
    qamont = 0._DOUBLE
    IF (0._DOUBLE .LT. zam - singularite%cotecrete) THEN
      charge = zam - singularite%cotecrete
    ELSE
      charge = 0._DOUBLE
    END IF
    IF (charge .GT. 0._DOUBLE) qamont = singularite%coeffdebit*&
&       largeur_seuil*rdg*(charge+dch)**w32
    rh = charge_aval/(zam-singularite%cotecrete)
    IF (singularite%epaisseur_seuil .EQ. 1) THEN
      IF (rh .GT. cdenoy) THEN
        IF (rh .LT. 1._DOUBLE) THEN
          qamont = (c1*rh**3+c2*rh**2+c3*rh+c4)*qamont
        ELSE
          qamont = 0._DOUBLE
        END IF
      END IF
    ELSE IF (zav - singularite%cotecrete .GE. 0._DOUBLE) THEN
      IF (rh .LT. 1._DOUBLE) THEN
        qamont = (1._DOUBLE-rh**d2)**d1*qamont
      ELSE
        qamont = 0._DOUBLE
      END IF
    END IF
    IF (qam - qamont .GE. 0.) THEN
      dabs2 = qam - qamont
    ELSE
      dabs2 = -(qam-qamont)
    END IF
    IF (dabs2 .GT. epsq) THEN
      num_iter = num_iter + 1
      IF (num_iter .GT. nb_iter_max) THEN
        GOTO 150
      ELSE
        IF (num_iter .EQ. 1) THEN
          IF (qamont .LT. qam) THEN
            CALL PUSHCONTROL3B(0)
            sens = -1
          ELSE IF (qamont .GT. qam) THEN
            sens = 1
            dz = -dz
            CALL PUSHCONTROL3B(1)
          ELSE
            CALL PUSHCONTROL3B(2)
          END IF
        ELSE IF ((qamont .GT. qam .AND. sens .EQ. -1) .OR. (qamont .LT. &
&           qam .AND. sens .EQ. 1)) THEN
          dz = -(dz/pas)
          sens = -sens
          CALL PUSHCONTROL3B(3)
        ELSE
          CALL PUSHCONTROL3B(4)
        END IF
        ad_count1 = ad_count1 + 1
        GOTO 60
      END IF
    END IF
    CALL PUSHCONTROL1B(0)
    CALL PUSHINTEGER4(ad_count1)
    GOTO 160
 150 CALL PUSHCONTROL1B(1)
    CALL PUSHINTEGER4(ad_count1)
    erreur%numero = 44
    erreur%ft = err_44
    erreur%ft_c = err_44c
    CALL TRAITER_ERREUR(erreur, singularite%numero, singularite%type, &
&                 temps)
 160 CALL POPINTEGER4(ad_count1)
    DO i1=1,ad_count1
      IF (i1 .EQ. 1) THEN
        CALL POPCONTROL1B(branch)
        IF (branch .EQ. 0) THEN
          dzb = 0.D0
        ELSE
          dzb = 0.D0
        END IF
      ELSE
        CALL POPCONTROL3B(branch)
        IF (branch .LT. 2) THEN
          IF (branch .NE. 0) dzb = -dzb
        ELSE IF (branch .NE. 2) THEN
          IF (branch .EQ. 3) dzb = -(dzb/pas)
        END IF
      END IF
      dzb = dzb + zamb
    END DO
    dmax14b = zamb
    dzb = dzb - zamb
    CALL POPCONTROL1B(branch)
    IF (branch .EQ. 0) THEN
      zavb = dmax14b
      zamb = 0.D0
    ELSE
      zamb = dmax14b
      zavb = 0.D0
    END IF
    dmax13b = dzb/pas
    CALL POPCONTROL1B(branch)
    IF (branch .EQ. 0) THEN
      charge_avalb = dmax13b
      charge_amontb = 0.D0
    ELSE
      charge_amontb = dmax13b
      charge_avalb = 0.D0
    END IF
    charge_amontb = charge_amontb + zamb
    temp0 = singularite%coeffdebit*largeur_seuil*rdg
    IF (.NOT.(qam/temp0 .LE. 0.0 .AND. (w23 .EQ. 0.0 .OR. w23 .NE. INT(&
&       w23)))) qamb = qamb + w23*(qam/temp0)**(w23-1)*charge_amontb/&
&       temp0
    CALL POPCONTROL1B(branch)
    IF (branch .EQ. 0) zavb = zavb + charge_avalb
    zamb = 0.D0
  CASE (singularite_type_z_t)
    zamb = 0.D0
    zavb = 0.D0
  CASE (singularite_type_q_zamont)
    arg1 = SIZE(singularite%ptz)
    CALL INTERPOLATION_S(zam, qam, premier_ordre_interpolation, &
&                  singularite%ptq(:), singularite%ptz(:), arg1, erreur)
    dummyzerodiffb5 = 0.D0
    dummyzerodiffb6 = 0.D0
    CALL INTERPOLATION_S_B(zam, zamb, qam, qamb, &
&                    premier_ordre_interpolation, singularite%ptq(:), &
&                    dummyzerodiffb5, singularite%ptz(:), &
&                    dummyzerodiffb6, arg1, erreur)
    zavb = 0.D0
  CASE DEFAULT
    erreur%numero = 46
    erreur%ft = err_46
    erreur%ft_c = err_46c
    CALL TRAITER_ERREUR(erreur, singularite%numero, temps, singularite%&
&                 type)
    zavb = 0.D0
  END SELECT
END SUBROUTINE PSING_B




SUBROUTINE CRITIQ_B(zcrit, zcritb, section, zref, q, qb, cf1, cf1b, cf2&
& , cf2b, idt, xdt, profil, profil_plan, modelelit, loifrottement, &
& unitelisting, erreur)
! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P.CHERUBINI
!                             S. PERON
!                             S. MANDELKERN
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
!
!   FONCTION :
!   --------
!
!   CALCUL DE LA COTE CRITIQUE DANS LA SECTION Section
!
! ----------------------------------------------------------------------
! ARGUMENTS
! .__________________.____._______________________________________________
! !    NOM      !TYPE!MODE!                   ROLE
! !_____________!____!____!______________________________________________
! ! ZCRIT       ! R  !<-- ! COTE CRITIQUE
! ! Section           ! I  ! -->! SECTION DU CALCUL DE LA COTE CRITIQUE
! ! ZREF        ! R  ! -->! COTE DU FOND
! ! Q           ! R  ! -->! DEBIT GLOBAL
! ! CF1         ! R  ! -->! COEF. DE FROTTEMENT , LIT MINEUR
! ! CF2         ! R  ! -->! COEF. DE FROTTEMENT , LIT MAJEUR
! ! IDT         ! I  ! -->!  )PERMET DE DETERMINER LA SECTION DE CALCUL A
! ! XDT         ! R  ! -->!  )PARTIR DES SECTIONS DE DONNEES
! ! Profil      ! T  ! -->! Caracteristiques du planimetrage d'un profil
! ! Profil_plan ! T  ! -->! Variables de profil planimetrees
! ! ModeleLit   ! I  ! -->! Modele du lit
! !_____________!____!____!______________________________________________
!
! VARIABLES LOCALES
! .___________.________________________________________________________
! !           !    !    !
! !___________!____!____!______________________________________________
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!
!   FICHIERS  ENTREE/SORTIE :
!   -----------------------
!
!   SOUS PROGRAMME APPELANT :  PERMAT
!   -------------------------
!   SOUS PROGRAMMES APPELES :
!   -------------------------
!
!   RHSBP_S : CALCUL DES PARAMETRES DE LA GEOMETRIE D'UNE SECTION
!
!   REPAR : CALCUL DE LA REPARTITION DES DEBITS ENTRE LE LIT MINEUR
!           ET LE LIT MAJEUR ACTIF
!
! ----------------------------------------------------------------------
!============================= Declarations ===========================
!.. Modules importes ..
!----------------------
  USE M_PRECISION
! Constantes nommees
  USE M_PARAMETRE_C
! Messages d'erreur
  USE M_MESSAGE_C
! Types derives
! Type PROFIL_T
  USE M_PROFIL_T
! Type PROFIL_PLAN_T
  USE M_PROFIL_PLAN_T
! Type ERREUR_T
  USE M_ERREUR_T
! Procedures-module
! Sous-programme RHSBP_S
  USE M_RHSBP_S_B
  USE M_RHSBP_S
! Traitement des erreurs
  USE M_TRAITER_ERREUR_I
! Interfaces
  USE M_REPAR_I_B
  IMPLICIT NONE
!.. Arguments ..
! --------------
  DOUBLE PRECISION :: zcrit
  DOUBLE PRECISION :: zcritb
  INTEGER, INTENT(IN) :: section
! TABLEAUX DIMENSIONNES A NMSCAL
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: zref
  DOUBLE PRECISION, INTENT(IN) :: q
  DOUBLE PRECISION :: qb
  DOUBLE PRECISION, INTENT(IN) :: cf1
  DOUBLE PRECISION :: cf1b
  DOUBLE PRECISION, INTENT(IN) :: cf2
  DOUBLE PRECISION :: cf2b
  INTEGER, DIMENSION(:), INTENT(IN) :: idt
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: xdt
  TYPE(PROFIL_T), DIMENSION(:), INTENT(IN) :: profil
  TYPE(PROFIL_PLAN_T), INTENT(IN) :: profil_plan
  INTEGER, INTENT(IN) :: modelelit
  INTEGER, INTENT(IN) :: loifrottement
  INTEGER, INTENT(IN) :: unitelisting
  TYPE(ERREUR_T), INTENT(INOUT) :: erreur
!.. Variables locales ..
! ----------------------
  DOUBLE PRECISION :: zk, pas, fcrit1, fcrit2, beta
  DOUBLE PRECISION :: zkb, fcrit1b, fcrit2b, betab
  DOUBLE PRECISION :: b1, b2, s1, s2, p2, p1, rh1, rh2
  DOUBLE PRECISION :: b1b, b2b, s1b, s2b, p2b, p1b, rh1b, rh2b
  DOUBLE PRECISION :: q1, q2, deb, v, bst
  DOUBLE PRECISION :: debb, vb
  INTEGER :: num_profil, ipassage, icompt
  INTEGER :: ad_count
  INTEGER :: i
  INTEGER :: branch
  DOUBLE PRECISION :: tempb3
  DOUBLE PRECISION :: tempb2
  DOUBLE PRECISION :: tempb1
  DOUBLE PRECISION :: tempb0
  DOUBLE PRECISION :: tempb
  DOUBLE PRECISION :: temp
!character(132) :: !arbredappel_old
!============================= Instructions ===========================
! INITIALISATIONS
! ---------------
!arbredappel_old    = trim(!Erreur%arbredappel)
!Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>CRITIQ'
  zk = zref(section)
  num_profil = idt(section)
  pas = profil(num_profil)%pas/2._DOUBLE
  fcrit1 = 100._DOUBLE
  CALL PUSHCONTROL1B(0)
! CALCUL DE COTE CRITIQUE
! -----------------------
label_boucle_n:DO ipassage=0,2
    fcrit2 = 100._DOUBLE
    IF (ipassage .EQ. 1) THEN
      zk = zcrit - pas
      pas = 0.2_DOUBLE*pas
      CALL PUSHCONTROL2B(0)
    ELSE IF (ipassage .EQ. 2) THEN
      zk = zcrit - pas
      pas = 0.01_DOUBLE
      CALL PUSHCONTROL2B(1)
    ELSE
      CALL PUSHCONTROL2B(2)
    END IF
    icompt = 0
    ad_count = 1
    DO WHILE (icompt .LT. 3000 .AND. fcrit2 .GT. 0._DOUBLE)
      icompt = icompt + 1
      CALL PUSHREAL8(zcrit)
      zcrit = zk + icompt*pas
      CALL PUSHREAL8(s2)
      CALL PUSHREAL8(s1)
      CALL PUSHREAL8(p2)
      CALL PUSHREAL8(p1)
      CALL PUSHREAL8(b2)
      CALL PUSHREAL8(b1)
      CALL RHSBP_S(b1, b2, bst, p1, p2, s1, s2, rh1, rh2, section, zcrit&
&            , zref(section), idt, xdt, profil, profil_plan, &
&            unitelisting, erreur)
      IF (erreur%numero .NE. 0) THEN
        GOTO 100
      ELSE
! Resultats
! Donnees modifiees
! Donnees non modifiees
        CALL PUSHREAL8(rh2)
        CALL PUSHREAL8(rh1)
        CALL PUSHREAL8(s2)
        CALL PUSHREAL8(s1)
        CALL PUSHREAL8(beta)
        CALL REPAR(deb, v, beta, q1, q2, s1, s2, rh1, rh2, p1, p2, q&
&               , cf1, cf2, modelelit, loifrottement&
&               , profil(num_profil)%Nom, erreur)
        IF (erreur%numero .NE. 0) THEN
          GOTO 110
        ELSE
          fcrit2 = beta*q**2*(b1+b2)/gpes/(s1+s2)**3 - 1._DOUBLE
          IF (fcrit2 .GT. 0._DOUBLE) THEN
            fcrit1 = fcrit2
            CALL PUSHCONTROL1B(1)
          ELSE
            CALL PUSHCONTROL1B(0)
          END IF
          ad_count = ad_count + 1
        END IF
      END IF
    END DO
    CALL PUSHCONTROL2B(0)
    CALL PUSHINTEGER4(ad_count)
    CALL PUSHCONTROL1B(1)
  END DO label_boucle_n
  tempb2 = pas*zcritb/(fcrit1-fcrit2)
  tempb3 = -(fcrit2*tempb2/(fcrit1-fcrit2))
  fcrit2b = tempb2 - tempb3
  fcrit1b = tempb3
  p1b = 0.D0
  p2b = 0.D0
  betab = 0.D0
  zkb = 0.D0
  GOTO 150
 100 CALL PUSHCONTROL2B(1)
  CALL PUSHINTEGER4(ad_count)
  GOTO 120
 110 CALL PUSHCONTROL2B(2)
  CALL PUSHINTEGER4(ad_count)
 120 CALL POPINTEGER4(ad_count)
  DO 140 i=1,ad_count
    IF (i .EQ. 1) THEN
      CALL POPCONTROL2B(branch)
      IF (branch .EQ. 0) THEN
        GOTO 140
      ELSE IF (branch .EQ. 1) THEN
        fcrit1b = 0.D0
        s1b = 0.D0
        s2b = 0.D0
        p1b = 0.D0
        p2b = 0.D0
        rh1b = 0.D0
        rh2b = 0.D0
        betab = 0.D0
        b1b = 0.D0
        b2b = 0.D0
        zkb = 0.D0
        GOTO 130
      ELSE
        fcrit1b = 0.D0
        s1b = 0.D0
        s2b = 0.D0
        p1b = 0.D0
        p2b = 0.D0
        betab = 0.D0
        b1b = 0.D0
        b2b = 0.D0
        zkb = 0.D0
      END IF
    ELSE
      CALL POPCONTROL1B(branch)
      IF (branch .NE. 0) THEN
        fcrit2b = fcrit2b + fcrit1b
        fcrit1b = 0.D0
      END IF
      temp = gpes*(s1+s2)**3
      tempb = fcrit2b/temp
      tempb0 = q**2*tempb
      tempb1 = -(gpes*q**2*beta*(b1+b2)*3*(s1+s2)**2*tempb/temp)
      qb = qb + beta*(b1+b2)*2*q*tempb
      betab = betab + (b1+b2)*tempb0
      b1b = beta*tempb0
      b2b = beta*tempb0
      s1b = tempb1
      s2b = tempb1
    END IF
    CALL POPREAL8(beta)
    CALL POPREAL8(s1)
    CALL POPREAL8(s2)
    CALL POPREAL8(rh1)
    CALL POPREAL8(rh2)
    debb = 0.D0
    vb = 0.D0
    CALL REPAR_B(deb, debb, v, vb, beta, betab, q1, q2, s1, s1b, s2, s2b&
&          , rh1, rh1b, rh2, rh2b, p1, p1b, p2, p2b, q, qb, cf1, cf1b, &
&          cf2, cf2b, modelelit, loifrottement, erreur)
 130 CALL POPREAL8(b1)
    CALL POPREAL8(b2)
    CALL POPREAL8(p1)
    CALL POPREAL8(p2)
    CALL POPREAL8(s1)
    CALL POPREAL8(s2)
    CALL RHSBP_S_B(b1, b1b, b2, b2b, bst, p1, p1b, p2, p2b, s1, s1b, s2&
&            , s2b, rh1, rh1b, rh2, rh2b, section, zcrit, zcritb, zref(&
&            section), idt, xdt, profil, profil_plan, unitelisting, &
&            erreur)
    CALL POPREAL8(zcrit)
    zkb = zkb + zcritb
    zcritb = 0.D0
    fcrit2b = 0.D0
 140 CONTINUE
  CALL POPCONTROL2B(branch)
  IF (branch .EQ. 0) THEN
    zcritb = zcritb + zkb
    zkb = 0.D0
  ELSE IF (branch .EQ. 1) THEN
    zcritb = zcritb + zkb
    zkb = 0.D0
  END IF
  fcrit2b = 0.D0
 150 CALL POPCONTROL1B(branch)
  IF (branch .NE. 0) GOTO 120
END SUBROUTINE CRITIQ_B





SUBROUTINE PERMAT_B(z, zb, q, qb, zinit, zinitb, x, zref, cf1, cf1b, cf2&
& , cf2b, pcsing, pcsingb, idt, xdt, profil, profilplan, f1, connect, &
& numbief, nbsect, singularite, modelelit, impression, &
& unitelisting, temps, loifrottement, cqmv, decentrement, erreur)
!/ERREUR/)
! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P.CHERUBINI
!                             S. PERON
!                             S. MANDELKERN
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
!   FONCTION :
!   --------
!
!   CALCUL DE LIGNE D'EAU DANS UN BIEF EN REGIME PERMANENT
!   MODELISATION UNIDIMENSIONNELLE
!   LITS MINEUR ET MAJEUR - HYPOTHESES DEBORD OU PRADO
!   SINGULARITES
!
! ----------------------------------------------------------------------
!
! VARIABLES LOCALES
! ._________________.____.____.______________________________________________
! !  JS             ! R  ! -- ! ) PERTES DE CHARGE
! !  JAV            ! R  ! -- ! )      "
! !  JAVAM          ! R  ! -- ! )      "
! !  JAVC           ! R  ! -- ! )      "
! !  ZAM1,ZAM2      ! R  ! -- ! ITERES DE LA COTE AMONT DU BIEF DE CALCUL
! !  ZAV            ! R  ! -- ! COTE AVAL DU BIEF DE CALCUL
! !  ZCRIT          ! R  ! -- ! COTE CRITIQUE AU POINT AMONT DU BIEF DE CALCUL
! !  FRAV           ! R  ! -- ! VALEUR DE FROUDE
! !  FRAM           ! R  ! -- !         "
! !  BETAAM         ! R  ! -- ! REPARTITION DU DEBIT LIT MINEUR - LIT MAJEUR
! !  BETAAV         ! R  ! -- !         "
! !  BETAC          ! R  ! -- !         "
! !  DEBAM          ! R  ! -- ! DEBITANCE
! !  DEBAV          ! R  ! -- !         "
! !  DEBC           ! R  ! -- !         "
! !                 !    !    !
! !  CPCS           ! R  ! -- ! COEFFICIENT DE PERTE DE CHARGE SINGULIERE
! !                 !    !    ! POUR LES ELARGISSEMENTS
! !  FROUD1         ! R  ! -- ! VALEUR TEST DU FROUDE POUR LE PASSAGE EN C.
! !  FROUD2         ! R  ! -- ! VALEUR TEST DU FROUDE POUR LE PASSAGE EN T.
! !  DFROUD         ! R  ! -- ! VALEUR TEST DE VAR. MAX. DU FROUDE
! !  ITMAX1         ! I  ! -- ! NOMBRE MAXIMUM D'ITERATIONS POUR LE CALCUL DE
! !                 !    !    ! LA LIGNE D'EAU
! !_________________!____!____!______________________________________________
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
! ----------------------------------------------------------------------
!
!   FICHIERS  ENTREE/SORTIE :
!   -----------------------
!
!   UniteListing   : IMPRESSION DES RESULTATS GLOBAUX
!
!   SOUS PROGRAMMES APPELANTS :  PERSAR
!   ---------------------------
!   SOUS PROGRAMMES APPELES :
!   -------------------------
!
!   CRITIQ : CALCUL DE LA COTE CRITIQUE
!   FROUDE : CALCUL DU NOMBRE DE FROUDE
!   PSING  : TRAITEMENT DES SINGULARITES EN PERMANENT
!   REPAR  : CALCUL DE LA REPARTITION DES DEBITS ENTRE LE LIT MINEUR
!            ET LE LIT MAJEUR ACTIF, HYPOTHESES 'DEBORD'
!   RHSBP_S: CALCUL DES PARAMETRES DE LA GEOMETRIE D'UNE SECTION
!
!   COMMENTAIRES :
!   ------------
!
!   LE TERME REPRESENTANT LES PERTES DE CHARGE REGULIERES EST PRIS
!   SELON LA MOYENNE HARMONIQUE DES
!   COEFFICIENTS CALCULES DANS CHACUNE DES SECTIONS AMONT ET AVAL:
!                  2      1     1
!                ----- = --- + ---
!                JAVAM   JAV   JAM
!
!   LES PERTES DE CHARGE SINGULIERES SONT :
!      - RALENTISSEMENT : JS=CPCS*(BETAAM*VAM-BETAAV*VAV)**2/2./G
!      - OBSTACLE  EN A : JS=PCSing(A)*BETAAM*VAM**2/2./G
!
!   LE CALCUL EST SUPPOSE SE FAIRE EN REGIME FLUVIAL.
!   TOUTEFOIS SI AU COURS DES ITERATIONS ON DETECTE UN REGIME FLUVIAL
!   A L'AVAL ET TORRENTIEL A L'AMONT , ON IMPOSE ALORS Z=ZCRITIQUE A
!   LA SECTION AMONT , CECI TANT QU'ON N'A PAS RETROUVE UN ECOULEMENT
!   FLUVIAL. DE PLUS UN TEST EST REALISE POUR VERIFIER QUE CE PASSAGE
!   FLUVIAL AMONT - CRITIQUE AVAL N'EST PAS TROP BRUTAL
!
! ----------------------------------------------------------------------
!============================= Declarations ===========================
!.. Modules importes ..
!----------------------
  USE M_PRECISION
! Constantes nommees
! GPES
  USE M_PARAMETRE_C
! Messages d'erreur
  USE M_MESSAGE_C
! Types derives
! Type PROFIL_T
  USE M_PROFIL_T
! Type PROFIL_PLAN_T
  USE M_PROFIL_PLAN_T
! Type SINGULARITE_T
  USE M_SINGULARITE_T
! Type EREUR_T
  USE M_ERREUR_T
! Type CONNECT_T
  USE M_CONNECT_T
! Procedures-module
! Sous-programme RHSBP_S
  USE M_RHSBP_S
  USE M_RHSBP_S_B
! Sous programme RHSBP1
  USE M_RHSB1_S
! Numero de bief d'une section
  USE M_NUM_BIEF_S
! Calcul du nombre de Froude
  USE M_FROUDE_S
! Interface generique de traitement des erreurs
  USE M_TRAITER_ERREUR_I
! Interfaces
  USE M_REPAR_I
  USE M_REPAR_I_B
  USE M_PSING_I_B
  USE M_CRITIQ_I
  USE M_CRITIQ_I_B
  IMPLICIT NONE
!.. Arguments ..
!---------------
! TABLEAU  DIMENSIONNE  A NbSect
  DOUBLE PRECISION, DIMENSION(:),INTENT(OUT) :: z
  DOUBLE PRECISION, DIMENSION(:) :: zb
! TABLEAU  DIMENSIONNE  A NbSect
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: q
  DOUBLE PRECISION, DIMENSION(:) :: qb
  DOUBLE PRECISION, INTENT(IN) :: zinit
  DOUBLE PRECISION :: zinitb
! TABLEAUX DIMENSIONNES A NMSCAL
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: x
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: zref
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf1
  DOUBLE PRECISION, DIMENSION(:) :: cf1b
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf2
  DOUBLE PRECISION, DIMENSION(:) :: cf2b
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: pcsing
  DOUBLE PRECISION, DIMENSION(:) :: pcsingb
  INTEGER, DIMENSION(:), INTENT(IN) :: idt
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: xdt
  TYPE(PROFIL_T), DIMENSION(:), INTENT(IN) :: profil
  TYPE(PROFIL_PLAN_T), INTENT(IN) :: profilplan
  DOUBLE PRECISION, DIMENSION(:, :), INTENT(IN) :: f1
  INTEGER, INTENT(IN) :: numbief
  TYPE(CONNECT_T), INTENT(IN) :: connect
  TYPE(SINGULARITE_T), DIMENSION(:), INTENT(IN) :: singularite
  INTEGER, INTENT(IN) :: modelelit
  LOGICAL, INTENT(IN) :: impression
  INTEGER, INTENT(IN) :: unitelisting
  DOUBLE PRECISION, INTENT(IN) :: temps
  INTEGER, INTENT(IN) :: loifrottement
  INTEGER, INTENT(IN) :: nbsect
  INTEGER, INTENT(IN) :: cqmv
  LOGICAL, INTENT(IN) :: decentrement
  TYPE(ERREUR_T), INTENT(INOUT) :: erreur
!.. Constantes ..
!----------------
  DOUBLE PRECISION, PARAMETER :: cpcs=0.3_DOUBLE
!   real(DOUBLE), parameter :: CQMV   = 0.0_DOUBLE
  DOUBLE PRECISION, PARAMETER :: froud1=0.95_DOUBLE
  DOUBLE PRECISION, PARAMETER :: froud2=1.05_DOUBLE
  DOUBLE PRECISION, PARAMETER :: dfroud=0.5_DOUBLE
  INTEGER, PARAMETER :: itmax1=20
!.. Variables locales ..
!-----------------------
  INTEGER, DIMENSION(:) :: idebtor(nbsect), kpass(nbsect)
  DOUBLE PRECISION, DIMENSION(:) :: zc(nbsect), fimp(nbsect)
  DOUBLE PRECISION :: zcb(nbsect)
  INTEGER :: izone, nb_zone
  DOUBLE PRECISION :: jav, js, javam, javc
  DOUBLE PRECISION :: javb, jsb, javamb
  DOUBLE PRECISION :: zam1, zam2, zav, zam, zam3, &
& zam22, zam21, zmil
  DOUBLE PRECISION :: zam1b, zam2b, zavb, zam22b, zam21b, zmilb
  DOUBLE PRECISION :: dx, dq, ypmil, yp21, dz
  DOUBLE PRECISION :: dqb
  DOUBLE PRECISION :: cqmvj
  DOUBLE PRECISION :: y
  DOUBLE PRECISION :: debav
  DOUBLE PRECISION :: debavb
  DOUBLE PRECISION :: hav
  DOUBLE PRECISION :: betaav
  DOUBLE PRECISION :: betaavb
  DOUBLE PRECISION :: vav
  DOUBLE PRECISION :: vavb
  DOUBLE PRECISION :: vav2
  DOUBLE PRECISION :: vav2b
  DOUBLE PRECISION :: h
  DOUBLE PRECISION :: sav, sm1, sm2
  DOUBLE PRECISION :: savb, sm1b, sm2b
  DOUBLE PRECISION :: b1, b2
  DOUBLE PRECISION :: b1b, b2b
  DOUBLE PRECISION :: bstock
  DOUBLE PRECISION :: ss1, ss2
  DOUBLE PRECISION :: ss1b, ss2b
  DOUBLE PRECISION :: rh1, rh2
  DOUBLE PRECISION :: rh1b, rh2b
  DOUBLE PRECISION :: frav, fram
  DOUBLE PRECISION :: epsil
  DOUBLE PRECISION :: sc
  DOUBLE PRECISION :: debc
  DOUBLE PRECISION :: hc
  DOUBLE PRECISION :: zcrit
  DOUBLE PRECISION :: zcritb
  DOUBLE PRECISION :: betac
  DOUBLE PRECISION :: vc
  DOUBLE PRECISION :: vc2
  DOUBLE PRECISION :: dxbeta
  DOUBLE PRECISION :: dxbetab
  DOUBLE PRECISION :: dxq
  DOUBLE PRECISION :: dxqb
  DOUBLE PRECISION :: ham
  DOUBLE PRECISION :: sam
  DOUBLE PRECISION :: samb
  DOUBLE PRECISION :: debam
  DOUBLE PRECISION :: debamb
  DOUBLE PRECISION :: betaam
  DOUBLE PRECISION :: betaamb
  DOUBLE PRECISION :: vam
  DOUBLE PRECISION :: vamb
  DOUBLE PRECISION :: vam2
  DOUBLE PRECISION :: vam2b
  DOUBLE PRECISION :: dxv
  DOUBLE PRECISION :: dxvb
  DOUBLE PRECISION :: xp, yp
  DOUBLE PRECISION :: xpb, ypb
  DOUBLE PRECISION :: xq, yq
  DOUBLE PRECISION :: xqb, yqb
  DOUBLE PRECISION :: ymin, ymax
  DOUBLE PRECISION :: yminb, ymaxb
  DOUBLE PRECISION :: xmin, xmax
  DOUBLE PRECISION :: xminb, xmaxb
  DOUBLE PRECISION :: xm, ym
  DOUBLE PRECISION :: xmb, ymb
  DOUBLE PRECISION :: q1, q2
  DOUBLE PRECISION :: p1, p2
  DOUBLE PRECISION :: p1b, p2b
  INTEGER :: j, iecrit, iter1
  INTEGER :: debut_bief
  INTEGER :: fin_bief
! Compteur sur les singularites
  INTEGER :: ising
  INTEGER :: num_bief
  DOUBLE PRECISION :: absc_rel
!  CHARACTER(len=132) :: arbredappel_old
  INTRINSIC TRIM
  INTRINSIC SIGN
  INTRINSIC SIZE
  INTRINSIC DABS
  INTRINSIC DMIN1
  INTRINSIC DMAX1
  INTEGER :: arg1
  INTEGER :: ad_count
  INTEGER :: i
  INTEGER :: branch
  INTEGER :: ad_count0
  INTEGER :: i0
  INTEGER :: ad_count1
  INTEGER :: i1
  INTEGER :: ad_count2
  INTEGER :: i2
  INTEGER :: ad_count3
  INTEGER :: i3
  DOUBLE PRECISION :: temp3
  DOUBLE PRECISION :: temp2
  DOUBLE PRECISION :: temp1
  DOUBLE PRECISION :: temp0
  DOUBLE PRECISION :: tempb9
  DOUBLE PRECISION :: tempb8
  DOUBLE PRECISION :: tempb7
  DOUBLE PRECISION :: tempb6
  DOUBLE PRECISION :: tempb5
  DOUBLE PRECISION :: tempb4
  DOUBLE PRECISION :: tempb3
  DOUBLE PRECISION :: tempb2
  DOUBLE PRECISION :: tempb1
  DOUBLE PRECISION :: tempb0
  DOUBLE PRECISION :: tempb12
  DOUBLE PRECISION :: tempb11
  DOUBLE PRECISION :: tempb10
  DOUBLE PRECISION :: tempb
  DOUBLE PRECISION :: dabs4
  DOUBLE PRECISION :: dabs3
  DOUBLE PRECISION :: dabs2
  DOUBLE PRECISION :: dabs1
  DOUBLE PRECISION :: dabs0
  DOUBLE PRECISION :: temp
!============================= Instructions ===========================
! INITIALISATIONS
! ---------------
  erreur%arbredappel = TRIM(erreur%arbredappel)//'=>PERMAT'
  iecrit = 0
  debut_bief = connect%originebief(numbief)
  fin_bief = connect%finbief(numbief)
!
!  Prise compte des apports de debit dans la qte de mvt
!
  IF (cqmv .EQ. 0) THEN
    cqmvj = 0.d0
  ELSE
    cqmvj = 1.d0
  END IF
!
! CONDITION INITIALE A L'AVAL
! ------------ Balayage d'AMONT EN AVAL DES ZONES FLUVIALES ---------------
!
  j = fin_bief
  izone = 0
  CALL PUSHREAL8(z(j))
  z(j) = zinit
  zam1 = zinit
! 1ER  TEST SUR LA CONDITION AVAL DU BIEF :
! COHERENCE AVEC LA COTE DES FONDS A L'AVAL
! -----------------------------------------
  IF (z(j) .LT. zref(j)) THEN
    erreur%numero = 37
    erreur%ft = err_37
    erreur%ft_c = err_37c
    CALL TRAITER_ERREUR(erreur, temps, numbief, z(j), zref(j))
    zam1b = 0.D0
  ELSE
    ad_count1 = 1
! ELEMENT DE LA PREMIERE SECTION DE CALCUL AVAL = AV
! --------------------------------------------------
 100 iter1 = -1
    CALL PUSHREAL8(zav)
    zav = zam1
    CALL PUSHREAL8(dx)
    dx = x(j) - x(j-1)
    CALL PUSHREAL8(dq)
    dq = q(j) - q(j-1)
!   CQMVJ = CQMV
    CALL PUSHREAL8(p2)
    CALL PUSHREAL8(p1)
    CALL RHSBP_S(b1, b2, bstock, p1, p2, sm1, sm2, rh1, rh2, j, zav, &
&          zref(j), idt, xdt, profil, profilplan, unitelisting, erreur)
    IF (erreur%numero .EQ. 0) THEN
      y = zav - zref(j)
      IF (y .LT. eps6) THEN
        GOTO 170
      ELSE
! Resultats
! Donnees modifiees
! Donnees non modifiees
! Erreur
        CALL PUSHREAL8(rh2)
        CALL PUSHREAL8(rh1)
        CALL PUSHREAL8(sm2)
        CALL PUSHREAL8(sm1)
        CALL PUSHREAL8(betaav)
        CALL PUSHREAL8(vav)
        CALL PUSHREAL8(debav)
        CALL REPAR(debav, vav, betaav, q1, q2, sm1, sm2, rh1, rh2, p1&
&               , p2, q(j), cf1(j), cf2(j), modelelit, loifrottement&
&               , profil(idt(j))%Nom, erreur)
        IF (erreur%numero .NE. 0) THEN
          GOTO 180
        ELSE
          CALL PUSHREAL8(sav)
          sav = sm1 + sm2
          jav = (q(j)/debav)**2
          CALL PUSHREAL8(jav)
          jav = SIGN(jav, q(j))
          vav2 = vav**2
          hav = zav + 0.5_DOUBLE*betaav*vav2/gpes
          h = (sm1+sm2)/(b1+b2)
          CALL FROUDE_S(frav, betaav, vav, h, j, connect, x, erreur)
          IF (erreur%numero .NE. 0) THEN
            GOTO 190
          ELSE
! PASSAGE A LA SECTION AMONT = AM
! -------------------------------
            CALL PUSHINTEGER4(j)
            j = j - 1
            epsil = eps3*profil(idt(j))%pas
! TEST DE PRESENCE D'UNE SINGULARITE
! ----------------------------------
            IF (SIZE(singularite) .NE. 0) THEN
              CALL PUSHINTEGER4(ising)
              ad_count = 1
              DO ising=1,SIZE(singularite)
                IF (singularite(ising)%section .EQ. j) THEN
                  GOTO 110
                ELSE
                  CALL PUSHINTEGER4(ising)
                  ad_count = ad_count + 1
                END IF
              END DO
              CALL PUSHCONTROL1B(0)
              CALL PUSHINTEGER4(ad_count)
              CALL PUSHCONTROL1B(0)
              GOTO 120
 110          CALL PUSHCONTROL1B(1)
              CALL PUSHINTEGER4(ad_count)
!/RESULTATS/
!/DONNEES NON MODIFIEES/
!/Erreur/
              CALL PUSHCHARACTERARRAY(erreur%message, 400)
              CALL PUSHCHARACTERARRAY(erreur%arbredappel, 132)
              CALL PUSHREAL8(zam2)
              CALL PSING(zam2, singularite(ising), zref(j), zav, q(j)&
&                     , profil, profilplan%b1, idt, xdt, j, temps, &
&                     erreur)
              IF (erreur%numero .NE. 0) THEN
                GOTO 210
              ELSE
                CALL PUSHCONTROL2B(3)
                GOTO 300
              END IF
            ELSE
              CALL PUSHCONTROL1B(1)
            END IF
! TEST DU PASSAGE EN REGIME TORRENTIEL
! ------------------------------------
! /RESULTATS/
! /DONNEES NON MODIFIEES/
! /DONNEES NON MODIFIEES
!  (ARGUMENTS DE S.P APPELES)/
! Erreur
 120        CALL PUSHCHARACTERARRAY(erreur%message, 400)
            CALL PUSHCHARACTERARRAY(erreur%arbredappel, 132)
            CALL PUSHREAL8(zcrit)
            CALL CRITIQ(zcrit, j, zref, q(j), cf1(j), cf2(j), idt, &
&                    xdt, profil, profilplan, modelelit, loifrottement, &
&                    unitelisting, erreur)
            zc(j) = zcrit
            IF (erreur%numero .NE. 0) THEN
              GOTO 220
            ELSE
              CALL PUSHREAL8(p2)
              CALL PUSHREAL8(p1)
              CALL RHSBP_S(b1, b2, bstock, p1, p2, ss1, ss2, rh1, rh2, j&
&                    , zcrit, zref(j), idt, xdt, profil, profilplan, &
&                    unitelisting, erreur)
              IF (erreur%numero .NE. 0) THEN
                GOTO 230
              ELSE
                y = zcrit - zref(j)
                IF (y .LT. eps6) THEN
                  GOTO 240
                ELSE
! Resultats
! Donnees modifiees
! Donnees non modifiees
                  CALL REPAR(debc, vc, betac, q1, q2, ss1, ss2, rh1, &
&                         rh2, p1, p2, q(j), cf1(j), cf2(j), modelelit, &
&                         loifrottement, profil(idt(j))%Nom, erreur)
                  IF (erreur%numero .NE. 0) THEN
                    GOTO 250
                  ELSE
                    sc = ss1 + ss2
                    javc = q(j)**2/(0.5_DOUBLE*(debav**2+debc**2))
                    javc = SIGN(javc, q(j))
                    vc2 = vc**2
                    hc = zcrit + 0.5_DOUBLE*betac*vc2/gpes
                    dxbeta = 0.25_DOUBLE*(betaav-betac)*(vav2+vc2)/gpes
                    dxq = 0.5_DOUBLE*dq/gpes*((betaav-cqmvj)*vav/sav+(&
&                     betac-cqmvj)*vc/sc)
!----------------------------------------------------
! PERTE DE CHARGE SINGULIERE LIEE A UN RALENTISSEMENT
!----------------------------------------------------
                    IF (betac*vc .GT. betaav*vav .AND. pcsing(j+1) .LT. &
&                       eps2) THEN
                      js = 0.5_DOUBLE*cpcs*(betac*vc-betaav*vav)**2/gpes
                    ELSE
                      js = 0._DOUBLE
                    END IF
!-----------------------------------------------
! PERTE DE CHARGE SINGULIERE LIEE A UN EVENEMENT
!-----------------------------------------------
                    IF (vc .GT. 0._DOUBLE) THEN
                      js = js + 0.5_DOUBLE*pcsing(j+1)*betac*vc2/gpes
                    ELSE
                      js = js - 0.5_DOUBLE*pcsing(j+1)*betaav*vav2/gpes
                    END IF
                    ham = hav + javc*dx + js + dxbeta + dxq
                    IF (hc .GE. ham) THEN
                      IF (izone .EQ. 0) THEN
                        izone = 1
                        kpass(izone) = 1
                        idebtor(izone) = j
                      ELSE IF (j .LT. -1 + idebtor(izone)) THEN
                        izone = izone + 1
                        idebtor(izone) = j
                        kpass(izone) = 1
                      ELSE
                        kpass(izone) = kpass(izone) + 1
                        idebtor(izone) = j
                      END IF
                      zc(j) = zcrit
!
! DETECTION D'UN RESSAUT HYDRAULIQUE OU D'UNE CHUTE
!
                      zam2 = zcrit
                      IF (impression .AND. iecrit .EQ. 0) THEN
                        CALL PUSHCONTROL2B(0)
                        WRITE(unitelisting, 2000) debut_bief, q(&
&                       debut_bief), fin_bief, z(fin_bief)
                      ELSE
                        CALL PUSHCONTROL2B(0)
                      END IF
                      iecrit = 1
                      IF (impression) THEN
                        num_bief = NUM_BIEF_S(connect, j, erreur)
                        absc_rel = x(j) - x(connect%originebief(num_bief&
&                         ))
                        WRITE(unitelisting, 2030) j, num_bief, absc_rel&
&                       , zam2
                      END IF
                    ELSE
! COTE CHOISIE EN AMONT A PRIORI : ZAM1
                      zam1 = zav + jav*dx
                      ad_count0 = 1
! DEMARRAGE DE L'ALGORITHME ITERATIF VISANT A OBTENIR ZAM2 = ZAM1
! ---------------------------------------------------------------
 200                  iter1 = iter1 + 1
!-------------------------------------------------------
! Calcul des grandeurs hydrauliques correspondant a ZAM1
!-------------------------------------------------------
                      CALL PUSHREAL8(p2)
                      CALL PUSHREAL8(p1)
                      CALL RHSBP_S(b1, b2, bstock, p1, p2, ss1, ss2, rh1&
&                            , rh2, j, zam1, zref(j), idt, xdt, profil, &
&                            profilplan, unitelisting, erreur)
                      IF (erreur%numero .NE. 0) THEN
                        GOTO 260
                      ELSE
!----------------------------
! Test de hauteur d'eau nulle
!----------------------------
                        y = zam1 - zref(j)
                        IF (y .LT. eps6) THEN
                          GOTO 270
                        ELSE
!---------------------------------
! Calcul de la debitance pour ZAM1
!---------------------------------
! Resultats
! Donnees modifiees
! Donnees non modifiees
                          CALL PUSHREAL8(rh2)
                          CALL PUSHREAL8(rh1)
                          CALL PUSHREAL8(ss2)
                          CALL PUSHREAL8(ss1)
                          CALL PUSHREAL8(betaam)
                          CALL PUSHREAL8(vam)
                          CALL PUSHREAL8(debam)
                          CALL REPAR(debam, vam, betaam, q1, q2, ss1&
&                                 , ss2, rh1, rh2, p1, p2, q(j), cf1(j)&
&                                 , cf2(j), modelelit, loifrottement&
&                                 , profil(idt(j))%Nom, erreur)
                          IF (erreur%numero .NE. 0) THEN
                            GOTO 280
                          ELSE
                            CALL PUSHREAL8(sam)
                            sam = ss1 + ss2
                            javam = SIGN(q(j)**2, q(j))/(0.5_DOUBLE*(&
&                             debav**2+debam**2))
                            CALL PUSHREAL8(vam2)
                            vam2 = vam**2
!---------------------
! termes de l'equation
!---------------------
                            dxbeta = 0.25_DOUBLE*(betaav-betaam)*(vav2+&
&                             vam2)/gpes
                            dxq = 0.5_DOUBLE*dq/gpes*((betaav-cqmvj)*vav&
&                             /sav+(betaam-cqmvj)*vam/sam)
                            dxv = 0.5_DOUBLE*(betaav*vav2-betaam*vam2)/&
&                             gpes
!----------------------------------------------------
! PERTE DE CHARGE SINGULIERE LIEE A UN RALENTISSEMENT
!----------------------------------------------------
                            IF (betaam*vam .GT. betaav*vav .AND. pcsing(&
&                               j+1) .LT. eps2) THEN
                              js = 0.5_DOUBLE*cpcs*(betaam*vam-betaav*&
&                               vav)**2/gpes
                              CALL PUSHCONTROL1B(0)
                            ELSE
                              CALL PUSHCONTROL1B(1)
                              js = 0._DOUBLE
                            END IF
!-----------------------------------------------
! PERTE DE CHARGE SINGULIERE LIEE A UN EVENEMENT
!-----------------------------------------------
                            IF (vam .GT. 0._DOUBLE) THEN
                              js = js + 0.5_DOUBLE*pcsing(j+1)*betaam*&
&                               vam2/gpes
                              CALL PUSHCONTROL1B(0)
                            ELSE
                              js = js - 0.5_DOUBLE*pcsing(j+1)*betaav*&
&                               vav2/gpes
                              CALL PUSHCONTROL1B(1)
                            END IF
                            h = (ss1+ss2)/(b1+b2)
                            CALL FROUDE_S(fram, betaam, vam, h, j, &
&                                   connect, x, erreur)
                            IF (erreur%numero .NE. 0) THEN
                              GOTO 290
                            ELSE
!------------------------------------------
! COTE OBTENUE EN AMONT A POSTERIORI : ZAM2
!------------------------------------------
                              zam2 = zav + javam*dx + js + dxbeta + dxq &
&                               + dxv
!----------------------------------
! PASSAGE EN TORRENTIEL NON DETECTE
!----------------------------------
                              IF (fram .LT. 1.d0) THEN
! CHOIX DES POINTS P,Q A LA BASE DE L'INTERPOLATION
                                IF (iter1 .EQ. 0) THEN
                                  xp = zam1
                                  yp = zam2 - zam1
                                  xq = zam2
                                  IF (yp .GE. 0.) THEN
                                    dabs0 = yp
                                  ELSE
                                    dabs0 = -yp
                                  END IF
                                  IF (dabs0 .LE. epsil) THEN
                                    GOTO 130
                                  ELSE
                                    CALL PUSHREAL8(zam1)
                                    zam1 = zam2
                                    CALL PUSHCONTROL1B(1)
                                  END IF
                                ELSE
                                  IF (iter1 .EQ. 1) THEN
                                    yq = zam2 - zam1
                                    IF (yq .GE. 0.) THEN
                                      dabs1 = yq
                                    ELSE
                                      dabs1 = -yq
                                    END IF
                                    IF (dabs1 .LE. epsil) THEN
                                      GOTO 140
                                    ELSE
                                      IF (yp .GT. yq) THEN
                                        CALL PUSHREAL8(ymin)
                                        ymin = yq
                                        CALL PUSHCONTROL1B(0)
                                      ELSE
                                        CALL PUSHREAL8(ymin)
                                        ymin = yp
                                        CALL PUSHCONTROL1B(1)
                                      END IF
                                      IF (yp .LT. yq) THEN
                                        CALL PUSHREAL8(ymax)
                                        ymax = yq
                                        CALL PUSHCONTROL1B(0)
                                      ELSE
                                        CALL PUSHREAL8(ymax)
                                        ymax = yp
                                        CALL PUSHCONTROL1B(1)
                                      END IF
                                      IF (ymax .EQ. yp) THEN
                                        CALL PUSHREAL8(xmin)
                                        xmin = xq
                                        CALL PUSHREAL8(xmax)
                                        xmax = xp
                                        CALL PUSHCONTROL3B(0)
                                      ELSE
                                        CALL PUSHREAL8(xmin)
                                        xmin = xp
                                        CALL PUSHREAL8(xmax)
                                        xmax = xq
                                        CALL PUSHCONTROL3B(1)
                                      END IF
                                    END IF
                                  ELSE
                                    ym = zam2 - zam1
                                    IF (ym .GE. 0.) THEN
                                      dabs2 = ym
                                    ELSE
                                      dabs2 = -ym
                                    END IF
                                    IF (dabs2 .LE. epsil) THEN
                                      GOTO 150
                                    ELSE IF (ymin*ymax .LE. 0._DOUBLE) &
&                                   THEN
                                      IF (ym .LE. 0._DOUBLE) THEN
                                        CALL PUSHREAL8(xmin)
                                        xmin = xm
                                        CALL PUSHREAL8(ymin)
                                        ymin = ym
                                        CALL PUSHCONTROL3B(2)
                                      ELSE
                                        CALL PUSHREAL8(xmax)
                                        xmax = xm
                                        CALL PUSHREAL8(ymax)
                                        ymax = ym
                                        CALL PUSHCONTROL3B(3)
                                      END IF
                                    ELSE IF (ymax .LT. 0._DOUBLE) THEN
                                      CALL PUSHREAL8(xmin)
                                      xmin = xmax
                                      CALL PUSHREAL8(ymin)
                                      ymin = ymax
                                      CALL PUSHREAL8(xmax)
                                      xmax = xm
                                      CALL PUSHREAL8(ymax)
                                      ymax = ym
                                      CALL PUSHCONTROL3B(4)
                                    ELSE
                                      CALL PUSHREAL8(xmax)
                                      xmax = xmin
                                      CALL PUSHREAL8(ymax)
                                      ymax = ymin
                                      CALL PUSHREAL8(xmin)
                                      xmin = xm
                                      CALL PUSHREAL8(ymin)
                                      ymin = ym
                                      CALL PUSHCONTROL3B(5)
                                    END IF
                                  END IF
! INTERPOLATION DE LAGRANGE : NOUVEAU POINT DE COORDONNEE XM
                                  IF (ymin*ymax .GT. 0._DOUBLE) THEN
                                    IF (ymax - ymin .GE. 0.) THEN
                                      dabs3 = ymax - ymin
                                    ELSE
                                      dabs3 = -(ymax-ymin)
                                    END IF
                                    IF (dabs3 .LE. eps6) THEN
                                      GOTO 160
                                    ELSE
                                      xm = xmin - (xmax-xmin)*(ymin/(&
&                                       ymax-ymin))
                                      IF (xm .LT. zref(j)) THEN
                                        xm = zref(j)
                                        CALL PUSHCONTROL1B(0)
                                      ELSE
                                        CALL PUSHCONTROL1B(1)
                                        xm = xm
                                      END IF
                                      IF (ymin .LE. 0._DOUBLE) THEN
                                        xm = 0.5_DOUBLE*(xm+xmax)
                                        CALL PUSHCONTROL2B(0)
                                      ELSE
                                        xm = 0.5_DOUBLE*(xm+xmin)
                                        CALL PUSHCONTROL2B(1)
                                      END IF
                                    END IF
                                  ELSE
                                    xm = (xmin+xmax)/2._DOUBLE
                                    CALL PUSHCONTROL2B(2)
                                  END IF
!------------------------
! TEST DE NON CONVERGENCE
!------------------------
                                  IF (iter1 .GE. itmax1) THEN
                                    GOTO 540
                                  ELSE
! FIN DE L'ALGORITHME ITERATIF
                                    CALL PUSHREAL8(zam1)
                                    zam1 = xm
                                    CALL PUSHCONTROL1B(0)
                                  END IF
                                END IF
                                ad_count0 = ad_count0 + 1
                                GOTO 200
                              END IF
                            END IF
                          END IF
                        END IF
                      END IF
                      CALL PUSHCONTROL4B(4)
                      CALL PUSHINTEGER4(ad_count0)
                      IF (impression .AND. iecrit .EQ. 0) WRITE(&
&                                                         unitelisting, &
&                                                         2000) &
&                                                         debut_bief, q(&
&                                                         debut_bief), &
&                                                         fin_bief, z(&
&                                                         fin_bief)
                      IF (izone .EQ. 0) THEN
                        izone = 1
                        kpass(izone) = 1
                        idebtor(izone) = j
                      ELSE IF (j .LT. -1 + idebtor(izone)) THEN
                        izone = izone + 1
                        idebtor(izone) = j
                        kpass(izone) = 1
                      ELSE
                        kpass(izone) = kpass(izone) + 1
                        idebtor(izone) = j
                      END IF
                      zc(j) = zam2
!
! DETECTION D'UN RESSAUT HYDRAULIQUE OU D'UNE CHUTE
!
                      iecrit = 1
                      CALL PUSHCONTROL2B(2)
                      GOTO 300
 130                  CALL PUSHCONTROL4B(5)
                      CALL PUSHINTEGER4(ad_count0)
                      CALL PUSHCONTROL2B(1)
                      GOTO 300
 140                  CALL PUSHCONTROL4B(6)
                      CALL PUSHINTEGER4(ad_count0)
                      CALL PUSHCONTROL2B(1)
                      GOTO 300
 150                  CALL PUSHCONTROL4B(7)
                      CALL PUSHINTEGER4(ad_count0)
                      CALL PUSHCONTROL2B(1)
                      GOTO 300
 160                  CALL PUSHCONTROL4B(8)
                      CALL PUSHINTEGER4(ad_count0)
                      CALL PUSHCONTROL2B(1)
                    END IF
                  END IF
                END IF
              END IF
            END IF
!
! RESULTAT DEFINITIF POUR LA SECTION AMONT
! ----------------------------------------
 300        CALL PUSHREAL8(z(j))
            z(j) = zam2
            IF (js .GE. 0.) THEN
              dabs4 = js
            ELSE
              dabs4 = -js
            END IF
! TESTS : PERTE DE CHARGE SINGULIERE OU PASSAGE FLUVIAL AMONT
! - CRITIQUE AVAL TROP BRUTAL
! -----------------------------------------------------------
            IF (dabs4 .GT. eps2) THEN
              IF (impression .AND. iecrit .EQ. 0) WRITE(unitelisting, &
&                                                 2000) debut_bief, q(&
&                                                 debut_bief), fin_bief&
&                                                 , z(fin_bief)
              iecrit = 1
              IF (impression) THEN
                num_bief = NUM_BIEF_S(connect, j + 1, erreur)
                absc_rel = x(j) - x(connect%originebief(num_bief))
                WRITE(unitelisting, 2080) j + 1, num_bief, absc_rel, js
              END IF
            END IF
            IF (frav - fram .GT. dfroud) THEN
              IF (impression .AND. iecrit .EQ. 0) WRITE(unitelisting, &
&                                                 2000) debut_bief, q(&
&                                                 debut_bief), fin_bief&
&                                                 , z(fin_bief)
              iecrit = 1
              IF (impression) THEN
                num_bief = NUM_BIEF_S(connect, j, erreur)
                absc_rel = x(j) - x(connect%originebief(num_bief))
                WRITE(unitelisting, 2090) j, num_bief, absc_rel
              END IF
            END IF
!------------------------------
! SORTIE DE LA BOUCLE DE CALCUL
!------------------------------
            CALL PUSHREAL8(zam1)
            zam1 = zam2
            IF (j .GT. debut_bief) THEN
              ad_count1 = ad_count1 + 1
              GOTO 100
            ELSE
              GOTO 310
            END IF
          END IF
        END IF
      END IF
    END IF
    CALL PUSHCONTROL4B(0)
    CALL PUSHINTEGER4(ad_count1)
    GOTO 550
 170 CALL PUSHCONTROL4B(1)
    CALL PUSHINTEGER4(ad_count1)
    erreur%ft = err_31
    erreur%ft_c = err_31c
    num_bief = NUM_BIEF_S(connect, j, erreur)
    absc_rel = x(j) - x(connect%originebief(num_bief))
    CALL TRAITER_ERREUR(erreur, temps, j, num_bief, absc_rel, zav, zref(&
&                 j))
    GOTO 550
 180 CALL PUSHCONTROL4B(2)
    CALL PUSHINTEGER4(ad_count1)
    GOTO 550
 190 CALL PUSHCONTROL4B(3)
    CALL PUSHINTEGER4(ad_count1)
    GOTO 550
 210 CALL PUSHCONTROL4B(4)
    CALL PUSHINTEGER4(ad_count1)
    GOTO 550
 220 CALL PUSHCONTROL4B(5)
    CALL PUSHINTEGER4(ad_count1)
    GOTO 550
 230 CALL PUSHCONTROL4B(6)
    CALL PUSHINTEGER4(ad_count1)
    GOTO 550
 240 CALL PUSHCONTROL4B(7)
    CALL PUSHINTEGER4(ad_count1)
    erreur%ft = err_35
    erreur%ft_c = err_35c
    num_bief = NUM_BIEF_S(connect, j, erreur)
    absc_rel = x(j) - x(connect%originebief(num_bief))
    CALL TRAITER_ERREUR(erreur, temps, j, num_bief, absc_rel, zcrit, &
&                 zref(j))
    GOTO 550
 250 CALL PUSHCONTROL4B(8)
    CALL PUSHINTEGER4(ad_count1)
    GOTO 550
 260 CALL PUSHCONTROL4B(0)
    CALL PUSHINTEGER4(ad_count0)
    CALL PUSHCONTROL4B(9)
    CALL PUSHINTEGER4(ad_count1)
    GOTO 550
 270 CALL PUSHCONTROL4B(1)
    CALL PUSHINTEGER4(ad_count0)
    CALL PUSHCONTROL4B(9)
    CALL PUSHINTEGER4(ad_count1)
    erreur%ft = err_31
    erreur%ft_c = err_31c
    num_bief = NUM_BIEF_S(connect, j, erreur)
    absc_rel = x(j) - x(connect%originebief(num_bief))
    CALL TRAITER_ERREUR(erreur, temps, j, num_bief, absc_rel, zav, zref(&
&                 j))
    GOTO 550
 280 CALL PUSHCONTROL4B(2)
    CALL PUSHINTEGER4(ad_count0)
    CALL PUSHCONTROL4B(9)
    CALL PUSHINTEGER4(ad_count1)
    GOTO 550
 290 CALL PUSHCONTROL4B(3)
    CALL PUSHINTEGER4(ad_count0)
    CALL PUSHCONTROL4B(9)
    CALL PUSHINTEGER4(ad_count1)
    GOTO 550
 310 CALL PUSHCONTROL4B(10)
    CALL PUSHINTEGER4(ad_count1)
    nb_zone = izone
    CALL PUSHCONTROL1B(0)
!-------------------------------------
!  BOUCLE SUR LES ZONES TORRENTIELLES
!  BALAYAGE AMONT -AVAL
!-------------------------------------
    DO 340 izone=1,nb_zone
      CALL PUSHINTEGER4(j)
      j = idebtor(izone) - 1
      IF (j .EQ. 0) j = idebtor(izone)
      ad_count3 = 1
 101  zam = z(j)
      zam1 = zam
      CALL PUSHREAL8(dx)
      dx = x(j+1) - x(j)
      CALL PUSHREAL8(dq)
      dq = q(j+1) - q(j)
!       CQMVJ = CQMV
      CALL PUSHREAL8(p2)
      CALL PUSHREAL8(p1)
      CALL RHSBP_S(b1, b2, bstock, p1, p2, sm1, sm2, rh1, rh2, j, zam1, &
&            zref(j), idt, xdt, profil, profilplan, unitelisting, erreur&
&           )
      IF (erreur%numero .NE. 0) THEN
        GOTO 350
      ELSE
! Resultats
! Donnees modifiees
! Donnees non modifiees
! Erreur
        CALL PUSHREAL8(betaam)
        CALL PUSHREAL8(vam)
        CALL PUSHREAL8(debam)
        CALL REPAR(debam, vam, betaam, q1, q2, sm1, sm2, rh1, rh2, p1&
&               , p2, q(j), cf1(j), cf2(j), modelelit, loifrottement&
&               , profil(idt(j))%Nom, erreur)
        IF (erreur%numero .NE. 0) THEN
          GOTO 360
        ELSE
          CALL PUSHREAL8(sam)
          sam = sm1 + sm2
          CALL PUSHREAL8(vam2)
          vam2 = vam**2
!
! DICHOTOMIE  : Z est compris entre ZRZF et ZC avec les cas analytiques la dichotomoie commence a 0.001 + convergence a 0.001
          iter1 = 0
          dz = 0.001001_double
          IF (iter1 .EQ. 0) THEN
            zam21 = zref(j+1) + dz
            zam22 = zc(j+1)
            CALL PUSHCONTROL1B(0)
          ELSE
            CALL PUSHCONTROL1B(1)
          END IF
!
! Initialisation des valeurs de la fonctions aux bornes
!
          zam2 = zam21
          arg1 = j + 1
          CALL RHSBP_S(b1, b2, bstock, p1, p2, sm1, sm2, rh1, rh2, arg1&
&                , zam2, zref(j+1), idt, xdt, profil, profilplan, &
&                unitelisting, erreur)
! Resultats
! Donnees modifiees
! Donnees non modifiees
! Erreur
          CALL PUSHREAL8(betaav)
          CALL PUSHREAL8(vav)
          CALL PUSHREAL8(debav)
          CALL REPAR(debav, vav, betaav, q1, q2, sm1, sm2, rh1, rh2, &
&                 p1, p2, q(j+1), cf1(j+1), cf2(j+1), modelelit, &
&                 loifrottement, profil(idt(arg1))%Nom, erreur)
          IF (erreur%numero .NE. 0) THEN
            GOTO 370
          ELSE
            CALL PUSHREAL8(sav)
            sav = sm1 + sm2
            jav = (q(j+1)/debav)**2
            jav = SIGN(jav, q(j+1))
            vav2 = vav**2
!---------------------
! termes de l'equation
!---------------------
            dxbeta = 0.25_DOUBLE*(betaav-betaam)*(vav2+vam2)/gpes
            dxq = 0.5_DOUBLE*dq/gpes*((betaav-cqmvj)*vav/sav+(betaam-&
&             cqmvj)*vam/sam)
            dxv = 0.5_DOUBLE*(betaav*vav2-betaam*vam2)/gpes
            h = sav/(b1+b2)
            CALL FROUDE_S(fram, betaam, vam, h, j, connect, x, erreur)
            IF (erreur%numero .NE. 0) THEN
              GOTO 380
            ELSE
!---------------------
! COTE OBTENUE  F (Z)
!---------------------
              zam3 = zam1 - jav*dx - dxbeta - dxq - dxv
              yp21 = zam3 - zam2
              zam2 = zam22
              arg1 = j + 1
              CALL RHSBP_S(b1, b2, bstock, p1, p2, sm1, sm2, rh1, rh2, &
&                    arg1, zam2, zref(j+1), idt, xdt, profil, profilplan&
&                    , unitelisting, erreur)
              IF (erreur%numero .NE. 0) THEN
                GOTO 390
              ELSE
! Resultats
! Donnees modifiees
! Donnees non modifiees
! Erreur
                CALL REPAR(debav, vav, betaav, q1, q2, sm1, sm2, rh1&
&                       , rh2, p1, p2, q(j+1), cf1(j+1), cf2(j+1), &
&                       modelelit, loifrottement, profil(idt(j+1))%Nom&
&                       , erreur)
                IF (erreur%numero .NE. 0) THEN
                  GOTO 400
                ELSE
                  sav = sm1 + sm2
!---------------------
! termes de l'equation
!---------------------
                  h = sav/(b1+b2)
                  CALL FROUDE_S(fram, betaam, vam, h, j, connect, x, &
&                         erreur)
                  IF (erreur%numero .NE. 0) THEN
                    GOTO 410
                  ELSE
!---------------------
! COTE OBTENUE  F (Z)
!---------------------
!
!  Recherche des zeros de F par dichotomie
!
                    zmil = (zam22+zam21)/2.d0
                    ad_count2 = 1
!
!   Calcul de F(Z) pour ces 3 valeurs
!
 201                zam2 = zmil
!
! Calcul de la fonction non lineaire a annuler aux pts ZAM21 ZAM22 ZMIL
! ---------------------------------------------------------------------
!
                    arg1 = j + 1
                    CALL RHSBP_S(b1, b2, bstock, p1, p2, sm1, sm2, rh1, &
&                          rh2, arg1, zam2, zref(j+1), idt, xdt, profil&
&                          , profilplan, unitelisting, erreur)
                    IF (erreur%numero .NE. 0) THEN
                      GOTO 420
                    ELSE
                      y = zam2 - zref(j+1)
                      IF (y .LT. eps6) THEN
                        GOTO 430
                      ELSE
! Resultats
! Donnees modifiees
! Donnees non modifiees
! Erreur
                        CALL REPAR(debav, vav, betaav, q1, q2, sm1, &
&                               sm2, rh1, rh2, p1, p2, q(j+1), cf1(j+1)&
&                               , cf2(j+1), modelelit, loifrottement&
&                               , profil(idt(arg1))%Nom, erreur)
                        IF (erreur%numero .NE. 0) THEN
                          GOTO 440
                        ELSE
                          sav = sm1 + sm2
                          jav = (q(j+1)/debav)**2
                          jav = SIGN(jav, q(j+1))
                          vav2 = vav**2
!---------------------
! termes de l'equation
!---------------------
                          dxbeta = 0.25_DOUBLE*(betaav-betaam)*(vav2+&
&                           vam2)/gpes
                          dxq = 0.5_DOUBLE*dq/gpes*((betaav-cqmvj)*vav/&
&                           sav+(betaam-cqmvj)*vam/sam)
                          dxv = 0.5_DOUBLE*(betaav*vav2-betaam*vam2)/&
&                           gpes
!----------------------------------------------------
! PERTE DE CHARGE SINGULIERE LIEE A UN RALENTISSEMENT
!----------------------------------------------------
                          IF (betaam*vam .GT. betaav*vav .AND. pcsing(j+&
&                             1) .LT. eps2) THEN
                            js = 0.5_DOUBLE*cpcs*(betaam*vam-betaav*vav)&
&                             **2/gpes
                          ELSE
                            js = 0._DOUBLE
                          END IF
!-----------------------------------------------
! PERTE DE CHARGE SINGULIERE LIEE A UN EVENEMENT
!-----------------------------------------------
                          IF (vam .GT. 0._DOUBLE) THEN
                            js = js + 0.5_DOUBLE*pcsing(j+1)*betaam*vam2&
&                             /gpes
                          ELSE
                            js = js - 0.5_DOUBLE*pcsing(j+1)*betaav*vav2&
&                             /gpes
                          END IF
                          h = sav/(b1+b2)
                          CALL FROUDE_S(fram, betaam, vam, h, j, connect&
&                                 , x, erreur)
                          IF (erreur%numero .NE. 0) THEN
                            GOTO 450
                          ELSE
!----------------------
! COTE OBTENUE  F (Z)
!----------------------
                            zam3 = zam1 - jav*dx - js - dxbeta - dxq - &
&                             dxv
                            ypmil = zam3 - zam2
                            IF (zam22 - zam21 .GT. 0.0001_DOUBLE) THEN
                              IF (yp21*ypmil .LE. 0.0_DOUBLE) THEN
                                zam22 = zmil
                                zmil = (zam21+zmil)/2.0_DOUBLE
                                CALL PUSHCONTROL1B(1)
                              ELSE
                                zam21 = zmil
                                zmil = (zam22+zmil)/2.0_DOUBLE
                                yp21 = ypmil
                                CALL PUSHCONTROL1B(0)
                              END IF
                              ad_count2 = ad_count2 + 1
                              GOTO 201
                            END IF
                          END IF
                        END IF
                      END IF
                    END IF
                    CALL PUSHCONTROL3B(4)
                    CALL PUSHINTEGER4(ad_count2)
                    CALL PUSHREAL8(z(j+1))
                    z(j+1) = zmil
                    CALL PUSHINTEGER4(j)
                    j = j + 1
                    IF (j .LT. idebtor(izone) + kpass(izone)) THEN
                      CALL PUSHCONTROL1B(1)
                    ELSE IF (fram .GE. 1.1_DOUBLE) THEN
!
! Verification de la position du ressaut - fonction impulsion
!
                      IF (j + 1 .EQ. fin_bief) THEN
                        GOTO 460
                      ELSE
                        CALL RHSB1_S(fimp(j), j, z(j), zref(j), idt, xdt&
&                              , profil, f1, unitelisting, erreur)
                        arg1 = j + 1
                        CALL RHSB1_S(fimp(j+1), arg1, z(j+1), zref(j+1)&
&                              , idt, xdt, profil, f1, unitelisting, &
&                              erreur)
                        arg1 = j + 1
                        CALL RHSBP_S(b1, b2, bstock, p1, p2, sm1, sm2, &
&                              rh1, rh2, arg1, z(j+1), zref(j+1), idt, &
&                              xdt, profil, profilplan, unitelisting, &
&                              erreur)
                        fimp(j+1) = z(j+1) - zref(j+1) - fimp(j+1)/(sm1+&
&                         sm2)
                        fimp(j+1) = q(j+1)**2/(sm1+sm2) + gpes*(sm1+sm2)&
&                         *fimp(j+1)
                        CALL RHSBP_S(b1, b2, bstock, p1, p2, sm1, sm2, &
&                              rh1, rh2, j, z(j), zref(j), idt, xdt, &
&                              profil, profilplan, unitelisting, erreur)
                        fimp(j) = z(j) - zref(j) - fimp(j)/(sm1+sm2)
                        fimp(j) = q(j)**2/(sm1+sm2) + gpes*(sm1+sm2)*&
&                         fimp(j)
                        IF (fimp(j) .GT. fimp(j+1) .AND. j .LT. fin_bief&
&                       ) THEN
                          CALL PUSHCONTROL1B(0)
                        ELSE
                          GOTO 320
                        END IF
                      END IF
                    ELSE
                      GOTO 330
                    END IF
                    ad_count3 = ad_count3 + 1
                    GOTO 101
                  END IF
                END IF
              END IF
            END IF
          END IF
        END IF
      END IF
 320  CALL PUSHCONTROL4B(10)
      CALL PUSHINTEGER4(ad_count3)
      CALL PUSHCONTROL1B(1)
      GOTO 340
 330  CALL PUSHCONTROL4B(8)
      CALL PUSHINTEGER4(ad_count3)
      CALL PUSHCONTROL1B(1)
 340 CONTINUE
    zam21b = 0.D0
    zam22b = 0.D0
    zcb = 0.D0
    GOTO 530
 350 CALL PUSHCONTROL4B(0)
    CALL PUSHINTEGER4(ad_count3)
    GOTO 470
 360 CALL PUSHCONTROL4B(1)
    CALL PUSHINTEGER4(ad_count3)
    GOTO 470
 370 CALL PUSHCONTROL4B(2)
    CALL PUSHINTEGER4(ad_count3)
    GOTO 470
 380 CALL PUSHCONTROL4B(3)
    CALL PUSHINTEGER4(ad_count3)
    GOTO 470
 390 CALL PUSHCONTROL4B(4)
    CALL PUSHINTEGER4(ad_count3)
    GOTO 470
 400 CALL PUSHCONTROL4B(5)
    CALL PUSHINTEGER4(ad_count3)
    GOTO 470
 410 CALL PUSHCONTROL4B(6)
    CALL PUSHINTEGER4(ad_count3)
    GOTO 470
 420 CALL PUSHCONTROL3B(0)
    CALL PUSHINTEGER4(ad_count2)
    CALL PUSHCONTROL4B(7)
    CALL PUSHINTEGER4(ad_count3)
    GOTO 470
 430 CALL PUSHCONTROL3B(1)
    CALL PUSHINTEGER4(ad_count2)
    CALL PUSHCONTROL4B(7)
    CALL PUSHINTEGER4(ad_count3)
    erreur%ft = err_31
    erreur%ft_c = err_31c
    num_bief = NUM_BIEF_S(connect, j, erreur)
    absc_rel = x(j) - x(connect%originebief(num_bief))
    CALL TRAITER_ERREUR(erreur, temps, j, num_bief, absc_rel, zav, zref(&
&                 j))
    GOTO 470
 440 CALL PUSHCONTROL3B(2)
    CALL PUSHINTEGER4(ad_count2)
    CALL PUSHCONTROL4B(7)
    CALL PUSHINTEGER4(ad_count3)
    GOTO 470
 450 CALL PUSHCONTROL3B(3)
    CALL PUSHINTEGER4(ad_count2)
    CALL PUSHCONTROL4B(7)
    CALL PUSHINTEGER4(ad_count3)
    GOTO 470
 460 CALL PUSHCONTROL4B(9)
    CALL PUSHINTEGER4(ad_count3)
    zb(j) = zb(j) + zb(j+1)
    zb(j+1) = 0.D0
 470 CALL POPINTEGER4(ad_count3)
    DO i3=1,ad_count3
      IF (i3 .EQ. 1) THEN
        CALL POPCONTROL4B(branch)
        IF (branch .LT. 5) THEN
          IF (branch .LT. 2) THEN
            IF (branch .EQ. 0) THEN
              zam21b = 0.D0
              zam22b = 0.D0
              zcb = 0.D0
              GOTO 520
            ELSE
              zam21b = 0.D0
              zam22b = 0.D0
              zcb = 0.D0
              GOTO 510
            END IF
          ELSE IF (branch .EQ. 2) THEN
            zam21b = 0.D0
            zam22b = 0.D0
            zcb = 0.D0
            GOTO 500
          ELSE
            IF (branch .EQ. 3) THEN
              zam21b = 0.D0
              zam22b = 0.D0
              zcb = 0.D0
            ELSE
              zam21b = 0.D0
              zam22b = 0.D0
              zcb = 0.D0
            END IF
            GOTO 490
          END IF
        ELSE IF (branch .LT. 8) THEN
          IF (branch .EQ. 5) THEN
            zam21b = 0.D0
            zam22b = 0.D0
            zcb = 0.D0
            GOTO 490
          ELSE IF (branch .EQ. 6) THEN
            zam21b = 0.D0
            zam22b = 0.D0
            zcb = 0.D0
            GOTO 490
          ELSE
            zmilb = 0.D0
            zam21b = 0.D0
            zam22b = 0.D0
            zcb = 0.D0
            GOTO 480
          END IF
        ELSE IF (branch .NE. 8) THEN
          IF (branch .EQ. 9) THEN
            zam21b = 0.D0
            zam22b = 0.D0
            zcb = 0.D0
          END IF
        END IF
      ELSE
        CALL POPCONTROL1B(branch)
      END IF
      CALL POPINTEGER4(j)
      CALL POPREAL8(z(j+1))
      zmilb = zb(j+1)
      zb(j+1) = 0.D0
 480  CALL POPINTEGER4(ad_count2)
      DO i2=1,ad_count2
        IF (i2 .EQ. 1) THEN
          CALL POPCONTROL3B(branch)
          IF (branch .LT. 2) THEN
            IF (branch .EQ. 0) THEN
              zmilb = 0.D0
              zam21b = 0.D0
              zam22b = 0.D0
              zcb = 0.D0
            ELSE
              zmilb = 0.D0
              zam21b = 0.D0
              zam22b = 0.D0
              zcb = 0.D0
            END IF
          ELSE IF (branch .EQ. 2) THEN
            zmilb = 0.D0
            zam21b = 0.D0
            zam22b = 0.D0
            zcb = 0.D0
          ELSE IF (branch .EQ. 3) THEN
            zmilb = 0.D0
            zam21b = 0.D0
            zam22b = 0.D0
            zcb = 0.D0
          END IF
        ELSE
          CALL POPCONTROL1B(branch)
          IF (branch .EQ. 0) THEN
            zam22b = zam22b + zmilb/2.0_DOUBLE
            zmilb = zam21b + zmilb/2.0_DOUBLE
            zam21b = 0.D0
          ELSE
            zam21b = zam21b + zmilb/2.0_DOUBLE
            zmilb = zam22b + zmilb/2.0_DOUBLE
            zam22b = 0.D0
          END IF
        END IF
      END DO
      zam22b = zam22b + zmilb/2.d0
      zam21b = zam21b + zmilb/2.d0
 490  CALL POPREAL8(sav)
 500  CALL POPREAL8(debav)
      CALL POPREAL8(vav)
      CALL POPREAL8(betaav)
      CALL POPCONTROL1B(branch)
      IF (branch .EQ. 0) THEN
        zcb(j+1) = zcb(j+1) + zam22b
        zam21b = 0.D0
        zam22b = 0.D0
      END IF
      CALL POPREAL8(vam2)
      CALL POPREAL8(sam)
 510  CALL POPREAL8(debam)
      CALL POPREAL8(vam)
      CALL POPREAL8(betaam)
 520  CALL POPREAL8(p1)
      CALL POPREAL8(p2)
      CALL POPREAL8(dq)
      CALL POPREAL8(dx)
    END DO
    CALL POPINTEGER4(j)
 530 CALL POPCONTROL1B(branch)
    IF (branch .EQ. 0) THEN
      GOTO 550
    ELSE
      GOTO 470
    END IF
 540 CALL PUSHCONTROL4B(9)
    CALL PUSHINTEGER4(ad_count0)
    CALL PUSHCONTROL4B(9)
    CALL PUSHINTEGER4(ad_count1)
    erreur%numero = 38
    erreur%ft = err_38
    erreur%ft_c = err_38c
    CALL TRAITER_ERREUR(erreur, temps, numbief, j)
 550 CALL POPINTEGER4(ad_count1)
    DO i1=1,ad_count1
      IF (i1 .EQ. 1) THEN
        CALL POPCONTROL4B(branch)
        IF (branch .LT. 5) THEN
          IF (branch .LT. 2) THEN
            IF (branch .EQ. 0) THEN
              betaamb = 0.D0
              dqb = 0.D0
              vamb = 0.D0
              p1b = 0.D0
              betaavb = 0.D0
              p2b = 0.D0
              vavb = 0.D0
              yminb = 0.D0
              rh1b = 0.D0
              rh2b = 0.D0
              xmb = 0.D0
              xpb = 0.D0
              xqb = 0.D0
              xminb = 0.D0
              sm1b = 0.D0
              sm2b = 0.D0
              zam2b = 0.D0
              ypb = 0.D0
              zavb = 0.D0
              zcb = 0.D0
              debamb = 0.D0
              ymaxb = 0.D0
              debavb = 0.D0
              xmaxb = 0.D0
              zcritb = 0.D0
            ELSE
              betaamb = 0.D0
              dqb = 0.D0
              vamb = 0.D0
              p1b = 0.D0
              betaavb = 0.D0
              p2b = 0.D0
              vavb = 0.D0
              yminb = 0.D0
              rh1b = 0.D0
              rh2b = 0.D0
              xmb = 0.D0
              xpb = 0.D0
              xqb = 0.D0
              xminb = 0.D0
              sm1b = 0.D0
              sm2b = 0.D0
              zam2b = 0.D0
              ypb = 0.D0
              zavb = 0.D0
              zcb = 0.D0
              debamb = 0.D0
              ymaxb = 0.D0
              debavb = 0.D0
              xmaxb = 0.D0
              zcritb = 0.D0
            END IF
            GOTO 720
          ELSE IF (branch .EQ. 2) THEN
            betaamb = 0.D0
            dqb = 0.D0
            vamb = 0.D0
            p1b = 0.D0
            betaavb = 0.D0
            p2b = 0.D0
            vavb = 0.D0
            yminb = 0.D0
            xmb = 0.D0
            xpb = 0.D0
            xqb = 0.D0
            xminb = 0.D0
            sm1b = 0.D0
            sm2b = 0.D0
            zam2b = 0.D0
            ypb = 0.D0
            zavb = 0.D0
            zcb = 0.D0
            debamb = 0.D0
            ymaxb = 0.D0
            debavb = 0.D0
            xmaxb = 0.D0
            zcritb = 0.D0
            GOTO 710
          ELSE IF (branch .EQ. 3) THEN
            betaamb = 0.D0
            dqb = 0.D0
            vamb = 0.D0
            p1b = 0.D0
            betaavb = 0.D0
            p2b = 0.D0
            vavb = 0.D0
            yminb = 0.D0
            xmb = 0.D0
            xpb = 0.D0
            xqb = 0.D0
            xminb = 0.D0
            savb = 0.D0
            zam2b = 0.D0
            ypb = 0.D0
            vav2b = 0.D0
            zavb = 0.D0
            zcb = 0.D0
            debamb = 0.D0
            ymaxb = 0.D0
            javb = 0.D0
            debavb = 0.D0
            xmaxb = 0.D0
            zcritb = 0.D0
            GOTO 700
          ELSE
            betaamb = 0.D0
            vamb = 0.D0
            p1b = 0.D0
            betaavb = 0.D0
            p2b = 0.D0
            vavb = 0.D0
            yminb = 0.D0
            xmb = 0.D0
            xpb = 0.D0
            xqb = 0.D0
            xminb = 0.D0
            zam2b = 0.D0
            ypb = 0.D0
            zcb = 0.D0
            debamb = 0.D0
            ymaxb = 0.D0
            debavb = 0.D0
            xmaxb = 0.D0
            zcritb = 0.D0
            GOTO 670
          END IF
        ELSE IF (branch .LT. 8) THEN
          IF (branch .EQ. 5) THEN
            betaamb = 0.D0
            dqb = 0.D0
            vamb = 0.D0
            p1b = 0.D0
            betaavb = 0.D0
            p2b = 0.D0
            vavb = 0.D0
            yminb = 0.D0
            xmb = 0.D0
            xpb = 0.D0
            xqb = 0.D0
            xminb = 0.D0
            savb = 0.D0
            ypb = 0.D0
            vav2b = 0.D0
            zavb = 0.D0
            zcb = 0.D0
            debamb = 0.D0
            ymaxb = 0.D0
            javb = 0.D0
            debavb = 0.D0
            xmaxb = 0.D0
            zcritb = 0.D0
            GOTO 660
          ELSE
            IF (branch .EQ. 6) THEN
              betaamb = 0.D0
              dqb = 0.D0
              vamb = 0.D0
              p1b = 0.D0
              betaavb = 0.D0
              p2b = 0.D0
              vavb = 0.D0
              yminb = 0.D0
              xmb = 0.D0
              xpb = 0.D0
              xqb = 0.D0
              xminb = 0.D0
              savb = 0.D0
              ypb = 0.D0
              vav2b = 0.D0
              zavb = 0.D0
              zcb = 0.D0
              debamb = 0.D0
              ymaxb = 0.D0
              javb = 0.D0
              debavb = 0.D0
              xmaxb = 0.D0
              zcritb = 0.D0
            ELSE
              betaamb = 0.D0
              dqb = 0.D0
              vamb = 0.D0
              p1b = 0.D0
              betaavb = 0.D0
              p2b = 0.D0
              vavb = 0.D0
              yminb = 0.D0
              xmb = 0.D0
              xpb = 0.D0
              xqb = 0.D0
              xminb = 0.D0
              savb = 0.D0
              ypb = 0.D0
              vav2b = 0.D0
              zavb = 0.D0
              zcb = 0.D0
              debamb = 0.D0
              ymaxb = 0.D0
              javb = 0.D0
              debavb = 0.D0
              xmaxb = 0.D0
              zcritb = 0.D0
            END IF
            GOTO 650
          END IF
        ELSE IF (branch .EQ. 8) THEN
          betaamb = 0.D0
          dqb = 0.D0
          vamb = 0.D0
          p1b = 0.D0
          betaavb = 0.D0
          p2b = 0.D0
          vavb = 0.D0
          yminb = 0.D0
          xmb = 0.D0
          xpb = 0.D0
          xqb = 0.D0
          xminb = 0.D0
          savb = 0.D0
          ypb = 0.D0
          vav2b = 0.D0
          zavb = 0.D0
          zcb = 0.D0
          debamb = 0.D0
          ymaxb = 0.D0
          javb = 0.D0
          debavb = 0.D0
          xmaxb = 0.D0
          zcritb = 0.D0
          GOTO 650
        ELSE IF (branch .EQ. 9) THEN
          betaamb = 0.D0
          dqb = 0.D0
          vamb = 0.D0
          p1b = 0.D0
          betaavb = 0.D0
          p2b = 0.D0
          vavb = 0.D0
          yminb = 0.D0
          rh1b = 0.D0
          rh2b = 0.D0
          xmb = 0.D0
          ss1b = 0.D0
          ss2b = 0.D0
          xpb = 0.D0
          xqb = 0.D0
          xminb = 0.D0
          savb = 0.D0
          zam1b = 0.D0
          ypb = 0.D0
          vav2b = 0.D0
          zavb = 0.D0
          zcb = 0.D0
          debamb = 0.D0
          ymaxb = 0.D0
          debavb = 0.D0
          xmaxb = 0.D0
          zcritb = 0.D0
          GOTO 560
        ELSE
          betaamb = 0.D0
          vamb = 0.D0
          p1b = 0.D0
          betaavb = 0.D0
          p2b = 0.D0
          vavb = 0.D0
          yminb = 0.D0
          xmb = 0.D0
          xpb = 0.D0
          xqb = 0.D0
          xminb = 0.D0
          zam1b = 0.D0
          zam2b = 0.D0
          ypb = 0.D0
          debamb = 0.D0
          ymaxb = 0.D0
          debavb = 0.D0
          xmaxb = 0.D0
          zcritb = 0.D0
        END IF
      END IF
      CALL POPREAL8(zam1)
      zam2b = zam2b + zam1b
      CALL POPREAL8(z(j))
      zam2b = zam2b + zb(j)
      zb(j) = 0.D0
      vav2 = vav**2
      CALL POPCONTROL2B(branch)
      IF (branch .LT. 2) THEN
        IF (branch .EQ. 0) THEN
          zcritb = zcritb + zcb(j) + zam2b
          zcb(j) = 0.D0
          dqb = 0.D0
          savb = 0.D0
          vav2b = 0.D0
          zavb = 0.D0
          javb = 0.D0
          GOTO 650
        END IF
      ELSE IF (branch .EQ. 2) THEN
        zam2b = zam2b + zcb(j)
        zcb(j) = 0.D0
      ELSE
        GOTO 670
      END IF
 560  CALL POPINTEGER4(ad_count0)
      DO i0=1,ad_count0
        IF (i0 .EQ. 1) THEN
          CALL POPCONTROL4B(branch)
          IF (branch .LT. 5) THEN
            IF (branch .LT. 2) THEN
              IF (branch .EQ. 0) THEN
                betaamb = 0.D0
                dqb = 0.D0
                vamb = 0.D0
                p1b = 0.D0
                betaavb = 0.D0
                p2b = 0.D0
                vavb = 0.D0
                yminb = 0.D0
                rh1b = 0.D0
                rh2b = 0.D0
                xmb = 0.D0
                ss1b = 0.D0
                ss2b = 0.D0
                xpb = 0.D0
                xqb = 0.D0
                xminb = 0.D0
                savb = 0.D0
                zam1b = 0.D0
                ypb = 0.D0
                vav2b = 0.D0
                zavb = 0.D0
                zcb = 0.D0
                debamb = 0.D0
                ymaxb = 0.D0
                debavb = 0.D0
                xmaxb = 0.D0
                zcritb = 0.D0
              ELSE
                betaamb = 0.D0
                dqb = 0.D0
                vamb = 0.D0
                p1b = 0.D0
                betaavb = 0.D0
                p2b = 0.D0
                vavb = 0.D0
                yminb = 0.D0
                rh1b = 0.D0
                rh2b = 0.D0
                xmb = 0.D0
                ss1b = 0.D0
                ss2b = 0.D0
                xpb = 0.D0
                xqb = 0.D0
                xminb = 0.D0
                savb = 0.D0
                zam1b = 0.D0
                ypb = 0.D0
                vav2b = 0.D0
                zavb = 0.D0
                zcb = 0.D0
                debamb = 0.D0
                ymaxb = 0.D0
                debavb = 0.D0
                xmaxb = 0.D0
                zcritb = 0.D0
              END IF
              GOTO 640
            ELSE IF (branch .EQ. 2) THEN
              betaamb = 0.D0
              dqb = 0.D0
              vamb = 0.D0
              p1b = 0.D0
              betaavb = 0.D0
              p2b = 0.D0
              vavb = 0.D0
              yminb = 0.D0
              xmb = 0.D0
              ss1b = 0.D0
              ss2b = 0.D0
              xpb = 0.D0
              xqb = 0.D0
              xminb = 0.D0
              savb = 0.D0
              zam1b = 0.D0
              ypb = 0.D0
              vav2b = 0.D0
              zavb = 0.D0
              zcb = 0.D0
              debamb = 0.D0
              ymaxb = 0.D0
              debavb = 0.D0
              xmaxb = 0.D0
              zcritb = 0.D0
              GOTO 630
            ELSE IF (branch .EQ. 3) THEN
              jsb = 0.D0
              betaamb = 0.D0
              dqb = 0.D0
              vamb = 0.D0
              p1b = 0.D0
              betaavb = 0.D0
              p2b = 0.D0
              vavb = 0.D0
              yminb = 0.D0
              xmb = 0.D0
              xpb = 0.D0
              xqb = 0.D0
              dxqb = 0.D0
              xminb = 0.D0
              dxvb = 0.D0
              savb = 0.D0
              zam1b = 0.D0
              ypb = 0.D0
              vav2b = 0.D0
              zavb = 0.D0
              javamb = 0.D0
              zcb = 0.D0
              debamb = 0.D0
              ymaxb = 0.D0
              debavb = 0.D0
              xmaxb = 0.D0
              dxbetab = 0.D0
              zcritb = 0.D0
              GOTO 620
            ELSE
              dqb = 0.D0
              savb = 0.D0
              zam1b = 0.D0
              vav2b = 0.D0
              zavb = 0.D0
              GOTO 610
            END IF
          ELSE IF (branch .LT. 7) THEN
            IF (branch .EQ. 5) THEN
              dqb = 0.D0
              savb = 0.D0
              vav2b = 0.D0
              zavb = 0.D0
              GOTO 600
            ELSE
              dqb = 0.D0
              savb = 0.D0
              vav2b = 0.D0
              yqb = 0.D0
              zavb = 0.D0
              GOTO 580
            END IF
          ELSE IF (branch .EQ. 7) THEN
            dqb = 0.D0
            savb = 0.D0
            ymb = 0.D0
            vav2b = 0.D0
            zavb = 0.D0
            GOTO 590
          ELSE IF (branch .EQ. 8) THEN
            dqb = 0.D0
            savb = 0.D0
            vav2b = 0.D0
            zavb = 0.D0
            GOTO 570
          ELSE
            betaamb = 0.D0
            dqb = 0.D0
            vamb = 0.D0
            p1b = 0.D0
            betaavb = 0.D0
            p2b = 0.D0
            vavb = 0.D0
            yminb = 0.D0
            xmb = 0.D0
            xpb = 0.D0
            xqb = 0.D0
            xminb = 0.D0
            savb = 0.D0
            ypb = 0.D0
            vav2b = 0.D0
            zavb = 0.D0
            zcb = 0.D0
            debamb = 0.D0
            ymaxb = 0.D0
            debavb = 0.D0
            xmaxb = 0.D0
            zcritb = 0.D0
          END IF
        ELSE
          CALL POPCONTROL1B(branch)
          IF (branch .EQ. 0) THEN
            CALL POPREAL8(zam1)
            xmb = xmb + zam1b
          ELSE
            CALL POPREAL8(zam1)
            zam2b = zam1b
            GOTO 600
          END IF
        END IF
        CALL POPCONTROL2B(branch)
        IF (branch .EQ. 0) THEN
          xmaxb = xmaxb + 0.5_DOUBLE*xmb
          xmb = 0.5_DOUBLE*xmb
        ELSE IF (branch .EQ. 1) THEN
          xminb = xminb + 0.5_DOUBLE*xmb
          xmb = 0.5_DOUBLE*xmb
        ELSE
          xminb = xminb + xmb/2._DOUBLE
          xmaxb = xmaxb + xmb/2._DOUBLE
          xmb = 0.D0
          zam2b = 0.D0
          GOTO 570
        END IF
        CALL POPCONTROL1B(branch)
        IF (branch .EQ. 0) xmb = 0.D0
        tempb11 = -(xmb/(ymax-ymin))
        tempb12 = -((xmax-xmin)*ymin*tempb11/(ymax-ymin))
        xminb = xminb + xmb - ymin*tempb11
        xmaxb = xmaxb + ymin*tempb11
        yminb = yminb + (xmax-xmin)*tempb11 - tempb12
        ymaxb = ymaxb + tempb12
        xmb = 0.D0
        zam2b = 0.D0
 570    CALL POPCONTROL3B(branch)
        IF (branch .LT. 3) THEN
          IF (branch .EQ. 0) THEN
            CALL POPREAL8(xmax)
            xpb = xpb + xmaxb
            CALL POPREAL8(xmin)
            xqb = xqb + xminb
          ELSE IF (branch .EQ. 1) THEN
            CALL POPREAL8(xmax)
            xqb = xqb + xmaxb
            CALL POPREAL8(xmin)
            xpb = xpb + xminb
          ELSE
            CALL POPREAL8(ymin)
            ymb = yminb
            CALL POPREAL8(xmin)
            xmb = xmb + xminb
            yminb = 0.D0
            xminb = 0.D0
            GOTO 590
          END IF
          CALL POPCONTROL1B(branch)
          IF (branch .EQ. 0) THEN
            CALL POPREAL8(ymax)
            yqb = ymaxb
          ELSE
            CALL POPREAL8(ymax)
            ypb = ypb + ymaxb
            yqb = 0.D0
          END IF
          CALL POPCONTROL1B(branch)
          IF (branch .EQ. 0) THEN
            CALL POPREAL8(ymin)
            yqb = yqb + yminb
          ELSE
            CALL POPREAL8(ymin)
            ypb = ypb + yminb
          END IF
          yminb = 0.D0
          xminb = 0.D0
          ymaxb = 0.D0
          xmaxb = 0.D0
        ELSE
          IF (branch .EQ. 3) THEN
            CALL POPREAL8(ymax)
            ymb = ymaxb
            CALL POPREAL8(xmax)
            xmb = xmb + xmaxb
            ymaxb = 0.D0
            xmaxb = 0.D0
          ELSE IF (branch .EQ. 4) THEN
            CALL POPREAL8(ymax)
            ymb = ymaxb
            CALL POPREAL8(xmax)
            xmb = xmb + xmaxb
            CALL POPREAL8(ymin)
            ymaxb = yminb
            CALL POPREAL8(xmin)
            xmaxb = xminb
            yminb = 0.D0
            xminb = 0.D0
          ELSE
            CALL POPREAL8(ymin)
            ymb = yminb
            CALL POPREAL8(xmin)
            xmb = xmb + xminb
            CALL POPREAL8(ymax)
            yminb = ymaxb
            CALL POPREAL8(xmax)
            xminb = xmaxb
            ymaxb = 0.D0
            xmaxb = 0.D0
          END IF
          GOTO 590
        END IF
 580    zam2b = zam2b + yqb
        zam1b = -yqb
        GOTO 610
 590    zam2b = zam2b + ymb
        zam1b = -ymb
        GOTO 610
 600    zam2b = zam2b + ypb + xqb
        zam1b = xpb - ypb
        xpb = 0.D0
        xqb = 0.D0
        ypb = 0.D0
 610    zavb = zavb + zam2b
        javamb = dx*zam2b
        jsb = zam2b
        dxbetab = zam2b
        dxqb = zam2b
        dxvb = zam2b
 620    CALL POPCONTROL1B(branch)
        IF (branch .EQ. 0) THEN
          tempb9 = 0.5_DOUBLE*pcsing(j+1)*jsb/gpes
          pcsingb(j+1) = pcsingb(j+1) + betaam*vam2*0.5_DOUBLE*jsb/gpes
          betaamb = betaamb + vam2*tempb9
          vam2b = betaam*tempb9
        ELSE
          tempb10 = -(0.5_DOUBLE*pcsing(j+1)*jsb/gpes)
          pcsingb(j+1) = pcsingb(j+1) - betaav*vav2*0.5_DOUBLE*jsb/gpes
          betaavb = betaavb + vav2*tempb10
          vav2b = vav2b + betaav*tempb10
          vam2b = 0.D0
        END IF
        CALL POPCONTROL1B(branch)
        IF (branch .EQ. 0) THEN
          tempb8 = cpcs*0.5_DOUBLE*2*(betaam*vam-betaav*vav)*jsb/gpes
          betaamb = betaamb + vam*tempb8
          vamb = vamb + betaam*tempb8
          betaavb = betaavb - vav*tempb8
          vavb = vavb - betaav*tempb8
        END IF
        tempb3 = 0.25_DOUBLE*(betaav-betaam)*dxbetab/gpes
        tempb2 = (vav2+vam2)*0.25_DOUBLE*dxbetab/gpes
        tempb1 = 0.5_DOUBLE*dq*dxqb/gpes
        temp2 = vav/sav
        temp3 = vam/sam
        tempb0 = 0.5_DOUBLE*dxvb/gpes
        betaavb = betaavb + temp2*tempb1 + tempb2 + vav2*tempb0
        vav2b = vav2b + tempb3 + betaav*tempb0
        betaamb = betaamb + temp3*tempb1 - tempb2 - vam2*tempb0
        vam2b = vam2b + tempb3 - betaam*tempb0
        tempb4 = (betaav-cqmvj)*tempb1/sav
        tempb5 = (betaam-cqmvj)*tempb1/sam
        vavb = vavb + tempb4
        savb = savb - temp2*tempb4
        vamb = vamb + 2*vam*vam2b + tempb5
        samb = -(temp3*tempb5)
        dqb = dqb + ((betaav-cqmvj)*temp2+(betaam-cqmvj)*temp3)*&
&         0.5_DOUBLE*dxqb/gpes
        CALL POPREAL8(vam2)
        temp1 = 0.5_DOUBLE*(debav**2+debam**2)
        tempb6 = javamb/temp1
        temp0 = q(j)**2
        tempb7 = -(SIGN(temp0, q(j))*0.5_DOUBLE*tempb6/temp1)
        qb(j) = qb(j) + SIGN(1.d0, temp0*q(j))*2*q(j)*tempb6
        debavb = debavb + 2*debav*tempb7
        debamb = debamb + 2*debam*tempb7
        CALL POPREAL8(sam)
        ss1b = samb
        ss2b = samb
 630    CALL POPREAL8(debam)
        CALL POPREAL8(vam)
        CALL POPREAL8(betaam)
        CALL POPREAL8(ss1)
        CALL POPREAL8(ss2)
        CALL POPREAL8(rh1)
        CALL POPREAL8(rh2)
        CALL REPAR_B(debam, debamb, vam, vamb, betaam, betaamb, q1, q2, &
&              ss1, ss1b, ss2, ss2b, rh1, rh1b, rh2, rh2b, p1, p1b, p2, &
&              p2b, q(j), qb(j), cf1(j), cf1b(j), cf2(j), cf2b(j), &
&              modelelit, loifrottement, erreur)
 640    CALL POPREAL8(p1)
        CALL POPREAL8(p2)
        b1b = 0.D0
        b2b = 0.D0
        CALL RHSBP_S_B(b1, b1b, b2, b2b, bstock, p1, p1b, p2, p2b, ss1, &
&                ss1b, ss2, ss2b, rh1, rh1b, rh2, rh2b, j, zam1, zam1b, &
&                zref(j), idt, xdt, profil, profilplan, unitelisting, &
&                erreur)
      END DO
      zavb = zavb + zam1b
      javb = dx*zam1b
 650  CALL POPREAL8(p1)
      CALL POPREAL8(p2)
      b1b = 0.D0
      b2b = 0.D0
      ss1b = 0.D0
      ss2b = 0.D0
      rh1b = 0.D0
      rh2b = 0.D0
      CALL RHSBP_S_B(b1, b1b, b2, b2b, bstock, p1, p1b, p2, p2b, ss1, &
&              ss1b, ss2, ss2b, rh1, rh1b, rh2, rh2b, j, zcrit, zcritb, &
&              zref(j), idt, xdt, profil, profilplan, unitelisting, &
&              erreur)
 660  zcritb = zcritb + zcb(j)
      zcb(j) = 0.D0
      CALL POPREAL8(zcrit)
      CALL POPCHARACTERARRAY(erreur%arbredappel, 132)
      CALL POPCHARACTERARRAY(erreur%message, 400)
      CALL CRITIQ_B(zcrit, zcritb, j, zref, q(j), qb(j), cf1(j), cf1b(j)&
&             , cf2(j), cf2b(j), idt, xdt, profil, profilplan, modelelit&
&             , loifrottement, unitelisting, erreur)
      CALL POPCONTROL1B(branch)
      IF (branch .EQ. 0) THEN
        GOTO 680
      ELSE
        zam2b = 0.D0
        GOTO 690
      END IF
 670  CALL POPREAL8(zam2)
      CALL POPCHARACTERARRAY(erreur%arbredappel, 132)
      CALL POPCHARACTERARRAY(erreur%message, 400)
      CALL PSING_B(zam2, zam2b, singularite(ising)&
&            , zref(j), zav, zavb, q(j), qb(j), profil, profilplan%b1, &
&            idt, xdt, j, temps, erreur)
 680  CALL POPINTEGER4(ad_count)
      DO i=1,ad_count
        IF (i .EQ. 1) THEN
          CALL POPCONTROL1B(branch)
          IF (branch .EQ. 0) THEN
            zam2b = 0.D0
          ELSE
            dqb = 0.D0
            savb = 0.D0
            vav2b = 0.D0
            javb = 0.D0
          END IF
        END IF
        CALL POPINTEGER4(ising)
      END DO
 690  CALL POPINTEGER4(j)
 700  vavb = vavb + 2*vav*vav2b
      CALL POPREAL8(jav)
      javb = SIGN(1.d0, jav*q(j))*javb
      temp = q(j)/debav
      tempb = 2*temp*javb/debav
      qb(j) = qb(j) + tempb
      debavb = debavb - temp*tempb
      CALL POPREAL8(sav)
      sm1b = savb
      sm2b = savb
 710  CALL POPREAL8(debav)
      CALL POPREAL8(vav)
      CALL POPREAL8(betaav)
      CALL POPREAL8(sm1)
      CALL POPREAL8(sm2)
      CALL POPREAL8(rh1)
      CALL POPREAL8(rh2)
      CALL REPAR_B(debav, debavb, vav, vavb, betaav, betaavb, q1, q2, &
&            sm1, sm1b, sm2, sm2b, rh1, rh1b, rh2, rh2b, p1, p1b, p2, &
&            p2b, q(j), qb(j), cf1(j), cf1b(j), cf2(j), cf2b(j), &
&            modelelit, loifrottement, erreur)
 720  CALL POPREAL8(p1)
      CALL POPREAL8(p2)
      b1b = 0.D0
      b2b = 0.D0
      CALL RHSBP_S_B(b1, b1b, b2, b2b, bstock, p1, p1b, p2, p2b, sm1, &
&              sm1b, sm2, sm2b, rh1, rh1b, rh2, rh2b, j, zav, zavb, zref&
&              (j), idt, xdt, profil, profilplan, unitelisting, erreur)
      CALL POPREAL8(dq)
      qb(j) = qb(j) + dqb
      qb(j-1) = qb(j-1) - dqb
      CALL POPREAL8(dx)
      CALL POPREAL8(zav)
      zam1b = zavb
    END DO
  END IF
  zinitb = zb(j) + zam1b
  CALL POPREAL8(z(j))
  zb(j) = 0.D0
! FORMATS POUR LES WARNINGS
! -------------------------
 2000 FORMAT('<<ATTENTION>>',/, 'Pour Q(',i4,') amont = ',f10.2,&
&        ' et Z (',i4,') aval = ',f10.2)
 2030 FORMAT('Passage en torrentiel DETECTE a la section : ',i5,/, &
&        'Bief n0 ',i3,', Abscisse relative : ',g12.3,/, &
&        'cote critique Zcrit = ',f8.3)
 2080 FORMAT('A la section : ',i5,', bief n0 ',i3,&
&        ', abscisse relative = ',g12.3,/, &
&        'perte de charge singuliere JS = ',f8.3)
 2090 FORMAT('<<CONSEIL>>',/, 'A la section : ',i5,', bief n0 ',i3,&
&        ', abscisse relative : ',g12.3, &
&        'passage amont fluvial - aval critique trop brutal.',/, &
&        'Raffiner le maillage dans cette zone.')
END SUBROUTINE PERMAT_B




SUBROUTINE CALC_PC_CONFLU_B(pcsing, pcsingb, z, zb, q, qb, x, zref, &
& profil, profilplan, confluent, abaque, idt, xdt, connect, unitelisting&
& , erreur)
! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P.CHERUBINI
!                             S. PERON
!                             S. MANDELKERN
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
!============================== Instructions ================================
! type DOUBLE
  USE M_PRECISION
! EPS3
  USE M_PARAMETRE_C
! Liste des messages d'erreur
  USE M_MESSAGE_C
! type CONNECT_T
  USE M_CONNECT_T
! type CONFLUENT_T
  USE M_CONFLUENT_T
! type ERREUR_T
  USE M_ERREUR_T
! type PROFIL_T
  USE M_PROFIL_T
! type PROFIL_T
  USE M_PROFIL_PLAN_T
! Traitement des erreurs
  USE M_TRAITER_ERREUR_I
! sous-programme RHSBP_SECTION_S
  USE M_RHSBP_S_B
  USE M_RHSBP_S
  USE M_INTERPOLATION_S
  IMPLICIT NONE
! Arguments
  DOUBLE PRECISION, DIMENSION(:) :: pcsing
  DOUBLE PRECISION, DIMENSION(:) :: pcsingb
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: z
  DOUBLE PRECISION, DIMENSION(:) :: zb
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: q
  DOUBLE PRECISION, DIMENSION(:) :: qb
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: x
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: zref
  TYPE(PROFIL_T), DIMENSION(:), INTENT(IN) :: profil
  TYPE(PROFIL_PLAN_T), INTENT(IN) :: profilplan
  TYPE(CONFLUENT_T), DIMENSION(:), INTENT(IN) :: confluent
  DOUBLE PRECISION, DIMENSION(:, :, :), INTENT(IN) :: abaque
  INTEGER, DIMENSION(:), INTENT(IN) :: idt
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: xdt
  TYPE(CONNECT_T), INTENT(IN) :: connect
  INTEGER, INTENT(IN) :: unitelisting
  TYPE(ERREUR_T), INTENT(INOUT) :: erreur
! Constantes
  INTEGER, PARAMETER :: type_riviere_naturelle=1
  INTEGER, PARAMETER :: type_designation=2
  INTEGER, PARAMETER :: type_derniere_section=3
  INTEGER, PARAMETER :: branche_amont=1
  INTEGER, PARAMETER :: branche_aval=0
  INTEGER, PARAMETER :: branche_princ_amont=1
  INTEGER, PARAMETER :: branche_princ_aval=0
  INTEGER, PARAMETER :: branche_laterale=2
  INTEGER, PARAMETER :: abaque_a=1
  INTEGER, PARAMETER :: abaque_b=2
  INTEGER, PARAMETER :: abaque_c=3
! Variables locales
  TYPE CONF_T
      SEQUENCE
      INTEGER, DIMENSION(3) :: section
      DOUBLE PRECISION, DIMENSION(3) :: debit
      DOUBLE PRECISION, DIMENSION(3) :: largeur
      INTEGER, DIMENSION(3) :: position
      INTEGER, DIMENSION(3) :: nature
  END TYPE CONF_T
  TYPE CONF_T_B
      SEQUENCE
      DOUBLE PRECISION, DIMENSION(3) :: debit
      DOUBLE PRECISION, DIMENSION(3) :: largeur
  END TYPE CONF_T_B
  TYPE(CONF_T) :: conf
  TYPE(CONF_T_B) :: confb
  INTEGER :: j, k
! compteur sur les branches ou placer une PDCS
  INTEGER :: iabaque
! compteur sur les abaques
  INTEGER :: jabaque
! numero de l'abaque a utiliser
  INTEGER :: numero_abaque
  INTEGER :: nb_ext
  INTEGER :: num_bief
  INTEGER :: num_section
  LOGICAL :: debut_bief
  DOUBLE PRECISION :: c6abc(3)
  DOUBLE PRECISION :: c6abcb(3)
  DOUBLE PRECISION :: alpha_c6(6), qc6(5)
  DOUBLE PRECISION :: c6qinf(6), c6qsup(6)
  DOUBLE PRECISION :: c6(2), c6a, c6b, c6c
  DOUBLE PRECISION :: c6b0(2), c6ab, c6bb, c6cb
  DOUBLE PRECISION :: angle_conf
  DOUBLE PRECISION :: lf, ll, lp, llp, qp, ql, qpl
  DOUBLE PRECISION :: lfb, llb, lpb, llpb, qpb, qlb, qplb
! elargissement branche amont princ / branche aval
  DOUBLE PRECISION :: elargissement
  DOUBLE PRECISION :: elargissementb
  DOUBLE PRECISION :: qinf, qsup, c6_inf, c6_sup
  DOUBLE PRECISION :: rapport
  DOUBLE PRECISION :: rapportb
  DOUBLE PRECISION :: q_max
  INTEGER :: type_riviere
! Compteur sur les noeuds
  INTEGER :: inoeu
! compteur sur les profils
  INTEGER :: iprof
! compteur sur les sections
  INTEGER :: isect
! compteur sur les sections
  INTEGER :: ibief
  INTEGER :: num_prof
  DOUBLE PRECISION :: abs_prof
  DOUBLE PRECISION :: xsect, xprof
  DOUBLE PRECISION :: dxmin
  INTEGER :: idxmin
  DOUBLE PRECISION :: xprofc(SIZE(connect%nbbiefconfluence), 5)
  INTRINSIC SIZE
  INTEGER :: nb_branche_amont
  INTEGER :: nb_branche_aval
  INTEGER :: iext
  INTRINSIC DABS
  INTEGER :: num_sect
  INTRINSIC DMAX1
  REAL :: angle1
  REAL :: angle2
  INTRINSIC ABS
  INTEGER :: branch
  INTEGER :: ad_count
  INTEGER :: i
  INTEGER :: ad_count0
  INTEGER :: i0
  INTEGER :: ad_to1
  INTEGER :: ad_to2
  INTEGER :: ad_count1
  INTEGER :: i1
  INTEGER :: ad_count2
  INTEGER :: i2
  INTEGER :: ad_to3
  DOUBLE PRECISION :: tempb3
  DOUBLE PRECISION :: tempb2
  DOUBLE PRECISION :: tempb1
  DOUBLE PRECISION :: tempb0
  DOUBLE PRECISION :: tempb
  DOUBLE PRECISION :: dabs2
  DOUBLE PRECISION :: dabs1
  DOUBLE PRECISION :: dabs0
!Character(132)  :: !arbredappel_old ! ancien arbre d'appel
  DATA alpha_c6 /30., 40., 50., 60., 70., 80./
  DATA qc6 /1., 1.5, 2., 3., 5./
!============================== Instructions ================================
! INITIALISATIONS
!----------------
!arbredappel_old = trim(!Erreur%arbredappel)
!Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>CALC_PC_CONFLU'
  type_riviere = type_derniere_section
  CALL PUSHINTEGER4(inoeu)
  CALL PUSHCONTROL1B(0)
! BOUCLE SUR LES NOEUDS
label_noeud:DO inoeu=1,SIZE(connect%nbbiefconfluence)
! INITIALISATIONS
!----------------
    nb_branche_amont = 0
    nb_branche_aval = 0
! Detection des extremites amont et aval de la confluence
! -------------------------------------------------------
! Nombre d extremites liees a ce noeud
    nb_ext = connect%nbbiefconfluence(inoeu)
    CALL PUSHINTEGER4(iext)
    ad_count = 1
label_ext:DO iext=1,SIZE(connect%numbiefconfluence(inoeu, :))
! Numero de la branche
      num_bief = connect%numbiefconfluence(inoeu, iext)
! numero de la section extremite de la branche
      num_section = connect%numsectionconfluence(inoeu, iext)
! Position de l'extremite debut ou fin de branche
      debut_bief = .false.
      DO ibief=1,SIZE(connect%originebief(:))
        IF (num_section .EQ. connect%originebief(ibief)) GOTO 100
      END DO
      GOTO 110
 100  debut_bief = .true.
! Numero de la section
! Si option riviere naturelle
 110  IF (type_riviere .EQ. type_riviere_naturelle) THEN
! abscisse de la section extremite de la branche
        xsect = x(num_section)
! recherche du numero du profil correspondant a cette extremite
        DO iprof=1,SIZE(profil)
          xprof = profil(iprof)%absabs
          IF (xprof - xsect .GE. 0.) THEN
            dabs0 = xprof - xsect
          ELSE
            dabs0 = -(xprof-xsect)
          END IF
          IF (dabs0 .LT. eps3) GOTO 120
        END DO
        GOTO 130
 120    num_prof = iprof
! si on est en debut de branche, on avance d'un profil
 130    IF (debut_bief) THEN
          num_prof = num_prof + 1
        ELSE
! Si fin de branche
          num_prof = num_prof - 1
        END IF
! recherche du numero de la section la plus proche du profil
        dxmin = 1000._DOUBLE
        idxmin = 0
        abs_prof = profil(num_prof)%absabs
        DO isect=1,SIZE(x)
          IF (x(isect) - abs_prof .GE. 0.) THEN
            dabs1 = x(isect) - abs_prof
          ELSE
            dabs1 = -(x(isect)-abs_prof)
          END IF
          IF (dabs1 .LE. dxmin) THEN
            IF (x(isect) - abs_prof .GE. 0.) THEN
              dxmin = x(isect) - abs_prof
            ELSE
              dxmin = -(x(isect)-abs_prof)
            END IF
            idxmin = isect
          END IF
        END DO
        CALL PUSHINTEGER4(isect - 1)
! le numero de la section est donc
        CALL PUSHINTEGER4(conf%section(iext))
        conf%section(iext) = idxmin
! designation des profils en abscisses absolues
        CALL PUSHCONTROL3B(0)
      ELSE IF (type_riviere .EQ. type_designation) THEN
! recherche du numero du section la plus proche du profil
        dxmin = 1000._DOUBLE
        idxmin = 0
        DO isect=1,SIZE(x)
          IF (x(isect) - xprofc(inoeu, iext) .GE. 0.) THEN
            dabs2 = x(isect) - xprofc(inoeu, iext)
          ELSE
            dabs2 = -(x(isect)-xprofc(inoeu, iext))
          END IF
          IF (dabs2 .LE. dxmin) THEN
            IF (x(isect) - xprofc(inoeu, iext) .GE. 0.) THEN
              dxmin = x(isect) - xprofc(inoeu, iext)
            ELSE
              dxmin = -(x(isect)-xprofc(inoeu, iext))
            END IF
            idxmin = isect
          END IF
        END DO
        CALL PUSHINTEGER4(isect - 1)
! le numero est donc
        CALL PUSHINTEGER4(conf%section(iext))
        conf%section(iext) = idxmin
! dernier profil de la branche
        CALL PUSHCONTROL3B(1)
      ELSE IF (type_riviere .EQ. type_derniere_section) THEN
        IF (debut_bief) THEN
          CALL PUSHINTEGER4(conf%section(iext))
          conf%section(iext) = connect%originebief(num_bief)
          CALL PUSHCONTROL3B(2)
        ELSE
          CALL PUSHINTEGER4(conf%section(iext))
          conf%section(iext) = connect%finbief(num_bief)
          CALL PUSHCONTROL3B(3)
        END IF
      ELSE
        CALL PUSHCONTROL3B(4)
      END IF
      IF (debut_bief) THEN
        IF (q(conf%section(iext)) .GT. 0._DOUBLE) THEN
          CALL PUSHCONTROL2B(0)
          conf%position(iext) = branche_aval
        ELSE IF (q(conf%section(iext)) .LT. 0._DOUBLE) THEN
          CALL PUSHCONTROL2B(1)
          conf%position(iext) = branche_amont
          nb_branche_amont = nb_branche_amont + 1
        ELSE
          GOTO 200
        END IF
      ELSE IF (q(conf%section(iext)) .GT. 0._DOUBLE) THEN
        CALL PUSHCONTROL2B(2)
        conf%position(iext) = branche_amont
        nb_branche_amont = nb_branche_amont + 1
      ELSE IF (q(conf%section(iext)) .LT. 0._DOUBLE) THEN
        CALL PUSHCONTROL2B(3)
        conf%position(iext) = branche_aval
      ELSE
        GOTO 220
      END IF
      conf%debit(iext) = q(conf%section(iext))
      num_sect = conf%section(iext)
! Variable a interpoler sur les profils
! Cote du fond a la section de calcul
! Cote d'eau   a la section de calcul
! Indices du profil de donnees amont
! Position de la section / profils
! Profils geometriques
! Variable planimetree
! Erreur
      CALL RHSBP_SECTION_S(conf%largeur(iext), zref(num_sect), z(&
&                    num_sect), idt(num_sect), xdt(num_sect), profil, &
&                    profilplan%b1, erreur)
      IF (erreur%numero .NE. 0) THEN
        GOTO 210
      ELSE
        CALL PUSHINTEGER4(iext)
        ad_count = ad_count + 1
      END IF
    END DO label_ext
    CALL PUSHCONTROL2B(0)
    CALL PUSHINTEGER4(ad_count)
! TEST NBRE BRANCHES AMONT
    IF (nb_branche_amont .GT. 2) THEN
      GOTO 160
    ELSE
! Calcul de Qpl=Qp/Ql
! -------------------
      q_max = 0._DOUBLE
      ad_count0 = 1
      DO iext=1,nb_ext
        IF (conf%position(iext) .EQ. branche_aval) THEN
! BRANCHE AVAL
          conf%nature(iext) = branche_princ_aval
          nb_branche_aval = nb_branche_aval + 1
! TEST DEFLUENCE
          IF (nb_branche_aval .GT. 1) THEN
            GOTO 190
          ELSE
            CALL PUSHCONTROL1B(1)
          END IF
        ELSE
          CALL PUSHCONTROL1B(0)
          IF (conf%debit(iext) .LT. q_max) THEN
            q_max = q_max
          ELSE
            q_max = conf%debit(iext)
          END IF
        END IF
        ad_count0 = ad_count0 + 1
      END DO
      CALL PUSHCONTROL1B(0)
      CALL PUSHINTEGER4(ad_count0)
      DO iext=1,nb_ext
        IF (conf%position(iext) .EQ. branche_amont .AND. conf%debit(iext&
&           ) .EQ. q_max) THEN
          conf%nature(iext) = branche_princ_amont
          CALL PUSHREAL8(qp)
          qp = conf%debit(iext)
          CALL PUSHCONTROL2B(2)
        ELSE IF (conf%position(iext) .EQ. branche_amont .AND. conf%debit&
&           (iext) .LT. q_max) THEN
          conf%nature(iext) = branche_laterale
          CALL PUSHREAL8(ql)
          ql = conf%debit(iext)
          CALL PUSHCONTROL2B(1)
        ELSE
          CALL PUSHCONTROL2B(0)
        END IF
      END DO
      CALL PUSHINTEGER4(iext - 1)
      qpl = qp/ql
! TEST DOMAINE
!-------------
      IF (qpl .GT. 5._DOUBLE) THEN
        IF (unitelisting .GT. 0) WRITE(unitelisting, 10000)
      END IF
! Calcul de Llp=Ll/Lp
! -------------------
      DO iext=1,nb_ext
        IF (conf%nature(iext) .EQ. branche_princ_amont) THEN
          CALL PUSHREAL8(lp)
          lp = conf%largeur(iext)
          CALL PUSHCONTROL1B(0)
        ELSE
          CALL PUSHCONTROL1B(1)
        END IF
        IF (conf%nature(iext) .EQ. branche_laterale) THEN
          CALL PUSHREAL8(ll)
          ll = conf%largeur(iext)
          CALL PUSHCONTROL1B(0)
        ELSE
          CALL PUSHCONTROL1B(1)
        END IF
        IF (conf%nature(iext) .EQ. branche_princ_aval) THEN
          lf = conf%largeur(iext)
          CALL PUSHCONTROL1B(1)
        ELSE
          CALL PUSHCONTROL1B(0)
        END IF
      END DO
      CALL PUSHINTEGER4(iext - 1)
      llp = ll/lp
      CALL PUSHREAL8(elargissement)
      elargissement = lf - lp
! TEST DOMAINE
!-------------
      IF (llp .LT. 0.25_DOUBLE) THEN
        CALL PUSHINTEGER4(iabaque)
        CALL PUSHCONTROL1B(0)
        IF (unitelisting .GT. 0) WRITE(unitelisting, 10010)
      ELSE
        CALL PUSHINTEGER4(iabaque)
        CALL PUSHCONTROL1B(0)
      END IF
      IF (llp .GT. 0.625_DOUBLE) THEN
        IF (unitelisting .GT. 0) WRITE(unitelisting, 10020)
      END IF
! Prise en compte de l'angle (il est en radians dans Confluent)
! -------------------------------------------------------------
      DO iext=1,nb_ext
        IF (conf%nature(iext) .EQ. branche_princ_amont) angle1 = &
&           real(confluent(inoeu)%angleafflu(iext))
        IF (conf%nature(iext) .EQ. branche_laterale) angle2 = real(confluent(&
&           inoeu)%angleafflu(iext))
      END DO
      IF (angle1 - angle2 .GE. 0.) THEN
        angle_conf = angle1 - angle2
      ELSE
        angle_conf = -(angle1-angle2)
      END IF
! CALCUL DES PERTES DE CHARGE
!----------------------------
      DO iabaque=branche_princ_amont,branche_laterale
        CALL PUSHINTEGER4(jabaque)
        ad_count2 = 1
        DO jabaque=abaque_a,abaque_c
          numero_abaque = (iabaque-1)*3 + jabaque
          ad_count1 = 1
! Chargement de C6 pour alpha_C6=30->80 pour Q> & < Qp/Ql
! ----------------------------------------------------
          DO j=1,5-1
            IF (qpl .GE. qc6(j) .AND. qpl .LE. qc6(j+1)) THEN
              GOTO 140
            ELSE
              ad_count1 = ad_count1 + 1
            END IF
          END DO
          CALL PUSHCONTROL1B(0)
          CALL PUSHINTEGER4(ad_count1)
          CALL PUSHCONTROL1B(0)
          GOTO 150
 140      CALL PUSHCONTROL1B(1)
          CALL PUSHINTEGER4(ad_count1)
          CALL PUSHREAL8(qinf)
          qinf = qc6(j)
          CALL PUSHREAL8(qsup)
          qsup = qc6(j+1)
          CALL PUSHCONTROL1B(1)
          DO k=1,6
            c6qinf(k) = abaque(numero_abaque, k, j)
            c6qsup(k) = abaque(numero_abaque, k, j+1)
          END DO
! Calcul de C6a, C6b, C6c
! -----------------------
 150      CALL PUSHREAL8(c6_inf)
          CALL INTERPOLATION_S(c6_inf, angle_conf, 1, alpha_c6, c6qinf, &
&                        6, erreur)
          IF (erreur%numero .NE. 0) THEN
            GOTO 170
          ELSE
            CALL PUSHREAL8(c6_sup)
            CALL INTERPOLATION_S(c6_sup, angle_conf, 1, alpha_c6, c6qsup&
&                          , 6, erreur)
            IF (erreur%numero .NE. 0) THEN
              GOTO 180
            ELSE
! RESULTAT
              rapport = (qpl-qinf)/(qsup-qinf)
              c6abc(jabaque) = (1._DOUBLE-rapport)*c6_inf + rapport*&
&               c6_sup
              CALL PUSHINTEGER4(jabaque)
              ad_count2 = ad_count2 + 1
            END IF
          END IF
        END DO
        CALL PUSHCONTROL2B(0)
        CALL PUSHINTEGER4(ad_count2)
! de jabaque
! Calcul de C6
! ------------
        CALL PUSHREAL8(c6a)
        c6a = c6abc(1)
        CALL PUSHREAL8(c6b)
        c6b = c6abc(2)
        CALL PUSHREAL8(c6c)
        c6c = c6abc(3)
        c6(iabaque) = (llp-0.25_DOUBLE-1.5_DOUBLE*(elargissement/lp))/&
&         .375_DOUBLE*(c6b-c6a) + 4._DOUBLE*(elargissement/lp)*(c6c-c6a)&
&         + c6a
! -----------------------------------
! Prise en compte de C6 dans PDCSing,
! tableau des Pertes de charge
! -----------------------------------
        DO iext=1,nb_ext
          IF (conf%nature(iext) .EQ. iabaque) THEN
            CALL PUSHCONTROL1B(1)
          ELSE
            CALL PUSHCONTROL1B(0)
          END IF
        END DO
        CALL PUSHINTEGER4(iext - 1)
        CALL PUSHINTEGER4(iabaque)
        CALL PUSHCONTROL1B(1)
      END DO
! de iabaque
! FIN DE LA BOUCLE SUR LES CONFLUENCES
! ------------------------------------
      IF (unitelisting .GT. 0) THEN
        CALL PUSHINTEGER4(inoeu)
        CALL PUSHCONTROL1B(1)
        WRITE(unitelisting, 10030) inoeu, c6(1), c6(2)
      ELSE
        CALL PUSHINTEGER4(inoeu)
        CALL PUSHCONTROL1B(1)
      END IF
    END IF
  END DO label_noeud
  qlb = 0.D0
  qpb = 0.D0
  c6b0 = 0.D0
  c6abcb = 0.D0
  lfb = 0.D0
  llb = 0.D0
  lpb = 0.D0
  confb%debit = 0.D0
  confb%largeur = 0.D0
  GOTO 260
 160 erreur%numero = 87
  erreur%ft = err_87
  erreur%ft_c = err_87c
  CALL TRAITER_ERREUR(erreur, nb_branche_amont, inoeu)
  qlb = 0.D0
  qpb = 0.D0
  c6b0 = 0.D0
  c6abcb = 0.D0
  lfb = 0.D0
  llb = 0.D0
  lpb = 0.D0
  confb%debit = 0.D0
  confb%largeur = 0.D0
  GOTO 230
 170 CALL PUSHCONTROL2B(1)
  CALL PUSHINTEGER4(ad_count2)
  GOTO 280
 180 CALL PUSHCONTROL2B(2)
  CALL PUSHINTEGER4(ad_count2)
  GOTO 280
 190 CALL PUSHCONTROL1B(1)
  CALL PUSHINTEGER4(ad_count0)
  erreur%numero = 88
  erreur%ft = err_88
  erreur%ft_c = err_88c
  CALL TRAITER_ERREUR(erreur, nb_branche_aval, inoeu)
  GOTO 320
 200 CALL PUSHCONTROL2B(1)
  CALL PUSHINTEGER4(ad_count)
  erreur%numero = 86
  erreur%ft = err_86
  erreur%ft_c = err_86c
  CALL TRAITER_ERREUR(erreur, inoeu, iext)
  GOTO 230
 210 CALL PUSHCONTROL2B(3)
  CALL PUSHINTEGER4(ad_count)
  GOTO 230
 220 CALL PUSHCONTROL2B(2)
  CALL PUSHINTEGER4(ad_count)
  erreur%numero = 86
  erreur%ft = err_86
  erreur%ft_c = err_86c
  CALL TRAITER_ERREUR(erreur, inoeu, iext)
 230 CALL POPINTEGER4(ad_count)
  DO i=1,ad_count
    IF (i .EQ. 1) THEN
      CALL POPCONTROL2B(branch)
      IF (branch .LT. 2) THEN
        IF (branch .EQ. 0) THEN
          GOTO 250
        ELSE
          qlb = 0.D0
          qpb = 0.D0
          c6b0 = 0.D0
          c6abcb = 0.D0
          lfb = 0.D0
          llb = 0.D0
          lpb = 0.D0
          confb%debit = 0.D0
          confb%largeur = 0.D0
          GOTO 240
        END IF
      ELSE IF (branch .EQ. 2) THEN
        qlb = 0.D0
        qpb = 0.D0
        c6b0 = 0.D0
        c6abcb = 0.D0
        lfb = 0.D0
        llb = 0.D0
        lpb = 0.D0
        confb%debit = 0.D0
        confb%largeur = 0.D0
        GOTO 240
      ELSE
        qlb = 0.D0
        qpb = 0.D0
        c6b0 = 0.D0
        c6abcb = 0.D0
        lfb = 0.D0
        llb = 0.D0
        lpb = 0.D0
        confb%debit = 0.D0
        confb%largeur = 0.D0
      END IF
    END IF
    num_sect = conf%section(iext)
    CALL RHSBP_SECTION_S_B(conf%largeur(iext), confb%largeur(iext), zref&
&                    (num_sect), z(num_sect), zb(num_sect), idt(num_sect&
&                    ), xdt(num_sect), profil, profilplan%b1, erreur)
    confb%largeur(iext) = 0.D0
    qb(conf%section(iext)) = qb(conf%section(iext)) + confb%debit(iext)
    confb%debit(iext) = 0.D0
    CALL POPCONTROL2B(branch)
 240 CALL POPCONTROL3B(branch)
    IF (branch .LT. 2) THEN
      IF (branch .EQ. 0) THEN
        CALL POPINTEGER4(conf%section(iext))
      ELSE
        CALL POPINTEGER4(conf%section(iext))
      END IF
    ELSE IF (branch .EQ. 2) THEN
      CALL POPINTEGER4(conf%section(iext))
    ELSE IF (branch .EQ. 3) THEN
      CALL POPINTEGER4(conf%section(iext))
    END IF
 250 CALL POPINTEGER4(iext)
  END DO
 260 CALL POPCONTROL1B(branch)
  IF (branch .EQ. 0) THEN
    GOTO 330
  ELSE
    CALL POPINTEGER4(inoeu)
    llp = ll/lp
    elargissementb = 0.D0
    llpb = 0.D0
    qplb = 0.D0
  END IF
 270 CALL POPCONTROL1B(branch)
  IF (branch .EQ. 0) THEN
    GOTO 310
  ELSE
    CALL POPINTEGER4(iabaque)
    CALL POPINTEGER4(ad_to3)
    DO iext=ad_to3,1,-1
      CALL POPCONTROL1B(branch)
      IF (branch .NE. 0) THEN
        c6b0(iabaque) = c6b0(iabaque) + pcsingb(conf%section(iext))
        pcsingb(conf%section(iext)) = 0.D0
      END IF
    END DO
    tempb = (c6b-c6a)*c6b0(iabaque)/.375_DOUBLE
    tempb0 = -(1.5_DOUBLE*tempb/lp)
    tempb1 = (llp-1.5_DOUBLE*(elargissement/lp)-0.25_DOUBLE)*c6b0(&
&     iabaque)/.375_DOUBLE
    tempb2 = 4._DOUBLE*elargissement*c6b0(iabaque)/lp
    tempb3 = (c6c-c6a)*4._DOUBLE*c6b0(iabaque)/lp
    llpb = llpb + tempb
    elargissementb = elargissementb + tempb3 + tempb0
    lpb = lpb - elargissement*tempb3/lp - elargissement*tempb0/lp
    c6bb = tempb1
    c6ab = c6b0(iabaque) - tempb2 - tempb1
    c6cb = tempb2
    c6b0(iabaque) = 0.D0
    CALL POPREAL8(c6c)
    c6abcb(3) = c6abcb(3) + c6cb
    CALL POPREAL8(c6b)
    c6abcb(2) = c6abcb(2) + c6bb
    CALL POPREAL8(c6a)
    c6abcb(1) = c6abcb(1) + c6ab
  END IF
 280 CALL POPINTEGER4(ad_count2)
  DO i2=1,ad_count2
    IF (i2 .EQ. 1) THEN
      CALL POPCONTROL2B(branch)
      IF (branch .EQ. 0) THEN
        GOTO 300
      ELSE IF (branch .EQ. 1) THEN
        qlb = 0.D0
        qpb = 0.D0
        c6b0 = 0.D0
        elargissementb = 0.D0
        c6abcb = 0.D0
        llpb = 0.D0
        lfb = 0.D0
        llb = 0.D0
        lpb = 0.D0
        qplb = 0.D0
        confb%debit = 0.D0
        confb%largeur = 0.D0
        GOTO 290
      ELSE
        qlb = 0.D0
        qpb = 0.D0
        c6b0 = 0.D0
        elargissementb = 0.D0
        c6abcb = 0.D0
        llpb = 0.D0
        lfb = 0.D0
        llb = 0.D0
        lpb = 0.D0
        qplb = 0.D0
        confb%debit = 0.D0
        confb%largeur = 0.D0
      END IF
    ELSE
      rapportb = (c6_sup-c6_inf)*c6abcb(jabaque)
      c6abcb(jabaque) = 0.D0
      qplb = qplb + rapportb/(qsup-qinf)
    END IF
    CALL POPREAL8(c6_sup)
 290 CALL POPREAL8(c6_inf)
    CALL POPCONTROL1B(branch)
    IF (branch .NE. 0) THEN
      CALL POPREAL8(qsup)
      CALL POPREAL8(qinf)
    END IF
    CALL POPINTEGER4(ad_count1)
    DO i1=1,ad_count1
      IF (i1 .EQ. 1) CALL POPCONTROL1B(branch)
    END DO
 300 CALL POPINTEGER4(jabaque)
  END DO
  GOTO 270
 310 CALL POPINTEGER4(iabaque)
  CALL POPREAL8(elargissement)
  lfb = lfb + elargissementb
  lpb = lpb - ll*llpb/lp**2 - elargissementb
  llb = llb + llpb/lp
  CALL POPINTEGER4(ad_to2)
  DO iext=ad_to2,1,-1
    CALL POPCONTROL1B(branch)
    IF (branch .NE. 0) THEN
      confb%largeur(iext) = confb%largeur(iext) + lfb
      lfb = 0.D0
    END IF
    CALL POPCONTROL1B(branch)
    IF (branch .EQ. 0) THEN
      CALL POPREAL8(ll)
      confb%largeur(iext) = confb%largeur(iext) + llb
      llb = 0.D0
    END IF
    CALL POPCONTROL1B(branch)
    IF (branch .EQ. 0) THEN
      CALL POPREAL8(lp)
      confb%largeur(iext) = confb%largeur(iext) + lpb
      lpb = 0.D0
    END IF
  END DO
  qpb = qpb + qplb/ql
  qlb = qlb - qp*qplb/ql**2
  CALL POPINTEGER4(ad_to1)
  DO iext=ad_to1,1,-1
    CALL POPCONTROL2B(branch)
    IF (branch .NE. 0) THEN
      IF (branch .EQ. 1) THEN
        CALL POPREAL8(ql)
        confb%debit(iext) = confb%debit(iext) + qlb
        qlb = 0.D0
      ELSE
        CALL POPREAL8(qp)
        confb%debit(iext) = confb%debit(iext) + qpb
        qpb = 0.D0
      END IF
    END IF
  END DO
 320 CALL POPINTEGER4(ad_count0)
  DO i0=1,ad_count0
    IF (i0 .EQ. 1) THEN
      CALL POPCONTROL1B(branch)
      IF (branch .NE. 0) THEN
        qlb = 0.D0
        qpb = 0.D0
        c6b0 = 0.D0
        c6abcb = 0.D0
        lfb = 0.D0
        llb = 0.D0
        lpb = 0.D0
        confb%debit = 0.D0
        confb%largeur = 0.D0
      END IF
    ELSE
      CALL POPCONTROL1B(branch)
    END IF
  END DO
  GOTO 230
 330 CALL POPINTEGER4(inoeu)
10000 FORMAT( /'<<ATTENTION>> : Sous-programme CALC_PC_CONFLU',/, &
&        '--------------- Qp/Ql > 5, hors domaine de validite')
10010 FORMAT( /'<<ATTENTION>> : Sous-programme CALC_PC_CONFLU',/, &
&        '--------------- Ll/Lp < 0.25, hors domaine de validite')
10020 FORMAT( /'<<ATTENTION>> : Sous-programme CALC_PC_CONFLU',/, &
&        '--------------- Ll/Lp > 0.625, hors domaine de validite')
10030 FORMAT( /&
&  '<<INFO>> : Calcul automatique des pertes de charge aux confluences.'&
&        ,/, '---------- A la confluence n0 : ',i3, /, &
&        '           PDCSing branche principale = ',f12.3, /, &
&        '           PDCSing branche laterale   = ',f12.3)
END SUBROUTINE CALC_PC_CONFLU_B






SUBROUTINE QNODE_B(q, qb, z, zb, numconfluence, numpassage, connect, &
& erreur)
! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             S. MANDELKERN
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
!
!  FONCTION :
!  --------
!
!        REPARTITION DES DEBITS A UN NOEUD AVEC EGALITE DES COTES :
!
!          CAS AVEC UNE SEULE BRANCHE AVAL
!                   ---------
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! ._______________.____.____._______________________________________________
! !    NOM        !TYPE!MODE!                   ROLE
! !_______________!____!____!______________________________________________
! ! Q             ! R  !<-->! TABLEAU DES DEBITS
! ! Z             ! R  !<-->! TABLEAU DES COTES DE LA SURFACE LIBRE
! ! NumConfluence ! I  ! -->! Numero de la confluence a traiter
! ! NumPassage    ! I  ! -->! VARIABLE INDICATRICE :
! !               !    !    !  = 1  CALCUL DU DEBIT DANS LA BRANCHE AVAL
! !               !    !    !  = 2  CALCUL DES COTES DANS LES BRANCHES AMONT
! ! Connect       ! T  ! -->! Structure contenant la table de connectivite
! ! Erreur        ! T  !<-->! ERREUR
! !_______________!____!____!______________________________________________
!
!                         VARIABLES LOCALES
! ._______________.____.____.______________________________________________
! !               !    !    !
! !_______________!____!____!______________________________________________
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ----------------------
!   SOUS PROGRAMME APPELANT :  PERSAR
!   -------------------------
!   SOUS PROGRAMMES APPELES :  ---
!   -------------------------
!
!***********************************************************************
!============================= Declarations ===========================
!.. Modules importes ..
!----------------------
  USE M_PRECISION
! Messages d'erreur
  USE M_MESSAGE_C
! Type CONNECT_T
  USE M_CONNECT_T
! Type ERREUR_T
  USE M_ERREUR_T
! Traitement de l'erreur
  USE M_TRAITER_ERREUR_I
  IMPLICIT NONE
!.. Arguments ..
!---------------
! TABLEAUX DIMENSIONNES A NbSection
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: q
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: qb
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: z
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: zb
  INTEGER, INTENT(IN) :: numconfluence
  INTEGER, INTENT(IN) :: numpassage
  TYPE(CONNECT_T), INTENT(IN) :: connect
  TYPE(ERREUR_T), INTENT(INOUT) :: erreur
!.. Variables locales ..
!-----------------------
  INTEGER :: ibief
  INTEGER :: isec
  INTEGER :: ii1
  INTEGER :: num_sect
  LOGICAL :: type_origine
  DOUBLE PRECISION :: qamontb
  INTRINSIC SIZE
  INTEGER :: branch
  DOUBLE PRECISION :: tmpb
!character(132) :: !arbredappel_old
!============================= Instructions ===========================
!arbredappel_old    = trim(!Erreur%arbredappel)
!Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>QNODE'
  IF (numpassage .EQ. 1) THEN
    DO ibief=1,connect%nbbiefconfluence(numconfluence)
      num_sect = connect%numsectionconfluence(numconfluence, ibief)
! On cherche si la section num_sect est une origine ou une fin du bief
      type_origine = .false.
      DO isec=1,SIZE(connect%originebief)
        IF (connect%originebief(isec) .EQ. num_sect) GOTO 100
      END DO
      GOTO 110
 100  type_origine = .true.
 110  IF (type_origine) THEN
        CALL PUSHCONTROL1B(1)
! Il s'agit de la branche aval unique
        ii1 = num_sect
      ELSE
        CALL PUSHCONTROL1B(0)
      END IF
    END DO
    qamontb = qb(ii1)
    qb(ii1) = 0.D0
    DO ibief=connect%nbbiefconfluence(numconfluence),1,-1
      CALL POPCONTROL1B(branch)
      IF (branch .EQ. 0) THEN
        num_sect = connect%numsectionconfluence(numconfluence, ibief)
        qb(num_sect) = qb(num_sect) + qamontb
      END IF
    END DO
  ELSE IF (numpassage .EQ. 2) THEN
!========================================
!  SECOND PASSAGE :
! ON ECRIT L'EGALITE DES COTES AUX NOEUDS
!========================================
!    PREMIER PARCOURS DU NOEUD POUR DETERMINER LA BRANCHE AVAL
    DO ibief=1,connect%nbbiefconfluence(numconfluence)
      num_sect = connect%numsectionconfluence(numconfluence, ibief)
! On cherche si la section num_sect est une origine ou une fin du bief
      type_origine = .false.
      DO isec=1,SIZE(connect%originebief)
        IF (connect%originebief(isec) .EQ. num_sect) GOTO 120
      END DO
      GOTO 130
 120  type_origine = .true.
 130  IF (type_origine) ii1 = num_sect
! -- BRANCHE AVAL --
    END DO
!    SECOND PARCOURS DU NOEUD , ON REPORTE LA COTE A L'AMONT
    DO ibief=1,connect%nbbiefconfluence(numconfluence)
      num_sect = connect%numsectionconfluence(numconfluence, ibief)
! On cherche si la section num_sect est une origine ou une fin du bief
      type_origine = .false.
      DO isec=1,SIZE(connect%originebief)
        IF (connect%originebief(isec) .EQ. num_sect) GOTO 140
      END DO
      GOTO 150
 140  type_origine = .true.
 150  IF (.NOT.type_origine) THEN
        CALL PUSHCONTROL1B(1)
      ELSE
        CALL PUSHCONTROL1B(0)
      END IF
    END DO
    DO ibief=connect%nbbiefconfluence(numconfluence),1,-1
      CALL POPCONTROL1B(branch)
      IF (branch .NE. 0) THEN
        num_sect = connect%numsectionconfluence(numconfluence, ibief)
        tmpb = zb(num_sect)
        zb(num_sect) = 0.D0
        zb(ii1) = zb(ii1) + tmpb
      END IF
    END DO
  END IF
END SUBROUTINE QNODE_B




SUBROUTINE QREPAR_B(sommedebitance, sommedebitanceb, zaval, zavalb, &
& numpassage, q, qb, z, zb, zref, x, cf1, cf1b, cf2, cf2b, idt, xdt, &
& profil, profil_plan, numconfluence, connect, modelelit, epsil, dzprev&
& , unitelisting, loifrottement, erreur)
! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P. CHERUBINI
!                             S. PERON
!                             S. MANDELKERN
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
!
!   FONCTION :
!   --------
!
!   REPARTITION DES DEBITS AU NOEUD NumConfluence
!   CAS DE PLUSIEURS BRANCHES AVAL
!
! ----------------------------------------------------------------------
! ARGUMENTS
! ._______________.____.____._______________________________________________
! ! NOM           !TYPE!MODE! ROLE
! !_______________!____!____!______________________________________________
! ! SommeDebitance! R  !<-- ! SOMME DES DEBITANCES DES BRANCHES AVAL
! ! ZAval         ! R  !<-- ! TABLEAU DE COTE (INTERNE A QREPAR)
! ! NumPassage    ! I  !<-->! VARIABLE INDICATRICE :
! !               !    !    !  = 1  CALCUL DU DEBIT DANS LA BRANCHE AVAL
! !               !    !    !  = 2  CALCUL DES COTES DANS LES BRANCHES AMONT
! ! Q             ! R  !<-->! TABLEAU DES DEBITS
! ! Z             ! R  ! -->! TABLEAU DES COTES DE LA SURFACE LIBRE
! ! ZREF          ! R  ! -->! COTES DU FOND DU LIT
! ! X             ! R  ! -->! ABSCISSES DES SECTIONS DE CALCUL
! ! CF1           ! R  ! -->! COEF. DE STRICKLER , LIT MINEUR
! ! CF2           ! R  ! -->! COEF. DE STRICKLER , LIT MAJEUR
! ! IDT           ! I  ! -->!  )PERMET DE DETERMINER LA SECTION DE CALCUL A
! ! XDT           ! R  ! -->!  )PARTIR DES SECTIONS DE DONNEES
! ! Profil        ! T  ! -->! Caracteristiques du planimetrage d'un profil
! ! Profil_plan   ! R  ! -->! Variables de profil planimetrees
! ! NumConfluence ! I  ! -->! NUMERO DU NOEUD A TRAITER
! ! Connect       ! T  ! -->! Structure contenant la table de connectivite
! ! ModeleLit     ! I  ! -->! Modele du lit
! !_______________!____!____!______________________________________________
!
! VARIABLES LOCALES
! ._______________.____.____.______________________________________________
! !               !    !    !
! !_______________!____!____!______________________________________________
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ----------------------
!
!   SOUS PROGRAMME APPELANT :  PERSAR
!   -------------------------
!   SOUS PROGRAMMES APPELES :
!   -------------------------
!
!   INTERPOLATION_S : INTERPOLATION DE LAGRANGE D'ODRE N
!
!   REPAR           : CALCUL DE LA REPARTITION DES DEBITS ENTRE
!                     LE LIT MINEUR ET LE LIT MAJEUR
!
!   RHSBP_S :  CALCUL DES PARAMETRES DE LA GEOMETRIE D'UNE
!              SECTION
!
! ----------------------------------------------------------------------
!============================= Declarations ===========================
!.. Modules importes ..
!----------------------
  USE M_PRECISION
! Messages d'erreur
  USE M_MESSAGE_C
! Types derives
! Type CONNECT_T
  USE M_CONNECT_T
! Type PROFIL_T
  USE M_PROFIL_T
! Type PROFIL_PLAN_T
  USE M_PROFIL_PLAN_T
! Type ERREUR_T
  USE M_ERREUR_T
! Procedures-module
! Sous-programme INTERPOLATION_S
  USE M_INTERPOLATION_S
  USE M_INTERPOLATION_S_B
! Sous-programme RHSBP_GENERIQUE_S
  USE M_RHSBP_S
  USE M_RHSBP_S_B
! Traitement de l'erreur
  USE M_TRAITER_ERREUR_I
! Interfaces
  USE M_REPAR_I_B
  IMPLICIT NONE
!.. Arguments ..
! --------------
! TABLEAUX DIMENSIONNES A NMPLAN
  DOUBLE PRECISION, DIMENSION(:) :: sommedebitance
  DOUBLE PRECISION, DIMENSION(:) :: sommedebitanceb
  DOUBLE PRECISION, DIMENSION(:) :: zaval
  DOUBLE PRECISION, DIMENSION(:) :: zavalb
  INTEGER, INTENT(INOUT) :: numpassage
! TABLEAU  DIMENSIONNE  A NbSect
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: q
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: qb
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: z
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: zb
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: zref
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: x
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf1
  DOUBLE PRECISION, DIMENSION(:) :: cf1b
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf2
  DOUBLE PRECISION, DIMENSION(:) :: cf2b
  INTEGER, DIMENSION(:), INTENT(IN) :: idt
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: xdt
  TYPE(PROFIL_T), DIMENSION(:), INTENT(IN) :: profil
  TYPE(PROFIL_PLAN_T), INTENT(IN) :: profil_plan
  INTEGER, INTENT(IN) :: numconfluence
  TYPE(CONNECT_T), INTENT(IN) :: connect
  INTEGER, INTENT(IN) :: modelelit
  DOUBLE PRECISION, INTENT(OUT) :: epsil
  DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: dzprev
  INTEGER, INTENT(IN) :: unitelisting
  INTEGER, INTENT(IN) :: loifrottement
  TYPE(ERREUR_T), INTENT(INOUT) :: erreur
!.. Constantes ..
!----------------
! PENTMI : VALEUR MINIMUM SIGNIFICATIVE DE LA PENTE
! EPS    : COEFFICIENT UTILISE POUR MESURER LES VARIATIONS
!          SIGNIFICATIVES DE COTE
  DOUBLE PRECISION, PARAMETER :: pentmi=1.0e-6_DOUBLE
  DOUBLE PRECISION, PARAMETER :: eps=0.5e-3_DOUBLE
!.. Variables locales ..
!-----------------------
! A DIMENSIONNER AU NOMBRE DE BIEFS A UN NOEUD
! En pratique a NBBIEF
  INTEGER :: i2am(SIZE(connect%originebief))
  INTEGER :: i1av(SIZE(connect%originebief))
  INTEGER :: i2av(SIZE(connect%originebief))
  DOUBLE PRECISION :: qddz(SIZE(connect%originebief))
  DOUBLE PRECISION :: qddzb(SIZE(connect%originebief))
  DOUBLE PRECISION :: delz(SIZE(connect%originebief))
  DOUBLE PRECISION :: delzb(SIZE(connect%originebief))
  DOUBLE PRECISION :: b1, b2
  DOUBLE PRECISION :: b1b, b2b
  DOUBLE PRECISION :: beta
  DOUBLE PRECISION :: betab
  DOUBLE PRECISION :: bst
  DOUBLE PRECISION :: ddz
  DOUBLE PRECISION :: deb
  DOUBLE PRECISION :: debb
  DOUBLE PRECISION :: debm1
  DOUBLE PRECISION :: debm1b
  DOUBLE PRECISION :: debp1
  DOUBLE PRECISION :: debp1b
  DOUBLE PRECISION :: delqb
  DOUBLE PRECISION :: derivd
  DOUBLE PRECISION :: derivdb
  DOUBLE PRECISION :: p1, p2
  DOUBLE PRECISION :: p1b, p2b
  DOUBLE PRECISION :: pente
  DOUBLE PRECISION :: qamont
  DOUBLE PRECISION :: qamontb
! Debitance
  DOUBLE PRECISION :: qampen
  DOUBLE PRECISION :: qampenb
  DOUBLE PRECISION :: q2
  DOUBLE PRECISION :: q1
  DOUBLE PRECISION :: rh1, rh2
  DOUBLE PRECISION :: rh1b, rh2b
  DOUBLE PRECISION :: s1, s2
  DOUBLE PRECISION :: s1b, s2b
  DOUBLE PRECISION :: sdelqb
  DOUBLE PRECISION :: sdelz
  DOUBLE PRECISION :: sqb
  DOUBLE PRECISION :: sqddz
  DOUBLE PRECISION :: sqddzb
  DOUBLE PRECISION :: sqddzi
  DOUBLE PRECISION :: sqddzib
  DOUBLE PRECISION :: vmoy
  DOUBLE PRECISION :: vmoyb
  DOUBLE PRECISION :: z1
  DOUBLE PRECISION :: z1b
  DOUBLE PRECISION :: zf
  DOUBLE PRECISION :: zmax
  DOUBLE PRECISION :: zmaxi
  DOUBLE PRECISION :: znode
  DOUBLE PRECISION :: znodeb
  INTEGER :: i
  INTEGER :: j
  INTEGER :: javal, jamont
  INTEGER :: k
  INTEGER :: k1
  INTEGER :: nbaval
  INTEGER :: nbavq
  INTEGER :: nbamon
  INTEGER :: nb_pas_1
  INTEGER :: nb_pas_profil_max
  INTEGER :: ibief, isec, iprof
  INTEGER :: num_bief, num_sect
  LOGICAL :: type_origine
  INTRINSIC SIZE
  INTRINSIC REAL
  INTRINSIC DMIN1
  INTRINSIC MAX
  INTRINSIC DSQRT
  INTRINSIC ABS
  INTEGER :: arg1
  DOUBLE PRECISION :: result1
  INTEGER :: branch
  INTEGER :: ad_count
  INTEGER :: i0
  INTEGER :: ad_from
  INTEGER :: ad_to0
  INTEGER :: ad_count0
  INTEGER :: i1
  INTEGER :: ad_count1
  INTEGER :: i2
  INTEGER :: ad_count2
  INTEGER :: i3
  INTEGER :: ad_count3
  INTEGER :: i4
  DOUBLE PRECISION :: tempb0
  DOUBLE PRECISION :: tempb
  DOUBLE PRECISION :: abs0
!character(132) :: !arbredappel_old
!============================= Instructions ===========================
! INITIALISATIONS
! ---------------
!arbredappel_old    = trim(!Erreur%arbredappel)
!Erreur%Arbredappel = trim(!Erreur%arbredappel)//'=>QREPAR'
! REPERAGE DES BRANCHES AMONT, DES BRANCHES AVAL, CALCUL DU DEBIT AMONT
! ---------------------------------------------------------------------
  javal = 0
  jamont = 0
  qamont = 0._DOUBLE
  DO ibief=1,connect%nbbiefconfluence(numconfluence)
    num_bief = connect%numbiefconfluence(numconfluence, ibief)
    num_sect = connect%numsectionconfluence(numconfluence, ibief)
! On cherche si la section num_sect est une origine ou une fin du bief
    type_origine = .false.
    DO isec=1,SIZE(connect%originebief)
      IF (connect%originebief(isec) .EQ. num_sect) GOTO 100
    END DO
    GOTO 110
 100 type_origine = .true.
 110 IF (type_origine) THEN
      CALL PUSHCONTROL1B(1)
      javal = javal + 1
      i1av(javal) = connect%originebief(num_bief)
      i2av(javal) = connect%finbief(num_bief)
    ELSE
      CALL PUSHINTEGER4(jamont)
      jamont = jamont + 1
      CALL PUSHINTEGER4(i2am(jamont))
      i2am(jamont) = connect%finbief(num_bief)
      qamont = qamont + q(i2am(jamont))
      CALL PUSHCONTROL1B(0)
    END IF
  END DO
! NBAVAL = NOMBRE DE BRANCHES AVAL
! NBAMON = NOMBRE DE BRANCHES AMONT
  nbaval = javal
  nbamon = jamont
! PREMIER PASSAGE
! ---------------
! CALCUL DES DEBITS DANS LES BRANCHES AVAL EN ECRIVANT L'EGALITE DES
! PENTES DE LIGNE DE CHARGE
  IF (numpassage .EQ. 1) THEN
! RECHERCHE DES COTES DES FONDS ET DES VARIABLES DE PLANIMETRAGE
! POUR DEFINIR UNE PLAGE DE VARIATION
    i = i1av(1)
    zf = zref(i)
    arg1 = profil(idt(i))%nbpas - 1
    result1 = REAL(arg1, double)
    zmax = zref(i) + result1*profil(idt(i))%pas
    DO j=2,nbaval
      i = i1av(j)
      IF (zf .GT. zref(i)) THEN
        zf = zref(i)
      ELSE
        zf = zf
      END IF
      arg1 = profil(idt(i))%nbpas - 1
      result1 = REAL(arg1, double)
      zmaxi = zref(i) + result1*profil(idt(i))%pas
      IF (zmax .GT. zmaxi) THEN
        zmax = zmaxi
      ELSE
        zmax = zmax
      END IF
    END DO
! On cherche le nombre maximal de pas de tous les profils
    nb_pas_profil_max = profil(1)%nbpas
    DO iprof=2,SIZE(profil)
      IF (nb_pas_profil_max .LT. profil(iprof)%nbpas) THEN
        nb_pas_profil_max = profil(iprof)%nbpas
      ELSE
        nb_pas_profil_max = nb_pas_profil_max
      END IF
    END DO
    CALL PUSHINTEGER4(iprof - 1)
    arg1 = nb_pas_profil_max - 1
    result1 = REAL(arg1, double)
    ddz = (zmax-zf)/result1
    epsil = eps*ddz
! CALCUL DES TABLEAUX DE DEBITANCES
    pente = 0._DOUBLE
    DO k=1,nb_pas_profil_max
      sommedebitance(k) = 0._DOUBLE
    END DO
    CALL PUSHCONTROL1B(0)
label_j:DO j=1,nbaval
      CALL PUSHINTEGER4(i)
      i = i1av(j)
      CALL PUSHINTEGER4(k)
      ad_count = 1
label_k:DO k=1,nb_pas_profil_max
        arg1 = k - 1
        result1 = REAL(arg1, double)
        zaval(k) = zf + result1*ddz
        IF (k .EQ. 1) THEN
          CALL PUSHCONTROL1B(0)
        ELSE
          IF (zaval(k) .GT. zref(i) + epsil) THEN
            CALL PUSHREAL8(p2)
            CALL PUSHREAL8(p1)
            CALL RHSBP_S(b1, b2, bst, p1, p2, s1, s2, rh1, rh2, i, zaval&
&                  (k), zref(i), idt, xdt, profil, profil_plan, &
&                  unitelisting, erreur)
            IF (erreur%numero .NE. 0) THEN
              GOTO 140
            ELSE
! Resultats
! Donnees modifiees
! Donnees non modifiees
              CALL PUSHREAL8(rh2)
              CALL PUSHREAL8(rh1)
              CALL PUSHREAL8(s2)
              CALL PUSHREAL8(s1)
              CALL REPAR(deb, vmoy, beta, q1, q2, s1, s2, rh1, rh2, &
&                     p1, p2, q(i), cf1(i), cf2(i), modelelit, &
&                     loifrottement, profil(idt(i))%Nom, erreur)
              IF (erreur%numero .NE. 0) THEN
                GOTO 150
              ELSE
                CALL PUSHCONTROL1B(0)
              END IF
            END IF
          ELSE
            deb = 0._DOUBLE
            CALL PUSHCONTROL1B(1)
          END IF
          sommedebitance(k) = sommedebitance(k) + deb
          CALL PUSHCONTROL1B(1)
        END IF
        CALL PUSHINTEGER4(k)
        ad_count = ad_count + 1
      END DO label_k
      CALL PUSHCONTROL2B(0)
      CALL PUSHINTEGER4(ad_count)
      CALL PUSHCONTROL1B(1)
! EVALUATION DE LA PENTE DE LA LIGNE DE CHARGE
      pente = pente + (zref(i1av(j))-zref(i2av(j)))/(x(i2av(j))-x(i1av(j&
&       )))
    END DO label_j
! INTERPOLATION SUR LE TABLEAU DES DEBITANCES
! EN VERIFIANT QUE LES DEBITANCES CROISSENT BIEN AVEC LES COTES
    pente = pente/nbaval
    IF (pente .LT. pentmi) pente = pentmi
    qampen = qamont/DSQRT(pente)
    IF (qampen .GT. sommedebitance(nb_pas_profil_max)) THEN
      qampen = sommedebitance(nb_pas_profil_max)
      CALL PUSHCONTROL1B(0)
    ELSE
      CALL PUSHCONTROL1B(1)
    END IF
    nb_pas_1 = nb_pas_profil_max
    k1 = 2
    ad_count1 = 0
    DO WHILE (k1 .LT. nb_pas_1)
      ad_count0 = 0
! Si la debitance n'est pas monotone => suppression d'un element du tableau
! et decalage
      DO WHILE (sommedebitance(k1) .LE. sommedebitance(k1-1) .AND. k1 &
&               .LT. nb_pas_1)
        ad_from = k1
        DO k=ad_from,nb_pas_1-1
          zaval(k) = zaval(k+1)
          sommedebitance(k) = sommedebitance(k+1)
        END DO
        CALL PUSHINTEGER4(k - 1)
        CALL PUSHINTEGER4(ad_from)
        nb_pas_1 = nb_pas_1 - 1
        ad_count0 = ad_count0 + 1
      END DO
      CALL PUSHINTEGER4(ad_count0)
      IF (sommedebitance(k1) .LE. sommedebitance(k1-1) .AND. k1 .GE. &
&         nb_pas_1) nb_pas_1 = nb_pas_1 - 1
      k1 = k1 + 1
      ad_count1 = ad_count1 + 1
    END DO
    CALL PUSHINTEGER4(ad_count1)
! Calcul d'une cote de depart ZNODE
    CALL INTERPOLATION_S(znode, qampen, 1, sommedebitance, zaval, &
&                  nb_pas_1, erreur)
    IF (erreur%numero .NE. 0) THEN
      CALL PUSHCONTROL5B(0)
      GOTO 240
    ELSE
      ad_count2 = 1
      DO j=1,nbaval
        CALL PUSHINTEGER4(i)
        i = i1av(j)
        IF (znode .GT. zref(i) + epsil) THEN
          CALL PUSHREAL8(p2)
          CALL PUSHREAL8(p1)
          CALL RHSBP_S(b1, b2, bst, p1, p2, s1, s2, rh1, rh2, i, znode, &
&                zref(i), idt, xdt, profil, profil_plan, unitelisting, &
&                erreur)
          IF (erreur%numero .NE. 0) THEN
            GOTO 120
          ELSE
! Resultats
! Donnees modifiees
! Donnees non modifiees
            CALL PUSHREAL8(rh2)
            CALL PUSHREAL8(rh1)
            CALL PUSHREAL8(s2)
            CALL PUSHREAL8(s1)
            CALL REPAR(deb, vmoy, beta, q1, q2, s1, s2, rh1, rh2, p1&
&                   , p2, q(i), cf1(i), cf2(i), modelelit, loifrottement&
&                   , profil(idt(i))%Nom, erreur)
            IF (erreur%numero .NE. 0) THEN
              GOTO 130
            ELSE
              CALL PUSHCONTROL1B(0)
            END IF
          END IF
        ELSE
! --  PAS D'ECOULEMENT DANS CE BIEF --
          deb = 0._DOUBLE
          CALL PUSHCONTROL1B(1)
        END IF
        CALL PUSHREAL8(q(i))
        q(i) = deb*DSQRT(pente)
! SQ = reste de debit
        ad_count2 = ad_count2 + 1
      END DO
      CALL PUSHCONTROL2B(0)
      CALL PUSHINTEGER4(ad_count2)
! CORRECTION POUR AVOIR EXACTEMENT LA CONTINUITE DES DEBITS
      DO j=1,nbaval
        CALL PUSHINTEGER4(i)
      END DO
      CALL PUSHCONTROL5B(8)
      GOTO 270
 120  CALL PUSHCONTROL2B(1)
      CALL PUSHINTEGER4(ad_count2)
      CALL PUSHCONTROL5B(1)
      GOTO 240
 130  CALL PUSHCONTROL2B(2)
      CALL PUSHINTEGER4(ad_count2)
      CALL PUSHCONTROL5B(2)
      GOTO 250
    END IF
 140 CALL PUSHCONTROL2B(1)
    CALL PUSHINTEGER4(ad_count)
    CALL PUSHCONTROL5B(3)
    GOTO 250
 150 CALL PUSHCONTROL2B(2)
    CALL PUSHINTEGER4(ad_count)
    CALL PUSHCONTROL5B(4)
    GOTO 260
  ELSE
! SECOND PASSAGE
! --------------
! CORRECTION DES DEBITS EN FONCTION DES ECARTS CONSTATES SUR LES COTES
    IF (numpassage .EQ. 2) THEN
      sdelz = 0._DOUBLE
      sqddz = 0._DOUBLE
      sqddzi = 0._DOUBLE
      znode = 0._DOUBLE
! TEST SUR L'EGALITE DES COTES
! ( NBAVQ EST LE NOMBRE DE BIEFS AVAL OU IL EXISTE UN ECOULEMENT )
      nbavq = nbaval
      DO j=1,nbaval
        i = i1av(j)
        IF (z(i) .GT. zref(i) + epsil) THEN
          znode = znode + z(i)
          CALL PUSHCONTROL1B(1)
        ELSE
          CALL PUSHCONTROL1B(0)
!        -- PAS D'ECOULEMENT DANS CETTE BRANCHE --
          nbavq = nbavq - 1
        END IF
      END DO
! DANS LE CAS OU IL Y A UNE BRANCHE SANS ECOULEMENT,
! STOP, SAUF SI LE NOEUD EST SIMPLE ( 2 BRANCHES AVAL )
      IF (nbavq .NE. nbaval .AND. nbaval .GT. 2) THEN
        erreur%numero = 47
        erreur%ft = err_47
        erreur%ft_c = err_47c
        CALL TRAITER_ERREUR(erreur, numconfluence, nbaval)
        CALL PUSHCONTROL5B(5)
        GOTO 260
      ELSE
        result1 = REAL(nbavq, double)
        znode = znode/result1
        DO j=1,nbaval
          i = i1av(j)
          IF (z(i) .GT. zref(i) + epsil) THEN
            delz(j) = znode - z(i)
            IF (delz(j) .GE. 0.) THEN
              CALL PUSHCONTROL1B(1)
              abs0 = delz(j)
            ELSE
              CALL PUSHCONTROL1B(1)
              abs0 = -delz(j)
            END IF
            sdelz = sdelz + abs0
          ELSE
            delz(j) = 0._DOUBLE
            CALL PUSHCONTROL1B(0)
          END IF
        END DO
! SI ON A CONVERGE :
! .CORRECTION DES COTES AMONT
! .SORTIE AVEC NumPassage=999
        IF (sdelz .LT. epsil) THEN
          CALL PUSHCONTROL5B(6)
        ELSE IF (sdelz .GE. dzprev(numconfluence)) THEN
! STOP SI LE RESULTAT N'EST PAS AMELIORE
          erreur%numero = 48
          erreur%ft = err_48
          erreur%ft_c = err_48c
          CALL TRAITER_ERREUR(erreur, numconfluence)
          CALL PUSHCONTROL5B(7)
        ELSE
          CALL PUSHINTEGER4(j)
          ad_count3 = 1
          DO j=1,nbaval
            CALL PUSHINTEGER4(i)
            i = i1av(j)
! CALCUL DE LA DEBITANCE
            CALL PUSHREAL8(p2)
            CALL PUSHREAL8(p1)
            CALL RHSBP_S(b1, b2, bst, p1, p2, s1, s2, rh1, rh2, i, z(i)&
&                  , zref(i), idt, xdt, profil, profil_plan, &
&                  unitelisting, erreur)
            IF (erreur%numero .NE. 0) THEN
              GOTO 160
            ELSE
! Resultats
! Donnees modifiees
! Donnees non modifiees
              CALL PUSHREAL8(rh2)
              CALL PUSHREAL8(rh1)
              CALL PUSHREAL8(s2)
              CALL PUSHREAL8(s1)
              CALL PUSHREAL8(deb)
              CALL REPAR(deb, vmoy, beta, q1, q2, s1, s2, rh1, rh2, &
&                     p1, p2, q(i), cf1(i), cf2(i), modelelit, &
&                     loifrottement, profil(idt(i))%Nom, erreur)
              IF (erreur%numero .NE. 0) THEN
                GOTO 170
              ELSE
! CALCUL DE LA DERIVEE DE LA DEBITANCE PAR RAPPORT A Z
                z1 = z(i) + delz(j)
                CALL PUSHREAL8(p2)
                CALL PUSHREAL8(p1)
                CALL RHSBP_S(b1, b2, bst, p1, p2, s1, s2, rh1, rh2, i, &
&                      z1, zref(i), idt, xdt, profil, profil_plan, &
&                      unitelisting, erreur)
                IF (erreur%numero .NE. 0) THEN
                  GOTO 180
                ELSE
! Resultats
! Donnees modifiees
! Donnees non modifiees
                  CALL PUSHREAL8(rh2)
                  CALL PUSHREAL8(rh1)
                  CALL PUSHREAL8(s2)
                  CALL PUSHREAL8(s1)
                  CALL PUSHREAL8(debp1)
                  CALL REPAR(debp1, vmoy, beta, q1, q2, s1, s2, rh1, &
&                         rh2, p1, p2, q(i), cf1(i), cf2(i), modelelit, &
&                         loifrottement, profil(idt(i))%Nom, erreur)
                  IF (erreur%numero .NE. 0) THEN
                    GOTO 190
                  ELSE
                    z1 = z(i) - delz(j)
                    CALL PUSHREAL8(p2)
                    CALL PUSHREAL8(p1)
                    CALL RHSBP_S(b1, b2, bst, p1, p2, s1, s2, rh1, rh2, &
&                          i, z1, zref(i), idt, xdt, profil, profil_plan&
&                          , unitelisting, erreur)
                    IF (erreur%numero .NE. 0) THEN
                      GOTO 200
                    ELSE
! Resultats
! Donnees modifiees
! Donnees non modifiees
                      CALL PUSHREAL8(rh2)
                      CALL PUSHREAL8(rh1)
                      CALL PUSHREAL8(s2)
                      CALL PUSHREAL8(s1)
                      CALL PUSHREAL8(debm1)
                      CALL REPAR(debm1, vmoy, beta, q1, q2, s1, s2, &
&                             rh1, rh2, p1, p2, q(i), cf1(i), cf2(i), &
&                             modelelit, loifrottement, &
&                             profil(idt(i))%Nom, erreur)
                      IF (erreur%numero .NE. 0) THEN
                        GOTO 210
                      ELSE
                        derivd = (debp1-debm1)/2._DOUBLE/delz(j)
                        CALL PUSHREAL8(qddz(j))
                        qddz(j) = q(i)*derivd/deb
                        sqddz = sqddz + qddz(j)
                        sqddzi = sqddzi + qddz(j)*z(i)
                        CALL PUSHINTEGER4(j)
                        ad_count3 = ad_count3 + 1
                      END IF
                    END IF
                  END IF
                END IF
              END IF
            END IF
          END DO
          CALL PUSHCONTROL3B(0)
          CALL PUSHINTEGER4(ad_count3)
! CALCUL DE LA NOUVELLE COTE A ATTEINDRE, PUIS CORRECTION DES DEBITS
          znode = sqddzi/sqddz
          DO j=1,nbaval
            CALL PUSHINTEGER4(i)
            i = i1av(j)
            CALL PUSHREAL8(delz(j))
            delz(j) = znode - z(i)
          END DO
          DO j=1,nbaval
            CALL PUSHINTEGER4(i)
          END DO
          CALL PUSHCONTROL5B(9)
          GOTO 270
 160      CALL PUSHCONTROL3B(1)
          CALL PUSHINTEGER4(ad_count3)
          CALL PUSHCONTROL5B(10)
          GOTO 380
 170      CALL PUSHCONTROL3B(2)
          CALL PUSHINTEGER4(ad_count3)
          CALL PUSHCONTROL5B(11)
          GOTO 380
 180      CALL PUSHCONTROL3B(3)
          CALL PUSHINTEGER4(ad_count3)
          CALL PUSHCONTROL5B(12)
          GOTO 380
 190      CALL PUSHCONTROL3B(4)
          CALL PUSHINTEGER4(ad_count3)
          CALL PUSHCONTROL5B(13)
          GOTO 380
 200      CALL PUSHCONTROL3B(5)
          CALL PUSHINTEGER4(ad_count3)
          CALL PUSHCONTROL5B(14)
          GOTO 220
 210      CALL PUSHCONTROL3B(6)
          CALL PUSHINTEGER4(ad_count3)
          CALL PUSHCONTROL5B(15)
          GOTO 230
        END IF
      END IF
    ELSE
      CALL PUSHCONTROL5B(16)
! ERREUR DANS L'ALGORITHME
! ------------------------
      erreur%numero = 49
      erreur%ft = err_49
      erreur%ft_c = err_49c
      CALL TRAITER_ERREUR(erreur, numconfluence, numpassage)
      CALL POPCONTROL5B(branch)
      IF (branch .LT. 8) THEN
        IF (branch .LT. 4) THEN
          IF (branch .LT. 2) THEN
            GOTO 240
          ELSE
            GOTO 250
          END IF
        ELSE IF (branch .LT. 6) THEN
          GOTO 260
        END IF
      ELSE IF (branch .LT. 12) THEN
        IF (branch .LT. 10) THEN
          GOTO 270
        ELSE
          GOTO 380
        END IF
      ELSE IF (branch .LT. 14) THEN
        GOTO 380
      ELSE
        GOTO 220
      END IF
    END IF
    IF (branch .EQ. 6) THEN
      znodeb = 0.D0
      DO j=nbamon,1,-1
        i = i2am(j)
        znodeb = znodeb + zb(i)
        zb(i) = 0.D0
      END DO
      delzb = 0.D0
      GOTO 460
    ELSE
      delzb = 0.D0
      GOTO 450
    END IF
 220 IF (branch .EQ. 14) GOTO 380
 230 IF (branch .EQ. 15) THEN
      GOTO 380
    ELSE
      GOTO 480
    END IF
  END IF
 240 IF (branch .EQ. 0) THEN
    qamontb = 0.D0
    debb = 0.D0
    znodeb = 0.D0
    GOTO 320
  ELSE
    GOTO 280
  END IF
 250 IF (branch .EQ. 2) THEN
    GOTO 280
  ELSE
    GOTO 340
  END IF
 260 IF (branch .EQ. 4) THEN
    GOTO 340
  ELSE
    znodeb = 0.D0
    GOTO 470
  END IF
 270 IF (branch .EQ. 8) THEN
    sqb = 0.D0
    DO j=nbaval,1,-1
      i = i1av(j)
      sqb = sqb + qb(i)/nbaval
      CALL POPINTEGER4(i)
    END DO
  ELSE
    qddzb = 0.D0
    delzb = 0.D0
    sdelqb = 0.D0
    DO j=nbaval,1,-1
      i = i1av(j)
      delqb = qb(i)
      qddzb(j) = qddzb(j) + delz(j)*delqb
      delzb(j) = delzb(j) + qddz(j)*delqb
      sdelqb = sdelqb - delqb/nbaval
      CALL POPINTEGER4(i)
    END DO
    znodeb = 0.D0
    DO j=nbaval,1,-1
      delqb = sdelqb
      qddzb(j) = qddzb(j) + delz(j)*delqb
      delzb(j) = delzb(j) + qddz(j)*delqb
      i = i1av(j)
      CALL POPREAL8(delz(j))
      znodeb = znodeb + delzb(j)
      zb(i) = zb(i) - delzb(j)
      delzb(j) = 0.D0
      CALL POPINTEGER4(i)
    END DO
    sqddzib = znodeb/sqddz
    sqddzb = -(sqddzi*znodeb/sqddz**2)
    GOTO 380
  END IF
 280 CALL POPINTEGER4(ad_count2)
  DO 310 i3=1,ad_count2
    IF (i3 .EQ. 1) THEN
      CALL POPCONTROL2B(branch)
      IF (branch .EQ. 0) THEN
        p1b = 0.D0
        p2b = 0.D0
        debb = 0.D0
        znodeb = 0.D0
        GOTO 310
      ELSE IF (branch .EQ. 1) THEN
        s1b = 0.D0
        s2b = 0.D0
        p1b = 0.D0
        p2b = 0.D0
        rh1b = 0.D0
        rh2b = 0.D0
        debb = 0.D0
        znodeb = 0.D0
        sqb = 0.D0
        GOTO 290
      ELSE
        p1b = 0.D0
        p2b = 0.D0
        debb = 0.D0
        znodeb = 0.D0
        sqb = 0.D0
      END IF
    ELSE
      qb(i) = qb(i) - sqb
      CALL POPREAL8(q(i))
      debb = debb + DSQRT(pente)*qb(i)
      qb(i) = 0.D0
      CALL POPCONTROL1B(branch)
      IF (branch .NE. 0) THEN
        debb = 0.D0
        GOTO 300
      END IF
    END IF
    CALL POPREAL8(s1)
    CALL POPREAL8(s2)
    CALL POPREAL8(rh1)
    CALL POPREAL8(rh2)
    vmoyb = 0.D0
    betab = 0.D0
    s1b = 0.D0
    s2b = 0.D0
    CALL REPAR_B(deb, debb, vmoy, vmoyb, beta, betab, q1, q2, s1, s1b, &
&          s2, s2b, rh1, rh1b, rh2, rh2b, p1, p1b, p2, p2b, q(i), qb(i)&
&          , cf1(i), cf1b(i), cf2(i), cf2b(i), modelelit, loifrottement&
&          , erreur)
 290 CALL POPREAL8(p1)
    CALL POPREAL8(p2)
    b1b = 0.D0
    b2b = 0.D0
    CALL RHSBP_S_B(b1, b1b, b2, b2b, bst, p1, p1b, p2, p2b, s1, s1b, s2&
&            , s2b, rh1, rh1b, rh2, rh2b, i, znode, znodeb, zref(i), idt&
&            , xdt, profil, profil_plan, unitelisting, erreur)
 300 CALL POPINTEGER4(i)
 310 CONTINUE
  qamontb = sqb
 320 qampenb = 0.D0
  CALL INTERPOLATION_S_B(znode, znodeb, qampen, qampenb, 1, &
&                  sommedebitance, sommedebitanceb, zaval, zavalb, &
&                  nb_pas_1, erreur)
  CALL POPINTEGER4(ad_count1)
  DO i2=1,ad_count1
    CALL POPINTEGER4(ad_count0)
    DO i1=1,ad_count0
      CALL POPINTEGER4(ad_from)
      CALL POPINTEGER4(ad_to0)
      DO k=ad_to0,ad_from,-1
        sommedebitanceb(k+1) = sommedebitanceb(k+1) + sommedebitanceb(k)
        sommedebitanceb(k) = 0.D0
      END DO
    END DO
  END DO
  CALL POPCONTROL1B(branch)
  IF (branch .EQ. 0) THEN
    sommedebitanceb(nb_pas_profil_max) = sommedebitanceb(&
&     nb_pas_profil_max) + qampenb
    qampenb = 0.D0
  END IF
  qamontb = qamontb + qampenb/DSQRT(pente)
 330 CALL POPCONTROL1B(branch)
  IF (branch .EQ. 0) GOTO 370
 340 CALL POPINTEGER4(ad_count)
  DO i0=1,ad_count
    IF (i0 .EQ. 1) THEN
      CALL POPCONTROL2B(branch)
      IF (branch .EQ. 0) THEN
        GOTO 360
      ELSE IF (branch .EQ. 1) THEN
        qamontb = 0.D0
        debb = 0.D0
        GOTO 350
      ELSE
        qamontb = 0.D0
        debb = 0.D0
      END IF
    ELSE
      CALL POPCONTROL1B(branch)
      IF (branch .EQ. 0) THEN
        GOTO 360
      ELSE
        debb = debb + sommedebitanceb(k)
        CALL POPCONTROL1B(branch)
        IF (branch .NE. 0) THEN
          debb = 0.D0
          GOTO 360
        END IF
      END IF
    END IF
    CALL POPREAL8(s1)
    CALL POPREAL8(s2)
    CALL POPREAL8(rh1)
    CALL POPREAL8(rh2)
    vmoyb = 0.D0
    betab = 0.D0
    s1b = 0.D0
    s2b = 0.D0
    p1b = 0.D0
    p2b = 0.D0
    CALL REPAR_B(deb, debb, vmoy, vmoyb, beta, betab, q1, q2, s1, s1b, &
&          s2, s2b, rh1, rh1b, rh2, rh2b, p1, p1b, p2, p2b, q(i), qb(i)&
&          , cf1(i), cf1b(i), cf2(i), cf2b(i), modelelit, loifrottement&
&          , erreur)
 350 CALL POPREAL8(p1)
    CALL POPREAL8(p2)
 360 CALL POPINTEGER4(k)
  END DO
  CALL POPINTEGER4(i)
  GOTO 330
 370 DO k=nb_pas_profil_max,1,-1
    sommedebitanceb(k) = 0.D0
  END DO
  GOTO 490
 380 CALL POPINTEGER4(ad_count3)
  DO i4=1,ad_count3
    IF (i4 .EQ. 1) THEN
      CALL POPCONTROL3B(branch)
      IF (branch .LT. 3) THEN
        IF (branch .EQ. 0) THEN
          p1b = 0.D0
          p2b = 0.D0
          debb = 0.D0
          debp1b = 0.D0
          debm1b = 0.D0
          GOTO 440
        ELSE IF (branch .EQ. 1) THEN
          qddzb = 0.D0
          s1b = 0.D0
          s2b = 0.D0
          sqddzb = 0.D0
          p1b = 0.D0
          p2b = 0.D0
          rh1b = 0.D0
          rh2b = 0.D0
          sqddzib = 0.D0
          debb = 0.D0
          delzb = 0.D0
          debp1b = 0.D0
          debm1b = 0.D0
          GOTO 430
        ELSE
          qddzb = 0.D0
          sqddzb = 0.D0
          p1b = 0.D0
          p2b = 0.D0
          sqddzib = 0.D0
          debb = 0.D0
          delzb = 0.D0
          debp1b = 0.D0
          debm1b = 0.D0
          GOTO 420
        END IF
      ELSE IF (branch .LT. 5) THEN
        IF (branch .EQ. 3) THEN
          qddzb = 0.D0
          s1b = 0.D0
          s2b = 0.D0
          sqddzb = 0.D0
          p1b = 0.D0
          p2b = 0.D0
          rh1b = 0.D0
          rh2b = 0.D0
          sqddzib = 0.D0
          debb = 0.D0
          delzb = 0.D0
          debp1b = 0.D0
          debm1b = 0.D0
          GOTO 410
        ELSE
          qddzb = 0.D0
          sqddzb = 0.D0
          p1b = 0.D0
          p2b = 0.D0
          sqddzib = 0.D0
          debb = 0.D0
          delzb = 0.D0
          debp1b = 0.D0
          debm1b = 0.D0
          GOTO 400
        END IF
      ELSE IF (branch .EQ. 5) THEN
        qddzb = 0.D0
        s1b = 0.D0
        s2b = 0.D0
        sqddzb = 0.D0
        p1b = 0.D0
        p2b = 0.D0
        rh1b = 0.D0
        rh2b = 0.D0
        sqddzib = 0.D0
        debb = 0.D0
        delzb = 0.D0
        debp1b = 0.D0
        debm1b = 0.D0
        GOTO 390
      ELSE
        qddzb = 0.D0
        sqddzb = 0.D0
        p1b = 0.D0
        p2b = 0.D0
        sqddzib = 0.D0
        debb = 0.D0
        delzb = 0.D0
        debp1b = 0.D0
        debm1b = 0.D0
      END IF
    ELSE
      qddzb(j) = qddzb(j) + sqddzb + z(i)*sqddzib
      zb(i) = zb(i) + qddz(j)*sqddzib
      derivd = (debp1-debm1)/2._DOUBLE/delz(j)
      CALL POPREAL8(qddz(j))
      tempb = q(i)*qddzb(j)/deb
      qb(i) = qb(i) + derivd*qddzb(j)/deb
      derivdb = tempb
      debb = debb - derivd*tempb/deb
      qddzb(j) = 0.D0
      tempb0 = derivdb/(2._DOUBLE*delz(j))
      debp1b = debp1b + tempb0
      debm1b = debm1b - tempb0
      delzb(j) = delzb(j) - (debp1-debm1)*tempb0/delz(j)
    END IF
    CALL POPREAL8(debm1)
    CALL POPREAL8(s1)
    CALL POPREAL8(s2)
    CALL POPREAL8(rh1)
    CALL POPREAL8(rh2)
    vmoyb = 0.D0
    betab = 0.D0
    s1b = 0.D0
    s2b = 0.D0
    CALL REPAR_B(debm1, debm1b, vmoy, vmoyb, beta, betab, q1, q2, s1, &
&          s1b, s2, s2b, rh1, rh1b, rh2, rh2b, p1, p1b, p2, p2b, q(i), &
&          qb(i), cf1(i), cf1b(i), cf2(i), cf2b(i), modelelit, &
&          loifrottement, erreur)
 390 z1 = z(i) - delz(j)
    CALL POPREAL8(p1)
    CALL POPREAL8(p2)
    b1b = 0.D0
    b2b = 0.D0
    z1b = 0.D0
    CALL RHSBP_S_B(b1, b1b, b2, b2b, bst, p1, p1b, p2, p2b, s1, s1b, s2&
&            , s2b, rh1, rh1b, rh2, rh2b, i, z1, z1b, zref(i), idt, xdt&
&            , profil, profil_plan, unitelisting, erreur)
    zb(i) = zb(i) + z1b
    delzb(j) = delzb(j) - z1b
 400 CALL POPREAL8(debp1)
    CALL POPREAL8(s1)
    CALL POPREAL8(s2)
    CALL POPREAL8(rh1)
    CALL POPREAL8(rh2)
    vmoyb = 0.D0
    betab = 0.D0
    s1b = 0.D0
    s2b = 0.D0
    CALL REPAR_B(debp1, debp1b, vmoy, vmoyb, beta, betab, q1, q2, s1, &
&          s1b, s2, s2b, rh1, rh1b, rh2, rh2b, p1, p1b, p2, p2b, q(i), &
&          qb(i), cf1(i), cf1b(i), cf2(i), cf2b(i), modelelit, &
&          loifrottement, erreur)
 410 z1 = z(i) + delz(j)
    CALL POPREAL8(p1)
    CALL POPREAL8(p2)
    b1b = 0.D0
    b2b = 0.D0
    z1b = 0.D0
    CALL RHSBP_S_B(b1, b1b, b2, b2b, bst, p1, p1b, p2, p2b, s1, s1b, s2&
&            , s2b, rh1, rh1b, rh2, rh2b, i, z1, z1b, zref(i), idt, xdt&
&            , profil, profil_plan, unitelisting, erreur)
    zb(i) = zb(i) + z1b
    delzb(j) = delzb(j) + z1b
 420 CALL POPREAL8(deb)
    CALL POPREAL8(s1)
    CALL POPREAL8(s2)
    CALL POPREAL8(rh1)
    CALL POPREAL8(rh2)
    vmoyb = 0.D0
    betab = 0.D0
    s1b = 0.D0
    s2b = 0.D0
    CALL REPAR_B(deb, debb, vmoy, vmoyb, beta, betab, q1, q2, s1, s1b, &
&          s2, s2b, rh1, rh1b, rh2, rh2b, p1, p1b, p2, p2b, q(i), qb(i)&
&          , cf1(i), cf1b(i), cf2(i), cf2b(i), modelelit, loifrottement&
&          , erreur)
 430 CALL POPREAL8(p1)
    CALL POPREAL8(p2)
    b1b = 0.D0
    b2b = 0.D0
    CALL RHSBP_S_B(b1, b1b, b2, b2b, bst, p1, p1b, p2, p2b, s1, s1b, s2&
&            , s2b, rh1, rh1b, rh2, rh2b, i, z(i), zb(i), zref(i), idt, &
&            xdt, profil, profil_plan, unitelisting, erreur)
    CALL POPINTEGER4(i)
 440 CALL POPINTEGER4(j)
  END DO
 450 znodeb = 0.D0
 460 DO j=nbaval,1,-1
    CALL POPCONTROL1B(branch)
    IF (branch .EQ. 0) THEN
      delzb(j) = 0.D0
    ELSE
      i = i1av(j)
      znodeb = znodeb + delzb(j)
      zb(i) = zb(i) - delzb(j)
      delzb(j) = 0.D0
    END IF
  END DO
  znodeb = znodeb/result1
 470 DO j=nbaval,1,-1
    CALL POPCONTROL1B(branch)
    IF (branch .NE. 0) THEN
      i = i1av(j)
      zb(i) = zb(i) + znodeb
    END IF
  END DO
 480 qamontb = 0.D0
 490 DO ibief=connect%nbbiefconfluence(numconfluence),1,-1
    CALL POPCONTROL1B(branch)
    IF (branch .EQ. 0) THEN
      qb(i2am(jamont)) = qb(i2am(jamont)) + qamontb
      CALL POPINTEGER4(i2am(jamont))
      CALL POPINTEGER4(jamont)
    END IF
  END DO
END SUBROUTINE QREPAR_B







SUBROUTINE PERSAR_B(z, zb, q, qb, x, zref, cf1, cf1b, cf2, cf2b, pcsing&
& , pcsingb, idt, xdt, profil, profilplan, f1, qinjec, qinjecb, connect&
& , singularite, extremite, modelelit, confluent, abaque, &
& impression, unitelisting, temps, algorithme, loifrottement, &
& pertechargeconfluent, cqmv, decentrement, erreur)
! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P.CHERUBINI
!                             S. MANDELKERN
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
!   FONCTION :
!   ----------
!
!   CALCUL DE LA LIGNE D'EAU
!
!-----------------------------------------------------------------------
! ARGUMENTS
! .________________.____.____._______________________________________________
! !    NOM         !TYPE!MODE!                   ROLE
! !________________!____!____!_______________________________________________
! ! Z              ! R  !<-- ! COTE DE LA SURFACE LIBRE
! ! Q              ! R  !<-- ! DEBITS
! ! X              ! R  ! -->! ABSCISSE DES SECTIONS DE CALCUL(SUR LE BIEF)
! ! ZREF           ! R  ! -->! COTE DU FOND
! ! CF1,CF2        ! R  ! -->! COEF. DE STRICKLER ,  1= MINEUR   2= MAJEUR
! ! PCSing         ! R  ! -->! PERTE DE CHARGE SINGULIERE
! ! IDT            ! I  ! -->!  )PERMET DE DETERMINER LA SECTION DE CALCUL A
! ! XDT            ! R  ! -->!  )PARTIR DES SECTIONS DE DONNEES
! ! Profil         ! R  ! -->! Caracteristiques du planimetrage d'un profil
! ! ProfilPlan     ! R  ! -->! SECTION MOUILLEE  ZONE DE STOCKAGE
! ! QInjec         ! R  ! -->! DEBIT APPORTE OU SOUTIRE
! ! Connect        ! T  ! -->! Structure contenant la table de connectivite
! ! Singularite    ! T  ! -->! Structure decrivant la singularite
! ! Extremite      ! T  ! -->! Structure decrivant les extremites
! ! ModeleLit      ! I  ! -->! Modele du lit
! ! Impression     ! L  ! -->! Flag d'impression
! ! Temps          ! R  ! -->! Temps
! ! Algorithme     ! I  ! -->! TABLEAUX DES APPELS AUX S.P. RESOLVANT LE
! !                !    !    ! CALCUL DE LIGNES D'EAU :
! !                !    !    !  Algorithme = ISUB + I
! !                !    !    !   AVEC  ISUB = 100  ==>       QBIEF
! !                !    !    !         ISUB = 200  ==>  CALL PERMAT
! !                !    !    !         ISUB = 300  ==>  CALL QNODE (1ER PAS)
! !                !    !    !         ISUB = 400  ==>  CALL QNODE (2EM PAS)
! !                !    !    !         ISUB = 500  ==>  CALL QREPAR(1ER PAS)
! !                !    !    !         ISUB = 600  ==>  CALL QREPAR(2EM PAS)
! !                !    !    !     ET   I = LE NUMERO DU NOEUD OU DU BIEF
! ! Erreur         ! T  !<-->! Erreur
! !________________!____!____!______________________________________________
!
! VARIABLES LOCALES
! .______________________________________________________________________
! !   IA        ! I  ! -- ! COMPTEUR DU TABLEAU DES APPELS
! !   ICOMPT    ! I  ! -- ! COMPTEUR LIMITANT LE NOMBRE D'ITERATION
! !   NumPassage! I  ! -- ! INDICATEUR POUR LES APPELS A QNODE ET QREPAR
! !   noeud_bief! I  ! -- ! NUMERO DU NOEUD OU DU BIEF CONSIDERE
! !SommeDebitance R  ! -->! SOMME DES DEBITANCES DES BRANCHES AVAL
! !   ZAval     ! R  ! -->! TABLEAU DE COTE (INTERNE A QREPAR)
! !_____________!____!____!______________________________________________
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ------------------------
!   SOUS PROGRAMME APPELANT :  SARAP
!   -------------------------
!   SOUS PROGRAMMES APPELES :
!   -------------------------
!
!   - PERMAT :  SOUS-PROGRAMME DU CALCUL DE LA LIGNE D'EAU
!               DANS UN BIEF
!   - QNODE  :  REPARTITION DES DEBITS A UN NOEUD AVEC
!               EGALITE DES COTES ( UNE SEULE BRANCHE AVAL)
!   - QREPAR :  REPARTITION DES DEBITS A UN NOEUD AVEC
!               EGALITE DES COTES (PLUSIEURS BRANCHES AVAL)
!
! **********************************************************************
!============================= Declarations ===========================
!
!.. Modules importes ..
!----------------------
  USE M_PRECISION
! Messages d'erreur
  USE M_MESSAGE_C
! Types derives
! Type confluent
  USE M_CONFLUENT_T
! Definition du Type CONNECT_T
  USE M_CONNECT_T
! Definition du Type EXTREMITE_T
  USE M_EXTREMITE_T
! Definition du Type PROFIL_T
  USE M_PROFIL_T
! Definition du Type PROFIL_PLAN_T
  USE M_PROFIL_PLAN_T
! Definition du type ERREUR_T
  USE M_ERREUR_T
! Definition du Type SINGULARITE_T
  USE M_SINGULARITE_T
! Interfaces
! Calcul des pertes de charge auto aux confluents
  USE M_CALC_PC_CONFLU_I_B
  USE M_PERMAT_I
  USE M_PERMAT_I_B
  USE M_QNODE_I_B
  USE M_QREPAR_I_B
! Traitement de l'erreur
  USE M_TRAITER_ERREUR_I
  IMPLICIT NONE
!.. Arguments ..
! --------------
  DOUBLE PRECISION, DIMENSION(:) :: z
  DOUBLE PRECISION, DIMENSION(:) :: zb
  DOUBLE PRECISION, DIMENSION(:) :: q
  DOUBLE PRECISION, DIMENSION(:) :: qb
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: x
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: zref
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf1
  DOUBLE PRECISION, DIMENSION(:) :: cf1b
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf2
  DOUBLE PRECISION, DIMENSION(:) :: cf2b
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: pcsing
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: pcsingb
  INTEGER, DIMENSION(:), INTENT(IN) :: idt
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: xdt
  TYPE(PROFIL_T), DIMENSION(:), INTENT(IN) :: profil
  TYPE(PROFIL_PLAN_T), INTENT(IN) :: profilplan
  DOUBLE PRECISION, DIMENSION(:, :), INTENT(IN) :: f1
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: qinjec
  DOUBLE PRECISION, DIMENSION(:) :: qinjecb
  TYPE(CONNECT_T), INTENT(IN) :: connect
  TYPE(SINGULARITE_T), DIMENSION(:), INTENT(IN) :: singularite
  TYPE(EXTREMITE_T), DIMENSION(:), INTENT(IN) :: extremite
  INTEGER, INTENT(IN) :: modelelit
  TYPE(CONFLUENT_T), DIMENSION(:), INTENT(IN) :: confluent
  DOUBLE PRECISION, DIMENSION(:, :, :), INTENT(IN) :: abaque
  LOGICAL, INTENT(IN) :: impression
  INTEGER, INTENT(IN) :: unitelisting
  DOUBLE PRECISION, INTENT(IN) :: temps
  INTEGER, DIMENSION(:), INTENT(IN) :: algorithme
  INTEGER, INTENT(IN) :: loifrottement
  LOGICAL, INTENT(IN) :: pertechargeconfluent
  INTEGER, INTENT(IN) :: cqmv
  LOGICAL, INTENT(IN) :: decentrement
  TYPE(ERREUR_T), INTENT(INOUT) :: erreur
!.. Constantes ..
!----------------
  INTEGER, PARAMETER :: npass=5000
  INTEGER, PARAMETER :: appel_qbief=1
  INTEGER, PARAMETER :: appel_permat=2
  INTEGER, PARAMETER :: appel_qnode_1er_pass=3
  INTEGER, PARAMETER :: appel_qnode_2nd_pass=4
  INTEGER, PARAMETER :: appel_qrepar_1er_pass=5
  INTEGER, PARAMETER :: appel_qrepar_2nd_pass=6
!.. Variables locales ..
!-----------------------
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: sommedebitance
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: sommedebitanceb
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: zaval
  INTEGER :: ia
  INTEGER :: icompt
  INTEGER :: numpassage
  INTEGER :: iap
  INTEGER :: isub
  INTEGER :: noeud_bief
  LOGICAL :: limite_libre
  DOUBLE PRECISION :: epsil
  DOUBLE PRECISION, DIMENSION(SIZE(connect%nbbiefconfluence)) :: dzprev
  DOUBLE PRECISION :: zinit
  DOUBLE PRECISION :: zinitb
! Compteur sur les sections
  INTEGER :: isec, nbsect
! Compteur sur les extremites
  INTEGER :: iext
! Compteur sur les profils
  INTEGER :: iprof
! Nombre maximal de pas
  INTEGER :: nb_pas_profil_max
! pour tous les profils
  INTEGER :: nappel
!character(132) :: !arbredappel_old
! code de retour des fonction d'e/s
  INTEGER :: retour
  INTRINSIC SIZE
  INTRINSIC MAX
  INTRINSIC INT
  INTEGER :: ad_from
  INTEGER :: ad_to0
  INTEGER :: branch
  INTEGER :: ad_count
  INTEGER :: i
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: zavalb
!============================= Instructions ===========================
! INITIALISATIONS
!----------------
!arbredappel_old    = trim(!Erreur%arbredappel)
!Erreur%Arbredappel = trim(!Erreur%arbredappel)//'=>PERSAR'
  nappel = SIZE(algorithme)
  nbsect = SIZE(x)
  nb_pas_profil_max = profil(1)%nbpas
  DO iprof=2,SIZE(profil)
    IF (nb_pas_profil_max .LT. profil(iprof)%nbpas) THEN
      nb_pas_profil_max = profil(iprof)%nbpas
    ELSE
      nb_pas_profil_max = nb_pas_profil_max
    END IF
  END DO
  CALL PUSHINTEGER4(iprof - 1)
  ALLOCATE(sommedebitanceb(nb_pas_profil_max), stat=retour)
  ALLOCATE(sommedebitance(nb_pas_profil_max), stat=retour)
  IF (retour .NE. 0) THEN
    erreur%numero = 5
    erreur%ft = err_5
    erreur%ft_c = err_5c
    CALL TRAITER_ERREUR(erreur, 'SommeDebitance')
    CALL PUSHCONTROL4B(0)
  ELSE
    ALLOCATE(zaval(nb_pas_profil_max), stat=retour)
    IF (retour .NE. 0) THEN
      erreur%numero = 5
      erreur%ft = err_5
      erreur%ft_c = err_5c
      CALL TRAITER_ERREUR(erreur, 'Zaval')
      CALL PUSHCONTROL4B(1)
    ELSE
!------------------------------------------
!    RESOLUTION DU CALCUL DE LA LIGNE D'EAU
!    DANS L'ORDRE DU TABLEAU Algorithme
!------------------------------------------
      ia = 0
      icompt = 0
      ad_count = 1
label_ia:DO WHILE (ia .LT. nappel)
        ia = ia + 1
        icompt = icompt + 1
        isub = INT(algorithme(ia)/100)
        CALL PUSHINTEGER4(noeud_bief)
        noeud_bief = algorithme(ia) - 100*isub
        SELECT CASE  (isub)
        CASE (appel_qbief)
! Connect%OrigineBief(noeud_bief) : NUMERO DE LA SECTION ORIGINE DU BIEF
! On cherche l'extremite libre qui correspond
! a la section Connect%OrigineBief(noeud_bief)
          limite_libre = .false.
          DO iext=1,SIZE(connect%numsectionextlibre(:))
            IF (connect%numsectionextlibre(iext) .EQ. connect%&
&               originebief(noeud_bief)) GOTO 100
          END DO
          GOTO 110
! iext : NUMERO DE LA LIMITE LIBRE CORRESPONDANTE
 100      limite_libre = .true.
 110      IF (limite_libre) THEN
!             --  ON PART D'UNE LIMITE LIBRE --
            CALL PUSHREAL8(q(connect%originebief(noeud_bief)))
            q(connect%originebief(noeud_bief)) = extremite(iext)%ptq(1)
! else        --  ON PART D'UN NOEUD --
            CALL PUSHCONTROL1B(1)
          ELSE
            CALL PUSHCONTROL1B(0)
          END IF
          ad_from = connect%originebief(noeud_bief) + 1
          DO isec=ad_from,connect%finbief(noeud_bief)
            CALL PUSHREAL8(q(isec))
            q(isec) = q(isec-1) + qinjec(isec)
          END DO
          CALL PUSHINTEGER4(isec - 1)
          CALL PUSHINTEGER4(ad_from)
          CALL PUSHCONTROL3B(6)
        CASE (appel_permat)
! Calcul des pertes de charge automatique aux confluences
          IF (pertechargeconfluent) THEN
            CALL PUSHCHARACTERARRAY(erreur%message, 400)
            CALL PUSHCHARACTERARRAY(erreur%arbredappel, 132)
            CALL PUSHREAL8ARRAY(pcsing, SIZE(pcsing, 1))
            CALL CALC_PC_CONFLU(pcsing, z, q, x, zref, profil, &
&                            profilplan, confluent, abaque, idt, xdt, &
&                            connect, unitelisting, erreur)
            IF (erreur%numero .NE. 0) THEN
              GOTO 190
            ELSE
              CALL PUSHCONTROL1B(0)
            END IF
          ELSE
            CALL PUSHCONTROL1B(1)
          END IF
! Connect%FinBief(noeud_bief) : NUMERO DE LA SECTION EXTREME DU BIEF
! On cherche l'extremite libre qui correspond
! a la section Connect%FinBief(noeud_bief)
          limite_libre = .false.
          DO iext=1,SIZE(connect%numsectionextlibre(:))
            IF (connect%numsectionextlibre(iext) .EQ. connect%finbief(&
&               noeud_bief)) GOTO 120
          END DO
          GOTO 130
! iext : NUMERO DE LA LIMITE LIBRE CORRESPONDANTE
 120      limite_libre = .true.
 130      IF (limite_libre) THEN
!             --  ON PART D'UNE LIMITE LIBRE --
            CALL PUSHREAL8(zinit)
            zinit = extremite(iext)%ptz(1)
            CALL PUSHCONTROL1B(0)
          ELSE
!             --  ON PART D'UN NOEUD --
            CALL PUSHREAL8(zinit)
            zinit = z(connect%finbief(noeud_bief))
            CALL PUSHCONTROL1B(1)
          END IF
!/RESULTATS/
!/DONNEES NON MODIFIEES/
!/DONNEES NON MODIFIEES
! (ARGUMENTS DE S.P) /
          CALL PUSHCHARACTERARRAY(erreur%message, 400)
          CALL PUSHCHARACTERARRAY(erreur%arbredappel, 132)
          CALL PUSHREAL8ARRAY(z, SIZE(z, 1))
          CALL PERMAT(z, q, zinit, x, zref, cf1, cf2, pcsing, idt, &
&                  xdt, profil, profilplan, f1, connect, noeud_bief, &
&                  nbsect, singularite, modelelit, impression, &
&                  unitelisting, temps, loifrottement, cqmv, decentrement, &
&                  erreur)
!Erreur
          IF (erreur%numero .NE. 0) THEN
            GOTO 200
          ELSE
            CALL PUSHCONTROL3B(5)
          END IF
        CASE (appel_qnode_1er_pass)
          numpassage = 1
!/DONNEES MODIFIEES/
!/DONNEES NON MODIFIEES/
          CALL PUSHREAL8ARRAY(z, SIZE(z, 1))
          CALL PUSHREAL8ARRAY(q, SIZE(q, 1))
          CALL QNODE(q, z, noeud_bief, numpassage, connect, erreur)
          IF (erreur%numero .NE. 0) THEN
            GOTO 180
          ELSE
            CALL PUSHCONTROL3B(4)
          END IF
        CASE (appel_qnode_2nd_pass)
          numpassage = 2
!/DONNEES MODIFIEES/
!/DONNEES NON MODIFIEES/
          CALL PUSHREAL8ARRAY(z, SIZE(z, 1))
          CALL PUSHREAL8ARRAY(q, SIZE(q, 1))
          CALL QNODE(q, z, noeud_bief, numpassage, connect, erreur)
          IF (erreur%numero .NE. 0) THEN
            GOTO 170
          ELSE
            CALL PUSHCONTROL3B(3)
          END IF
        CASE (appel_qrepar_1er_pass)
          numpassage = 1
!/RESULTATS/
!/DONNEES MODIFIEES/
!/DONNEES NON MODIFIEES/
          CALL PUSHCHARACTERARRAY(erreur%message, 400)
          CALL PUSHCHARACTERARRAY(erreur%arbredappel, 132)
          CALL PUSHREAL8ARRAY(dzprev, SIZE(connect%nbbiefconfluence))
          CALL PUSHREAL8(epsil)
          CALL PUSHREAL8ARRAY(z, SIZE(z, 1))
          CALL PUSHREAL8ARRAY(q, SIZE(q, 1))
          CALL PUSHINTEGER4(numpassage)
          CALL PUSHREAL8ARRAY(zaval, SIZE(zaval, 1))
          CALL PUSHREAL8ARRAY(sommedebitance, SIZE(sommedebitance, 1))
          CALL QREPAR(sommedebitance, zaval, numpassage, q, z, zref, &
&                  x, cf1, cf2, idt, xdt, profil, profilplan, noeud_bief&
&                  , connect, modelelit, epsil, dzprev, unitelisting, &
&                  loifrottement, erreur)
          IF (erreur%numero .NE. 0) THEN
            GOTO 160
          ELSE
            CALL PUSHCONTROL3B(2)
          END IF
        CASE (appel_qrepar_2nd_pass)
          numpassage = 2
!/RESULTATS/
!/DONNEES MODIFIEES/
!/DONNEES NON MODIFIEES/
! Unite logisue du fichier listing
          CALL PUSHCHARACTERARRAY(erreur%message, 400)
          CALL PUSHCHARACTERARRAY(erreur%arbredappel, 132)
          CALL PUSHREAL8ARRAY(dzprev, SIZE(connect%nbbiefconfluence))
          CALL PUSHREAL8(epsil)
          CALL PUSHREAL8ARRAY(z, SIZE(z, 1))
          CALL PUSHREAL8ARRAY(q, SIZE(q, 1))
          CALL PUSHINTEGER4(numpassage)
          CALL PUSHREAL8ARRAY(zaval, SIZE(zaval, 1))
          CALL PUSHREAL8ARRAY(sommedebitance, SIZE(sommedebitance, 1))
          CALL QREPAR(sommedebitance, zaval, numpassage, q, z, zref, &
&                  x, cf1, cf2, idt, xdt, profil, profilplan, noeud_bief&
&                  , connect, modelelit, epsil, dzprev, unitelisting, &
&                  loifrottement, erreur)
          IF (erreur%numero .NE. 0) THEN
            GOTO 140
          ELSE IF (numpassage .NE. 999 .AND. icompt .LT. npass) THEN
!  TEST DU RETOUR DE QREPAR :
! . SI NumPassage=999 ON A CONVERGE , LA REPARTITION DE DEBIT EST BONNE
! . SINON IL FAUT ITERER , ON FAIT UN RETOUR AU PREMIER APPEL A QREPAR
!   LE NOMBRE D'ITERATIONS ETANT LIMITE PAR NPASS
            CALL PUSHCONTROL3B(1)
            iap = nappel - ia + 1
            ia = iap
          ELSE IF (icompt .GE. npass) THEN
            GOTO 150
          ELSE
            CALL PUSHCONTROL3B(0)
          END IF
        CASE DEFAULT
          GOTO 210
        END SELECT
        ad_count = ad_count + 1
      END DO label_ia
      CALL PUSHCONTROL4B(0)
      CALL PUSHINTEGER4(ad_count)
!  Desallocation des tableaux locaux
!  ---------------------------------
      IF (retour .NE. 0) THEN
        CALL PUSHCONTROL4B(2)
        erreur%numero = 6
        erreur%ft = err_6
        erreur%ft_c = err_6c
        CALL TRAITER_ERREUR(erreur, 'SommeDebitance')
      ELSE
        CALL PUSHCONTROL4B(2)
        IF (retour .NE. 0) THEN
          erreur%numero = 6
          erreur%ft = err_6
          erreur%ft_c = err_6c
          CALL TRAITER_ERREUR(erreur, 'Zaval')
        END IF
      END IF
      CALL POPCONTROL4B(branch)
      IF (branch .LT. 5) THEN
        IF (branch .LT. 2) THEN
          GOTO 300
        ELSE
          GOTO 220
        END IF
      ELSE
        GOTO 220
      END IF
 140  CALL PUSHCONTROL4B(7)
      CALL PUSHINTEGER4(ad_count)
      CALL PUSHCONTROL4B(8)
      GOTO 220
 150  CALL PUSHCONTROL4B(8)
      CALL PUSHINTEGER4(ad_count)
      erreur%numero = 32
      erreur%ft = err_32
      erreur%ft_c = err_32c
      CALL TRAITER_ERREUR(erreur, noeud_bief)
      CALL PUSHCONTROL4B(9)
      GOTO 220
 160  CALL PUSHCONTROL4B(6)
      CALL PUSHINTEGER4(ad_count)
      CALL PUSHCONTROL4B(7)
      GOTO 220
 170  CALL PUSHCONTROL4B(5)
      CALL PUSHINTEGER4(ad_count)
      CALL PUSHCONTROL4B(6)
      GOTO 220
 180  CALL PUSHCONTROL4B(4)
      CALL PUSHINTEGER4(ad_count)
      CALL PUSHCONTROL4B(5)
      GOTO 220
 190  CALL PUSHCONTROL4B(2)
      CALL PUSHINTEGER4(ad_count)
      CALL PUSHCONTROL4B(3)
      GOTO 220
 200  CALL PUSHCONTROL4B(3)
      CALL PUSHINTEGER4(ad_count)
      CALL PUSHCONTROL4B(4)
      GOTO 220
 210  CALL PUSHCONTROL4B(1)
      CALL PUSHINTEGER4(ad_count)
      erreur%numero = 33
      erreur%ft = err_33
      erreur%ft_c = err_33c
      CALL TRAITER_ERREUR(erreur, ia)
      CALL PUSHCONTROL4B(10)
 220  CALL POPINTEGER4(ad_count)
      DO 290 i=1,ad_count
        IF (i .EQ. 1) THEN
          CALL POPCONTROL4B(branch)
          IF (branch .LT. 4) THEN
            IF (branch .LT. 2) THEN
              IF (branch .EQ. 0) THEN
                sommedebitanceb(1:nb_pas_profil_max) = 0.D0
                qb = 0.D0
                cf1b = 0.D0
                cf2b = 0.D0
                qinjecb = 0.D0
                pcsingb = 0.D0
                GOTO 290
              ELSE
                sommedebitanceb(1:nb_pas_profil_max) = 0.D0
                qb = 0.D0
                cf1b = 0.D0
                cf2b = 0.D0
                qinjecb = 0.D0
                pcsingb = 0.D0
                GOTO 280
              END IF
            ELSE IF (branch .EQ. 2) THEN
              sommedebitanceb(1:nb_pas_profil_max) = 0.D0
              qb = 0.D0
              cf1b = 0.D0
              cf2b = 0.D0
              qinjecb = 0.D0
              pcsingb = 0.D0
              GOTO 270
            ELSE
              sommedebitanceb(1:nb_pas_profil_max) = 0.D0
              qb = 0.D0
              cf1b = 0.D0
              cf2b = 0.D0
              qinjecb = 0.D0
              pcsingb = 0.D0
              GOTO 260
            END IF
          ELSE IF (branch .LT. 6) THEN
            IF (branch .EQ. 4) THEN
              sommedebitanceb(1:nb_pas_profil_max) = 0.D0
              qb = 0.D0
              cf1b = 0.D0
              cf2b = 0.D0
              qinjecb = 0.D0
              pcsingb = 0.D0
              GOTO 250
            ELSE
              sommedebitanceb(1:nb_pas_profil_max) = 0.D0
              qb = 0.D0
              cf1b = 0.D0
              cf2b = 0.D0
              qinjecb = 0.D0
              pcsingb = 0.D0
              GOTO 240
            END IF
          ELSE IF (branch .EQ. 6) THEN
            sommedebitanceb(1:nb_pas_profil_max) = 0.D0
            qb = 0.D0
            cf1b = 0.D0
            cf2b = 0.D0
            qinjecb = 0.D0
            pcsingb = 0.D0
            GOTO 230
          ELSE IF (branch .EQ. 7) THEN
            sommedebitanceb(1:nb_pas_profil_max) = 0.D0
            qb = 0.D0
            cf1b = 0.D0
            cf2b = 0.D0
            qinjecb = 0.D0
            pcsingb = 0.D0
          ELSE
            sommedebitanceb(1:nb_pas_profil_max) = 0.D0
            qb = 0.D0
            cf1b = 0.D0
            cf2b = 0.D0
            qinjecb = 0.D0
            pcsingb = 0.D0
          END IF
        ELSE
          CALL POPCONTROL3B(branch)
          IF (branch .LT. 3) THEN
            IF (branch .NE. 0) THEN
              IF (branch .NE. 1) GOTO 230
            END IF
          ELSE IF (branch .LT. 5) THEN
            IF (branch .EQ. 3) THEN
              GOTO 240
            ELSE
              GOTO 250
            END IF
          ELSE IF (branch .EQ. 5) THEN
            GOTO 260
          ELSE
            CALL POPINTEGER4(ad_from)
            CALL POPINTEGER4(ad_to0)
            DO isec=ad_to0,ad_from,-1
              CALL POPREAL8(q(isec))
              qb(isec-1) = qb(isec-1) + qb(isec)
              qinjecb(isec) = qinjecb(isec) + qb(isec)
              qb(isec) = 0.D0
            END DO
            CALL POPCONTROL1B(branch)
            IF (branch .NE. 0) THEN
              CALL POPREAL8(q(connect%originebief(noeud_bief)))
              qb(connect%originebief(noeud_bief)) = 0.D0
            END IF
            GOTO 280
          END IF
        END IF
        CALL POPREAL8ARRAY(sommedebitance, SIZE(sommedebitance, 1))
        CALL POPREAL8ARRAY(zaval, SIZE(zaval, 1))
        CALL POPINTEGER4(numpassage)
        CALL POPREAL8ARRAY(q, SIZE(q, 1))
        CALL POPREAL8ARRAY(z, SIZE(z, 1))
        CALL POPREAL8(epsil)
        CALL POPREAL8ARRAY(dzprev, SIZE(connect%nbbiefconfluence))
        CALL POPCHARACTERARRAY(erreur%arbredappel, 132)
        CALL POPCHARACTERARRAY(erreur%message, 400)
        CALL QREPAR_B(sommedebitance, sommedebitanceb, zaval, zavalb, &
&               numpassage, q, qb, z, zb, zref, x, cf1, cf1b, cf2, cf2b&
&               , idt, xdt, profil, profilplan, noeud_bief, connect, &
&               modelelit, epsil, dzprev, unitelisting, loifrottement, &
&               erreur)
        GOTO 280
 230    CALL POPREAL8ARRAY(sommedebitance, SIZE(sommedebitance, 1))
        CALL POPREAL8ARRAY(zaval, SIZE(zaval, 1))
        CALL POPINTEGER4(numpassage)
        CALL POPREAL8ARRAY(q, SIZE(q, 1))
        CALL POPREAL8ARRAY(z, SIZE(z, 1))
        CALL POPREAL8(epsil)
        CALL POPREAL8ARRAY(dzprev, SIZE(connect%nbbiefconfluence))
        CALL POPCHARACTERARRAY(erreur%arbredappel, 132)
        CALL POPCHARACTERARRAY(erreur%message, 400)
        CALL QREPAR_B(sommedebitance, sommedebitanceb, zaval, zavalb, &
&               numpassage, q, qb, z, zb, zref, x, cf1, cf1b, cf2, cf2b&
&               , idt, xdt, profil, profilplan, noeud_bief, connect, &
&               modelelit, epsil, dzprev, unitelisting, loifrottement, &
&               erreur)
        GOTO 280
 240    numpassage = 2
        CALL POPREAL8ARRAY(q, SIZE(q, 1))
        CALL POPREAL8ARRAY(z, SIZE(z, 1))
        CALL QNODE_B(q, qb, z, zb, noeud_bief, numpassage, connect, &
&              erreur)
        GOTO 280
 250    numpassage = 1
        CALL POPREAL8ARRAY(q, SIZE(q, 1))
        CALL POPREAL8ARRAY(z, SIZE(z, 1))
        CALL QNODE_B(q, qb, z, zb, noeud_bief, numpassage, connect, &
&              erreur)
        GOTO 280
 260    CALL POPREAL8ARRAY(z, SIZE(z, 1))
        CALL POPCHARACTERARRAY(erreur%arbredappel, 132)
        CALL POPCHARACTERARRAY(erreur%message, 400)
        CALL PERMAT_B(z, zb, q, qb, zinit, zinitb, x, zref, cf1, cf1b, &
&               cf2, cf2b, pcsing, pcsingb, idt, xdt, profil, profilplan&
&               , f1, connect, noeud_bief, nbsect, singularite, &
&                modelelit, impression, unitelisting, temps&
&               , loifrottement, cqmv, decentrement, erreur)
        CALL POPCONTROL1B(branch)
        IF (branch .EQ. 0) THEN
          CALL POPREAL8(zinit)
        ELSE
          CALL POPREAL8(zinit)
          zb(connect%finbief(noeud_bief)) = zb(connect%finbief(&
&           noeud_bief)) + zinitb
        END IF
        CALL POPCONTROL1B(branch)
        IF (branch .NE. 0) GOTO 280
 270    write(*,*) 'PCSING', pcsing, 'SIZE', size(pcsing,1)
        CALL POPREAL8ARRAY(pcsing, SIZE(pcsing, 1))
        CALL POPCHARACTERARRAY(erreur%arbredappel, 132)
        CALL POPCHARACTERARRAY(erreur%message, 400)
        CALL CALC_PC_CONFLU_B(pcsing, pcsingb, z, zb, q, qb, x, zref, &
&                       profil, profilplan, confluent, abaque, idt, xdt&
&                       , connect, unitelisting, erreur)
 280    CALL POPINTEGER4(noeud_bief)
 290  CONTINUE
      GOTO 310
    END IF
  END IF
 300 IF (branch .EQ. 0) THEN
    cf1b = 0.D0
    cf2b = 0.D0
    qinjecb = 0.D0
  ELSE
    cf1b = 0.D0
    cf2b = 0.D0
    qinjecb = 0.D0
  END IF
 310 DEALLOCATE(sommedebitance)
  DEALLOCATE(sommedebitanceb)
END SUBROUTINE PERSAR_B






SUBROUTINE CQINJ_B(qinjec, qinjecb, x, z, zb, apport, deversoir, &
& qdeverse, erreur)
! *********************************************************************
! PROGICIEL : MASCARET        C. RISSOAN      N. GOUTAL
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
!   FONCTION : CALCUL DU TABLEAU QINJEC DES APPORTS (DEBITS + DEVRESOIRS)
!   --------
!
!
! ----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :     - UL_LST : Sortie listing
!   ----------------------
!   SOUS PROGRAMMES APPELANTS :  - REZO, SARAP
!   ---------------------------
!   SOUS PROGRAMMES APPELES :    - INTERPOLATION_S
!   -------------------------
!=========================================================================
!=========================== Declarations ================================
! Type DOUBLE
  USE M_PRECISION
! Parametres de calcul
  USE M_PARAMETRE_C
! Liste des messages d'erreur
  USE M_MESSAGE_C
! Numero du canal UL_LST
  USE M_FICHIER_T
! Definition du type DEVERSOIR_T
  USE M_DEVERSOIR_T
! Definition du type APPORT_T
  USE M_APPORT_T
! Definition du type ERREUR_T
  USE M_ERREUR_T
! Interpolation
  USE M_INTERPOLATION_S
  USE M_INTERPOLATION_S_B
! Traitement des erreurs
  USE M_TRAITER_ERREUR_I
  IMPLICIT NONE
!.. Arguments ..
  DOUBLE PRECISION, DIMENSION(:) :: qinjec
  DOUBLE PRECISION, DIMENSION(:) :: qinjecb
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: x
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: z
  DOUBLE PRECISION, DIMENSION(:) :: zb
  TYPE(APPORT_T), DIMENSION(:), INTENT(IN) :: apport
  TYPE(DEVERSOIR_T), DIMENSION(:), INTENT(IN) :: deversoir
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: qdeverse
  TYPE(ERREUR_T), INTENT(INOUT) :: erreur
!.. Variables locales ..
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: q_dever
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: q_deverb
! Indice de section de calcul
  INTEGER :: is
! Compteurs
  INTEGER :: jj, k
  INTEGER :: nb_point
  INTEGER :: ns_debut, ns_fin
  DOUBLE PRECISION :: long_apport_modele
  DOUBLE PRECISION :: coeff_debit
  DOUBLE PRECISION :: cote_crete
! retour de fonction intrinseque
  INTEGER :: retour
  INTRINSIC SIZE
  INTEGER :: branch
  INTEGER :: ad_count
  INTEGER :: i
  INTEGER :: ad_count0
  INTEGER :: i0
  DOUBLE PRECISION :: dummyzerodiffb0(SIZE(deversoir(1)%ptq, 1))
  DOUBLE PRECISION :: dummyzerodiffb(SIZE(deversoir(1)%ptz, 1))
  DOUBLE PRECISION :: dummyzerodiffb4(SIZE(deversoir(1)%ptq, 1))
  DOUBLE PRECISION :: dummyzerodiffb3(SIZE(deversoir(1)%ptz, 1))
  DOUBLE PRECISION :: dummyzerodiffb2(SIZE(deversoir(1)%ptq, 1))
  DOUBLE PRECISION :: dummyzerodiffb1(SIZE(deversoir(1)%ptz, 1))
!character(132) :: arbredappel_old
!========================== Instructions =============================
! INITIALISATION
!---------------
!arbredappel_old    = trim(Erreur%arbredappel)
!Erreur%arbredappel = trim(Erreur%arbredappel)//'=>CQINJ'
! CAS DES DEBITS D'APPORT
!------------------------
  IF (SIZE(apport) .NE. 0) THEN
    DO k=1,SIZE(apport)
      ns_debut = apport(k)%sectionam
      ns_fin = apport(k)%sectionav
! DEBIT PONCTUEL
      IF (apport(k)%longueur .NE. 0._DOUBLE) long_apport_modele = x(&
&         ns_fin) - x(ns_debut)
! DEBIT LINEIQUE (EXPRIME EN M3/M)
    END DO
    CALL PUSHINTEGER4(k - 1)
  END IF
! de if(size(Deversoir) /= 0)
! CAS DES DEVERSOIRS
!-------------------
  IF (SIZE(deversoir) .NE. 0) THEN
    CALL PUSHINTEGER4(k)
    ad_count0 = 1
    DO k=1,SIZE(deversoir)
      ns_debut = deversoir(k)%sectionam
      ns_fin = deversoir(k)%sectionav
      CALL PUSHINTEGER4(nb_point)
      nb_point = SIZE(deversoir(k)%ptz)
      coeff_debit = deversoir(k)%coeffdebit
      cote_crete = deversoir(k)%cotecrete
! DEVERSOIR PONCTUEL
      IF (ns_debut .EQ. ns_fin) THEN
        ALLOCATE(q_deverb(1), stat=retour)
        q_deverb = 0.D0
        ALLOCATE(q_dever(1), stat=retour)
        IF (retour .NE. 0) THEN
          GOTO 100
        ELSE
          IF (deversoir(k)%type .EQ. deversoir_type_crete_coeff) THEN
            IF (z(ns_debut) - cote_crete .GT. 0._DOUBLE) THEN
              CALL PUSHCONTROL2B(0)
            ELSE
              CALL PUSHCONTROL2B(1)
            END IF
          ELSE IF (deversoir(k)%type .EQ. deversoir_type_loi_z_q) THEN
            CALL INTERPOLATION_S(q_dever(1), z(ns_debut), 1, deversoir(k&
&                          )%ptz, deversoir(k)%ptq, nb_point, erreur)
            IF (erreur%numero .NE. 0) THEN
              GOTO 110
            ELSE
              CALL PUSHCONTROL2B(2)
            END IF
          ELSE
            CALL PUSHCONTROL2B(3)
          END IF
          IF (retour .NE. 0) THEN
            GOTO 120
          ELSE
            CALL PUSHCONTROL1B(1)
          END IF
        END IF
      ELSE
! DEVERSOIR LINEIQUE
        ALLOCATE(q_deverb(ns_fin-ns_debut+1), stat=retour)
        q_deverb = 0.D0
        ALLOCATE(q_dever(ns_fin-ns_debut+1), stat=retour)
        IF (retour .NE. 0) THEN
          GOTO 130
        ELSE
          CALL PUSHINTEGER4(is)
          ad_count = 1
          DO is=ns_debut,ns_fin-1
            CALL PUSHINTEGER4(jj)
            jj = is - ns_debut + 1
            IF (deversoir(k)%type .EQ. deversoir_type_crete_coeff) THEN
              IF (z(is) - cote_crete .GT. 0._DOUBLE) THEN
                CALL PUSHCONTROL2B(0)
              ELSE
                CALL PUSHCONTROL2B(1)
              END IF
            ELSE IF (deversoir(k)%type .EQ. deversoir_type_loi_z_q) THEN
              CALL INTERPOLATION_S(q_dever(jj), z(is), 1, deversoir(k)%&
&                            ptz, deversoir(k)%ptq, nb_point, erreur)
              IF (erreur%numero .NE. 0) THEN
                GOTO 160
              ELSE
                CALL PUSHCONTROL2B(2)
              END IF
            ELSE
              CALL PUSHCONTROL2B(3)
            END IF
            CALL PUSHREAL8(long_apport_modele)
            long_apport_modele = x(ns_fin) - x(ns_debut)
            IF (long_apport_modele .GT. 0._DOUBLE) THEN
              CALL PUSHCONTROL1B(1)
            ELSE
              CALL PUSHCONTROL1B(0)
            END IF
            CALL PUSHINTEGER4(is)
            ad_count = ad_count + 1
          END DO
          CALL PUSHCONTROL1B(0)
          CALL PUSHINTEGER4(ad_count)
          IF (deversoir(k)%type .EQ. deversoir_type_crete_coeff) THEN
            IF (z(ns_fin) - cote_crete .GT. 0._DOUBLE) THEN
              CALL PUSHCONTROL2B(0)
            ELSE
              CALL PUSHCONTROL2B(1)
            END IF
          ELSE IF (deversoir(k)%type .EQ. deversoir_type_loi_z_q) THEN
            CALL INTERPOLATION_S(q_dever(jj+1), z(ns_fin), 1, deversoir(&
&                          k)%ptz, deversoir(k)%ptq, nb_point, erreur)
            IF (erreur%numero .NE. 0) THEN
              GOTO 140
            ELSE
              CALL PUSHCONTROL2B(2)
            END IF
          ELSE
            CALL PUSHCONTROL2B(3)
          END IF
          IF (retour .NE. 0) THEN
            GOTO 150
          ELSE
            CALL PUSHCONTROL1B(0)
          END IF
        END IF
      END IF
      CALL PUSHINTEGER4(k)
      ad_count0 = ad_count0 + 1
    END DO
    CALL PUSHCONTROL3B(0)
    CALL PUSHINTEGER4(ad_count0)
    CALL PUSHCONTROL1B(0)
    GOTO 170
 100 CALL PUSHCONTROL3B(1)
    CALL PUSHINTEGER4(ad_count0)
    CALL PUSHCONTROL1B(0)
    erreur%numero = 5
    erreur%ft = err_5
    erreur%ft_c = err_5c
    CALL TRAITER_ERREUR(erreur, 'q_dever')
    GOTO 170
 110 CALL PUSHCONTROL3B(2)
    CALL PUSHINTEGER4(ad_count0)
    CALL PUSHCONTROL1B(0)
    GOTO 170
 120 CALL PUSHCONTROL3B(3)
    CALL PUSHINTEGER4(ad_count0)
    CALL PUSHCONTROL1B(0)
    erreur%numero = 6
    erreur%ft = err_6
    erreur%ft_c = err_6c
    CALL TRAITER_ERREUR(erreur, 'q_dever')
    GOTO 170
 130 CALL PUSHCONTROL3B(4)
    CALL PUSHINTEGER4(ad_count0)
    CALL PUSHCONTROL1B(0)
    erreur%numero = 5
    erreur%ft = err_5
    erreur%ft_c = err_5c
    CALL TRAITER_ERREUR(erreur, 'q_dever')
    GOTO 170
 140 CALL PUSHCONTROL3B(6)
    CALL PUSHINTEGER4(ad_count0)
    CALL PUSHCONTROL1B(0)
    GOTO 170
 150 CALL PUSHCONTROL3B(7)
    CALL PUSHINTEGER4(ad_count0)
    CALL PUSHCONTROL1B(0)
    erreur%numero = 6
    erreur%ft = err_6
    erreur%ft_c = err_6c
    CALL TRAITER_ERREUR(erreur, 'q_dever')
    GOTO 170
 160 CALL PUSHCONTROL1B(1)
    CALL PUSHINTEGER4(ad_count)
    CALL PUSHCONTROL3B(5)
    CALL PUSHINTEGER4(ad_count0)
    CALL PUSHCONTROL1B(0)
  ELSE
    CALL PUSHCONTROL1B(1)
  END IF
 170 CALL POPCONTROL1B(branch)
  IF (branch .EQ. 0) THEN
    CALL POPINTEGER4(ad_count0)
    DO i0=1,ad_count0
      IF (i0 .EQ. 1) THEN
        CALL POPCONTROL3B(branch)
        IF (branch .LT. 4) THEN
          IF (branch .LT. 2) THEN
            IF (branch .EQ. 0) THEN
              GOTO 280
            ELSE
              GOTO 260
            END IF
          ELSE IF (branch .EQ. 2) THEN
            GOTO 250
          ELSE
            GOTO 240
          END IF
        ELSE IF (branch .LT. 6) THEN
          IF (branch .EQ. 4) THEN
            GOTO 230
          ELSE
            GOTO 200
          END IF
        ELSE IF (branch .EQ. 6) THEN
          GOTO 180
        END IF
      ELSE
        CALL POPCONTROL1B(branch)
        IF (branch .NE. 0) GOTO 240
      END IF
      ns_fin = deversoir(k)%sectionav
      q_deverb(jj+1) = q_deverb(jj+1) - (deversoir(k)%longueur-&
&       long_apport_modele)*qinjecb(ns_fin)
      CALL POPCONTROL2B(branch)
      IF (branch .LT. 2) THEN
        IF (branch .EQ. 0) THEN
          coeff_debit = deversoir(k)%coeffdebit
          cote_crete = deversoir(k)%cotecrete
          IF (.NOT.(z(ns_fin) - cote_crete .LE. 0.0 .AND. (w32 .EQ. 0.0 &
&             .OR. w32 .NE. INT(w32)))) zb(ns_fin) = zb(ns_fin) + &
&             coeff_debit*(gpes*2._DOUBLE)**w12*w32*(z(ns_fin)-&
&             cote_crete)**(w32-1)*q_deverb(jj+1)
          q_deverb(jj+1) = 0.D0
        ELSE
          q_deverb(jj+1) = 0.D0
          coeff_debit = deversoir(k)%coeffdebit
          cote_crete = deversoir(k)%cotecrete
        END IF
        GOTO 200
      ELSE IF (branch .NE. 2) THEN
        GOTO 190
      END IF
 180  dummyzerodiffb3 = 0.D0
      dummyzerodiffb4 = 0.D0
      CALL INTERPOLATION_S_B(q_dever(jj+1), q_deverb(jj+1), z(ns_fin), &
&                      zb(ns_fin), 1, deversoir(k)%ptz, dummyzerodiffb3&
&                      , deversoir(k)%ptq, dummyzerodiffb4, nb_point, &
&                      erreur)
 190  coeff_debit = deversoir(k)%coeffdebit
      cote_crete = deversoir(k)%cotecrete
 200  CALL POPINTEGER4(ad_count)
      DO i=1,ad_count
        IF (i .EQ. 1) THEN
          CALL POPCONTROL1B(branch)
          IF (branch .EQ. 0) GOTO 220
        ELSE
          CALL POPCONTROL1B(branch)
          IF (branch .EQ. 0) THEN
            q_deverb(jj) = q_deverb(jj) - qinjecb(is)
          ELSE
            q_deverb(jj) = q_deverb(jj) - (x(is+1)-x(is))*qinjecb(is)
          END IF
          CALL POPREAL8(long_apport_modele)
          CALL POPCONTROL2B(branch)
          IF (branch .LT. 2) THEN
            IF (branch .EQ. 0) THEN
              IF (.NOT.(z(is) - cote_crete .LE. 0.0 .AND. (w32 .EQ. 0.0 &
&                 .OR. w32 .NE. INT(w32)))) zb(is) = zb(is) + &
&                 coeff_debit*(gpes*2._DOUBLE)**w12*w32*(z(is)-&
&                 cote_crete)**(w32-1)*q_deverb(jj)
              q_deverb(jj) = 0.D0
            ELSE
              q_deverb(jj) = 0.D0
            END IF
            GOTO 210
          ELSE IF (branch .NE. 2) THEN
            GOTO 210
          END IF
        END IF
        dummyzerodiffb1 = 0.D0
        dummyzerodiffb2 = 0.D0
        CALL INTERPOLATION_S_B(q_dever(jj), q_deverb(jj), z(is), zb(is)&
&                        , 1, deversoir(k)%ptz, dummyzerodiffb1, &
&                        deversoir(k)%ptq, dummyzerodiffb2, nb_point, &
&                        erreur)
 210    CALL POPINTEGER4(jj)
 220    CALL POPINTEGER4(is)
      END DO
 230  DEALLOCATE(q_dever)
      DEALLOCATE(q_deverb)
      GOTO 270
 240  ns_debut = deversoir(k)%sectionam
      q_deverb(1) = q_deverb(1) - qinjecb(ns_debut)
      CALL POPCONTROL2B(branch)
      IF (branch .LT. 2) THEN
        IF (branch .EQ. 0) THEN
          coeff_debit = deversoir(k)%coeffdebit
          cote_crete = deversoir(k)%cotecrete
          IF (.NOT.(z(ns_debut) - cote_crete .LE. 0.0 .AND. (w32 .EQ. &
&             0.0 .OR. w32 .NE. INT(w32)))) zb(ns_debut) = zb(ns_debut) &
&             + coeff_debit*(gpes*2._DOUBLE)**w12*w32*(z(ns_debut)-&
&             cote_crete)**(w32-1)*q_deverb(1)
          q_deverb(1) = 0.D0
        ELSE
          q_deverb(1) = 0.D0
        END IF
        GOTO 260
      ELSE IF (branch .NE. 2) THEN
        GOTO 260
      END IF
 250  dummyzerodiffb = 0.D0
      dummyzerodiffb0 = 0.D0
      CALL INTERPOLATION_S_B(q_dever(1), q_deverb(1), z(ns_debut), zb(&
&                      ns_debut), 1, deversoir(k)%ptz, dummyzerodiffb, &
&                      deversoir(k)%ptq, dummyzerodiffb0, nb_point, &
&                      erreur)
 260  DEALLOCATE(q_dever)
      DEALLOCATE(q_deverb)
 270  CALL POPINTEGER4(nb_point)
 280  CALL POPINTEGER4(k)
    END DO
  END IF
END SUBROUTINE CQINJ_B






SUBROUTINE SARAP_B(z, zb, q1, q2, p1, p1b, p2, p2b, b1, b1b, b2, b2b, bs&
& , rh1, rh1b, rh2, rh2b, s1, s1b, s2, s2b, beta, betab, froude, &
& extremite, apport, qinjec, qinjecb, qdeverse, temps, profil, &
& profilplan, f1, x, cf1, cf1b, cf2, cf2b, zref, xdt, idt, connect, &
& singularite, pcsing, pcsingb, deversoir, &
& modelelit, confluent, abaque, algorithme, impression, unitelisting, &
& loifrottement, pertechargeconfluent, cqmv, decentrement, erreur)
! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             S. PERON
!                             S. MANDELKERN
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
! FONCTION :                                                          .
! .          CALCUL EN REGIME PERMANENT A L'AIDE DU CODE SARA         .
! .          ADAPTE A LA RESOLUTION D'UN RESEAU RAMIFIE               .
! .....................................................................
! . ADAPTE A LA RESOLUTION D'UN RESEAU RAMIFIE                        .
! . LIT MINEUR / MAJEUR                                               .
! .                                                                   .
! . LE SENS DE PARCOURS DU RESEAU EST DETERMINE DANS LE SOUS-PROGRAMME.
! . ALGOP, APPELE LORS DE LA PHASE DE LECTURE DES DONNEES             .
! .....................................................................
!============================= Declarations ===========================
  USE M_PRECISION
! Constantes nommees
  USE M_CONSTANTES_CALCUL_C
  USE M_MESSAGE_C
  USE M_PARAMETRE_C
! Types derives
  USE M_APPORT_T
! Type confluent
  USE M_CONFLUENT_T
  USE M_CONNECT_T
  USE M_DEVERSOIR_T
  USE M_ERREUR_T
  USE M_EXTREMITE_T
  USE M_PROFIL_T
  USE M_PROFIL_PLAN_T
  USE M_SINGULARITE_T
! Procedures-module
! Calcul du numero du bief d'une section
  USE M_NUM_BIEF_S
! Sous-programme RHSBP_GENERIQUE_S
  USE M_RHSBP_S
! calcul du nombre de Froude
  USE M_FROUDE_S
! Interface generique d'appel aux
  USE M_TRAITER_ERREUR_I
! procedures de traitement des erreurs
! Interfaces
  USE M_CQINJ_I
  USE M_CQINJ_I_B
  USE M_PERSAR_I
  USE M_PERSAR_I_B
  USE M_REPAR_I
  IMPLICIT NONE
! ... Format Declarations ...
!.. Donnees/Resultats ..
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: z, q1, q2
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: zb
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: p1, p2
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: p1b
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: b1, b2, bs
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: b1b
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: rh1, rh2
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: rh1b
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: s1, s2
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: s1b
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: beta
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: betab
  DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: froude
! Conditions aux limites des extremites libres
  TYPE(EXTREMITE_T), DIMENSION(:), INTENT(IN) :: extremite
  TYPE(ERREUR_T), INTENT(INOUT) :: erreur
! Maillage
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: x, cf1, cf2
  DOUBLE PRECISION, DIMENSION(:) :: cf1b, cf2b
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: zref
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: xdt
  INTEGER, DIMENSION(:), INTENT(IN) :: idt
! Variables planimetrees
  TYPE(PROFIL_T), DIMENSION(:), INTENT(IN) :: profil
  TYPE(PROFIL_PLAN_T), INTENT(IN) :: profilplan
  DOUBLE PRECISION, DIMENSION(:, :), INTENT(IN) :: f1
! Debits d apports
  TYPE(APPORT_T), DIMENSION(:), INTENT(IN) :: apport
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: qdeverse
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: qinjec
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: qinjecb
! Pertes de charge singulieres
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: pcsing
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: pcsingb
! Table du connectivite du reseau
  TYPE(CONNECT_T), INTENT(IN) :: connect
! Algorithme de resolution
  INTEGER, DIMENSION(:), INTENT(IN) :: algorithme
! Singularites
  TYPE(SINGULARITE_T), DIMENSION(:), INTENT(IN) :: singularite
! Modelisation du lit
  INTEGER, INTENT(IN) :: modelelit
! Confluences
  TYPE(CONFLUENT_T), DIMENSION(:), INTENT(IN) :: confluent
  DOUBLE PRECISION, DIMENSION(6, 6, 5), INTENT(IN) :: abaque
! Parametres
  LOGICAL, INTENT(IN) :: impression
  INTEGER, INTENT(IN) :: unitelisting
  INTEGER, INTENT(IN) :: loifrottement, cqmv
  LOGICAL, INTENT(IN) :: pertechargeconfluent
  LOGICAL, INTENT(IN) :: decentrement
! Temps
  DOUBLE PRECISION, INTENT(IN) :: temps
! Deversoirs
  TYPE(DEVERSOIR_T), DIMENSION(:), INTENT(IN) :: deversoir
!.. Local Scalars ..
  DOUBLE PRECISION :: vmoy
  DOUBLE PRECISION :: deb
! Compteur sur les debits d'apport
  INTEGER :: iinj
! Compteur sur les sections
  INTEGER :: isec
! Numero du bief
  INTEGER :: num_bief
! abscisse relative d'une section
  DOUBLE PRECISION :: absc_rel
!character(132) :: !arbredappel_old  ! arbre dappel precedent
!.. Local Arrays ..
  DOUBLE PRECISION, DIMENSION(SIZE(x)) :: q
  DOUBLE PRECISION, DIMENSION(SIZE(x)) :: qb
  INTRINSIC SIZE
  INTEGER :: ad_count
  INTEGER :: i
  INTEGER :: branch
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: b2b
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: s2b
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: rh2b
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: p2b
!============================= Instructions ===========================
! INITIALISATIONS ET ALLOCATIONS
! ------------------------------
!arbredappel_old    = trim(!Erreur%arbredappel)
!Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>SARAP'
! REMPLISSAGE DU TABLEAU DES APPORTS
! ----------------------------------
  IF (SIZE(apport) .GT. 0) THEN
    ad_count = 1
    DO iinj=1,SIZE(apport)
! Calcul de QInjec, tableau dimensionne a NbSect
! representant les apports
! ----------------------------------------------
!      CALL PUSHCHARACTERARRAY(erreur%message, 400)
!      CALL PUSHCHARACTERARRAY(erreur%arbredappel, 132)
      CALL CQINJ(qinjec, x, z, apport, deversoir, qdeverse, erreur)
      IF (erreur%numero .NE. 0) THEN
        GOTO 100
      ELSE
        ad_count = ad_count + 1
      END IF
    END DO
    CALL PUSHCONTROL1B(0)
    CALL PUSHINTEGER4(ad_count)
    CALL PUSHCONTROL1B(0)
    GOTO 110
 100 CALL PUSHCONTROL1B(1)
    CALL PUSHINTEGER4(ad_count)
    CALL PUSHCONTROL2B(0)
    GOTO 150
  ELSE
    CALL PUSHCONTROL1B(1)
    qinjec(:) = 0._DOUBLE
  END IF
! CALCUL DE BASE DE LA LIGNE D'EAU
! --------------------------------
!/RESULTATS/
!/DONNEES NON MODIFIEES/
! Caracteristiques des confluences
! Abaques des pertes de  charges aux confluences
! 110 CALL PUSHCHARACTERARRAY(erreur%message, 400)
!  CALL PUSHCHARACTERARRAY(erreur%arbredappel, 132)
110  CALL PUSHREAL8ARRAY(pcsing, SIZE(pcsing, 1))
  CALL PUSHREAL8ARRAY(q, SIZE(x))
  CALL PUSHREAL8ARRAY(z, SIZE(z, 1))
  CALL PERSAR(z, q, x, zref, cf1, cf2, pcsing, idt, xdt, profil, &
&          profilplan, f1, qinjec, connect, singularite, extremite, &
&          modelelit, confluent, abaque, impression, unitelisting, temps&
&          , algorithme, loifrottement, pertechargeconfluent, cqmv, &
&          decentrement, erreur)
  IF (erreur%numero .NE. 0) THEN
    CALL PUSHCONTROL2B(1)
  ELSE
    CALL PUSHINTEGER4(isec)
    DO isec=1,SIZE(x)
! CALCUL DES GRANDEURS HYDRAULIQUES
! ---------------------------------
      CALL RHSBP_S(b1(isec), b2(isec), bs(isec), p1(isec), p2(isec), s1(&
&            isec), s2(isec), rh1(isec), rh2(isec), isec, z(isec), zref(&
&            isec), idt, xdt, profil, profilplan, unitelisting, erreur)
      IF (erreur%numero .NE. 0) THEN
        GOTO 130
      ELSE
! CALCUL DE LA REPARTITION LIT MINEUR / LIT MAJEUR
! ------------------------------------------------
! Resultats
! Donnees modifiees
! Donnees non modifiees
        CALL REPAR(deb, vmoy, beta(isec), q1(isec), q2(isec), s1(isec&
&               ), s2(isec), rh1(isec), rh2(isec), p1(isec), p2(isec), q&
&               (isec), cf1(isec), cf2(isec), modelelit, loifrottement, &
&               profil(idt(isec))%Nom, erreur)
        IF (erreur%numero .NE. 0) GOTO 140
      END IF
    END DO
    CALL PUSHCONTROL2B(2)
! VERIFICATION DE LA COTE EN TOUT POINT
! -------------------------------------
    DO isec=1,SIZE(x)
      IF (z(isec) - zref(isec) .LT. w0) GOTO 120
    END DO
    GOTO 150
 120 erreur%ft = err_31
    erreur%ft_c = err_31c
    num_bief = NUM_BIEF_S(connect, isec, erreur)
    absc_rel = x(isec) - x(connect%originebief(num_bief))
    CALL TRAITER_ERREUR(erreur, isec, num_bief, absc_rel)
    GOTO 150
 130 CALL PUSHCONTROL2B(2)
    GOTO 160
 140 CALL PUSHCONTROL2B(2)
  END IF
  GOTO 160
 150 CALL POPCONTROL2B(branch)
  IF (branch .EQ. 0) GOTO 170
 160 IF (branch .NE. 1) CALL POPINTEGER4(isec)
  CALL POPREAL8ARRAY(z, SIZE(z, 1))
  CALL POPREAL8ARRAY(q, SIZE(x))
  CALL POPREAL8ARRAY(pcsing, SIZE(pcsing, 1))
!  CALL POPCHARACTERARRAY(erreur%arbredappel, 132)
!  CALL POPCHARACTERARRAY(erreur%message, 400)
  CALL PERSAR_B(z, zb, q, qb, x, zref, cf1, cf1b, cf2, cf2b, pcsing, &
&         pcsingb, idt, xdt, profil, profilplan, f1, qinjec, qinjecb, &
&         connect, singularite, extremite, modelelit, &
&         confluent, abaque, impression, unitelisting, temps, algorithme&
&         , loifrottement, pertechargeconfluent, cqmv, decentrement, erreur)
  CALL POPCONTROL1B(branch)
  IF (branch .NE. 0) GOTO 190
 170 CALL POPINTEGER4(ad_count)
  DO 180 i=1,ad_count
    IF (i .EQ. 1) THEN
      CALL POPCONTROL1B(branch)
      IF (branch .EQ. 0) THEN
        GOTO 180
      ELSE
        cf1b = 0.D0
        cf2b = 0.D0
        qinjecb = 0.D0
      END IF
    END IF
!    CALL POPCHARACTERARRAY(erreur%arbredappel, 132)
!    CALL POPCHARACTERARRAY(erreur%message, 400)
    CALL CQINJ_B(qinjec, qinjecb, x, z, zb, apport, deversoir, &
&                qdeverse, erreur)
    qinjecb = 0.D0
 180 CONTINUE
 190 CONTINUE
END SUBROUTINE SARAP_B







SUBROUTINE DIFF_Z_CF12_BWD_SARAP(diff_z_bwd, dcf1_rwd, dcf2_rwd)

  USE M_PRECISION
  USE M_PARAMETRE_C
  USE M_MESSAGE_C
  USE M_PROFIL_T
  USE M_SINGULARITE_T
  USE M_ERREUR_T
  USE M_CONNECT_T
  USE M_EXTREMITE_T
  USE M_APPORT_T
  USE M_DEVERSOIR_T
  USE M_NUM_BIEF_S
  USE M_FROUDE_S
  USE M_SARAP_I_B
  USE M_SHARE_VAR
  IMPLICIT NONE

  DOUBLE PRECISION, DIMENSION(size(x)) :: diff_z_bwd
  DOUBLE PRECISION, DIMENSION(size(x)) :: dcf1_rwd
  DOUBLE PRECISION, DIMENSION(size(x)) :: dcf2_rwd


!VARIABLES LOCALES NON DIFFÉRENTIÉES
  DOUBLE PRECISION, DIMENSION(:), POINTER :: z, q1, q2
  DOUBLE PRECISION, DIMENSION(:), POINTER :: p1, p2
  DOUBLE PRECISION, DIMENSION(:), POINTER :: b1, b2, bs
  DOUBLE PRECISION, DIMENSION(:), POINTER :: rh1, rh2
  DOUBLE PRECISION, DIMENSION(:), POINTER :: s1, s2
  DOUBLE PRECISION, DIMENSION(:), POINTER :: beta
  DOUBLE PRECISION, DIMENSION(:), POINTER :: froude
  DOUBLE PRECISION, DIMENSION(:), POINTER :: qinjec
  DOUBLE PRECISION, DIMENSION(:), POINTER :: qdeverse
  DOUBLE PRECISION, DIMENSION(:), POINTER :: pcsing
!VARAIBLES LOCALES DIFFÉRENTIÉES
  DOUBLE PRECISION, DIMENSION(:), POINTER :: diff_pcsing
  DOUBLE PRECISION, DIMENSION(:), POINTER :: diff_p1
  DOUBLE PRECISION, DIMENSION(:), POINTER :: diff_p2
  DOUBLE PRECISION, DIMENSION(:), POINTER :: diff_s1
  DOUBLE PRECISION, DIMENSION(:), POINTER :: diff_s2
  DOUBLE PRECISION, DIMENSION(:), POINTER :: diff_rh1
  DOUBLE PRECISION, DIMENSION(:), POINTER :: diff_rh2
  DOUBLE PRECISION, DIMENSION(:), POINTER :: diff_b1
  DOUBLE PRECISION, DIMENSION(:), POINTER :: diff_b2
  DOUBLE PRECISION, DIMENSION(:), POINTER :: diff_beta
  DOUBLE PRECISION, DIMENSION(:), POINTER :: diff_qinjec

  ALLOCATE(z(size(x)))
  ALLOCATE(q1(size(x)))
  ALLOCATE(q2(size(x)))
  ALLOCATE(p1(size(x)))
  ALLOCATE(p2(size(x)))
  ALLOCATE(b1(size(x)))
  ALLOCATE(b2(size(x)))
  ALLOCATE(bs(size(x)))
  ALLOCATE(rh1(size(x)))
  ALLOCATE(rh2(size(x)))
  ALLOCATE(s1(size(x)))
  ALLOCATE(s2(size(x)))
  ALLOCATE(beta(size(x)))
  ALLOCATE(froude(size(x)))
  ALLOCATE(qinjec(size(x)))
  ALLOCATE(qdeverse(size(x)))
  ALLOCATE(pcsing(size(x)))

  ALLOCATE(diff_pcsing(size(x)))
  ALLOCATE(diff_p1(size(x)))
  ALLOCATE(diff_p2(size(x)))
  ALLOCATE(diff_s1(size(x)))
  ALLOCATE(diff_s2(size(x)))
  ALLOCATE(diff_rh1(size(x)))
  ALLOCATE(diff_rh2(size(x)))
  ALLOCATE(diff_b1(size(x)))
  ALLOCATE(diff_b2(size(x)))
  ALLOCATE(diff_beta(size(x)))
  ALLOCATE(diff_qinjec(size(x)))

  z = 0.D0
  q1 = 0.D0
  q2 = 0.D0
  p1 = 0.D0
  p2 = 0.D0
  b1 = 0.D0
  b2 = 0.D0
  bs = 0.D0
  rh1 = 0.D0
  rh2 = 0.D0
  s1 = 0.D0
  s2 = 0.D0
  beta = 0.D0
  froude = 0.D0
  qinjec = 0.D0
  qdeverse = 0.D0
  pcsing = 0.D0

  CALL SARAP_B(z, diff_z_bwd, q1, q2, p1, diff_p1, p2, &
&              diff_p2, b1, diff_b1, b2, diff_b2, bs, rh1, &
&              diff_rh1, rh2, diff_rh2, s1, diff_s1, s2, diff_s2&
&              , beta, diff_beta, froude, extremite&
&              , apport, qinjec, diff_qinjec, qdeverse, temps&
&              , profil, profilplan, f1, x&
&              , cf1, dcf1_rwd, cf2, dcf2_rwd, zref, xdt&
&              , idt, connect, singularite, pcsing&
&              , diff_pcsing, deversoir, modelelit, confluent, &
&              abaque, algorithme, impressioncalcul, fichierlisting%unite, &
&              loifrottement, pertechargeconfluent, cqmv, decentrement, erreur)


  DEALLOCATE(z)
  DEALLOCATE(q1)
  DEALLOCATE(q2)
  DEALLOCATE(p1)
  DEALLOCATE(p2)
  DEALLOCATE(b1)
  DEALLOCATE(b2)
  DEALLOCATE(bs)
  DEALLOCATE(rh1)
  DEALLOCATE(rh2)
  DEALLOCATE(s1)
  DEALLOCATE(s2)
  DEALLOCATE(beta)
  DEALLOCATE(froude)
  DEALLOCATE(qinjec)
  DEALLOCATE(qdeverse)
  DEALLOCATE(pcsing)

  DEALLOCATE(diff_pcsing)
  DEALLOCATE(diff_p1)
  DEALLOCATE(diff_p2)
  DEALLOCATE(diff_s1)
  DEALLOCATE(diff_s2)
  DEALLOCATE(diff_rh1)
  DEALLOCATE(diff_rh2)
  DEALLOCATE(diff_b1)
  DEALLOCATE(diff_b2)
  DEALLOCATE(diff_beta)
  DEALLOCATE(diff_qinjec)

END SUBROUTINE DIFF_Z_CF12_BWD_SARAP
