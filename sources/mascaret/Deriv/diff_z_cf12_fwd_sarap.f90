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

SUBROUTINE REPAR_D(deb, debd, vmoy, vmoyd, beta, betad, q1, q2, s1, s1d&
& , s2, s2d, rh1, rh1d, rh2, rh2d, p1, p1d, p2, p2d, q, qd, cf1, cf1d, &
& cf2, cf2d, modelelit, loifrottement, erreur)
!
  USE M_PRECISION
  USE M_PARAMETRE_C
! Messages d'erreur
  USE M_MESSAGE_C
! MODELE_LIT
  USE M_CONSTANTES_CALCUL_C
! type ERREUR_T
  USE M_ERREUR_T
! Traitement de l'erreur
  USE M_TRAITER_ERREUR_I
  USE M_DEBITANCE_S_D
  IMPLICIT NONE
!Erreur%arbredappel = !arbredappel_old
!.. Formal Arguments ..
  DOUBLE PRECISION, INTENT(OUT) :: deb
  DOUBLE PRECISION, INTENT(OUT) :: debd
  DOUBLE PRECISION, INTENT(OUT) :: vmoy
  DOUBLE PRECISION, INTENT(OUT) :: vmoyd
  DOUBLE PRECISION, INTENT(OUT) :: beta
  DOUBLE PRECISION, INTENT(OUT) :: betad
  DOUBLE PRECISION, INTENT(OUT) :: q1
  DOUBLE PRECISION, INTENT(OUT) :: q2
  DOUBLE PRECISION, INTENT(INOUT) :: s1
  DOUBLE PRECISION, INTENT(INOUT) :: s1d
  DOUBLE PRECISION, INTENT(INOUT) :: s2
  DOUBLE PRECISION, INTENT(INOUT) :: s2d
  DOUBLE PRECISION, INTENT(INOUT) :: rh1
  DOUBLE PRECISION, INTENT(INOUT) :: rh1d
  DOUBLE PRECISION, INTENT(INOUT) :: rh2
  DOUBLE PRECISION, INTENT(INOUT) :: rh2d
  DOUBLE PRECISION, INTENT(IN) :: q
  DOUBLE PRECISION, INTENT(IN) :: qd
  DOUBLE PRECISION, INTENT(IN) :: p1
  DOUBLE PRECISION, INTENT(IN) :: p1d
  DOUBLE PRECISION, INTENT(IN) :: p2
  DOUBLE PRECISION, INTENT(IN) :: p2d
  DOUBLE PRECISION, INTENT(IN) :: cf1
  DOUBLE PRECISION, INTENT(IN) :: cf1d
  DOUBLE PRECISION, INTENT(IN) :: cf2
  DOUBLE PRECISION, INTENT(IN) :: cf2d
  INTEGER, INTENT(IN) :: modelelit
  INTEGER, INTENT(IN) :: loifrottement
  TYPE(ERREUR_T), INTENT(INOUT) :: erreur
!.. Local Scalars ..
  DOUBLE PRECISION :: st1_temp
  DOUBLE PRECISION :: rh
  DOUBLE PRECISION :: rhd
  DOUBLE PRECISION :: a, a0, deb1, deb2, eta, fp1, fp2, fs1, fs2, r0, s&
& , stequi, useta
  DOUBLE PRECISION :: ad, a0d, deb1d, deb2d, etad, fp1d, fp2d, fs1d, &
& fs2d, r0d, sd, stequid
!character(132) :: !arbredappel_old
! Les Constantes sont declares dans le module M_PARAMETRES
  DOUBLE PRECISION, SAVE :: put=0.3_DOUBLE
!.. Intrinsic Functions ..
  INTRINSIC DCOS, DSQRT
  DOUBLE PRECISION :: pwr1
  DOUBLE PRECISION :: pwr1d
  DOUBLE PRECISION :: pwx1
  DOUBLE PRECISION :: pwx1d
  DOUBLE PRECISION :: arg1
  DOUBLE PRECISION :: arg1d
  DOUBLE PRECISION :: result1
  DOUBLE PRECISION :: result1d
!============================= Instructions =============================
! INITIALISATION
! --------------
  erreur%numero = 0
!arbredappel_old    = trim(!Erreur%arbredappel)
!Erreur%Arbredappel = trim(!Erreur%arbredappel)//'=>REPAR'
! GRANDEURS INDEPENDANTES DU MODELE
! ---------------------------------
! Tests
  IF (p1 .LE. eps6) THEN
    erreur%numero = 40
    erreur%ft = err_40
    erreur%ft_c = err_40c
    CALL TRAITER_ERREUR(erreur, p1)
    RETURN
  ELSE IF (s1 .LE. eps6) THEN
    erreur%numero = 41
    erreur%ft = err_41
    erreur%ft_c = err_41c
    CALL TRAITER_ERREUR(erreur, s1)
    RETURN
  ELSE
    rhd = ((s1d+s2d)*(p1+p2)-(s1+s2)*(p1d+p2d))/(p1+p2)**2
    rh = (s1+s2)/(p1+p2)
    vmoyd = (qd*(s1+s2)-q*(s1d+s2d))/(s1+s2)**2
    vmoy = q/(s1+s2)
! MODELE DE COMPOSITION DES RUGOSITES EN LIT UNIQUE
! -------------------------------------------------
    IF (modelelit .EQ. modele_lit_fond_berge) THEN
! MODELISATION FOND/BERGE
! CALCULS INTERNES DU MODELE
      sd = s1d + s2d
      s = s1 + s2
      IF (cf1 .GT. 0.0 .OR. (cf1 .LT. 0.0 .AND. w32 .EQ. INT(w32))) THEN
        pwr1d = w32*cf1**(w32-1)*cf1d
      ELSE IF (cf1 .EQ. 0.0 .AND. w32 .EQ. 1.0) THEN
        pwr1d = cf1d
      ELSE
        pwr1d = 0.0
      END IF
      pwr1 = cf1**w32
      fp1d = (p1d*pwr1-p1*pwr1d)/pwr1**2
      fp1 = p1/pwr1
      IF (cf2 .GT. 0.0 .OR. (cf2 .LT. 0.0 .AND. w32 .EQ. INT(w32))) THEN
        pwr1d = w32*cf2**(w32-1)*cf2d
      ELSE IF (cf2 .EQ. 0.0 .AND. w32 .EQ. 1.0) THEN
        pwr1d = cf2d
      ELSE
        pwr1d = 0.0
      END IF
      pwr1 = cf2**w32
      fp2d = (p2d*pwr1-p2*pwr1d)/pwr1**2
      fp2 = p2/pwr1
      fs1d = sd*fp1/(fp1+fp2) + s*(fp1d*(fp1+fp2)-fp1*(fp1d+fp2d))/(fp1+&
&       fp2)**2
      fs1 = s*(fp1/(fp1+fp2))
      fs2d = sd*fp2/(fp1+fp2) + s*(fp2d*(fp1+fp2)-fp2*(fp1d+fp2d))/(fp1+&
&       fp2)**2
      fs2 = s*(fp2/(fp1+fp2))
      useta = fs2/fs1
      pwx1d = ((p1d+p2d)*(fp1+fp2)-(p1+p2)*(fp1d+fp2d))/(fp1+fp2)**2
      pwx1 = (p1+p2)/(fp1+fp2)
      IF (pwx1 .GT. 0.0 .OR. (pwx1 .LT. 0.0 .AND. w23 .EQ. INT(w23))) &
&     THEN
        stequid = w23*pwx1**(w23-1)*pwx1d
      ELSE IF (pwx1 .EQ. 0.0 .AND. w23 .EQ. 1.0) THEN
        stequid = pwx1d
      ELSE
        stequid = 0.0
      END IF
      stequi = pwx1**w23
! RESULTATS
      IF (rh .GT. 0.0 .OR. (rh .LT. 0.0 .AND. w23 .EQ. INT(w23))) THEN
        pwr1d = w23*rh**(w23-1)*rhd
      ELSE IF (rh .EQ. 0.0 .AND. w23 .EQ. 1.0) THEN
        pwr1d = rhd
      ELSE
        pwr1d = 0.0
      END IF
      pwr1 = rh**w23
      debd = (stequid*s+stequi*sd)*pwr1 + stequi*s*pwr1d
      deb = stequi*s*pwr1
      beta = 1._DOUBLE
      q1 = q/(1._DOUBLE+useta)
      q2 = useta*q1
! MODIFICATION DES VARIABLES GEOMETRIQUES
      s1d = fs1d
      s1 = fs1
      s2d = fs2d
      s2 = fs2
      rh1 = s1/p1
      IF (p2 .NE. 0._DOUBLE) THEN
        rh2 = s2/p2
        betad = 0.D0
      ELSE
        rh2 = 0._DOUBLE
        betad = 0.D0
      END IF
    ELSE
! VALEUR DE BASE DU PARAMETRE DU MODELE DEBORD
! --------------------------------------------
! COMPOSITION DES RUGOSITE DEBORD (OPTIO2) OU NON DEFINIE (OPTIO3)
      a = 1._DOUBLE
! MODELE DEBORD
! -------------
      IF (modelelit .EQ. modele_lit_debord) THEN
        pwx1d = (cf2d*cf1-cf2*cf1d)/cf1**2
        pwx1 = cf2/cf1
        IF (pwx1 .GT. 0.0 .OR. (pwx1 .LT. 0.0 .AND. w16 .EQ. INT(w16))) &
&       THEN
          pwr1d = w16*pwx1**(w16-1)*pwx1d
        ELSE IF (pwx1 .EQ. 0.0 .AND. w16 .EQ. 1.0) THEN
          pwr1d = pwx1d
        ELSE
          pwr1d = 0.0
        END IF
        pwr1 = pwx1**w16
        a0d = w09*pwr1d
        a0 = w09*pwr1
        r0d = (rh2d*rh1-rh2*rh1d)/rh1**2
        r0 = rh2/rh1
        IF (r0 .GE. put) THEN
          ad = a0d
          a = a0
        ELSE
          arg1d = pi*r0d/put
          arg1 = pi*r0/put
          ad = (a0d-(1._DOUBLE-a0)*arg1d*DSIN(arg1)-a0d*DCOS(arg1))/&
&           2._DOUBLE
          a = ((1._DOUBLE-a0)*DCOS(arg1)+1._DOUBLE+a0)/2._DOUBLE
        END IF
      ELSE
        ad = 0.D0
      END IF
      CALL DEBITANCE_S_D(deb1, deb1d, st1_temp, rh1, rh1d, s1, s1d, &
&                  loifrottement, cf1, cf1d, erreur)
      IF (erreur%numero .NE. 0) THEN
        RETURN
      ELSE
        deb1d = deb1d*a + deb1*ad
        deb1 = deb1*a
        arg1d = 2*s2*s2d + (s1d*s2+s1*s2d)*(1._DOUBLE-a*a) + s1*s2*(-(ad&
&         *a)-a*ad)
        arg1 = s2**2 + s1*s2*(1._DOUBLE-a*a)
        IF (arg1 .EQ. 0.0) THEN
          result1d = 0.D0
        ELSE
          result1d = arg1d/(2.D0*DSQRT(arg1))
        END IF
        result1 = DSQRT(arg1)
        IF (rh2 .GT. 0.0 .OR. (rh2 .LT. 0.0 .AND. w23 .EQ. INT(w23))) &
&       THEN
          pwr1d = w23*rh2**(w23-1)*rh2d
        ELSE IF (rh2 .EQ. 0.0 .AND. w23 .EQ. 1.0) THEN
          pwr1d = rh2d
        ELSE
          pwr1d = 0.0
        END IF
        pwr1 = rh2**w23
        deb2d = (cf2d*result1+cf2*result1d)*pwr1 + cf2*result1*pwr1d
        deb2 = cf2*result1*pwr1
        debd = deb1d + deb2d
        deb = deb1 + deb2
        IF (s2 .LE. s1*eps4) THEN
          beta = 1._DOUBLE
          q2 = 0._DOUBLE
          q1 = q
          betad = 0.D0
        ELSE
          etad = (deb1d*deb2-deb1*deb2d)/deb2**2
          eta = deb1/deb2
          betad = ((((2*eta*etad*s1-eta**2*s1d)/s1**2-s2d/s2**2)*(s1+s2)&
&           +(eta**2/s1+1._DOUBLE/s2)*(s1d+s2d))*(1._DOUBLE+eta)**2-(eta&
&           **2/s1+1._DOUBLE/s2)*(s1+s2)*2*(1._DOUBLE+eta)*etad)/((&
&           1._DOUBLE+eta)**2)**2
          beta = (eta**2/s1+1._DOUBLE/s2)*(s1+s2)/(1._DOUBLE+eta)**2
          q2 = q/(1._DOUBLE+eta)
          q1 = eta*q2
        END IF
      END IF
    END IF
  END IF
END SUBROUTINE REPAR_D




SUBROUTINE PSING_D(zam, zamd, singularite, zref, zav, zavd&
& , qam, qamd, profil, b1plan, idt, xdt, section, temps, erreur)
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
  USE M_INTERPOLATION_S_D
! Sous programme RHSBP_SECTION_S
  USE M_RHSBP_S
! Traitement de l'erreur
  USE M_TRAITER_ERREUR_I
  IMPLICIT NONE
!.. Arguments ..
!----------------
  DOUBLE PRECISION, INTENT(INOUT) :: zam
  DOUBLE PRECISION, INTENT(INOUT) :: zamd
  TYPE(SINGULARITE_T), INTENT(IN) :: singularite
  DOUBLE PRECISION, INTENT(IN) :: zref
  DOUBLE PRECISION, INTENT(IN) :: zav
  DOUBLE PRECISION, INTENT(IN) :: zavd
  DOUBLE PRECISION, INTENT(IN) :: qam
  DOUBLE PRECISION, INTENT(IN) :: qamd
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
  DOUBLE PRECISION :: alphad
  DOUBLE PRECISION :: dch
  DOUBLE PRECISION :: dx
  DOUBLE PRECISION :: dz
  DOUBLE PRECISION :: dzd
  DOUBLE PRECISION :: epsq
  DOUBLE PRECISION :: charge_amont, charge_aval
  DOUBLE PRECISION :: charge_amontd, charge_avald
  DOUBLE PRECISION :: charge
  DOUBLE PRECISION :: largeur_seuil
  DOUBLE PRECISION :: q1, q2
  DOUBLE PRECISION :: qamont
  DOUBLE PRECISION :: rh
  DOUBLE PRECISION :: zam1, zam2
  DOUBLE PRECISION :: zam1d, zam2d
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
  DOUBLE PRECISION :: pwx1
  DOUBLE PRECISION :: pwx1d
  DOUBLE PRECISION :: pwr1
  DOUBLE PRECISION :: pwx2
  DOUBLE PRECISION :: pwr2
  DOUBLE PRECISION :: dummyzerodiffd(SIZE(singularite%ptzaval(:), 1))
  DOUBLE PRECISION :: dmax10d
  DOUBLE PRECISION :: dmax13d
  DOUBLE PRECISION :: dummyzerodiffd2(SIZE(singularite%ptq(:), 1))
  DOUBLE PRECISION :: dummyzerodiffd1(SIZE(singularite%ptq(:), 1))
  DOUBLE PRECISION :: dummyzerodiffd0(SIZE(singularite%ptzaval(:), 1))
  DOUBLE PRECISION :: dmax12d
  DOUBLE PRECISION :: dabs2
  DOUBLE PRECISION :: dabs1
  DOUBLE PRECISION :: dabs0
  DOUBLE PRECISION :: dmax14
  DOUBLE PRECISION :: dmax13
  DOUBLE PRECISION :: dmax12
  DOUBLE PRECISION :: dmax11
  DOUBLE PRECISION :: dmax11d
  DOUBLE PRECISION :: dmax10
  DOUBLE PRECISION :: dmax14d
!character(132) :: !arbredappel_old ! ancien arbre d'appel
!============================= Instructions ===========================
! INITIALISATIONS
! ---------------
  erreur%numero = 0
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
      RETURN
    ELSE
      DO iq=1,SIZE(singularite%ptq)-1
        q1 = singularite%ptq(iq)
        q2 = singularite%ptq(iq+1)
        IF (qam .GE. q1 .AND. qam .LE. q2) GOTO 100
      END DO
! INTERPOLATION DE LA COTE AMONT A PARTIR DES DEBITS DE REFERENCE
 100  zam1d = 0.D0
      dummyzerodiffd = 0.D0
      CALL INTERPOLATION_S_D(zam1, zam1d, zav, zavd, &
&                      premier_ordre_interpolation, singularite%ptzaval(&
&                      :), dummyzerodiffd, singularite%ptzamont(iq, :), &
&                      SIZE(singularite%ptzamont(1, :)), erreur)
      IF (erreur%numero .NE. 0) THEN
        RETURN
      ELSE
        zam2d = 0.D0
        dummyzerodiffd0 = 0.D0
        CALL INTERPOLATION_S_D(zam2, zam2d, zav, zavd, &
&                        premier_ordre_interpolation, singularite%&
&                        ptzaval(:), dummyzerodiffd0, singularite%&
&                        ptzamont(1+iq, :), SIZE(singularite%ptzamont(1&
&                        , :)), erreur)
        IF (erreur%numero .NE. 0) THEN
          RETURN
        ELSE
! RESULTAT
          alphad = qamd/(q2-q1)
          alpha = (qam-q1)/(q2-q1)
          zamd = (1._DOUBLE-alpha)*zam1d - alphad*zam1 + alphad*zam2 + &
&           alpha*zam2d
          zam = (1._DOUBLE-alpha)*zam1 + alpha*zam2
        END IF
      END IF
    END IF
  CASE (singularite_type_zamont_q)
! COTE AMONT EN REGIME DENOYE
    dummyzerodiffd1 = 0.D0
    CALL INTERPOLATION_S_D(zam, zamd, qam, qamd, &
&                    premier_ordre_interpolation, singularite%ptq(:), &
&                    dummyzerodiffd1, singularite%ptz(:), SIZE(&
&                    singularite%ptz), erreur)
    IF (erreur%numero .NE. 0) THEN
      RETURN
    ELSE
! TEST DU REGIME DENOYE
      charge_amontd = zamd
      charge_amont = zam - singularite%cotecrete
      charge_avald = zavd
      charge_aval = zav - singularite%cotecrete
      IF (charge_amont .LE. 0._DOUBLE) THEN
        erreur%numero = 43
        erreur%ft = err_43
        erreur%ft_c = err_43c
        CALL TRAITER_ERREUR(erreur, singularite%numero, temps, zam, &
&                     singularite%cotecrete)
        RETURN
      ELSE
        rh = charge_aval/charge_amont
        IF (rh .LE. cdenoy) THEN
!Erreur%arbredappel = !arbredappel_old
          RETURN
        ELSE
! ITERATIONS EN REGIME NOYE
          num_iter = 0
          epsq = eps3*qam
          IF (zam .LT. zav) THEN
            zamd = zavd
            zam = zav
          ELSE
            zam = zam
          END IF
          sens = -1
          IF (charge_amont .LT. charge_aval) THEN
            dmax10d = charge_avald
            dmax10 = charge_aval
          ELSE
            dmax10d = charge_amontd
            dmax10 = charge_amont
          END IF
          dzd = dmax10d/pas
          dz = dmax10/pas
 30       zamd = zamd + dzd
          zam = zam + dz
          CALL INTERPOLATION_S(qamont, zam, premier_ordre_interpolation&
&                        , singularite%ptz(:), singularite%ptq(:), SIZE(&
&                        singularite%ptq), erreur)
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
                dzd = -(dzd/pas)
                dz = -(dz/pas)
                sens = -sens
              END IF
              GOTO 30
            END IF
          END IF
          RETURN
!Erreur%arbredappel = !arbredappel_old
 110      RETURN
        END IF
      END IF
    END IF
  CASE (singularite_type_profil_crete)
    largeur_seuil = singularite%ptx(SIZE(singularite%ptx)) - singularite&
&     %ptx(1)
    IF (0._DOUBLE .LT. zav - singularite%cotecrete) THEN
      charge_avald = zavd
      charge_aval = zav - singularite%cotecrete
    ELSE
      charge_aval = 0._DOUBLE
      charge_avald = 0.D0
    END IF
! ESTIMATION INITIALE
    pwx1d = qamd/(singularite%coeffdebit*largeur_seuil*rdg)
    pwx1 = qam/(singularite%coeffdebit*largeur_seuil*rdg)
    IF (pwx1 .GT. 0.0 .OR. (pwx1 .LT. 0.0 .AND. w23 .EQ. INT(w23))) THEN
      charge_amontd = w23*pwx1**(w23-1)*pwx1d
    ELSE IF (pwx1 .EQ. 0.0 .AND. w23 .EQ. 1.0) THEN
      charge_amontd = pwx1d
    ELSE
      charge_amontd = 0.0
    END IF
    charge_amont = pwx1**w23
    zamd = charge_amontd
    zam = singularite%cotecrete + charge_amont
    dch = (qam/(largeur_seuil*(zam-zref)))**2/rdg**2
    dch = 0._DOUBLE
! RECHERCHE ITERATIVE
    num_iter = 0
    epsq = eps3*qam
    IF (charge_amont .LT. charge_aval) THEN
      dmax11d = charge_avald
      dmax11 = charge_aval
    ELSE
      dmax11d = charge_amontd
      dmax11 = charge_amont
    END IF
    dzd = dmax11d/pas
    dz = dmax11/pas
    IF (zam .LT. zav) THEN
      dmax12d = zavd
      dmax12 = zav
    ELSE
      dmax12d = zamd
      dmax12 = zam
    END IF
    zamd = dmax12d - dzd
    zam = dmax12 - dz
 50 zamd = zamd + dzd
    zam = zam + dz
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
      IF (charge .GT. 0._DOUBLE) THEN
        pwx1 = charge + dch
        pwr1 = pwx1**w32
        qamont = qamont + singularite%coeffdebit*dx*rdg*pwr1
      END IF
    END DO
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
        pwr1 = rh**d2
        pwx2 = 1._DOUBLE - pwr1
        pwr2 = pwx2**d1
        qamont = pwr2*qamont
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
        GOTO 120
      ELSE
        IF (num_iter .EQ. 1) THEN
          IF (qamont .LT. qam) THEN
            sens = -1
          ELSE IF (qamont .GT. qam) THEN
            sens = 1
            dzd = -dzd
            dz = -dz
          END IF
        ELSE IF ((qamont .GT. qam .AND. sens .EQ. -1) .OR. (qamont .LT. &
&           qam .AND. sens .EQ. 1)) THEN
          dzd = -(dzd/pas)
          dz = -(dz/pas)
          sens = -sens
        END IF
        GOTO 50
      END IF
    END IF
!Erreur%arbredappel = !arbredappel_old
    RETURN
 120 erreur%numero = 44
    erreur%ft = err_44
    erreur%ft_c = err_44c
    CALL TRAITER_ERREUR(erreur, singularite%numero, singularite%type, &
&                 temps)
    RETURN
  CASE (singularite_type_crete_coeff)
    CALL RHSBP_SECTION_S(largeur_seuil, zref, singularite%cotecrete, idt&
&                  (section), xdt(section), profil, b1plan, erreur)
    IF (0._DOUBLE .LT. zav - singularite%cotecrete) THEN
      charge_avald = zavd
      charge_aval = zav - singularite%cotecrete
    ELSE
      charge_aval = 0._DOUBLE
      charge_avald = 0.D0
    END IF
! ESTIMATION INITIALE
    pwx1d = qamd/(singularite%coeffdebit*largeur_seuil*rdg)
    pwx1 = qam/(singularite%coeffdebit*largeur_seuil*rdg)
    IF (pwx1 .GT. 0.0 .OR. (pwx1 .LT. 0.0 .AND. w23 .EQ. INT(w23))) THEN
      charge_amontd = w23*pwx1**(w23-1)*pwx1d
    ELSE IF (pwx1 .EQ. 0.0 .AND. w23 .EQ. 1.0) THEN
      charge_amontd = pwx1d
    ELSE
      charge_amontd = 0.0
    END IF
    charge_amont = pwx1**w23
    zamd = charge_amontd
    zam = singularite%cotecrete + charge_amont
    dch = (qam/(largeur_seuil*(zam-zref)))**2/rdg**2
    dch = 0._DOUBLE
! RECHERCHE ITERATIVE
    num_iter = 0
    epsq = eps3*qam
    IF (charge_amont .LT. charge_aval) THEN
      dmax13d = charge_avald
      dmax13 = charge_aval
    ELSE
      dmax13d = charge_amontd
      dmax13 = charge_amont
    END IF
    dzd = dmax13d/pas
    dz = dmax13/pas
    IF (zam .LT. zav) THEN
      dmax14d = zavd
      dmax14 = zav
    ELSE
      dmax14d = zamd
      dmax14 = zam
    END IF
    zamd = dmax14d - dzd
    zam = dmax14 - dz
 60 zamd = zamd + dzd
    zam = zam + dz
    qamont = 0._DOUBLE
    IF (0._DOUBLE .LT. zam - singularite%cotecrete) THEN
      charge = zam - singularite%cotecrete
    ELSE
      charge = 0._DOUBLE
    END IF
    IF (charge .GT. 0._DOUBLE) THEN
      pwx1 = charge + dch
      pwr1 = pwx1**w32
      qamont = singularite%coeffdebit*largeur_seuil*rdg*pwr1
    END IF
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
        pwr1 = rh**d2
        pwx2 = 1._DOUBLE - pwr1
        pwr2 = pwx2**d1
        qamont = pwr2*qamont
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
        GOTO 130
      ELSE
        IF (num_iter .EQ. 1) THEN
          IF (qamont .LT. qam) THEN
            sens = -1
          ELSE IF (qamont .GT. qam) THEN
            sens = 1
            dzd = -dzd
            dz = -dz
          END IF
        ELSE IF ((qamont .GT. qam .AND. sens .EQ. -1) .OR. (qamont .LT. &
&           qam .AND. sens .EQ. 1)) THEN
          dzd = -(dzd/pas)
          dz = -(dz/pas)
          sens = -sens
        END IF
        GOTO 60
      END IF
    END IF
!Erreur%arbredappel = !arbredappel_old
    RETURN
 130 erreur%numero = 44
    erreur%ft = err_44
    erreur%ft_c = err_44c
    CALL TRAITER_ERREUR(erreur, singularite%numero, singularite%type, &
&                 temps)
    RETURN
  CASE (singularite_type_z_t)
    zam = singularite%ptz(1)
    zamd = 0.D0
  CASE (singularite_type_q_zamont)
    dummyzerodiffd2 = 0.D0
    CALL INTERPOLATION_S_D(zam, zamd, qam, qamd, &
&                    premier_ordre_interpolation, singularite%ptq(:), &
&                    dummyzerodiffd2, singularite%ptz(:), SIZE(&
&                    singularite%ptz), erreur)
    IF (erreur%numero .NE. 0) RETURN
  CASE DEFAULT
    erreur%numero = 46
    erreur%ft = err_46
    erreur%ft_c = err_46c
    CALL TRAITER_ERREUR(erreur, singularite%numero, temps, singularite%&
&                 type)
    RETURN
  END SELECT
!Erreur%arbredappel = !arbredappel_old
  RETURN
END SUBROUTINE PSING_D




SUBROUTINE CRITIQ_D(zcrit, zcritd, section, zref, q, qd, cf1, cf1d, cf2&
& , cf2d, idt, xdt, profil, profil_plan, modelelit, loifrottement, &
& unitelisting, erreur)

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
  USE M_RHSBP_S_D
! Traitement des erreurs
  USE M_TRAITER_ERREUR_I
! Interfaces
  USE M_REPAR_I_D
  IMPLICIT NONE
!.. Arguments ..
! --------------
  DOUBLE PRECISION, INTENT(OUT) :: zcrit
  DOUBLE PRECISION, INTENT(OUT) :: zcritd
  INTEGER, INTENT(IN) :: section
! TABLEAUX DIMENSIONNES A NMSCAL
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: zref
  DOUBLE PRECISION, INTENT(IN) :: q
  DOUBLE PRECISION, INTENT(IN) :: qd
  DOUBLE PRECISION, INTENT(IN) :: cf1
  DOUBLE PRECISION, INTENT(IN) :: cf1d
  DOUBLE PRECISION, INTENT(IN) :: cf2
  DOUBLE PRECISION, INTENT(IN) :: cf2d
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
  DOUBLE PRECISION :: zkd, fcrit1d, fcrit2d, betad
  DOUBLE PRECISION :: b1, b2, s1, s2, p2, p1, rh1, rh2
  DOUBLE PRECISION :: b1d, b2d, s1d, s2d, p2d, p1d, rh1d, rh2d
  DOUBLE PRECISION :: q1, q2, deb, v, bst
  DOUBLE PRECISION :: debd, vd
  INTEGER :: num_profil, ipassage, icompt
!character(132) :: !arbredappel_old
!============================= Instructions ===========================
! INITIALISATIONS
! ---------------
  erreur%numero = 0
!arbredappel_old    = trim(!Erreur%arbredappel)
!Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>CRITIQ'
  zk = zref(section)
  num_profil = idt(section)
  pas = profil(num_profil)%pas/2._DOUBLE
  fcrit1 = 100._DOUBLE
  fcrit1d = 0.D0
  fcrit2d = 0.D0
  p1d = 0.D0
  p2d = 0.D0
  betad = 0.D0
  zkd = 0.D0
! CALCUL DE COTE CRITIQUE
! -----------------------
label_boucle_n:DO ipassage=0,2
    fcrit2 = 100._DOUBLE
    IF (ipassage .EQ. 1) THEN
      zkd = zcritd
      zk = zcrit - pas
      pas = 0.2_DOUBLE*pas
    ELSE IF (ipassage .EQ. 2) THEN
      zkd = zcritd
      zk = zcrit - pas
      pas = 0.01_DOUBLE
    END IF
    icompt = 0
    fcrit2d = 0.D0
    DO WHILE (icompt .LT. 3000 .AND. fcrit2 .GT. 0._DOUBLE)
      icompt = icompt + 1
      zcritd = zkd
      zcrit = zk + icompt*pas
      CALL RHSBP_S_D(b1, b1d, b2, b2d, bst, p1, p1d, p2, p2d, s1, s1d, &
&              s2, s2d, rh1, rh1d, rh2, rh2d, section, zcrit, zcritd, &
&              zref(section), idt, xdt, profil, profil_plan, &
&              unitelisting, erreur)
      IF (erreur%numero .NE. 0) THEN
        GOTO 100
      ELSE
! Resultats
! Donnees modifiees
! Donnees non modifiees
        vd = 0.D0
        debd = 0.D0
        CALL REPAR_D(deb, debd, v, vd, beta, betad, q1, q2, s1, s1d, s2&
&              , s2d, rh1, rh1d, rh2, rh2d, p1, p1d, p2, p2d, q, qd, cf1&
&              , cf1d, cf2, cf2d, modelelit, loifrottement, erreur)
        IF (erreur%numero .NE. 0) THEN
          GOTO 110
        ELSE
          fcrit2d = (((betad*(b1+b2)+beta*(b1d+b2d))*q**2+beta*(b1+b2)*2&
&           *q*qd)*(s1+s2)**3/gpes-beta*q**2*(b1+b2)*3*(s1+s2)**2*(s1d+&
&           s2d)/gpes)/((s1+s2)**3)**2
          fcrit2 = beta*q**2*(b1+b2)/gpes/(s1+s2)**3 - 1._DOUBLE
          IF (fcrit2 .GT. 0._DOUBLE) THEN
            fcrit1d = fcrit2d
            fcrit1 = fcrit2
          END IF
        END IF
      END IF
    END DO
  END DO label_boucle_n
  zcritd = zcritd + (pas*fcrit2d*(fcrit1-fcrit2)-pas*fcrit2*(fcrit1d-&
&   fcrit2d))/(fcrit1-fcrit2)**2
  zcrit = zcrit + pas*fcrit2/(fcrit1-fcrit2)
!Erreur%arbredappel = !arbredappel_old
  RETURN
 100 RETURN
 110 RETURN
END SUBROUTINE CRITIQ_D


SUBROUTINE PERMAT_D(z, zd, q, qd, zinit, zinitd, x, zref, cf1, cf1d, cf2&
& , cf2d, pcsing, pcsingd, idt, xdt, profil, profilplan, f1, connect, &
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
  USE M_RHSBP_S_D
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
  USE M_REPAR_I_D
  USE M_PSING_I_D
  USE M_CRITIQ_I_D
  IMPLICIT NONE
!.. Arguments ..
!---------------
! TABLEAU  DIMENSIONNE  A NbSect
  DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: z
  DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: zd
! TABLEAU  DIMENSIONNE  A NbSect
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: q
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: qd
  DOUBLE PRECISION, INTENT(IN) :: zinit
  DOUBLE PRECISION, INTENT(IN) :: zinitd
! TABLEAUX DIMENSIONNES A NMSCAL
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: x
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: zref
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf1
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf1d
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf2
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf2d
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: pcsing
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: pcsingd
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
  DOUBLE PRECISION :: zcd(nbsect)
  INTEGER :: izone, nb_zone
  DOUBLE PRECISION :: jav, js, javam, javc, jam
  DOUBLE PRECISION :: javd, jsd, javamd
  DOUBLE PRECISION :: zam1, zam2, zav, zam, yp2, zam3, &
& zam22, zam21, zmil
  DOUBLE PRECISION :: zam1d, zam2d, zavd, zam22d, zam21d, zmild
  DOUBLE PRECISION :: dx, dq, ypmil, yp22, yp21, dz
  DOUBLE PRECISION :: dqd
  DOUBLE PRECISION :: cqmvj
  DOUBLE PRECISION :: y
  DOUBLE PRECISION :: debav
  DOUBLE PRECISION :: debavd
  DOUBLE PRECISION :: hav
  DOUBLE PRECISION :: betaav
  DOUBLE PRECISION :: betaavd
  DOUBLE PRECISION :: vav
  DOUBLE PRECISION :: vavd
  DOUBLE PRECISION :: vav2
  DOUBLE PRECISION :: vav2d
  DOUBLE PRECISION :: h
  DOUBLE PRECISION :: sav, sm1, sm2
  DOUBLE PRECISION :: savd, sm1d, sm2d
  DOUBLE PRECISION :: b1, b2
  DOUBLE PRECISION :: b1d, b2d
  DOUBLE PRECISION :: bstock
  DOUBLE PRECISION :: ss1, ss2
  DOUBLE PRECISION :: ss1d, ss2d
  DOUBLE PRECISION :: rh1, rh2
  DOUBLE PRECISION :: rh1d, rh2d
  DOUBLE PRECISION :: frav, fram
  DOUBLE PRECISION :: epsil
  DOUBLE PRECISION :: sc
  DOUBLE PRECISION :: debc
  DOUBLE PRECISION :: hc
  DOUBLE PRECISION :: zcrit
  DOUBLE PRECISION :: zcritd
  DOUBLE PRECISION :: betac
  DOUBLE PRECISION :: vc
  DOUBLE PRECISION :: vc2
  DOUBLE PRECISION :: dxbeta
  DOUBLE PRECISION :: dxbetad
  DOUBLE PRECISION :: dxq
  DOUBLE PRECISION :: dxqd
  DOUBLE PRECISION :: ham
  DOUBLE PRECISION :: sam
  DOUBLE PRECISION :: samd
  DOUBLE PRECISION :: debam
  DOUBLE PRECISION :: debamd
  DOUBLE PRECISION :: betaam
  DOUBLE PRECISION :: betaamd
  DOUBLE PRECISION :: vam
  DOUBLE PRECISION :: vamd
  DOUBLE PRECISION :: vam2
  DOUBLE PRECISION :: vam2d
  DOUBLE PRECISION :: dxv
  DOUBLE PRECISION :: dxvd
  DOUBLE PRECISION :: xp, yp
  DOUBLE PRECISION :: xpd, ypd
  DOUBLE PRECISION :: xq, yq
  DOUBLE PRECISION :: xqd, yqd
  DOUBLE PRECISION :: ymin, ymax
  DOUBLE PRECISION :: ymind, ymaxd
  DOUBLE PRECISION :: xmin, xmax
  DOUBLE PRECISION :: xmind, xmaxd
  DOUBLE PRECISION :: xm, ym
  DOUBLE PRECISION :: xmd, ymd
  DOUBLE PRECISION :: q1, q2
  DOUBLE PRECISION :: p1, p2
  DOUBLE PRECISION :: p1d, p2d
  INTEGER :: j, iecrit, iter1
  INTEGER :: debut_bief
  INTEGER :: fin_bief
! Compteur sur les singularites
  INTEGER :: ising
  INTEGER :: num_bief
  DOUBLE PRECISION :: absc_rel
  CHARACTER(len=132) :: arbredappel_old
  INTRINSIC TRIM
  INTRINSIC SIGN
  INTRINSIC SIZE
  INTRINSIC DABS
  INTRINSIC DMIN1
  INTRINSIC DMAX1
  DOUBLE PRECISION :: arg1
  DOUBLE PRECISION :: arg1d
  DOUBLE PRECISION :: dabs4
  DOUBLE PRECISION :: dabs3
  DOUBLE PRECISION :: dabs2
  DOUBLE PRECISION :: dabs1
  DOUBLE PRECISION :: dabs0
!============================= Instructions ===========================
! INITIALISATIONS
! ---------------
  erreur%numero = 0
  arbredappel_old = TRIM(erreur%arbredappel)
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
  zd(j) = zinitd
  z(j) = zinit
  zam1d = zinitd
  zam1 = zinit
! 1ER  TEST SUR LA CONDITION AVAL DU BIEF :
! COHERENCE AVEC LA COTE DES FONDS A L'AVAL
! -----------------------------------------
  IF (z(j) .LT. zref(j)) THEN
    erreur%numero = 37
    erreur%ft = err_37
    erreur%ft_c = err_37c
    CALL TRAITER_ERREUR(erreur, temps, numbief, z(j), zref(j))
    RETURN
  ELSE
    betaamd = 0.D0
    vamd = 0.D0
    p1d = 0.D0
    betaavd = 0.D0
    p2d = 0.D0
    vavd = 0.D0
    ymind = 0.D0
    xmd = 0.D0
    xpd = 0.D0
    xqd = 0.D0
    xmind = 0.D0
    zam2d = 0.D0
    ypd = 0.D0
    zcd = 0.D0
    debamd = 0.D0
    ymaxd = 0.D0
    debavd = 0.D0
    xmaxd = 0.D0
    zcritd = 0.D0
! ELEMENT DE LA PREMIERE SECTION DE CALCUL AVAL = AV
! --------------------------------------------------
 100 iter1 = -1
    zavd = zam1d
    zav = zam1
    dx = x(j) - x(j-1)
    dqd = qd(j) - qd(j-1)
    dq = q(j) - q(j-1)
!   CQMVJ = CQMV
    CALL RHSBP_S_D(b1, b1d, b2, b2d, bstock, p1, p1d, p2, p2d, sm1, sm1d&
&            , sm2, sm2d, rh1, rh1d, rh2, rh2d, j, zav, zavd, zref(j), &
&            idt, xdt, profil, profilplan, unitelisting, erreur)
    IF (erreur%numero .EQ. 0) THEN
      y = zav - zref(j)
      IF (y .LT. eps6) THEN
        GOTO 130
      ELSE
! Resultats
! Donnees modifiees
! Donnees non modifiees
! Erreur
        CALL REPAR_D(debav, debavd, vav, vavd, betaav, betaavd, q1, q2, &
&              sm1, sm1d, sm2, sm2d, rh1, rh1d, rh2, rh2d, p1, p1d, p2, &
&              p2d, q(j), qd(j), cf1(j), cf1d(j), cf2(j), cf2d(j), &
&              modelelit, loifrottement, erreur)
        IF (erreur%numero .NE. 0) THEN
          GOTO 140
        ELSE
          savd = sm1d + sm2d
          sav = sm1 + sm2
          javd = 2*q(j)*(qd(j)*debav-q(j)*debavd)/debav**3
          jav = (q(j)/debav)**2
          javd = javd*SIGN(1.d0, jav*q(j))
          jav = SIGN(jav, q(j))
          vav2d = 2*vav*vavd
          vav2 = vav**2
          hav = zav + 0.5_DOUBLE*betaav*vav2/gpes
          h = (sm1+sm2)/(b1+b2)
          CALL FROUDE_S(frav, betaav, vav, h, j, connect, x, erreur)
          IF (erreur%numero .NE. 0) THEN
            GOTO 150
          ELSE
! PASSAGE A LA SECTION AMONT = AM
! -------------------------------
            j = j - 1
            epsil = eps3*profil(idt(j))%pas
! TEST DE PRESENCE D'UNE SINGULARITE
! ----------------------------------
            IF (SIZE(singularite) .NE. 0) THEN
              DO ising=1,SIZE(singularite)
                IF (singularite(ising)%section .EQ. j) GOTO 110
              END DO
              GOTO 120
!/RESULTATS/
!/DONNEES NON MODIFIEES/
!/Erreur/
 110          CALL PSING_D(zam2, zam2d, singularite(ising), zref(j)&
&                    , zav, zavd, q(j), qd(j), profil, &
&                    profilplan%b1, idt, xdt, j, temps, erreur)
              IF (erreur%numero .NE. 0) THEN
                GOTO 160
              ELSE
                GOTO 300
              END IF
            END IF
! TEST DU PASSAGE EN REGIME TORRENTIEL
! ------------------------------------
! /RESULTATS/
! /DONNEES NON MODIFIEES/
! /DONNEES NON MODIFIEES
!  (ARGUMENTS DE S.P APPELES)/
! Erreur
 120        CALL CRITIQ_D(zcrit, zcritd, j, zref, q(j), qd(j), cf1(j), &
&                   cf1d(j), cf2(j), cf2d(j), idt, xdt, profil, &
&                   profilplan, modelelit, loifrottement, unitelisting, &
&                   erreur)
            zcd(j) = zcritd
            zc(j) = zcrit
            IF (erreur%numero .NE. 0) THEN
              GOTO 170
            ELSE
              CALL RHSBP_S_D(b1, b1d, b2, b2d, bstock, p1, p1d, p2, p2d&
&                      , ss1, ss1d, ss2, ss2d, rh1, rh1d, rh2, rh2d, j, &
&                      zcrit, zcritd, zref(j), idt, xdt, profil, &
&                      profilplan, unitelisting, erreur)
              IF (erreur%numero .NE. 0) THEN
                GOTO 180
              ELSE
                y = zcrit - zref(j)
                IF (y .LT. eps6) THEN
                  GOTO 190
                ELSE
! Resultats
! Donnees modifiees
! Donnees non modifiees
                  CALL REPAR(debc, vc, betac, q1, q2, ss1, ss2, rh1, &
&                         rh2, p1, p2, q(j), cf1(j), cf2(j), modelelit, &
&                         loifrottement, profil(idt(j))%Nom, erreur)
                  IF (erreur%numero .NE. 0) THEN
                    GOTO 210
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
                      zcd(j) = zcritd
                      zc(j) = zcrit
!
! DETECTION D'UN RESSAUT HYDRAULIQUE OU D'UNE CHUTE
!
                      zam2d = zcritd
                      zam2 = zcrit
                      IF (impression .AND. iecrit .EQ. 0) WRITE(&
&                                                         unitelisting, &
&                                                         2000) &
&                                                         debut_bief, q(&
&                                                         debut_bief), &
&                                                         fin_bief, z(&
&                                                         fin_bief)
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
                      zam1d = zavd + dx*javd
                      zam1 = zav + jav*dx
! DEMARRAGE DE L'ALGORITHME ITERATIF VISANT A OBTENIR ZAM2 = ZAM1
! ---------------------------------------------------------------
 200                  iter1 = iter1 + 1
!-------------------------------------------------------
! Calcul des grandeurs hydrauliques correspondant a ZAM1
!-------------------------------------------------------
                      CALL RHSBP_S_D(b1, b1d, b2, b2d, bstock, p1, p1d, &
&                              p2, p2d, ss1, ss1d, ss2, ss2d, rh1, rh1d&
&                              , rh2, rh2d, j, zam1, zam1d, zref(j), idt&
&                              , xdt, profil, profilplan, unitelisting, &
&                              erreur)
                      IF (erreur%numero .NE. 0) THEN
                        GOTO 220
                      ELSE
!----------------------------
! Test de hauteur d'eau nulle
!----------------------------
                        y = zam1 - zref(j)
                        IF (y .LT. eps6) THEN
                          GOTO 230
                        ELSE
!---------------------------------
! Calcul de la debitance pour ZAM1
!---------------------------------
! Resultats
! Donnees modifiees
! Donnees non modifiees
                          CALL REPAR_D(debam, debamd, vam, vamd, betaam&
&                                , betaamd, q1, q2, ss1, ss1d, ss2, ss2d&
&                                , rh1, rh1d, rh2, rh2d, p1, p1d, p2, &
&                                p2d, q(j), qd(j), cf1(j), cf1d(j), cf2(&
&                                j), cf2d(j), modelelit, loifrottement, &
&                                erreur)
                          IF (erreur%numero .NE. 0) THEN
                            GOTO 240
                          ELSE
                            samd = ss1d + ss2d
                            sam = ss1 + ss2
                            arg1d = 2*q(j)*qd(j)
                            arg1 = q(j)**2
                            javamd = (arg1d*SIGN(1.d0, arg1*q(j))*&
&                             0.5_DOUBLE*(debav**2+debam**2)-SIGN(arg1, &
&                             q(j))*0.5_DOUBLE*(2*debav*debavd+2*debam*&
&                             debamd))/(0.5_DOUBLE*(debav**2+debam**2))&
&                             **2
                            javam = SIGN(arg1, q(j))/(0.5_DOUBLE*(debav&
&                             **2+debam**2))
                            vam2d = 2*vam*vamd
                            vam2 = vam**2
!---------------------
! termes de l'equation
!---------------------
                            dxbetad = 0.25_DOUBLE*((betaavd-betaamd)*(&
&                             vav2+vam2)+(betaav-betaam)*(vav2d+vam2d))/&
&                             gpes
                            dxbeta = 0.25_DOUBLE*(betaav-betaam)*(vav2+&
&                             vam2)/gpes
                            dxqd = 0.5_DOUBLE*dqd*((betaav-cqmvj)*vav/&
&                             sav+(betaam-cqmvj)*vam/sam)/gpes + &
&                             0.5_DOUBLE*dq*(((betaavd*vav+(betaav-cqmvj&
&                             )*vavd)*sav-(betaav-cqmvj)*vav*savd)/sav**&
&                             2+((betaamd*vam+(betaam-cqmvj)*vamd)*sam-(&
&                             betaam-cqmvj)*vam*samd)/sam**2)/gpes
                            dxq = 0.5_DOUBLE*dq/gpes*((betaav-cqmvj)*vav&
&                             /sav+(betaam-cqmvj)*vam/sam)
                            dxvd = 0.5_DOUBLE*(betaavd*vav2+betaav*vav2d&
&                             -betaamd*vam2-betaam*vam2d)/gpes
                            dxv = 0.5_DOUBLE*(betaav*vav2-betaam*vam2)/&
&                             gpes
!----------------------------------------------------
! PERTE DE CHARGE SINGULIERE LIEE A UN RALENTISSEMENT
!----------------------------------------------------
                            IF (betaam*vam .GT. betaav*vav .AND. pcsing(&
&                               j+1) .LT. eps2) THEN
                              jsd = 0.5_DOUBLE*cpcs*2*(betaam*vam-betaav&
&                               *vav)*(betaamd*vam+betaam*vamd-betaavd*&
&                               vav-betaav*vavd)/gpes
                              js = 0.5_DOUBLE*cpcs*(betaam*vam-betaav*&
&                               vav)**2/gpes
                            ELSE
                              js = 0._DOUBLE
                              jsd = 0.D0
                            END IF
!-----------------------------------------------
! PERTE DE CHARGE SINGULIERE LIEE A UN EVENEMENT
!-----------------------------------------------
                            IF (vam .GT. 0._DOUBLE) THEN
                              jsd = jsd + 0.5_DOUBLE*(pcsingd(j+1)*&
&                               betaam*vam2+pcsing(j+1)*(betaamd*vam2+&
&                               betaam*vam2d))/gpes
                              js = js + 0.5_DOUBLE*pcsing(j+1)*betaam*&
&                               vam2/gpes
                            ELSE
                              jsd = jsd - 0.5_DOUBLE*(pcsingd(j+1)*&
&                               betaav*vav2+pcsing(j+1)*(betaavd*vav2+&
&                               betaav*vav2d))/gpes
                              js = js - 0.5_DOUBLE*pcsing(j+1)*betaav*&
&                               vav2/gpes
                            END IF
                            h = (ss1+ss2)/(b1+b2)
                            CALL FROUDE_S(fram, betaam, vam, h, j, &
&                                   connect, x, erreur)
                            IF (erreur%numero .NE. 0) THEN
                              GOTO 250
                            ELSE
!------------------------------------------
! COTE OBTENUE EN AMONT A POSTERIORI : ZAM2
!------------------------------------------
                              zam2d = zavd + dx*javamd + jsd + dxbetad +&
&                               dxqd + dxvd
                              zam2 = zav + javam*dx + js + dxbeta + dxq &
&                               + dxv
!----------------------------------
! PASSAGE EN TORRENTIEL NON DETECTE
!----------------------------------
                              IF (fram .LT. 1.d0) THEN
! CHOIX DES POINTS P,Q A LA BASE DE L'INTERPOLATION
                                IF (iter1 .EQ. 0) THEN
                                  xpd = zam1d
                                  xp = zam1
                                  ypd = zam2d - zam1d
                                  yp = zam2 - zam1
                                  xqd = zam2d
                                  xq = zam2
                                  IF (yp .GE. 0.) THEN
                                    dabs0 = yp
                                  ELSE
                                    dabs0 = -yp
                                  END IF
                                  IF (dabs0 .LE. epsil) THEN
                                    GOTO 300
                                  ELSE
                                    zam1d = zam2d
                                    zam1 = zam2
                                    GOTO 200
                                  END IF
                                ELSE
                                  IF (iter1 .EQ. 1) THEN
                                    yqd = zam2d - zam1d
                                    yq = zam2 - zam1
                                    IF (yq .GE. 0.) THEN
                                      dabs1 = yq
                                    ELSE
                                      dabs1 = -yq
                                    END IF
                                    IF (dabs1 .LE. epsil) THEN
                                      GOTO 300
                                    ELSE
                                      IF (yp .GT. yq) THEN
                                        ymind = yqd
                                        ymin = yq
                                      ELSE
                                        ymind = ypd
                                        ymin = yp
                                      END IF
                                      IF (yp .LT. yq) THEN
                                        ymaxd = yqd
                                        ymax = yq
                                      ELSE
                                        ymaxd = ypd
                                        ymax = yp
                                      END IF
                                      IF (ymax .EQ. yp) THEN
                                        xmind = xqd
                                        xmin = xq
                                        xmaxd = xpd
                                        xmax = xp
                                      ELSE
                                        xmind = xpd
                                        xmin = xp
                                        xmaxd = xqd
                                        xmax = xq
                                      END IF
                                    END IF
                                  ELSE
                                    ymd = zam2d - zam1d
                                    ym = zam2 - zam1
                                    IF (ym .GE. 0.) THEN
                                      dabs2 = ym
                                    ELSE
                                      dabs2 = -ym
                                    END IF
                                    IF (dabs2 .LE. epsil) THEN
                                      GOTO 300
                                    ELSE IF (ymin*ymax .LE. 0._DOUBLE) &
&                                   THEN
                                      IF (ym .LE. 0._DOUBLE) THEN
                                        xmind = xmd
                                        xmin = xm
                                        ymind = ymd
                                        ymin = ym
                                      ELSE
                                        xmaxd = xmd
                                        xmax = xm
                                        ymaxd = ymd
                                        ymax = ym
                                      END IF
                                    ELSE IF (ymax .LT. 0._DOUBLE) THEN
                                      xmind = xmaxd
                                      xmin = xmax
                                      ymind = ymaxd
                                      ymin = ymax
                                      xmaxd = xmd
                                      xmax = xm
                                      ymaxd = ymd
                                      ymax = ym
                                    ELSE
                                      xmaxd = xmind
                                      xmax = xmin
                                      ymaxd = ymind
                                      ymax = ymin
                                      xmind = xmd
                                      xmin = xm
                                      ymind = ymd
                                      ymin = ym
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
                                      GOTO 300
                                    ELSE
                                      xmd = xmind - (xmaxd-xmind)*ymin/(&
&                                       ymax-ymin) - (xmax-xmin)*(ymind*&
&                                       (ymax-ymin)-ymin*(ymaxd-ymind))/&
&                                       (ymax-ymin)**2
                                      xm = xmin - (xmax-xmin)*(ymin/(&
&                                       ymax-ymin))
                                      IF (xm .LT. zref(j)) THEN
                                        xm = zref(j)
                                        xmd = 0.D0
                                      ELSE
                                        xm = xm
                                      END IF
                                      IF (ymin .LE. 0._DOUBLE) THEN
                                        xmd = 0.5_DOUBLE*(xmd+xmaxd)
                                        xm = 0.5_DOUBLE*(xm+xmax)
                                      ELSE
                                        xmd = 0.5_DOUBLE*(xmd+xmind)
                                        xm = 0.5_DOUBLE*(xm+xmin)
                                      END IF
                                    END IF
                                  ELSE
                                    xmd = (xmind+xmaxd)/2._DOUBLE
                                    xm = (xmin+xmax)/2._DOUBLE
                                  END IF
!------------------------
! TEST DE NON CONVERGENCE
!------------------------
                                  IF (iter1 .GE. itmax1) THEN
                                    GOTO 400
                                  ELSE
! FIN DE L'ALGORITHME ITERATIF
                                    zam1d = xmd
                                    zam1 = xm
                                    GOTO 200
                                  END IF
                                END IF
                              END IF
                            END IF
                          END IF
                        END IF
                      END IF
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
                      zcd(j) = zam2d
                      zc(j) = zam2
!
! DETECTION D'UN RESSAUT HYDRAULIQUE OU D'UNE CHUTE
!
                      iecrit = 1
                    END IF
                  END IF
                END IF
              END IF
            END IF
!
! RESULTAT DEFINITIF POUR LA SECTION AMONT
! ----------------------------------------
 300        zd(j) = zam2d
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
            zam1d = zam2d
            zam1 = zam2
            IF (j .GT. debut_bief) THEN
              GOTO 100
            ELSE
              GOTO 260
            END IF
          END IF
        END IF
      END IF
    END IF
    RETURN
 130 erreur%numero = 31
    erreur%ft = err_31
    erreur%ft_c = err_31c
    num_bief = NUM_BIEF_S(connect, j, erreur)
    absc_rel = x(j) - x(connect%originebief(num_bief))
    CALL TRAITER_ERREUR(erreur, temps, j, num_bief, absc_rel, zav, zref(&
&                 j))
    RETURN
 140 RETURN
 150 RETURN
 160 RETURN
 170 RETURN
 180 RETURN
 190 erreur%numero = 35
    erreur%ft = err_35
    erreur%ft_c = err_35c
    num_bief = NUM_BIEF_S(connect, j, erreur)
    absc_rel = x(j) - x(connect%originebief(num_bief))
    CALL TRAITER_ERREUR(erreur, temps, j, num_bief, absc_rel, zcrit, &
&                 zref(j))
    RETURN
 210 RETURN
 220 RETURN
 230 erreur%numero = 31
    erreur%ft = err_31
    erreur%ft_c = err_31c
    num_bief = NUM_BIEF_S(connect, j, erreur)
    absc_rel = x(j) - x(connect%originebief(num_bief))
    CALL TRAITER_ERREUR(erreur, temps, j, num_bief, absc_rel, zav, zref(&
&                 j))
    RETURN
 240 RETURN
 250 RETURN
 260 nb_zone = izone
    zam21d = 0.D0
    zam22d = 0.D0
!-------------------------------------
!  BOUCLE SUR LES ZONES TORRENTIELLES
!  BALAYAGE AMONT -AVAL
!-------------------------------------
    DO izone=1,nb_zone
      j = idebtor(izone) - 1
      IF (j .EQ. 0) j = idebtor(izone)
 101  zam = z(j)
      zam2 = zref(j+1)
      iter1 = -1
      zam1 = zam
      dx = x(j+1) - x(j)
      dq = q(j+1) - q(j)
!       CQMVJ = CQMV
      CALL RHSBP_S(b1, b2, bstock, p1, p2, sm1, sm2, rh1, rh2, j, zam1, &
&            zref(j), idt, xdt, profil, profilplan, unitelisting, erreur&
&           )
      IF (erreur%numero .NE. 0) THEN
        GOTO 270
      ELSE
! Resultats
! Donnees modifiees
! Donnees non modifiees
! Erreur
        CALL REPAR(debam, vam, betaam, q1, q2, sm1, sm2, rh1, rh2, p1&
&               , p2, q(j), cf1(j), cf2(j), modelelit, loifrottement&
&               , profil(idt(j))%Nom, erreur)
        IF (erreur%numero .NE. 0) THEN
          GOTO 280
        ELSE
          sam = sm1 + sm2
          jam = (q(j)/debam)**2
          jam = SIGN(jam, q(j))
          vam2 = vam**2
!
! DICHOTOMIE  : Z est compris entre ZRZF et ZC avec les cas analytiques la dichotomoie commence a 0.001 + convergence a 0.001
          iter1 = 0
          dz = 0.001001_double
          IF (iter1 .EQ. 0) THEN
            yp2 = 0._DOUBLE
            zam21 = zref(j+1) + dz
            zam22d = zcd(j+1)
            zam22 = zc(j+1)
            zam21d = 0.D0
          END IF
!
! Initialisation des valeurs de la fonctions aux bornes
!
          zam2 = zam21
          CALL RHSBP_S(b1, b2, bstock, p1, p2, sm1, sm2, rh1, rh2, j + 1&
&                , zam2, zref(j+1), idt, xdt, profil, profilplan, &
&                unitelisting, erreur)
! Resultats
! Donnees modifiees
! Donnees non modifiees
! Erreur
          CALL REPAR(debav, vav, betaav, q1, q2, sm1, sm2, rh1, rh2, &
&                 p1, p2, q(j+1), cf1(j+1), cf2(j+1), modelelit, &
&                 loifrottement, profil(idt(j+1))%Nom, erreur)
          IF (erreur%numero .NE. 0) THEN
            GOTO 290
          ELSE
            sav = sm1 + sm2
            jav = (q(j+1)/debav)**2
            jav = SIGN(jav, q(j+1))
            vav2 = vav**2
            arg1 = q(j+1)**2
            javam = SIGN(arg1, q(j+1))/(0.5_DOUBLE*(debav**2+debam**2))
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
              GOTO 310
            ELSE
!---------------------
! COTE OBTENUE  F (Z)
!---------------------
              zam3 = zam1 - jav*dx - dxbeta - dxq - dxv
              yp21 = zam3 - zam2
              zam2 = zam22
              CALL RHSBP_S(b1, b2, bstock, p1, p2, sm1, sm2, rh1, rh2, j&
&                    + 1, zam2, zref(j+1), idt, xdt, profil, profilplan&
&                    , unitelisting, erreur)
              IF (erreur%numero .NE. 0) THEN
                GOTO 320
              ELSE
                y = zam2 - zref(j+1)
! Resultats
! Donnees modifiees
! Donnees non modifiees
! Erreur
                CALL REPAR(debav, vav, betaav, q1, q2, sm1, sm2, rh1&
&                       , rh2, p1, p2, q(j+1), cf1(j+1), cf2(j+1), &
&                       modelelit, loifrottement, profil(idt(j+1))%Nom&
&                       , erreur)
                IF (erreur%numero .NE. 0) THEN
                  GOTO 330
                ELSE
                  sav = sm1 + sm2
                  jav = (q(j+1)/debav)**2
                  jav = SIGN(jav, q(j+1))
                  vav2 = vav**2
                  arg1 = q(j+1)**2
                  javam = SIGN(arg1, q(j+1))/(0.5_DOUBLE*(debav**2+debam&
&                   **2))
!---------------------
! termes de l'equation
!---------------------
                  dxbeta = 0.25_DOUBLE*(betaav-betaam)*(vav2+vam2)/gpes
                  dxq = 0.5_DOUBLE*dq/gpes*((betaav-cqmvj)*vav/sav+(&
&                   betaam-cqmvj)*vam/sam)
                  dxv = 0.5_DOUBLE*(betaav*vav2-betaam*vam2)/gpes
                  h = sav/(b1+b2)
                  CALL FROUDE_S(fram, betaam, vam, h, j, connect, x, &
&                         erreur)
                  IF (erreur%numero .NE. 0) THEN
                    GOTO 340
                  ELSE
!---------------------
! COTE OBTENUE  F (Z)
!---------------------
!
!  Recherche des zeros de F par dichotomie
!
                    zam3 = zam1 - jav*dx - dxbeta - dxq - dxv
                    yp22 = zam3 - zam2
                    zmild = (zam22d+zam21d)/2.d0
                    zmil = (zam22+zam21)/2.d0
!
!   Calcul de F(Z) pour ces 3 valeurs
!
 201                zam2 = zmil
!
! Calcul de la fonction non lineaire a annuler aux pts ZAM21 ZAM22 ZMIL
! ---------------------------------------------------------------------
!
                    CALL RHSBP_S(b1, b2, bstock, p1, p2, sm1, sm2, rh1, &
&                          rh2, j + 1, zam2, zref(j+1), idt, xdt, profil&
&                          , profilplan, unitelisting, erreur)
                    IF (erreur%numero .NE. 0) THEN
                      GOTO 350
                    ELSE
                      y = zam2 - zref(j+1)
                      IF (y .LT. eps6) THEN
                        GOTO 360
                      ELSE
! Resultats
! Donnees modifiees
! Donnees non modifiees
! Erreur
                        CALL REPAR(debav, vav, betaav, q1, q2, sm1, &
&                               sm2, rh1, rh2, p1, p2, q(j+1), cf1(j+1)&
&                               , cf2(j+1), modelelit, loifrottement, &
&                               profil(idt(j+1))%Nom, erreur)
                        IF (erreur%numero .NE. 0) THEN
                          GOTO 370
                        ELSE
                          sav = sm1 + sm2
                          jav = (q(j+1)/debav)**2
                          jav = SIGN(jav, q(j+1))
                          vav2 = vav**2
                          arg1 = q(j+1)**2
                          javam = SIGN(arg1, q(j+1))/(0.5_DOUBLE*(debav&
&                           **2+debam**2))
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
                            GOTO 380
                          ELSE
!----------------------
! COTE OBTENUE  F (Z)
!----------------------
                            zam3 = zam1 - jav*dx - js - dxbeta - dxq - &
&                             dxv
                            ypmil = zam3 - zam2
                            IF (zam22 - zam21 .GT. 0.0001_DOUBLE) THEN
                              IF (yp21*ypmil .LE. 0.0_DOUBLE) THEN
                                zam22d = zmild
                                zam22 = zmil
                                zmild = (zam21d+zmild)/2.0_DOUBLE
                                zmil = (zam21+zmil)/2.0_DOUBLE
                                yp22 = ypmil
                              ELSE
                                zam21d = zmild
                                zam21 = zmil
                                zmild = (zam22d+zmild)/2.0_DOUBLE
                                zmil = (zam22+zmil)/2.0_DOUBLE
                                yp21 = ypmil
                              END IF
                              GOTO 201
                            END IF
                          END IF
                        END IF
                      END IF
                    END IF
                    zd(j+1) = zmild
                    z(j+1) = zmil
                    j = j + 1
                    IF (j .LT. idebtor(izone) + kpass(izone)) THEN
                      GOTO 101
                    ELSE IF (fram .GE. 1.1_DOUBLE) THEN
!
! Verification de la position du ressaut - fonction impulsion
!
                      IF (j + 1 .EQ. fin_bief) THEN
                        GOTO 390
                      ELSE
                        CALL RHSB1_S(fimp(j), j, z(j), zref(j), idt, xdt&
&                              , profil, f1, unitelisting, erreur)
                        CALL RHSB1_S(fimp(j+1), j + 1, z(j+1), zref(j+1)&
&                              , idt, xdt, profil, f1, unitelisting, &
&                              erreur)
                        CALL RHSBP_S(b1, b2, bstock, p1, p2, sm1, sm2, &
&                              rh1, rh2, j + 1, z(j+1), zref(j+1), idt, &
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
&                       ) GOTO 101
                      END IF
                    END IF
                  END IF
                END IF
              END IF
            END IF
          END IF
        END IF
      END IF
    END DO
    GOTO 102
 270 RETURN
 280 RETURN
 290 RETURN
 310 RETURN
 320 RETURN
 330 RETURN
 340 RETURN
 350 RETURN
 360 erreur%numero = 31
    erreur%ft = err_31
    erreur%ft_c = err_31c
    num_bief = NUM_BIEF_S(connect, j, erreur)
    absc_rel = x(j) - x(connect%originebief(num_bief))
    CALL TRAITER_ERREUR(erreur, temps, j, num_bief, absc_rel, zav, zref(&
&                 j))
    RETURN
 370 RETURN
 380 RETURN
 390 zd(j+1) = zd(j)
    z(j+1) = z(j)
!--------------------
! Fin des traitements
!--------------------
!  Erreur%arbredappel = arbredappel_old
 102 RETURN
 400 erreur%numero = 38
    erreur%ft = err_38
    erreur%ft_c = err_38c
    CALL TRAITER_ERREUR(erreur, temps, numbief, j)
    RETURN
  END IF
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
END SUBROUTINE PERMAT_D





SUBROUTINE CALC_PC_CONFLU_D(pcsing, pcsingd, z, zd, q, qd, x, zref, &
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
  USE M_RHSBP_S_D
  USE M_INTERPOLATION_S
  IMPLICIT NONE
! Arguments
  DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: pcsing
  DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: pcsingd
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: z
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: zd
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: q
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: qd
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
  TYPE CONF_T_D
      SEQUENCE
      DOUBLE PRECISION, DIMENSION(3) :: debit
      DOUBLE PRECISION, DIMENSION(3) :: largeur
  END TYPE CONF_T_D
  TYPE(CONF_T) :: conf
  TYPE(CONF_T_D) :: confd
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
  DOUBLE PRECISION :: c6abcd(3)
  DOUBLE PRECISION :: alpha_c6(6), qc6(5)
  DOUBLE PRECISION :: c6qinf(6), c6qsup(6)
  DOUBLE PRECISION :: c6qinfd(6), c6qsupd(6)
  DOUBLE PRECISION :: c6(2), c6a, c6b, c6c
  DOUBLE PRECISION :: c6d(2), c6ad, c6bd, c6cd
  DOUBLE PRECISION :: angle_conf
  DOUBLE PRECISION :: lf, ll, lp, llp, qp, ql, qpl
  DOUBLE PRECISION :: lfd, lld, lpd, llpd, qpd, qld, qpld
! elargissement branche amont princ / branche aval
  DOUBLE PRECISION :: elargissement
  DOUBLE PRECISION :: elargissementd
  DOUBLE PRECISION :: qinf, qsup, c6_inf, c6_sup
  DOUBLE PRECISION :: rapport
  DOUBLE PRECISION :: rapportd
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
  DOUBLE PRECISION :: dabs2
  DOUBLE PRECISION :: dabs1
  DOUBLE PRECISION :: dabs0
!Character(132)  :: !arbredappel_old ! ancien arbre d'appel
  DATA alpha_c6 /30., 40., 50., 60., 70., 80./
  DATA qc6 /1., 1.5, 2., 3., 5./
!============================== Instructions ================================
! INITIALISATIONS
!----------------
  erreur%numero = 0
!arbredappel_old = trim(!Erreur%arbredappel)
!Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>CALC_PC_CONFLU'
  type_riviere = type_derniere_section
  qld = 0.D0
  qpd = 0.D0
  c6d = 0.D0
  c6abcd = 0.D0
  lfd = 0.D0
  lld = 0.D0
  lpd = 0.D0
  confd%debit = 0.D0
  confd%largeur = 0.D0
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
! le numero de la section est donc
        conf%section(iext) = idxmin
! designation des profils en abscisses absolues
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
! le numero est donc
        conf%section(iext) = idxmin
! dernier profil de la branche
      ELSE IF (type_riviere .EQ. type_derniere_section) THEN
        IF (debut_bief) THEN
          conf%section(iext) = connect%originebief(num_bief)
        ELSE
          conf%section(iext) = connect%finbief(num_bief)
        END IF
      END IF
      IF (debut_bief) THEN
        IF (q(conf%section(iext)) .GT. 0._DOUBLE) THEN
          conf%position(iext) = branche_aval
        ELSE IF (q(conf%section(iext)) .LT. 0._DOUBLE) THEN
          conf%position(iext) = branche_amont
          nb_branche_amont = nb_branche_amont + 1
        ELSE
          GOTO 200
        END IF
      ELSE IF (q(conf%section(iext)) .GT. 0._DOUBLE) THEN
        conf%position(iext) = branche_amont
        nb_branche_amont = nb_branche_amont + 1
      ELSE IF (q(conf%section(iext)) .LT. 0._DOUBLE) THEN
        conf%position(iext) = branche_aval
      ELSE
        GOTO 220
      END IF
      confd%debit(iext) = qd(conf%section(iext))
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
      CALL RHSBP_SECTION_S_D(conf%largeur(iext), confd%largeur(iext), &
&                      zref(num_sect), z(num_sect), zd(num_sect), idt(&
&                      num_sect), xdt(num_sect), profil, profilplan%b1, &
&                      erreur)
      IF (erreur%numero .NE. 0) GOTO 210
    END DO label_ext
! TEST NBRE BRANCHES AMONT
    IF (nb_branche_amont .GT. 2) THEN
      GOTO 160
    ELSE
! Calcul de Qpl=Qp/Ql
! -------------------
      q_max = 0._DOUBLE
      DO iext=1,nb_ext
        IF (conf%position(iext) .EQ. branche_aval) THEN
! BRANCHE AVAL
          conf%nature(iext) = branche_princ_aval
          nb_branche_aval = nb_branche_aval + 1
! TEST DEFLUENCE
          IF (nb_branche_aval .GT. 1) GOTO 190
        ELSE IF (conf%debit(iext) .LT. q_max) THEN
          q_max = q_max
        ELSE
          q_max = conf%debit(iext)
        END IF
      END DO
      DO iext=1,nb_ext
        IF (conf%position(iext) .EQ. branche_amont .AND. conf%debit(iext&
&           ) .EQ. q_max) THEN
          conf%nature(iext) = branche_princ_amont
          qpd = confd%debit(iext)
          qp = conf%debit(iext)
        ELSE IF (conf%position(iext) .EQ. branche_amont .AND. conf%debit&
&           (iext) .LT. q_max) THEN
          conf%nature(iext) = branche_laterale
          qld = confd%debit(iext)
          ql = conf%debit(iext)
        END IF
      END DO
      qpld = (qpd*ql-qp*qld)/ql**2
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
          lpd = confd%largeur(iext)
          lp = conf%largeur(iext)
        END IF
        IF (conf%nature(iext) .EQ. branche_laterale) THEN
          lld = confd%largeur(iext)
          ll = conf%largeur(iext)
        END IF
        IF (conf%nature(iext) .EQ. branche_princ_aval) THEN
          lfd = confd%largeur(iext)
          lf = conf%largeur(iext)
        END IF
      END DO
      llpd = (lld*lp-ll*lpd)/lp**2
      llp = ll/lp
      elargissementd = lfd - lpd
      elargissement = lf - lp
! TEST DOMAINE
!-------------
      IF (llp .LT. 0.25_DOUBLE) THEN
        IF (unitelisting .GT. 0) WRITE(unitelisting, 10010)
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
        DO jabaque=abaque_a,abaque_c
          numero_abaque = (iabaque-1)*3 + jabaque
! Chargement de C6 pour alpha_C6=30->80 pour Q> & < Qp/Ql
! ----------------------------------------------------
          DO j=1,5-1
            IF (qpl .GE. qc6(j) .AND. qpl .LE. qc6(j+1)) GOTO 140
          END DO
          GOTO 150
 140      qinf = qc6(j)
          qsup = qc6(j+1)
          DO k=1,6
            c6qinfd(k) = 0.D0
            c6qinf(k) = abaque(numero_abaque, k, j)
            c6qsupd(k) = 0.D0
            c6qsup(k) = abaque(numero_abaque, k, j+1)
          END DO
! Calcul de C6a, C6b, C6c
! -----------------------
 150      CALL INTERPOLATION_S(c6_inf, angle_conf, 1, alpha_c6, c6qinf, &
&                        6, erreur)
          IF (erreur%numero .NE. 0) THEN
            GOTO 170
          ELSE
            CALL INTERPOLATION_S(c6_sup, angle_conf, 1, alpha_c6, c6qsup&
&                          , 6, erreur)
            IF (erreur%numero .NE. 0) THEN
              GOTO 180
            ELSE
! RESULTAT
              rapportd = qpld/(qsup-qinf)
              rapport = (qpl-qinf)/(qsup-qinf)
              c6abcd(jabaque) = c6_sup*rapportd - c6_inf*rapportd
              c6abc(jabaque) = (1._DOUBLE-rapport)*c6_inf + rapport*&
&               c6_sup
            END IF
          END IF
        END DO
! de jabaque
! Calcul de C6
! ------------
        c6ad = c6abcd(1)
        c6a = c6abc(1)
        c6bd = c6abcd(2)
        c6b = c6abc(2)
        c6cd = c6abcd(3)
        c6c = c6abc(3)
        c6d(iabaque) = (llpd-1.5_DOUBLE*(elargissementd*lp-elargissement&
&         *lpd)/lp**2)*(c6b-c6a)/.375_DOUBLE + (llp-0.25_DOUBLE-&
&         1.5_DOUBLE*(elargissement/lp))*(c6bd-c6ad)/.375_DOUBLE + &
&         4._DOUBLE*((elargissementd*lp-elargissement*lpd)*(c6c-c6a)/lp&
&         **2+elargissement*(c6cd-c6ad)/lp) + c6ad
        c6(iabaque) = (llp-0.25_DOUBLE-1.5_DOUBLE*(elargissement/lp))/&
&         .375_DOUBLE*(c6b-c6a) + 4._DOUBLE*(elargissement/lp)*(c6c-c6a)&
&         + c6a
! -----------------------------------
! Prise en compte de C6 dans PDCSing,
! tableau des Pertes de charge
! -----------------------------------
        DO iext=1,nb_ext
          IF (conf%nature(iext) .EQ. iabaque) THEN
            pcsingd(conf%section(iext)) = c6d(iabaque)
            pcsing(conf%section(iext)) = c6(iabaque)
          END IF
        END DO
      END DO
! de iabaque
! FIN DE LA BOUCLE SUR LES CONFLUENCES
! ------------------------------------
      IF (unitelisting .GT. 0) WRITE(unitelisting, 10030) inoeu, c6(1), &
&                              c6(2)
    END IF
  END DO label_noeud
  RETURN
 160 erreur%numero = 87
  erreur%ft = err_87
  erreur%ft_c = err_87c
  CALL TRAITER_ERREUR(erreur, nb_branche_amont, inoeu)
  RETURN
 170 RETURN
 180 RETURN
 190 erreur%numero = 88
  erreur%ft = err_88
  erreur%ft_c = err_88c
  CALL TRAITER_ERREUR(erreur, nb_branche_aval, inoeu)
  RETURN
 200 erreur%numero = 86
  erreur%ft = err_86
  erreur%ft_c = err_86c
  CALL TRAITER_ERREUR(erreur, inoeu, iext)
  RETURN
 210 RETURN
 220 erreur%numero = 86
  erreur%ft = err_86
  erreur%ft_c = err_86c
  CALL TRAITER_ERREUR(erreur, inoeu, iext)
  RETURN
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
END SUBROUTINE CALC_PC_CONFLU_D




SUBROUTINE QNODE_D(q, qd, z, zd, numconfluence, numpassage, connect, &
& erreur)
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
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: qd
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: z
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: zd
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
  DOUBLE PRECISION :: qamont
  DOUBLE PRECISION :: qamontd
  INTRINSIC SIZE
!character(132) :: !arbredappel_old
!============================= Instructions ===========================
  erreur%numero = 0
!arbredappel_old    = trim(!Erreur%arbredappel)
!Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>QNODE'
  IF (numpassage .EQ. 1) THEN
!=====================================
!  PREMIER PASSAGE :
! CALCUL DU DEBIT DANS LA BRANCHE AVAL
!=====================================
    qamont = 0._DOUBLE
    qamontd = 0.D0
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
! Il s'agit de la branche aval unique
        ii1 = num_sect
      ELSE
! c'est une branche amont :
! on somme les debits
        qamontd = qamontd + qd(num_sect)
        qamont = qamont + q(num_sect)
      END IF
    END DO
! debit aval = somme des debits amont
    qd(ii1) = qamontd
    q(ii1) = qamont
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
! -- BRANCHE AMONT --
        zd(num_sect) = zd(ii1)
        z(num_sect) = z(ii1)
      END IF
    END DO
  ELSE
    erreur%numero = 34
    erreur%ft = err_34
    erreur%ft_c = err_34c
    CALL TRAITER_ERREUR(erreur, numpassage)
    RETURN
  END IF
!Erreur%arbredappel = !arbredappel_old
  RETURN
END SUBROUTINE QNODE_D





SUBROUTINE QREPAR_D(sommedebitance, sommedebitanced, zaval, zavald, &
& numpassage, q, qd, z, zd, zref, x, cf1, cf1d, cf2, cf2d, idt, xdt, &
& profil, profil_plan, numconfluence, connect, modelelit, epsil, dzprev&
& , unitelisting, loifrottement, erreur)

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
  USE M_INTERPOLATION_S_D
! Sous-programme RHSBP_GENERIQUE_S
  USE M_RHSBP_S
  USE M_RHSBP_S_D
! Traitement de l'erreur
  USE M_TRAITER_ERREUR_I
! Interfaces
  USE M_REPAR_I_D
  IMPLICIT NONE
!.. Arguments ..
! --------------
! TABLEAUX DIMENSIONNES A NMPLAN
  DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: sommedebitance
  DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: sommedebitanced
  DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: zaval
  DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: zavald
  INTEGER, INTENT(INOUT) :: numpassage
! TABLEAU  DIMENSIONNE  A NbSect
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: q
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: qd
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: z
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: zd
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: zref
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: x
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf1
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf1d
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf2
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf2d
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
  DOUBLE PRECISION :: qddzd(SIZE(connect%originebief))
  DOUBLE PRECISION :: delz(SIZE(connect%originebief))
  DOUBLE PRECISION :: delzd(SIZE(connect%originebief))
  DOUBLE PRECISION :: b1, b2
  DOUBLE PRECISION :: b1d, b2d
  DOUBLE PRECISION :: beta
  DOUBLE PRECISION :: betad
  DOUBLE PRECISION :: bst
  DOUBLE PRECISION :: ddz
  DOUBLE PRECISION :: deb
  DOUBLE PRECISION :: debd
  DOUBLE PRECISION :: debm1
  DOUBLE PRECISION :: debm1d
  DOUBLE PRECISION :: debp1
  DOUBLE PRECISION :: debp1d
  DOUBLE PRECISION :: delq
  DOUBLE PRECISION :: delqd
  DOUBLE PRECISION :: derivd
  DOUBLE PRECISION :: derivdd
  DOUBLE PRECISION :: p1, p2
  DOUBLE PRECISION :: p1d, p2d
  DOUBLE PRECISION :: pente
  DOUBLE PRECISION :: qamont
  DOUBLE PRECISION :: qamontd
! Debitance
  DOUBLE PRECISION :: qampen
  DOUBLE PRECISION :: qampend
  DOUBLE PRECISION :: q2
  DOUBLE PRECISION :: q1
  DOUBLE PRECISION :: rh1, rh2
  DOUBLE PRECISION :: rh1d, rh2d
  DOUBLE PRECISION :: s1, s2
  DOUBLE PRECISION :: s1d, s2d
  DOUBLE PRECISION :: sdelq
  DOUBLE PRECISION :: sdelqd
  DOUBLE PRECISION :: sdelz
  DOUBLE PRECISION :: sq
  DOUBLE PRECISION :: sqd
  DOUBLE PRECISION :: sqddz
  DOUBLE PRECISION :: sqddzd
  DOUBLE PRECISION :: sqddzi
  DOUBLE PRECISION :: sqddzid
  DOUBLE PRECISION :: vmoy
  DOUBLE PRECISION :: vmoyd
  DOUBLE PRECISION :: z1
  DOUBLE PRECISION :: z1d
  DOUBLE PRECISION :: zf
  DOUBLE PRECISION :: zmax
  DOUBLE PRECISION :: zmaxi
  DOUBLE PRECISION :: znode
  DOUBLE PRECISION :: znoded
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
  DOUBLE PRECISION :: result1
  DOUBLE PRECISION :: abs0
!character(132) :: !arbredappel_old
!============================= Instructions ===========================
! INITIALISATIONS
! ---------------
  erreur%numero = 0
!arbredappel_old    = trim(!Erreur%arbredappel)
!Erreur%Arbredappel = trim(!Erreur%arbredappel)//'=>QREPAR'
! REPERAGE DES BRANCHES AMONT, DES BRANCHES AVAL, CALCUL DU DEBIT AMONT
! ---------------------------------------------------------------------
  javal = 0
  jamont = 0
  qamont = 0._DOUBLE
  qamontd = 0.D0
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
      javal = javal + 1
      i1av(javal) = connect%originebief(num_bief)
      i2av(javal) = connect%finbief(num_bief)
    ELSE
      jamont = jamont + 1
      i2am(jamont) = connect%finbief(num_bief)
      qamontd = qamontd + qd(i2am(jamont))
      qamont = qamont + q(i2am(jamont))
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
    dzprev(numconfluence) = 1000._DOUBLE
    i = i1av(1)
    zf = zref(i)
    result1 = REAL(profil(idt(i))%nbpas - 1, double)
    zmax = zref(i) + result1*profil(idt(i))%pas
    DO j=2,nbaval
      i = i1av(j)
      IF (zf .GT. zref(i)) THEN
        zf = zref(i)
      ELSE
        zf = zf
      END IF
      result1 = REAL(profil(idt(i))%nbpas - 1, double)
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
    result1 = REAL(nb_pas_profil_max - 1, double)
    ddz = (zmax-zf)/result1
    epsil = eps*ddz
! CALCUL DES TABLEAUX DE DEBITANCES
    pente = 0._DOUBLE
    DO k=1,nb_pas_profil_max
      sommedebitanced(k) = 0.D0
      sommedebitance(k) = 0._DOUBLE
    END DO
    debd = 0.D0
label_j:DO j=1,nbaval
      i = i1av(j)
label_k:DO k=1,nb_pas_profil_max
        result1 = REAL(k - 1, double)
        zavald(k) = 0.D0
        zaval(k) = zf + result1*ddz
        IF (k .NE. 1) THEN
          IF (zaval(k) .GT. zref(i) + epsil) THEN
            CALL RHSBP_S(b1, b2, bst, p1, p2, s1, s2, rh1, rh2, i, zaval&
&                  (k), zref(i), idt, xdt, profil, profil_plan, &
&                  unitelisting, erreur)
            IF (erreur%numero .NE. 0) THEN
              GOTO 140
            ELSE
! Resultats
! Donnees modifiees
! Donnees non modifiees
              p2d = 0.D0
              p1d = 0.D0
              rh2d = 0.D0
              rh1d = 0.D0
              s2d = 0.D0
              s1d = 0.D0
              betad = 0.D0
              vmoyd = 0.D0
              CALL REPAR_D(deb, debd, vmoy, vmoyd, beta, betad, q1, q2, &
&                    s1, s1d, s2, s2d, rh1, rh1d, rh2, rh2d, p1, p1d, p2&
&                    , p2d, q(i), qd(i), cf1(i), cf1d(i), cf2(i), cf2d(i&
&                    ), modelelit, loifrottement, erreur)
              IF (erreur%numero .NE. 0) GOTO 150
            END IF
          ELSE
            deb = 0._DOUBLE
            debd = 0.D0
          END IF
          sommedebitanced(k) = sommedebitanced(k) + debd
          sommedebitance(k) = sommedebitance(k) + deb
        END IF
      END DO label_k
! EVALUATION DE LA PENTE DE LA LIGNE DE CHARGE
      pente = pente + (zref(i1av(j))-zref(i2av(j)))/(x(i2av(j))-x(i1av(j&
&       )))
    END DO label_j
! INTERPOLATION SUR LE TABLEAU DES DEBITANCES
! EN VERIFIANT QUE LES DEBITANCES CROISSENT BIEN AVEC LES COTES
    pente = pente/nbaval
    IF (pente .LT. pentmi) pente = pentmi
    result1 = DSQRT(pente)
    qampend = qamontd/result1
    qampen = qamont/result1
    IF (qampen .GT. sommedebitance(nb_pas_profil_max)) THEN
      qampend = sommedebitanced(nb_pas_profil_max)
      qampen = sommedebitance(nb_pas_profil_max)
    END IF
    nb_pas_1 = nb_pas_profil_max
    k1 = 2
    DO WHILE (k1 .LT. nb_pas_1)
! Si la debitance n'est pas monotone => suppression d'un element du tableau
! et decalage
      DO WHILE (sommedebitance(k1) .LE. sommedebitance(k1-1) .AND. k1 &
&               .LT. nb_pas_1)
        DO k=k1,nb_pas_1-1
          zavald(k) = 0.D0
          zaval(k) = zaval(k+1)
          sommedebitanced(k) = sommedebitanced(k+1)
          sommedebitance(k) = sommedebitance(k+1)
        END DO
        nb_pas_1 = nb_pas_1 - 1
      END DO
      IF (sommedebitance(k1) .LE. sommedebitance(k1-1) .AND. k1 .GE. &
&         nb_pas_1) nb_pas_1 = nb_pas_1 - 1
      k1 = k1 + 1
    END DO
! Calcul d'une cote de depart ZNODE
    znoded = 0.D0
    CALL INTERPOLATION_S_D(znode, znoded, qampen, qampend, 1, &
&                    sommedebitance, sommedebitanced, zaval, nb_pas_1, &
&                    erreur)
    IF (erreur%numero .NE. 0) THEN
      RETURN
    ELSE
! CALCUL DES DEBITS DES BRANCHES
! Somme de debits
      sqd = qamontd
      sq = qamont
      p1d = 0.D0
      p2d = 0.D0
      DO j=1,nbaval
        i = i1av(j)
        IF (znode .GT. zref(i) + epsil) THEN
          CALL RHSBP_S_D(b1, b1d, b2, b2d, bst, p1, p1d, p2, p2d, s1, &
&                  s1d, s2, s2d, rh1, rh1d, rh2, rh2d, i, znode, znoded&
&                  , zref(i), idt, xdt, profil, profil_plan, &
&                  unitelisting, erreur)
          IF (erreur%numero .NE. 0) THEN
            GOTO 120
          ELSE
! Resultats
! Donnees modifiees
! Donnees non modifiees
            betad = 0.D0
            vmoyd = 0.D0
            CALL REPAR_D(deb, debd, vmoy, vmoyd, beta, betad, q1, q2, s1&
&                  , s1d, s2, s2d, rh1, rh1d, rh2, rh2d, p1, p1d, p2, &
&                  p2d, q(i), qd(i), cf1(i), cf1d(i), cf2(i), cf2d(i), &
&                  modelelit, loifrottement, erreur)
            IF (erreur%numero .NE. 0) GOTO 130
          END IF
        ELSE
! --  PAS D'ECOULEMENT DANS CE BIEF --
          deb = 0._DOUBLE
          debd = 0.D0
        END IF
        result1 = DSQRT(pente)
        qd(i) = result1*debd
        q(i) = deb*result1
! SQ = reste de debit
        sqd = sqd - qd(i)
        sq = sq - q(i)
      END DO
! CORRECTION POUR AVOIR EXACTEMENT LA CONTINUITE DES DEBITS
      DO j=1,nbaval
        i = i1av(j)
        qd(i) = qd(i) + sqd/nbaval
        q(i) = q(i) + sq/nbaval
      END DO
      GOTO 220
 120  RETURN
 130  RETURN
    END IF
 140 RETURN
 150 RETURN
  ELSE IF (numpassage .EQ. 2) THEN
! SECOND PASSAGE
! --------------
! CORRECTION DES DEBITS EN FONCTION DES ECARTS CONSTATES SUR LES COTES
    sdelz = 0._DOUBLE
    sdelq = 0._DOUBLE
    sqddz = 0._DOUBLE
    sqddzi = 0._DOUBLE
    znode = 0._DOUBLE
! TEST SUR L'EGALITE DES COTES
! ( NBAVQ EST LE NOMBRE DE BIEFS AVAL OU IL EXISTE UN ECOULEMENT )
    nbavq = nbaval
    znoded = 0.D0
    DO j=1,nbaval
      i = i1av(j)
      IF (z(i) .GT. zref(i) + epsil) THEN
        znoded = znoded + zd(i)
        znode = znode + z(i)
      ELSE
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
      RETURN
    ELSE
      result1 = REAL(nbavq, double)
      znoded = znoded/result1
      znode = znode/result1
      delzd = 0.D0
      DO j=1,nbaval
        i = i1av(j)
        IF (z(i) .GT. zref(i) + epsil) THEN
          delzd(j) = znoded - zd(i)
          delz(j) = znode - z(i)
          IF (delz(j) .GE. 0.) THEN
            abs0 = delz(j)
          ELSE
            abs0 = -delz(j)
          END IF
          sdelz = sdelz + abs0
        ELSE
          delzd(j) = 0.D0
          delz(j) = 0._DOUBLE
        END IF
      END DO
! SI ON A CONVERGE :
! .CORRECTION DES COTES AMONT
! .SORTIE AVEC NumPassage=999
      IF (sdelz .LT. epsil) THEN
        DO j=1,nbamon
          i = i2am(j)
          zd(i) = znoded
          z(i) = znode
        END DO
        numpassage = 999
!Erreur%arbredappel = !arbredappel_old
!------
        RETURN
!------
      ELSE IF (sdelz .GE. dzprev(numconfluence)) THEN
! STOP SI LE RESULTAT N'EST PAS AMELIORE
        erreur%numero = 48
        erreur%ft = err_48
        erreur%ft_c = err_48c
        CALL TRAITER_ERREUR(erreur, numconfluence)
        RETURN
      ELSE
        dzprev(numconfluence) = sdelz
        qddzd = 0.D0
        sqddzd = 0.D0
        p1d = 0.D0
        p2d = 0.D0
        sqddzid = 0.D0
        debd = 0.D0
        debp1d = 0.D0
        debm1d = 0.D0
        DO j=1,nbaval
          i = i1av(j)
! CALCUL DE LA DEBITANCE
          CALL RHSBP_S_D(b1, b1d, b2, b2d, bst, p1, p1d, p2, p2d, s1, &
&                  s1d, s2, s2d, rh1, rh1d, rh2, rh2d, i, z(i), zd(i), &
&                  zref(i), idt, xdt, profil, profil_plan, unitelisting&
&                  , erreur)
          IF (erreur%numero .NE. 0) THEN
            GOTO 160
          ELSE
! Resultats
! Donnees modifiees
! Donnees non modifiees
            betad = 0.D0
            vmoyd = 0.D0
            CALL REPAR_D(deb, debd, vmoy, vmoyd, beta, betad, q1, q2, s1&
&                  , s1d, s2, s2d, rh1, rh1d, rh2, rh2d, p1, p1d, p2, &
&                  p2d, q(i), qd(i), cf1(i), cf1d(i), cf2(i), cf2d(i), &
&                  modelelit, loifrottement, erreur)
            IF (erreur%numero .NE. 0) THEN
              GOTO 170
            ELSE
! CALCUL DE LA DERIVEE DE LA DEBITANCE PAR RAPPORT A Z
              z1d = zd(i) + delzd(j)
              z1 = z(i) + delz(j)
              CALL RHSBP_S_D(b1, b1d, b2, b2d, bst, p1, p1d, p2, p2d, s1&
&                      , s1d, s2, s2d, rh1, rh1d, rh2, rh2d, i, z1, z1d&
&                      , zref(i), idt, xdt, profil, profil_plan, &
&                      unitelisting, erreur)
              IF (erreur%numero .NE. 0) THEN
                GOTO 180
              ELSE
! Resultats
! Donnees modifiees
! Donnees non modifiees
                betad = 0.D0
                vmoyd = 0.D0
                CALL REPAR_D(debp1, debp1d, vmoy, vmoyd, beta, betad, q1&
&                      , q2, s1, s1d, s2, s2d, rh1, rh1d, rh2, rh2d, p1&
&                      , p1d, p2, p2d, q(i), qd(i), cf1(i), cf1d(i), cf2&
&                      (i), cf2d(i), modelelit, loifrottement, erreur)
                IF (erreur%numero .NE. 0) THEN
                  GOTO 190
                ELSE
                  z1d = zd(i) - delzd(j)
                  z1 = z(i) - delz(j)
                  CALL RHSBP_S_D(b1, b1d, b2, b2d, bst, p1, p1d, p2, p2d&
&                          , s1, s1d, s2, s2d, rh1, rh1d, rh2, rh2d, i, &
&                          z1, z1d, zref(i), idt, xdt, profil, &
&                          profil_plan, unitelisting, erreur)
                  IF (erreur%numero .NE. 0) THEN
                    GOTO 200
                  ELSE
! Resultats
! Donnees modifiees
! Donnees non modifiees
                    betad = 0.D0
                    vmoyd = 0.D0
                    CALL REPAR_D(debm1, debm1d, vmoy, vmoyd, beta, betad&
&                          , q1, q2, s1, s1d, s2, s2d, rh1, rh1d, rh2, &
&                          rh2d, p1, p1d, p2, p2d, q(i), qd(i), cf1(i), &
&                          cf1d(i), cf2(i), cf2d(i), modelelit, &
&                          loifrottement, erreur)
                    IF (erreur%numero .NE. 0) THEN
                      GOTO 210
                    ELSE
                      derivdd = ((debp1d-debm1d)*delz(j)/2._DOUBLE-(&
&                       debp1-debm1)*delzd(j)/2._DOUBLE)/delz(j)**2
                      derivd = (debp1-debm1)/2._DOUBLE/delz(j)
                      qddzd(j) = ((qd(i)*derivd+q(i)*derivdd)*deb-q(i)*&
&                       derivd*debd)/deb**2
                      qddz(j) = q(i)*derivd/deb
                      sqddzd = sqddzd + qddzd(j)
                      sqddz = sqddz + qddz(j)
                      sqddzid = sqddzid + qddzd(j)*z(i) + qddz(j)*zd(i)
                      sqddzi = sqddzi + qddz(j)*z(i)
                    END IF
                  END IF
                END IF
              END IF
            END IF
          END IF
        END DO
! CALCUL DE LA NOUVELLE COTE A ATTEINDRE, PUIS CORRECTION DES DEBITS
        znoded = (sqddzid*sqddz-sqddzi*sqddzd)/sqddz**2
        znode = sqddzi/sqddz
        sdelqd = 0.D0
        DO j=1,nbaval
          i = i1av(j)
          delzd(j) = znoded - zd(i)
          delz(j) = znode - z(i)
          delqd = qddzd(j)*delz(j) + qddz(j)*delzd(j)
          delq = qddz(j)*delz(j)
          sdelqd = sdelqd + delqd
          sdelq = sdelq + delq
        END DO
        DO j=1,nbaval
          i = i1av(j)
          delqd = qddzd(j)*delz(j) + qddz(j)*delzd(j) - sdelqd/nbaval
          delq = qddz(j)*delz(j) - sdelq/nbaval
          qd(i) = qd(i) + delqd
          q(i) = q(i) + delq
        END DO
        GOTO 220
 160    RETURN
 170    RETURN
 180    RETURN
 190    RETURN
 200    RETURN
 210    RETURN
      END IF
    END IF
  ELSE
! ERREUR DANS L'ALGORITHME
! ------------------------
    erreur%numero = 49
    erreur%ft = err_49
    erreur%ft_c = err_49c
    CALL TRAITER_ERREUR(erreur, numconfluence, numpassage)
    RETURN
  END IF
!Erreur%arbredappel = !arbredappel_old
 220 RETURN
END SUBROUTINE QREPAR_D





SUBROUTINE PERSAR_D(z, zd, q, qd, x, zref, cf1, cf1d, cf2, cf2d, pcsing&
& , pcsingd, idt, xdt, profil, profilplan, f1, qinjec, connect, &
& singularite, extremite, modelelit, confluent, abaque, &
& impression, unitelisting, temps, algorithme, loifrottement, &
& pertechargeconfluent, cqmv, decentrement, erreur)
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
  USE M_CALC_PC_CONFLU_I_D
  USE M_PERMAT_I_D
  USE M_QNODE_I_D
  USE M_QREPAR_I_D
! Traitement de l'erreur
  USE M_TRAITER_ERREUR_I
  IMPLICIT NONE
!.. Arguments ..
! --------------
  DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: z
  DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: zd
  DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: q
  DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: qd
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: x
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: zref
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf1
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf1d
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf2
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf2d
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: pcsing
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: pcsingd
  INTEGER, DIMENSION(:), INTENT(IN) :: idt
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: xdt
  TYPE(PROFIL_T), DIMENSION(:), INTENT(IN) :: profil
  TYPE(PROFIL_PLAN_T), INTENT(IN) :: profilplan
  DOUBLE PRECISION, DIMENSION(:, :), INTENT(IN) :: f1
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: qinjec
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
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: sommedebitanced
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: zaval
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: zavald
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
  DOUBLE PRECISION :: zinitd
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
!============================= Instructions ===========================
! INITIALISATIONS
!----------------
  erreur%numero = 0
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
  ALLOCATE(sommedebitanced(nb_pas_profil_max), stat=retour)
  ALLOCATE(sommedebitance(nb_pas_profil_max), stat=retour)
  IF (retour .NE. 0) THEN
    erreur%numero = 5
    erreur%ft = err_5
    erreur%ft_c = err_5c
    CALL TRAITER_ERREUR(erreur, 'SommeDebitance')
    zd = 0.D0
    RETURN
  ELSE
    ALLOCATE(zavald(nb_pas_profil_max), stat=retour)
    ALLOCATE(zaval(nb_pas_profil_max), stat=retour)
    IF (retour .NE. 0) THEN
      erreur%numero = 5
      erreur%ft = err_5
      erreur%ft_c = err_5c
      CALL TRAITER_ERREUR(erreur, 'Zaval')
      zd = 0.D0
      RETURN
    ELSE
!------------------------------------------
!    RESOLUTION DU CALCUL DE LA LIGNE D'EAU
!    DANS L'ORDRE DU TABLEAU Algorithme
!------------------------------------------
      ia = 0
      icompt = 0
      sommedebitanced(1:nb_pas_profil_max) = 0.D0
      qd = 0.D0
      zd = 0.D0
      pcsingd = 0.D0
label_ia:DO WHILE (ia .LT. nappel)
        ia = ia + 1
        icompt = icompt + 1
        isub = INT(algorithme(ia)/100)
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
            qd(connect%originebief(noeud_bief)) = 0.D0
            q(connect%originebief(noeud_bief)) = extremite(iext)%ptq(1)
! else        --  ON PART D'UN NOEUD --
          END IF
          DO isec=connect%originebief(noeud_bief)+1,connect%finbief(&
&             noeud_bief)
            qd(isec) = qd(isec-1)
            q(isec) = q(isec-1) + qinjec(isec)
          END DO
        CASE (appel_permat)
! Calcul des pertes de charge automatique aux confluences
          IF (pertechargeconfluent) THEN
            CALL CALC_PC_CONFLU_D(pcsing, pcsingd, z, zd, q, qd, x, zref&
&                           , profil, profilplan, confluent, abaque, idt&
&                           , xdt, connect, unitelisting, erreur)
            IF (erreur%numero .NE. 0) GOTO 200
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
            zinit = extremite(iext)%ptz(1)
            zinitd = 0.D0
          ELSE
!             --  ON PART D'UN NOEUD --
            zinitd = zd(connect%finbief(noeud_bief))
            zinit = z(connect%finbief(noeud_bief))
          END IF
!/RESULTATS/
!/DONNEES NON MODIFIEES/
!/DONNEES NON MODIFIEES
! (ARGUMENTS DE S.P) /
          CALL PERMAT_D(z, zd, q, qd, zinit, zinitd, x, zref, cf1, cf1d&
&                 , cf2, cf2d, pcsing, pcsingd, idt, xdt, profil, &
&                 profilplan, f1, connect, noeud_bief, nbsect, &
&                 singularite, modelelit, impression, &
&                 unitelisting, temps, loifrottement, cqmv, decentrement, &
&                 erreur)
!Erreur
          IF (erreur%numero .NE. 0) GOTO 210
        CASE (appel_qnode_1er_pass)
          numpassage = 1
!/DONNEES MODIFIEES/
!/DONNEES NON MODIFIEES/
          CALL QNODE_D(q, qd, z, zd, noeud_bief, numpassage, connect, &
&                erreur)
          IF (erreur%numero .NE. 0) GOTO 190
        CASE (appel_qnode_2nd_pass)
          numpassage = 2
!/DONNEES MODIFIEES/
!/DONNEES NON MODIFIEES/
          CALL QNODE_D(q, qd, z, zd, noeud_bief, numpassage, connect, &
&                erreur)
          IF (erreur%numero .NE. 0) GOTO 180
        CASE (appel_qrepar_1er_pass)
          numpassage = 1
!/RESULTATS/
!/DONNEES MODIFIEES/
!/DONNEES NON MODIFIEES/
          CALL QREPAR_D(sommedebitance, sommedebitanced, zaval, zavald, &
&                 numpassage, q, qd, z, zd, zref, x, cf1, cf1d, cf2, &
&                 cf2d, idt, xdt, profil, profilplan, noeud_bief, &
&                 connect, modelelit, epsil, dzprev, unitelisting, &
&                 loifrottement, erreur)
          IF (erreur%numero .NE. 0) GOTO 170
        CASE (appel_qrepar_2nd_pass)
          numpassage = 2
!/RESULTATS/
!/DONNEES MODIFIEES/
!/DONNEES NON MODIFIEES/
! Unite logisue du fichier listing
          CALL QREPAR_D(sommedebitance, sommedebitanced, zaval, zavald, &
&                 numpassage, q, qd, z, zd, zref, x, cf1, cf1d, cf2, &
&                 cf2d, idt, xdt, profil, profilplan, noeud_bief, &
&                 connect, modelelit, epsil, dzprev, unitelisting, &
&                 loifrottement, erreur)
          IF (erreur%numero .NE. 0) THEN
            GOTO 150
          ELSE IF (numpassage .NE. 999 .AND. icompt .LT. npass) THEN
!  TEST DU RETOUR DE QREPAR :
! . SI NumPassage=999 ON A CONVERGE , LA REPARTITION DE DEBIT EST BONNE
! . SINON IL FAUT ITERER , ON FAIT UN RETOUR AU PREMIER APPEL A QREPAR
!   LE NOMBRE D'ITERATIONS ETANT LIMITE PAR NPASS
            iap = nappel - ia + 1
            ia = iap
          ELSE IF (icompt .GE. npass) THEN
            GOTO 160
          END IF
        CASE DEFAULT
          GOTO 140
        END SELECT
      END DO label_ia
!  Desallocation des tableaux locaux
!  ---------------------------------
      DEALLOCATE(sommedebitanced)
      DEALLOCATE(sommedebitance, stat=retour)
      IF (retour .NE. 0) THEN
        erreur%numero = 6
        erreur%ft = err_6
        erreur%ft_c = err_6c
        CALL TRAITER_ERREUR(erreur, 'SommeDebitance')
        RETURN
      ELSE
        DEALLOCATE(zavald)
        DEALLOCATE(zaval, stat=retour)
        IF (retour .NE. 0) THEN
          erreur%numero = 6
          erreur%ft = err_6
          erreur%ft_c = err_6c
          CALL TRAITER_ERREUR(erreur, 'Zaval')
          RETURN
        ELSE
!Erreur%arbredappel = !arbredappel_old
          RETURN
        END IF
      END IF
 140  erreur%numero = 33
      erreur%ft = err_33
      erreur%ft_c = err_33c
      CALL TRAITER_ERREUR(erreur, ia)
      RETURN
 150  RETURN
 160  erreur%numero = 32
      erreur%ft = err_32
      erreur%ft_c = err_32c
      CALL TRAITER_ERREUR(erreur, noeud_bief)
      RETURN
 170  RETURN
 180  RETURN
 190  RETURN
 200  RETURN
 210  RETURN
    END IF
  END IF
END SUBROUTINE PERSAR_D





SUBROUTINE SARAP_D(z, zd, q1, q2, p1, p1d, p2, p2d, b1, b1d, b2, b2d, bs&
& , rh1, rh1d, rh2, rh2d, s1, s1d, s2, s2d, beta, betad, froude, &
& extremite, apport, qinjec, qdeverse, temps, profil, profilplan, f1, x&
& , cf1, cf1d, cf2, cf2d, zref, xdt, idt, connect, singularite, pcsing, &
& pcsingd, deversoir, modelelit, confluent, abaque, algorithme, impression,&
& unitelisting, loifrottement, pertechargeconfluent, cqmv, decentrement, &
& erreur)
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
  USE M_PERSAR_I_D
  USE M_REPAR_I
  IMPLICIT NONE
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: z, q1, q2
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: zd
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: p1, p2
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: p1d, p2d
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: b1, b2, bs
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: b1d, b2d
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: rh1, rh2
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: rh1d, rh2d
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: s1, s2
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: s1d, s2d
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: beta
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: betad
  DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: froude
! Conditions aux limites des extremites libres
  TYPE(EXTREMITE_T), DIMENSION(:), INTENT(IN) :: extremite
  TYPE(ERREUR_T), INTENT(INOUT) :: erreur
! Maillage
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: x, cf1, cf2
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf1d, cf2d
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
! Pertes de charge singulieres
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: pcsing
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: pcsingd
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
  DOUBLE PRECISION :: hi, vi
!.. Local Arrays ..
  DOUBLE PRECISION, DIMENSION(SIZE(x)) :: q
  DOUBLE PRECISION, DIMENSION(SIZE(x)) :: qd
  INTRINSIC SIZE
!============================= Instructions ===========================
! INITIALISATIONS ET ALLOCATIONS
! ------------------------------
  erreur%numero = 0
!arbredappel_old    = trim(!Erreur%arbredappel)
!Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>SARAP'
! REMPLISSAGE DU TABLEAU DES APPORTS
! ----------------------------------
  IF (SIZE(apport) .GT. 0) THEN
    DO iinj=1,SIZE(apport)
! Calcul de QInjec, tableau dimensionne a NbSect
! representant les apports
! ----------------------------------------------
      CALL CQINJ(qinjec, x, z, apport, deversoir, qdeverse, erreur)
      IF (erreur%numero .NE. 0) GOTO 100
    END DO
    GOTO 110
 100 zd = 0.D0
    RETURN
  ELSE
    qinjec(:) = 0._DOUBLE
  END IF
! CALCUL DE BASE DE LA LIGNE D'EAU
! --------------------------------
!/RESULTATS/
!/DONNEES NON MODIFIEES/
! Caracteristiques des confluences
! Abaques des pertes de  charges aux confluences
 110 CALL PERSAR_D(z, zd, q, qd, x, zref, cf1, cf1d, cf2, cf2d, pcsing, &
&            pcsingd, idt, xdt, profil, profilplan, f1, qinjec, connect&
&            , singularite, extremite, modelelit, &
&            confluent, abaque, impression, unitelisting, temps, &
&            algorithme, loifrottement, pertechargeconfluent, cqmv, &
&            decentrement, erreur)
  IF (erreur%numero .NE. 0) THEN
    RETURN
  ELSE
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
        IF (erreur%numero .NE. 0) THEN
          GOTO 140
        ELSE
! CALCUL ET CONTROLE DU NOMBRE DE FROUDE
!---------------------------------------
          vi = q(isec)/(s1(isec)+s2(isec))
          IF (b1(isec) .NE. 0._DOUBLE) THEN
            hi = (s1(isec)+s2(isec))/(b1(isec)+b2(isec))
          ELSE
            hi = eps3
          END IF
          CALL FROUDE_S(froude(isec), beta(isec), vi, hi, isec, connect&
&                 , x, erreur)
        END IF
      END IF
    END DO
! VERIFICATION DE LA COTE EN TOUT POINT
! -------------------------------------
    DO isec=1,SIZE(x)
      IF (z(isec) - zref(isec) .LT. w0) GOTO 120
    END DO
! FIN DU CALCUL
! -------------
!Erreur%arbredappel = !arbredappel_old
    RETURN
 120 erreur%numero = 31
    erreur%ft = err_31
    erreur%ft_c = err_31c
    num_bief = NUM_BIEF_S(connect, isec, erreur)
    absc_rel = x(isec) - x(connect%originebief(num_bief))
    CALL TRAITER_ERREUR(erreur, isec, num_bief, absc_rel)
    RETURN
 130 RETURN
 140 RETURN
  END IF
END SUBROUTINE SARAP_D




SUBROUTINE DIFF_Z_CF12_FWD_SARAP(diff_z_fwd, dcf1_fwd, dcf2_fwd)

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
  USE M_SARAP_I_D
  USE M_SHARE_VAR
  IMPLICIT NONE

  DOUBLE PRECISION, DIMENSION(size(x)) :: diff_z_fwd
  DOUBLE PRECISION, DIMENSION(size(x)) :: dcf1_fwd
  DOUBLE PRECISION, DIMENSION(size(x)) :: dcf2_fwd


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

  CALL SARAP_D(z, diff_z_fwd, q1, q2, p1, diff_p1, p2, &
&              diff_p2, b1, diff_b1, b2, diff_b2, bs, rh1, &
&              diff_rh1, rh2, diff_rh2, s1, diff_s1, s2, diff_s2&
&              , beta, diff_beta, froude, extremite&
&              , apport, qinjec, qdeverse, temps&
&              , profil, profilplan, f1, x&
&              , cf1, dcf1_fwd, cf2, dcf2_fwd, zref, xdt&
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

END SUBROUTINE DIFF_Z_CF12_FWD_SARAP
