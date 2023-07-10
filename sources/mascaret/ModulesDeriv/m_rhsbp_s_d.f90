MODULE M_RHSBP_S_D
  IMPLICIT NONE

CONTAINS
  SUBROUTINE RHSBP_S_D(b1, b1d, b2, b2d, bs, p1, p1d, p2, p2d, s1, s1d, &
&   s2, s2d, r1, r1d, r2, r2d, section, z, zd, zref, idt, xdt, profil, &
&   prof, unitelisting, erreur)
!***********************************************************************
! PROGICIEL : MASCARET      A. LEBOSSE
!                             P. CHERUBINI
!                             S. PERON
!                             S. MANDELKERN
!
! VERSION : V8P4R0             EDF-CEREMA
!***********************************************************************
!   FONCTION :
!   --------
!
!   CALCUL DU RAYON HYDAULIQUE, DE LA SURFACE MOUILLEE, DE LA LARGEUR
!   AU MIROIR ET DU PERIMETRE MOUILLE DANS UNE SECTION DE
!   DE CALCUL (LIT MINEUR, ,LIT MAJEUR)
!
! ----------------------------------------------------------------------
! ARGUMENTS
! .________________.____._______________________________________________
! !    NOM    !TYPE!MODE!                   ROLE
! !___________!____!____!______________________________________________
! ! B1,B2,BS  ! R  !<-- ! LARGEUR AU MIROIR   )
! ! P1,P2     ! R  !<-- ! PERIMETRE MOUILLE   )
! ! S1,S2     ! R  !<-- ! SECTION MOUILLEE    ) INDICE 1 LIT MINEUR
! ! R1,R2     ! R  !<-- ! RAYON HYDRAULIQUE   )        2 LIT MAJEUR
! ! Section   ! I  ! -->! SECTION DE CALCUL            S STOCKAGE
! ! Z         ! R  ! -->! COTE D'EAU DANS LA SECTION Section
! ! ZREF      ! R  ! -->! COTE DU FOND
! ! IDT       ! I  ! -->!  )PERMET DE DETERMINER LA SECTION DE CALCUL A
! ! XDT       ! R  ! -->!  )PARTIR DES SECTIONS DE DONNEES
! !DB1,DB2,DBS! T  ! -->! LARGEUR AU MIROIR  )  PLANIMETRAGE
! !DP1,DP2    ! T  ! -->! PERIMETRE MOUILLE  )  1= MINEUR   2= MAJEUR
! !DS1,DS2    ! T  ! -->! SECTION MOUILEE    )  S= STOCKAGE
! !___________!____!____!______________________________________________
!
!  VARIABLES LOCALES
! .___________.________________________________________________________
! !   FB1,FB2 ! R  ! -- !
! !   FP1,FP2 ! R  ! -- !
! !   FS1,FS2 ! R  ! -- !
! !   FS2G    ! R  ! -- !
! !   PAS     ! R  ! -->! PAS EN HAUTEUR DU PLANIMETRAGE D'UN PROFIL
! !   NBPAS   ! R  ! -->! NOMBRE DE PAS DE PLANIMETRAGE
! !___________!____!____!______________________________________________
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
! ----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :    - UniteListing : IMPRESSION DES RESULTATS GLOBAUX
!   ----------------------
!   SOUS PROGRAMMES APPELANTS :  REZO, CRITIQ, PERMAT, QREPAR, SARAP
!   ---------------------------
!   SOUS PROGRAMMES APPELES :    ---
!   -------------------------
!============================ Declarations ==============================
    USE M_PRECISION
    USE M_MESSAGE_C
    USE M_PARAMETRE_C
    USE M_PROFIL_T
    USE M_PROFIL_PLAN_T
    USE M_FICHIER_T
    USE M_ERREUR_T
! Traitement de l'erreur
    USE M_TRAITER_ERREUR_I
    IMPLICIT NONE
!.. Formal Arguments ..
    DOUBLE PRECISION, INTENT(OUT) :: b1, b2, bs, p1, p2, s1, s2, r1, r2
    DOUBLE PRECISION, INTENT(OUT) :: b1d, b2d, p1d, p2d, s1d, s2d, r1d, &
&   r2d
    TYPE(PROFIL_T), DIMENSION(:), INTENT(IN) :: profil
    TYPE(PROFIL_PLAN_T), INTENT(IN) :: prof
    INTEGER, INTENT(IN) :: section
    DOUBLE PRECISION, INTENT(INOUT) :: z
    DOUBLE PRECISION, INTENT(INOUT) :: zd
    DOUBLE PRECISION, INTENT(IN) :: zref
    DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: xdt
    INTEGER, DIMENSION(:), INTENT(IN) :: idt
    INTEGER, INTENT(IN) :: unitelisting
    TYPE(ERREUR_T), INTENT(INOUT) :: erreur
!.. Local Scalars ..
    INTEGER :: i, ip1, j, k
    DOUBLE PRECISION :: pas, xd, y, yd
    DOUBLE PRECISION :: yd0, ydd
    INTEGER :: nbpas
!character(132) :: !arbredappel_old
! Les Constantes sont declares dans le module M_PARAMETRES_C
!.. Local Arrays ..
    DOUBLE PRECISION, DIMENSION(2) :: fb1, fb2, fbs, fp1, fp2, fs1, fs2
    DOUBLE PRECISION, DIMENSION(2) :: fb1d, fb2d, fp1d, fp2d, fs1d, fs2d
!.. Intrinsic Functions ..
    INTRINSIC INT
!============================= Instructions =============================
! INITIALISATIONS
! ---------------
    erreur%numero = 0
!arbredappel_old = trim(!Erreur%arbredappel)
!Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>RHSBP'
    s1 = w0
    s2 = w0
    b1 = w0
    b2 = w0
    bs = w0
    r1 = w0
    r2 = w0
    fs1 = w0
    yd0 = zd
    y = z - zref
! Cas des sections a sec
    IF (y .LE. eps3) THEN
      erreur%numero = 1
      erreur%ft = err_1
      erreur%ft_c = err_1c
      CALL TRAITER_ERREUR(erreur, section)
      s1d = 0.D0
      s2d = 0.D0
      r1d = 0.D0
      r2d = 0.D0
      b1d = 0.D0
      b2d = 0.D0
      RETURN
    ELSE
      xd = xdt(section)
      i = idt(section)
      ip1 = i + 1
      IF (xd .LE. eps6) ip1 = i
      pas = profil(i)%pas + (profil(ip1)%pas-profil(i)%pas)*xd
      nbpas = profil(i)%nbpas
! TIRANT D'EAU CORRECT (POSITIF)
! ------------------------------
      k = INT(y/pas) + 1
      IF (k .GE. nbpas) THEN
        k = nbpas - 1
        IF (unitelisting .GT. 0) WRITE(unitelisting, 10000) section, y, &
&                                nbpas, pas
      END IF
      ydd = yd0
      yd = y - (k-1)*pas
      DO j=1,2
        fb1d(j) = 0.D0
        fb1(j) = prof%b1(i, k) + (prof%b1(ip1, k)-prof%b1(i, k))*xd
        fb2d(j) = 0.D0
        fb2(j) = prof%b2(i, k) + (prof%b2(ip1, k)-prof%b2(i, k))*xd
        fp1d(j) = 0.D0
        fp1(j) = prof%p1(i, k) + (prof%p1(ip1, k)-prof%p1(i, k))*xd
        fp2d(j) = 0.D0
        fp2(j) = prof%p2(i, k) + (prof%p2(ip1, k)-prof%p2(i, k))*xd
        fs1d(j) = 0.D0
        fs1(j) = prof%s1(i, k) + (prof%s1(ip1, k)-prof%s1(i, k))*xd
        fs2d(j) = 0.D0
        fs2(j) = prof%s2(i, k) + (prof%s2(ip1, k)-prof%s2(i, k))*xd
        fbs(j) = prof%bs(i, k) + (prof%bs(ip1, k)-prof%bs(i, k))*xd
        k = k + 1
      END DO
      b1d = (fb1(2)-fb1(1))*ydd/pas
      b1 = fb1(1) + (fb1(2)-fb1(1))*yd/pas
      b2d = (fb2(2)-fb2(1))*ydd/pas
      b2 = fb2(1) + (fb2(2)-fb2(1))*yd/pas
      p1d = (fp1(2)-fp1(1))*ydd/pas
      p1 = fp1(1) + (fp1(2)-fp1(1))*yd/pas
      p2d = (fp2(2)-fp2(1))*ydd/pas
      p2 = fp2(1) + (fp2(2)-fp2(1))*yd/pas
      s1d = (fs1(2)-fs1(1))*ydd/pas
      s1 = fs1(1) + (fs1(2)-fs1(1))*yd/pas
      s2d = (fs2(2)-fs2(1))*ydd/pas
      s2 = fs2(1) + (fs2(2)-fs2(1))*yd/pas
      bs = fbs(1) + (fbs(2)-fbs(1))*yd/pas
      r1d = (s1d*p1-s1*p1d)/p1**2
      r1 = s1/p1
      IF (bs .LT. eps3) bs = 0._DOUBLE
      IF (p2 .GT. eps3) THEN
        r2d = (s2d*p2-s2*p2d)/p2**2
        r2 = s2/p2
      ELSE
        r2 = 0._DOUBLE
        s2 = 0._DOUBLE
        p2 = 0._DOUBLE
        b2 = 0._DOUBLE
        s2d = 0.D0
        p2d = 0.D0
        r2d = 0.D0
        b2d = 0.D0
      END IF
!Erreur%arbredappel = !arbredappel_old
      RETURN
    END IF
! ... Format Declarations ...
10000 FORMAT( '<< ATTENTION >>', 'Dans la section de calcul n0 ',i4,/, &
&          'Tirant d''eau = ',g15.7,' depassant la hauteur du profil'&
&          ,/, 'Augmenter le nombre de pas de planimetrage : ',i4,&
&          ' ou le pas : ',f7.2)
  END SUBROUTINE RHSBP_S_D

  SUBROUTINE RHSBP_SECTION_S_D(varsect, varsectd, zref, z, zd, idt, xdt&
&   , profil, varprof, erreur)
!***********************************************************************
! PROGICIEL : MASCARET      S. MANDELKERN
!
! VERSION : V8P4R0             EDF-CEREMA
!***********************************************************************
!
!  FONCTION :
!  --------
!         CALCUL D'<<UNE>> VARIABLE INTERPOLEE SUR LES PROFILS DE DONNEES
!         A <<UNE>> SECTION DE CALCUL
!
!  SOUS-PROGRAMME(S) APPELANT(S) : - PSING
!  -----------------------------
!  SOUS-PROGRAMME(S) APPELE(S)   : ---
!  ---------------------------
!***********************************************************************
!============================= Declarations ===========================
!.. Modules importes ..
    USE M_PRECISION
! Definition du type PROFIL
    USE M_PROFIL_T
! Definition de la structure ERREUR_T
    USE M_ERREUR_T
! Traitement de l'erreur
    USE M_TRAITER_ERREUR_I
! EPS3, EPS6
    USE M_PARAMETRE_C
    IMPLICIT NONE
!.. Arguments ..
    DOUBLE PRECISION, INTENT(OUT) :: varsect
    DOUBLE PRECISION, INTENT(OUT) :: varsectd
    DOUBLE PRECISION, DIMENSION(:, :), INTENT(IN) :: varprof
    DOUBLE PRECISION, INTENT(IN) :: zref
    DOUBLE PRECISION, INTENT(IN) :: z
    DOUBLE PRECISION, INTENT(IN) :: zd
    INTEGER, INTENT(IN) :: idt
    DOUBLE PRECISION, INTENT(IN) :: xdt
    TYPE(PROFIL_T), DIMENSION(:), INTENT(IN) :: profil
    TYPE(ERREUR_T), INTENT(INOUT) :: erreur
!.. Variables locales ..
! Pas de planimetrage
    DOUBLE PRECISION :: pas
! Hauteur d'eau a la section
    DOUBLE PRECISION :: yd
    DOUBLE PRECISION :: ydd
! Hauteur d'eau a la section
    DOUBLE PRECISION :: y_loc
    DOUBLE PRECISION :: y_locd
! Fonction locale d'interpolation
    DOUBLE PRECISION, DIMENSION(2) :: fs
    DOUBLE PRECISION, DIMENSION(2) :: fsd
! Indice
    INTEGER :: kdt
! indice du profil aval de la section
    INTEGER :: ip1t
! Nombre de pas de planimetrage
    INTEGER :: nbpas
    INTRINSIC INT
!character(132)             :: !arbredappel_old
!============================ Instructions =================================
    erreur%numero = 0
!arbredappel_old = trim(!Erreur%arbredappel)
!Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>RHSBP_SECTION_S'
    y_locd = zd
    y_loc = z - zref
!------------------------
! Cas d'une section a sec
!------------------------
    IF (y_loc .LE. eps3) THEN
      y_loc = eps3
      y_locd = 0.D0
    END IF
!---------------------------------------
! Si on est tres proche du profil amont,
! pas d'interpolation amont-aval
!---------------------------------------
    IF (xdt .LE. eps6) THEN
      ip1t = idt
    ELSE
      ip1t = idt + 1
    END IF
!-----------------------------------------------
! Calcul du pas et du nombre de pas a la section
!-----------------------------------------------
    pas = profil(idt)%pas + (profil(ip1t)%pas-profil(idt)%pas)*xdt
    nbpas = profil(idt)%nbpas
! TIRANT D'EAU CORRECT (POSITIF)
    kdt = INT(y_loc/pas) + 1
    IF (kdt .GE. nbpas) kdt = nbpas - 1
    ydd = y_locd
    yd = y_loc - (kdt-1)*pas
    IF (y_loc .GE. eps6) THEN
      fsd(1) = 0.D0
      fs(1) = varprof(idt, kdt) + (varprof(ip1t, kdt)-varprof(idt, kdt))&
&       *xdt
      fsd(2) = 0.D0
      fs(2) = varprof(idt, kdt+1) + (varprof(ip1t, kdt+1)-varprof(idt, &
&       kdt+1))*xdt
      varsectd = (fs(2)-fs(1))*ydd/pas
      varsect = fs(1) + (fs(2)-fs(1))*yd/pas
    ELSE
      varsect = 0._DOUBLE
      varsectd = 0.D0
    END IF
!Erreur%arbredappel = !arbredappel_old
    RETURN
  END SUBROUTINE RHSBP_SECTION_S_D
END MODULE M_RHSBP_S_D

