MODULE M_RHSBP_S_B
  IMPLICIT NONE

CONTAINS

  SUBROUTINE RHSBP_S_B(b1, b1b, b2, b2b, bs, p1, p1b, p2, p2b, s1, s1b, &
&   s2, s2b, r1, r1b, r2, r2b, section, z, zb, zref, idt, xdt, profil, &
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
    DOUBLE PRECISION :: b1, b2, bs, p1, p2, s1, s2, r1, r2
    DOUBLE PRECISION :: b1b, b2b, p1b, p2b, s1b, s2b, r1b, r2b
    TYPE(PROFIL_T), DIMENSION(:), INTENT(IN) :: profil
    TYPE(PROFIL_PLAN_T), INTENT(IN) :: prof
    INTEGER, INTENT(IN) :: section
    DOUBLE PRECISION, INTENT(INOUT) :: z
    DOUBLE PRECISION :: zb
    DOUBLE PRECISION, INTENT(IN) :: zref
    DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: xdt
    INTEGER, DIMENSION(:), INTENT(IN) :: idt
    INTEGER, INTENT(IN) :: unitelisting
    TYPE(ERREUR_T), INTENT(INOUT) :: erreur
!.. Local Scalars ..
    INTEGER :: i, ip1, j, k
    DOUBLE PRECISION :: pas, xd, y, yd
    DOUBLE PRECISION :: yb, ydb
    INTEGER :: nbpas
!character(132) :: !arbredappel_old
! Les Constantes sont declares dans le module M_PARAMETRES_C
!.. Local Arrays ..
    DOUBLE PRECISION, DIMENSION(2) :: fb1, fb2, fp1, fp2, fs1, fs2
!.. Intrinsic Functions ..
    INTRINSIC INT
!============================= Instructions =============================
! INITIALISATIONS
! ---------------
!arbredappel_old = trim(!Erreur%arbredappel)
!Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>RHSBP'
    y = z - zref
! Cas des sections a sec
    IF (y .LE. eps3) THEN
      yb = 0.D0
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
      IF (k .GE. nbpas) k = nbpas - 1
      yd = y - (k-1)*pas
      DO j=1,2
        fb1(j) = prof%b1(i, k) + (prof%b1(ip1, k)-prof%b1(i, k))*xd
        fb2(j) = prof%b2(i, k) + (prof%b2(ip1, k)-prof%b2(i, k))*xd
        fp1(j) = prof%p1(i, k) + (prof%p1(ip1, k)-prof%p1(i, k))*xd
        fp2(j) = prof%p2(i, k) + (prof%p2(ip1, k)-prof%p2(i, k))*xd
        fs1(j) = prof%s1(i, k) + (prof%s1(ip1, k)-prof%s1(i, k))*xd
        fs2(j) = prof%s2(i, k) + (prof%s2(ip1, k)-prof%s2(i, k))*xd
        k = k + 1
      END DO
      CALL PUSHREAL8(p1)
      p1 = fp1(1) + (fp1(2)-fp1(1))*yd/pas
      CALL PUSHREAL8(p2)
      p2 = fp2(1) + (fp2(2)-fp2(1))*yd/pas
      CALL PUSHREAL8(s1)
      s1 = fs1(1) + (fs1(2)-fs1(1))*yd/pas
      CALL PUSHREAL8(s2)
      s2 = fs2(1) + (fs2(2)-fs2(1))*yd/pas
      IF (p2 .GT. eps3) THEN
        s2b = s2b + r2b/p2
        p2b = p2b - s2*r2b/p2**2
      ELSE
        s2b = 0.D0
        p2b = 0.D0
        b2b = 0.D0
      END IF
      s1b = s1b + r1b/p1
      p1b = p1b - s1*r1b/p1**2
      CALL POPREAL8(s2)
      ydb = (fs1(2)-fs1(1))*s1b/pas + (fp1(2)-fp1(1))*p1b/pas + (fb1(2)-&
&       fb1(1))*b1b/pas + (fb2(2)-fb2(1))*b2b/pas + (fp2(2)-fp2(1))*p2b/&
&       pas + (fs2(2)-fs2(1))*s2b/pas
      CALL POPREAL8(s1)
      CALL POPREAL8(p2)
      CALL POPREAL8(p1)
      yb = ydb
      p1b = 0.D0
      p2b = 0.D0
    END IF
    zb = zb + yb
! ... Format Declarations ...
  END SUBROUTINE RHSBP_S_B
!========================================================================
! Variables hydrauliques
! Indice de la section de calcul
! Cote et cote de reference
! positionnement section/profils
! Profil et Profils planimetres
! Unite logique fichier listing
! Erreur

  SUBROUTINE RHSBP_SECTION_S_B(varsect, varsectb, zref, z, zb, idt, xdt&
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
    DOUBLE PRECISION :: varsect
    DOUBLE PRECISION :: varsectb
    DOUBLE PRECISION, DIMENSION(:, :), INTENT(IN) :: varprof
    DOUBLE PRECISION, INTENT(IN) :: zref
    DOUBLE PRECISION, INTENT(IN) :: z
    DOUBLE PRECISION :: zb
    INTEGER, INTENT(IN) :: idt
    DOUBLE PRECISION, INTENT(IN) :: xdt
    TYPE(PROFIL_T), DIMENSION(:), INTENT(IN) :: profil
    TYPE(ERREUR_T), INTENT(INOUT) :: erreur
!.. Variables locales ..
! Pas de planimetrage
    DOUBLE PRECISION :: pas
! Hauteur d'eau a la section
    DOUBLE PRECISION :: ydb
! Hauteur d'eau a la section
    DOUBLE PRECISION :: y_loc
    DOUBLE PRECISION :: y_locb
! Fonction locale d'interpolation
    DOUBLE PRECISION, DIMENSION(2) :: fs
! Indice
    INTEGER :: kdt
! indice du profil aval de la section
    INTEGER :: ip1t
! Nombre de pas de planimetrage
    INTEGER :: nbpas
    INTRINSIC INT
    INTEGER :: branch
!character(132)             :: !arbredappel_old
!============================ Instructions =================================
!arbredappel_old = trim(!Erreur%arbredappel)
!Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>RHSBP_SECTION_S'
    y_loc = z - zref
!------------------------
! Cas d'une section a sec
!------------------------
    IF (y_loc .LE. eps3) THEN
      y_loc = eps3
      CALL PUSHCONTROL1B(0)
    ELSE
      CALL PUSHCONTROL1B(1)
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
    IF (y_loc .GE. eps6) THEN
      fs(1) = varprof(idt, kdt) + (varprof(ip1t, kdt)-varprof(idt, kdt))&
&       *xdt
      fs(2) = varprof(idt, kdt+1) + (varprof(ip1t, kdt+1)-varprof(idt, &
&       kdt+1))*xdt
      ydb = (fs(2)-fs(1))*varsectb/pas
    ELSE
      ydb = 0.D0
    END IF
    y_locb = ydb
    CALL POPCONTROL1B(branch)
    IF (branch .EQ. 0) y_locb = 0.D0
    zb = zb + y_locb
  END SUBROUTINE RHSBP_SECTION_S_B

END MODULE M_RHSBP_S_B

