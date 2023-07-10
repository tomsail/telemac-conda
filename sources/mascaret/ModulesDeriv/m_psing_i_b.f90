MODULE M_PSING_I_B
  IMPLICIT NONE

  INTERFACE 
      SUBROUTINE PSING_B(zam, zamb, singularite, zref, zav&
&       , zavb, qam, qamb, profil, b1plan, idt, xdt, section, temps, &
&       erreur)
!
! **********************************************************************
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
!
! **********************************************************************
!============================= Declarations ===========================
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
      END SUBROUTINE PSING_B
  END INTERFACE

END MODULE M_PSING_I_B

