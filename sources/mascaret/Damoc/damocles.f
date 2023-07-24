C                       ***********************
                        SUBROUTINE DAMOCLESMASC
C                       ***********************
C
     *( ADRESS , DIMENS , NMAX   , DOC    , LLNG , LLU ,
     *  MOTINT , MOTREA , MOTLOG , MOTCAR ,
     *  MOTCLE , TROUVE , NFICMO , NFICDA , GESTD  )
C
C***********************************************************************
C DAMOCLES VERSION 5.0     16/08/94   J.M. HERVOUET (LNH)   30 87 80 18
C                                      A. YESSAYAN
C                                      L. LEGUE
C                          14/12/93    O. QUIQUEMPOIX (LNH)  30 87 78 70
C                          01/05/98    Arnaud Desitter (NAG)
C Copyright EDF 1998
C
C
C***********************************************************************
C
C FONCTION  : CORPS PRINCIPAL DE LA BIBILIOTHEQUE DAMOCLES
C             APPELE PAR L'EXECUTABLE DAMOCLES (DAMOCLE.f)
C             APPELE PAR LES CODES DE CALCULS DU LNH
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C !      NOM       !MODE!                   ROLE                       !
C !________________!____!______________________________________________!
C !                !    !                                              !
C !  ADRESS        !<-- ! TABLEAU DES ADRESSES DES MOTS CLES           !
C !  DIMENS        !<-- ! TABLEAU DES DIMENSIONS DES MOTS CLES         !
C !  NMAX          ! -->! TAILLE MAXIMALE AUTORISEE POUR LES TABLEAUX  !
C !  DOC           ! -->! LOGIQUE DE DOCUMENTATION DE LA SORTIE        !
C !                !    ! = VRAI : IMPRIME L'AIDE (FICHIER RESULTAT)   !
C !                !    ! = FAUX : N'IMPRIME PAS L'AIDE                !
C !  LLNG          ! -->! NUMERO DE LA LANGUE DE DECODAGE              !
C !  LLU           ! -->! NUMERO DE L'UNITE LOGIQUE DES SORTIES        !
C !  MOTINT        !<-- ! TABLEAU DES VALEURS ENTIERES                 !
C !  MOTREA        !<-- ! TABLEAU DES VALEURS REELLES                  !
C !  MOTLOG        !<-- ! TABLEAU DES VALEURS LOGIQUES                 !
C !  MOTCAR        !<-- ! TABLEAU DES VALEURS CARACTERES               !
C !  MOTATT        !<-- ! TABLEAU DES SUBMITS                          !
C !  DEFINT        !<-- ! TABLEAU DES VALEURS ENTIERES PAR DEFAUT      !
C !  DEFREA        !<-- ! TABLEAU DES VALEURS REELLES PAR DEFAUT       !
C !  DEFLOG        !<-- ! TABLEAU DES VALEURS LOGIQUES PAR DEFAUT      !
C !  DEFCAR        !<-- ! TABLEAU DES VALEURS CARACTERES PAR DEFAUT    !
C !  DEFATT        !<-- ! TABLEAU DES SUBMITS PAR DEFAUT               !
C !  USRINT        !<-- ! TABLEAU DES VALEURS ENTIERES A USAGE LOCAL   !
C !  USRREA        !<-- ! TABLEAU DES VALEURS REELLES A USAGE LOCAL    !
C !  USRLOG        !<-- ! TABLEAU DES VALEURS LOGIQUES A USAGE LOCAL   !
C !  USRCAR        !<-- ! TABLEAU DES VALEURS CARACTERES A USAGE LOCAL !
C !  USRATT        !<-- ! TABLEAU DES SUBMITS A USAGE LOCAL            !
C !  MOTCLE        !<-- ! TABLEAU DES MOTS CLES ACTIFS                 !
C !  SIZE          !<-- ! TABLEAU DES LONGUEURS DES MOTS CLES          !
C !  TROUVE        !<-- ! INDICATEUR D'ETAT DES MOTS CLES              !
C !                !    ! = 0 : AUCUNE VALEUR TROUVEE                  !
C !                !    ! = 1 : VALEUR PAR DEFAUT TROUVEE              !
C !                !    ! = 2 : VALEUR TROUVEE (FICHIER DE DONNEES)    !
C !                !    ! = 3 : AUCUNE VALEUR TROUVEE (OPTIONNELLE)    !
C !                !    ! = 5 : TABLEAU DE MOTS A SUBMIT COMPACTE      !
C !                !    ! = 6 : MOT CLE A SUBMIT FORCE NON AFFECTE     !
C !                !    ! = 7 : MOT CLE A SUBMIT FORCE AFFECTE (DICO)  !
C !                !    ! = 8 : MOT CLE A SUBMIT FORCE AFFECTE (CAS)   !
C !                !    ! = 9 : FICHIER DICO : SUBMIT + VALEUR LANCEUR !
C !                !    ! =10 : FICHIER CAS  : SUBMIT + VALEUR LANCEUR !
C !  UTINDX        !<-- ! TABLEAU DE LOGIQUES D'UTILISATION DES INDEX  !
C !  NFICMO        ! -->! NUMERO DE CANAL DU FICHIER DES MOTS-CLES     !
C !  NFICDA        ! -->! NUMERO DE CANAL DU FICHIER DES DONNEES       !
C !  INDIC         !<-- ! TABLEAU D'INDICATEURS D'ETAT DES MOTS CLES   !
C !                !    ! = 0 : PAS DE SUBMIT & NON TABLEAU            !
C !                !    ! = 1 : PAS DE SUBMIT & TABLEAU                !
C !                !    ! = 2 : AVEC   SUBMIT & NON TABLEAU            !
C !                !    ! = 3 : AVEC   SUBMIT & TABLEAU                !
C !  GESTD         ! -->! LOGIQUE D'APPEL PAR LE GESTIONNAIRE D'ETUDES !
C !  NBLANG        ! -->! NOMBRE DE LANGUES CONNUES                    !
C !________________!____!______________________________________________!
C !                !    !                                              !
C !   /COMMON/     !    !                                              !
C !                !    !                                              !
C !    DCINFO      !    !                                              !
C !  . LNG         ! -->! NUMERO DE LA LANGUE DE DECODAGE              !
C !  . LU          ! -->! NUMERO DE L'UNITE LOGIQUE DES SORTIES        !
C !                !    !                                              !
C !    DCMLIG      !    !                                              !
C !  . NLIGN       !<-->! NUMERO DE LA LIGNE TRAITEE DANS LE FICHIER LU!
C !  . LONGLI      ! -->! LONGUEUR DES LIGNES                          !
C !                !    !                                              !
C !    DCRARE      !    !                                              !
C !  . ERREUR      !<-->! SORT AVEC LA VALEUR .TRUE. EN CAS D'ERREUR   !
C !  . RETOUR      !<-->! SORT AVEC LA VALEUR .TRUE. EN CAS DE FIN DE  !
C !                !    ! FIN DE FICHIER OU D'ERREUR DE LECTURE.       !
C !                !    !                                              !
C !    DCNGEC      !    !                                              !
C !  . PARAM       !<-->! NOM DU MOT CLE EN COURS                      !
C !                !    !                                              !
C !    DCNGE       !    !                                              !
C !  . INDX        !<-->! INDEX DU MOT CLE EN COURS                    !
C !  . NTYP        !<-->! TYPE DU MOT CLE EN COURS                     !
C !  . ITAI        !<-->! TAILLE DU MOT CLE EN COURS                   !
C !  . LONGU       !<-->! LONGUEUR DU MOT CLE EN COURS                 !
C !  . NMOT        !<-->! TABLEAU DU NOMBRE DE MOTS CLES PAR TYPE      !
C !  . DEFLU       !<-->! NOMBRE DE VALEURS LUES POUR LE MOT CLE       !
C !                !    !                                              !
C !    DCCHIE      !    !                                              !
C !  . NFIC        ! -->! NUMERO DE CANAL DU FICHIER EN COURS DE LECT. !
C !________________!____!______________________________________________!
C
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C***********************************************************************
C
      IMPLICIT NONE
C
      INTEGER            ,INTENT(IN)    :: NMAX,LLNG,LLU,NFICMO,NFICDA
      INTEGER            ,INTENT(OUT)   :: MOTINT(*),ADRESS(4,*)
      INTEGER            ,INTENT(OUT)   :: DIMENS(4,*),TROUVE(4,*)
      LOGICAL            ,INTENT(OUT)   :: MOTLOG(*)
      LOGICAL            ,INTENT(IN)    :: DOC, GESTD
      CHARACTER(LEN=72)  ,INTENT(OUT)   :: MOTCLE(4,*)
      CHARACTER(LEN=144) ,INTENT(OUT)   :: MOTCAR(*)
      DOUBLE PRECISION   ,INTENT(OUT)   :: MOTREA(*)
C
C     TABLEAUX AUTOMATIQUES
C
C      INTEGER :: DEFINT(NMAX),USRINT(NMAX)
C      INTEGER          :: SIZE(4,NMAX)
C      INTEGER          :: INDIC(4,NMAX)
C      LOGICAL          :: DEFLOG(NMAX),USRLOG(NMAX),UTINDX(4,NMAX)
C      CHARACTER(len=144) :: MOTATT(4,NMAX),DEFATT(NMAX),USRATT(NMAX)
C      CHARACTER(len=144) :: DEFCAR(NMAX),USRCAR(NMAX)
C      DOUBLE PRECISION :: DEFREA(NMAX),USRREA(NMAX)

C       PU2017 : Declaration
      INTEGER, ALLOCATABLE :: DEFINT(:),USRINT(:)
      INTEGER, ALLOCATABLE :: SIZE(:,:)
      INTEGER, ALLOCATABLE :: INDIC(:,:)
      LOGICAL, ALLOCATABLE :: DEFLOG(:),USRLOG(:),UTINDX(:,:)
      CHARACTER(len=144),ALLOCATABLE:: MOTATT(:,:),DEFATT(:),USRATT(:)
      CHARACTER(len=144), ALLOCATABLE :: DEFCAR(:),USRCAR(:)
      DOUBLE PRECISION  , ALLOCATABLE :: DEFREA(:),USRREA(:)
C
      INTEGER,PARAMETER :: NBLANG = 2
C
C     PU2017 : Allocation
      ALLOCATE( DEFINT(NMAX), USRINT(NMAX) )
      ALLOCATE( SIZE(4,NMAX) )
      ALLOCATE( INDIC(4,NMAX) )
      ALLOCATE( DEFLOG(NMAX),USRLOG(NMAX),UTINDX(4,NMAX) )
      ALLOCATE( MOTATT(4,NMAX),DEFATT(NMAX),USRATT(NMAX) )
      ALLOCATE( DEFCAR(NMAX),USRCAR(NMAX) )
      ALLOCATE( DEFREA(NMAX),USRREA(NMAX)  )
C
C     APPEL DE DAMOC
C
      CALL DAMOCMASC( ADRESS , DIMENS , NMAX   , DOC ,
     *            LLNG   , LLU  ,
     *            MOTINT , MOTREA , MOTLOG , MOTCAR , MOTATT ,
     *            DEFINT , DEFREA , DEFLOG , DEFCAR , DEFATT ,
     *            USRINT , USRREA , USRLOG , USRCAR , USRATT ,
     *            MOTCLE , SIZE   , TROUVE , UTINDX , NFICMO , NFICDA ,
     *            INDIC  , GESTD  , NBLANG )
C
C-----------------------------------------------------------------------
C
      RETURN
      END
