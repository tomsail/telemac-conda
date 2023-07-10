!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET-TRACER.
!
!   MASCARET-TRACER is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET-TRACER is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET-TRACER.  If not, see <http://www.gnu.org/licenses/>
!

subroutine STOCK_TRACER( X , &
                      ZREF , &
                   QT , AT , &
                  Ctraceur , &
                   nb_trac , &
                     TEMPS , &
                             ! Modele
                   Connect , &
                             ! Parametre
           FichierResultat , &
                 OptionSto , &
                FormatResu , &
           PhaseSimulation , &
            NumeroPasTemps , &
                    VarSto , &
                SectionSto , &
             FichierMotCle , &
                    Erreur )

!*****************************************************************************
! PROGICIEL : TRACER         S.MANDELKERN - M. LUCK
!
! VERSION : V8P4R0              EDF-CEREMA
!*****************************************************************************
!
!  FONCTION :
!  --------
!
!             STOCKAGE DES RESULTATS SUR FICHIER
!
!-----------------------------------------------------------------------
!                             VARIABLES LOCALES
! .______________________.____._______________________________________________
! !    NOM          !TYPE!MODE!                   ROLE
! !_________________!____!____!_______________________________________________
! ! retour          ! R  !    ! Variable contenant le code de retour de fonctions
! ! NbSectEff       ! R  !    ! Dimension de la liste des sections a stocker
! ! isec            ! R  !    ! Compteur sur les secteurs
! ! SectionStoEff   ! R  !    ! Numeros des sections effectives a sortir (suivant option)
! !_________________!____!____!_______________________________________________
!
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-------------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :       FichierResultat
!   ----------------------
!
!   SOUS-PROGRAMME(S) APPELANT(S) :  Neant
!   -----------------------------
!   SOUS-PROGRAMME(S) APPELE(S) :    STO_OPTHYCA, STO_NONPER, STO_PER
!   ---------------------------      (sous-programmes internes)
!                                    INIT_VAR_SORTIE_S (sous-programme de module)
!***********************************************************************

   !============================= Declarations ============================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_CONNECT_T           ! Definition du type CONNECT_T
   use M_FICHIER_T           ! Definition du type FICHIER_T
   use M_ERREUR_T            ! Definition du type ERREUR_T
   use M_CONSTANTES_CALCUL_C ! Constantes servant a reperer la phase de calcul
   use M_INDEX_VARIABLE_TRACER_C    ! Constantes servant a reperer les variables
   use M_PARAMETRE_C         ! Epsilon pour la difference de 2 temps
   use M_INIT_VAR_SORTIE_TRACER_S   ! Initialisation des structures des variables a sortir
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs
   use M_MESSAGE_C           ! Liste des messages d'erreur

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE)   , dimension(:)  , pointer :: X
   real(DOUBLE)   , dimension(:)  , pointer :: ZREF
   real(DOUBLE)   , dimension(:)  , pointer :: QT,AT
   real (DOUBLE)  , dimension(:,:), pointer :: Ctraceur
   integer                        , intent(in   ) :: nb_trac
   real(DOUBLE)                   , intent(in   ) :: TEMPS
   ! Modele
   type(CONNECT_T)                , intent(in   ) :: Connect
   ! Parametres
   type(FICHIER_T)                , intent(in   ) :: FichierResultat
   type(FICHIER_T)                , intent(in   ) :: FichierMotCle
   integer                        , intent(in   ) :: OptionSto
   integer                        , intent(in   ) :: FormatResu
   integer                        , intent(in   ) :: PhaseSimulation, NumeroPasTemps
   logical        , dimension(:)  , intent(in   ) :: VarSto
   integer        , dimension(:)  , pointer       :: SectionSto
   type(ERREUR_T)                 , intent(inout) :: Erreur
   !.. Variables locales ..
   !-----------------------
   integer, dimension(:), pointer :: SectionStoEff => null() ! Numeros des sections a sortir
   integer                        :: retour
   integer                        :: NbSectEff
   integer                        :: isec
   ! Declaration des 2 structures qui permettront de realiser des boucles
   ! sur les variables a stocker :
   ! 1. Declaration de la structure contenant toutes les informations constantes
   ! sur les variables a stocker.
   type(VAR_NOM_T), dimension(NB_TOT_VARTR) :: var_nom
   ! 2. Declaration de la structure contenant toutes les informations dependantes
   ! de la simulation sur les variables a stocker.
   type(GDR_STO_T), dimension(NB_TOT_VARTR) :: gdr

   !============================== Instructions ==============================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   retour        = 0
   !arbredappel_old = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>STOCK_TRACER'

   call INIT_VAR_SORTIE_TRACER_S( &
                    var_nom     , &
                    gdr         , &
                    X           , &
                    ZREF        , &
                    QT   , AT   , &
                    Ctraceur    , &
                    nb_trac     , &
                    VarSto      , &
                    PHASE_INITIALISATION &
                               )

   ! Traitement du probleme spatial
   ! ------------------------------
   if( OptionSto == STOCKAGE_LISTE_SECTION ) then
      ! les sections a considerer sont celles du tableau donne en entree
      NbSectEff = size(SectionSto)
      SectionStoEff => SectionSto
   else
      ! toutes les sections sont a considerer
      NbSectEff = size(X)
      allocate( SectionStoEff(NbSectEff) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'SectionStoEff' )
         return
      end if

      do isec = 1 , NbSectEff
         SectionStoEff(isec) = isec
      end do

   end if

   ! APPEL AU SOUS PROGRAMME INTERNE DE STOCKAGE SUIVANT LE FORMAT CHOISI
   ! --------------------------------------------------------------------
   select case( FormatResu )

      case( FORMAT_STO_NONPERMANENT )

         call STO_NONPER(       &
             Connect          , &
             NbSectEff        , &
             var_nom          , &
             gdr              , &
             FichierResultat  , &
             PhaseSimulation  , &
             TEMPS            , &
             NumeroPasTemps   , &
             SectionStoEff    , &
             Erreur             &
             )

         if( Erreur%Numero /= 0 ) then
            return
         endif

      case( FORMAT_STO_OPTHYCA )

         call STO_OPTHYCA(      &
             Connect          , &
             NbSectEff        , &
             var_nom          , &
             gdr              , &
             FichierResultat  , &
             PhaseSimulation  , &
             TEMPS            , &
             SectionStoEff    , &
             Erreur             &
             )

         if( Erreur%Numero /= 0 ) then
            return
         endif

      end select

      ! liberation des allocations internes
      if( OptionSto /= STOCKAGE_LISTE_SECTION ) then

         deallocate( SectionStoEff ,STAT = retour )

         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'SectionStoEff' )
            return
         end if

   endif

   !Erreur%arbredappel = arbredappel_old

   return

contains

!***********************************************************************
!
!  SOUS-PROGRAMME INTERNE
!
!***********************************************************************

!            ===========
  subroutine STO_OPTHYCA( &
!            ===========
       Connect          , & ! Structure contenant la table de connectivite
       NbSectEff        , & ! Nombre des sections pour le stockage
       var_nom          , & ! Infos     constantes sur les variables a stocker
       gdr              , & ! Infos non constantes sur les variables a stocker
       FichierResultat  , & ! Structure pour le fichier de stockage
       PhaseSimulation  , & ! Variable indiquant la phase de la simulation
       TEMPS            , & ! Temps
       SectionStoEff    , & ! Tableau contenant la liste des sections a sortir
       Erreur             & ! Erreur
       )
!***********************************************************************
!
!  FONCTION :   STOCKAGE SUR FICHIER AU FORMAT OPTHYCA
!  --------
!
!  SOUS PROGRAMMES APPELANT(S) : STOCK
!  ---------------------------
!  SOUS PROGRAMMES APPELE(S) :   Neant
!  -------------------------
!***********************************************************************

   !============================= Declarations ===========================
   use M_CONNECT_T           ! Definition du type CONNECT_T
   use M_ERREUR_T            ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs
   use M_MESSAGE_C           ! Liste des messages d'erreur
   use M_NUM_BIEF_S          ! Calcul du bief relatif a une section

   !.. Declarations implicites ..
   implicit none

   !.. Arguments ..
   type(CONNECT_T),                       intent(in) :: Connect
   integer                              , intent(in) :: NbSectEff
   type(VAR_NOM_T), dimension(:)        , intent(in) :: var_nom
   type(GDR_STO_T), dimension(:), target, intent(in) :: gdr
   type(FICHIER_T)                      , intent(in) :: FichierResultat
   integer                              , intent(in) :: PhaseSimulation
   real(DOUBLE)                         , intent(in) :: TEMPS
   integer        , dimension(:)        , intent(in) :: SectionStoEff
   type(ERREUR_T)                       , intent(inout) :: Erreur
   !.. Constantes ..
   integer     , parameter :: LEN_PRECISION= 20 ! Precision sur 7 chiffres maxi
   !.. Variables locales ..
   character(LEN_PRECISION) :: fmt_precision    ! Variable contenant le format
   integer :: ul                             ! Unite logique du fichier de stockage
   integer :: retour                         ! Variable contenant le code de retour
                                             ! des fonctions d'E/S
   integer :: ivar                           ! Compteur sur les variables a sortir
   integer :: isec                           ! Compteur sur les sections
   integer :: num_sect                       ! Indice sur la liste des sections a stocker

   !============================ Instructions ==============================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   !arbredappel_old = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>STO_OPTHYCA'

   ul = FichierResultat%Unite

   if( PhaseSimulation == PHASE_INITIALISATION ) then
      open( unit = ul , file = FichierResultat%Nom , access = 'SEQUENTIAL' , &
            action = 'WRITE' , form = 'FORMATTED', iostat = RETOUR , &
            position = 'append' , status = 'REPLACE' )
   else
      open( unit = ul , file = FichierResultat%Nom , access = 'SEQUENTIAL' , &
            action = 'WRITE' , form = 'FORMATTED' , iostat = RETOUR , &
            position = 'append' , status = 'OLD' )
   end if

   if( RETOUR /= 0 ) then
      Erreur%Numero = 4
      Erreur%ft     = err_4
      Erreur%ft_c   = err_4c
      call TRAITER_ERREUR( Erreur , FichierResultat%Nom )
      return
   end if

   ! ECRITURE DES DONNEES INITIALES
   ! ==============================
   if( PhaseSimulation == PHASE_INITIALISATION ) then
      write(ul,'(A)') "[variables]"

      do ivar = 2 , NB_TOT_VARTR

         ! En commencant la boucle a 2 on exclut la definition de
         ! la variable X dont les valeurs numeriques seront toujours stockees
         if( gdr(ivar)%ASortir ) then
            write(ul,"('""',A,'""',';','""',A,'""',';','""',A,'""',';',I2)") &
                 trim(var_nom(ivar)%NomLong), &
                 trim(var_nom(ivar)%NomA4)  , &
                 trim(var_nom(ivar)%Unite)  , &
                 var_nom(ivar)%Precision
         end if
      end do

      write(ul,'(A)') "[resultats]"
   end if

   ! ECRITURE DES DONNEES AU COURS DU TEMPS (et au pas de temps 0)
   ! ======================================
   do isec = 1 , NbSectEff
      num_sect = SectionStoEff(isec)

      write(ul,"(F12.1,';','""',i2,'""',';','""',i5,'""')",advance='NO') &
            TEMPS, NUM_BIEF_S(Connect, num_sect, Erreur) , num_sect

      do ivar = 1 , NB_TOT_VARTR

         if( gdr(ivar)%ASortir ) then

            write(fmt_precision,'("("";""f",I2,".",I2,")")') &
                  var_nom(ivar)%Precision + 9 , &
                  var_nom(ivar)%Precision
            write(ul, fmt_precision,advance='NO') &
                  REAL(gdr(ivar)%Valeur(num_sect), kind=SIMPLE)

         end if

      end do

      write(ul, *) ! pour le retour a la ligne

   end do

   close(ul)

   !Erreur%arbredappel = arbredappel_old

   return

end subroutine STO_OPTHYCA

!***********************************************************************
!
!  SOUS-PROGRAMME INTERNE
!
!***********************************************************************
!            ==========
  subroutine STO_NONPER ( &
!            ==========
       Connect          , & ! Indices des sections des biefs
       NbSectEff        , & ! Nombre des sections pour le stockage
       var_nom          , & ! Infos constantes     sur les variables a stocker
       gdr              , & ! Infos non constantes sur les variables a stocker
       FichierResultat  , & ! Structure pour le fichier de stockage
       PhaseSimulation  , & ! Variable indiquant la phase de la simulation
       TEMPS            , & ! Temps
       NumeroPasTemps   , & ! Numero du pas de temps en cours
       SectionStoEff    , & ! Tableau contenant la liste des sections a sortir
       Erreur             & ! Erreur
                          )

!***********************************************************************
!
!  FONCTION :  STOCKAGE SUR FICHIER AU FORMAT `LIDO NON PERMANENT'
!  --------
!
!  SOUS PROGRAMMES APPELANT(S) : STOCK
!  ---------------------------
!  SOUS PROGRAMMES APPELE(S)   : Neant
!  -------------------------
!
!***********************************************************************

   !============================= Declarations ===========================
   use M_CONNECT_T           ! Definition du type CONNECT_T
   use M_ERREUR_T            ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs
   use M_MESSAGE_C           ! Liste des messages d'erreur

   !.. Declarations explicites ..
   implicit none

   !.. Arguments ..
   type(CONNECT_T),                       intent(in) :: Connect
   integer                              , intent(in) :: NbSectEff
   type(VAR_NOM_T), dimension(:)        , intent(in) :: var_nom
   type(GDR_STO_T), dimension(:)        , intent(in) :: gdr
   type(FICHIER_T)                      , intent(in) :: FichierResultat
   integer                              , intent(in) :: PhaseSimulation, NumeroPasTemps
   real(DOUBLE)                         , intent(in) :: TEMPS
   integer        , dimension(:)        , intent(in) :: SectionStoEff
   type(ERREUR_T)                       , intent(inout) :: Erreur
   !.. Constantes ..
   character(LEN=4), parameter :: NOM_FIN       = "FIN "
   character(LEN=4), parameter :: NOM_IDEB_BIEF = "I1  "
   character(LEN=4), parameter :: NOM_IFIN_BIEF = "I2  "
   !.. Variables locales ..
   character(LEN=72), dimension(3) :: TITRE
   !    integer :: indice_temps = 0
   integer :: ul                           ! Unite logique du fichier de stockage
   integer :: retour                       ! Variable contenant le code de retour
                                           ! des fonctions d'E/S
   integer :: nb_bief                      ! Nombre de bief
   integer :: ivar                         ! Compteur sur les variables a sortir
   integer :: isec                         ! Compteur sur les valeurs des var a sortir
   integer :: ibief                        ! Compteur sur les biefs

   !============================ Instructions ==============================
   ! INITIALISATIONS
   ! ---------------
   Erreur%Numero = 0
   ! arbredappel_old = trim(Erreur%arbredappel)
   ! Erreur%arbredappel = trim(Erreur%arbredappel)//'=>STO_NONPER'
   TITRE(1) = ' '
   TITRE(2) = ' '
   TITRE(3) = ' '

   ul = FichierResultat%Unite

   nb_bief = size(Connect%OrigineBief)

   if( PhaseSimulation == PHASE_INITIALISATION ) then

      open( unit = ul , file = FichierResultat%Nom , access = 'SEQUENTIAL' , &
            action = 'WRITE' , form = 'UNFORMATTED' , iostat = RETOUR , &
            position = 'append' , status = 'REPLACE' )

   else ! PhaseSimulation == PHASE_CALCUL

      open( unit = ul , file = FichierResultat%Nom , access = 'SEQUENTIAL' , &
            action = 'WRITE' , form = 'UNFORMATTED' , iostat = RETOUR , &
            position = 'append' , status = 'OLD' )

   end if

   if( RETOUR /= 0 ) then
      Erreur%Numero = 4
      Erreur%ft     = err_4
      Erreur%ft_c   = err_4c
      call TRAITER_ERREUR( Erreur , FichierResultat%Nom )
      return
   end if

   if( PhaseSimulation == PHASE_INITIALISATION ) then

      ! ECRITURE DES DONNEES INITIALES
      ! ------------------------------

      ! TITRE
      write (ul) TITRE(1)
      write (ul) TITRE(2)
      write (ul) TITRE(3)

      ! VARIABLES INITIALES : indices origine et fin de bief
      write (ul) NOM_IDEB_BIEF
      write (ul) NOM_IFIN_BIEF
      write (ul) NOM_FIN
      write (ul) nb_bief, nb_bief
      write (ul) (Connect%OrigineBief(ibief), ibief = 1, nb_bief)
      write (ul) (Connect%FinBief(ibief), ibief = 1, nb_bief)

      ! DONNEES INDEPENDANTES DU TEMPS
      ! ------------------------------

      ! NOMS DES VARIABLES
      do ivar = 1 , NB_TOT_VARTR
         if( gdr(ivar)%ASortir .and. .not. var_nom(ivar)%DependantTemps ) then
            write(ul) var_nom(ivar)%NomA4
         end if
      end do

      write(ul) NOM_FIN

      ! VALEURS DES VARIABLES INDEPENDANTES DU TEMPS

      write (ul) NbSectEff , NbSectEff

      do ivar = 1 , NB_TOT_VARTR

         if( gdr(ivar)%ASortir .and. .not. var_nom(ivar)%DependantTemps ) then

            write(ul) (REAL(gdr(ivar)%Valeur(SectionStoEff(isec)),kind=SIMPLE), &
                  isec = 1, NbSectEff)

         end if

      end do

      ! NOMS DES VARIABLES DEPENDANTES DU TEMPS

      do ivar = 1 , NB_TOT_VARTR

          if( gdr(ivar)%ASortir .and. var_nom(ivar)%DependantTemps ) then

             write(ul) var_nom(ivar)%NomA4

          end if

      end do

      write(ul) NOM_FIN

   end if            ! de IF Phase_Simul

   ! ECRITURE DES DONNEES DEPENDANTES DU TEMPS
   ! -----------------------------------------

   write(ul) NumeroPasTemps , NumeroPasTemps
   write(ul) real(TEMPS,kind=SIMPLE) , real(TEMPS,kind=SIMPLE)
   write(ul) NbSectEff , NbSectEff

   do ivar = 1 , NB_TOT_VARTR

      if (gdr(ivar)%ASortir .and. var_nom(ivar)%DependantTemps) then

         write(ul) (real(gdr(ivar)%Valeur(SectionStoEff(isec)),kind=SIMPLE), &
               isec = 1,NbSectEff)

      end if

   end do

   close(ul)

   ! Erreur%arbredappel = arbredappel_old

   return

end subroutine STO_NONPER

end subroutine STOCK_TRACER
