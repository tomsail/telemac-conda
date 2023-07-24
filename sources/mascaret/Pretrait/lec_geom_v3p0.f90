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
!


  subroutine LEC_GEOM_V3P0       ( &
       Profil                    , & ! Tableau des profils geometriques
       FrottParoiVerticale       , & ! Flag
       Fichier                   , & ! Fichier contenant les profils
       FormatGeom                , & ! Format du fichier
       UniteListing              , & ! Unite logique fichier listing
       Erreur                      & ! Erreur
                                 )

! *********************************************************************
! PROGICIEL : MASCARET       S. MANDELKERN
!                            D. POIZAT
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
!  FONCTION : LECTURE DES FICHIERS DE DONNEES HYDRAULIQUE
!  --------   ET FUSION DES TABLEAUX AVEC CEUX ENTREES EN
!             ARGUMENT
!
!----------------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :       - Fichier listing (UniteListing)
!   ----------------------         - Fichier des profils
!
!   SOUS-PROGRAMME(S) APPELANT(S) : - LEC_GEOM
!   -----------------------------
!   SOUS-PROGRAMME(S) APPELE(S)   : - LIRE_CHAINE_S (module)
!   ---------------------------     - DECODER_GEOM
!   COMMENTAIRES :
!   ------------
!
!   DOCUMENTATION EXTERNE :
!   ---------------------
!
!***********************************************************************

   !============================= Declarations ============================
   use M_PRECISION
   use M_PARAMETRE_C
   use M_FICHIER_T            ! Definition du type FICHIER_T
   use M_PROFIL_T             ! Definition du type PROFIL_T
   use M_ERREUR_T             ! Definition du type ERREUR_T
   use M_MESSAGE_C            ! Definition des messages d'erreur
   use M_TRAITER_ERREUR_I     ! Inteface generique de gestion des erreurs
   use M_DECODER_GEOM_I       ! Interface de  sous-programme
   use M_LIRE_CHAINE_S        ! lecture de lignes de commentaire

   !.. Declarations explicites ..
   implicit none

   !.. Arguments ..
   type(PROFIL_T) , dimension(:), pointer       :: Profil
   type(FICHIER_T)              , intent(in   ) :: fichier
   logical                      , intent(in   ) :: FrottParoiVerticale
                                                   ! test de frottement
   integer                      , intent(in   ) :: FormatGeom
   integer                      , intent(in   ) :: UniteListing
   type(ERREUR_T)               , intent(inout) :: Erreur
   !.. Constantes ..
   integer     , parameter :: LEN_CHAINE = 80
   !.. Variables locales ..
   ! Constantes sur les mots-cles
   integer , parameter :: NB_MOT_CLE = 3
   character(*), dimension(NB_MOT_CLE)   , parameter :: MOT_CLE   = &
       (/            &
       "PROFIL"    , &
       "profil"    , &
       "Profil"      &
       /)
   character(1), parameter :: CHAINE_COMMENTAIRE  = "#"   ! caractere commentaire qui debute
                                                           ! une ligne commentaire
   logical       :: presence_mot_cle   ! Test de presence du mot cle
   integer       :: ul                 ! Unite logique du fichier
   integer       :: retour             ! Code de retour de la fonction read
   integer       :: ibief              ! Compteur sur les biefs
   integer       :: iprof              ! Compteur sur les profils
   integer       :: j                  ! Compteur
   integer       :: nb_prof            ! Nombre de Profils lues dans le fichier
   integer       :: ipoint             ! Compteur sur les points d'un profil
   integer       :: nb_point           ! Nombre de points d'un profil
   character(30) :: nom_prof           ! Nom du profil
   character(30) :: nom_bief           ! Nom du bief
   real(DOUBLE)  :: absc_rel           ! abscisse relative du profil
   character(1)  :: label              ! Lit auquel le point appartient bathy/topo
   character(1)  :: label_old          ! Lit auquel le point appartient bathy/topo
   logical       :: presence_lim_g     ! test d'une limite lit mineur gauche
   logical       :: presence_lim_d     ! test d'une limite lit mineur droite
   real(DOUBLE)  :: cote_max           ! Cote extreme maximum d'un profil
   logical       :: paroi_verticale    ! test de presence de parois verticales
   integer       :: iparoi             ! co;pteur de parois verticales
   character(LEN_CHAINE) :: chaine     ! Chaine contenant une ligne du fichier
   !character(132):: !arbredappel_old    ! Arbredappel initial

   !============================ Commentaires ============================
   ! Description du fichier lu :
   !----------------------------
   ! Toute ligne de commentaire est debutee par le caractere "#"
   ! Ligne 1 : mot-cle MOT_CLE, nom_bief, nom_profil, abscisse profil.
   !           sans blanc dans les noms : le blanc est le caractere separateur
   !           Ex: <MOT_CLE bief1 profil1 340.3
   ! Ligne 2 et jusqu'au prochain mot-cle MOT ou la fin de fichier :
   !           (T, Z, LIT)
   !           Exemple :
   !                   <   0. 2000. B>
   !                   <  10. 3000. T>
   !                   < 100. 4000. T>
   !                   etc.

   !============================ Instructions ==============================
   Erreur%Numero = 0
   retour = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>LEC_GEOM_V3P0'

   ! ===============================================================
   ! Premiere lecture du fichier afin de connaitre le nombre de profils
   ! ===============================================================
   ! et de deceler les erreurs
   ! ===============================================================

   ! Ouverture du fichier a lire
   ! ---------------------------
   open( unit=fichier%Unite, file=fichier%Nom, access='SEQUENTIAL', &
         action='READ'      , form='FORMATTED' , iostat=RETOUR      , &
         position='rewind'  , status='OLD'     )
   if( RETOUR /= 0 ) then
      Erreur%Numero = 3
      Erreur%ft     = err_3
      Erreur%ft_c   = err_3c
      call TRAITER_ERREUR( Erreur , fichier%Nom )
      return
   end if

   ul = fichier%Unite

   call LIRE_CHAINE_S (chaine, Fichier, CHAINE_COMMENTAIRE, RETOUR)

   ! Ligne 1 : mot-cle "MOT_CLE"
   !----------------------------
   iprof = 0

   label_nb_prof : do while( RETOUR == 0 )
      !========================================================
      ! Tant qu'on n'a pas atteint la fin du fichier
      !  ou qu'une erreur de lecture systeme n'est pas apparue
      !========================================================
      call DECODER_GEOM       ( &
            presence_mot_cle  , &
            nom_bief          , &
            nom_prof          , &
            absc_rel          , &
            chaine            , &
            MOT_CLE           , &
            iprof+1           , &
            Erreur            )
      if( Erreur%Numero /=0 ) then
         return
      end if

      ! Ligne 1, le mot-cle MOT_CLE doit etre present
      if( presence_mot_cle ) then
         ! on incremente le nombre de profils
         iprof = iprof + 1
      else
         !-----------------------------------------------------
         ! Si le mot-cle MOT_CLE n'est pas present sur la ligne
         !-----------------------------------------------------
         if( iprof == 0 ) then
            Erreur%Numero = 78
            Erreur%ft     = err_78
            Erreur%ft_c   = err_78c
            call TRAITER_ERREUR( Erreur )
            return
         endif
      end if

      !-----------------------------
      ! On passe a la ligne suivante
      !-----------------------------
      call LIRE_CHAINE_S( chaine , Fichier , CHAINE_COMMENTAIRE , RETOUR )

   end do label_nb_prof

   !====================================
   ! La lecture du fichier est terminee
   !====================================
   if( RETOUR > 0 ) then
      ! Le code de retour de la fonction READ informe d'une erreur
      !===========================================================
      Erreur%Numero = 79
      Erreur%ft     = err_79
      Erreur%ft_c   = err_79c
      call TRAITER_ERREUR( Erreur , iprof )
      return
   endif

   ! Sinon c'est la fin du fichier
   !==============================
   ! Allocation du tableau Profil
   ! ----------------------------
   nb_prof = iprof
   if(.not.associated(Profil)) allocate( Profil(nb_prof) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'Profil' )
      return
   end if

   ! ===============================================================
   ! 2 eme lecture du fichier pour dimensionner la taille des profils
   ! ===============================================================
   ! Retour au debut du fichier
   rewind (ul)

   ! N Ligne de commentaires
   !========================
   call LIRE_CHAINE_S( chaine , Fichier , CHAINE_COMMENTAIRE , RETOUR )
   if( retour /= 0 ) then
      Erreur%Numero = 77
      Erreur%ft     = err_77
      Erreur%ft_c   = err_77c
      call TRAITER_ERREUR( Erreur )
      return
   end if

   ! Ligne 1 : mot-cle "MOT_CLE"
   !==========
   iprof = 0

   label_dim_prof : do while( RETOUR == 0 )

      call DECODER_GEOM       ( &
            presence_mot_cle  , &
            nom_bief          , &
            nom_prof          , &
            absc_rel          , &
            chaine            , &
            MOT_CLE           , &
            iprof+1           , &
            Erreur            )
      if( Erreur%Numero /=0 ) then
         return
      end if

      !========================================================
      ! Tant qu'on n'a pas atteint la fin du fichier
      !  ou qu'une erreur de lecture systeme n'est pas apparue
      !========================================================
      ! Ligne 1, le mot-cle MOT_CLE doit etre present
      if( presence_mot_cle ) then
         ! On stocke le nombre d'abscisse lus pour le profil precedente
         ! si on n'est pas a la lecture du premier profil
         ! Et on alloue les tableaux Abscisse et Variable
         if( iprof > 0 ) then

            if(.not.associated(Profil(iprof)%X)) allocate( Profil(iprof)%X(ipoint + 2) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Profil/Abscisse' )
               return
            end if

            if(.not.associated(Profil(iprof)%Y)) allocate( Profil(iprof)%Y(ipoint + 2) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Profil/Ordonnee' )
               return
            end if
         end if

         ! on incremente le nombre de profils
         iprof = iprof + 1

         !-------------------------------------------------
         ! on stocke le nom de la branche, le nom du profil
         ! et l'abscisse utilisateur du profil
         !-------------------------------------------------
         Profil(iprof)%NomBief = nom_bief
         Profil(iprof)%Nom     = nom_prof
         Profil(iprof)%AbsRel  = absc_rel

         ! on remet le compteur des abscisses a zero
         ipoint = 0

         !-------------------------------------------------------------
         ! Si le mot-cle MOT_CLE n'est pas present sur la ligne
         !-------------------------------------------------------------
      else
         ! On incremente l'indice de lecture des abscisses
         ipoint = ipoint + 1
      end if

      !------------------------------
      ! On passe a la ligne suivante
      !------------------------------
      call LIRE_CHAINE_S (chaine, Fichier, CHAINE_COMMENTAIRE, RETOUR)

   end do label_dim_prof

   !====================================
   ! La lecture du fichier est terminee
   !====================================
   label_retour : if( RETOUR > 0 ) then
      ! Le code de retour de la fonction READ informe d'une erreur
      !============================================================
      Erreur%Numero = 80
      Erreur%ft     = err_80
      Erreur%ft_c   = err_80c
      call TRAITER_ERREUR( Erreur , iprof , ipoint )
      return
   else label_retour
      ! Le code de retour de la fonction READ informe de la fin du fichier
      !===================================================================
      ! On stocke le nombre d'abscisse lus pour le dernier mot-cle.
      ! Et on alloue les tableaux Abscisse et Ordonnee
      ! en reservant 2 points en plus aux extremites
      if(.not.associated(Profil(iprof)%X)) allocate( Profil(iprof)%X(ipoint + 2) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Profil/Abscisse' )
         return
      end if
      if(.not.associated(Profil(iprof)%Y)) allocate( Profil(iprof)%Y(ipoint + 2) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Profil/Ordonnee' )
         return
      end if

      ! ===============================================================
      !             3 eme lecture - Lecture effective des profils
      ! ===============================================================
      ! Retour au debut du fichier
      rewind (ul)

      ! N Ligne de commentaires
      !========================
      call LIRE_CHAINE_S( chaine , Fichier , CHAINE_COMMENTAIRE , RETOUR )
      if( retour /= 0 ) then
         Erreur%Numero = 77
         Erreur%ft     = err_77
         Erreur%ft_c   = err_77c
         call TRAITER_ERREUR( Erreur )
         return
      end if

      ! Ligne 1 : mot-cle "MOT_CLE"
      !==========
      boucle_profils : do iprof = 1 , nb_prof
         nb_point = size( Profil(iprof)%X(:) )
         !------------------------------------------------------
         ! initialisation de la position de la lim du lit mineur
         !------------------------------------------------------
         Profil(iprof)%LimiteMin(1) = 2
         Profil(iprof)%LimiteMin(2) = nb_point - 1
         label_old                  = 'T'
         presence_lim_g             = .false.
         presence_lim_d             = .false.

         !-----------------
         ! et du lit majeur
         !-----------------
         Profil(iprof)%LimiteMaj(1) = 2
         Profil(iprof)%LimiteMaj(2) = nb_point - 1

         !------------------------------------------------
         ! Si ce n'est pas le premier profil, il faut lire
         ! la ligne d'en-tete
         !------------------------------------------------
         test_profil1 : if( iprof /= 1 ) then
            ! Lecture d'un mot-cle et d'un numero
            call LIRE_CHAINE_S( chaine , Fichier , CHAINE_COMMENTAIRE , RETOUR )
            if( retour /= 0 ) then
               Erreur%Numero = 77
               Erreur%ft   = err_77
               Erreur%ft_c = err_77c
               call TRAITER_ERREUR  (Erreur)
               return
            end if
         end if test_profil1

         ! Lecture des points du profil iprof
         boucle_points : do ipoint = 2 , nb_point - 1
            call LIRE_CHAINE_S( chaine , Fichier , CHAINE_COMMENTAIRE , RETOUR )
            read(chaine,*, iostat = RETOUR)           &
                       Profil(iprof)%X(ipoint)      , &
                       Profil(iprof)%Y(ipoint)      , &
                       label
            if( RETOUR /= 0 ) then
               Erreur%Numero = 80
               Erreur%ft     = err_80
               Erreur%ft_c   = err_80c
               call TRAITER_ERREUR( Erreur , iprof , ipoint )
               return
            end if

            !-------------------------------------------------------------------
            ! Calcul des limites du lit mineur et controle d'unicite des limites
            !-------------------------------------------------------------------
            if( label == 'B' .and. label_old == 'T' ) then
               if( presence_lim_g ) then
                  Erreur%Numero = 81
                  Erreur%ft     = err_81
                  Erreur%ft_c   = err_81c
                  call TRAITER_ERREUR( Erreur , Profil(iprof)%Nom , 'gauche' )
                  return
               end if
               Profil(iprof)%LimiteMin(1) = ipoint
               presence_lim_g             = .true.
            else if( label == 'T' .and. label_old == 'B' ) then
               if( presence_lim_d ) then
                  Erreur%Numero = 81
                  Erreur%ft     = err_81
                  Erreur%ft_c   = err_81c
                  call TRAITER_ERREUR( Erreur , Profil(iprof)%Nom , 'droite' )
                  return
               end if
               Profil(iprof)%LimiteMin(2) = ipoint - 1
               presence_lim_d             = .true.
            end if
            label_old = label
         end do boucle_points

         !-------------------------------
         ! Definition des points extremes
         !-------------------------------
         cote_max                  = DMAX1( Profil(iprof)%Y(2) , Profil(iprof)%Y(nb_point-1) )
         Profil(iprof)%X(1)        = Profil(iprof)%X(2)
         Profil(iprof)%Y(1)        = cote_max + 10._DOUBLE
         Profil(iprof)%X(nb_point) = Profil(iprof)%X(nb_point-1)
         Profil(iprof)%Y(nb_point) = cote_max + 10._DOUBLE
         !------------------------------
         ! Definition des cotes de rives
         !------------------------------
         Profil(iprof)%Zrive(1) = Profil(iprof)%Y(2)
         Profil(iprof)%Zrive(2) = Profil(iprof)%Y(nb_point - 1)

      end do boucle_profils
   end if label_retour

   !  ==============================================
   !  VERIFICATION DE L'ORDRE DES PROFILS, EFFECTUEE
   !  D' APRES L'ABSCISSE DES PROFILS
   !  ==============================================
   ibief             = 1
   Profil(1)%NumBief = 1

   !-----------------------
   ! boucle sur les profils
   !-----------------------
   do iprof = 2 , nb_prof
      j = iprof - 1
      if( Profil(iprof)%NomBief /= Profil(j)%NomBief ) then
         ibief = ibief + 1
      else if( Profil(iprof)%AbsRel <= Profil(j)%AbsRel ) then
         Erreur%Numero = 63
         Erreur%ft     = err_63
         Erreur%ft_c   = err_63c
         call TRAITER_ERREUR( Erreur , iprof , Profil(iprof)%AbsRel , j , Profil(j)%AbsRel )
         return
      end if
      !-----------------------------------------------------------
      ! Affectation des numeros de branches aux structures profils
      !-----------------------------------------------------------
      Profil(iprof)%NumBief = ibief
   end do

   ! ==========================================================
   ! TEST AFIN DE DETECTER LES PAROIS VERTICALES SUR LESQUELLES
   ! AUCUN FROTTEMENT NE SERA PRIS EN COMPTE
   ! IMPRESSION SYSTEMATIQUE SUR LE LISTING
   ! ==========================================================
   paroi_verticale = .false.

   do iprof = 1 , nb_prof
      iparoi = 0
      do j = 1 , size(Profil(iprof)%X) - 1
         if( abs(Profil(iprof)%X(j)-Profil(iprof)%X(j+1)).lt.EPS15 ) then
            iparoi = iparoi + 1
            if (UniteListing > 0) then
               if( .not.paroi_verticale .and. .not.FrottParoiVerticale ) then
                  write(UniteListing,10012)
               else if( .not.paroi_verticale .and. FrottParoiVerticale ) then
                  write(UniteListing,10013)
               end if
            end if
            paroi_verticale = .true.

            if (UniteListing > 0) then
               if( iparoi == 1 ) then
                  write(UniteListing,10014) iprof , Profil(iprof)%Nom
               end if
               write (UniteListing,10015) j, j+1
            end if
         end if
      end do
   end do

   !----------------------
   ! Fin du sous-programme
   !----------------------
   close(ul)
   !Erreur%arbredappel = !arbredappel_old
   return

   ! Formats d ecriture WARNINGS
   ! ---------------------------

  10012 format (/                                                          , &
          '<<ATTENTION>> : DETECTION DE PAROIS VERTICALES SUR LESQUELLES',/, &
          ' AUCUN FROTTEMENT NE SERA PRIS EN COMPTE',/,80('*'),/)
  10013 format (/                                                          , &
          '<<ATTENTION>> : DETECTION DE PAROIS VERTICALES SUR LESQUELLES',/, &
          ' LE FROTTEMENT SERA PRIS EN COMPTE',/,80('*'),/)
  10014 format ('   SUR LE PROFIL NUMERO   ',i5,'   (',a12,')')
  10015 format ('     ENTRE LES POINTS   ',2i5)

end subroutine LEC_GEOM_V3P0
