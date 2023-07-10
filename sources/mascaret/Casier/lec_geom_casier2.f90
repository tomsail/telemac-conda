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

subroutine LEC_GEOM_CASIER2( &
                             Casier  , & ! Tableau des casiers
                             Fichier , & ! Fichier contenant les casiers
                             Option  , & ! Unite logique fichier listing
                             Erreur  )   ! Erreur

! .....................................................................
!  PROGICIEL : MASCARET        C. RISSOAN
!
!    VERSION : V8P4R0        EDF-CEREMA
! .....................................................................
!
!
!  FONCTION : LECTURE DES FICHIERS DE DONNEES HYDRAULIQUE
!  --------   ET FUSION DES TABLEAUX AVEC CEUX ENTREES EN
!             ARGUMENT
!
!----------------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :       - Fichier listing (UniteListing)
!   ----------------------         - Fichier des profils
!
!   SOUS-PROGRAMME(S) APPELANT(S) : - LEC_CASIER
!   -----------------------------
!   SOUS-PROGRAMME(S) APPELE(S)   : - LIRE_CHAINE_S (module)
!   ---------------------------     - DECODER_GEOM_CASIER
!   COMMENTAIRES :
!   ------------
!
!   DOCUMENTATION EXTERNE :
!   ---------------------
!
!***********************************************************************

!============================= Declarations ============================
   use M_PRECISION
   use M_FICHIER_T                ! Definition du type FICHIER_T
   use M_CASIER_T                 ! Definition du type PROFIL_T
   use M_ERREUR_T                 ! Definition du type ERREUR_T
   use M_MESSAGE_CASIER_C         ! Definition des messages d'erreur
   use M_TRAITER_ERREUR_CASIER_I
   use M_DECODER_GEOM_CASIER_I    ! Interface de  sous-programme
   use M_LIRE_CHAINE_S            ! lecture de lignes de commentaire
   use M_PLANIM_CASIER_I

   !.. Declarations explicites ..
   implicit none

   !.. Arguments ..
   type(CASIER_T) , dimension(:) , pointer       :: Casier
   type(FICHIER_T)               , intent(in   ) :: fichier
   integer                       , intent(in   ) :: Option
   type(ERREUR_T)                , intent(inout) :: Erreur

   !.. Constantes ..
   integer                      , parameter     :: LEN_CHAINE = 80

   !.. Variables locales ..

   ! Constantes sur les mots-cles
   integer                      , parameter     :: NB_MOT_CLE = 3
   character(*), dimension(NB_MOT_CLE)   , parameter :: MOT_CLE   = &
       (/            &
       "CASIER"    , &
       "casier"    , &
       "Casier"      &
       /)
   character(1), parameter              :: CHAINE_COMMENTAIRE  = "#"     ! caractere commentaire qui debute
                                                                         ! une ligne commentaire
   logical                              :: presence_mot_cle              ! Test de presence du mot cle
   integer                              :: ul                            ! Unite logique du fichier
   integer                              :: retour                        ! Code de retour de la fonction read
   integer                              :: icasier                       ! Compteur sur les casiers
   integer                              :: nb_casier                     ! Nombre de casiers lus dans le fichier
   integer                              :: ipoint_front , nb_point_front ! Compteur sur les points frontieres
   integer                              :: ipoint_int , nb_point_int     ! Compteur sur les points interieurs
   integer                              :: ipoint
   integer , dimension(:) , allocatable :: nb_point
   character(30)                        :: nom_casier                    ! Nom du bief
   character(1)                         :: label                         ! Lit auquel le point appartient bathy/topo
   character(LEN_CHAINE)                :: chaine                        ! Chaine contenant une ligne du fichier
   !character(132):: arbredappel_old                                     ! Arbredappel initial
   real(DOUBLE)                         :: X , Y , Z

   !============================ Commentaires ============================
    ! Description du fichier lu :
    !----------------------------
    ! Toute ligne de commentaire est debutee par le caractere "#"
    ! Ligne 1 : mot-cle MOT_CLE, nom_casier.
    !           sans blanc dans les noms : le blanc est le caractere separateur
    !           Ex: <MOT_CLE casier1
    ! Ligne 2 et jusqu'au prochain mot-cle MOT ou la fin de fichier :
    !           (Z, S, V)

   !============================ Instructions ==============================
   Erreur%Numero = 0
   !arbredappel_old = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>LEC_GEOM_CASIER2'

   ! ===============================================================
   ! Premiere lecture du fichier afin de connaitre le nombre de profils
   ! ===============================================================
   ! et de deceler les erreurs
   ! ===============================================================

   ! Ouverture du fichier a lire
   ! ---------------------------
   open( unit = fichier%Unite , file =fichier%Nom , access = 'SEQUENTIAL' , &
         action = 'READ' , form = 'FORMATTED' , iostat=RETOUR , &
         position = 'rewind' , status='OLD' )
   if( RETOUR /= 0 ) then
      Erreur%Numero = 3
      Erreur%ft     = err_3
      Erreur%ft_c   = err_3c
      call TRAITER_ERREUR_CASIER( Erreur , fichier%Nom )
      return
   end if

   ul = fichier%Unite

   call LIRE_CHAINE_S (chaine, Fichier, CHAINE_COMMENTAIRE, RETOUR)

   ! Ligne 1 : mot-cle "MOT_CLE"
   !----------------------------
   icasier = 0

   label_nb_casier : do while( RETOUR == 0 )
      !========================================================
      ! Tant qu'on n'a pas atteint la fin du fichier
      !  ou qu'une erreur de lecture systeme n'est pas apparue
      !========================================================
      call DECODER_GEOM_CASIER ( presence_mot_cle  ,  nom_casier , chaine , MOT_CLE , &
                                 icasier + 1 , Erreur )
      if( Erreur%Numero /=0 ) then
         return
      end if

      ! Ligne 1, le mot-cle MOT_CLE doit etre present

      if( presence_mot_cle ) then
         ! on incremente le nombre de casiers
         icasier = icasier + 1
      else
      !-----------------------------------------------------
      ! Si le mot-cle MOT_CLE n'est pas present sur la ligne
      !-----------------------------------------------------
         if( icasier == 0 ) then
            Erreur%Numero = 78
            Erreur%ft     = err_78
            Erreur%ft_c   = err_78c
            call TRAITER_ERREUR_CASIER( Erreur )
            return
         endif
      end if

      !-----------------------------
      ! On passe a la ligne suivante
      !-----------------------------
      call LIRE_CHAINE_S( chaine , Fichier , CHAINE_COMMENTAIRE , RETOUR )

   end do label_nb_casier

   !====================================
   ! La lecture du fichier est terminee
   !====================================

   if( RETOUR > 0 ) then
   ! Le code de retour de la fonction READ informe d'une erreur
   !==========================================================
      Erreur%Numero = 79
      Erreur%ft     = err_79
      Erreur%ft_c   = err_79c
      call TRAITER_ERREUR_CASIER( Erreur , icasier )
      return
   endif

   ! Sinon c'est la fin du fichier
   !==============================

   nb_casier = icasier
   if( nb_casier /= size( Casier ) ) then
      Erreur%Numero = 81
      Erreur%ft     = err_81
      Erreur%ft_c   = err_81c
      call TRAITER_ERREUR_CASIER( Erreur )
      return
   end if

   allocate( nb_point(nb_casier) )

   ! ===============================================================
   ! 2 eme lecture du fichier pour dimensionner la taille des tableaux
   ! ===============================================================

   ! Retour au debut du fichier
   rewind( ul )

   ! N Ligne de commentaires
   !========================
   call LIRE_CHAINE_S( chaine , Fichier , CHAINE_COMMENTAIRE , RETOUR )
   if( retour /= 0 ) then
      Erreur%Numero = 77
      Erreur%ft     = err_77
      Erreur%ft_c   = err_77c
      call TRAITER_ERREUR_CASIER( Erreur )
      return
   end if

   ! Ligne 1 : mot-cle "MOT_CLE"
   !==========
   icasier = 0

   label_dim_loi : do while( RETOUR == 0 )

      call DECODER_GEOM_CASIER( presence_mot_cle  , nom_casier , chaine , &
                                MOT_CLE , icasier + 1 , Erreur )
      if( Erreur%Numero /=0 ) then
         return
      end if

      !========================================================
      ! Tant qu'on n'a pas atteint la fin du fichier
      !  ou qu'une erreur de lecture systeme n'est pas apparue
      !========================================================

      ! Ligne 1, le mot-cle MOT_CLE doit etre present
      if( presence_mot_cle ) then
         ! on incremente le nombre de casiers
         icasier = icasier + 1
         if( icasier > 1 ) then
            nb_point(icasier - 1) = ipoint
         end if
         ! on remet le compteur des points a zero
         ipoint = 0

      !-------------------------------------------------------------
      ! Si le mot-cle MOT_CLE n'est pas present sur la ligne
      !-------------------------------------------------------------
      else
         ! On incremente l'indice de lecture des points
         ipoint = ipoint + 1
      end if

      !------------------------------
      ! On passe a la ligne suivante
      !------------------------------
      call LIRE_CHAINE_S ( chaine , Fichier , CHAINE_COMMENTAIRE , RETOUR )

   end do label_dim_loi

   !====================================
   ! La lecture du fichier est terminee
   !====================================

   label_retour : if( RETOUR > 0 ) then

      ! Le code de retour de la fonction READ informe d'une erreur
      !============================================================
      Erreur%Numero = 80
      Erreur%ft     = err_80
      Erreur%ft_c   = err_80c
      call TRAITER_ERREUR_CASIER( Erreur , icasier , ipoint )
      return

   else label_retour
      nb_point( icasier ) = ipoint
      ! ===============================================================
      ! 2bis eme lecture du fichier pour dimensionner la taille des tableaux
      ! ===============================================================

      ! Retour au debut du fichier
      rewind( ul )

      ! N Ligne de commentaires
      !========================

      call LIRE_CHAINE_S( chaine , Fichier , CHAINE_COMMENTAIRE , RETOUR )
      if (retour /= 0) then
         Erreur%Numero = 77
         Erreur%ft   = err_77
         Erreur%ft_c = err_77c
         call TRAITER_ERREUR_CASIER ( Erreur )
         return
      end if

      ! Ligne 1 : mot-cle "MOT_CLE"
      !==========

      icasier = 0

      label_dim_loi2 : do while( RETOUR == 0 )

         call DECODER_GEOM_CASIER( presence_mot_cle  , nom_casier , chaine , MOT_CLE , icasier + 1 , Erreur )
         if( Erreur%Numero /=0 ) then
            return
         end if

         !========================================================
         ! Tant qu'on n'a pas atteint la fin du fichier
         !  ou qu'une erreur de lecture systeme n'est pas apparue
         !========================================================

         ! Ligne 1, le mot-cle MOT_CLE doit etre present
         if( presence_mot_cle ) then
            if( icasier > 0 ) then
               if(.not.associated(Casier(icasier)%PointFrontiere)) &
                      allocate( Casier(icasier)%PointFrontiere(ipoint_front,3) , STAT=retour )
               if( retour /= 0 ) then
                  Erreur%Numero = 5
                  Erreur%ft     = err_5
                  Erreur%ft_c   = err_5c
                  call TRAITER_ERREUR_CASIER( Erreur , 'Casier/PointFront' )
                  return
               end if

               if(.not.associated(Casier(icasier)%PointInterieur)) &
                         allocate( Casier(icasier)%PointInterieur(ipoint_int,3) , STAT=retour )
               if( retour /= 0 ) then
                  Erreur%Numero = 5
                  Erreur%ft     = err_5
                  Erreur%ft_c   = err_5c
                  call TRAITER_ERREUR_CASIER( Erreur , 'Casier/PointInt' )
                  return
               end if
            end if
            ! on incremente le nombre de casiers
            icasier      = icasier + 1
            ! on remet le compteur des abscisses a zero
            ipoint_front = 0
            ipoint_int   = 0

            !-------------------------------------------------------------
            ! Si le mot-cle MOT_CLE n'est pas present sur la ligne
            !-------------------------------------------------------------
         else
            ! On incremente l'indice de lecture des abscisses
            if( label == 'F' ) then
               ipoint_front = ipoint_front + 1
            else
               ipoint_int = ipoint_int + 1
            end if
         end if

         !------------------------------
         ! On passe a la ligne suivante
         !------------------------------
         call LIRE_CHAINE_S( chaine , Fichier , CHAINE_COMMENTAIRE , RETOUR )
         if( ( ipoint_front + ipoint_int ) < nb_point( icasier ) ) then
            read( chaine , * , iostat = RETOUR )   X , Y , Z , label
         end if
      end do label_dim_loi2

      !====================================
      ! La lecture du fichier est terminee
      !====================================

      label_retour2 : if( RETOUR > 0 ) then

         ! Le code de retour de la fonction READ informe d'une erreur
         !============================================================
         Erreur%Numero = 80
         Erreur%ft     = err_80
         Erreur%ft_c   = err_80c
         call TRAITER_ERREUR_CASIER( Erreur , icasier , ipoint )
         return
      else label_retour2
         ! Le code de retour de la fonction READ informe de la fin du fichier
         !===================================================================

         ! On stocke le nombre de points lus pour le dernier mot-cle.
         ! Et on alloue les tableaux PointFront et PointInt

         if(.not.associated(Casier(icasier)%PointFrontiere)) &
                   allocate( Casier(icasier)%PointFrontiere(ipoint_front,3) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR_CASIER( Erreur , 'Casier/PointFront' )
            return
         end if

         if(.not.associated(Casier(icasier)%PointInterieur)) &
                    allocate( Casier(icasier)%PointInterieur(ipoint_int,3) , STAT=retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR_CASIER( Erreur , 'Casier/PointInt' )
            return
         end if

         ! ===============================================================
         !             3 eme lecture - Lecture effective des lois
         ! ===============================================================

         ! Retour au debut du fichier
         rewind( ul )

         ! N Ligne de commentaires
         !========================
         call LIRE_CHAINE_S( chaine , Fichier , CHAINE_COMMENTAIRE , RETOUR )
         if( retour /= 0 ) then
            Erreur%Numero = 77
            Erreur%ft     = err_77
            Erreur%ft_c   = err_77c
            call TRAITER_ERREUR_CASIER( Erreur )
            return
         end if

         ! Ligne 1 : mot-cle "MOT_CLE"
         !==========
         boucle_casier : do icasier = 1 , nb_casier
            nb_point_front = size( Casier(icasier)%PointFrontiere(:,3) )
            if( nb_point_front < 4 ) then
               Erreur%Numero = 2000
               Erreur%ft     = err_2000
               Erreur%ft_c   = err_2000c
               call TRAITER_ERREUR_CASIER( Erreur , icasier )
               return
            end if

            nb_point_int = size( Casier(icasier)%PointInterieur(:,3) )
            if( nb_point_int == 0 ) then
               Erreur%Numero = 2001
               Erreur%ft     = err_2001
               Erreur%ft_c   = err_2001c
               call TRAITER_ERREUR_CASIER( Erreur , icasier )
               return
            end if

            !------------------------------------------------
            ! Si ce n'est pas le premier casier, il faut lire
            ! la ligne d'en-tete
            !------------------------------------------------
            test_casier1 : if( icasier /= 1 ) then
               ! Lecture d'un mot-cle et d'un numero
               call LIRE_CHAINE_S( chaine , Fichier , CHAINE_COMMENTAIRE , RETOUR )
               if( retour /= 0 ) then
                  Erreur%Numero = 77
                  Erreur%ft   = err_77
                  Erreur%ft_c = err_77c
                  call TRAITER_ERREUR_CASIER( Erreur )
                  return
               end if
            end if test_casier1

            ! Lecture des points de la loi du casier icasier
            nb_point(icasier)   = nb_point_front + nb_point_int
            ipoint_front        = 0
            ipoint_int          = 0

            boucle_points : do ipoint = 1 , nb_point(icasier)

               call LIRE_CHAINE_S( chaine , Fichier , CHAINE_COMMENTAIRE , RETOUR )
               read( chaine , * , iostat = RETOUR ) X , Y , Z , label
               if( RETOUR /= 0 ) then
                  Erreur%Numero = 80
                  Erreur%ft     = err_80
                  Erreur%ft_c   = err_80c
                  call TRAITER_ERREUR_CASIER( Erreur , icasier , ipoint )
                  return
               end if

               if (label == 'F') then
                  ipoint_front = ipoint_front + 1
                  Casier(icasier)%PointFrontiere(ipoint_front,1) = X
                  Casier(icasier)%PointFrontiere(ipoint_front,2) = Y
                  Casier(icasier)%PointFrontiere(ipoint_front,3) = Z
               else
                  ipoint_int = ipoint_int + 1
                  Casier(icasier)%PointInterieur(ipoint_int,1) = X
                  Casier(icasier)%PointInterieur(ipoint_int,2) = Y
                  Casier(icasier)%PointInterieur(ipoint_int,3) = Z
               end if
            end do boucle_points
         end do boucle_casier
      end if label_retour2
   end if label_retour

   ! Planimetrage du casier
   ! ----------------------
   do icasier = 1 , nb_casier
      call PLANIM_CASIER( Casier(icasier) , icasier , Option , Erreur )
      if( Erreur%Numero /= 0 ) then
         return
      end if
   end do

   !----------------------
   ! Fin du sous-programme
   !----------------------
   close(ul)

   deallocate(nb_point)
   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine LEC_GEOM_CASIER2
