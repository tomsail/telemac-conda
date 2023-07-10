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

subroutine LEC_METEO( &
                      ! Resultats
              Meteo , & ! loi hydraulique
                      ! Donnees
      Fichier_Meteo , & ! Fichier contenant les donnees
    Modele_Qual_Eau , & ! Type du modele de qualite d'eau
             Erreur   & ! Erreur
                    )

!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN - N.GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
! ......................................................................
!
!
!  FONCTION : LECTURE DES FICHIERS DE DONNEES METEO
!  --------
!
!-----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :        - Fichier meteo
!
!   SOUS-PROGRAMME(S) APPELANT(S) : - PRETRAIT
!   -----------------------------
!   SOUS-PROGRAMME(S) APPELE(S)   : - LIRE_CHAINE_S
!   ---------------------------
!   COMMENTAIRES :
!   ------------
!
!   DOCUMENTATION EXTERNE :
!   ---------------------
!
!***********************************************************************

   !============================= Declarations ============================
   use M_PRECISION
   use M_METEO_T             ! Definition du type LOI_T
   use M_ERREUR_T            ! Definition du type ERREUR_T
   use M_FICHIER_T           ! Definition du type FICHIER_T
   use M_MESSAGE_C           ! Definition des messages d'erreur
   use M_TRAITER_ERREUR_I    ! Inteface generique de gestion des erreurs
   use M_LIRE_CHAINE_S       ! Lecture des lignes commentaires
   use M_CONSTANTES_CALCUL_TRACER_C

   !.. Declarations explicites ..
   implicit none

   !.. Arguments ..
   type(METEO_T)   , intent(inout) :: meteo
   type(FICHIER_T) , intent(in   ) :: fichier_meteo
   type(ERREUR_T)  , intent(inout) :: Erreur
   integer         , intent(in   ) :: Modele_Qual_Eau
   !.. Constantes ..
   ! Chaine caractere pour identifier les lignes de commentaire
   ! dans l'en-tete du fichier
   character(1) , parameter :: CHAINE_COMMENTAIRE = '#'
   integer      , parameter :: LEN_CHAINE = 80
   !.. Variables locales ..
   integer               :: ul                 ! Unite logique du fichier
   integer               :: retour             ! Code de retour de la fonction read
   integer               :: ipoint             ! Compteur sur les points d'une loi
   integer               :: indice_abscisse    ! Index des valeurs d'abscisse
   integer               :: nb_point           ! Nombre d'abscisses dans une loi
   character(LEN_CHAINE) :: chaine     ! Chaine contenant une ligne du fichier
   integer               :: rang
   character(LEN=1)      :: chaine_unite_temps

   !============================ Commentaires ============================
   ! Description du fichier lu :
   !----------------------------
   ! Toute ligne de commentaire commence par le caractere "#"
   !
   !    Exemple :
   !
   !    #ESSAI
   !    #18/12/1995
   !    # donnees meteo
   !    # temps    Teau    I0
   !    ...
   !============================ Instructions ==============================

   Erreur%Numero      = 0
   !arbredappel_old    = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>LEC_METEO'

   ! Ouverture du fichier a lire
   ! ---------------------------
   open( unit = fichier_meteo%Unite , file = fichier_meteo%Nom , access = 'SEQUENTIAL' , &
         action = 'READ' , form = 'FORMATTED' , iostat = RETOUR , &
         position = 'rewind' , status = 'OLD' )
   if( RETOUR /= 0 ) then
      Erreur%Numero = 3
      Erreur%ft     = err_3
      Erreur%ft_c   = err_3c
      call TRAITER_ERREUR( Erreur , fichier_meteo%Nom )
      return
   end if

   ul = fichier_meteo%Unite

   ! =================================================================
   ! 1 ere lecture du fichier pour dimensionner la taille des tableaux
   ! =================================================================

   ! chronique temporelle => verification que la premiere
   ! ligne non commentaire contient l'unite de temps
   call LIRE_CHAINE_S( chaine , Fichier_Meteo , CHAINE_COMMENTAIRE , retour )
   if( RETOUR /= 0 ) then
      Erreur%Numero = 380
      Erreur%ft     = err_380
      Erreur%ft_c   = err_380c
      call TRAITER_ERREUR( Erreur , Fichier_meteo%Nom )
      return
   end if

   rang = scan( chaine , 'SsMmHhJj' )
   if( rang == 0 ) then
      Erreur%Numero = 379
      Erreur%ft     = err_379
      Erreur%ft_c   = err_379c
      call TRAITER_ERREUR( Erreur , fichier_meteo%Nom )
      return
   endif

   indice_abscisse = 0

   label_dimension_donnees : do while( RETOUR == 0 )
      indice_abscisse = indice_abscisse + 1
      call LIRE_CHAINE_S (chaine, Fichier_meteo, CHAINE_COMMENTAIRE, retour)
   end do label_dimension_donnees

   !====================================
   ! La lecture du fichier est terminee
   !====================================
   label_retour : if( RETOUR > 0 ) then
      ! Le code de retour de la fonction READ informe d'une erreur
      !============================================================
      Erreur%Numero = 11
      Erreur%ft     = err_11
      Erreur%ft_c   = err_11c
      call TRAITER_ERREUR( Erreur , fichier_meteo%Nom , indice_abscisse )
      return
   else label_retour
      ! Le code de retour de la fonction READ informe de la fin du fichier
      !===================================================================
      ! on stocke le nombre d'abscisses lues
      nb_point = indice_abscisse - 1
      ! Allocations
      !------------
      select case( Modele_Qual_Eau )

         case( MODELE_BIOMASS )

            allocate( Meteo%Temps(nb_point) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Meteo%Temps' )
               return
            end if

            allocate( Meteo%Temp( nb_point ) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Meteo%Temp' )
               return
            end if

            allocate( Meteo%I0( nb_point ) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Meteo%I0' )
               return
            end if

         case( MODELE_EUTRO )

            allocate( Meteo%Temps(nb_point) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Meteo%Temps' )
               return
            end if

            allocate( Meteo%Temp(nb_point) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Meteo%Temp' )
               return
            end if

            allocate( Meteo%I0(nb_point) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Meteo%I0' )
               return
            end if

         case( MODELE_THERMIC )

            allocate( Meteo%Temps(nb_point) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Meteo%Temps' )
               return
            end if

            allocate( Meteo%T_air(nb_point) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Meteo%T_air' )
               return
            end if

            allocate( Meteo%P_vap(nb_point) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Meteo%P_vap' )
               return
            end if

            allocate( Meteo%Vit_vent(nb_point) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Meteo%Vit_vent' )
               return
            end if

            allocate( Meteo%Nebulo(nb_point) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Meteo%Nebulo' )
               return
            end if

            allocate( Meteo%Ray3(nb_point) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Meteo%Ray3' )
               return
            end if

            allocate( Meteo%P_atm(nb_point) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Meteo%P_atm' )
               return
            end if
      end select

      ! ===============================================================
      !             2eme lecture - Lecture effective des lois
      ! ===============================================================

      ! Retour au debut du fichier
      rewind (ul)

      ! Lecture de l'unite de temps
      call LIRE_CHAINE_S( chaine , Fichier_Meteo , CHAINE_COMMENTAIRE , retour )
      read( chaine(rang:rang+1) , * , IOSTAT = retour ) chaine_unite_temps
      if( RETOUR /= 0 ) then
         Erreur%Numero = 380
         Erreur%ft     = err_380
         Erreur%ft_c   = err_380c
         call TRAITER_ERREUR( Erreur , Fichier_meteo%Nom )
         return
      end if

      ! Lecture des donnees
      select case( Modele_Qual_Eau )

         case( MODELE_BIOMASS )

            do ipoint = 1 , size(Meteo%Temps)

               call LIRE_CHAINE_S (chaine, Fichier_meteo, CHAINE_COMMENTAIRE, retour)
               read(chaine,*,IOSTAT = retour) Meteo%Temps(ipoint) , Meteo%Temp(ipoint) , Meteo%I0(ipoint)

               if( RETOUR /= 0 ) then
                  Erreur%Numero = 11
                  Erreur%ft     = err_11
                  Erreur%ft_c   = err_11c
                  call TRAITER_ERREUR( Erreur , Fichier_meteo%Nom , ipoint )
                  return
               end if

            end do

         case( MODELE_EUTRO )

            do ipoint = 1 , size(Meteo%Temps)

               call LIRE_CHAINE_S( chaine , Fichier_meteo , CHAINE_COMMENTAIRE , retour )
               read( chaine , * , IOSTAT = retour ) Meteo%Temps(ipoint) , Meteo%Temp(ipoint) , Meteo%I0(ipoint)

               if( RETOUR /= 0 ) then
                  Erreur%Numero = 11
                  Erreur%ft     = err_11
                  Erreur%ft_c   = err_11c
                  call TRAITER_ERREUR( Erreur , Fichier_meteo%Nom , ipoint )
                  return
               end if

            end do

         case( MODELE_THERMIC )

            do ipoint = 1, size(Meteo%Temps)

               call LIRE_CHAINE_S( chaine , Fichier_meteo , CHAINE_COMMENTAIRE , retour )
               read( chaine , * , IOSTAT = retour ) Meteo%Temps(ipoint) , Meteo%T_air(ipoint) , &
                                                    Meteo%P_vap(ipoint) , Meteo%Vit_vent(ipoint) , Meteo%Nebulo(ipoint) , &
                                                    Meteo%Ray3(ipoint) , Meteo%P_atm(ipoint)

               if( RETOUR /= 0 ) then
                  Erreur%Numero = 11
                  Erreur%ft     = err_11
                  Erreur%ft_c   = err_11c
                  call TRAITER_ERREUR( Erreur , Fichier_meteo%Nom , ipoint )
                  return
               end if

            end do

      end select

      select case( chaine_unite_temps )

         ! passage du temps en seconde
         case( "M" , "m" )
            do ipoint = 1 , nb_point
               Meteo%Temps(ipoint) = Meteo%Temps(ipoint) * 60.D0
            enddo
         case( "H" , "h" )
            do ipoint = 1 , nb_point
               Meteo%Temps(ipoint) = Meteo%Temps(ipoint) * 3600.D0
            enddo
         case( "J" , "j" )
            do ipoint = 1 , nb_point
               Meteo%Temps(ipoint) = Meteo%Temps(ipoint) * 86400.D0
            enddo

      end select

   endif label_retour

   close(ul)

   !Erreur%arbredappel = arbredappel_old

   return

end subroutine LEC_METEO
