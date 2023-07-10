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

subroutine      MAILLER ( &
         X              , & ! maillage
         maille_r       , & ! mailles reelles
         maille_e       , & ! mailles entieres
         Profil         , & ! tableau des profils
         TypeMaillage   , & ! type de calcul du maillage
         impression_geo , & ! test d'impression de la geometrie
         UniteListing   , & ! Unite logique fichier listing
         Erreur           & ! Erreur
                     )

! *********************************************************************
! PROGICIEL : MASCARET       D. ROUGE
!                            S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
! *********************************************************************
!
!  FONCTION :
!  --------
!
!          DEFINITION DES ABSCISSES DES SECTIONS DE CALCUL
!          SUR L'ENSEMBLE DU FICHIER.
!
!----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ----------------------
!                FichMaillage  : Fichier de definition du maillage
!                UniteListing  : Fichier listing
!
!   SOUS PROGRAMME APPELANT :
!   -------------------------
!   SOUS PROGRAMMES APPELES :
!   -------------------------
!   COMMENTAIRES :
!   ------------
!
!           DEFINITION DES ABSCISSES DES SECTIONS DE CALCUL
!
!   **  TypeMaillage = 1 **   LES SECTIONS SONT LES PROFILS DE DONNEES
!
!
!   **  TypeMaillage = 2 **   LES SECTIONS SONT DEFINIES PAR SERIES
!
!                         XD      XF
!           !     !        !       !
!       ----!-----!--:--:--!-:-:-:-!---!------------!------
!  -->      !     !  :  :  ! : : : !
!  -->      !     !  :  :  ! : : : !
!       ----!-----!--:--:--!-:-:-:-!---!------------!------
!       IPX !  0  !    2   !  3    ! 0 !     0
!
!
!         XD   ABSCISSE DE DEPART D'UNE SERIE
!         XF   ABSCISSE DE FIN D'UNE SERIE
!        IPX   NOMBRE DE SECTIONS INTERMEDIAIRES
!
!   **  TypeMaillage = 3 **   LES SECTIONS SONT DEFINIES UNE A UNE
!
!
!   **  TypeMaillage = 4 **   LES SECTIONS ONT ETE DEFINIES DANS LE CALCUL
!                       PERMANENT ET SERONT LUES DANS LE FICHIER
!                       CONTENANT LA LIGNE D'EAU INITIALE
!                       ( SOUS-PROGRAMME LEC_LIGNE )
!   **  TypeMaillage = 5 **   LES SECTIONS SONT DEFINIES PAR SERIE.
!                       ON DONNE ENTRE 2 PROFILS, LA TAILLE MAXIMALE
!                       D UNE MAILLE
!***********************************************************************

   !=========================== Declarations ================================
   use M_PRECISION
   use M_PARAMETRE_C
   use M_CONSTANTES_CALCUL_C ! Type de maillage
   use M_PROFIL_T            ! Definition du type PROFIL_T
   use M_MAILLE_T            ! Definition du type MAILLE_R_T
                             !                 et MAILLE_E_T
   use M_ERREUR_T            ! Type ERREUR_T
   use M_MESSAGE_C           ! Messages d'erreur
   use M_TRAITER_ERREUR_I    ! Interface de traitement des erreurs

   !.. Implicit Declarations ..
   implicit none

   !.. Formal Arguments ..
   real(DOUBLE), dimension(:), pointer :: X
   type(MAILLE_R_T), dimension(:), intent(in   ) :: Maille_r
   type(MAILLE_E_T), dimension(:), intent(in   ) :: Maille_e
   type(PROFIL_T)  , dimension(:), intent(in   ) :: Profil
   logical                       , intent(in   ) :: impression_geo
   integer                       , intent(in   ) :: UniteListing
   integer                       , intent(in   ) :: TypeMaillage
   type(ERREUR_T)            , intent(inout) :: Erreur
   !.. Local Scalars ..
   integer        :: nb_profil       ! nombre de profils
   integer        :: I, J,NB
   real(DOUBLE)   :: xd, xf          ! abscisses de debut et fin de maille
   integer        :: nb_sect_maille  ! nombre de sections entre 2 profils
   real(DOUBLE)   :: dx              ! ecart entre 2 sections
   integer        :: prof_deb        ! Profil de debut de maille
   integer        :: prof_fin        ! Profil de fin de maille
   real(DOUBLE)   :: pas             ! Pas approximatif entre 2 sections
   integer        :: imai            ! compteur sur les mailles
   integer        :: isec            ! compteur sur les sections de calcul
   real(DOUBLE)   :: distance        ! distance
   !character(132) :: !arbredappel_old ! ancien arbre d'appel
   integer        :: retour          ! code retour d'une fonction intrinseque
   integer        :: nb_sect         ! nombre de sections de calcul
   real(DOUBLE)   :: x_isec          ! abscisse de la section isec
   real(DOUBLE)   :: x_isec_old      ! abscisse de la section isec -1

   !.. External Calls ..

   !.. Intrinsic Functions ..
   intrinsic ABS, INT

   !========================== Instructions =============================
   ! INITIALISATIONS
   ! ---------------
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>MAILLER'
   Erreur%Numero = 0
   retour        = 0
   nb_profil = size(Profil)

   if( impression_geo ) then
      write(UniteListing,10000)
   end if

   if( TypeMaillage < 1 .or. Typemaillage > 5 ) then
      Erreur%Numero = 30
      call TRAITER_ERREUR( Erreur , TypeMaillage )
      return
   end if

   select case( TypeMaillage )
      ! LES SECTIONS DE CALCUL CORRESPONDENT AUX PROFILS DU BIEF
      ! --------------------------------------------------------
      case( TYPE_MAILLAGE_PROFIL )

         if(.not.associated(X)) allocate( X(nb_profil) , stat = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'Abscisses' )
            return
         end if

         X(:) = Profil(:)%AbsAbs

      ! LES SECTIONS DE CALCUL SONT DEFINIES PAR SERIES
      ! -----------------------------------------------
      case(TYPE_MAILLAGE_SERIE)
         !-----------------------------------------
         ! Etape 1 : comptage du nombre de sections
         !-----------------------------------------
         isec   = 0
         x_isec = 0._DOUBLE   ! valeur arbitraire mais elle n'est pas utilisee

         do imai = 1 , size(Maille_r)
            xd             = Maille_r(imai)%AbscisseDeb
            xf             = Maille_r(imai)%AbscisseFin
            nb_sect_maille = Maille_r(imai)%NbSection
            if( xf < xd .or. nb_sect_maille < 0 ) then
               Erreur%Numero = 28
               Erreur%ft     = err_28
               Erreur%ft_c   = err_28c
               call TRAITER_ERREUR( Erreur , xd , xf , nb_sect_maille )
               return
            end if

            dx = ( xf - xd ) / ( nb_sect_maille + 1 )

            do i = 1 , nb_sect_maille + 2
               isec = isec + 1

               ! Calcul de l'abscisse du point isec apres stockage
               ! du precedent
               x_isec_old = x_isec
               x_isec     = xd + dx*(i-1)

               ! fusion des sections corespondants a des profils extremites
               ! de fin et de debut de branches
               if( isec > 1 ) then
                  if( DABS( x_isec - x_isec_old ) < EPS3 ) then
                     isec = isec - 1
                  endif
               end if
            end do
         end do

         nb_sect = isec

         !------------------------------------
         ! Etape 2 : allocation et calcul de X
         !------------------------------------
         if(.not.associated(X)) allocate( X(nb_sect) , stat = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'Abscisses' )
            return
         end if

         isec = 0
         do imai = 1 , size(Maille_r)
            xd             = Maille_r(imai)%AbscisseDeb
            xf             = Maille_r(imai)%AbscisseFin
            nb_sect_maille = Maille_r(imai)%NbSection
            dx             = ( xf - xd ) / ( nb_sect_maille + 1 )
            do i = 1 , nb_sect_maille + 2
               isec    = isec + 1
               X(isec) = xd + dx * ( i - 1 )
               ! fusion des sections corespondants a des profils extremites
               ! de fin et de debut de branches
               if( isec > 1 ) then
                  if( DABS( x(isec) - x(isec-1) ) < EPS3 ) then
                     isec = isec - 1
                  end if
               end if
            end do
         end do

         ! LES SECTIONS DE CALCUL SONT DEFINIES UNE A UNE
         ! ----------------------------------------------
         case( TYPE_MAILLAGE_INDIVIDUELLE )

            nb_sect = size (X)
            ! le tableau des abscisses est directement lu via le preprocesseur
            ! LES SECTIONS ONT ETE DEFINIES DANS LE CALCUL PERMANENT ET SERONT
            ! LUES DANS LE FICHIER CONTENANT LA LIGNE D'EAU INITIALE
            ! ----------------------------------------------------------------
         case( TYPE_MAILLAGE_PRECEDENT )

         ! LES SECTIONS SONT ETE DEFINIES PAR SERIES EN INCLUANT TOUS LES PROFILS
         ! ----------------------------------------------------------------------
         case( TYPE_MAILLAGE_SERIE_PROFIL )
            !---------------------------------------
            ! Etape 1 : calcul du nombre de sections
            !---------------------------------------
            isec   = 0
            x_isec = 0._DOUBLE   ! valeur arbitraire mais elle n'est pas utilisee

            do imai = 1 , size(Maille_e)
               prof_deb = Maille_e(imai)%ProfilDeb
               prof_fin = Maille_e(imai)%ProfilFin
               pas      = Maille_e(imai)%Pas

               if( Profil(prof_deb)%AbsAbs > Profil(prof_fin)%AbsAbs .or. pas < 0 ) then
                  Erreur%Numero = 29
                  Erreur%ft     = err_29
                  Erreur%ft_c   = err_29c
                  call TRAITER_ERREUR( Erreur , prof_deb , prof_fin , pas )
                  return
               end if

               do i = prof_deb , prof_fin - 1

                  isec = isec + 1

                  ! Calcul de l'abscisse du point isec apres stockage
                  ! du precedent
                  x_isec_old = x_isec
                  x_isec = Profil(i)%AbsAbs

                  ! controle de double section dans le cas ou l'on change de zone de
                  ! discretisation et que les numeros de profils de fon de zone et
                  ! de debut de zone sont les memes
                  if( isec > 1 ) then
                     if( DABS( x_isec - x_isec_old ) < EPS3 ) then
                        isec = isec - 1
                     endif
                  end if

                  distance = Profil(i+1)%AbsAbs - Profil(i)%AbsAbs
                  nb = int(abs(distance)/pas)
                  if( nb > 1 ) then
                     dx = distance / nb
                     do j = 2 , nb
                        isec = isec + 1
                        ! Calcul de l'abscisse du point isec apres stockage
                        ! du precedent
                        x_isec_old = x_isec
                        x_isec     = x_isec_old + dx
                     end do
                  endif
               end do
               Isec   = Isec +1
               x_isec = Profil(prof_fin)%AbsAbs

               ! fusion des sections corespondants a des profils extremites
               ! de fin et de debut de branches
               if( isec > 1 ) then
                  if( DABS( x_isec - x_isec_old ) < EPS3 ) then
                     isec = isec - 1
                  endif
               end if
            end do

         nb_sect = isec

         !------------------------------------
         ! Etape 2 : allocation et calcul de X
         !------------------------------------
         if(.not.associated(X)) allocate( X(nb_sect) , stat = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'Abscisses' )
            return
         end if

         X(:) = 0
         isec = 0

         do imai = 1 , size(Maille_e)
            prof_deb = Maille_e(imai)%ProfilDeb
            prof_fin = Maille_e(imai)%ProfilFin
            pas      = Maille_e(imai)%Pas
            do i = prof_deb , prof_fin - 1
               isec    = isec + 1
               X(isec) = Profil(i)%AbsAbs
               ! controle de double section dans le cas ou l'on change de zone de
               ! discretisation et que les numeros de profils de fon de zone et
               ! de debut de zone sont les memes
               if( isec > 1 ) then
                  if( ABS( X(isec) - X(isec-1) ) < EPS3 ) then
                     isec = isec - 1
                  endif
               end if

               distance = Profil(i+1)%AbsAbs - Profil(i)%AbsAbs
               nb       = int(abs(distance)/pas)
               if( nb > 1 ) then
                  dx = distance / nb
                  do j = 2 , nb
                     isec    = isec + 1
                     X(isec) = X(isec - 1) + dx
                  end do
               endif
            end do
            X(isec+1) = Profil(prof_fin)%AbsAbs
            isec      = isec+1

            ! fusion des sections corespondants a des profils extremites
            ! de fin et de debut de branches
            if( isec > 1 ) then
               if( ABS(X(isec) - X(isec-1) ) < EPS3 ) then
                  isec = isec - 1
               endif
            end if
         end do
   end select

   ! ECRITURE DES ABSCISSES DES SECTIONS DE CALCUL
   if( (impression_geo) .and. (TypeMaillage /= TYPE_MAILLAGE_PRECEDENT) ) then
      write (UniteListing,10005) nb_sect
      write (UniteListing,10006)
      write (UniteListing,10007) (X(I), I = 1,nb_sect)
   end if

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old
   return

   ! ... Format Declarations ...
   ! FORMATS D'ECRITURE
  10000 format (/,'DEFINITION DES SECTIONS DE CALCUL',/,33('-'))
  10005 format (/,'Nombre de sections de calcul : ',i5)
  10006 format (/,'Abscisses des sections de calcul :',/)
  10007 format (2x,5f12.2)

end subroutine MAILLER
