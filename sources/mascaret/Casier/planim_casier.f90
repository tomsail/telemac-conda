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

subroutine PLANIM_CASIER( Casier , Icasier , Option , Erreur )

! ******************************************************************
! PROGICIEL : MASCARET                     C. RISSOAN
!
! VERSION : V8P4R0                  EDF-CEREMA
!
! PLANIMETRAGE AUTOMATIQUE DE LA VARIABLE CASIER
! ******************************************************************
!
!   FICHIERS ENTREE/SORTIE :  --
!   ----------------------
!   SOUS PROGRAMMES APPELANTS : - GEO_CASIER
!   ---------------------------
!   SOUS PROGRAMMES APPELES :    --
!   -------------------------

!========================== Declarations ==============================
   use M_PRECISION
   use M_CASIER_T                  ! type Casier
   use M_ERREUR_T                  ! type Erreur
   use M_MESSAGE_CASIER_C          ! messages d erreur propres a Casier
   use M_CONSTANTES_CASIER_C       ! constantes propres a casier
   use M_PARAMETRE_C               ! constantes numeriques
   use M_TRAITER_ERREUR_CASIER_I   ! traitement des erreurs

   implicit none

   !.. Arguments ..
   type(CASIER_T) , intent(inout) :: Casier
   type(ERREUR_T) , intent(inout) :: Erreur
   integer ,        intent(in   ) :: Option
   integer ,        intent(in   ) :: Icasier
   !.. Variables locales ..
   integer :: retour                  ! code de retour des fonctions intrinseques
   integer :: nb_point_interieur , &  ! nombre de points interieurs au Casier
              nb_point_frontiere , &  ! nombre de points frontiere du casier
              iinterieur         , &  ! compteur sur le nombre de points interieurs
              compteur           , &  ! compteur quelconque
              stock              , &  ! variable de stockage
              icote                   ! compteur sur le nombre de cotes de planimetrage
   integer , dimension(:) , allocatable :: rangement_cote ! rangement des cotes par ordre croissant
   !character(132) :: !arbredappel_old
   real(DOUBLE) :: surface_max        , &  ! surface maximale du casier
                   surface            , &  ! variable surface de stockage
                   volume             , &  ! variable volume de stockage
                   Zcourant           , &  ! cote du pas courant
                   compteur_precedent , &  ! reel correspondant au compteur - 1
                   compteur_actuel    , &  ! reel correspondant au compteur
                   Z1                 , &  ! cote du point interieur au rang actuel - 1
                   Z2                 , &  ! cote du point interieur au rang actuel
                   alpha              , &
                   surface1           , &
                   surface2

!========================== Instructions ==============================

   ! INITIALISATIONS
   ! ---------------
   Erreur%Numero = 0
   retour        = 0
   !arbredappel_old = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>PLANIM_CASIER'

   ! Verification de la fermeture du Casier
   ! --------------------------------------
   nb_point_frontiere = size( Casier%PointFrontiere(:,3) )
   if( ( abs(Casier%PointFrontiere(nb_point_frontiere,1)-Casier%PointFrontiere(nb_point_frontiere,1)).GT.EPS6 ) .or. &
       ( abs(Casier%PointFrontiere(nb_point_frontiere,2)-Casier%PointFrontiere(nb_point_frontiere,2)).GT.EPS6 ) .or. &
       ( abs(Casier%PointFrontiere(nb_point_frontiere,3)-Casier%PointFrontiere(nb_point_frontiere,3)).GT.EPS6 ) ) then
      Erreur%Numero = 2007
      Erreur%ft     = err_2007
      Erreur%ft_c   = err_2007c
      call TRAITER_ERREUR_CASIER  (Erreur, Icasier)
      return
   end if

   ! Rangement des cotes des points interieur en valeur croissante
   ! -------------------------------------------------------------
   nb_point_interieur = size( Casier%PointInterieur(:,3) )
   allocate( rangement_cote(nb_point_interieur) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR_CASIER( Erreur , 'Loi(Z,V)' )
      return
   end if

   do iinterieur = 1 , nb_point_interieur
      rangement_cote(iinterieur) = iinterieur
   end do

   if( nb_point_interieur > 2 ) then
      do iinterieur = 2 , nb_point_interieur
         do compteur = iinterieur , 2 , -1
            if( Casier%PointInterieur(rangement_cote(compteur),3) < Casier%PointInterieur(rangement_cote(compteur -1),3) ) then
               stock                        = rangement_cote(compteur)
               rangement_cote(compteur)     = rangement_cote(compteur - 1)
               rangement_cote(compteur - 1) = stock
            end if
         end do
      end do
   end if

   ! Calcul des parametres generaux
   ! ------------------------------
   Casier%CoteFond = Casier%PointInterieur(rangement_cote(1),3)
   surface_max     = DABS( AIRE(Casier%PointFrontiere(:,1) , Casier%PointFrontiere(:,2) , size( Casier%PointFrontiere(:,3) ) ) )

   ! planimetrage
   ! ------------
   select case( Option )
      case( SURFACE_CONSTANTE )
      ! geometrie elementaire,la surface su casier est independante de la cote
      do icote = 1 , Casier%NbCotePlanim
         Casier%Loi_Z_S(icote,2) = surface_max
         Casier%Loi_Z_V(icote,2) = surface_max * Casier%PasPlanim * real( icote - 1 )
         if( icote /= 1 ) then
            Casier%Loi_Z_S(icote,1) = real( icote - 1 ) * Casier%PasPlanim + Casier%CoteFond
            Casier%Loi_Z_V(icote,1) = Casier%Loi_Z_S(icote,1)
         else
            Casier%Loi_Z_S(1,1) = Casier%CoteFond
            Casier%Loi_Z_V(1,1) = Casier%CoteFond
         end if
      end do
      case(SURFACE_DEPEND_COTE)
      ! geometrie definie a partir de la donnee des cotes des points interieurs
      ! la methode est la plus simple envisageable : a une cote donnee, la surface
      ! est supposee proportionnelle au nombre de points interieurs de cote
      ! inferieure ou egale a celle-ci
         compteur = 1
         surface  = 0._DOUBLE
         volume   = 0._DOUBLE
         do icote = 1 , Casier%NbCotePlanim
            Zcourant = Casier%CoteFond + real( icote - 1 ) * Casier%PasPlanim
            if( Casier%PointInterieur(rangement_cote(1),3) > Zcourant) then
               Casier%Loi_Z_S(icote,2) = (1._DOUBLE/dble( nb_point_interieur ) ) * surface_max
               Casier%Loi_Z_V(icote,2) = 0._DOUBLE
            else if( Casier%PointInterieur(rangement_cote(nb_point_interieur),3) <= Zcourant ) then
               Casier%Loi_Z_S(icote,2) = surface_max
               if( surface > 0._DOUBLE ) then
                  Casier%Loi_Z_V(icote,2) = volume + Casier%PasPlanim * ((surface + Casier%Loi_Z_S(icote,2))/2._DOUBLE)
               else
                  Casier%Loi_Z_V(icote,2) = 0._DOUBLE
               end if
            else
               compteur = compteur + 1
               do while( Casier%PointInterieur(rangement_cote(compteur),3) <= Zcourant )
                  compteur = compteur + 1
               end do
               compteur_precedent      = float( compteur - 1 )
               compteur_actuel         = float( compteur )
               Z1                      = Casier%PointInterieur(rangement_cote(compteur - 1),3)
               Z2                      = Casier%PointInterieur(rangement_cote(compteur),3)
               surface1                = ( compteur_precedent / dble(nb_point_interieur) ) * surface_max
               surface2                = ( compteur_actuel / dble(nb_point_interieur) ) * surface_max
               alpha                   = ( Zcourant - Z1 ) / ( Z2 - Z1 )
               Casier%Loi_Z_S(icote,2) = ( 1. - alpha ) * surface1 + alpha * surface2
               if( surface > 0. ) then
                  Casier%Loi_Z_V(icote,2) = volume + Casier%PasPlanim * ( ( surface + Casier%Loi_Z_S(icote,2) ) / 2._DOUBLE )
               else
                  Casier%Loi_Z_V(icote,2) = 0._DOUBLE
               end if
            end if
            surface  = Casier%Loi_Z_S(icote,2)
            volume   = Casier%Loi_Z_V(icote,2)
            compteur = compteur - 1
            if( icote /= 1 ) then
               Casier%Loi_Z_S(icote,1) = dble(icote-1) * Casier%PasPlanim + Casier%CoteFond
               Casier%Loi_Z_V(icote,1) = Casier%Loi_Z_S(icote,1)
            else
               Casier%Loi_Z_S(1,1) = Casier%CoteFond
               Casier%Loi_Z_V(1,1) = Casier%CoteFond
            end if
         end do
   end select

   deallocate (rangement_cote, STAT = retour)
   if( retour /= 0 ) then
      Erreur%Numero = 6
      Erreur%ft     = err_6
      Erreur%ft_c   = err_6c
      call TRAITER_ERREUR_CASIER( Erreur , 'rangement_cote' )
      return
   end if

   return

   contains

      real(DOUBLE) function AIRE( X , Y , NbPoint )
         real(DOUBLE) , dimension(:) :: X , Y
         integer                     :: NbPoint , n

         AIRE = ( X(NbPoint) * Y(1) ) - ( Y(NbPoint) * X(1) )

         do n = 1 , ( NbPoint -1 )
            AIRE = AIRE + ( X(n) * Y(n+1) ) - ( Y(n) * X(n+1) )
         end do

         AIRE = AIRE * 0.5D0

      end function AIRE

end subroutine PLANIM_CASIER
