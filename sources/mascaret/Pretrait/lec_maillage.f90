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

subroutine LEC_MAILLAGE( &
     X                 , & ! Tableau des abscisses
     maille_e          , & !
     maille_r          , & !
     TypeMaillage      , & ! Type de calcul du maillage
     fichier           , & ! Fichier du maillage
     UniteListing      , & ! Unite logique fichier listing
     Erreur              & ! Erreur
                       )

! *********************************************************************
! PROGICIEL : MASCARET       S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
! *********************************************************************

   !========================= Declarations ===========================
   use M_PRECISION
   use M_ERREUR_T            ! Type ERREUR_T
   use M_FICHIER_T           ! Type FICHIER_T
   use M_MAILLE_T            ! Types MAILLE_E_T et MAILLE_R_T
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_LIRE_CHAINE_S       ! Sous programme de lecture de chaine
   use M_TRAITER_ERREUR_I    ! Traitement de l'errreur

   implicit none

   ! Arguments
   real(DOUBLE)      , dimension(:), pointer       :: X
   integer                         , intent(  out) :: TypeMaillage
   type(FICHIER_T)                 , intent(in   ) :: fichier
   type(MAILLE_R_T), dimension(:)  , pointer       :: maille_r
   type(MAILLE_E_T), dimension(:)  , pointer       :: maille_e
   integer                         , intent(in   ) :: UniteListing
   type(ERREUR_T)                  , intent(inout) :: Erreur
   ! Variables locales
   character(1), parameter :: CHAINE_COMMENTAIRE = '#'
   integer     , parameter :: LEN_CHAINE = 80
   character(LEN_CHAINE)   :: chaine     ! Chaine contenant une ligne du fichier
   integer             :: nb_section
   integer             :: nb_maille
   character(LEN=60)   :: libel, libel2, libel3
   integer :: retour              ! code de retour des fonctions
                                  ! intrinseques
   integer :: k                   ! compteur sur les mailles
   !character(132) :: !arbredappel_old

   !========================= Instructions ===========================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>LEC_MAILLAGE'

   open(unit=fichier%Unite, file=fichier%Nom, access='SEQUENTIAL', &
        action='READ'      , form='FORMATTED' , iostat=RETOUR    , &
        position='rewind'  , status='OLD'     )
   if( RETOUR /= 0 ) then
      Erreur%Numero = 3
      Erreur%ft     = err_3
      Erreur%ft_c   = err_3c
      call TRAITER_ERREUR( Erreur , fichier%Nom )
      return
   end if

   call LIRE_CHAINE_S( chaine , Fichier , CHAINE_COMMENTAIRE , retour )
   if( RETOUR /= 0 ) then
      Erreur%Numero = 382
      Erreur%ft     = err_382
      Erreur%ft_c   = err_382c
      call TRAITER_ERREUR( Erreur , 'le type du maillage' )
      return
   end if

   ! Methode de calcul du maillage
   read(chaine,*) libel , Typemaillage
   if( TypeMaillage < 1 .or. TypeMaillage > TYPE_MAILLAGE_NB_MAX ) then
      Erreur%Numero = 371
      Erreur%ft     = err_371
      Erreur%ft_c   = err_371c
      call TRAITER_ERREUR( Erreur , TYPE_MAILLAGE_NB_MAX )
      return
   end if

   select case( TypeMaillage )
      case( TYPE_MAILLAGE_PROFIL )
         if (UniteListing >0) write(UniteListing,10470) 'AUX PROFILS'
      case( TYPE_MAILLAGE_SERIE )
         if (UniteListing >0) write(UniteListing,10470) 'PAR SERIES'
      case( TYPE_MAILLAGE_INDIVIDUELLE )
         if (UniteListing >0) write(UniteListing,10470) 'SECTION PAR SECTION'
      case( TYPE_MAILLAGE_PRECEDENT )
         if (UniteListing >0) write(UniteListing,10470) 'REPRISE DU MAILLAGE PRECEDENT'
      case( TYPE_MAILLAGE_SERIE_PROFIL )
         if (UniteListing >0) write(UniteListing,10470) 'PAR SERIE AUX PROFILS'
   end select

   select case( TypeMaillage )

      case( TYPE_MAILLAGE_SERIE )

         call LIRE_CHAINE_S( chaine , Fichier , CHAINE_COMMENTAIRE , retour )
         if( RETOUR /= 0 ) then
            Erreur%Numero = 382
            Erreur%ft     = err_382
            Erreur%ft_c   = err_382c
            call TRAITER_ERREUR( Erreur , 'le nombre de zones du maillage' )
            return
         end if

         read(chaine,*) libel2 , nb_maille
         if( nb_maille <= 0 ) then
            Erreur%Numero = 306
            Erreur%ft     = err_306
            Erreur%ft_c   = err_306c
            call TRAITER_ERREUR( Erreur , 'Nombre de zones pour le calcul du maillage' )
            return
         end if

         if(.not.associated(maille_r)) allocate( maille_r(nb_maille) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'maille_r' )
            return
         end if

         if(.not.associated(maille_e)) allocate( maille_e(0) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'maille_e' )
            return
         end if

         do k = 1, nb_maille

            call LIRE_CHAINE_S( chaine , Fichier , CHAINE_COMMENTAIRE , retour )
            if( RETOUR /= 0 ) then
               Erreur%Numero = 383
               Erreur%ft     = err_383
               Erreur%ft_c   = err_383c
               call TRAITER_ERREUR( Erreur , k )
               return
            end if

            read(chaine,*)                        &
                 libel , maille_r(k)%AbscisseDeb, &
                 libel2, maille_r(k)%AbscisseFin, &
                 libel3, maille_r(k)%NbSection

         end do

      case( TYPE_MAILLAGE_SERIE_PROFIL )

         call LIRE_CHAINE_S( chaine , Fichier , CHAINE_COMMENTAIRE , retour )
         if( RETOUR /= 0 ) then
            Erreur%Numero = 382
            Erreur%ft     = err_382
            Erreur%ft_c   = err_382c
            call TRAITER_ERREUR( Erreur , 'le nombre de zones du maillage' )
            return
         end if

         read(chaine,*) libel , nb_maille
         if( nb_maille <= 0 ) then
            Erreur%Numero = 306
            Erreur%ft     = err_306
            Erreur%ft_c   = err_306c
            call TRAITER_ERREUR( Erreur , 'Nombre de zones du maillage' )
            return
         end if

         if(.not.associated(maille_e)) allocate( maille_e(nb_maille) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'maille_e' )
            return
         end if

         if(.not.associated(maille_r)) allocate( maille_r(0) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'maille_r' )
            return
         end if

         do k = 1 , nb_maille

            call LIRE_CHAINE_S( chaine , Fichier , CHAINE_COMMENTAIRE , retour )
            if( RETOUR /= 0 ) then
               Erreur%Numero = 383
               Erreur%ft     = err_383
               Erreur%ft_c   = err_383c
               call TRAITER_ERREUR( Erreur , k )
               return
            end if

            read(chaine,*)                     &
                libel , maille_e(k)%ProfilDeb, &
                libel2, maille_e(k)%ProfilFin, &
                libel3, maille_e(k)%Pas

         end do

      case( TYPE_MAILLAGE_INDIVIDUELLE )

         if(.not.associated(maille_r)) allocate( maille_r(0) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'maille_r' )
            return
         end if

         if(.not.associated(maille_e)) allocate( maille_e(0) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'maille_e' )
            return
         end if

         call LIRE_CHAINE_S( chaine , Fichier, CHAINE_COMMENTAIRE , retour )
         if( RETOUR /= 0 ) then
            Erreur%Numero = 382
            Erreur%ft     = err_382
            Erreur%ft_c   = err_382c
            call TRAITER_ERREUR( Erreur , 'le nombre de sections du maillage' )
            return
         end if

         read(chaine,*) libel , nb_section

         if(.not.associated(X)) allocate( X(nb_section) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'X' )
            return
         end if

         do k = 1 , nb_section

            call LIRE_CHAINE_S( chaine , Fichier , CHAINE_COMMENTAIRE , retour )
            if( RETOUR /= 0 ) then
               Erreur%Numero = 383
               Erreur%ft     = err_383
               Erreur%ft_c   = err_383c
               call TRAITER_ERREUR( Erreur , k )
               return
            end if

            read(chaine,*) libel , X(k)

         end do

      case default

         if(.not.associated(maille_r)) allocate( maille_r(0) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'maille_r' )
            return
         end if

         if(.not.associated(maille_e)) allocate( maille_e(0) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'maille_e' )
            return
         end if

      !----------------------------------------------------
      ! cas TYPE_MAILLAGE_PRECEDENT et TYPE_MAILLAGE_PROFIL
      !----------------------------------------------------
   end select

   ! Fin des traitements

   !Erreur%arbredappel = !arbredappel_old
   return

   ! Formats d'ecriture
   10470 format ('Type de calcul du maillage : ',A)

end subroutine LEC_MAILLAGE
