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

subroutine CALC_CONNECT( &
     Connect             , & ! Table de connectivite
     NbBief              , & ! Nombre de biefs
     AbscAbsExtDebBief   , & ! Abscisse abs de l'extremite debut du bief
     AbscAbsExtFinBief   , & ! Abscisse abs de l'extremite debut du bief
     X                   , & ! Abscisses de sections de calcul
     NbNoeud             , & ! Nombre de noeuds
     NbExtNoeud          , & ! Nombre d'extremite relie a chaque noeud
     ExtDebBief          , & ! Numero de l'extremite debut de chaque bief
     ExtFinBief          , & ! Numero de l'extremite fin de chaque bief
     ExtNoeud            , & ! Numero d'extremite lie a un noeud
     NbExtLibre          , & ! Nombre d'extremites libres
     NumExtLibre         , & ! Numero d'extremite libre
     Erreur                & ! Erreur
                         )

! *********************************************************************
! PROGICIEL : MASCARET         A. LEBOSSE
!                              S. MANDELKERN
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   !========================= Declarations ===========================
   use M_PRECISION
   use M_CONNECT_T           ! Type CONNECT
   use M_ERREUR_T            ! Type ERREUR_T
   use M_PROFIL_T            ! Type  PROFIL_T
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_TRAITER_ERREUR_I    ! Traitement de l'errreur
   use M_XINDIC_S            ! Calc de l'indice corresp a une absc

   implicit none
   ! Arguments
   type(CONNECT_T)                   , intent(  out) :: Connect
   integer                           , intent(in   ) :: NbBief
   real(DOUBLE)      , dimension(:)  , pointer       :: AbscAbsExtDebBief
   real(DOUBLE)      , dimension(:)  , pointer       :: AbscAbsExtFinBief
   real(DOUBLE)      , dimension(:)  , intent(in   ) :: X
   integer                           , intent(in   ) :: NbNoeud
   integer           , dimension(:)  , pointer       :: NbExtNoeud
   integer           , dimension(:)  , pointer       :: ExtDebBief
   integer           , dimension(:)  , pointer       :: ExtFinBief
   integer           , dimension(:,:), pointer       :: ExtNoeud
   integer                           , intent(in   ) :: NbExtLibre
   integer           , dimension(:)  , pointer       :: NumExtLibre
   type(ERREUR_T)                    , intent(inout) :: Erreur
   ! Variables locales
   integer :: nb_ext_max
   integer :: indice
   integer :: ibief  ! compteur sur les biefs
   integer :: inoeud ! compteur sur les noeud
   integer :: iext   ! compteur sur les extremites
   integer :: retour ! code de retour des fonctions intrinseques
   !character(132) :: !arbredappel_old

   !========================= Instructions ===========================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   retour = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>CALC_CONNECT'

   ! Les biefs
   !----------
   if(.not.associated(Connect%OrigineBief)) allocate( Connect%OrigineBief(NbBief), stat = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'Connect%OrigineBief' )
      return
   end if

   if(.not.associated(Connect%FinBief)) allocate( Connect%FinBief(NbBief) , stat = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft   = err_5
      Erreur%ft_c = err_5c
      call TRAITER_ERREUR  (Erreur, 'Connect%FinBief')
      return
   end if

   do ibief = 1 , NbBief
      call XINDIC_S( indice , AbscAbsExtDebBief(ibief) , X , Erreur )
      if( Erreur%Numero /= 0 ) then
         return
      endif
      Connect%OrigineBief(ibief) = indice
      call XINDIC_S( indice , AbscAbsExtFinBief(ibief) , X , Erreur )
      if( Erreur%Numero /= 0 ) then
         return
      endif
      Connect%FinBief(ibief) = indice
   end do

   ! Les noeuds
   !-----------
   !---------------------------
   ! Si il y a au moins 1 noeud
   !---------------------------
   if( NbNoeud > 0 ) then
      if(.not.associated(Connect%NbBiefConfluence)) allocate( Connect%NbBiefConfluence(NbNoeud) , stat = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Connect%NbBiefConfluence' )
         return
      end if

      do inoeud = 1 , NbNoeud
         Connect%NbBiefConfluence(inoeud) = NbExtNoeud(inoeud)
      end do

      nb_ext_max = 0

      do inoeud = 1 , NbNoeud
         if( NbExtNoeud(inoeud) > nb_ext_max ) then
            nb_ext_max = NbExtNoeud(inoeud)
         endif
      end do

      if(.not.associated(Connect%NumBiefConfluence)) allocate( Connect%NumBiefConfluence(NbNoeud, nb_ext_max) , stat = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Connect%NumBiefConfluence' )
         return
      end if
      do inoeud = 1 , NbNoeud
         do iext = 1 , Nb_ext_max
            Connect%NumBiefConfluence(inoeud,iext) = 0
         Enddo
      enddo

      if(.not.associated(Connect%NumSectionConfluence)) &
                     allocate( Connect%NumSectionConfluence( NbNoeud , nb_ext_max ) , stat = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Connect%NumSectionConfluence' )
         return
      end if

      do inoeud = 1 , NbNoeud
         do iext = 1 , NbExtNoeud(inoeud)
            do ibief = 1 , NbBief
               if(ExtDebBief(ibief) == ExtNoeud(iext, inoeud)) then
                  call XINDIC_S( indice , AbscAbsExtDebBief(ibief) , X , Erreur )
                  if( Erreur%Numero /= 0 ) then
                     return
                  endif
                  Connect%NumSectionConfluence(inoeud, iext) = indice
                  exit
               else if( ExtFinBief(ibief) == ExtNoeud(iext, inoeud) ) then
                  call XINDIC_S( indice , AbscAbsExtFinBief(ibief) , X , Erreur )
                  if( Erreur%Numero /= 0 ) then
                     return
                  endif
                  Connect%NumSectionConfluence(inoeud,iext) = indice
                  exit
               endif
            end do
            Connect%NumBiefConfluence(inoeud, iext) = ibief
         end do
      end do
   !-------------------------
   ! S'il n'y a pas de noeud
   !-------------------------
   else
      if(.not.associated(Connect%NbBiefConfluence)) allocate( Connect%NbBiefConfluence(0) , stat = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Connect%NbBiefConfluence' )
         return
      end if

      if(.not.associated(Connect%NumBiefConfluence)) allocate( Connect%NumBiefConfluence(0,0) , stat = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Connect%NumBiefConfluence' )
         return
      end if

      if(.not.associated(Connect%NumSectionConfluence)) allocate( Connect%NumSectionConfluence(0,0) , stat = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Connect%NumSectionConfluence' )
         return
      end if
   endif

   ! Les extremites libres
   !----------------------
   if(.not.associated(Connect%NumBiefExtLibre)) allocate( Connect%NumBiefExtLibre(NbExtLibre) , stat = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'Connect%NumBiefExtLibre' )
      return
   end if

   if(.not.associated(Connect%NumSectionExtLibre)) allocate( Connect%NumSectionExtLibre(NbExtLibre) , stat = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'Connect%NumBiefExtLibre' )
      return
   end if

   do iext = 1 , NbExtLibre
      do ibief = 1 , NbBief
         if( ExtDebBief(ibief) == NumExtLibre(iext) ) then
            call XINDIC_S( indice , AbscAbsExtDebBief(ibief) , X , Erreur )
            if( Erreur%Numero /= 0 ) then
               return
            endif
            Connect%NumSectionExtLibre(iext) = indice
            exit
         else if( ExtFinBief(ibief) == NumExtLibre(iext) ) then
            call XINDIC_S( indice , AbscAbsExtFinBief(ibief) , X , Erreur )
            if( Erreur%Numero /= 0 ) then
               return
            endif
            Connect%NumSectionExtLibre(iext) = indice
            exit
         endif
      end do
      Connect%NumBiefExtLibre(iext) = ibief
   end do

   !--------------------------------------
   ! Deallocation des tableaux temporaires
   !--------------------------------------
   if(associated(ExtDebBief)) deallocate( ExtDebBief , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 6
      Erreur%ft     = err_6
      Erreur%ft_c   = err_6c
      call TRAITER_ERREUR( Erreur , 'ExtDebBief' )
      return
   end if

   if(associated(ExtFinBief)) deallocate( ExtFinBief , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 6
      Erreur%ft   = err_6
      Erreur%ft_c = err_6c
      call TRAITER_ERREUR( Erreur , 'ExtFinBief' )
      return
   end if

   if( NbNoeud > 0 ) then
      if(associated(ExtNoeud)) deallocate( ExtNoeud , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 6
         Erreur%ft     = err_6
         Erreur%ft_c   = err_6c
         call TRAITER_ERREUR( Erreur , 'ExtNoeud' )
         return
      end if
   end if

   if(associated(NumExtLibre)) deallocate( NumExtLibre , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 6
      Erreur%ft     = err_6
      Erreur%ft_c   = err_6c
      call TRAITER_ERREUR( Erreur , 'NumExtLibre' )
      return
   end if

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine CALC_CONNECT
