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

subroutine LEC_ABAQUE( &
    Abaque           , & ! Barrage
    FichierAbaque    , & ! Fichier abaque
    Erreur             & ! Erreur
                      )

! *********************************************************************
! PROGICIEL : MASCARET         A. LEBOSSE
!                              S. MANDELKERN
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
   !========================= Declarations ===========================
   use M_PRECISION
   use M_MESSAGE_C            ! messages d'erreur
   use M_ERREUR_T             ! Type ERREUR_T
   use M_FICHIER_T            ! Type FICHIER_T
   use M_TRAITER_ERREUR_I     ! traitement des erreurs

   implicit none

   ! Arguments
   real(DOUBLE)    , dimension(6,6,5) , intent(inout) :: Abaque
   type(FICHIER_T)                    , intent(inout) :: FichierAbaque
   type(ERREUR_T)                     , intent(inout) :: Erreur

   ! Variables locales
   integer :: iabaque ! compteur sur les abaques
   integer :: i, j    ! compteurs
   integer :: ul      ! Unite logique du fichier des abaques
   integer :: retour  ! Code de retour d'erreur
   character(132) :: label
   !character(132) :: !arbredappel_old

   !========================= Instructions ===========================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>LEC_ABAQUE'

   !
   ! Ouverture du fichier des abaques
   ! --------------------------------
   ul = FichierAbaque%Unite
   open( unit=ul          , file=FichierAbaque%Nom, access='SEQUENTIAL', &
         action='READ'    , form='FORMATTED', iostat=RETOUR            , &
         position='rewind', status='OLD'    )

   if( RETOUR /= 0 ) then
      Erreur%Numero = 4
      Erreur%ft     = err_4
      Erreur%ft_c   = err_4c
      call TRAITER_ERREUR( Erreur , FichierAbaque%Nom )
      return
   end if

   ! lecture des abaques 1a, 1b, 1c, 2a, 2b, 2c
   !-------------------------------------------
   do iabaque = 1 , 6
      read (ul,'(A)') label
      read (ul,*) ((ABAQUE(iabaque,i,j),j = 1,5),i = 1,6 )
   end do

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine LEC_ABAQUE
