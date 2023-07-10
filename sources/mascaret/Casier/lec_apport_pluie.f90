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

subroutine LEC_APPORT_PLUIE ( &
                              ApportPluie  , &
                              NbCasier     , &
                              Loi          , &
                              unitNum      , & ! unite logique du fichier .xcas
                              UniteListing , &
                              Erreur )

! ******************************************************************
! PROGICIEL : MASCARET             C. RISSOAN
!                                  F. ZAOUI
!
! VERSION : V8P4R0                  EDF-CEREMA
!
! LECTURE DE LA VARIABLE APPORT_PLUIE
! ******************************************************************
!
!   FICHIERS ENTREE/SORTIE :  --
!   ----------------------
!   SOUS PROGRAMMES APPELANTS : - PRETRAIT_CASIER
!   ---------------------------
!   SOUS PROGRAMMES APPELES :    --
!   -------------------------
!
!
!
   !========================== Declarations ==============================
   use M_APPORT_PLUIE_T
   use M_ERREUR_T
   use M_LOI_T
   use M_TRAITER_ERREUR_CASIER_I
   use M_TRAITER_ERREUR_I         ! Traitement de l'errreur
   use M_MESSAGE_CASIER_C
   use M_PRECISION
   use M_XCAS_S

   implicit none

   !.. Arguments ..
   type(APPORT_PLUIE_T)   , dimension(:) , pointer       :: ApportPluie
   type(ERREUR_T)                        , intent(inout) :: Erreur
   type(LOI_T)            , dimension(: ), intent(in   ) :: Loi
   integer                               , intent(in   ) :: NbCasier, UniteListing
   integer, intent(in)                                   :: unitNum

   !.. Variables locales ..
   !character(132) :: arbredappel_old
   integer :: nombre_apport, iapport, nbloi
   integer :: retour          ! code de retour des fonctions intrinseques
   integer, allocatable :: itab1(:),itab2(:)
   character(len=256)  :: pathNode
   character(len=8192) :: line

   !========================== Instructions =============================

   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   retour        = 0
   !arbredappel_old = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>LEC_APPORT_PLUIE'

   ! Nombre d apports
   !-----------------
   pathNode = 'parametresApportDeversoirs'
   line = xcasReader(unitNum, pathNode)

   !if(len(trim(line)).eq.0) then
   !      print*,"Parse error => parametresApportDeversoirs"
   !      call xerror(Erreur)
   !      return
   !endif

   pathNode = 'parametresApportDeversoirs/apportCasier'
   line = xcasReader(unitNum, pathNode)

   if(len(trim(line)).eq.0) then
       nombre_apport = 0
   else
       pathNode = 'parametresApportDeversoirs/apportCasier/nbApportPluie'
       line = xcasReader(unitNum, pathNode)
       read(unit=line, fmt=*) nombre_apport
   endif
   if( nombre_apport == 0 ) then
      if (UniteListing >0) then
         write( UniteListing , 10010 )
      end if
      allocate( ApportPluie(0) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR_CASIER( Erreur , 'ApportPluie' )
         return
      end if
      return
   end if

   ! Allocation des apports
   !-----------------------
   if(.not.associated(ApportPluie)) allocate( ApportPluie(nombre_apport) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR_CASIER( Erreur , 'ApportPluie' )
      return
   end if

   allocate( itab1(nombre_apport) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'itab1' )
       return
   end if
   allocate( itab2(nombre_apport) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'itab2' )
       return
   end if

   pathNode = 'parametresApportDeversoirs/apportCasier/numCasier'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) itab1

   pathNode = 'parametresApportDeversoirs/apportCasier/numLoi'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) itab2

   do iapport = 1 , nombre_apport

      ApportPluie(iapport)%Numero = itab1(iapport)
      if( ( ApportPluie(iapport)%Numero == 0 ) .or. ( ApportPluie(iapport)%Numero > NbCasier ) ) then
         Erreur%Numero = 1010
         Erreur%ft     = err_1010
         Erreur%ft_c   = err_1010c
         call TRAITER_ERREUR_CASIER( Erreur , iapport , NbCasier )
         return
      end if

      ApportPluie(iapport)%NumeroLoi = itab2(iapport)
      nbloi = size( Loi )
      if( ( ApportPluie(iapport)%NumeroLoi == 0 ) .or. ( ApportPluie(iapport)%NumeroLoi > nbloi ) ) then
         Erreur%Numero = 1020
         Erreur%ft     = err_1020
         Erreur%ft_c   = err_1020c
         call TRAITER_ERREUR_CASIER( Erreur , iapport , nbloi )
         return
      end if

      if( Loi( ApportPluie(iapport)%NumeroLoi )%Type /= LOI_TYPE_HYDROGRAMME ) then
         Erreur%Numero = 1030
         Erreur%ft     = err_1030
         Erreur%ft_c   = err_1030c
         call TRAITER_ERREUR_CASIER( Erreur , iapport )
         return
      end if

      ApportPluie(iapport)%Debit = 0._DOUBLE

   end do

   deallocate(itab1)
   deallocate(itab2)

   !.. Fin des traitements ..

   !Erreur%arbredappel = arbredappel_old

   return

   !.. Format Declarations ..
   10010 format( 'Il n''y a aucun apport de pluie dans les casiers' )

   contains

   subroutine xerror(Erreur)

       use M_MESSAGE_C
       use M_ERREUR_T            ! Type ERREUR_T

       type(ERREUR_T)                   , intent(inout) :: Erreur

       Erreur%Numero = 704
       Erreur%ft     = err_704
       Erreur%ft_c   = err_704c
       call TRAITER_ERREUR( Erreur )

       return

   end subroutine xerror

end subroutine LEC_APPORT_PLUIE
