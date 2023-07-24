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

subroutine LEC_PCSING( &
     PCSing          , & ! Pertes de charges singulieres
     X               , & ! Tableau du maillage
     Profil          , & ! Profils geometriques
     ProfDebBief     , & ! Premiers profils des biefs
     ProfFinBief     , & ! Derniers profils des biefs
    AbscRelExtDebBief, & ! Abscisse rel de l'extremite debut du bief
    AbscRelExtFinBief, & ! Abscisse rel de l'extremite debut du bief
     UlLst           , & ! Unite logique fichier listing
     unitNum         , & ! Unite logique .xcas
     Erreur            & ! Erreur
                     )

! *********************************************************************
! PROGICIEL : MASCARET       S. MANDELKERN
!                            F. ZAOUI
!
! VERSION : V8P4R0              EDF-CEREMA
! *********************************************************************

   !========================= Declarations ===========================
   use M_PRECISION
   use M_ERREUR_T            ! Type ERREUR_T
   use M_CONNECT_T           ! Type CONNECT_T : connectivite du reseau
   use M_PROFIL_T            ! Type PROFIL_T : profils geometriques
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_TRAITER_ERREUR_I    ! Traitement de l'errreur
   use M_ABS_ABS_S           ! Calcul de l'abscisse absolue
   use M_XINDIC_S            ! Calc de l'indice corresp a une absc
   use M_XCAS_S

   implicit none

   ! Arguments
   real(DOUBLE)    , dimension(:), pointer       :: PCSing
   real(DOUBLE)    , dimension(:), intent(in   ) :: X
   type(PROFIL_T)    , dimension(:) , intent(in   ) :: Profil
   integer           , dimension(:) , intent(in   ) :: ProfDebBief
   integer           , dimension(:) , intent(in   ) :: ProfFinBief
   real(DOUBLE)      , dimension(:) , intent(in   ) :: AbscRelExtDebBief
   real(DOUBLE)      , dimension(:) , intent(in   ) :: AbscRelExtFinBief
   integer                          , intent(in   ) :: UlLst
   integer, intent(in)                              :: unitNum
   ! Variables locales
   integer        :: nb_pc_sing   ! nombre de pertes
   real(DOUBLE)   :: abs_abs
   integer        :: num_branche  ! Numero de branche
   real(DOUBLE)   :: abscisse_rel ! Abscisse relative de la pdc
   real(DOUBLE)   :: coeff        ! Coefficient de la pdc
   integer        :: k            ! Compteur
   integer        :: indice       ! Indice de section de calcul
   integer        :: retour       ! Code de retour des fonctions intrinseques
   !character(132) :: !arbredappel_old
   integer, allocatable :: itab(:)
   real(double), allocatable :: rtab1(:),rtab2(:)
   ! Traitement des erreurs
   type(ERREUR_T), intent(inout) :: Erreur
   character(len=256)  :: pathNode
   character(len=8192) :: line

   !========================= Instructions ===========================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   retour        = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>LEC_PCSING'
   if (UlLst >0) write(UlLst,10000)

   ! Nombre de pertes de charge singulieres
   !---------------------------------------
   pathNode = 'parametresSingularite/pertesCharges'
   line = xcasReader(unitNum, pathNode)
   if(len(trim(line)).eq.0) then
       nb_pc_sing = 0
   else
       pathNode = 'parametresSingularite/pertesCharges/nbPerteCharge'
       line = xcasReader(unitNum, pathNode)
       read(unit=line, fmt=*) nb_pc_sing
       if( nb_pc_sing < 0 ) then
          Erreur%Numero = 306
          Erreur%ft     = err_306
          Erreur%ft_c   = err_306c
          call TRAITER_ERREUR( Erreur , 'Nombre de pertes de charge singulieres' )
          return
       end if
   endif

   if (UlLst >0) write(UlLst,10010) nb_pc_sing

   ! Allocation du tableau PCSING de meme taille que X
   !--------------------------------------------------
   if(.not.associated(PCSing)) allocate( PCSing(size(X(:))) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'PCSing' )
      return
   end if

   ! Initialisation
   PCSing(:) = 0._DOUBLE
   if( nb_pc_sing > 0 ) then

      allocate( itab(nb_pc_sing) , STAT = retour )
      if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'itab' )
          return
      end if
      allocate( rtab1(nb_pc_sing) , STAT = retour )
      if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'rtab1' )
          return
      end if
      allocate( rtab2(nb_pc_sing) , STAT = retour )
      if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'rtab2' )
          return
      end if

      pathNode = 'parametresSingularite/pertesCharges/numBranche'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) itab

      pathNode = 'parametresSingularite/pertesCharges/abscisses'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) rtab1

      pathNode = 'parametresSingularite/pertesCharges/coefficients'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) rtab2

      do k = 1 , nb_pc_sing

         num_branche  = itab(k)
         if( num_branche <= 0 .or. num_branche > size(AbscRelExtDebBief) ) then
            Erreur%Numero = 332
            Erreur%ft     = err_332
            Erreur%ft_c   = err_332c
            call TRAITER_ERREUR( Erreur , 'pertes de charge singulieres' , num_branche , k )
            return
         end if

         abscisse_rel = rtab1(k)

         if( abscisse_rel < AbscRelExtDebBief(num_branche) .or. abscisse_rel > AbscRelExtFinBief(num_branche) ) then
            Erreur%Numero   = 361
            Erreur%ft   = err_361
            Erreur%ft_c = err_361c
            call TRAITER_ERREUR( Erreur , k , abscisse_rel , num_branche )
            return
         endif

         !-----------------------------
         ! Calcul de l'abscisse absolue
         !-----------------------------
         abs_abs = ABS_ABS_S         ( &
             num_branche             , &
             abscisse_rel            , &
             Profil                  , &
             ProfDebBief             , &
             ProfFinBief             , &
             Erreur                    &
                                     )
         if( Erreur%Numero /= 0 ) then
            return
         end if

         !----------------------------------------------
         ! Calcul de la section de calcul correspondante
         !----------------------------------------------
         call XINDIC_S( indice , abs_abs , X , Erreur )
         if( Erreur%Numero /= 0 ) then
            return
         endif

         coeff = rtab2(k)
         if( coeff < 0._DOUBLE ) then
            Erreur%Numero = 333
            Erreur%ft     = err_333
            Erreur%ft_c   = err_333c
            call TRAITER_ERREUR( Erreur , k , coeff )
            return
         end if

         if (UlLst >0) write(UlLst,10020) k , num_branche , abscisse_rel , coeff

         PCSing(indice) = coeff

      end do

      deallocate(itab)
      deallocate(rtab1)
      deallocate(rtab2)

   endif   ! de if (nb_pc_sing > 0)

   !Erreur%Arbredappel = !arbredappel_old

   return

   ! Formats
   10000 format (/,'PERTES DE CHARGE SINGULIERE',/, &
                &  '---------------------------',/)
   10010 format ('Nombre de pertes de charge singulieres : ',i3,/)
   10020 format ('No ',i3,' Branche ',i3,' Abscisse = ',f12.3,' Coeff = ',f12.3)

end subroutine LEC_PCSING
