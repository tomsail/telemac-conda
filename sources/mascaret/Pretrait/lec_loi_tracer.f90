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

subroutine LEC_LOI_TRACER( &
               LoiTracer , & ! Tableau des lois tracer
                  nbtrac , & ! Nombre de traceurs
        FichierLoiTracer , & ! Fichier des lois tracer
              impression , & ! Flag d'impression des lois
            UniteListing , & ! Unite logique fichier listing
            TempsMaximum , & ! Temps maximum du calcul
                 unitNum , & ! Unite logique du fichier .xcas
                  Erreur   & ! Erreur
                        )

!*****************************************************************************
! PROGICIEL : TRACER         M. LUCK
!                            F. ZAOUI
!
! VERSION : V8P4R0              EDF-CEREMA
!*****************************************************************************
!  Fonction : Lecture des lois temporelles d'evolution de concentration
!  --------    (pour CL et apports)
!
!  Sous-programme appelant : Pretrait_Traceur
!  -----------------------
!
!  Sous-programme appele : LecFicLoi_Tracer
!  ---------------------
!*************************************************************************

   !========================= Declarations ===========================
   use M_PRECISION
   use M_ERREUR_T            ! Type ERREUR_T
   use M_FICHIER_T           ! UniteListing
   use M_LOI_TRACER_T        ! Types LOI_TRACER_T
   use M_MESSAGE_C           ! Messages d'erreur
   use M_MESSAGE_TRACER_C
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_TRAITER_ERREUR_I    ! Traitement de l'erreur
   use M_LEC_FIC_LOI_TRACER_I  ! Interface de sous-programme
   use M_XCAS_S

   implicit none

   ! Arguments
   type(LOI_TRACER_T), dimension(:), pointer    :: LoiTracer
   type(FICHIER_T)              , intent(inout) :: FichierLoiTracer
   logical                      , intent(in   ) :: impression
   integer                      , intent(in   ) :: Nbtrac
   integer                      , intent(in   ) :: UniteListing
   real(DOUBLE)                 , intent(in   ) :: TempsMaximum
   integer, intent(in)                          :: unitNum
   ! Variables locales
   integer :: nb_loi_tracer         ! nombre de lois Tracer
   integer :: nb_point              ! nombre de points
   integer :: iloi                  ! compteur sur les lois
   integer :: i                     ! compteur sur les points
   integer :: retour                ! code de retour des fonctions intrinseques
   integer :: mode_entree_loi       ! type d'entree clavier/fichier
   integer :: unite_temps           ! unite de temps des lois entres par clavier
   character(132) :: arbredappel_old
   character(len=256)  :: pathNode
   character(len=8192) :: line
   ! Traitement des erreurs
   type(ERREUR_T), intent(inout) :: Erreur

   !========================= Instructions ===========================
   ! INITIALISATION
   ! --------------
   Erreur%Numero      = 0
   retour             = 0
   arbredappel_old    = trim(Erreur%arbredappel)
   Erreur%arbredappel = trim(Erreur%arbredappel)//'=>LEC_LOI_TRACER'

   write(UniteListing,10000)

   ! Nombre de lois
   !---------------
   pathNode = 'parametresTraceur/parametresLoisTraceur/nbLoisTracer'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) nb_loi_tracer

   if( impression ) then
      write(UniteListing,10010) nb_loi_tracer
   endif

   ! Allocation des lois
   !--------------------
   if(.not.associated(LoiTracer)) allocate( LoiTracer(nb_loi_tracer) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft   = err_5
      Erreur%ft_c = err_5c
      call TRAITER_ERREUR  (Erreur, 'LoiTracer')
      return
   end if

   pathNode = 'parametresTraceur/parametresLoisTraceur/loisTracer'
   line = xcasReader(unitNum, pathNode)
   if(len(trim(line)).eq.0) then
       print*,"Parse error => loisTracer"
       call xerror(Erreur)
       return
   endif

   do iloi = 1 , nb_loi_tracer
      pathNode = 'structureSParametresLoiTraceur/nom'
      if(iloi.eq.1) then
        LoiTracer(iloi)%Nom = xcasReader(unitNum, pathNode, 0)
      else
        LoiTracer(iloi)%Nom = xcasReader(unitNum, pathNode, 1)
      endif

      pathNode = 'modeEntree'
      line = xcasReader(unitNum, pathNode, 0)
      read(unit=line, fmt=*) mode_entree_loi

      if( mode_entree_loi /= SAISIE_PAR_FICHIER .and. &
          mode_entree_loi /= SAISIE_PAR_CLAVIER ) then
         Erreur%Numero = 510
         Erreur%ft     = err_510
         Erreur%ft_c   = err_510c
         call TRAITER_ERREUR( Erreur , 'la loi tracer' )
         return
      end if

      if( impression ) then
         write(UniteListing,10020) iloi , LoiTracer(iloi)%Nom
      endif

      if( mode_entree_loi == SAISIE_PAR_FICHIER ) then

         pathNode = 'fichier'
         FichierLoiTracer%Nom = xcasReader(unitNum, pathNode, 0)

         if( impression ) then
            write(UniteListing,10030) 'PAR FICHIER' , FichierLoiTracer%Nom
         endif

         call LEC_FIC_LOI_TRACER( &
               FichierLoiTracer , &	! Fic. contenant une evolution temporelle de conc
                LoiTracer(iloi) , & ! Concentration initiale des traceurs
                    unite_temps , & ! unite de temps des chroniques temporelles
                           iloi , & ! Numero de loi
                         nbtrac , & ! Nombre de traceurs
                         Erreur  )

         if( Erreur%Numero /= 0 ) then
            return
         endif

     endif ! de mode de saisie

      ! Passage du temps en secondes
      nb_point = size(LoiTracer(iloi)%Temps(:))

      if( unite_temps /= LOI_UNITE_SECONDE ) then

         select case (unite_temps)

            case( LOI_UNITE_MINUTE )
               do i = 1 , nb_point
                  LoiTracer(iloi)%Temps(i) = LoiTracer(iloi)%Temps(i) * 60.d0
               end do
            case( LOI_UNITE_HEURE )
               do i = 1 , nb_point
                  LoiTracer(iloi)%Temps(i) = LoiTracer(iloi)%Temps(i) * 3600.d0
               end do
            case( LOI_UNITE_JOUR )
               do i = 1 , nb_point
                  LoiTracer(iloi)%Temps(i) = LoiTracer(iloi)%Temps(i) * 86400.d0
               end do

         end select

      endif  ! de unite de temps

      ! Coherence avec le nombre de pas de temps de la simulation
      if( LoiTracer(iloi)%Temps(nb_point) < TempsMaximum ) then
         Erreur%Numero = 517
         Erreur%ft     = err_517
         Erreur%ft_c   = err_517c
         call TRAITER_ERREUR( Erreur , iloi , trim(LoiTracer(iloi)%Nom) )
         return
      end if

   end do

   !Erreur%Arbredappel = arbredappel_old

   return

   10000 format (/,'LOIS TRACER',/, &
                &  '-----------------',/)
   10010 format ('Nombre de lois = ',i3)
   10020 format (/,'Loi ',i3,' : Nom = ',A)
   10030 format ('Mode d''entree      = ',A,' Nom du fichier = ',A)

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

end subroutine LEC_LOI_TRACER
