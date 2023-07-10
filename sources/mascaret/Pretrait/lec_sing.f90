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

subroutine LEC_SING(    &
  Singularite         , & ! Pertes de charges singulieres
     LoiHydrau        , & ! Loi hydraulique
     Connect          , & ! Connectivite du reseau
     X                , & ! Maillage
     Profil           , & ! Profils geometriques
     ProfDebBief      , & ! Premiers profils des biefs
     ProfFinBief      , & ! Derniers profils des biefs
     AbscRelExtDebBief, & ! Abscisse rel de l'extremite debut du bief
     AbscRelExtFinBief, & ! Abscisse rel de l'extremite debut du bief
     Noyau            , & ! Noyau de calcul utilise
     UlLst            , & ! Unite logique fichier listing
     unitNum          , & ! Unite logique .xcas
     Erreur             & ! Erreur
                      )

! *********************************************************************
! PROGICIEL : MASCARET       S. MANDELKERN
!                            F. ZAOUI
!
! VERSION : V8P4R0              EDF-CEREMA
! *********************************************************************

   !========================= Declarations ===========================
   use M_PRECISION
   use M_CONNECT_T           ! Type CONNECT_T : connectivite du reseau
   use M_ERREUR_T            ! Type ERREUR_T
   use M_LOI_T               ! Type LOI_T
   use M_PROFIL_T            ! Type PROFIL_T
   use M_SINGULARITE_T       ! Type SINGULARITE_T
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_TRAITER_ERREUR_I    ! Traitement de l'errreur
   use M_ABS_ABS_S           ! Calcul de l'abscisse absolue
   use M_XINDIC_S            ! Calc de l'indice corresp a une absc
   use M_XCAS_S

   implicit none

   ! Arguments
   type(SINGULARITE_T), dimension(:), pointer       :: Singularite
   type(LOI_T)        , dimension(:), pointer       :: LoiHydrau
   type(CONNECT_T)                  , intent(in   ) :: Connect
   real(DOUBLE)       , dimension(:), intent(in   ) :: X
   type(PROFIL_T)    , dimension(:)  , intent(in   ) :: Profil
   integer           , dimension(:)  , intent(in   ) :: ProfDebBief
   integer           , dimension(:)  , intent(in   ) :: ProfFinBief
   real(DOUBLE)      , dimension(:)  , intent(in   ) :: AbscRelExtDebBief
   real(DOUBLE)      , dimension(:)  , intent(in   ) :: AbscRelExtFinBief
   integer                           , intent(in   ) :: Noyau
   integer                           , intent(in   ) :: UlLst
   integer, intent(in)                               :: unitNum
   ! Variables locales
   real(DOUBLE) :: abs_abs     ! abscisse absolue de la singularite
   integer      :: num_branche ! numero de la branche
   integer      :: num_loi     ! Numero d'une loi hydraulique
   integer      :: nb_sing     ! Nombre de singularites
   integer      :: ising       ! Compteur sur les singularites
   integer      :: ipoint      ! Compteur sur les points
   integer      :: jpoint      ! Compteur sur les points
   integer      :: nb_point    ! nombre de points des lois
   integer      :: nb_point_z  ! nombre de points Z des lois
   integer      :: nb_point_q  ! nombre de points Q des lois
   integer      :: retour      ! Code de retour des fonctions intrinseques
   !character(132) :: !arbredappel_old
   real(double), allocatable :: rtab1(:),rtab2(:)
   character(len=256)  :: pathNode
   character(len=8192) :: line
   ! Traitement des erreurs
   type(ERREUR_T), intent(inout) :: Erreur

   !========================= Instructions ===========================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   retour        = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>LEC_SING'

   ! Nombre de singularites
   !-----------------------
   if (UlLst >0) write(UlLst,10000)

   pathNode = 'parametresSingularite/nbSeuils'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) nb_sing

   if( nb_sing < 0 ) then
      Erreur%Numero = 306
      Erreur%ft     = err_306
      Erreur%ft_c   = err_306c
      call TRAITER_ERREUR( Erreur , 'Nombre de singularites' )
      return
   end if

   if (UlLst >0) write(UlLst,10010) nb_sing
   if( nb_sing > 0 ) then

      if(.not.associated(Singularite)) allocate( Singularite(nb_sing) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Singularite' )
         return
      end if

      pathNode = 'parametresSingularite/seuils'
      line = xcasReader(unitNum, pathNode)

      do ising = 1 , nb_sing
         ! Initialisation des pointeurs pour l'API
         nullify(Singularite(ising)%PtZ)
         nullify(Singularite(ising)%PtQ)
         nullify(Singularite(ising)%PtZamont)
         nullify(Singularite(ising)%PtZaval)
         nullify(Singularite(ising)%PtX)
         nullify(Singularite(ising)%PtY)

         Singularite(ising)%EtatEfface      = .FALSE.

         pathNode = 'structureParametresSeuil'
         if(ising.eq.1) then
           line = xcasReader(unitNum, pathNode, 0)
         else
           line = xcasReader(unitNum, pathNode, 1)
         endif

         pathNode = 'nom'
         Singularite(ising)%Nom = xcasReader(unitNum, pathNode, 2)

         pathNode = 'type'
         line = xcasReader(unitNum, pathNode, 2)
         read(unit=line, fmt=*) Singularite(ising)%Type

         pathNode = 'numBranche'
         line = xcasReader(unitNum, pathNode, 2)
         read(unit=line, fmt=*) Singularite(ising)%NumBranche
         num_branche = Singularite(ising)%NumBranche

         pathNode = 'abscisse'
         line = xcasReader(unitNum, pathNode, 2)
         read(unit=line, fmt=*) Singularite(ising)%AbscisseRel

         if( Singularite(ising)%Type /= SINGULARITE_TYPE_PROFIL_CRETE ) then
            pathNode = 'coteCrete'
            line = xcasReader(unitNum, pathNode, 2)
            read(unit=line, fmt=*) Singularite(ising)%CoteCrete
            if( Singularite(ising)%CoteCrete < 0._DOUBLE ) then
               Erreur%Numero = 326
               Erreur%ft     = err_326
               Erreur%ft_c   = err_326c
               call TRAITER_ERREUR( Erreur , ising , Singularite(ising)%CoteCrete )
               return
            end if
         else if( Singularite(ising)%Type == SINGULARITE_TYPE_PROFIL_CRETE ) then
            pathNode = 'coteCreteMoy'
            line = xcasReader(unitNum, pathNode, 2)
            read(unit=line, fmt=*) Singularite(ising)%CoteCrete
            if( Singularite(ising)%CoteCrete < 0._DOUBLE ) then
               Erreur%Numero = 326
               Erreur%ft     = err_326
               Erreur%ft_c   = err_326c
               call TRAITER_ERREUR( Erreur , ising , Singularite(ising)%CoteCrete )
               return
            end if
         end if

         pathNode = 'coteRupture'
         line = xcasReader(unitNum, pathNode, 2)
         read(unit=line, fmt=*) Singularite(ising)%CoteRupture
         if( Singularite(ising)%CoteRupture < Singularite(ising)%CoteCrete ) then
            Erreur%Numero = 328
            Erreur%ft     = err_328
            Erreur%ft_c   = err_328c
            call TRAITER_ERREUR( Erreur , ising , Singularite(ising)%CoteRupture )
            return
         end if

         pathNode = 'coeffDebit'
         line = xcasReader(unitNum, pathNode, 2)
         read(unit=line, fmt=*) Singularite(ising)%coeffDebit
         if( Singularite(ising)%CoeffDebit < 0._DOUBLE .or. Singularite(ising)%CoeffDebit > 1._DOUBLE ) then
            Erreur%Numero = 327
            Erreur%ft     = err_327
            Erreur%ft_c   = err_327c
            call TRAITER_ERREUR( Erreur , ising , Singularite(ising)%CoeffDebit )
            return
         end if

         if( Singularite(ising)%Type == SINGULARITE_TYPE_VANNE ) then
             pathNode = 'largVanne'
             line = xcasReader(unitNum, pathNode, 2)
             read(unit=line, fmt=*) Singularite(ising)%LargeurVanne
             if( Singularite(ising)%LargeurVanne < 0._DOUBLE ) then
                Erreur%Numero = 370
                Erreur%ft     = err_370
                Erreur%ft_c   = err_370c
                call TRAITER_ERREUR( Erreur , ising , Singularite(ising)%LargeurVanne )
                return
             end if
         end if

         if( Singularite(ising)%Type /= SINGULARITE_TYPE_PROFIL_CRETE ) then
             pathNode = 'numLoi'
             line = xcasReader(unitNum, pathNode, 2)
             read(unit=line, fmt=*) Singularite(ising)%NumeroLoi
             if( Singularite(ising)%Type /= SINGULARITE_TYPE_CRETE_COEFF ) then
                if( Singularite(ising)%NumeroLoi <= 0 .or. Singularite(ising)%NumeroLoi > size(LoiHydrau) ) then
                   Erreur%Numero = 329
                   Erreur%ft     = err_329
                   Erreur%ft_c   = err_329c
                   call TRAITER_ERREUR( Erreur , ising , num_loi )
                   return
                end if
             end if
             num_loi = Singularite(ising)%NumeroLoi
             if (UlLst >0) write(UlLst,10050) Singularite(ising)%NumeroLoi
          endif

          ! Implementation des loi de singularite
          !--------------------------------------
          select case( Singularite(ising)%Type )

             case( SINGULARITE_TYPE_CRETE_COEFF )

                ! Controle de coherence type de singularite / type de loi
                !--------------------------------------------------------
                if( num_loi /= 0 ) then
                   if( LoiHydrau(num_loi)%Type /= LOI_TYPE_HYDROGRAMME ) then
                      Erreur%Numero = 359
                      Erreur%ft     = err_359
                      Erreur%ft_c   = err_359c
                      call TRAITER_ERREUR( Erreur, num_loi ,    &
                                      LoiHydrau(num_loi)%Type , &
                                      ising ,                   &
                                      Singularite(ising)%Type)
                      return
                   endif
                endif

                if( num_loi /= 0 ) then
                   Singularite(ising)%Debit = LoiHydrau(num_loi)%Debit(1)
                else
                   Singularite(ising)%Debit = 0._DOUBLE
                end if

                ! Impressions
                if (UlLst >0) write (Ullst,10130)
                if (UlLst >0) write(UlLst,10140) Singularite(ising)%Debit

                if(.not.associated(Singularite(ising)%PtX)) allocate( Singularite(ising)%PtX(2) , STAT = retour )
                if( retour /= 0 ) then
                   Erreur%Numero = 5
                   Erreur%ft     = err_5
                   Erreur%ft_c   = err_5c
                   call TRAITER_ERREUR( Erreur , 'Singularite%PtX' )
                   return
                end if

                if(.not.associated(Singularite(ising)%PtY)) allocate( Singularite(ising)%PtY(2) , STAT = retour )
                if( retour /= 0 ) then
                   Erreur%Numero = 5
                   Erreur%ft     = err_5
                   Erreur%ft_c   = err_5c
                   call TRAITER_ERREUR( Erreur , 'Singularite%PtY' )
                   return
                end if

             case( SINGULARITE_TYPE_ZAMONT_ZAVAL_Q )
                ! Controle de coherence type de singularite / type de loi
                !--------------------------------------------------------
                if( LoiHydrau(num_loi)%Type /= LOI_TYPE_ZAMONT_ZAVAL_Q ) then
                   Erreur%Numero = 359
                   Erreur%ft     = err_359
                   Erreur%ft_c   = err_359c
                   call TRAITER_ERREUR( Erreur , num_loi ,        &
                                        LoiHydrau(num_loi)%Type , &
                                        ising ,                   &
                                        Singularite(ising)%Type)
                   return
                endif

                ! Nombre de points des lois
                !--------------------------
                nb_point_z = size(LoiHydrau(num_loi)%CoteAval)
                nb_point_q = size(LoiHydrau(num_loi)%Debit)

                ! Allocations
                !------------
                if(.not.associated(Singularite(ising)%PtZaval)) allocate( Singularite(ising)%PtZaval(nb_point_z) , STAT = retour )
                if( retour /= 0 ) then
                   Erreur%Numero = 5
                   Erreur%ft     = err_5
                   Erreur%ft_c   = err_5c
                   call TRAITER_ERREUR( Erreur , 'Singularite(ising)%PtZaval' )
                   return
                end if

                if(.not.associated(Singularite(ising)%PtZamont)) &
                         allocate( Singularite(ising)%PtZamont(nb_point_q,nb_point_z) , STAT = retour )
                if( retour /= 0 ) then
                   Erreur%Numero = 5
                   Erreur%ft     = err_5
                   Erreur%ft_c   = err_5c
                   call TRAITER_ERREUR( Erreur , 'Singularite(ising)%PtZamont' )
                   return
                end if

                if(.not.associated(Singularite(ising)%PtQ)) allocate( Singularite(ising)%PtQ(nb_point_q) , STAT = retour )
                if( retour /= 0 ) then
                   Erreur%Numero = 5
                   Erreur%ft     = err_5
                   Erreur%ft_c   = err_5c
                   call TRAITER_ERREUR( Erreur , 'Singularite(ising)%PtQ' )
                   return
                end if

                if (UlLst >0) write(UlLst,10060) nb_point_z, nb_point_q
                if (UlLst >0) write(UlLst,10080)

                Singularite(ising)%PtQ(:)        = LoiHydrau(num_loi)%Debit(:)
                Singularite(ising)%PtZaval(:)    = LoiHydrau(num_loi)%CoteAval(:)
                Singularite(ising)%PtZamont(:,:) = LoiHydrau(num_loi)%CoteAmont(:,:)

                ! Impressions
                do ipoint = 1 , nb_point_q
                   do jpoint = 1,nb_point_z
                      if (UlLst >0) write(UlLst,10120) Singularite(ising)%PtQ(ipoint)     , &
                                         Singularite(ising)%PtZaval(jpoint) , &
                                         Singularite(ising)%PtZamont(ipoint,jpoint)
                   end do
                end do

             case( SINGULARITE_TYPE_ZAMONT_Q )
             !------------------------------

             ! Controle de coherence type de singularite / type de loi
             if( LoiHydrau(num_loi)%Type /= LOI_TYPE_TARAGE_Z_Q ) then
                Erreur%Numero = 359
                Erreur%ft     = err_359
                Erreur%ft_c   = err_359c
                call TRAITER_ERREUR( Erreur , num_loi ,        &
                                     LoiHydrau(num_loi)%Type , &
                                     ising ,                   &
                                     Singularite(ising)%Type )
                return
             endif

             ! Nombre de points des lois
             !--------------------------
             nb_point = size(LoiHydrau(num_loi)%Debit)

             ! Allocations
             if(.not.associated(Singularite(ising)%PtZ)) allocate( Singularite(ising)%PtZ(nb_point) , STAT = retour )
             if( retour /= 0 ) then
                Erreur%Numero = 5
                Erreur%ft     = err_5
                Erreur%ft_c   = err_5c
                call TRAITER_ERREUR( Erreur , 'Singularite(ising)%PtZ' )
                return
             end if

             if(.not.associated(Singularite(ising)%PtQ)) allocate( Singularite(ising)%PtQ(nb_point) , STAT = retour )
             if( retour /= 0 ) then
                Erreur%Numero = 5
                Erreur%ft   = err_5
                Erreur%ft_c = err_5c
                call TRAITER_ERREUR( Erreur , 'Singularite(ising)%PtQ' )
                return
             end if

             if (UlLst >0) write(UlLst,10070)

             Singularite(ising)%PtQ(:) = LoiHydrau(num_loi)%Debit(:)
             Singularite(ising)%PtZ(:) = LoiHydrau(num_loi)%Cote (:)

             do jpoint = 1 , nb_point
               if (UlLst >0) write(UlLst,10110) Singularite(ising)%PtQ(jpoint), &
                                  Singularite(ising)%PtZ(jpoint)
             end do

          case( SINGULARITE_TYPE_Q_ZAMONT , SINGULARITE_TYPE_Q_ZAVAL )
             !--------------------------------------------------------
             ! Controle de coherence type de singularite / type de loi
             if( LoiHydrau(num_loi)%Type /= LOI_TYPE_TARAGE_Q_Z ) then
                Erreur%Numero = 359
                Erreur%ft     = err_359
                Erreur%ft_c   = err_359c
                call TRAITER_ERREUR( Erreur, num_loi,         &
                                     LoiHydrau(num_loi)%Type, &
                                     ising,                   &
                                     Singularite(ising)%Type)
                return
             endif

             ! Nombre de points des lois
             !--------------------------
             nb_point = size(LoiHydrau(num_loi)%Debit)

             ! Allocations
             if(.not.associated(Singularite(ising)%PtZ)) allocate( Singularite(ising)%PtZ(nb_point) , STAT = retour )
             if( retour /= 0 ) then
                Erreur%Numero = 5
                Erreur%ft     = err_5
                Erreur%ft_c   = err_5c
                call TRAITER_ERREUR( Erreur , 'Singularite(ising)%PtZ' )
                return
             end if

             if(.not.associated(Singularite(ising)%PtQ)) allocate( Singularite(ising)%PtQ(nb_point) , STAT = retour )
             if( retour /= 0 ) then
                Erreur%Numero = 5
                Erreur%ft     = err_5
                Erreur%ft_c   = err_5c
                call TRAITER_ERREUR( Erreur , 'Singularite(ising)%PtQ' )
                return
             end if

             if (UlLst >0) write(UlLst,10090)

             Singularite(ising)%PtQ(:) = LoiHydrau(num_loi)%Debit(:)
             Singularite(ising)%PtZ(:) = LoiHydrau(num_loi)%Cote (:)

             do jpoint = 1 , nb_point
                if (UlLst >0) write(UlLst,10110) Singularite(ising)%PtZ(jpoint), &
                                   Singularite(ising)%PtQ(jpoint)
             end do

          case( SINGULARITE_TYPE_Z_T )
             !-------------------------
             ! Controle de coherence type de singularite / type de loi
             if( LoiHydrau(num_loi)%Type /= LOI_TYPE_LIMNIGRAMME ) then
                Erreur%Numero = 359
                Erreur%ft     = err_359
                Erreur%ft_c   = err_359c
                call TRAITER_ERREUR( Erreur, num_loi,         &
                                     LoiHydrau(num_loi)%Type, &
                                     ising,                   &
                                     Singularite(ising)%Type )
                return
             endif

             ! Allocation du tableau PtZ de la singularite
             if(.not.associated(Singularite(ising)%PtZ)) allocate( Singularite(ising)%PtZ(1) )
             if( retour /= 0 ) then
                Erreur%Numero = 5
                Erreur%ft     = err_5
                Erreur%ft_c   = err_5
                call TRAITER_ERREUR( Erreur , 'Singularite%PtZ' )
                return
             end if

          case( SINGULARITE_TYPE_VANNE )
             !---------------------------
             ! Controle de coherence type de singularite / type de loi
             if( LoiHydrau(num_loi)%Type /= LOI_TYPE_ZINF_ZSUP_T ) then
                Erreur%Numero = 359
                Erreur%ft     = err_359
                Erreur%ft_c   = err_359c
                call TRAITER_ERREUR( Erreur, num_loi,         &
                                     LoiHydrau(num_loi)%Type, &
                                     ising,                   &
                                     Singularite(ising)%Type)
                return
             endif

          case( SINGULARITE_TYPE_PROFIL_CRETE )
             !----------------------------------
             ! Allocations
             pathNode = 'nbPtLoiSeuil'
             line = xcasReader(unitNum, pathNode, 2)
             read(unit=line, fmt=*) nb_point
             if(.not.associated(Singularite(ising)%PtX)) allocate( Singularite(ising)%PtX(nb_point) , STAT = retour )
             if( retour /= 0 ) then
                Erreur%Numero = 5
                Erreur%ft     = err_5
                Erreur%ft_c   = err_5c
                call TRAITER_ERREUR( Erreur , 'Singularite(ising)%PtX' )
                return
             end if

             if(.not.associated(Singularite(ising)%PtY)) allocate( Singularite(ising)%PtY(nb_point) , STAT = retour )
             if( retour /= 0 ) then
                Erreur%Numero = 5
                Erreur%ft     = err_5
                Erreur%ft_c   = err_5c
                call TRAITER_ERREUR( Erreur , 'Singularite(ising)%PtY' )
                return
             end if

              allocate( rtab1(nb_point) , STAT = retour )
              if( retour /= 0 ) then
                  Erreur%Numero = 5
                  Erreur%ft     = err_5
                  Erreur%ft_c   = err_5c
                  call TRAITER_ERREUR( Erreur , 'rtab1' )
                  return
              end if
              allocate( rtab2(nb_point) , STAT = retour )
              if( retour /= 0 ) then
                  Erreur%Numero = 5
                  Erreur%ft     = err_5
                  Erreur%ft_c   = err_5c
                  call TRAITER_ERREUR( Erreur , 'rtab2' )
                  return
              end if

             if (UlLst >0) write(UlLst,10100)

             pathNode = 'abscTravCrete'
             line = xcasReader(unitNum, pathNode, 2)
             read(unit=line, fmt=*) rtab1

             pathNode = 'cotesCrete'
             line = xcasReader(unitNum, pathNode, 2)
             read(unit=line, fmt=*) rtab2

             do ipoint = 1 , nb_point
                Singularite(ising)%PtX(ipoint) = rtab1(ipoint)
                Singularite(ising)%PtY(ipoint) = rtab2(ipoint)

                if (UlLst >0) write(UlLst,10110) Singularite(ising)%PtX(ipoint) , Singularite(ising)%PtY(ipoint)
             end do

             deallocate(rtab1)
             deallocate(rtab2)

          end select

         pathNode = 'epaisseur'
         line = xcasReader(unitNum, pathNode, 2)
         read(unit=line, fmt=*) Singularite(ising)%Epaisseur_Seuil
         !
         !    Permet de traiter des ruptures non instantanees
         !
         pathNode = 'gradient'
         line = xcasReader(unitNum, pathNode, 2)
         read(unit=line, fmt=*) Singularite(ising)%Pente

         Singularite(ising)%debit  = 0

         if( Singularite(ising)%Type < 1 .or. &
             Singularite(ising)%Type > SINGULARITE_TYPE_NB_MAX ) then
            Erreur%Numero = 324
            Erreur%ft     = err_324
            Erreur%ft_c   = err_324c
            call TRAITER_ERREUR( Erreur , ising , Singularite(ising)%Type )
            return
         end if

         ! Controle de la compatibilite du type avec le noyau de calcul
         if( Noyau == NOYAU_SARAP                                  .and. &
           ( Singularite(ising)%Type == SINGULARITE_TYPE_Q_ZAVAL  .or.  &
             Singularite(ising)%Type == SINGULARITE_TYPE_VANNE))  then
            Erreur%Numero = 368
            Erreur%ft     = err_368
            Erreur%ft_c   = err_368c
            call TRAITER_ERREUR( Erreur , ising , Singularite(ising)%Type )
            return
         end if

         if( Noyau == NOYAU_MASCARET .and. Singularite(ising)%Type == 5 ) then
            Erreur%Numero = 368
            Erreur%ft     = err_368
            Erreur%ft_c   = err_368c
            call TRAITER_ERREUR( Erreur , ising , Singularite(ising)%Type )
            return
         end if

         if( Noyau == NOYAU_MASCARET .and. Singularite(ising)%Type == 8 ) then
            Erreur%Numero = 368
            Erreur%ft     = err_368
            Erreur%ft_c   = err_368c
            call TRAITER_ERREUR( Erreur , ising , Singularite(ising)%Type )
            return
         end if

         if( Noyau == NOYAU_MASCARET .and. Singularite(ising)%Type < 4 ) then
            Erreur%Numero = 368
            Erreur%ft     = err_368
            Erreur%ft_c   = err_368c
            call TRAITER_ERREUR( Erreur , ising , Singularite(ising)%Type )
            return
         end if

         if (UlLst >0) write(UlLst,10020) ising , Singularite(ising)%Nom , Singularite(ising)%Type

         if( Singularite(ising)%NumBranche <= 0 .or. &
             Singularite(ising)%NumBranche > size(Connect%OrigineBief)) then
            Erreur%Numero = 332
            Erreur%ft     = err_332
            Erreur%ft_c   = err_332c
            call TRAITER_ERREUR( Erreur , 'singularite' , &
                                 Singularite(ising)%NumBranche , ising )
            return
         end if

         if( Singularite(ising)%AbscisseRel < AbscRelExtDebBief(num_branche) .or. &
             Singularite(ising)%AbscisseRel > AbscRelExtFinBief(num_branche)) then
            Erreur%Numero = 347
            Erreur%ft     = err_347
            Erreur%ft_c   = err_347c
            call TRAITER_ERREUR( Erreur , ising , Singularite(ising)%AbscisseRel , num_branche )
            return
         endif

         if (UlLst >0) write(UlLst,10030) Singularite(ising)%NumBranche , Singularite(ising)%AbscisseRel

         !-----------------------------
         ! Calcul de l'abscisse absolue
         !-----------------------------
         abs_abs = ABS_ABS_S                ( &
              num_branche                    , &
              Singularite(ising)%AbscisseRel , &
              Profil                         , &
              ProfDebBief                    , &
              ProfFinBief                    , &
              Erreur                           &
                                          )
         if( Erreur%Numero /= 0 ) then
            return
         end if

         !----------------------------------------------
         ! Calcul de la section de calcul correspondante
         !----------------------------------------------
         call XINDIC_S( Singularite(ising)%Section , abs_abs , X , Erreur )
         if( Erreur%Numero /= 0 ) then
            return
         endif

         if (UlLst >0) write(UlLst,10040) Singularite(ising)%CoteCrete, Singularite(ising)%CoeffDebit, &
                            Singularite(ising)%CoteRupture , Singularite(ising)%Pente
         if( Singularite(ising)%Epaisseur_Seuil == 1 ) then
            if (UlLst >0) write (ULLST, *) ' Seuil de type epais'
         else
            if (UlLst >0) write (ULLST, *) ' Seuil de type mince'
         endif

      end do   ! boucle sur les singularites

   !-----------------------
   ! Si pas de singularites
   !-----------------------
   else

      if(.not.associated(Singularite)) allocate( Singularite(0) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Singularite' )
         return
      end if
   endif    ! de if(nb_sing > 0)

   ! Fin des traitements
   !--------------------

   !Erreur%Arbredappel = !arbredappel_old
   return

   ! Formats
  10000 format (/,'SINGULARITES',/, &
               &  '------------',/)
  10010 format ('Nombre de singularites : ',i3)
  10020 format (/,'Singularites  : ',i3,'    Nom : ',A,'    Type : ',i3)
  10030 format ('Branche       : ',i3,'    Abscisse : ',f12.3)
  10040 format ('Cote de crete : ',f12.3,'    Coefficient de debit : ',f7.3, &
                '    Cote de rupture : ',f12.3, &
                '  gradient d abaissement : ', F12.2)

  10050 format ('Numero de loi : ',i3)
  10060 format ('Nombre de points Z : ',i3,', Nombre de points Q : ',i3)
  10070 format (/,'           Q           Z')
  10080 format (/,'           Q         Zav         Zam')
  10090 format (/,'           Z           Q')
  10100 format (/,'           X           Y')
  10110 format (2f12.3)
  10120 format (6f12.3)
  10130 format (/,'           Debit  constant        ')
  10140 format (f12.3)

end subroutine LEC_SING
