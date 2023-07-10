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

subroutine LEC_APPORT ( &
     Apport           , & ! Tableau des lois hydrauliques
     Connect          , & ! Table de connectivite du reseau
     X                , & ! Maillage
     LoiHydrau        , & ! Lois hydrauliques
     Profil           , & ! Profils geometriques
     ProfDebBief      , & ! Premiers profils des biefs
     ProfFinBief      , & ! Derniers profils des biefs
     AbscRelExtDebBief, & ! Abscisse rel de l'extremite debut du bief
     AbscRelExtFinBief, & ! Abscisse rem de l'extremite debut du bief
     UniteListing     , & ! Unite logique fichier listing
     unitNum          , & ! Unite logique .xcas
     Erreur             & ! Erreur
                      )

! *********************************************************************
! PROGICIEL : MASCARET         A. LEBOSSE
!                              S. MANDELKERN
!                              F. ZAOUI
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   !========================= Declarations ===========================
   use M_PRECISION
   use M_APPORT_T            ! Type APPORT_T
   use M_CONNECT_T           ! Type CONNECT_T
   use M_ERREUR_T            ! Type ERREUR_T
   use M_LOI_T               ! Type LOI_T
   use M_PROFIL_T            ! Type PROFIL_T
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_TRAITER_ERREUR_I    ! Traitement de l'errreur
   use M_ABS_ABS_S           ! Calcul de l'abscisse absolue
   use M_XINDIC_S            ! Calc de l'indice corresp a une absc
   use M_XCAS_S

   implicit none

   ! Arguments
   type(APPORT_T)   , dimension(:), pointer       :: Apport
   type(CONNECT_T)                , intent(in   ) :: Connect
   real(DOUBLE)     , dimension(:), intent(in   ) :: X
   type(LOI_T)      , dimension(:), intent(in   ) :: LoiHydrau
   type(PROFIL_T)    , dimension(:)  , intent(in   ) :: Profil
   integer           , dimension(:)  , intent(in   ) :: ProfDebBief
   integer           , dimension(:)  , intent(in   ) :: ProfFinBief
   real(DOUBLE)      , dimension(:)  , intent(in   ) :: AbscRelExtDebBief
   real(DOUBLE)      , dimension(:)  , intent(in   ) :: AbscRelExtFinBief
   integer                           , intent(in   ) :: UniteListing
   integer, intent(in)                               :: unitNum
   type(ERREUR_T)                    , intent(inout) :: Erreur
   ! Variables locales
   real(DOUBLE) :: abs_abs     ! abscisse absolue de l'apport
   real(DOUBLE) :: abs_fin     ! abscisse relative de fin de l'apport
   integer      :: nb_apport   ! nombre d'apports
   integer      :: iapp         ! compteur sur les deversoirs
   integer      :: retour       ! code de retour des fonctions intrinseques
   integer      :: num_branche  ! numero de branche du deversoir
   integer      :: num_loi      ! numero de loi
   integer, allocatable :: itab1(:),itab2(:)
   real(double), allocatable :: rtab1(:),rtab2(:)
   character(len=256)  :: pathNode
   character(len=8192) :: line
   !character(132) :: !arbredappel_old

   !========================= Instructions ===========================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   retour = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>LEC_APPORT'

   if (UniteListing >0) write(UniteListing,10000)
   pathNode = 'parametresApportDeversoirs/debitsApports'
   line = xcasReader(unitNum, pathNode)
   if(len(trim(line)).eq.0) then
       nb_apport = 0
   else
       pathNode = 'parametresApportDeversoirs/debitsApports/nbQApport'
       line = xcasReader(unitNum, pathNode)
       read(unit=line, fmt=*) nb_apport
       if( nb_apport < 0 ) then
          Erreur%Numero = 306
          Erreur%ft     = err_306
          Erreur%ft_c   = err_306c
          call TRAITER_ERREUR( Erreur , 'Nombre de debits d''apport' )
          return
       end if
   endif

   if (UniteListing >0) write(UniteListing,10010) nb_apport

   if( nb_apport > 0 ) then

      if(.not.associated(Apport)) allocate( Apport(nb_apport) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Apport' )
         return
      end if

      allocate( itab1(nb_apport) , STAT = retour )
      if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'itab1' )
          return
      end if
      allocate( itab2(nb_apport) , STAT = retour )
      if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'itab2' )
          return
      end if
      allocate( rtab1(nb_apport) , STAT = retour )
      if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'rtab1' )
          return
      end if
      allocate( rtab2(nb_apport) , STAT = retour )
      if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'rtab2' )
          return
      end if

      pathNode = 'parametresApportDeversoirs/debitsApports/numBranche'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) itab1

      pathNode = 'parametresApportDeversoirs/debitsApports/numLoi'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) itab2

      pathNode = 'parametresApportDeversoirs/debitsApports/abscisses'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) rtab1

      pathNode = 'parametresApportDeversoirs/debitsApports/longueurs'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) rtab2

      do iapp = 1 , nb_apport

         if(iapp.eq.1) then
           pathNode = 'parametresApportDeversoirs/debitsApports/noms/string'
           Apport(iapp)%Nom = xcasReader(unitNum, pathNode)
        else
          pathNode = 'string'
          Apport(iapp)%Nom = xcasReader(unitNum, pathNode, 0)
        endif

         if (UniteListing >0) write(UniteListing,10020) iapp , Apport(iapp)%Nom
         Apport(iapp)%NumBranche = itab1(iapp)
         num_branche              = Apport(iapp)%NumBranche
         if( num_branche < 1 .or. num_branche > size(Connect%OrigineBief)) then
            Erreur%Numero = 332
            Erreur%ft     = err_332
            Erreur%ft_c   = err_332c
            call TRAITER_ERREUR( Erreur , 'debits d''apport' , num_branche , iapp )
            return
         end if

         Apport(iapp)%AbscisseRel = rtab1(iapp)
         if( Apport(iapp)%AbscisseRel < AbscRelExtDebBief(num_branche) .or. &
             Apport(iapp)%AbscisseRel > AbscRelExtFinBief(num_branche)) then
            Erreur%Numero   = 362
            Erreur%ft   = err_362
            Erreur%ft_c = err_362c
            call TRAITER_ERREUR( Erreur , iapp , Apport(iapp)%AbscisseRel , num_branche )
            return
         endif

         Apport(iapp)%Longueur = rtab2(iapp)
         if( Apport(iapp)%Longueur < 0 ) then
            Erreur%Numero = 336
            Erreur%ft     = err_336
            Erreur%ft_c   = err_336c
            call TRAITER_ERREUR( Erreur , iapp , Apport(iapp)%Longueur )
            return
         end if

         abs_fin = Apport(iapp)%AbscisseRel + Apport(iapp)%Longueur
         if( abs_fin < AbscRelExtDebBief(num_branche) .or. &
             abs_fin > AbscRelExtFinBief(num_branche)) then
            Erreur%Numero   = 363
            Erreur%ft       = err_363
            Erreur%ft_c     = err_363c
            call TRAITER_ERREUR( Erreur , iapp , abs_fin , num_branche )
            return
         endif

         ! Calcul des sections amont et aval des apports
         !----------------------------------------------
         ! 1) Amont
         ! Calcul de l'abscisse absolue
         abs_abs = ABS_ABS_S        ( &
           num_branche              , &
           Apport(iapp)%AbscisseRel , &
           Profil                   , &
           ProfDebBief              , &
           ProfFinBief              , &
           Erreur                     &
                                    )
         if( Erreur%Numero /= 0 ) then
            return
         end if

         ! Calcul de la section de calcul correspondante
         call XINDIC_S(Apport(iapp)%SectionAm, abs_abs, X, Erreur)
         if( Erreur%Numero /= 0 ) then
            return
         endif

         ! 2) Aval
         ! Calcul de l'abscisse absolue
         abs_abs = ABS_ABS_S ( &
           num_branche       , &
           abs_fin           , &
           Profil            , &
           ProfDebBief       , &
           ProfFinBief       , &
           Erreur              &
                            )
         if( Erreur%Numero /= 0 ) then
            return
         end if

         ! Calcul de la section de calcul correspondante
         call XINDIC_S( Apport(iapp)%SectionAv , abs_abs , X , Erreur )
         if( Erreur%Numero /= 0 ) then
            return
         endif

         if (UniteListing >0) write(UniteListing,10030) Apport(iapp)%NumBranche , Apport(iapp)%AbscisseRel , Apport(iapp)%Longueur

         Apport(iapp)%NumeroLoi   = itab2(iapp)
         num_loi                  = Apport(iapp)%NumeroLoi

         if( Apport(iapp)%NumeroLoi <= 0 .or. Apport(iapp)%NumeroLoi > size(LoiHydrau) ) then
            Erreur%Numero = 337
            Erreur%ft     = err_337
            Erreur%ft_c   = err_337c
            call TRAITER_ERREUR( Erreur , iapp , num_loi )
            return
         end if

         ! Controle de l'existence de la loi
         if( num_loi > size(LoiHydrau) ) then
            Erreur%Numero = 338
            Erreur%ft     = err_338
            Erreur%ft_c   = err_338c
            call TRAITER_ERREUR( Erreur , iapp )
            return
         endif

         ! Controle de coherence apport / type de loi
         if( LoiHydrau(num_loi)%Type /= LOI_TYPE_HYDROGRAMME ) then
            Erreur%Numero = 357
            Erreur%ft     = err_357
            Erreur%ft_c   = err_357c
            call TRAITER_ERREUR( Erreur, iapp,            &
                                 num_loi,                 &
                                 LoiHydrau(num_loi)%Type )
            return
         endif

         if (UniteListing >0) write(UniteListing,10040) num_loi

      end do

      deallocate(itab1)
      deallocate(itab2)
      deallocate(rtab1)
      deallocate(rtab2)

      !----------------
      ! Si pas d'apport
      !----------------
   else
      allocate( Apport(0) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Apport' )
         return
      end if
   end if

   !Erreur%Arbredappel = !arbredappel_old

   return

! Formats
   10000 format (/,'APPORTS',/, &
               &  '-------',/)
   10010 format ('Nombre d''apports : ',i3)
   10020 format (/,'Apport n0 ',i3,'    Nom : ',A)
   10030 format ('Branche ',i3,'    Abscisse : ',f12.3,'    Longueur : ',f12.3)
   10040 format ('Numero loi : ',i3)

end subroutine LEC_APPORT
