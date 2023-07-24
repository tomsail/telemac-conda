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

subroutine LEC_SOURCE( &
       Source_tracer , & ! Tableau des sources Tracer
              Apport , & ! Tableau des apports hydrauliques
             Connect , & ! Table de connectivite du reseau
                   X , & ! Maillage
           LoiTracer , & ! Lois tracer
              Profil , & ! Profils geometriques
              nbtrac , & ! nb de traceurs
        UniteListing , & ! Unite logique fichier listing
             unitNum , & ! Unite logique du fichier .xcas
              Erreur   & ! Erreur
                     )

!*****************************************************************************
! PROGICIEL : TRACER         M. LUCK
!                            F. ZAOUI
!
! VERSION : V8P4R0              EDF-CEREMA
!*****************************************************************************

   !========================= Declarations ===========================
   use M_PRECISION
   use M_PARAMETRE_C
   use M_SOURCE_TRACER_T     ! Type SOURCE_TRACER_T
   use M_APPORT_T            ! Type APPORT_T
   use M_CONNECT_T           ! Type CONNECT_T
   use M_ERREUR_T            ! Type ERREUR_T
   use M_LOI_TRACER_T        ! Type LOI_TRACER_T
   use M_PROFIL_T            ! Type PROFIL_T
   use M_MESSAGE_C           ! Messages d'erreur
   use M_MESSAGE_TRACER_C    ! Messages d'erreur Tracer
   use M_TRAITER_ERREUR_I    ! Traitement de l'errreur
   use M_ABS_ABS_S           ! Calcul de l'abscisse absolue
   use M_XINDIC_S            ! Calc de l'indice corresp a une absc
   use M_XCAS_S

   implicit none

   ! Arguments
   type(SOURCE_TRACER_T), dimension(:), pointer       :: Source_tracer
   type(APPORT_T)       , dimension(:), intent(in   ) :: Apport
   type(CONNECT_T)                    , intent(in   ) :: Connect
   real(DOUBLE)         , dimension(:), intent(in   ) :: X
   type(LOI_TRACER_T)   , dimension(:), intent(in   ) :: LoiTracer
   type(PROFIL_T)       , dimension(:), intent(in   ) :: Profil
   integer                            , intent(in   ) :: nbtrac
   integer                            , intent(in   ) :: UniteListing
   integer, intent(in)                                :: unitNum
   type(ERREUR_T)                     , intent(inout) :: Erreur
   ! Variables locales
   real(DOUBLE) :: abs_abs     ! abscisse absolue de l'apport
   real(DOUBLE) :: abs_fin     ! abscisse relative de fin de l'apport
   integer      :: nb_sources  ! nombre de sources
   integer      :: nb_apports  ! nombre d'appports hydrauliques
   integer      :: i           ! compteur sur les sources
   integer      :: retour      ! code de retour des fonctions intrinseques
   integer      :: num_branche ! numero de branche de la source
   integer      :: j,nb_prof,nb_bief
   integer      , dimension(size(Connect%OrigineBief))   :: ProfDebBief
   integer      , dimension(size(Connect%OrigineBief))   :: ProfFinBief
   real(DOUBLE) , dimension(size(Connect%OrigineBief))   :: AbscRelExtDebBief
   real(DOUBLE) , dimension(size(Connect%OrigineBief))   :: AbscRelExtFinBief
   character(len=256)  :: pathNode
   character(len=8192) :: line
   integer, allocatable :: itab1(:),itab2(:),itab3(:)
   real(double), allocatable :: rtab1(:),rtab2(:)

   !========================= Instructions ===========================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   retour        = 0
   !arbredappel_old = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>LEC_SOURCE'

   ! RECONSTRUCTION DES STRUCTURES ProfDebBief, ProfFinBief, AbscRelExtDebBief, AbscRelExtFinBief
   ! --------------------------------------------------------------------------------------------
   nb_prof    = size(Profil)
   nb_bief    = size(Connect%OrigineBief)
   nb_apports = size(Apport)

   ! .. detection des profils debut et fin de bief
   ProfDebBief(Profil(1)%NumBief) = 1
   do j = 2 , nb_prof
      if( Profil(j)%NumBief.ne.Profil(j-1)%NumBief ) then
         ProfDebBief(Profil(j)%NumBief) = j
         ProfFinBief(Profil(j-1)%NumBief) = j - 1
      endif
   enddo

   ProfFinBief(Profil(nb_prof)%NumBief) = nb_prof
   ! .. abscisse relative de ces profils debut et fin de bief
   do i = 1 , nb_bief
      AbscRelExtDebBief(i) = Profil(ProfDebBief(i))%AbsRel
      AbscRelExtFinBief(i) = Profil(ProfFinBief(i))%AbsRel
   enddo

   ! LECTURE DES DONNEES RELATIVES AUX SOURCES
   ! -----------------------------------------
   pathNode = 'parametresTraceur/parametresSourcesTraceur/nbSources'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) nb_Sources

   if( nb_Sources > 0 ) then

      if(.not.associated(Source_tracer)) allocate( Source_tracer(nb_sources) , STAT = Retour )

      allocate( itab1(nb_sources) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'itab1' )
         return
      end if
      allocate( itab2(nb_sources) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'itab2' )
         return
      end if
      allocate( itab3(nb_sources) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'itab3' )
         return
      end if
      allocate( rtab1(nb_sources) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'rtab1' )
         return
      end if
      allocate( rtab2(nb_sources) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'rtab2' )
         return
      end if

      pathNode = 'parametresTraceur/parametresSourcesTraceur/typeSources'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) itab1

      pathNode = 'parametresTraceur/parametresSourcesTraceur/numBranche'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) itab2

      pathNode = 'parametresTraceur/parametresSourcesTraceur/numLoi'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) itab3

      pathNode = 'parametresTraceur/parametresSourcesTraceur/abscisses'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) rtab1

      pathNode = 'parametresTraceur/parametresSourcesTraceur/longueurs'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) rtab2

      pathNode = 'parametresTraceur/parametresSourcesTraceur/noms'
      line = xcasReader(unitNum, pathNode)
      if(len(trim(line)).eq.0) then
         print*,"Parse error => noms"
         call xerror(Erreur)
         return
      endif

      do i = 1 , nb_Sources

         pathNode = 'string'
         Source_tracer(i)%Nom = xcasReader(unitNum, pathNode, 0)

         Source_Tracer(i)%Type = itab1(i)
         Source_Tracer(i)%NumBranche = itab2(i)
         num_branche                 = Source_Tracer(i)%NumBranche
         if( num_branche < 1 .or. num_branche > size(Connect%OrigineBief) ) then
            Erreur%Numero = 332
            Erreur%ft     = err_332
            Erreur%ft_c   = err_332c
            call TRAITER_ERREUR( Erreur ,'sources tracer' , num_branche , i )
            return
         end if

         Source_Tracer(i)%AbscisseRel = rtab1(i)
         if( Source_Tracer(i)%AbscisseRel < AbscRelExtDebBief(num_branche) .or. &
             Source_Tracer(i)%AbscisseRel > AbscRelExtFinBief(num_branche) ) then
            Erreur%Numero = 362
            Erreur%ft     = err_362
            Erreur%ft_c   = err_362c
            call TRAITER_ERREUR( Erreur , i  , Source_Tracer(i)%AbscisseRel , num_branche )
            return
         endif

         Source_Tracer(i)%Longueur = rtab2(i)
         if( Source_Tracer(i)%Longueur < 0 ) then
            Erreur%Numero = 336
            Erreur%ft     = err_336
            Erreur%ft_c   = err_336c
            call TRAITER_ERREUR( Erreur , i , Source_Tracer(i)%Longueur )
            return
         end if

         abs_fin = Source_Tracer(i)%AbscisseRel + Source_Tracer(i)%Longueur
         if( abs_fin < AbscRelExtDebBief(num_branche) .or. &
             abs_fin > AbscRelExtFinBief(num_branche) ) then
            Erreur%Numero = 363
            Erreur%ft     = err_363
            Erreur%ft_c   = err_363c
            call TRAITER_ERREUR( Erreur , i , abs_fin , num_branche )
            return
         endif

         ! Calcul des sections amont et aval des sources
         !... Amont
         abs_abs = ABS_ABS_S              ( &
            num_branche                   , &
            Source_Tracer(i)%AbscisseRel  , &
            Profil                        , &
            ProfDebBief                   , &
            ProfFinBief                   , &
            Erreur                          )
         if( Erreur%Numero /= 0 ) then
            return
         end if

         call XINDIC_S( Source_Tracer(i)%SectionAm , abs_abs , X , Erreur )
         if( Erreur%Numero /= 0 ) then
            return
         endif

         !... Aval
         abs_abs = ABS_ABS_S ( &
            num_branche       , &
            abs_fin           , &
            Profil            , &
            ProfDebBief       , &
            ProfFinBief       , &
            Erreur              )
         if( Erreur%Numero /= 0 ) then
            return
         end if

         call XINDIC_S( Source_Tracer(i)%SectionAv , abs_abs , X , Erreur )
         if( Erreur%Numero /= 0 ) then
            return
         endif

         Source_Tracer(i)%NumeroLoi = itab3(i)

         if( Source_Tracer(i)%NumeroLoi <= 0 ) then
            Erreur%Numero = 583
            Erreur%ft     = err_583
            Erreur%ft_c   = err_583c
            call TRAITER_ERREUR( Erreur , i )
            return
         endif
         if( Source_Tracer(i)%NumeroLoi > size(LoiTracer) ) then
            Erreur%Numero = 584
            Erreur%ft     = err_584
            Erreur%ft_c   = err_584c
            call TRAITER_ERREUR( Erreur , i )
            return
         endif

         ! Recherche si source superposee a un apport
         Source_Tracer(i)%SuperpositionApport = .false.
         do j = 1 , nb_apports
            if( abs(Apport(j)%AbscisseRel-Source_Tracer(i)%AbscisseRel).lt.EPS6 ) then
               Source_Tracer(i)%SuperpositionApport = .true.
               Source_Tracer(i)%NumeroApport        = j
            endif
         enddo
      enddo

      deallocate(itab1)
      deallocate(itab2)
      deallocate(itab3)
      deallocate(rtab1)
      deallocate(rtab2)

      !-----------------
      ! Si pas de source
      !-----------------
   else

      allocate( Source_tracer(0) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur ,'Source_tracer' )
         return
      end if

   end if

   !Erreur%Arbredappel = arbredappel_old

   return

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

end subroutine LEC_SOURCE
