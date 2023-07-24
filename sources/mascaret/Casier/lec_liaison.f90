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

subroutine LEC_LIAISON( &
                        Liaison     , &  ! tableau des liaisons
                        Connect     , &
                        X           , &  ! Tableau des sections de calcul
                        Profil      , &  ! Profils geometriques
                        ProfDebBief , &  ! Premiers profils des biefs
                        ProfFinBief , &  ! Derniers profils des biefs
                        unitNum     , & ! unite logique du fichier .xcas
                        Erreur )         ! erreur

! ******************************************************************
! PROGICIEL : MASCARET                C. RISSOAN
!                                     F. ZAOUI
!
! VERSION : V8P4R0                     EDF-CEREMA
!
! LECTURE DE LA VARIABLE LIAISON
! ******************************************************************
!
!   FICHIERS ENTREE/SORTIE :  --
!   ----------------------
!   SOUS PROGRAMMES APPELANTS : - PRETRAIT
!   ---------------------------
!   SOUS PROGRAMMES APPELES :    --
!   -------------------------

!========================== Declarations ==============================
   use M_PRECISION                ! type double
   use M_LIAISON_T                ! type liaison
   use M_ERREUR_T                 ! type erreur
   use M_PROFIL_T
   use M_MESSAGE_CASIER_C         ! messages d erreur propres a CASIER
   use M_CONSTANTES_CASIER_C      ! constantes de calcul propres a CASIER
   use M_TRAITER_ERREUR_CASIER_I  ! traitement des erreurs
   use M_TRAITER_ERREUR_I         ! Traitement de l'errreur
   use M_ABS_ABS_S
   use M_XINDIC_S
   use M_XCAS_S

   implicit none

   !.. Arguments ..
   type(LIAISON_T) , dimension(:) , pointer       :: Liaison
   integer , dimension(:,:)       , intent(inout) :: Connect
   type(ERREUR_T)                 , intent(inout) :: Erreur
   integer, intent(in)                            :: unitNum
   real(DOUBLE) , dimension(:) ,    intent(in   ) :: X
   type(PROFIL_T) , dimension(:) , pointer        :: Profil
   integer        , dimension(:) , pointer        :: ProfDebBief
   integer        , dimension(:) , pointer        :: ProfFinBief

   !.. Variables locales ..
   integer :: iliaison, nombre_liaison, num_casier_origine, num_casier_fin, nb_casier
   integer :: retour          ! code de retour des fonctions intrinseques
   real(DOUBLE) :: abs_abs
   integer, allocatable :: itab1(:),itab2(:),itab3(:),itab4(:),itab5(:),itab6(:)
   real(double), allocatable :: rtab1(:),rtab2(:),rtab3(:),rtab4(:),rtab5(:),rtab6(:)
   real(double), allocatable :: rtab7(:),rtab8(:),rtab9(:),rtab10(:),rtab11(:)
   character(len=256)  :: pathNode
   character(len=8192) :: line
   !character(132) :: !arbredappel_old

!========================== Instructions ==============================

   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   retour        = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>LEC_LIAISON'
   nb_casier = size( Connect(1,:) )

   ! Nombre de liaisons
   !-------------------
   pathNode = 'parametresCasier/liaisons/nbLiaisons'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) nombre_liaison
   if( nombre_liaison == 0 ) then
      Erreur%Numero = 900
      Erreur%ft     = err_900
      Erreur%ft_c   = err_900c
      call TRAITER_ERREUR_CASIER( Erreur , 'Existence de casiers' , 'liaisons' )
      return
   end if

   ! Allocation des liaisons
   !------------------------
   if(.not.associated(Liaison)) allocate( Liaison(nombre_liaison) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR_CASIER( Erreur , 'Liaison' )
      return
   end if

   allocate( itab1(nombre_liaison) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'itab1' )
       return
   end if
   allocate( itab2(nombre_liaison) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'itab2' )
       return
   end if
   allocate( itab3(nombre_liaison) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'itab3' )
       return
   end if
   allocate( itab4(nombre_liaison) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'itab4' )
       return
   end if
   allocate( itab5(nombre_liaison) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'itab5' )
       return
   end if
   allocate( itab6(nombre_liaison) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'itab6' )
       return
   end if
   allocate( rtab1(nombre_liaison) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'rtab1' )
       return
   end if
   allocate( rtab2(nombre_liaison) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'rtab2' )
       return
   end if
   allocate( rtab3(nombre_liaison) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'rtab3' )
       return
   end if
   allocate( rtab4(nombre_liaison) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'rtab4' )
       return
   end if
   allocate( rtab5(nombre_liaison) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'rtab5' )
       return
   end if
   allocate( rtab6(nombre_liaison) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'rtab6' )
       return
   end if
   allocate( rtab7(nombre_liaison) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'rtab7' )
       return
   end if
   allocate( rtab8(nombre_liaison) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'rtab8' )
       return
   end if
   allocate( rtab9(nombre_liaison) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'rtab9' )
       return
   end if
   allocate( rtab10(nombre_liaison) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'rtab10' )
       return
   end if
   allocate( rtab11(nombre_liaison) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'rtab11' )
       return
   end if

   pathNode = 'parametresCasier/liaisons/types'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) itab1

   pathNode = 'parametresCasier/liaisons/nature'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) itab2

   pathNode = 'parametresCasier/liaisons/numCasierOrigine'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) itab3

   pathNode = 'parametresCasier/liaisons/numBiefAssocie'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) itab4

   pathNode = 'parametresCasier/liaisons/numCasierOrigine'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) itab5

   pathNode = 'parametresCasier/liaisons/numCasierFin'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) itab6

   pathNode = 'parametresCasier/liaisons/abscBief'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) rtab1

   pathNode = 'parametresCasier/liaisons/largeur'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) rtab2

   pathNode = 'parametresCasier/liaisons/cote'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) rtab3

   pathNode = 'parametresCasier/liaisons/coefDebitSeuil'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) rtab4

   pathNode = 'parametresCasier/liaisons/coefActivation'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) rtab5

   pathNode = 'parametresCasier/liaisons/longueur'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) rtab6

   pathNode = 'parametresCasier/liaisons/rugosite'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) rtab7

   pathNode = 'parametresCasier/liaisons/section'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) rtab8

   pathNode = 'parametresCasier/liaisons/coefPerteCharge'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) rtab9

   pathNode = 'parametresCasier/liaisons/coefDebitOrifice'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) rtab10

   pathNode = 'parametresCasier/liaisons/typeOrifice'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) rtab11

   do iliaison = 1 , nombre_liaison

      Liaison(iliaison)%TypeLiaison = itab1(iliaison)
      if( ( Liaison(iliaison)%TypeLiaison <= 0 ) .or. &
          ( Liaison(iliaison)%TypeLiaison > LIAISON_TYPE_NB_MAX ) ) then
         Erreur%Numero = 907
         Erreur%ft     = err_907
         Erreur%ft_c   = err_907c
         call TRAITER_ERREUR_CASIER( Erreur , iliaison )
         return
      end if

      Liaison(iliaison)%NatureLiaison = itab2(iliaison)
      if( ( Liaison(iliaison)%NatureLiaison /= LIAISON_TYPE_RIVIERE_CASIER ) .and. &
        ( Liaison(iliaison)%NatureLiaison /= LIAISON_TYPE_CASIER_CASIER ) ) then
         Erreur%Numero = 908
         Erreur%ft     = err_908
         Erreur%ft_c   = err_908c
         call TRAITER_ERREUR_CASIER( Erreur , iliaison , LIAISON_TYPE_RIVIERE_CASIER , &
                                     LIAISON_TYPE_CASIER_CASIER )
         return
      end if

      select case( Liaison(iliaison)%NatureLiaison )

         case( LIAISON_TYPE_RIVIERE_CASIER )

            Liaison(iliaison)%CaracRC%NumCasier = itab3(iliaison)
            if( ( Liaison(iliaison)%CaracRC%NumCasier <= 0 ) .or. &
               ( Liaison(iliaison)%CaracRC%NumCasier > nb_casier ) ) then
               Erreur%Numero = 909
               Erreur%ft     = err_909
               Erreur%ft_c   = err_909c
               call TRAITER_ERREUR_CASIER( Erreur , iliaison , nb_casier )
               return
            end if

            Liaison(iliaison)%CaracRC%Abscisse = rtab1(iliaison)
            Liaison(iliaison)%CaracRC%NumBief  = itab4(iliaison)
            if( Liaison(iliaison)%CaracRC%NumBief <= 0 ) then
               Erreur%Numero = 9091
               Erreur%ft     = err_9091
               Erreur%ft_c   = err_9091c
               call TRAITER_ERREUR_CASIER (Erreur, iliaison)
               return
            end if
            ! test sur abscisse a faire, par rapport a abscisse de debut et fin de bief
            ! calcul de l'abscisse absolue
            abs_abs = ABS_ABS_S( &
                                Liaison(iliaison)%CaracRC%NumBief  , &
                                Liaison(iliaison)%CaracRC%Abscisse , &
                                Profil                             , &
                                ProfDebBief                        , &
                                ProfFinBief                        , &
                                Erreur )
            if( Erreur%Numero /= 0 ) then
               return
            end if

            if( ( abs_abs < X(1) ) .or.( abs_abs > X(size( X )) ) ) then
               Erreur%Numero = 910
               Erreur%ft     = err_910
               Erreur%ft_c   = err_910c
               call TRAITER_ERREUR_CASIER( Erreur , iliaison , abs_abs , X(1) , X(size( X )) )
               return
            end if

            ! Calcul de la section de calcul correspondante
            call XINDIC_S( Liaison(iliaison)%CaracRC%Section , abs_abs , X , Erreur )
            if( Erreur%Numero /= 0 ) then
               return
            endif

            Liaison(iliaison)%CaracRC%DQDZcasier  = 0._DOUBLE
            Liaison(iliaison)%CaracRC%DQDZriviere = 0._DOUBLE

         case( LIAISON_TYPE_CASIER_CASIER )

            num_casier_origine = itab5(iliaison)
            num_casier_fin     = itab6(iliaison)
            if( ( num_casier_origine <= 0 ) .or. ( num_casier_origine > nb_casier ) ) then
               Erreur%Numero = 909
               Erreur%ft     = err_909
               Erreur%ft_c   = err_909c
               call TRAITER_ERREUR_CASIER( Erreur , iliaison , nb_casier )
               return
            end if
            if( ( num_casier_fin <= 0 ) .or. ( num_casier_fin > nb_casier ) ) then
               Erreur%Numero = 909
               Erreur%ft     = err_909
               Erreur%ft_c   = err_909c
               call TRAITER_ERREUR_CASIER (Erreur, iliaison, nb_casier)
               return
            end if

            Connect(num_casier_origine,num_casier_fin) = 1
            Connect(num_casier_fin,num_casier_origine) = 1
            Liaison(iliaison)%CaracCC%CasierOrigine    = num_casier_origine
            Liaison(iliaison)%CaracCC%CasierFin        = num_casier_fin
            Liaison(iliaison)%CaracCC%DQDZamont        = 0._DOUBLE
            Liaison(iliaison)%CaracCC%DQDZaval         = 0._DOUBLE

      end select

      select case( Liaison(iliaison)%TypeLiaison )

         case( LIAISON_TYPE_SEUIL )

            Liaison(iliaison)%Largeur = rtab2(iliaison)
            if( Liaison(iliaison)%Largeur <= 0) then
               Erreur%Numero = 911
               Erreur%ft   = err_911
               Erreur%ft_c = err_911c
               call TRAITER_ERREUR_CASIER( Erreur , iliaison , 'seuil' , 'largeur' )
               return
            end if

            Liaison(iliaison)%Cote = rtab3(iliaison)
            if( Liaison(iliaison)%Cote <= 0 ) then
               Erreur%Numero = 911
               Erreur%ft     = err_911
               Erreur%ft_c   = err_911c
               call TRAITER_ERREUR_CASIER( Erreur , iliaison , 'seuil' , 'cote' )
               return
            end if

            Liaison(iliaison)%CoefDebitSeuil = rtab4(iliaison)
            if( Liaison(iliaison)%CoefDebitSeuil <= 0 ) then
               Erreur%Numero = 911
               Erreur%ft     = err_911
               Erreur%ft_c   = err_911c
               call TRAITER_ERREUR_CASIER( Erreur , iliaison , 'seuil' , 'coefficient de debit' )
               return
            end if

            Liaison(iliaison)%CoefNoye = rtab5(iliaison)
            if( ( Liaison(iliaison)%CoefNoye <= 0 ) .or. &
                ( Liaison(iliaison)%CoefNoye >= 1 ) ) then
                Erreur%Numero = 912
                Erreur%ft     = err_912
                Erreur%ft_c   = err_912c
                call TRAITER_ERREUR_CASIER( Erreur , iliaison )
                return
            end if

            Liaison(iliaison)%Longueur         = 0._DOUBLE
            Liaison(iliaison)%Section          = 0._DOUBLE
            Liaison(iliaison)%Rugosite         = 0._DOUBLE
            Liaison(iliaison)%CoefPerteCharge  = 0._DOUBLE
            Liaison(iliaison)%CoefDebitOrifice = 0._DOUBLE
            Liaison(iliaison)%TypeOrifice      = 0

         case( LIAISON_TYPE_CANAL )

            Liaison(iliaison)%Cote = rtab3(iliaison)
            if( Liaison(iliaison)%Cote <= 0 ) then
               Erreur%Numero = 911
               Erreur%ft     = err_911
               Erreur%ft_c   = err_911c
               call TRAITER_ERREUR_CASIER( Erreur , iliaison , 'canal' , 'cote' )
               return
            end if

            Liaison(iliaison)%Longueur = rtab6(iliaison)
            if( Liaison(iliaison)%Longueur <= 0 ) then
               Erreur%Numero = 911
               Erreur%ft     = err_911
               Erreur%ft_c   = err_911c
               call TRAITER_ERREUR_CASIER( Erreur , iliaison , 'canal' , 'longueur' )
               return
            end if

            Liaison(iliaison)%Largeur = rtab2(iliaison)
            if( Liaison(iliaison)%Largeur <= 0 ) then
               Erreur%Numero = 911
               Erreur%ft     = err_911
               Erreur%ft_c   = err_911c
               call TRAITER_ERREUR_CASIER( Erreur , iliaison , 'canal' , 'largeur' )
               return
            end if

            Liaison(iliaison)%Rugosite = rtab7(iliaison)
            if( Liaison(iliaison)%Rugosite <= 0 ) then
               Erreur%Numero = 911
               Erreur%ft     = err_911
               Erreur%ft_c   = err_911c
               call TRAITER_ERREUR_CASIER( Erreur , iliaison , 'canal' , 'rugosite' )
               return
            end if

            Liaison(iliaison)%CoefDebitSeuil   = 0._DOUBLE
            Liaison(iliaison)%Section          = 0._DOUBLE
            Liaison(iliaison)%CoefNoye         = 0._DOUBLE
            Liaison(iliaison)%CoefPerteCharge  = 0._DOUBLE
            Liaison(iliaison)%CoefDebitOrifice = 0._DOUBLE
            Liaison(iliaison)%TypeOrifice      = 0

         case( LIAISON_TYPE_CHENAL )

            Liaison(iliaison)%Longueur = rtab6(iliaison)
            if( Liaison(iliaison)%Longueur <= 0 ) then
               Erreur%Numero = 911
               Erreur%ft   = err_911
               Erreur%ft_c = err_911c
               call TRAITER_ERREUR_CASIER( Erreur , iliaison , 'chenal' , 'longueur' )
               return
            end if

            Liaison(iliaison)%Largeur = rtab2(iliaison)
            if( Liaison(iliaison)%Largeur <= 0 ) then
               Erreur%Numero = 911
               Erreur%ft     = err_911
               Erreur%ft_c   = err_911c
               call TRAITER_ERREUR_CASIER( Erreur , iliaison , 'chenal' , 'largeur' )
               return
            end if

            Liaison(iliaison)%Cote     = rtab3(iliaison)

            Liaison(iliaison)%Rugosite = rtab7(iliaison)
            if( Liaison(iliaison)%Rugosite <= 0 ) then
               Erreur%Numero = 911
               Erreur%ft     = err_911
               Erreur%ft_c   = err_911c
               call TRAITER_ERREUR_CASIER( Erreur , iliaison , 'chenal' , 'rugosite' )
               return
            end if

            Liaison(iliaison)%CoefDebitSeuil   = 0._DOUBLE
            Liaison(iliaison)%Section          = 0._DOUBLE
            Liaison(iliaison)%CoefNoye         = 0._DOUBLE
            Liaison(iliaison)%CoefPerteCharge  = 0._DOUBLE
            Liaison(iliaison)%CoefDebitOrifice = 0._DOUBLE
            Liaison(iliaison)%TypeOrifice      = 0

         case( LIAISON_TYPE_SIPHON )

             Liaison(iliaison)%Longueur = rtab6(iliaison)
            if( Liaison(iliaison)%Longueur <= 0 ) then
               Erreur%Numero = 911
               Erreur%ft     = err_911
               Erreur%ft_c   = err_911c
               call TRAITER_ERREUR_CASIER( Erreur , iliaison , 'siphon' , 'longueur' )
               return
            end if

            Liaison(iliaison)%Section = rtab8(iliaison)
            if( Liaison(iliaison)%Section <= 0 ) then
               Erreur%Numero = 911
               Erreur%ft     = err_911
               Erreur%ft_c   = err_911c
               call TRAITER_ERREUR_CASIER( Erreur , iliaison , 'siphon' , 'section' )
               return
            end if

            Liaison(iliaison)%Cote = rtab3(iliaison)
            if( Liaison(iliaison)%Cote <= 0 ) then
               Erreur%Numero = 911
               Erreur%ft     = err_911
               Erreur%ft_c   = err_911c
               call TRAITER_ERREUR_CASIER( Erreur , iliaison , 'siphon' , 'cote' )
               return
            end if

            Liaison(iliaison)%CoefPerteCharge = rtab9(iliaison)
            if( Liaison(iliaison)%CoefPerteCharge <= 0 ) then
               Erreur%Numero = 911
               Erreur%ft     = err_911
               Erreur%ft_c   = err_911c
               call TRAITER_ERREUR_CASIER( Erreur , iliaison , 'siphon' , 'coefficient de perte de charge' )
               return
            end if

            Liaison(iliaison)%CoefDebitSeuil   = 0._DOUBLE
            Liaison(iliaison)%Largeur          = 0._DOUBLE
            Liaison(iliaison)%CoefNoye         = 0._DOUBLE
            Liaison(iliaison)%Rugosite         = 0._DOUBLE
            Liaison(iliaison)%CoefDebitOrifice = 0._DOUBLE
            Liaison(iliaison)%TypeOrifice      = 0

         case( LIAISON_TYPE_ORIFICE )

            Liaison(iliaison)%Largeur = rtab2(iliaison)
            if( Liaison(iliaison)%Largeur <= 0 ) then
               Erreur%Numero = 911
               Erreur%ft     = err_911
               Erreur%ft_c   = err_911c
               call TRAITER_ERREUR_CASIER( Erreur , iliaison , 'orifice' , 'largeur' )
               return
            end if

            Liaison(iliaison)%Section = rtab8(iliaison)
            if( Liaison(iliaison)%Section <= 0 ) then
               Erreur%Numero = 911
               Erreur%ft     = err_911
               Erreur%ft_c   = err_911c
               call TRAITER_ERREUR_CASIER( Erreur , iliaison , 'orifice' , 'section' )
               return
            end if

            Liaison(iliaison)%Cote = rtab3(iliaison)
            if( Liaison(iliaison)%Cote <= 0 ) then
               Erreur%Numero = 911
               Erreur%ft     = err_911
               Erreur%ft_c   = err_911c
               call TRAITER_ERREUR_CASIER( Erreur , iliaison , 'orifice' , 'cote' )
               return
            end if

            Liaison(iliaison)%CoefDebitSeuil = rtab4(iliaison)
            if( Liaison(iliaison)%CoefDebitSeuil <= 0 ) then
               Erreur%Numero = 911
               Erreur%ft     = err_911
               Erreur%ft_c   = err_911c
               call TRAITER_ERREUR_CASIER( Erreur , iliaison , 'orifice' , 'coef debit seuil' )
               return
            end if

            Liaison(iliaison)%CoefDebitOrifice = rtab10(iliaison)
            if( Liaison(iliaison)%CoefDebitOrifice <= 0 ) then
               Erreur%Numero = 911
               Erreur%ft     = err_911
               Erreur%ft_c   = err_911c
               call TRAITER_ERREUR_CASIER( Erreur , iliaison , 'orifice' , 'coef debit orifice' )
               return
            end if

            Liaison(iliaison)%TypeOrifice = int(rtab11(iliaison))
            if( ( Liaison(iliaison)%TypeOrifice <= 0 ) .or. &
                ( Liaison(iliaison)%TypeOrifice > 3 ) ) then
                Erreur%Numero = 911
                Erreur%ft     = err_911
                Erreur%ft_c   = err_911c
                call TRAITER_ERREUR_CASIER( Erreur , iliaison , 'orifice' , 'type orifice' )
                return
            end if

            Liaison(iliaison)%Longueur        = 0._DOUBLE
            Liaison(iliaison)%Rugosite        = 0._DOUBLE
            Liaison(iliaison)%CoefNoye        = 0._DOUBLE
            Liaison(iliaison)%CoefPerteCharge = 0._DOUBLE

      end select

      Liaison(iliaison)%DebitEchange    = 0._DOUBLE
      Liaison(iliaison)%VitesseEchange  = 0._DOUBLE
      Liaison(iliaison)%DebitMax        = 0._DOUBLE
      Liaison(iliaison)%TempsDebitMax   = 0._DOUBLE
      Liaison(iliaison)%VitesseMax      = 0._DOUBLE
      Liaison(iliaison)%TempsVitesseMax = 0._DOUBLE

   end do

   !.. Fin des traitements ..

   deallocate(itab1)
   deallocate(itab2)
   deallocate(itab3)
   deallocate(itab4)
   deallocate(itab5)
   deallocate(itab6)
   deallocate(rtab1)
   deallocate(rtab2)
   deallocate(rtab3)
   deallocate(rtab4)
   deallocate(rtab5)
   deallocate(rtab6)
   deallocate(rtab7)
   deallocate(rtab8)
   deallocate(rtab9)
   deallocate(rtab10)
   deallocate(rtab11)

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine LEC_LIAISON
