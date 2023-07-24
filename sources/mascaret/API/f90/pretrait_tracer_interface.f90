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

subroutine PRETRAIT_Tracer_INTERFACE( &
            FichierMotCle , & ! Fichier des mots-cles
                  Connect , & ! Table de connectivite
                   Apport , & ! Apports hydrauliques
                   Profil , & ! Profils geometriques
                        X , & ! Abscisses des sections de calcul
                Extremite , & ! Extremites libres
             TempsMaximum , & ! Temps maximum du calcul
                            ! Lecture des parametres de Tracer
             OptionTracer , & ! Choix d'un calcul avec TRACER
                   Nbtrac , & ! Nombre de traceurs
                 ConsTrac , & ! Constantes pour TRACER
             FreqCouplage , & ! Frequence de couplage hydraulique/tracer
                            ! Conc init, CL, sources, lois tracer
         Presence_ConcIni , & ! Presence de conditions initiales
                 Cond_Lim , & ! Conditions aux limites
           Sources_tracer , & ! Sources pour le traceur
                LoiTracer , & ! Lois Tracer (CL ou sources)
       FichiersLoisTracer , & ! Liste de fichiers pour les lois Tracer
                            ! Lecture des parametres de QE
          Modele_Qual_Eau , & ! Modele de QE
                   ParPhy , & ! Parametres de modele de QE
                    Meteo , & ! Donnees meteo
           Fichier_Parphy , & ! Fichier des parametres de QE
            Fichier_Meteo , & ! Fichier meteo
                            ! Impression des parametres et resultats
        FichierResuTracer , & ! Fichier resultats
         FormatResuTracer , &
     FichierListingTracer , & ! Fichier listing
    ImpressionConcListing , & ! Logique pour les impressions
        ImpressionConcIni , & ! Logique pour les impressions initiales
    ImpressionBilanTracer , & ! Logique pour les impressions
              PasStockage , & ! Pas de stockage  (hydraulique)
            PasImpression , & ! Pas d'impression (hydraulique)
               Impression , &
                            ! Traitement des erreurs
                   Erreur )

!*****************************************************************************
! PROGICIEL : TRACER         S.MANDELKERN - M. LUCK
!                            F. ZAOUI
!
! VERSION : V8P4R0              EDF-CEREMA
!*****************************************************************************
!
!  FONCTION : LECTURE DU FICHIER CAS PAR APPEL DU LOGICIEL DAMOCLES
!             POUR LE MODULE TRACER
! ----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELANT : - SUPER_TRACER
!
!*****************************************************************************

   !=========================== Declarations ================================
   use M_PRECISION                 ! Definition de la precision DOUBLE ou SIMPLE
   use M_CONSTANTES_CALCUL_C       ! Constantes num, phys et info
   use M_PARAMETRE_C
   use M_FICHIER_T
   use M_CONNECT_T                 ! Type CONNECT_T : connectivite du reseau
   use M_APPORT_T                  ! Definition du type APPORT_T
   use M_PROFIL_T                  ! Definition du type PROFIL_T
   use M_EXTREMITE_T               ! Definition du type EXTREMITE_T
   use M_TRAITER_ERREUR_I          ! Traitement de l'errreur
   use M_PARAMETRES_QUALITE_EAU_T  ! Donnees physiques du traceur
   use M_METEO_T                   ! Donnees Meteo
   use M_SOURCE_TRACER_T           ! Sources de traceurs
   use M_LOI_TRACER_T              ! Lois tracer
   use M_CONSTANTES_TRACER_T
   use M_COND_LIM_TRACER_T         ! Conditions aux limites pour les traceurs
   use M_CONSTANTES_CALCUL_TRACER_C
   use M_LEC_LOI_TRACER_INTERFACE_I
   use M_LEC_SOURCE_I
   use M_ERREUR_T                  ! Traitement des erreurs
   use M_MESSAGE_C
   use M_MESSAGE_TRACER_C
   use M_XCAS_S

   !.. Implicit Declarations ..
   implicit none

   !.. Gestion des mots-cles ..
   type(FICHIER_T), intent(inout) :: FichierMotCle
   !.. Variables d'entree (maillage et hydraulique) ..
   type(CONNECT_T)               , intent(in   ) :: Connect
   type(APPORT_T)   ,dimension(:), intent(in   ) :: Apport
   type(PROFIL_T)   ,dimension(:), pointer       :: Profil       ! Profils geometriques
   real(DOUBLE)     ,dimension(:), pointer       :: X            ! Maillage
   Type(EXTREMITE_T),dimension(:), pointer       :: Extremite
   real(DOUBLE)                  , intent(in   ) :: TempsMaximum
   !.. Traceurs ..
   !
   logical                       , intent(  out) :: OptionTracer
   integer                                       :: Nbtrac
   integer                       , intent(  out) :: Modele_Qual_Eau
   integer                       , intent(  out) :: FreqCouplage
   type(CONSTANTES_TRACER_T), dimension(:)  , pointer :: ConsTrac
   type(COND_LIM_TRACER_T)  , dimension(:)  , pointer :: Cond_Lim
   type(SOURCE_TRACER_T)    , dimension(:)  , pointer :: Sources_tracer
   type(LOI_TRACER_T)       , dimension(:)  , pointer :: LoiTracer
   type(PARAMETRES_QUALITE_EAU_T)                     :: ParPhy
   type(METEO_T)                                      :: Meteo
   type(FICHIER_T)           , intent(inout)   :: FichierResuTracer
   type(FICHIER_T)           , intent(inout)   :: FichierListingTracer
   logical                   , intent(  out)   :: Presence_ConcIni
   type(FICHIER_T)           , intent(inout)   :: Fichier_Meteo
   type(FICHIER_T)           , intent(inout)   :: Fichier_Parphy
   logical                   , intent(  out)   :: ImpressionConcListing
   logical                   , intent(  out)   :: ImpressionConcIni
   logical                   , intent(  out)   :: ImpressionBilanTracer
   integer                   , intent(  out)   :: FormatResuTracer
   integer                   , intent(in   )   :: PasStockage
   integer                   , intent(in   )   :: PasImpression
   !.. Traitement des erreurs ..
   type(ERREUR_T)            , intent(inout)   :: Erreur

      ! Arguments specifiques pour l'interface
   type(FICHIER_T),  dimension(:), pointer       :: FichiersLoisTracer
   logical                       , intent(in   ) :: Impression

   ! VARIABLES LOCALES
   ! -----------------
   integer                                       :: post_processeur_tracer
   integer                       , parameter     :: POST_RUBENS  = 1
   integer                       , parameter     :: POST_OPTHYCA = 2
   logical  :: ImpressionLoiTracer
   integer  :: NbExtLibre, ult
   integer  i, ib, retour
   character(len=256)  :: pathNode
   character(len=8192) :: line
   character(len=256)  :: xcasFile
   integer             :: unitNum
   logical, allocatable :: ltab1(:),ltab2(:)
   integer, allocatable :: itab1(:),itab2(:)

   !=========================================================================
   ! INITIALISATION
   !=========================================================================
   Erreur%Numero = 0
   !arbredappel_old = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>PRETRAIT_TRACER'

   ! Open .xcas file
   unitNum = FichierMotCle%Unite
   xcasFile = FichierMotCle%Nom
   open(unit=unitNum, file=xcasFile, status="old", action="read", iostat=retour)
   if(retour.ne.0) then
    erreur%numero = 3
      erreur%ft     = err_3
      erreur%ft_c   = err_3c
      call traiter_erreur(erreur, xcasFile)
      return
   end if

   ! Couplage avec la qualite d'eau ? (implique 1 couplage Hydraulique / Convection Diffusion)
   pathNode = 'parametresTraceur'
   line = xcasReader(unitNum, pathNode)
   if(len(trim(line)).eq.0) then
       OptionTracer = .false.
   else
     pathNode = 'parametresTraceur/presenceTraceurs'
     line = xcasReader(unitNum, pathNode)
     read(unit=line, fmt=*) OptionTracer
   endif
   PretraitTracer : if (OptionTracer) then
      !=======================================================================
      !           LECTURE DES DONNEES PROPRES A TRACER
      !=======================================================================
      if (Impression) then
         !
         ! Fichier listing et de resultats
         ! -------------------------------
         pathNode = 'parametresTraceur/parametresImpressionTraceur/fichListTracer'
         FichierListingTracer%Nom = xcasReader(unitNum, pathNode)

         ult                      = FichierListingTracer%Unite

         pathNode = 'parametresTraceur/parametresImpressionTraceur/concentInit'
         line = xcasReader(unitNum, pathNode)
         read(unit=line, fmt=*) ImpressionConcIni

         pathNode = 'parametresTraceur/parametresImpressionTraceur/loiTracer'
         line = xcasReader(unitNum, pathNode)
         read(unit=line, fmt=*) ImpressionLoiTracer

         pathNode = 'parametresTraceur/parametresImpressionTraceur/concentrations'
         line = xcasReader(unitNum, pathNode)
         read(unit=line, fmt=*) ImpressionConcListing

         pathNode = 'parametresTraceur/parametresImpressionTraceur/bilanTracer'
         line = xcasReader(unitNum, pathNode)
         read(unit=line, fmt=*) ImpressionBilanTracer

         open( unit = ult , file = FichierListingTracer%Nom , access = 'SEQUENTIAL' , &
               action = 'WRITE' , form = 'FORMATTED' , iostat = RETOUR , &
               position = 'rewind' , status = 'REPLACE' )
         if( RETOUR /= 0 ) then
            Erreur%Numero = 4
            Erreur%ft     = err_4
            Erreur%ft_c   = err_4c
            call TRAITER_ERREUR( Erreur , FichierListingTracer%Nom )
            return
         end if

         pathNode = 'parametresTraceur/parametresImpressionTraceur/fichResultTracer'
         FichierResuTracer%Nom = xcasReader(unitNum, pathNode)

         pathNode = 'parametresTraceur/parametresImpressionTraceur/formatFichResultat'
         line = xcasReader(unitNum, pathNode)
         read(unit=line, fmt=*) post_processeur_tracer

         if( ( post_processeur_tracer /= POST_RUBENS ).and.( post_processeur_tracer /= POST_OPTHYCA ) ) then
            Erreur%Numero = 512
            Erreur%ft     = err_512
            Erreur%ft_c   = err_512c
            call TRAITER_ERREUR( Erreur , post_processeur_tracer )
            return
         endif

         if( post_processeur_tracer == POST_RUBENS ) then
            FormatResuTracer = FORMAT_STO_NONPERMANENT
         else if( post_processeur_tracer == POST_OPTHYCA ) then
            FormatResuTracer = FORMAT_STO_OPTHYCA
         endif

         write(ult,10630)
      else
         ImpressionConcIni     = .false.
         ImpressionLoiTracer   = .false.
         ImpressionConcListing = .false.
         ImpressionBilanTracer = .false.
      end if
      !
      ! Nombre de traceurs
      ! ------------------
      pathNode = 'parametresTraceur/nbTraceur'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) Nbtrac

      if (Impression) write(ult,10640) Nbtrac

      if( NbTrac > 10 ) Then
         Erreur%Numero = 508
         Erreur%ft     = err_508
         Erreur%ft_c   = err_508c
         call TRAITER_ERREUR( Erreur , 'Nombre de traceurs' , '< ou egal a 10' )
         return
      endif
      !
      ! Modele de qualite d'eau
      ! -----------------------
      !... Type de modele
      pathNode = 'parametresTraceur/parametresNumeriquesQualiteEau/modeleQualiteEau'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) Modele_Qual_Eau

      if (Impression) write(ult,10650) Modele_Qual_Eau

      if( Modele_Qual_Eau > 6 ) Then
         Erreur%Numero = 508
         Erreur%ft   = err_508
         Erreur%ft_c = err_508c
         call TRAITER_ERREUR  (Erreur, 'Modele de QE', '< ou egal a 6')
         return
      endif

      if( Modele_Qual_Eau.EQ.2 ) then  ! module O2
         Nbtrac = 3
      elseif( Modele_Qual_Eau.EQ.3 ) then  ! module BIOMASS
         Nbtrac = 5
      elseif( Modele_Qual_Eau.EQ.4 ) then  ! module EUTRO
         Nbtrac = 8
      elseif( Modele_Qual_Eau.EQ.5 ) then  ! module MICROPOL
         Nbtrac = 5
      elseif (Modele_Qual_Eau.EQ.6) then  ! module THERMIC
         Nbtrac = 1
      else
         Nbtrac = Nbtrac
      endif
      !
      !... Parametres physiques dependant du modele
      !
      if( Modele_Qual_Eau.NE.1 ) then
         call LEC_PARPHY( Fichier_ParPhy , ParPhy , Erreur )
      endif
      !
      !... Donnees meteo (pour les modeles biomass, eutro et thermic)
      !
      if( ( ( Modele_Qual_Eau.EQ.3 )   &
         .OR.(Modele_Qual_Eau.EQ.4) ) &
         .OR.(Modele_Qual_Eau.EQ.6)   ) then
         call LEC_METEO( Meteo , Fichier_Meteo , Modele_Qual_Eau , Erreur )
      endif
      !
      ! Parametres pour la convection / diffusion
      ! -----------------------------------------
      allocate( Constrac(nbtrac) , STAT = retour )

      allocate( ltab1(nbtrac) , STAT = retour )
      if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'ltab1' )
          return
      end if
      allocate( ltab2(nbtrac) , STAT = retour )
      if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'ltab2' )
          return
      end if

      pathNode = 'parametresTraceur/parametresConvectionDiffusion'
      line = xcasReader(unitNum, pathNode)
      if(len(trim(line)).eq.0) then
         print*,"Parse error => parametresConvectionDiffusion"
         call xerror(Erreur)
         return
      endif

      pathNode = 'parametresTraceur/parametresConvectionDiffusion/convectionTraceurs'
      line = xcasReader(unitNum, pathNode)
      if(len(trim(line)).eq.0) then
         print*,"Parse error => convectionTraceurs"
         call xerror(Erreur)
         return
      endif
      read(unit=line, fmt=*) ltab1

      pathNode = 'parametresTraceur/parametresConvectionDiffusion/diffusionTraceurs'
      line = xcasReader(unitNum, pathNode)
      if(len(trim(line)).eq.0) then
         print*,"Parse error => diffusionTraceurs"
         call xerror(Erreur)
         return
      endif
      read(unit=line, fmt=*) ltab2

      do ib = 1 , nbtrac
        Constrac(ib)%CONV             = ltab1(ib)

        pathNode = 'parametresTraceur/parametresConvectionDiffusion/optionConvection'
        line = xcasReader(unitNum, pathNode)
        read(unit=line, fmt=*) ConsTrac(ib)%Scheconv

        ConsTrac(ib)%DIFF             = ltab2(ib)

        pathNode = 'parametresTraceur/parametresConvectionDiffusion/optionCalculDiffusion'
        line = xcasReader(unitNum, pathNode)
        read(unit=line, fmt=*) ConsTrac(ib)%OptionCalculDisp

        pathNode = 'parametresTraceur/parametresConvectionDiffusion/coeffDiffusion1'
        line = xcasReader(unitNum, pathNode)
        read(unit=line, fmt=*) ConsTrac(ib)%CoefDiffu(1)

        pathNode = 'parametresTraceur/parametresConvectionDiffusion/coeffDiffusion2'
        line = xcasReader(unitNum, pathNode)
        read(unit=line, fmt=*) ConsTrac(ib)%CoefDiffu(2)

        pathNode = 'parametresTraceur/parametresConvectionDiffusion/ordreSchemaConvec'
        line = xcasReader(unitNum, pathNode)
        read(unit=line, fmt=*) ConsTrac(ib)%OrdreVF

        pathNode = 'parametresTraceur/parametresConvectionDiffusion/paramW'
        line = xcasReader(unitNum, pathNode)
        read(unit=line, fmt=*) ConsTrac(ib)%ParamW

        pathNode = 'parametresTraceur/parametresConvectionDiffusion/LimitPente'
        line = xcasReader(unitNum, pathNode)
        read(unit=line, fmt=*) ConsTrac(ib)%LimiteurPente
      end do

      if( ConsTrac(1)%Scheconv > 4 ) Then
         Erreur%Numero = 508
         Erreur%ft     = err_508
         Erreur%ft_c   = err_508c
         call TRAITER_ERREUR( Erreur , 'Schema de convection' , '< ou egal a 4' )
         return
      endif

      if( ConsTrac(1)%OrdreVF > 3 ) Then
         Erreur%Numero = 508
         Erreur%ft   = err_508
         Erreur%ft_c = err_508c
         call TRAITER_ERREUR  (Erreur, 'Ordre du schema VF', '< ou egal a 3')
         return
      endif

      if( ( ConsTrac(1)%ParamW < -1 ).or.( ConsTrac(1)%ParamW > 1 ) ) Then
         Erreur%Numero = 508
         Erreur%ft   = err_508
         Erreur%ft_c = err_508c
         call TRAITER_ERREUR  (Erreur, 'Parametre W', 'compris entre -1 et 1')
         return
      endif

      !   Cas particulier du modele MICROPOL :
      !   Traceurs relatifs aux sediments ni convectes ni diffuses
      if( Modele_Qual_Eau.EQ.5 ) then
         Constrac(2)%CONV             = .false.
         ConsTrac(2)%OptionCalculDisp = 0
         Constrac(5)%CONV             = .false.
         ConsTrac(5)%OptionCalculDisp = 0
      endif
      !
      ! Frequence de couplage entre hydraulique et TRACER
      ! -------------------------------------------------
      pathNode = 'parametresTraceur/parametresNumeriquesQualiteEau/frequenceCouplHydroTracer'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) FreqCouplage

      if( MOD(PasStockage, FreqCouplage) > EPS8 ) then
         Erreur%Numero = 506
         Erreur%ft     = err_506
         Erreur%ft_c   = err_506c
         call TRAITER_ERREUR( Erreur , 'Pas de stockage' )
         return
      end if

      if( MOD(PasImpression, FreqCouplage) > EPS8 ) then
         Erreur%Numero = 506
         Erreur%ft     = err_506
         Erreur%ft_c   = err_506c
         call TRAITER_ERREUR( Erreur , 'Pas d impression' )
         return
      end if

      if (Impression) write(ult,10655) FreqCouplage

      ! Concentrations initiales
      ! ------------------------
      ! presence de concentrations initiales
      !
      if( ImpressionConcIni ) write(ult,10660)

      pathNode = 'parametresTraceur/parametresConcentrationsInitialesTraceur/presenceConcInit'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) Presence_ConcIni

      if( ImpressionConcIni ) then
         if( Presence_ConcIni ) then
            write(ult,10690) 'OUI'
         else
            write(ult,10690) 'NON'
         endif
      endif

      !
      ! Lecture des lois
      ! ----------------
      call LEC_LOI_TRACER_INTERFACE( &
        LoiTracer           , & ! Tableau des lois tracer
        nbtrac              , & ! Nombre de traceurs
        FichiersLoisTracer  , & ! Fichier des lois tracer
        ImpressionLoiTracer , & ! Flag d'impression des lois tracer
        ult                 , & ! Unite logique fichier listing
        TempsMaximum        , & ! Temps maximum du calcul
        unitNum             , & ! Unite logique du fichier .xcas
        Erreur                )
      if( Erreur%Numero /= 0 ) then
         return
      endif
      !
      ! Sources de traceurs
      ! -------------------
      call LEC_SOURCE( &
      Sources_Tracer , & ! Tableau des sources tracer
              Apport , & ! Tableau des apports hydrauliques
             Connect , & ! Table de connectivite du reseau
                   X , & ! Maillage
           LoiTracer , & ! Lois Tracer
              Profil , & ! Profils geometriques
              nbtrac , & ! nb de traceurs
                 ult , & ! Unite listing
             unitNum , & ! Unite logique du fichier .xcas
              Erreur   & ! Erreur
                      )
      if( Erreur%Numero /= 0 ) then
         return
      endif
      !
      ! Conditions aux limites
      ! ----------------------
      !
      NbExtLibre = size(Extremite)
      allocate( Cond_Lim(NbExtLibre) , STAT = RETOUR )

      allocate( itab1(NbExtLibre) , STAT = retour )
      if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'itab1' )
          return
      end if
      allocate( itab2(NbExtLibre) , STAT = retour )
      if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'itab2' )
          return
      end if

      pathNode = 'parametresTraceur/parametresConditionsLimitesTraceur/typeCondLimTracer'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) itab1

      pathNode = 'parametresTraceur/parametresConditionsLimitesTraceur/numLoiCondLimTracer'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) itab2

      do i = 1 , NbExtlibre

         Cond_Lim(i)%Type       = itab1(i)
         Cond_Lim(i)%NumeroLoi  = itab2(i)

         if( Cond_Lim(i)%NumeroLoi <= 0 ) then
            Erreur%Numero = 585
            Erreur%ft     = err_585
            Erreur%ft_c   = err_585c
            call TRAITER_ERREUR( Erreur , i )
            return
         endif

         if( Cond_Lim(i)%NumeroLoi > size(LoiTracer) ) then
            Erreur%Numero = 586
            Erreur%ft   = err_586
            Erreur%ft_c = err_586c
            call TRAITER_ERREUR  (Erreur,i)
            return
         endif

      enddo

      deallocate(ltab1)
      deallocate(ltab2)
      deallocate(itab1)
      deallocate(itab2)

   endif PretraitTracer

   close(unitNum)

   !Erreur%arbredappel = arbredappel_old

   return

   10630 format (/,'PARAMETRES GENERAUX DU CALCUL AVEC TRACER',/, &
               &  '------------------------------------------',/)
   10640 format ('Nombre de traceurs                           : ',I2)
   10650 format ('Modele de qualite d''eau choisi              : ',I2,/,&
               &' [ 1 : Aucun / 2 : O2       / 3 : Biomass    ',/,&
               &'   4 : Eutro / 5 : Micropol / 6 : Thermic  ] ')
   10655 format ('Frequence de couplage hydraulique / Tracer   : ',I3)
   10660 format (/,'CONDITIONS INITIALES TRACER',/, &
               &  '----------------------------',/)
   10690 format ('Presence de concentrations initiales        : ',A3)

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

end subroutine PRETRAIT_Tracer_INTERFACE
