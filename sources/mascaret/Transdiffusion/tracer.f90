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

subroutine TRACER (       &
                          !  Resultats
                      C , & ! Concentration des traceurs
                          !  Donnees hydrauliques
              Q , A , B , & ! Donnees hydrauliques (lits mineur + majeur)
  Q_ANT , A_ANT , B_ANT , & ! Donnees hydrauliques au pas de temps precedent
                RH , ST , & ! Rayon hydraulique et frottement lit mineur
                 Qinjec , & ! Debit d'injection (apports)
                   ZREF , & ! Cote du fond
                          !  Donnees TRACER
               Cond_Lim , & ! Cond lim amont interpolees
          Source_Tracer , & ! Termes sources ajoutees aux traceurs
               ConsTrac , & ! Constantes lies au transport-diffusion
        Modele_Qual_Eau , & ! Modele de qualite d'eau considere
            PAR_QualEau , & ! Parametres lies au modele de qualite d'eau
                  Meteo , & ! Donnees meteo (pour les modeles Biomass, Eutro et Thermic)
               NodeTrac , & ! Connectivite Tracer
                          !  Modele
                      X , & ! Abscisses des sections de calcul
                 Nbtrac , & ! Nombre de traceurs
                 Nbsect , & ! Dimension spatiale des tableaux
            Singularite , & ! Singularite
                Connect , & ! Table de connectivite
                message , &
                          !  Donnees temporelles
                  TEMPS , & ! Temps
                          !  Bilan de Masse
                  MASSE , & ! Masse de traceurs
        FLUMAS , FLUENT , & ! Flux de traceurs
        FLUSOR , FLUSRC , & !
                          !  Etats
                     DT , & ! Pas de temps
                  IPASS , & ! Phase de calcul
           UniteListing , & ! Unite du fichier listing tracer
        ImpressionBilan , & ! Logique pour le bilan
              NbCourant , & ! Nombre de courant max
                 Erreur )

!*****************************************************************************
! PROGICIEL : TRACER         S. MANDELKERN - E.LEHMANN - N.GOUTAL - M.LUCK -
!                            F. ZAOUI
!
! VERSION : V8P4R0              EDF-CEREMA
!*****************************************************************************
!
!  FONCTION :
!  --------
!
!   Programme principal du module de tracage
!
!
!   SOUS-PROGRAMMES APPELANT :  SUPERVISEUR
!   -----------------------
!
!   SOUS-PROGRAMMES APPELES :   RESEQU, CALK, CALCSA, CALCS_*, BILMAS
!   -----------------------
!
!******************************************************************************

   use M_PRECISION
   use M_PARAMETRE_C
   use M_CONNECT_T
   use M_SINGULARITE_T
   use M_FICHIER_T
   use M_RESEQU_I
   use M_BILMAS_I
   use M_CONSTANTES_TRACER_T
   use M_CONSTANTES_CALCUL_C
   use M_SOURCE_TRACER_T
   use M_PARAMETRES_QUALITE_EAU_T
   use M_COND_LIM_TRACER_T
   use M_CONSTANTES_CALCUL_TRACER_C
   use M_CALCSA_I
   use M_CALCS_Rien_I
   use M_CALCS_O2_I
   use M_CALCS_BIOMASS_I
   use M_CALCS_EUTRO_I
   use M_CALCS_MICROPOL_I
   use M_CALCS_THERMIC_I
   use M_PARPHY_I
   use M_METEO_T
   use M_NODE_TRACER_T
   use M_ERREUR_T
   use M_TRAITER_ERREUR_I
   use M_MESSAGE_TRACER_C
   use M_MESSAGE_C
   use M_NODE_TRACER_T

   !==============================================================
   !           Declarations
   !==============================================================

   !.. Declaration Implicite ..
   implicit none

   !.. Arguments ..
   ! RESULTAT
   real(DOUBLE), dimension(:,:), intent(inout) :: C
   ! DONNEES HYDRAULIQUES
   real(DOUBLE), dimension(:)  , intent(in)    :: A
   real(DOUBLE), dimension(:)  , intent(in)    :: Q
   real(DOUBLE), dimension(:)  , intent(in)    :: B
   real(DOUBLE), dimension(:)  , intent(in)    :: QINJEC
   real(DOUBLE), dimension(:)  , intent(in)    :: RH , ZREF , ST
   real(DOUBLE), dimension(:)  , intent(in)    :: Q_ANT , A_ANT , B_ANT
   ! DONNEES TRACER
   integer, intent(in)                                     :: Modele_Qual_Eau
   type (CONSTANTES_TRACER_T) ,dimension(:) , intent(inout) :: ConsTrac
   type (PARAMETRES_QUALITE_EAU_T)          , intent(inout) :: Par_QualEau
   type (METEO_T)                           , intent(inout) :: Meteo
   type (NODE_TRACER_T)                     , intent(inout) :: NodeTrac
   type (SOURCE_TRACER_T)     ,dimension(:) , intent(inout) :: Source_Tracer
   type (COND_LIM_TRACER_T)   ,dimension(:) , intent(inout) :: Cond_Lim
   type (Singularite_T)       ,dimension(:) , intent(in   ) :: Singularite
   type (Erreur_T)                          , intent(inout) :: Erreur
   type (Fichier_T)                         , intent(in   ) :: message
   integer                                  , intent(in   ) :: UniteListing
   logical                                  , intent(in   ) :: ImpressionBilan
   real(DOUBLE)              ,dimension(:)     :: NbCourant
   ! MODELE
   real(DOUBLE), dimension(:)   ,intent(in)    :: X
   type(CONNECT_T)              ,intent(in   ) :: CONNECT
   integer                      ,intent(in)    :: Nbsect
   integer                      ,intent(inout) :: Nbtrac
   ! DONNEES TEMPORELLES
   real(DOUBLE)                 ,intent(in)    :: TEMPS
   ! ETATS
   real(DOUBLE)                 ,intent(in   ) :: DT
   integer                      ,intent(in)    :: IPASS
   !
   ! BILAN DE MASSE
   real(DOUBLE), dimension (:,:),intent(inout) :: MASSE , FLUMAS , FLUENT , FLUSOR , FLUSRC
   !.. Variables locales ..
   real(DOUBLE), dimension(Nbsect)             :: U            ! Vitesse de l'eau
   real(DOUBLE), dimension(Nbsect)             :: H            ! Hauteur d'eau
   real(DOUBLE), dimension(Nbsect,Nbtrac)      :: RNU , S , SEXP ! Termes sources implicites
   real(DOUBLE), dimension(Nbsect,Nbtrac)      :: RK           ! Coefficient de diffusion
   real(DOUBLE)                                :: CR
   integer                                     :: nb_noeud , nb_ext , nbbief , nbsing
   integer                                     :: IP , IS , INOEUD , IBIEF
   integer                                     :: num_sect , num_bief
   real(DOUBLE), dimension(size(Connect%OrigineBief)):: CL1 , CL2
   integer     , dimension(size(Connect%OrigineBief)):: TCL1 , TCL2
   logical, dimension(size(Connect%OrigineBief))     :: aval , resol
   real(DOUBLE)                                      :: Q_amont , Flux_amont
   integer :: I , J
   integer :: IM , NSAMB , NSAVB , ICL , NSCMP , nbb_aval
   integer :: retour
   integer :: nok,num_sect0,ip0,inoeud0
   logical :: nodeOK(size(Connect%NbBiefConfluence)),arret

   ! ... Fonctions intrinseques ...
   intrinsic ABS

   retour       = 0
   S(:,:)       = 0.d0
   NbCourant(:) = 0.d0
   RK(:,:)      = 0.d0
   U(:)         = 0.d0
   ! ... Initialisations ...
   nbbief   = size(Connect%OrigineBief)
   nbsing   = size(Singularite)
   nb_noeud = size(Connect%NbBiefConfluence)
   nb_ext   = size(Connect%NumBiefExtLibre)

   if( IPASS == PHASE_INITIALISATION ) then
      !***********************************************************************
      ! --------------------- TEMPS INITIAL---------------------------
      ! ------------ CALCUL DE LA MASSE INITIALE DE TRACEUR  --------
      !***********************************************************************
      if( ImpressionBilan ) then
         do j = 1 , nbtrac
            do ibief = 1 , nbbief
               NSAMB = Connect%OrigineBief(ibief)
               NSAVB = Connect%FinBief(ibief)
               IM    = NSAVB - NSAMB + 1
               call BILMAS( MASSE(ibief,j) , FLUMAS(ibief,j) , &
                           FLUENT(ibief,j) , FLUSOR(ibief,j) , &
                           FLUSRC(ibief,j) , S(NSAMB:NSAVB,j) , &
                           j , ibief , IM , &
                           DT , NSCMP , ConsTrac , &
                           C(NSAMB:NSAVB,J) , A(NSAMB:NSAVB) , &
                           X(NSAMB:NSAVB) , B(NSAMB:NSAVB) , &
                           U(NSAMB:NSAVB) , RK(NSAMB:NSAVB,j) , IPASS )
            enddo
         enddo
      endif
      !*****************************************************************
      ! Allocation et initialisation des listes de noeuds amont et aval
      !****************************************************************
      allocate(NodeTrac%NB_CHILD(nb_noeud),STAT=retour)
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft   = err_5
         Erreur%ft_c = err_5c
         call TRAITER_ERREUR  (Erreur, 'NodeTrac%NB_CHILD')
         !stop
         return
      end if
      NodeTrac%NB_CHILD(:) = 0
      allocate(NodeTrac%NB_PARENT(nb_noeud),STAT=retour)
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft   = err_5
         Erreur%ft_c = err_5c
         call TRAITER_ERREUR  (Erreur, 'NodeTrac%NB_PARENT')
         !stop
         return
      end if
      NodeTrac%NB_PARENT(:) = 0
      allocate(NodeTrac%CHILD(nb_noeud,nb_noeud),STAT=retour)
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft   = err_5
         Erreur%ft_c = err_5c
         call TRAITER_ERREUR  (Erreur, 'NodeTrac%CHILD')
         !stop
         return
      end if
      allocate(NodeTrac%PARENT(nb_noeud,nb_noeud),STAT=retour)
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft   = err_5
         Erreur%ft_c = err_5c
         call TRAITER_ERREUR  (Erreur, 'NodeTrac%PARENT')
         !stop
         return
      end if
      !***********************************************************
      ! ------- DETERMINATION DE LA CONNEXITE AMONT - AVAL -------
      !***********************************************************
      do inoeud = 1,nb_noeud
         do IP = 1,Connect%NbBiefConfluence(inoeud)
            num_sect = Connect%NumSectionConfluence(inoeud,IP)
            num_bief = Connect%NumBiefConfluence(inoeud,IP)
            ! Branche amont
            if( Connect%FinBief(num_bief) == num_sect ) then
               num_sect0 = Connect%OrigineBief(num_bief)
               do inoeud0 = 1,nb_noeud
                  arret = .false.
                  if(inoeud0.ne.inoeud) then
                     do ip0 = 1,Connect%NbBiefConfluence(inoeud0)
                        if(num_sect0.eq.Connect%NumSectionConfluence(inoeud0,ip0)) then
                           arret = .true.
                        endif
                        if(arret.eqv..true.) exit
                     enddo
                  endif
                  if(arret.eqv..true.) then
                     NodeTrac%NB_PARENT(inoeud) = NodeTrac%NB_PARENT(inoeud) + 1
                     NodeTrac%PARENT(inoeud,NodeTrac%NB_PARENT(inoeud)) = inoeud0
                  endif
               enddo
            endif
            ! Branche aval
            if( Connect%OrigineBief(num_bief) == num_sect ) then
               num_sect0 = Connect%FinBief(num_bief)
               do inoeud0 = 1,nb_noeud
                  arret = .false.
                  if(inoeud0.ne.inoeud) then
                     do ip0 = 1,Connect%NbBiefConfluence(inoeud0)
                        if(num_sect0.eq.Connect%NumSectionConfluence(inoeud0,ip0)) then
                           arret = .true.
                        endif
                        if(arret.eqv..true.) exit
                     enddo
                  endif
                  if(arret.eqv..true.) then
                     NodeTrac%NB_CHILD(inoeud) = NodeTrac%NB_CHILD(inoeud) + 1
                     NodeTrac%CHILD(inoeud,NodeTrac%NB_CHILD(inoeud)) = inoeud0
                  endif
               enddo
            endif
         enddo
      enddo
!
   else
   !***********************************************************************
   ! -------------- CALCUL A CHAQUE PAS DE TEMPS ------------------
   !***********************************************************************
   !==============================================================
   ! Calcul des variables hydrauliques pour TRACER
   !  (section mouillee, debit, largeur de lit,
   !   vitesse moyenne et profondeur moyenne)
   !==============================================================
      do I = 1 , Nbsect
         if( Constrac(1)%Scheconv.le.3 ) then
            if( abs(A(I)).gt.EPS15 ) then
               U(I) = Q(I) / A(I)
            else
               Erreur%Numero = 580
               Erreur%ft     = err_580
               Erreur%ft_c   = err_580c
               call TRAITER_ERREUR( Erreur , I )
               return
            endif
         else
            if( abs(A_ANT(I)).gt.EPS15 ) then
               U(I) = Q_ANT(I) / A_ANT(I)
            else
               Erreur%Numero = 580
               Erreur%ft     = err_580
               Erreur%ft_c   = err_580c
               call TRAITER_ERREUR( Erreur , I )
               return
            endif
         endif
         if( abs(B(I)).gt.EPS15 ) THEN
            H(I) = A(I) / B(I)
         else
            Erreur%Numero = 581
            Erreur%ft     = err_581
            Erreur%ft_c   = err_581c
            call TRAITER_ERREUR( Erreur , I )
            return
         endif
      enddo

      !==============================================================
      ! Cr maximum
      !==============================================================
      do ibief = 1 , nbbief
         do I = Connect%OrigineBief(ibief) , Connect%FinBief(ibief) - 1
            CR = U(I) * DT / ( X(I+1) - X(I) )
            if( CR >= NbCourant(ibief) ) then
               NbCourant(ibief) = CR
            endif
         enddo
      enddo

      !==============================================================
      ! Calcul du coefficient de diffusion
      !==============================================================
      do j = 1 , nbtrac
         if( ConsTrac(j)%Diff ) then
            call CALK( RK(1:Nbsect,j)        , &
                       ConsTrac(j)%CoefDiffu , &
                       U , ST , B , H , Nbsect , &
                       ConsTrac(j)%OptionCalculDisp )
         else
            do i = 1 , Nbsect
               RK(i,j) = 0.d0
            end do
         endif
      enddo

      !==============================================================
      ! Calcul des termes sources explicites volumiques et surfaciques
      ! ajoutees par l'utilisateur
      !==============================================================
      CALL CALCSA( S , Source_Tracer , Qinjec , H , X , A , Nbsect , Nbtrac , Erreur )

      !==============================================================
      ! Calcul des termes sources lies au modele de QE
      !==============================================================
      select case( Modele_Qual_Eau )

         case( AUCUN_MODELE )

            CALL CALCS_Rien( RNU , SEXP , &
                             Nbsect , Nbtrac , Nbsing , &
                             Q , A , H , RH , ST , C , &
                             S , Temps , DT )

         case( MODELE_O2 )

            CALL CALCS_O2( RNU , SEXP , &
                           Nbsect , Nbtrac , Nbsing , &
                           Singularite , &
                           Q , A , H , RH , ST , C , &
                           S , Temps , Par_QualEau , DT )

         case( MODELE_BIOMASS )

            CALL CALCS_Biomass( RNU , SEXP , &
                                Nbsect , Nbtrac , Nbsing , &
                                Q , A , H , RH , ST , C , &
                                S , Temps , Par_QualEau , &
                                Meteo , DT , Erreur )

         case( MODELE_EUTRO )

            CALL CALCS_Eutro( RNU , SEXP , &
                              Nbsect , Nbtrac , Nbsing , &
                              Singularite , &
                              Q , A , H , RH , ST , C , &
                              S , Temps , Par_QualEau , &
                              Meteo , DT , Erreur )

         case( MODELE_MICROPOL )

            CALL CALCS_Micropol( RNU , SEXP , &
                                 Nbsect , Nbtrac , Nbsing , &
                                 Q , A , H , RH , ST , C , &
                                 S , Temps , Par_QualEau , DT )

         case( MODELE_THERMIC )

            CALL CALCS_Thermic( RNU , SEXP , &
                                Nbsect , Nbtrac , Nbsing , &
                                Q , A , H , RH , ST , C , &
                                S , Temps , Par_QualEau , &
                                Meteo , DT , Erreur )

      end select

      !==============================================================
      ! Calcul des conditions aux limites a imposer sur les biefs
      ! (types et valeurs)
      !==============================================================
      ! Sur chaque bief, par defaut : toujours en conditions limites Dirichlet amont / Neumann aval valeurs mises a zero
      do ip = 1 , nbbief
         TCL1(ip) = 2
         TCL2(ip) = 1
         CL1(ip)   =0.
         CL2(ip) = 0.
      enddo

      ! Boucle sur les traceurs
      do J = 1 , nbtrac

         ! Boucle sur les conditions limites libres
         do ICL = 1 , nb_ext
            IP = Connect%NumBiefExtLibre(ICL)
            IS = Connect%NumSectionExtLibre(ICL)
            if( IS == Connect%OrigineBief(IP) ) then
               ! Extremites amont
               if( U(IS).GE.0.D0 ) then
                  CL1(IP)  = Cond_Lim(ICL)%Conc_lim(J)
               else                      ! si U<0 a l'amont, CL de type Neumann
                  TCL1(IP) = 1
               endif
            else
               ! Extremites aval
               if( U(IS).GE.0.D0 ) then
                  ! CL2(IP) = C(IS-1,J)
               else                      ! si U<0 a l'aval, CL de type Dirichlet
                  TCL2(IP) = 2
                  CL2(IP)  = C(IS,J)
               endif
            endif
         enddo

         !==============================================================
         ! Resolution de l'equation de transport
         ! (boucle sur les noeuds, puis sur les biefs)
         !==============================================================
         do ibief = 1 , nbbief
            resol(ibief) = .false.
         enddo
         nodeOK(:) = .FALSE.
         if( nb_noeud >= 1 ) then
            ! BOUCLE SUR LES NOEUDS
            nok = nb_noeud
            do while(nok.ne.0)
               do INOEUD = 1 , nb_noeud
                  if(nodeOK(inoeud).eqv..true.) cycle  ! noeud a deja ete traite
                  nbb_aval   = 0
                  Q_amont    = 0.d0
                  Flux_amont = 0.d0
                  ! Verification de la precedence du traitement
                  arret = .false.
                  do IP = 1,NodeTrac%NB_PARENT(inoeud)
                     if(nodeOK(NodeTrac%PARENT(inoeud,IP)).eqv..false.) then
                     arret = .true.
                     exit
                  endif
               enddo
               if(arret.eqv..true.) cycle ! un noeud amont n'a pas encore ete traite

               ! Recherche du bief aval
               do IP = 1 , Connect%NbBiefConfluence(inoeud)
                  num_sect       = Connect%NumSectionConfluence(inoeud,IP)
                  num_bief       = Connect%NumBiefConfluence(inoeud,IP)
                  aval(num_bief) = .false.
                     if( Connect%OrigineBief(num_bief) == num_sect ) then ! Branche aval
                     aval(num_bief) = .true.
                     nbb_aval       = nbb_aval + 1
                  endif
               enddo
               if( nbb_aval.ge.2 ) then ! Tracer pas prevu pour deffluents ...
                  Erreur%Numero = 582
                  Erreur%ft     = err_582
                  Erreur%ft_c   = err_582c
                  call TRAITER_ERREUR (Erreur,INOEUD)
                  return
               endif

               if( U(Connect%NumSectionConfluence(inoeud,1)).GE.0.D0 ) then   ! Vitesses positives au noeud => confluent "standard"
                  ! Resolution de l'eq. de transport pour les biefs amont
                  do IP = 1 , Connect%NbBiefConfluence(inoeud)
                     num_bief = Connect%NumBiefConfluence(inoeud,IP)
                     num_sect = Connect%NumSectionConfluence(inoeud,IP)
                     if( .not.aval(num_bief) ) then
                        if( .not.resol(num_bief) ) then
                           NSAMB = Connect%OrigineBief(num_bief)
                           NSAVB = Connect%FinBief(num_bief)
                           IM    = NSAVB - NSAMB + 1
                           call RESEQU( C(NSAMB:NSAVB,J) , A(NSAMB:NSAVB) , &
                             A_ANT(NSAMB:NSAVB)  , &
                             QINJEC(NSAMB:NSAVB) , U(NSAMB:NSAVB)     ,&
                             RK(NSAMB:NSAVB,J)   , SEXP(NSAMB:NSAVB,J),&
                             RNU(NSAMB:NSAVB,J)  ,                     &
                             TCL1(num_bief)      , TCL2(num_bief)     ,&
                             CL1(num_bief)       , CL2(num_bief)      ,&
                             X(NSAMB:NSAVB)      , ConsTrac(J)        ,&
                             IM                  , DT                 ,&
                             FLUENT(num_bief,j)  , FLUSOR(num_bief,j) ,&
                             message%nom         , erreur              )

                           resol(num_bief) = .true.
                        endif
                        Flux_amont = Flux_amont + C(num_sect,J) * Q(num_sect)
                        Q_amont    = Q_amont    + Q(num_sect)
                     endif
                  enddo
                  ! Resolution de l'eq. de transport pour le bief aval
                  do IP = 1 , Connect%NbBiefConfluence(inoeud)
                     num_bief = Connect%NumBiefConfluence(inoeud,IP)
                     if( aval(num_bief).and.(.not.resol(num_bief)) ) then
                        NSAMB = Connect%OrigineBief(num_bief)
                        NSAVB = Connect%FinBief(num_bief)
                        IM    = NSAVB - NSAMB + 1
                        if( abs(Q_amont).gt.EPS15 ) then
                           CL1(num_bief) = Flux_amont / Q(NSAMB)
                        else
                           CL1(num_bief) = 0.d0
                        endif
                        call RESEQU( C(NSAMB:NSAVB,J)    , A(NSAMB:NSAVB)     ,&
                                  A_ANT(NSAMB:NSAVB)  , &
                                  QINJEC(NSAMB:NSAVB) , U(NSAMB:NSAVB)     ,&
                                  RK(NSAMB:NSAVB,J)   , SEXP(NSAMB:NSAVB,J),&
                                  RNU(NSAMB:NSAVB,J)  ,                     &
                                  TCL1(num_bief)      , TCL2(num_bief)     ,&
                                  CL1(num_bief)       , CL2(num_bief)      ,&
                                  X(NSAMB:NSAVB)      , ConsTrac(J)        ,&
                                  IM                  , DT                 ,&
                                  FLUENT(num_bief,j)  , FLUSOR(num_bief,j) ,&
                                  message%nom         , erreur              )
                        resol(num_bief) = .true.
                     endif
                  enddo
                  nodeOK(inoeud) = .true.
                  nok = nok - 1
               else    ! Si les vitesses sont negatives au passage du noeud, on se trouve dans une configuration de type Deffluent
                       ! => on inverse l'ordre de resolution

                  if( nb_noeud > 1 ) then
                     Erreur%Numero = 587
                     Erreur%ft     = err_587
                     Erreur%ft_c   = err_587c
                     call TRAITER_ERREUR( Erreur , nb_noeud )
                     return
                  endif

                  ! Resolution de l'eq. de transport pour le bief aval
                  do IP = 1 , Connect%NbBiefConfluence(inoeud)
                     num_bief = Connect%NumBiefConfluence(inoeud,IP)
                     num_sect = Connect%NumSectionConfluence(inoeud,IP)
                     if( aval(num_bief).and.(.not.resol(num_bief)) ) then
                        TCL1(num_bief) = 1
                        TCL2(num_bief) = 2
                        NSAMB = Connect%OrigineBief(num_bief)
                        NSAVB = Connect%FinBief(num_bief)
                        IM    = NSAVB - NSAMB + 1
                        call RESEQU( C(NSAMB:NSAVB,J)    , A(NSAMB:NSAVB)     ,&
                                  A_ANT(NSAMB:NSAVB)  , &
                                  QINJEC(NSAMB:NSAVB) , U(NSAMB:NSAVB)     ,&
                                  RK(NSAMB:NSAVB,J)   , SEXP(NSAMB:NSAVB,J),&
                                  RNU(NSAMB:NSAVB,J)  ,                     &
                                  TCL1(num_bief)      , TCL2(num_bief)     ,&
                                  CL1(num_bief)       , CL2(num_bief)      ,&
                                  X(NSAMB:NSAVB)      , ConsTrac(J)        ,&
                                  IM                  , DT                 ,&
                                  FLUENT(num_bief,j)  , FLUSOR(num_bief,j) ,&
                                  message%nom         , erreur              )

                        resol(num_bief) = .true.
                        Flux_amont = C(NSAMB,J)*Q(NSAMB)
                        Q_amont    = Q(NSAMB)
                     endif
                  enddo
                  ! Resolution de l'eq. de transport pour les biefs amont
                  do IP = 1 , Connect%NbBiefConfluence(inoeud)
                     num_bief = Connect%NumBiefConfluence(inoeud,IP)
                     if( .not.aval(num_bief).and.(.not.resol(num_bief)) ) then
                        NSAMB = Connect%OrigineBief(num_bief)
                        NSAVB = Connect%FinBief(num_bief)
                        IM    = NSAVB - NSAMB + 1
                        TCL1(num_bief) = 1
                        TCL2(num_bief) = 2
                        CL2(num_bief) = Flux_amont / Q_amont   ! conservation de la concentration avant deffluence
                        call RESEQU( C(NSAMB:NSAVB,J)    , A(NSAMB:NSAVB)     ,&
                               A_ANT(NSAMB:NSAVB)  , &
                               QINJEC(NSAMB:NSAVB) , U(NSAMB:NSAVB)     ,&
                               RK(NSAMB:NSAVB,J)   , SEXP(NSAMB:NSAVB,J),&
                               RNU(NSAMB:NSAVB,J)  ,                     &
                               TCL1(num_bief)      , TCL2(num_bief)     ,&
                               CL1(num_bief)       , CL2(num_bief)      ,&
                               X(NSAMB:NSAVB)      , ConsTrac(J)        ,&
                               IM                  , DT                 ,&
                               FLUENT(num_bief,j)  , FLUSOR(num_bief,j) ,&
                               message%nom         , erreur              )
                        resol(num_bief) = .true.
                     endif
                  enddo
                  nodeOK(inoeud) = .true.
                  nok = nok - 1
               endif
            enddo ! Fin de la BOUCLE SUR LES NOEUDS
         end do ! Fin de la boucle de repetition

         ! CAS D'UN BIEF UNIQUE
         else

            NSAMB = Connect%OrigineBief(1)
            NSAVB = Connect%FinBief(1)
            IM    = NSAVB - NSAMB + 1
            call RESEQU( C(NSAMB:NSAVB,J)    , A(NSAMB:NSAVB)     ,&
                A_ANT(NSAMB:NSAVB)  , &
                QINJEC(NSAMB:NSAVB) , U(NSAMB:NSAVB)     ,&
                RK(NSAMB:NSAVB,J)   , SEXP(NSAMB:NSAVB,J),&
                RNU(NSAMB:NSAVB,J)  ,                     &
                TCL1(1)             , TCL2(1)            ,&
                CL1(1)              , CL2(1)             ,&
                X(NSAMB:NSAVB)      , ConsTrac(J)        ,&
                IM                  , DT                 ,&
                FLUENT(1,j)         , FLUSOR(1,j)        ,&
                message%nom         , erreur              )

         endif

         !==============================================================
         ! Bilan de masse
         ! (boucle sur les biefs)
         !==============================================================
         if( ImpressionBilan ) then
            do ibief = 1 , nbbief
               NSAMB = Connect%OrigineBief(ibief)
               NSAVB = Connect%FinBief(ibief)
               IM    = NSAVB - NSAMB + 1
               call BILMAS ( MASSE(ibief,j)  , FLUMAS(ibief,j)   ,       &
                       FLUENT(ibief,j) , FLUSOR(ibief,j)   ,       &
                       FLUSRC(ibief,j) , S(NSAMB:NSAVB,j)  ,       &
                       j      , ibief  , IM                ,       &
                       DT     , NSCMP  , ConsTrac          ,       &
                       C(NSAMB:NSAVB,J), A(NSAMB:NSAVB)    ,       &
                       X(NSAMB:NSAVB)  , B(NSAMB:NSAVB)    ,       &
                       U(NSAMB:NSAVB)  , RK(NSAMB:NSAVB,J) , IPASS )
            enddo ! Fin de la boucle sur les biefs
         endif

      enddo   ! Fin de la boucle sur les traceurs

   endif

   return

end subroutine TRACER
