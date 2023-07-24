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

subroutine CALC_PC_CONFLU ( &
    PCSing                , &
    Z                     , &
    Q                     , &
    X                     , &
    ZREF                  , &
    Profil                , &
    ProfilPlan            , &
    Confluent             , &
    Abaque                , &
    IDT                   , &
    XDT                   , &
    Connect               , &
    UniteListing          , &
    Erreur                  &
                          )

! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P.CHERUBINI
!                             S. PERON
!                             S. MANDELKERN
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************

   !============================== Instructions ================================
   use M_PRECISION        ! type DOUBLE
   use M_PARAMETRE_C      ! EPS3
   use M_MESSAGE_C        ! Liste des messages d'erreur
   use M_CONNECT_T        ! type CONNECT_T
   use M_CONFLUENT_T      ! type CONFLUENT_T
   use M_ERREUR_T         ! type ERREUR_T
   use M_PROFIL_T         ! type PROFIL_T
   use M_PROFIL_PLAN_T    ! type PROFIL_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs
   use M_RHSBP_S          ! sous-programme RHSBP_SECTION_S
   use M_INTERPOLATION_S

   ! Arguments
   real(DOUBLE)        , dimension(:)  , intent(  out) :: PCSing
   real(DOUBLE)        , dimension(:)  , intent(in   ) :: Z
   real(DOUBLE)        , dimension(:)  , intent(in   ) :: Q
   real(DOUBLE)        , dimension(:)    , intent(in   ) :: X
   real(DOUBLE)        , dimension(:)    , intent(in   ) :: ZREF
   type(PROFIL_T)      , dimension(:)    , intent(in   ) :: Profil
   type(PROFIL_PLAN_T)                   , intent(in   ) :: ProfilPlan
   type(CONFLUENT_T)   , dimension(:)    , intent(in   ) :: Confluent
   real(DOUBLE)        , dimension(:,:,:), intent(in   ) :: Abaque
   integer             , dimension(:)    , intent(in   ) :: IDT
   real(DOUBLE)        , dimension(:)    , intent(in   ) :: XDT
   type(CONNECT_T)                       , intent(in   ) :: Connect
   integer                               , intent(in   ) :: UniteListing
   type(ERREUR_T)                        , intent(inout) :: Erreur
   ! Constantes
   integer, parameter :: TYPE_RIVIERE_NATURELLE = 1
   integer, parameter :: TYPE_DESIGNATION       = 2
   integer, parameter :: TYPE_DERNIERE_SECTION  = 3
   integer, parameter :: BRANCHE_AMONT          = 1
   integer, parameter :: BRANCHE_AVAL           = 0
   integer, parameter :: BRANCHE_PRINC_AMONT    = 1
   integer, parameter :: BRANCHE_PRINC_AVAL     = 0
   integer, parameter :: BRANCHE_LATERALE       = 2
   integer, parameter :: ABAQUE_A               = 1
   integer, parameter :: ABAQUE_B               = 2
   integer, parameter :: ABAQUE_C               = 3
   ! Variables locales
   type CONF_T
      sequence
      integer     , dimension(3) :: Section
      real(DOUBLE), dimension(3) :: Debit
      real(DOUBLE), dimension(3) :: Largeur
      integer     , dimension(3) :: Position
      integer     , dimension(3) :: Nature
   end type CONF_T
   type(CONF_T)                  :: conf
   integer         :: j,k
   integer         :: iabaque       ! compteur sur les branches ou placer une PDCS
   integer         :: jabaque       ! compteur sur les abaques
   integer         :: numero_abaque ! numero de l'abaque a utiliser
   integer         :: nb_ext
   integer         :: num_bief
   integer         :: num_section
   logical         :: debut_bief
   real(DOUBLE)    :: C6abc(3)
   real(DOUBLE)    :: alpha_C6(6),QC6(5)
   real(DOUBLE)    :: C6Qinf(6),C6Qsup(6)
   real(DOUBLE)    :: C6(2),C6a,C6b,C6c
   real(DOUBLE)    :: angle_conf
   real(DOUBLE)    :: Lf,Ll,Lp,Llp,Qp,Ql,Qpl
   real(DOUBLE)    :: elargissement          ! elargissement branche amont princ / branche aval
   real(DOUBLE)    :: Qinf,Qsup,c6_inf,c6_sup
   real(DOUBLE)    :: rapport
   real(DOUBLE)    :: q_max
   integer         :: type_riviere
   integer         :: inoeu    ! Compteur sur les noeuds
   integer         :: iprof    ! compteur sur les profils
   integer         :: isect    ! compteur sur les sections
   integer         :: ibief    ! compteur sur les sections
   integer         :: num_prof
   real(DOUBLE)    :: abs_prof
   real(DOUBLE)    :: xsect, xprof
   real(DOUBLE)    :: dxmin
   integer         :: idxmin
   real(DOUBLE)    :: xprofc(size(Connect%NbBiefConfluence),5)
   !Character(132)  :: !arbredappel_old ! ancien arbre d'appel
   data alpha_C6 /30.d0,40.d0,50.d0,60.d0,70.d0,80.d0/
   data QC6   /1.d0,1.5d0,2.d0,3.d0,5.d0/

   !============================== Instructions ================================
   ! INITIALISATIONS
   !----------------
   Erreur%Numero = 0
   xprofc(:,:) = 0.d0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>CALC_PC_CONFLU'
   type_riviere = TYPE_DERNIERE_SECTION

   ! BOUCLE SUR LES NOEUDS
   label_noeud : do inoeu = 1 , size(Connect%NbBiefConfluence)

      ! INITIALISATIONS
      !----------------
      nb_branche_amont = 0
      nb_branche_aval  = 0

      ! Detection des extremites amont et aval de la confluence
      ! -------------------------------------------------------
      ! Nombre d extremites liees a ce noeud
      nb_ext = Connect%NbBiefConfluence(inoeu)

      label_ext : do iext = 1 , size(Connect%NumBiefConfluence(inoeu,:))

         ! Numero de la branche
         num_bief    = Connect%NumBiefConfluence(inoeu,iext)

         ! numero de la section extremite de la branche
         num_section = Connect%NumSectionConfluence(inoeu,iext)

         ! Position de l'extremite debut ou fin de branche
         debut_bief = .false.
         do ibief = 1 , size(Connect%OrigineBief(:))
            if( num_section == Connect%OrigineBief(ibief) ) then
               debut_bief = .true.
               exit
            endif
         end do

         ! Numero de la section
         ! Si option riviere naturelle
         if( type_riviere == TYPE_RIVIERE_NATURELLE ) then

            ! abscisse de la section extremite de la branche
            xsect = X(num_section)

            ! recherche du numero du profil correspondant a cette extremite
            do iprof = 1,size(Profil)
               xprof = Profil(iprof)%AbsAbs
               if( dabs(xprof - xsect) < EPS3 ) then
                  num_prof = iprof
                  exit
               endif
            enddo

            ! si on est en debut de branche, on avance d'un profil
            if( debut_bief ) then
               num_prof = num_prof + 1
            else
               ! Si fin de branche
               num_prof = num_prof - 1
            endif

            ! recherche du numero de la section la plus proche du profil
            dxmin  = 1000._DOUBLE
            idxmin = 0

            abs_prof = Profil(num_prof)%AbsAbs

            do isect = 1 , size(X)
               if( dabs( X(isect) - abs_prof ) <= dxmin ) then
                  dxmin  = dabs( X(isect) - abs_prof )
                  idxmin = isect
               endif
            enddo

            ! le numero de la section est donc
            conf%Section(iext) = idxmin

         ! designation des profils en abscisses absolues
         elseif( type_riviere == TYPE_DESIGNATION ) then

            ! recherche du numero du section la plus proche du profil
            dxmin  = 1000._DOUBLE
            idxmin = 0

            do isect = 1 , size(X)
               if( dabs( X(isect) - xprofc(inoeu,iext) ) <= dxmin ) then
                  dxmin  = dabs( X(isect) - xprofc(inoeu,iext) )
                  idxmin = isect
               endif
            enddo

            ! le numero est donc
            conf%Section(iext) = idxmin

         ! dernier profil de la branche
         elseif( type_riviere == TYPE_DERNIERE_SECTION ) then

            if( debut_bief ) then
               conf%Section(iext) = Connect%OrigineBief(num_bief)
            else
               conf%Section(iext) = Connect%FinBief(num_bief)
            endif

         endif

         if( debut_bief ) then
            if( Q(conf%Section(iext)) > 0._DOUBLE ) then
               conf%position(iext) = BRANCHE_AVAL
            elseif( Q(conf%Section(iext)) < 0._DOUBLE ) then
               conf%position(iext) = BRANCHE_AMONT
               nb_branche_amont    = nb_branche_amont + 1
            else
               Erreur%Numero = 86
               Erreur%ft     = err_86
               Erreur%ft_c   = err_86c
               call TRAITER_ERREUR( Erreur , inoeu , iext )
               return
            endif
         else
            if( Q(conf%Section(iext)) > 0._DOUBLE ) then
               conf%position(iext) = BRANCHE_AMONT
               nb_branche_amont    = nb_branche_amont + 1
            elseif( Q(conf%Section(iext)) < 0._DOUBLE ) then
               conf%position(iext) = BRANCHE_AVAL
            else
               Erreur%Numero = 86
               Erreur%ft     = err_86
               Erreur%ft_c   = err_86c
               call TRAITER_ERREUR( Erreur , inoeu , iext )
               return
            endif
         endif

         conf%Debit(iext) = Q(conf%Section(iext))
         num_sect         = conf%Section(iext)

         call RHSBP_SECTION_S  ( &
           conf%Largeur(iext)  , & ! Variable a interpoler sur les profils
           ZREF(num_sect)      , & ! Cote du fond a la section de calcul
           Z(num_sect)         , & ! Cote d'eau   a la section de calcul
           IDT(num_sect)       , & ! Indices du profil de donnees amont
           XDT(num_sect)       , & ! Position de la section / profils
           Profil              , & ! Profils geometriques
           ProfilPlan%B1       , & ! Variable planimetree
           Erreur                & ! Erreur
                            )
         if( Erreur%Numero /= 0 ) then
            return
         endif

      end do label_ext

      ! TEST NBRE BRANCHES AMONT
      if( nb_branche_amont > 2 ) then
         Erreur%Numero = 87
         Erreur%ft     = err_87
         Erreur%ft_c   = err_87c
         call TRAITER_ERREUR( Erreur , nb_branche_amont , inoeu )
         return
      endif

      ! Calcul de Qpl=Qp/Ql
      ! -------------------
      q_max = 0._DOUBLE

      do iext = 1 , nb_ext

         if( conf%position(iext) == BRANCHE_AVAL ) then
            ! BRANCHE AVAL
            conf%Nature(iext) = BRANCHE_PRINC_AVAL
            nb_branche_aval   = nb_branche_aval + 1
            ! TEST DEFLUENCE
            if( nb_branche_aval > 1 ) then
               Erreur%Numero = 88
               Erreur%ft     = err_88
               Erreur%ft_c   = err_88c
               call TRAITER_ERREUR( Erreur , nb_branche_aval , inoeu )
               return
            endif
         else
            q_max = dmax1( conf%Debit(iext) , q_max )
         endif

      end do

      do iext = 1 , nb_ext
         if( conf%Position(iext) == BRANCHE_AMONT .and. abs(conf%Debit(iext)-q_max).lt.EPS15 ) then
            conf%Nature(iext) = BRANCHE_PRINC_AMONT
            Qp                = conf%Debit(iext)
         else if( conf%Position(iext) == BRANCHE_AMONT .and. conf%Debit(iext) < q_max ) then
            conf%Nature(iext) = BRANCHE_LATERALE
            Ql                = conf%Debit(iext)
         endif
      end do

      Qpl = Qp / Ql

      ! TEST DOMAINE
      !-------------
      if( Qpl > 5._DOUBLE ) then
         if (UniteListing>0) then
             write (UniteListing,10000)
         endif
      endif

      ! Calcul de Llp=Ll/Lp
      ! -------------------
      do iext = 1 , nb_ext
         if( conf%Nature(iext) == BRANCHE_PRINC_AMONT ) Lp = conf%Largeur(iext)
         if( conf%Nature(iext) == BRANCHE_LATERALE )    Ll = conf%Largeur(iext)
         if( conf%Nature(iext) == BRANCHE_PRINC_AVAL )  Lf = conf%Largeur(iext)
      end do

      Llp             = Ll / Lp
      elargissement   = Lf - Lp

      ! TEST DOMAINE
      !-------------
      if( Llp < 0.25_DOUBLE ) then
         if (UniteListing>0) then
            write(UniteListing,10010)
         endif
      endif

      if( Llp > 0.625_DOUBLE ) then
         if (UniteListing>0) then
            write(UniteListing,10020)
         endif
      endif

      ! Prise en compte de l'angle (il est en radians dans Confluent)
      ! -------------------------------------------------------------
      do iext = 1 , nb_ext
         if( conf%Nature(iext) == BRANCHE_PRINC_AMONT ) angle1 = real(Confluent(inoeu)%AngleAfflu(iext))
         if( conf%Nature(iext) == BRANCHE_LATERALE )    angle2 = real(Confluent(inoeu)%AngleAfflu(iext))
      end do

      angle_conf = abs( angle1 - angle2 )


      ! CALCUL DES PERTES DE CHARGE
      !----------------------------
      do iabaque = BRANCHE_PRINC_AMONT , BRANCHE_LATERALE

         do jabaque = ABAQUE_A, ABAQUE_C

            numero_abaque = (iabaque - 1) * 3 + jabaque

            ! Chargement de C6 pour alpha_C6=30->80 pour Q> & < Qp/Ql
            ! ----------------------------------------------------
            do j = 1 , (5 - 1)

               if( Qpl >= QC6(j) .and. Qpl <= QC6(j+1) ) then

                  Qinf = QC6(j)
                  Qsup = QC6(j+1)

                  do k = 1 , 6
                     C6Qinf(k) = Abaque( numero_abaque , k , j )
                     C6Qsup(k) = Abaque( numero_abaque , k , j+1 )
                  end do

                  exit

               endif

            end do

            ! Calcul de C6a, C6b, C6c
            ! -----------------------

            call INTERPOLATION_S                       &
            ( c6_inf ,                                 &
              angle_conf , 1 , alpha_C6 , C6Qinf , 6 , &
              Erreur )

            if( Erreur%Numero /= 0 ) then
               return
            endif

            call INTERPOLATION_S                      &
            ( c6_sup ,                                &
              angle_conf , 1 , alpha_C6 , C6Qsup , 6 ,&
              Erreur )

            if( Erreur%Numero /= 0 ) then
               return
            endif

            ! RESULTAT
            rapport        = ( Qpl - Qinf ) / ( Qsup - Qinf )
            C6abc(jabaque) = ( 1._DOUBLE - rapport ) * c6_inf + rapport * c6_sup

         end do ! de jabaque

         ! Calcul de C6
         ! ------------
         C6a = C6abc(1)
         C6b = C6abc(2)
         C6c = C6abc(3)

         C6(iabaque) = ( ( Llp - 0.25_DOUBLE ) - 1.5_DOUBLE  * &
                       ( elargissement / Lp ) ) /              &
                        .375_DOUBLE *                          &
                       ( C6b - C6a ) +                         &
                        4._DOUBLE * ( elargissement / Lp ) *   &
                       ( C6c - C6a ) + C6a

         ! -----------------------------------
         ! Prise en compte de C6 dans PDCSing,
         ! tableau des Pertes de charge
         ! -----------------------------------
         do iext = 1 , nb_ext
            if( conf%Nature(iext) == iabaque ) then
               PCSing(conf%Section(iext)) = C6(iabaque)
            endif
         end do

      end do ! de iabaque

      ! FIN DE LA BOUCLE SUR LES CONFLUENCES
      ! ------------------------------------

      if (UniteListing>0) then
         write(UniteListing, 10030) inoeu, C6(1), C6(2)
      endif

   end do label_noeud

   return

   10000 format (                                                 &
        /'<<ATTENTION>> : Sous-programme CALC_PC_CONFLU',/,      &
         '--------------- Qp/Ql > 5, hors domaine de validite')

   10010 format (                                                 &
        /'<<ATTENTION>> : Sous-programme CALC_PC_CONFLU',/,      &
         '--------------- Ll/Lp < 0.25, hors domaine de validite')

   10020 format (                                                 &
        /'<<ATTENTION>> : Sous-programme CALC_PC_CONFLU',/,      &
         '--------------- Ll/Lp > 0.625, hors domaine de validite')

   10030 format (                                                 &
        /'<<INFO>> : Calcul automatique des pertes de charge aux confluences.',/,&
         '---------- A la confluence n0 : ',i3,                /,&
         '           PDCSing branche principale = ',f12.3,     /,&
         '           PDCSing branche laterale   = ',f12.3)

end subroutine CALC_PC_CONFLU
