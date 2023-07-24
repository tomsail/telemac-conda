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

subroutine PSING ( &
     ZAm         , & ! Cote de la section amont de la singularite
     Singularite , & ! Singularite
     ZREF        , & ! Cote du fond a la section amont
     ZAV         , & ! Cote de la section aval de la singularite
     QAM         , & ! Debit a la section amont de la singularite
     Profil      , & ! Profils geometriques
     B1Plan      , & ! Largeur au miroir planimetree
     IDT         , & ! numeros des profils amont des sections
     XDT         , & ! Positionnement des sections / profils
     Section     , & ! Numero de la section de la singularite
     Temps       , & ! Temps ou numero de la ligne d'eau a calculer
     Erreur        & ! Erreur
                 )

! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             S. PERON
!                             S. MANDELKERN
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
!
!  FONCTION :
!  ----------
!
!             CALCUL DE LA COTE A L'AMONT D'UNE SINGULARITE
!             EN REGIME PERMANENT
!
!-----------------------------------------------------------------------
!
!  FICHIERS ENTREE/SORTIE :
!  ----------------------
!
!  SOUS PROGRAMME APPELANT :  PERMAT
!  -------------------------
!  SOUS PROGRAMMES APPELES :  INTERPOLATION_S
!  -------------------------
!
!  COMMENTAIRES :
!  ------------
!
!  . LES CALCULS DEPENDENT DU TYPE DE LA SINGULARITE
!  . SI LA SINGULARITE EST DEFINIE AU MOYEN D'UNE FAMILLE DE COURBES
!    (TYPE 1), LA COTE AMONT EST OBTENUE DIRECTEMENT AU MOYEN
!    D'INTERPOLATIONS SUR CES COURBES
!  . SI LA SINGULARITE EST DEFINIE AU MOYEN D'UNE LOI
!    Q = F ( ZAMONT , ZAVAL) (TYPE 2) , LA COTE AMONT EST
!    ESTIMEE INITIALEMENT EN SUPPOSANT LE REGIME DENOYE, PUIS ELLE
!    EST MODIFIEE LE  CAS ECHEANT JUSQU'A OBTENIR LE DEBIT CORRECT
!  . SI LA SINGULARITE EST DE TYPE 3, LA COTE AMONT EST ESTIMEE
!    DE MANIERE SEMBLABLE, EN ASSIMILANT LA CHARGE A LA HAUTEUR
!    AU DESSUS DU SEUIL
!  . SI LA SINGULARITE EST DE TYPE 4 OU 5 LA SOLUTION EST
!    IMMEDIATE
!  . LES TYPES SUIVANTS NE SONT PAS ADMIS EN PERMANENT
!
!    EN REGIME NOYE , LA CORRECTION EST DONNEE PAR LE COEFFICIENT C :
!    RH=(HAVAL-Singularite%CoteCrete)/(HAMONT-Singularite%CoteCrete)
!       ---          RH < 0.8   C= +1
!       ---   0.8  < RH < 1.0   C= C1*RH**3 + C2*RH**2 + C3*RH + C4
! **********************************************************************

   !============================= Declarations ===========================
   !
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C      ! EPS3, W23, W32
   use M_MESSAGE_C        ! Messages d'erreur
   use M_ERREUR_T         ! Type ERREUR_T
   use M_PROFIL_T         ! Type PROFIL_T
   use M_SINGULARITE_T    ! Type SINGULARITE_T
   use M_INTERPOLATION_S  ! Sous-programme INTERPOLATION_S
   use M_RHSBP_S          ! Sous programme RHSBP_SECTION_S
   use M_TRAITER_ERREUR_I ! Traitement de l'erreur

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !----------------
   real(DOUBLE)       ,                  intent(inout) :: ZAm
   type(SINGULARITE_T),                  intent(in   ) :: Singularite
   real(DOUBLE)       ,                  intent(in   ) :: ZREF
   real(DOUBLE)       ,                  intent(in   ) :: ZAV
   real(DOUBLE)       ,                  intent(in   ) :: QAM
   type(PROFIL_T)     , dimension(:)  ,  intent(in   ) :: Profil
   real(DOUBLE)       , dimension(:,:),  intent(in   ) :: B1Plan
   integer            , dimension(:)  ,  intent(in   ) :: IDT
   real(DOUBLE)       , dimension(:)  ,  intent(in   ) :: XDT
   integer                            ,  intent(in   ) :: Section
   real(DOUBLE)       ,                  intent(in   ) :: Temps
   type(ERREUR_T)     ,                  intent(inout) :: Erreur
   !.. Constantes ..
   !----------------
   real(DOUBLE), parameter :: RDG    =  4.429447_DOUBLE ! racine(2*G)
   real(DOUBLE), parameter :: CDENOY =  0.8_DOUBLE      ! Seuil pour RH indicateur
                                                        ! du regime noye/denoye
   real(DOUBLE), parameter :: C1     =   0._DOUBLE      ! Coefficients
   real(DOUBLE), parameter :: C2     = -25._DOUBLE      ! de la loi
   real(DOUBLE), parameter :: C3     =  40._DOUBLE      ! en regime
   real(DOUBLE), parameter :: C4     = -15._DOUBLE      ! noye
   real(DOUBLE), parameter :: D1     = 0.385_DOUBLE     ! Coefficients de la
   real(DOUBLE), parameter :: D2     = 1.5_DOUBLE       ! loi en regime noye seuil mince
   integer     , parameter :: PAS         =   10        ! Pas pour la convergence
   integer     , parameter :: NB_ITER_MAX = 1000        ! Nombre d'iteration maximal
   !.. Variables Locales ..
   !-----------------------
   real(DOUBLE) :: ALPHA
   real(DOUBLE) :: DCH
   real(DOUBLE) :: DX
   real(DOUBLE) :: DZ
   real(DOUBLE) :: EPSQ
   real(DOUBLE) :: charge_amont , charge_aval
   real(DOUBLE) :: charge
   real(DOUBLE) :: largeur_seuil
   real(DOUBLE) :: Q1 , Q2
   real(DOUBLE) :: QAMONT
   real(DOUBLE) :: RH
   real(DOUBLE) :: ZAM1 , ZAM2
   real(DOUBLE) :: cote_crete
   integer      :: IQ
   integer      :: ipoint
   integer      :: num_iter        ! numero d'iteration
   integer      :: SENS
   !character(132) :: !arbredappel_old ! ancien arbre d'appel

   !============================= Instructions ===========================
   ! INITIALISATIONS
   ! ---------------
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%Arbredappel = trim(!Erreur%arbredappel)//'=>PSING'

   ! CALCUL SUIVANT LE TYPE DE LA SINGULARITE
   ! ----------------------------------------
   select case( Singularite%Type )

      case( SINGULARITE_TYPE_ZAMONT_ZAVAL_Q )

         ! RECHERCHE DES DEBITS ENCADRANT LE DEBIT AMONT
         Q1 = Singularite%PtQ(1)
         Q2 = Singularite%PtQ(size(Singularite%PtQ))

         if( QAM < Q1 .or. QAM > Q2 ) then
            Erreur%Numero = 42
            Erreur%ft     = err_42
            Erreur%ft_c   = err_42c
            call TRAITER_ERREUR( Erreur , Singularite%Numero , Temps , QAM , Q1 , Q2 )
            return
         endif

         do IQ = 1 , size(Singularite%PtQ) - 1

            Q1 = Singularite%PtQ(IQ)
            Q2 = Singularite%PtQ(IQ+1)
            if(QAM >= Q1 .and. QAM <= Q2) exit

         end do

         ! INTERPOLATION DE LA COTE AMONT A PARTIR DES DEBITS DE REFERENCE
         call INTERPOLATION_S (             &
           ZAM1                           , &
           ZAV                            , &
           PREMIER_ORDRE_INTERPOLATION    , &
           Singularite%PtZaval(:)         , &
           Singularite%PtZamont(IQ,:)     , &
           size(Singularite%PtZamont(1,:)), &
           Erreur                         )
         if( Erreur%numero /= 0 )then
            return
         endif

         call INTERPOLATION_S (              &
           ZAM2                             , &
           ZAV                              , &
           PREMIER_ORDRE_INTERPOLATION      , &
           Singularite%PtZaval(:)           , &
           Singularite%PtZamont(1+IQ,:)     , &
           size(Singularite%PtZamont(1,:))  , &
           Erreur                           )
         if( Erreur%numero /= 0 )then
            return
         endif

         ! RESULTAT
         ALPHA = ( QAM - Q1 ) / ( Q2 - Q1 )
         ZAm   = ( 1._DOUBLE - ALPHA ) * ZAM1 + ALPHA * ZAM2

      case( SINGULARITE_TYPE_ZAMONT_Q )

         ! COTE AMONT EN REGIME DENOYE
         call INTERPOLATION_S (         &
          ZAm                         , &
          QAM                         , &
          PREMIER_ORDRE_INTERPOLATION , &
          Singularite%PtQ(:)          , &
          Singularite%PtZ(:)          , &
          size(Singularite%PtZ)       , &
          Erreur                        )
         if (Erreur%Numero /= 0)then
            return
         endif

         ! TEST DU REGIME DENOYE
         charge_amont = ZAm  - Singularite%CoteCrete
         charge_aval  = ZAV  - Singularite%CoteCrete

         if( charge_amont <= 0._DOUBLE ) then
            Erreur%numero = 43
            Erreur%ft     = err_43
            Erreur%ft_c   = err_43c
            call TRAITER_ERREUR( Erreur , Singularite%Numero , Temps , ZAm , Singularite%CoteCrete )
            return
         endif

         RH = charge_aval / charge_amont
         if( RH <= CDENOY ) then
            !Erreur%arbredappel = !arbredappel_old
            return
         endif

         ! ITERATIONS EN REGIME NOYE
         num_iter  = 0
         EPSQ      = EPS3 * QAM
         ZAm       = DMAX1(ZAm,ZAV)
         SENS      = -1
         DZ        = DMAX1(charge_amont,charge_aval) / PAS

         30    ZAm = ZAm + DZ

         call INTERPOLATION_S (         &
          QAMONT                      , &
          ZAm                         , &
          PREMIER_ORDRE_INTERPOLATION , &
          Singularite%PtZ(:)          , &
          Singularite%PtQ(:)          , &
          size(Singularite%PtQ)       , &
          Erreur                        )
         if( Erreur%Numero /= 0 )then
            return
         endif

         RH = charge_aval / ( ZAm - Singularite%CoteCrete )
         if( RH > CDENOY ) then
            if( RH < 1._DOUBLE ) then
               QAMONT = ( C1 * (RH**3) + C2 * (RH**2) + C3 * RH + C4 ) * QAMONT
            else
               QAMONT = 0._DOUBLE
            endif
         endif

         if( dabs( QAM - QAMONT )  <=  EPSQ ) then
            !Erreur%arbredappel = !arbredappel_old
            return
         endif

         num_iter = num_iter + 1
         if( num_iter > NB_ITER_MAX ) then
            Erreur%numero = 44
            Erreur%ft   = err_44
            Erreur%ft_c = err_44c
            call TRAITER_ERREUR (Erreur, Singularite%Numero, &
                                         Singularite%Type,   &
                                       Temps)
         endif

         if( ( QAMONT > QAM .and. SENS == -1 ) .or. ( QAMONT < QAM .and. SENS == +1 ) ) then
            DZ   = -DZ / PAS
            SENS = -SENS
         end if

         goto 30

      case( SINGULARITE_TYPE_PROFIL_CRETE )

         largeur_seuil = Singularite%PtX(size(Singularite%PtX)) - Singularite%PtX(1)
         charge_aval   = DMAX1( 0._DOUBLE , ZAV - Singularite%CoteCrete )

         ! ESTIMATION INITIALE
         charge_amont = ( QAM / (Singularite%CoeffDebit * largeur_seuil * RDG) )**W23
         ZAm          = Singularite%CoteCrete + charge_amont
         DCH          = ( ( QAM / (largeur_seuil * (ZAm - ZREF)) )**2 ) / ( RDG**2 )
         DCH          = 0._DOUBLE

         ! RECHERCHE ITERATIVE
         num_iter = 0
         EPSQ  = EPS3 * QAM
         DZ    = DMAX1(charge_amont,charge_aval) / PAS
         ZAm   = DMAX1(ZAm,ZAV) - DZ

         50   ZAm = ZAm + DZ

         QAMONT = 0._DOUBLE

         do ipoint = 1 , size(Singularite%PtY) - 1

            DX =  Singularite%PtX(ipoint + 1)    - Singularite%PtX(ipoint)
            cote_crete = (Singularite%PtY(ipoint + 1) + Singularite%PtY(ipoint)) / 2._DOUBLE
            charge = MAX(0._DOUBLE, ZAm - cote_crete)
            if( charge > 0._DOUBLE ) then
               QAMONT = QAMONT + Singularite%CoeffDebit * DX * RDG * ( charge + DCH )**W32
            end if
         end do

         RH = charge_aval / ( ZAm - Singularite%CoteCrete )
         if( Singularite%Epaisseur_Seuil == 1 ) then
            if( RH > CDENOY ) then
               if( RH < 1._DOUBLE ) then
                  QAMONT = ( C1 * (RH**3) + C2 * (RH**2) + C3 * RH + C4 ) * QAMONT
               else
                  QAMONT = 0._DOUBLE
               endif
            endif
         else
            if( ZAV - Singularite%CoteCrete >= 0._DOUBLE ) then
               if( RH < 1._DOUBLE ) then
                  QAMONT = ( ( 1._DOUBLE - RH**D2 )**D1 ) * QAMONT
               else
                  QAMONT = 0._DOUBLE
               endif
            endif
         endif

         if( dabs( QAM - QAMONT )  <=  EPSQ ) then
            !Erreur%arbredappel = !arbredappel_old
            return
         endif

         num_iter = num_iter + 1
         if( num_iter > NB_ITER_MAX ) then
            Erreur%numero = 44
            Erreur%ft     = err_44
            Erreur%ft_c   = err_44c
            call TRAITER_ERREUR( Erreur, Singularite%Numero, &
                                         Singularite%Type,   &
                                      Temps)
            return
         endif

         if( num_iter == 1 ) then

            if( QAMONT < QAM ) then
               SENS = - 1
            else if( QAMONT > QAM ) then
               SENS = + 1
               DZ   = - DZ
            endif

         else

            if( ( QAMONT > QAM .and. SENS == -1 ) .or. ( QAMONT < QAM .and. SENS == +1 ) ) then
               DZ   = -DZ / PAS
               SENS = - SENS
            end if

         endif

         goto 50

      case( SINGULARITE_TYPE_CRETE_COEFF )

         call RHSBP_SECTION_S   (  &
          largeur_seuil          , &
          ZREF                   , &
          Singularite%CoteCrete  , &
          IDT(Section)           , &
          XDT(Section)           , &
          Profil                 , &
          B1Plan                 , &
          Erreur                   &
                           )

         charge_aval = DMAX1( 0._DOUBLE , ZAV - Singularite%CoteCrete )

         ! ESTIMATION INITIALE
         charge_amont = ( QAM / ( Singularite%CoeffDebit * largeur_seuil * RDG ) )**W23
         ZAm          = Singularite%CoteCrete + charge_amont
         DCH          = ( ( QAM / ( largeur_seuil * ( ZAm - ZREF ) ) )**2 ) / ( RDG**2 )
         DCH          = 0._DOUBLE

         ! RECHERCHE ITERATIVE
         num_iter = 0
         EPSQ     = EPS3 * QAM
         DZ       = DMAX1(charge_amont,charge_aval) / PAS
         ZAm      = DMAX1(ZAm,ZAV) - DZ

         60   ZAm = ZAm + DZ

         QAMONT = 0._DOUBLE

         charge = DMAX1( 0._DOUBLE , ZAm - Singularite%CoteCrete )
         if( charge > 0._DOUBLE ) then
            QAMONT = Singularite%CoeffDebit *largeur_seuil *RDG * (charge + DCH)**W32
         end if

         RH = charge_aval / ( ZAm - Singularite%CoteCrete )
         if( Singularite%Epaisseur_Seuil == 1 ) then
            if( RH > CDENOY ) then
               if( RH < 1._DOUBLE ) then
                  QAMONT = ( C1 * (RH**3) + C2 * (RH**2) + C3 * RH + C4 ) * QAMONT
               else
                  QAMONT = 0._DOUBLE
               endif
            endif
         else
            if( ZAV - Singularite%CoteCrete >= 0._DOUBLE ) then
               if( RH < 1._DOUBLE ) then
                  QAMONT = ( ( 1._DOUBLE - RH**D2 )**D1 ) * QAMONT
               else
                  QAMONT = 0._DOUBLE
               endif
            endif
         endif

         if( dabs( QAM - QAMONT )  <=  EPSQ ) then
            !Erreur%arbredappel = !arbredappel_old
            return
         endif

         num_iter = num_iter + 1
         if( num_iter > NB_ITER_MAX ) then
            Erreur%numero = 44
            Erreur%ft     = err_44
            Erreur%ft_c   = err_44c
            call TRAITER_ERREUR( Erreur, Singularite%Numero, &
                                         Singularite%Type,   &
                                         Temps)
            return
         endif

         if( num_iter == 1 ) then
            if( QAMONT < QAM ) then
               SENS = - 1
            else if( QAMONT > QAM ) then
               SENS = + 1
               DZ   = - DZ
            endif
         else
            if( ( QAMONT > QAM .and. SENS == -1 ) .or. ( QAMONT < QAM .and. SENS == +1 ) ) then
               DZ   = -DZ / PAS
               SENS = - SENS
            end if
         endif

         goto 60

      case( SINGULARITE_TYPE_Z_T )

         Zam = Singularite%PtZ(1)

      case( SINGULARITE_TYPE_Q_ZAMONT )

         call INTERPOLATION_S (         &
          Zam                         , &
          QAM                         , &
          PREMIER_ORDRE_INTERPOLATION , &
          Singularite%PtQ(:)          , &
          Singularite%PtZ(:)          , &
          size(Singularite%PtZ)       , &
          Erreur                        )
         if (Erreur%Numero /= 0)then
            return
         endif

      case default

         Erreur%Numero = 46
         Erreur%ft   = err_46
         Erreur%ft_c = err_46c
         call TRAITER_ERREUR (Erreur, Singularite%Numero, Temps, Singularite%Type)
         return

   end select

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine PSING
