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

subroutine  KSING                       ( &
            ASING,BSING,CSING,DSING     , & ! Coefficients du seuil
            QSING                       , & ! Debit au seuil
            NumSeuil                    , & ! Numero de la singularite
            Singularite                 , & ! Singularites
            Appel                       , & ! Flag
            ZAM                         , & ! Cote amont
            ZAV                         , & ! Cote aval
            QAM                         , & ! Debit amont
            QAV                         , & ! Debit aval
            BAM                         , & ! largeur au miroir amont
            VAM                         , & ! Vitesse amont
            Connect                     , & ! Table de connectivite du reseau
            X                           , & ! Maillage
            ZREF                        , & ! Cote de reference aux sections
            IDT                         , & ! Profils amont des sections
            XDT                         , & ! Position des sections/profils
            Profil                      , & ! Profils geometriques
            B1Plan                      , & ! Largeurs mineurs planimetrees
            Impression                  , & ! Flag d'impression
            UniteListing                , & ! Unite logique du fichier listing
            Erreur                        & ! Erreur
                                        )

! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P. CHERUBINI
!                             S. MANDELKERN
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
!   FONCTION :
!   --------
!
!   . CALCUL DES COEFFICIENTS A,B,C,D, DE L'EQUATION DISCRETISEE D'UNE
!     SINGULARITE :
!
!     A*DQ + B*DZAMONT + C*DZAVAL = D
!
!     Appel = 1 : LES COEFFICIENTS DE L'EQUATION
!                  DE LA SINGULARITE SONT CALCULES ET LE DEBIT
!     Appel = 2 : SEUL LE DEBIT THEORIQUE EST CALCULE
!_____________________________________________________________________________
!
!   FICHIERS ENTREE/SORTIE :    - UniteListing : IMPRESSION DES RESULTATS GLOBAUX
!
!   SOUS-PROGRAMMES APPELANT :  - REZODT
!   ------------------------
!
!   SOUS-PROGRAMMES APPELES :
!   -----------------------
!
! . SING2  : CALCUL DES COEFFICIENTS DE L'EQUATION DISCRETISEE D'UNE
!            SINGULARITE EN UTILISANT LA LOI EN REGIME DENOYE
!            (SINGULARITE DE TYPE 2)
!
! . SING3  : CALCUL DES COEFFICIENTS DE L'EQUATION DISCRETISEE D'UNE
!            SINGULARITE DEFINIE DE MANIERE STANDARD
!            (SINGULARITE DE TYPE 3)
!
! . SING10 : CALCUL DES COEFFICIENTS DE L'EQUATION DISCRETISEE D'UNE
!            VANNE OU D'UN ORIFICE
!            (SINGULARITE DE TYPE 10)
!
! . INTERPOLATION_S : SOUS-PROGRAMME D'INTERPOLATION
!
!
!   COMMENTAIRES :
!   ------------
!
! . LES CALCULS DEPENDENT DU TYPE DE LA SINGULARITE
! . SI LA SINGULARITE EST EST DEFINIE AU MOYEN D'UNE LOI
!   Q = F ( ZAMONT , ZAVAL) ALORS :
!   A=-1.   B=DF/DZAMONT   C=DF/DZAVAL D=0.
! . SI CETTE SINGULARITE EST DEFINIE AU MOYEN D'UNE FAMILLE DE COURBES
!   (SINGULARITE DE TYPE 1) ALORS F EST ECRITE SOUS LA FORME :
!   Q= F (X=DELZAV,Y=DELZAM) = ALPHA*X + BETA*Y + GAMMA*X*Y + DELTA
!   OU DELZAV,DELZAM SONT LES VARIATIONS DE COTES A PARTIR D'UN ETAT
!   DE REFERENCE
! . IL FAUT NOTER QUE DANS LE CAS D'UN SEUIL ET SELON LE SENS DE
!   L'ECOULEMENT, LES SECTIONS  AMONT ET AVAL SONT INVERSEES :
!   IL FAUT EN TENIR COMPTE ENSUITE POUR L'UTILISATION DES
!   COEFFICIENTS B,C
!
!------------------------------------------------------------------------

   !============================ Declarations ==============================
   use M_PRECISION        ! Type DOUBLE
   use M_PARAMETRE_C      ! Parametres de calcul
   use M_MESSAGE_C        ! Liste des messages d'erreur
   use M_CONNECT_T        ! Definition du type CONNECT_T
   use M_ERREUR_T         ! Definition du type ERREUR_T
   use M_PROFIL_T         ! Definition du type PROFIL_T
   use M_SINGULARITE_T    ! Definition du type SINGULARITE_T
   use M_INTERPOLATION_S  ! Interpolation
   use M_TRAITER_ERREUR_I ! Traitement des erreurs
   use M_NUM_BIEF_S       ! Calcul du num d'un bief d'apres num section calcul
   use M_RHSBP_S          ! Sous programme
   use M_SING2_I          ! Interface de sous-programme
   use M_SING3_I          ! Interface de sous-programme
   use M_SING10_I         ! Interface de sous-programme

   implicit none

   !.. Arguments ..
   real(DOUBLE)                     , intent(  out) :: ASING, BSING
   real(DOUBLE)                     , intent(  out) :: CSING, DSING
   real(DOUBLE)                     , intent(  out) :: QSING
   integer                          , intent(in   ) :: NumSeuil
   TYPE(SINGULARITE_T), dimension(:), intent(inout) :: Singularite
   integer                          , intent(in   ) :: Appel
   real(DOUBLE)                     , intent(in   ) :: ZAM, ZAV
   real(DOUBLE)                     , intent(inout) :: QAM, QAV
   real(DOUBLE)                     , intent(in   ) :: BAM, VAM
   logical                          , intent(in   ) :: Impression
   integer                          , intent(in   ) :: UniteListing
   type(CONNECT_T)                  , intent(in   ) :: Connect
   real(DOUBLE), dimension(:)       , intent(in   ) :: X
   real(DOUBLE), dimension(:)       , intent(in   ) :: ZREF
   integer       , dimension(:)     , intent(in   ) :: IDT
   real(DOUBLE)  , dimension(:)     , intent(in   ) :: XDT
   type(PROFIL_T), dimension(:)     , intent(in   ) :: Profil
   real(DOUBLE)  , dimension(:,:)   , intent(in   ) :: B1Plan
   type(ERREUR_T)                   , intent(inout) :: Erreur

   !.. Scalaires locaux ..
   TYPE(SINGULARITE_T) :: sing         ! Variable seuil locale
   integer             :: IQ, IQ1
   integer             :: IZ, IZ1
   integer             :: ipoint       ! Compteur
   integer :: nb_point, nb_q_ref, nb_z_ref
   integer :: type   ! Type de la singularite
   integer :: epaisseur_seuil
   real(DOUBLE)   :: ALPHA,BETA   ! Coefficients de discretisation de la loi d'une
   real(DOUBLE)   :: GAMMA, DELTA ! singularite definie au moyen d'une famille de courbes
   real(DOUBLE)   :: cote_amont, cote_aval
   real(DOUBLE)   :: DQDZAM,DQDZAV
   real(DOUBLE)   :: DX,DYQZ,DYQZ1,DYZQ1
   real(DOUBLE)   :: z_sing       ! Cote de l'eau a l'amont d'une sing
   real(DOUBLE)   :: Q,Q1
   real(DOUBLE)   :: QAM1,QAM2,QAV1,QAV2
   real(DOUBLE)   :: x_loc, y_loc
   real(DOUBLE)   :: largeur, ZBASSE,ZHAUTE
   real(DOUBLE)   :: cote_crete, coeff_debit
   real(DOUBLE)   :: debit_usine
   !character(132) :: arbredappel_old
   integer        :: num_section     ! numero d'une section
   integer        :: num_bief        ! numero du bief correspondant
   real(DOUBLE)   :: abs_rel         ! abscisse relative correspondante

   !.. Intrinsic Functions ..
   intrinsic DABS, DMAX1, DMIN1

   !============================ Instructions ==============================

   ! INITIALISATIONS
   ! ---------------
   Erreur%Numero = 0
   !arbredappel_old    = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>KSING'
   DQDZAM = 0._DOUBLE
   DQDZAV = 0._DOUBLE
   QSING  = 0._DOUBLE

   type   = Singularite(NumSeuil)%Type

   if( ZAM > Singularite(NumSeuil)%CoteRupture .or. ZAV > Singularite(NumSeuil)%CoteRupture ) then
      Singularite(NumSeuil)%EtatEfface = .true.
   endif

   ! -----------------------------------------------------------------------
   ! CALCUL DES PARAMETRES CARACTERISTIQUES DES SINGULARITES
   ! -----------------------------------------------------------------------
   if( Singularite(NumSeuil)%EtatEfface ) then
      QSING = QAM
   else
      select case( type )

         case( SINGULARITE_TYPE_ZAMONT_ZAVAL_Q )

            nb_z_ref   = size(Singularite(NumSeuil)%PtZamont)
            nb_q_ref   = size(Singularite(NumSeuil)%PtQ)
            cote_amont = DMAX1( ZAM , ZAV )
            cote_aval  = DMIN1( ZAV , ZAM )

            if( cote_amont >= Singularite(NumSeuil)%CoteCrete ) then
               ! CALCUL DE ALPHA,BETA,GAMMA,DELTA
               do ipoint = 2 , nb_z_ref
                  if( cote_aval <= Singularite(NumSeuil)%PtZaval(ipoint) ) goto 1000
               end do
               ipoint = nb_z_ref
1000           IZ     = ipoint
               IZ1    = IZ - 1
               !     nb_q_ref DOIT ETRE SUP A 1 (CF. BOUCLE CI-DESSOUS)
               do ipoint = 2 , nb_q_ref
                  if( DABS(QAM) <= Singularite(NumSeuil)%PtQ(ipoint) ) goto 1100
               end do
               ipoint = nb_q_ref
1100           IQ     = ipoint
               IQ1    = IQ - 1
               Q      = Singularite(NumSeuil)%PtQ(IQ)
               Q1     = Singularite(NumSeuil)%PtQ(IQ1)

               ! Zav sup - Zav inf
               DX = Singularite(NumSeuil)%PtZaval(IZ) - Singularite(NumSeuil)%PtZaval(IZ1)

               ! Zam (Qinf,Zsup) - Zam (Qinf,Zinf)
               DYZQ1 = Singularite(NumSeuil)%PtZamont(IQ1,IZ) - Singularite(NumSeuil)%PtZamont(IQ1,IZ1)

               ! Zam (Qsup,Zav inf) - Zam(Qinf,Zav inf)
               DYQZ1 = Singularite(NumSeuil)%PtZamont(IQ ,IZ1) - Singularite(NumSeuil)%PtZamont(IQ1,IZ1)

               ! Zam (Qsup,Zav sup) - Zam(Qinf,Zav sup)
               DYQZ =  Singularite(NumSeuil)%PtZamont(IQ ,IZ) - Singularite(NumSeuil)%PtZamont(IQ1,IZ)

               if( ABS(DYQZ1).LT.EPS6 ) then
                  DYQZ1 = EPS53
               end if
               if( ABS(DYQZ).LT.EPS6 ) then
                  DYQZ = EPS53
               end if

               DELTA = Q1
               BETA  = ( Q - Q1 ) / DYQZ1
               GAMMA = ( ( Q - Q1 ) / ( DYQZ ) - BETA ) / DX
               ALPHA = -DYZQ1 * ( BETA / DX + GAMMA )

               ! CALCUL DE DQDZAM,DQDZAV,QSING
               x_loc = cote_aval  - Singularite(NumSeuil)%PtZaval(IZ1)
               y_loc = cote_amont - Singularite(NumSeuil)%PtZamont(IQ1,IZ1)

               QSING  = ALPHA * x_loc + BETA * y_loc + GAMMA * x_loc * y_loc + DELTA
               DQDZAM = BETA + GAMMA * x_loc
               DQDZAV = ALPHA + GAMMA * y_loc

               if( ZAM < ZAV ) then
                  QSING  = -QSING
                  DQDZAM = -DQDZAM
                  DQDZAV = -DQDZAV
               end if
            end if

         case( SINGULARITE_TYPE_ZAMONT_Q )

            nb_q_ref   = size(Singularite(NumSeuil)%PtQ)
            cote_crete = Singularite(NumSeuil)%CoteCrete

            if( ZAM > cote_crete .or. ZAV > cote_crete ) then
               call SING2                          ( &
                    DQDZAM,DQDZAV,QSING            , &
                    ZAM,ZAV                        , &
                    cote_crete                     , &
                    nb_q_ref                       , &
                    Singularite(NumSeuil)%PtZ      , &
                    Singularite(NumSeuil)%PtQ      , &
                    Erreur                           &
                                          )

               if( Erreur%Numero /= 0 ) then
                  return
               end if
            end if

         case( SINGULARITE_TYPE_PROFIL_CRETE )

            nb_point    = size(Singularite(NumSeuil)%PtX)
            cote_crete  = Singularite(NumSeuil)%CoteCrete
            coeff_debit = Singularite(NumSeuil)%Coeffdebit
            epaisseur_seuil = Singularite(NumSeuil)%Epaisseur_Seuil
            debit_usine = singularite(Numseuil)%Debit

            if( ZAM > cote_crete .or. ZAV > cote_crete ) then

               call SING3                         ( &
                     DQDZAM,DQDZAV,QSING          , &
                     ZAM,ZAV,BAM,VAM              , &
                     cote_crete                   , &
                     Singularite(NumSeuil)%PtX    , &
                     Singularite(NumSeuil)%PtY    , &
                     coeff_debit                  , &
                     epaisseur_seuil              , &
                     Erreur                         &
                                          )

               if( Erreur%Numero /= 0 ) then
                  return
               end if
            else
               DQDZAM = 0._DOUBLE
               DQDZAV = 0._DOUBLE
               QSING  = debit_usine
            end if

         case( SINGULARITE_TYPE_CRETE_COEFF )

            sing        = Singularite(NumSeuil)
            cote_crete  = Singularite(NumSeuil)%CoteCrete
            coeff_debit = Singularite(NumSeuil)%Coeffdebit
            epaisseur_seuil = Singularite(NumSeuil)%Epaisseur_Seuil
            debit_usine = singularite(Numseuil)%Debit

            call RHSBP_SECTION_S      ( &
               largeur                , &
               ZREF(sing%Section)     , &
               cote_crete             , &
               IDT(sing%Section)      , &
               XDT(sing%Section)      , &
               Profil                 , &
               B1Plan                 , &
               Erreur                   &
                           )

            if( Erreur%Numero /= 0 ) then
               return
            endif

            sing%PtX(1) = 0._DOUBLE
            sing%PtX(2) = largeur
            sing%PtY(1) = cote_crete
            sing%PtY(2) = cote_crete

            if( ZAM > cote_crete .or. ZAV > cote_crete ) then
               call SING3                         ( &
                     DQDZAM,DQDZAV,QSING          , &
                     ZAM,ZAV,BAM,VAM              , &
                     cote_crete                   , &
                     sing%PtX                     , &
                     sing%PtY                     , &
                     coeff_debit                  , &
                     epaisseur_seuil              , &
                     Erreur                         &
                                          )

               if( Erreur%Numero /= 0 ) then
                  return
               end if
            else
               DQDZAM = 0._DOUBLE
               DQDZAV = 0._DOUBLE
               QSING  = debit_usine
            end if

         case( SINGULARITE_TYPE_Z_T )

            QSING = QAM

         case( SINGULARITE_TYPE_Q_ZAMONT )

            QSING = QAM

         case( SINGULARITE_TYPE_Q_ZAVAL )

            QSING = QAV

         case( SINGULARITE_TYPE_VANNE )

            ZHAUTE      = Singularite(NumSeuil)%PtZsup
            ZBASSE      = Singularite(NumSeuil)%PtZinf
            largeur     = Singularite(NumSeuil)%LargeurVanne
            coeff_debit = Singularite(NumSeuil)%COEFFDEBIT

            if( ZAM <= ZBASSE .and. ZAV <= ZBASSE ) then
               ZBASSE = DMIN1(ZAV,ZAM) - 0.01_DOUBLE
               ZHAUTE = ZBASSE + 0.02_DOUBLE
            end if

            call SING10                  ( &
               DQDZAM,DQDZAV,QSING       , &
               BAM,VAM,ZAM               , &
               ZAV                       , &
               coeff_debit, largeur      , &
               ZBASSE,ZHAUTE             , &
               Erreur                    )

         case default

            Erreur%Numero = 602
            Erreur%ft     = err_602
            Erreur%ft_c   = err_602c
            call TRAITER_ERREUR( Erreur , type )
            return

      end select

   endif ! de etat efface

   ! ---------------------------------------
   ! CALCUL DE ASING,BSING,CSING,DSING,QSING
   ! ---------------------------------------
   if( Appel == 1 ) then
      if( Singularite(NumSeuil)%EtatEfface ) then
         ASING = 0._DOUBLE
         BSING = 1._DOUBLE
         CSING = -1._DOUBLE
         DSING = 0._DOUBLE
      else

         select case (type)

            case( SINGULARITE_TYPE_ZAMONT_ZAVAL_Q, &
                  SINGULARITE_TYPE_ZAMONT_Q      , &
                  SINGULARITE_TYPE_PROFIL_CRETE  , &
                  SINGULARITE_TYPE_CRETE_COEFF   )

               ASING = -1._DOUBLE
               BSING = DQDZAM
               CSING = DQDZAV
               DSING = 0._DOUBLE

            case( SINGULARITE_TYPE_Z_T )

               ASING = 0._DOUBLE
               BSING = 1._DOUBLE
               CSING = 0._DOUBLE

               ! << Suppression de la maximisation de delta Z par une valeur parametre >>
               z_sing = Singularite(NumSeuil)%PtZ(1)
               DSING  = z_sing - ZAM

            case( SINGULARITE_TYPE_Q_ZAMONT )

               ASING = -1._DOUBLE
               CSING = 0._DOUBLE
               DSING = 0._DOUBLE

               nb_point = size(Singularite(NumSeuil)%PtZ)

               call INTERPOLATION_S            ( &
                  QAM1                         , &
                  ZAM                          , &
                  1                            , &
                  Singularite(NumSeuil)%PtZ    , &
                  Singularite(NumSeuil)%PtQ    , &
                  nb_point                     , &
                  Erreur                         &
                                        )

               if( Erreur%Numero /= 0 ) then
                  return
               end if

               call INTERPOLATION_S            ( &
                  QAM2                         , &
                  ZAM+EPS2                     , &
                  1                            , &
                  Singularite(NumSeuil)%PtZ    , &
                  Singularite(NumSeuil)%PtQ    , &
                  nb_point                     , &
                  Erreur                         &
                                        )

               if( Erreur%Numero /= 0 ) then
                  return
               endif

               BSING = ( QAM2 - QAM1 ) / EPS2

            case (SINGULARITE_TYPE_Q_ZAVAL)

               ASING = -1._DOUBLE
               BSING = 0._DOUBLE
               DSING = 0._DOUBLE

               nb_point = size(Singularite(NumSeuil)%PtZ)

               call INTERPOLATION_S            ( &
                  QAV1                         , &
                  ZAV                          , &
                  1                            , &
                  Singularite(NumSeuil)%PtZ    , &
                  Singularite(NumSeuil)%PtQ    , &
                  nb_point                     , &
                  Erreur                         &
                                        )

               if( Erreur%Numero /= 0 ) then
                  return
               end if

               call INTERPOLATION_S             ( &
                   QAV2                         , &
                   ZAV+EPS2                     , &
                   1                            , &
                   Singularite(NumSeuil)%PtZ    , &
                   Singularite(NumSeuil)%PtQ    , &
                   nb_point                     , &
                   Erreur                         &
                                        )

               if( Erreur%Numero /= 0 ) then
                  return
               endif

               CSING = ( QAV2 - QAV1 ) / EPS2

            case( SINGULARITE_TYPE_VANNE )

               ASING = -1._DOUBLE
               BSING = DQDZAM
               CSING = DQDZAV
               DSING = 0._DOUBLE

         end select

         if( Impression ) then
            num_section = Singularite(NumSeuil)%Section
            num_bief    = NUM_BIEF_S( Connect , num_section , Erreur )
            abs_rel     = X(num_section) - X(Connect%OrigineBief(num_bief))
            write (UniteListing,10001) NumSeuil, num_bief, abs_rel,           &
                                       QAM, QAV, ZAM, ZAV,                    &
                                       ASING, BSING, CSING, DSING, QSING
         end if

      endif ! de etatefface

   else    ! de if Appel = 1

      QAM = QSING
      QAV = QSING

      if( Impression ) then
         num_section = Singularite(NumSeuil)%Section
         num_bief    = NUM_BIEF_S(Connect, num_section, Erreur)
         abs_rel     = X(num_section) - X(Connect%OrigineBief(num_bief))
         write (UniteListing,10002) NumSeuil , num_bief , abs_rel , QSING
      end if
   end if  ! de if Appel = 1

   !Erreur%arbredappel = arbredappel_old

   return

   ! ... Format ...

  10001 format ( /,                                             &
         ' SINGULARITE NUMERO = ',i4,/,                         &
         ' Bief n0 ',i4,' Abscisse relative X = ',f12.3,/,      &
         ' QAM=',f7.2,' QAV=',f7.2,' ZAM=',f7.2,' ZAV=',f7.2,/, &
         ' A=',e12.4,' B=',e12.4,' C=',e12.4,' D=',e12.4,/,     &
         ' QSING=',f7.2)
  10002 format ( /,                                             &
         ' SINGULARITE NUMERO = ',i4,/,                         &
         ' Bief n0 ',i4,' Abscisse relative X = ',f12.3,/,      &
         ' QSING EFFECTIF  = ',f7.2/)

end subroutine KSING
