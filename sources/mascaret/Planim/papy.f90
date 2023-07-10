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

subroutine  PAPY            ( &
                              ! Resultats
            DB1, DB2, DBS       , & ! Variables largeur
            DP1, DP2            , & ! au miroir, perimetre mouille
            DS1, DS2            , & ! section mouillee, planimetres
            DSS, DS2G           , & ! pour la cote cote
                              ! Donnees
            NumProf             , & ! Numero du profil
            Cote                , & ! Cote de calcul des divers parametres
            LimiteMin           , & ! Limites du lit mineur du profil
            LimiteMaj           , & ! Limites du lit majeur du profil
            DXP, DYP            , & ! Coordonnees geometriques des points du profil
            S2SousCoteDebord    , & ! Surfaces mouillees majeur sous cote de debordement
            SSSousCoteDebord    , & ! Surfaces mouillees stockage sous cote de debordement
            DebProgressifLM     , & ! Debordement progressif en lit majeur
            DebProgressifZS     , & ! Debordement progressif en zone de stockage
            FrottParoiVerticale , & ! Conservation du frottement sur les parois verticales
            ImpressionPlani     , & ! Impression du planimetrage
            UniteListing        , & ! Unite logique listing
            Erreur                & ! Erreur
                                  )
! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P. CHERUBINI
!                             S. PERON
!                             E. BENSLAMA
!                             S. MANDELKERN
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
!  FONCTION :
!
!           CALCUL DES LARGEURS ,SECTIONS ET PERIMETRES MOUILLES
!                  (LIT MINEUR , LIT MAJEUR ET ZONE DE STOCKAGE)
!                  DE CHAQUE PROFIL
!
!                  PROFILS DONNES PAR POINTS
!                  =========================
!
!-----------------------------------------------------------------------
!
!                         VARIABLES LOCALES
! .___________.________________________________________________________
! !   MINEUR  ! L  ! -- ! DEFINITION DU LIT MINEUR
! !   MAJEUR  ! L  ! -- ! DEFINITION DU LIT MAJEUR
! !   LG,LD   ! I  ! -->! LIMITES D'ECOULEMENT
! !   XG,XD   ! R  ! -->! LIMITES D'ECOULEMENT
! !___________!____!____!______________________________________________
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!
!   FICHIERS  ENTREE/SORTIE :  UniteListing  : IMPRESSION LISTING
!   -------------------------
!
!   SOUS PROGRAMME APPELANT :  PLANIM
!   -------------------------
!   SOUS PROGRAMMES APPELES :  ---
!   -------------------------
!***********************************************************************

   !============================= Declarations ===========================
   use M_PRECISION           ! Type DOUBLE
   use M_PARAMETRE_C         ! Parametres de calcul
   use M_MESSAGE_C           ! Liste des messages d'erreur
   use M_ERREUR_T         ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   !.. Implicit Declarations ..
   implicit none

   integer, parameter :: NB_MAX_CHENAUX = 100

   !.. Formal Arguments ..
   real(DOUBLE), intent(  out) :: DB1,DB2,DBS
   real(DOUBLE), intent(  out) :: DP1,DP2
   real(DOUBLE), intent(  out) :: DS1,DS2,DS2G,DSS
   logical     , intent(in   ) :: DebProgressifLM
   logical     , intent(in   ) :: DebProgressifZS
   logical     , intent(in   ) :: FrottParoiVerticale
   logical     , intent(in   ) :: ImpressionPlani
   integer     , intent(in   ) :: UniteListing
   integer     , intent(in   ) :: NumProf
   type(ERREUR_T), intent(inout) :: Erreur
   real(DOUBLE), dimension(2), intent(in   ) :: S2SousCoteDebord
   real(DOUBLE), dimension(2), intent(in   ) :: SSSousCoteDebord
   real(DOUBLE)              , intent(inout) :: Cote
   integer     , dimension(2), intent(in   ) :: LimiteMin
   integer     , dimension(2), intent(in   ) :: LimiteMaj
   real(DOUBLE), dimension(:), intent(in   ) :: DXP,DYP

   !.. Local Scalars ..
   logical :: frottement
   logical :: cote_trop_grande
   integer :: ipoint,MD,MG
   integer :: largeur_chenal
   integer :: ichenal    ! compteur sur les chenaux
   integer :: nb_chenaux ! nombre de chenaux
   real(DOUBLE) :: ALPHA,DB2D,DB2G,DBRD,DBRG,DP2D,DP2G,DS2D,DSRD,DSRG,DSSUP, &
                   YI,YID,YIG,XDMAJ,XDMIN,XGMAJ,XGMIN,XI,XID,XIG,Cote0
   real(DOUBLE) :: delta_cote ! Difference entre Cote et Cote max du profil
   real(DOUBLE) :: XGJ, XDJ
   integer :: nb_point   ! Nombre de points du profil
   !character(132) :: !arbredappel_old
   !.. Local Arrays ..
   integer     , dimension(NB_MAX_CHENAUX) :: LD,LG
   real(DOUBLE), dimension(NB_MAX_CHENAUX) :: XD, XG
   !.. Intrinsic Functions ..
   intrinsic DABS, DMAX1, DMIN1, DSQRT

   !============================ Instructions ==============================

   ! INITIALISATIONS
   ! ---------------
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>PAPY'

   nb_point         = size(DXP)
   cote_trop_grande = .false.
   XGMIN            = DXP(LimiteMin(1))
   XDMIN            = DXP(LimiteMin(2))
   XGMAJ            = DXP(LimiteMaj(1))
   XDMAJ            = DXP(LimiteMaj(2))

   LG(:) = 0
   LD(:) = 0
   XG(:) = W0
   XD(:) = W0

   ! LIT MINEUR
   DP1 = EPS8
   DS1 = EPS8
   DB1 = EPS8

   ! LIT MAJEUR
   DP2  = EPS8
   DP2G = EPS8
   DP2D = EPS8

   DS2  = EPS8
   DS2G = EPS8
   DS2D = EPS8

   DB2  = EPS8
   DB2G = EPS8
   DB2D = EPS8

   ! ZONE DE STOCKAGE
   ! REMARQUE : PAS BESOIN DE PERIMETRE MOUILLE EN ZONE DE STOCKAGE
   ! CAR PAS D'ECOULEMENT
   DSS  = EPS8
   DSRG = EPS8
   DSRD = EPS8

   DBS  = EPS8
   DBRG = EPS8
   DBRD = EPS8

   ! DELIMITATION DES CHENAUX A PLANIMETRER POUR LA COTE COTE
   ! ------------------------------------------------------
   ichenal = 1
   ipoint  = 1

   if( Cote > DYP(ipoint) )  then
      if( ImpressionPlani )  then
         write(UniteListing,1000) NumProf, Cote, DYP(ipoint)
      endif
      Cote0            = Cote
      Cote             = DYP(1)
      cote_trop_grande = .true.
   end if

   ! RIVE GAUCHE D'UN CHENAL
   OUTER_LOOP : do while( ipoint < nb_point )
      ipoint = ipoint + 1
      if( Cote > DYP(ipoint) ) then
         LG(ichenal) = ipoint - 1
         XG(ichenal) = (                                  &
                    DXP(ipoint-1) * ( DYP(ipoint) - Cote ) - &
                    DXP(ipoint) * ( DYP(ipoint-1) - Cote )   &
                    ) / ( DYP(ipoint) - DYP(ipoint-1) )
         do
            ! RIVE DROITE D'UN CHENAL
            if( ipoint >= nb_point ) exit OUTER_LOOP
            ipoint = ipoint + 1
            if( Cote <= DYP(ipoint) ) exit
         end do

         LD(ichenal) = ipoint - 1
         XD(ichenal) = (                                  &
                    DXP(ipoint-1) * ( DYP(ipoint) - Cote ) - &
                    DXP(ipoint) * ( DYP(ipoint-1) - Cote )   &
                    ) / ( DYP(ipoint) - DYP(ipoint-1) )
         ichenal = ichenal + 1
         if( ichenal > NB_MAX_CHENAUX ) then
            Erreur%Numero = 201
            Erreur%ft   = err_201
            Erreur%ft_c = err_201c
            call TRAITER_ERREUR (Erreur, NumProf, NB_MAX_CHENAUX)
            return
         end if
      end if
   end do OUTER_LOOP

   ! PLANIMETRAGE
   ! ------------
   nb_chenaux = ichenal - 1
   if( nb_chenaux > 0 ) then
      ! BOUCLE SUR L ENSEMBLE DES CHENAUX
      do ichenal = 1 , nb_chenaux
         ! RIVE GAUCHE D'UN CHENAL
         MG = LG(ichenal)
         XI = DXP(MG+1)
         YI = DYP(MG+1)
         frottement = FrottParoiVerticale .or.    &
                      DABS( XG(ichenal) - XI ) > EPS54
         XGJ = XG(ichenal)
         if( DMIN1( XGJ , XI ) >= XGMIN .and. DMAX1( XGJ , XI ) <= XDMIN ) then
            if( frottement ) then
               DP1 = DP1 + DSQRT( ( XI - XG(ichenal) )**2 + ( Cote - YI )**2 )
            end if
            DS1 = DS1 + W12 * ( XI - XG(ichenal) ) * ( Cote - YI )
            DB1 = DB1 + XI - XG(ichenal)
         elseif (XG(ichenal)>=XGMAJ .and. XI<=XGMIN) then
            if (frottement) then
               DP2G = DP2G + SQRT((XI-XG(ichenal))**2 + ( Cote - YI )**2)
            end if
            DS2G = DS2G + W12*(XI-XG(ichenal))*(Cote-YI)
            DB2G = DB2G + XI - XG(ichenal)
         elseif( XG(ichenal) >= XDMIN .and. XI <= XDMAJ ) then
            if( frottement ) then
               DP2D = DP2D + DSQRT( ( XI - XG(ichenal) )**2 + ( Cote - YI )**2 )
            end if
            DS2D = DS2D + W12*( XI - XG(ichenal) ) * ( Cote - YI )
            DB2D = DB2D + XI - XG(ichenal)
         ! CAS DE LA ZONE DE STOCKAGE GAUCHE
         elseif( XG(ichenal) >= DXP(1) .and. XI <= XGMAJ ) then
            DSRG = DSRG + W12 * ( XI - XG(ichenal) ) * ( Cote - YI )
            DBRG = DBRG + XI - XG(ichenal)
            ! CAS DE LA ZONE DE STOCKAGE DROITE
         elseif( XG(ichenal) >= XDMAJ .and. XI <= DXP(nb_point) ) then
            DSRD = DSRD + W12 * ( XI - XG(ichenal) ) * ( Cote - YI )
            DBRD = DBRD + XI - XG(ichenal)
         end if
         ! RIVE DROITE D'UN CHENAL
         MD         = LD(ichenal)
         XI         = DXP(MD)
         YI         = DYP(MD)
         frottement = FrottParoiVerticale .or.DABS( XD(ichenal) - XI ) > EPS54
         XDJ        = XD(ichenal)
         if( DMIN1( XI , XDJ ) >= XGMIN .and. DMAX1( XI , XDJ ) <= XDMIN ) then
            if( frottement ) then
               DP1 = DP1 + DSQRT( ( XD(ichenal) - XI )**2 + ( Cote - YI )**2 )
            end if
            DS1 = DS1 + W12 * ( XD(ichenal) - XI ) * ( Cote - YI )
            DB1 = DB1 + XD(ichenal) - XI
         elseif( XI >= XGMAJ .and. XD(ichenal) <= XGMIN ) then
            if( frottement ) then
               DP2G = DP2G + DSQRT( ( XD(ichenal) - XI )**2 + ( Cote - YI )**2 )
            end if
            DS2G = DS2G + W12 * ( XD(ichenal) - XI ) * ( Cote - YI )
            DB2G = DB2G + XD(ichenal) - XI
         elseif( XI >= XDMIN .and. XD(ichenal) <= XDMAJ ) then
            if( frottement ) then
               DP2D = DP2D + DSQRT( ( XD(ichenal) - XI )**2 + ( Cote - YI )**2 )
            end if
            DS2D = DS2D + W12 * ( XD(ichenal) - XI ) * ( Cote - YI )
            DB2D = DB2D + XD(ichenal) - XI
         ! CAS DE LA ZONE DE STOCKAGE GAUCHE
         elseif( XD(ichenal) >= DXP(1) .and. XI <= XGMAJ ) then
            DSRG = DSRG + W12 * ( XD(ichenal) - XI ) * ( Cote - YI )
            DBRG = DBRG + XD(ichenal) - XI
         ! CAS DE LA ZONE DE STOCKAGE DROITE
         elseif( XD(ichenal) >= XDMAJ .and. XI <= DXP(nb_point) ) then
            DSRD = DSRD + W12 * ( XD(ichenal) - XI ) * ( Cote - YI )
            DBRD = DBRD + XD(ichenal) - XI
         end if
         ! CENTRE D'UN CHENAL
         largeur_chenal = MD - MG - 1
         if( largeur_chenal > 0 ) then
            do ipoint = 1 , largeur_chenal
               XIG = DXP(MG+ipoint)
               YIG = DYP(MG+ipoint)
               XID = DXP(MG+ipoint+1)
               YID = DYP(MG+ipoint+1)
               frottement = FrottParoiVerticale .or. DABS( XIG - XID ) > EPS54
               if( DMIN1( XIG , XID ) >= XGMIN .and. DMAX1( XID , XIG ) <= XDMIN ) then
                  if( frottement ) then
                     DP1 = DP1 + DSQRT( ( XIG - XID )**2 + ( YIG - YID )**2 )
                  end if
                  DS1 = DS1 + W12 * ( XID - XIG ) * ( W2 * Cote - YIG - YID )
                  DB1 = DB1 + XID - XIG
               elseif( XIG >= XGMAJ .and. XID <= XGMIN) then
                  if( frottement ) then
                     DP2G = DP2G + DSQRT( ( XIG - XID )**2 + ( YIG - YID )**2 )
                  end if
                  DS2G = DS2G + W12 * ( XID - XIG ) * ( W2 * Cote - YIG - YID )
                  DB2G = DB2G + XID - XIG
               elseif( XIG >= XDMIN .and. XID <= XDMAJ ) then
                  if( frottement ) then
                     DP2D = DP2D + DSQRT( ( XIG - XID )**2 + ( YIG - YID )**2 )
                  end if
                  DS2D = DS2D + W12 * ( XID - XIG ) * ( W2 * Cote - YIG - YID )
                  DB2D = DB2D + XID - XIG
               ! CAS DE LA ZONE DE STOCKAGE GAUCHE
               elseif( XIG >= DXP(1) .and. XID <= XGMAJ ) then
                  DSRG = DSRG + W12 * ( XID - XIG ) * ( W2 * Cote - YIG - YID )
                  DBRG = DBRG + XID - XIG
               ! CAS DE LA ZONE DE STOCKAGE DROITE
               elseif( XIG >= XDMAJ .and. XID <= DXP(nb_point) ) then
                  DSRD = DSRD + W12 * ( XID - XIG ) * ( W2 * Cote - YIG - YID )
                  DBRD = DBRD + XID - XIG
               end if
            end do ! boucle sur ipoint
         end if
      end do     ! boucle sur ichenal
   end if
   ! FIN BOUCLE : LE ENDIF PORTE SUR LE NOMBRE DE CHENAUX

   ! SURFACE AU DESSUS DU PROFIL (  Cote > DYP(1) )
   ! --------------------------------------------
   if( cote_trop_grande ) then
      delta_cote = Cote0 - Cote
      DS1        = DS1  + DB1 *delta_cote
      DS2G       = DS2G + DB2G*delta_cote
      DS2D       = DS2D + DB2D*delta_cote
      DSRG       = DSRG + DBRG*delta_cote
      DSRD       = DSRD + DBRD*delta_cote
   end if

   ! VALEURS DU PLANIMETRAGE POUR LE LIT MAJEUR
   ! ------------------------------------------
   ! DEBORDEMENT PROGRESSIF DANS LE LIT MAJEUR
   if( DebProgressifLM ) then
      ! RIVE GAUCHE
      DSSUP = DS2G - S2SousCoteDebord(1)
      if( DSSUP > 2 * EPS5 ) then
         ! ON INTEGRE PEU A PEU LA ZONE DE DEBORDEMENT DANS LA ZONE
         ! D ECOULEMENT EN LIT MAJEUR.
         ALPHA = DSSUP / DS2G
         DS2  = DS2 + DS2G - ( W1 - ALPHA ) * S2SousCoteDebord(1)
         DS2G = DS2G - ( W1 - ALPHA ) * S2SousCoteDebord(1)
         DP2  = DP2 + DP2G
         DB2  = DB2 + DB2G - ( W1 - ALPHA )**2 *  DB2G
!> correction P. CHASSE (Pb estuaire loire)
!         DSRG = DSRG + ( W1 - ALPHA ) * S2SousCoteDebord(1)
!         DBRG = DBRG + ( W1 - ALPHA )**2 * DB2G
!< fin modif
      else
         ! LE LIT MAJEUR EST SOLLICITE EN DESSOUS DES ZONES DE  DEBORDEMENT
         ! CELA N EST PAS COMPTE : TOUT EST MIS EN ZONE DE STOCKAGE
!> correction P. CHASSE (Pb estuaire loire)
!         DSRG = DSRG + DS2G
!         DBRG = DBRG + DB2G
         DSS = EPS8 !W0 (correction C. COULET)
         DBS = EPS8 !W0 (correction C. COULET)
!< fin modif
         DS2G = W0
         DS2 = DS2 + DS2G
         DB2G = W0
         DB2 = DB2 + DB2G
         DP2G = W0
         DP2 = DP2 + DP2G
      end if
      ! RIVE DROITE
      DSSUP = DS2D - S2SousCoteDebord(2)
      if( DSSUP >  2 * EPS5 ) then
         ALPHA = DSSUP / DS2D
         DS2   = DS2 + DS2D - ( W1 - ALPHA ) * S2SousCoteDebord(2)
         DS2D  = DS2D - ( W1 - ALPHA ) * S2SousCoteDebord(2)
         DP2   = DP2 + DP2D
         DB2   = DB2 + DB2D - ( W1 - ALPHA )**2 * DB2D
!> correction P. CHASSE (Pb estuaire loire)
!         DSRD  = DSRD + ( W1 - ALPHA ) * S2SousCoteDebord(2)
!         DBRD  = DBRD + ( W1 - ALPHA )**2 * DB2D
!< fin modif
      else
!> correction N. GOUTAL (Pb estuaire loire + symetrie RD/RG)
!         DSS = DSS + DS2D
!         DBS = DBS + DB2D
         ! LE LIT MAJEUR EST SOLLICITE EN DESSOUS DES ZONES DE  DEBORDEMENT
         ! CELA N EST PAS COMPTE : TOUT EST MIS EN ZONE DE STOCKAGE
!> correction P. CHASSE (Pb estuaire loire)
!         DSRD  = DSRD + DS2D
!         DBRD  = DBRD + DB2D 
!< fin modif N. GOUTAL
         DSS = EPS8 !W0 (correction C. COULET)
         DBS = EPS8 !W0 (correction C. COULET)
!< fin modif P. CHASSE
         DS2D = W0
         DS2 = DS2 + DS2D
         DB2D = W0
         DB2 = DB2 + DB2D
         DP2D = W0
         DP2 = DP2 + DP2D
      end if
   else
      DS2 = DS2G + DS2D
      DP2 = DP2G + DP2D
      DB2 = DB2G + DB2D
   endif

   ! FIN DebProgressifLM = TRUE

   ! DEBORDEMENT PROGRESSIF DANS LA ZONE DE STOCKAGE
   if( DebProgressifZS ) then
      ! RIVE GAUCHE
      DSSUP = DSRG - SSSousCoteDebord(1)
      if( DSSUP > 2 * EPS5 ) then
         ! ON NE PREND EN COMPTE LA ZONE DE STOCKAGE QUE PROGRESSIVEMENT
         ALPHA = DSSUP / DSRG
         DSS   = DSS + DSRG - ( W1 - ALPHA ) * SSSousCoteDebord(1)
         DSRG  = DSRG - ( W1 - ALPHA ) * SSSousCoteDebord(1)
         DBS   = DBS + DBRG - ( W1 - ALPHA )**2 * DBRG
      else
         DSRG = W0
         DSS  = DSS + DSRG
         DBRG = W0
         DBS  = DBS + DBRG
      end if
      ! RIVE DROITE
      DSSUP = DSRD - SSSousCoteDebord(2)
      if( DSSUP > 2 * EPS5 ) then
         ALPHA = DSSUP / DSRD
         DSS   = DSS + DSRD - ( W1 - ALPHA ) * SSSousCoteDebord(2)
         DSRD  = DSRD - ( W1 - ALPHA ) * SSSousCoteDebord(2)
         DBS   = DBS + DBRD - ( W1 - ALPHA )**2. * DBRD
      else
         DSRD = W0
         DBRD = W0
         DSS  = DSS + DSRD
         DBS  = DBS + DBRD
      end if
   else
      DSS = DSRG + DSRD
      DBS = DBRG + DBRD
   end if
   ! FIN DebProgressifZS = TRUE

   ! Fin des traitements
   ! -------------------

   !Erreur%arbredappel = !arbredappel_old

   return

!... Formats ...

1000     format (/,'INFO - FONCTION PLANIM : ',/, &
        'Au profil n0 ',i5,', le planimetrage depasse la cote limite du profil.',/, &
        'Cote planimetree = ',f8.2,'. Cote limite = ',f8.2,/)

end subroutine PAPY
