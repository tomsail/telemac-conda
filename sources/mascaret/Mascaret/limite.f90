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

subroutine LIMITE( &
                  Y1 , &
                  S1 , &
                  Q1 , &
              NBSECT , &
                ITYP , &
                QFIX , &
                YFIX , &
               AMONT , &
                  DT , &
                  IS , &
                  NB , &
                   X , &
               SNODE , &
               CNODE , &
              AKNODE , &
               QNODE , &
               UNODE , &
                COTR , &
              FRNODE , &
                  DZ , &
                SGEO , &
               ALGEO , &
              DEBGEO , &
               AKGEO , &
               AIGEO , &
               DYGEO , &
              NMLARG , &
          Impression , & ! Flag d'impression
        UniteListing , & ! Unite logique fichier listing
              Erreur )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL           F. LEPEINTRE
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!
!  FONCTION :
!  --------
!       CALCUL DES CONDITIONS AUX LIMITES
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____._______________________________________________
! !    NOM    !TYPE!MODE!                   ROLE
! !___________!____!____!______________________________________________
! !  Y1       !  R !  R ! COTE A L AMONT                               !
! !  S1       !  R !  R ! SECTION A L AMONT                            !
! !  Q1       !  R !  R ! DEBIT A LAMONT                               !
! !  NBSECT   !  I !  D ! NOMBRE DE POINTS DU MAILLAGE                 !
! !  ITYP     !  I !    !                                              !
! !  QFIX     !  R !  D ! DEBIT IMPOSE                                 !
! !  YFIX     !  R !  D ! COTE IMPOSEE                                 !
! !  AMONT    !  L !    !                                              !
! !  DT       !  R !  D ! PAS DE TEMPS                                 !
! !  IS       !  I !    !                                              !
! !  NB       !  I !    !                                              !
! !  X        ! TR !  D ! ABSCISSE DES SECTIONS DE CALCUL              !
! !  SNODE    ! TR !  D ! SECTION MOUILLEE AU TEMPS N                  !
! !  CNODE    ! TR !  D ! CELERITE         AU TEMPS N                  !
! !  AKNODE   ! TR !  D ! K                AU TEMPS N                  !
! !  QNODE    ! TR !  D ! Debit            AU TEMPS N                  !
! !  UNODE    ! TR !  D ! VITESSE          AU TEMPS N                  !
! !  COTR     ! TR !    !                                              !
! !  FRNODE   ! TR !  D ! FROTTEMENT  AU TEMPS N                       !
! !  DZ       ! TR !    !                                              !
! !  SGEO     ! TR !  D ! SECTION PLANIMETREE                          !
! !  ALGEO    ! TR !  D ! LARGEUR PLANIMETREE                          !
! !  DEBGEO   ! TR !    !                                              !
! !  AKGEO    ! TR !  D !       K PLANIMETREE SUR LE MAILLAGE          !
! !  AIGEO    ! TR !  D !  AIGEO  PLANIMETREE SUR LE MAILLAGE          !
! !  DYGEO    ! TR !  D !  DYGEO  PLANIMETREE SUR LE MAILLAGE          !
! !  NMLARG   !  I !  D !                                              !
! !___________!____!____!______________________________________________!
!
!***********************************************************************

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C ! EPS6
   use M_ERREUR_T    ! ERREUR
   use M_AKIV_I      ! Interface de la fonction    AKIV
   use M_AKIVM1_I    ! Interface de la fonction    AKIVM1
   use M_CARAC_I     ! Interface de la fonction    CARAC
   use M_CARAC3_I    ! Interface du sous-programme CARAC3
   use M_CELE_I      ! Interface de la fonction    CELE
   use M_CSUR_I      ! Interface de la fonction    CSUR
   use M_CSURM1_I    ! Interface de la fonction    CSURM1
   use M_FROTTE_I    ! Interface du sous-programme FROTTE

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE),                   intent(  out) :: Y1,S1,Q1
   integer     ,                   intent(in)    :: NBSECT,ITYP
   real(DOUBLE),                   intent(in)    :: QFIX,YFIX
   logical     ,                   intent(in)    :: AMONT
   real(DOUBLE),                   intent(in)    :: DT
   integer     ,                   intent(in)    :: IS,NB
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(in)    :: X
   real(DOUBLE), dimension(:)    , intent(inout) :: SNODE,CNODE
   real(DOUBLE), dimension(:)    , intent(  out) :: AKNODE
   real(DOUBLE), dimension(:)    , intent(in)    :: QNODE,UNODE
   real(DOUBLE), dimension(:)    , intent(in)    :: COTR
   real(DOUBLE), dimension(:)    , intent(  out) :: FRNODE
   real(DOUBLE), dimension(:)    , intent(in)    :: DZ
   ! 1ere dimension IM, 2nde dimension NMLARG
   real(DOUBLE), dimension(:,:)  , intent(in)    :: SGEO, ALGEO
   real(DOUBLE), dimension(:,:)  , intent(in)    :: DEBGEO
   real(DOUBLE), dimension(:,:)  , intent(in)    :: AKGEO
   real(DOUBLE), dimension(:,:)  , intent(in)    :: AIGEO,DYGEO
   integer     ,                   intent(in)    :: NMLARG
   logical                       , intent(in   ) :: Impression
   integer     ,                   intent(in   ) :: UniteListing
   type(ERREUR_T)                , intent(inout) :: Erreur

   !.. Variables locales ..
   !-----------------------
   real(DOUBLE) :: UM,SM,CM,FM
   real(DOUBLE) :: FP,S0,FPRMS0,ERROR,FS0
   real(DOUBLE) :: FROUDE,C,ALAMBM,SNO
   real(DOUBLE) :: UP,SP,CP,C1
   real(DOUBLE) :: AK1,S11,AKP
   real(DOUBLE) :: UDMCD
   real(DOUBLE) :: QP
   integer      :: INDIC
   integer      :: NO,IPOINT,IAMT,TCAR,M,I
   integer      :: NVAL,NOEUD ,NSECG,NSECD
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   !   !arbredappel_old    = trim(!Erreur%arbredappel)
   !  !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>LIMITE'
   ERROR = 0._DOUBLE

   ! CALCUL DU NOMBRE DE FROUDE
   if( CNODE(IS) <= EPS6 ) then
      FROUDE = UNODE(IS) / EPS6
   else
      FROUDE = UNODE(IS) / CNODE(IS)
   endif

   ! TRAITEMENT DE L'AMONT
   label_amont : if (AMONT) then

      label_froude : if( FROUDE < 1._DOUBLE ) then
         !                                  XXXX
         !                      -------
         !                      FLUVIAL:
         !                      -------
         !                      CALCUL DE FMG
         !                                ---
         !                      CONVECTION DE L'INVARIANT FMOINS
         IPOINT = IS
         TCAR   = -1
         ! IAMT INDICE DU POINT LE PLUS A DROITE
         IAMT  = min( 10 , NB )
         NSECG = IS
         NSECD = IS + IAMT

         do I = NSECG , NSECD

            call FROTTE (   &
                  FRNODE(I), &
                  I        , &
                  SNODE(I) , &
                  QNODE(I) , &
                  DEBGEO   , &
                  SGEO     , &
                  NMLARG   , &
                  Erreur     &
                  )
            if (Erreur%Numero /= 0) then
               return
            endif

            AKNODE(I) = AKIV( I , SNODE(I) , AKGEO , SGEO , NMLARG , Erreur )

         end do

         call CARAC3 ( &
             UM     , &
             SM     , &
             CM     , &
             FM     , &
             IPOINT , &
             UNODE  , &
             SNODE  , &
             CNODE  , &
             AKNODE , &
             TCAR   , &
             X      , &
             DT     , &
             NSECG  , &
             NSECD  , &
             COTR   , &
             FRNODE , &
             SGEO   , &
             AIGEO  , &
             DYGEO  , &
             NBSECT , &
             NMLARG , &
             Impression  , &
             UniteListing, &
             Erreur      &
             )

         if( Erreur%Numero /= 0 ) then
            return
         endif

         !  SI F- POSITIF TORRENTIEL L'AMONT
         if( FM > 0._DOUBLE .and. Impression ) then
            write (UniteListing,*)'SSP LIMITE: ATTENTION'
            write (UniteListing,*)'FM EST POSITIF'
            write (UniteListing,*)'FM',FM
            write (UniteListing,*)'L''ECOULEMENT DEVIENT TORRENTIEL A L''AMONT'
            write (UniteListing,*)'LE CALCUL SE FAIT AVEC COTE ET DEBIT IMPOSE A L AMONT '
            NO = IS
            S1 = CSUR( NO , YFIX , DZ , SGEO , NMLARG , Erreur )
            Q1 = QFIX
            Y1 = YFIX
           return
         endif

         if( ITYP == 1 ) then
            ! *** DEBIT IMPOSE
            S1 = SNODE(IS)
            M  = 0
            ! BOUCLE SUR LES ITERATIONS DE NEWTON
            do while( M == 0 .or. ( ( ERROR > EPS6 ) .and. ( M < 10 ) ) )
               S0     = S1
               NO     = IS
               FS0    = QFIX - S0 * AKIV( NO , S0 , AKGEO , SGEO , NMLARG , Erreur ) - S0 * FM
               FPRMS0 = - AKIV( NO , S0 , AKGEO , SGEO , NMLARG , Erreur ) - CELE( NO , S0 , SGEO , ALGEO , Erreur ) - FM
               S1     = S0 - FS0 / FPRMS0
               Y1     = CSURM1( S1 , DZ(NO) , SGEO(NO,:) , Erreur )
               Q1     = QFIX
               ERROR  = dabs( S1 - S0 ) / S0
               M      = M + 1

            end do

            S1 = S0
            Y1 = CSURM1( S1 , DZ(NO) , SGEO(NO,:) , Erreur )
            Q1 = QFIX

         else
            ! ***                 COTE IMPOSEE
            NO = IS
            Y1 = YFIX
            S1 = CSUR( NO , YFIX , DZ , SGEO , NMLARG , Erreur )
            Q1 = S1 * FM + S1 * AKIV( NO , S1 , AKGEO , SGEO , NMLARG , Erreur )
         endif
         !                      CALCUL DE LA SECTION, DE LA CELERITE, DE ALAMBM
         !                             ET DE FPG AU NOEUD 1
         NO = IS
         C  = CELE( NO , S1 , SGEO , ALGEO , Erreur )
         if( S1 < EPS6 ) then
            ALAMBM = Q1 / EPS6 - C
         else
            ALAMBM = Q1 / S1   - C
         endif

         if( ALAMBM > 0._DOUBLE .and. Impression ) then
           write(UniteListing,*) 'SSP LIMITE: FLUVIAL-DEBIT AMONT DONNE'
           write(UniteListing,*) 'L''ECOULEMENT DEVIENT TORRENTIEL A L'' AMONT'
           write(UniteListing,*) 'ALAMBM NEGATIF ',ALAMBM
         endif
         if( S1 < EPS6 ) then
            NO  = IS
            SNO = EPS6
            FP  = Q1 / EPS6 + AKIV( NO , SNO , AKGEO , SGEO , NMLARG , Erreur )
         else
            NO  = IS
            FP  = Q1 / S1   + AKIV( NO , S1 , AKGEO , SGEO , NMLARG , Erreur )
         endif
         ! IMPRESSION DE CONTROLE
         if( FP < 0._DOUBLE ) then !stop 3
            Erreur%Message = 'Error with a boundary condition (subroutine LIMITE) : FP < 0'
            Erreur%numero = 1
            return
         endif

      else label_froude
         !                            XXXX
         !                      -----------
         !                      TORRENTIEL:
         !                      -----------
         !                      HAUTEUR ET DEBIT IMPOSES
         NO = IS
         Y1 = YFIX
         Q1 = QFIX
         S1 = CSUR( NO , YFIX , DZ , SGEO , NMLARG , Erreur )

      endif label_froude
     !                            XXXXX

   else label_amont
      !        2) TRAITEMENT DE L'AVAL DU BIEF
      !          ------------------------------
      label_ityp : if( ITYP /= 6 )  then

         label_froude2 : if( FROUDE <= 1.01_DOUBLE ) then
            !                                     YYYY
            !                      -------
            !                      FLUVIAL:
            !                      -------
            !                      CALCUL DE FP
            !                                ---
            !                      CONVECTION DE L'INVARIANT FPLUS
            IPOINT = IS
            TCAR   = 1
            !                      NVAL NOMBRE DE POINTS PRIS EN COMPTE
            NVAL   = min( 10 , NB )
            NSECG  = IS - NVAL + 1
            NSECD  = IS

            do I = NSECG , NSECD
               call FROTTE ( &
                     FRNODE(I) , &
                             I , &
                      SNODE(I) , &
                      QNODE(I) , &
                        DEBGEO , &
                          SGEO , &
                        NMLARG , &
                        Erreur )
               if( Erreur%Numero /= 0 ) then
                  return
               endif

               AKNODE(I) = AKIV( I , SNODE(I) , AKGEO , SGEO , NMLARG , Erreur )
               if( Erreur%Numero /= 0 ) then
                  return
               endif
            end do

            call CARAC3 ( &
                UP     , &
                SP     , &
                CP     , &
                FP     , &
                IPOINT , &
                UNODE  , &
                SNODE  , &
                CNODE  , &
                AKNODE , &
                TCAR   , &
                X      , &
                DT     , &
                NSECG  , &
                NSECD  , &
                COTR   , &
                FRNODE , &
                SGEO   , &
                AIGEO  , &
                DYGEO  , &
                NBSECT , &
                NMLARG , &
                Impression  , &
                UniteListing, &
                Erreur      &
                )

            if( Erreur%Numero /= 0 ) then
               return
            endif

            ! EVITE LE BLOCAGE D'UN RESSAUT JUSTE A L'AVAL
            if( UP < CP ) then
               do NOEUD = NBSECT , NBSECT - 1 , -1
                  ! RECHERCHE D'UN EVENTUEL ECLT TORRENTIEL AU
                  !    VOISINAGE DE L'AVAL
                  ALAMBM = UNODE(NOEUD) - CNODE(NOEUD)
                  if( ALAMBM >= 0._DOUBLE ) then
                     write (UniteListing,*)'ATTENTION'
                     ! WRITE (UniteListing,*)'*********'
                     ! PRINT*,'TORRENTIEL FORCE A L''AVAL'
                     ! WRITE (UniteListing,*)' '
                     UP  = UNODE (NOEUD)
                     SP  = SNODE (NOEUD)
                     CP  = CNODE (NOEUD)
                     FP  = UNODE (NOEUD) + AKNODE(NOEUD)
                     Go TO 30
                  endif
               end do
            endif

            30 if( ITYP == 2 ) then
               ! XXXX
               ! HAUTEUR IMPOSEE A L'AVAL
               Y1 = YFIX
               S1 = CSUR( IS , YFIX , DZ , SGEO , NMLARG , Erreur )
               if (Erreur%Numero /= 0) then
                 return
               endif

               C1 = CELE(IS,S1,SGEO,ALGEO,Erreur)
               if( Erreur%Numero /= 0 ) then
                  return
               endif

               Q1 = S1 * FP - S1 * AKIV( IS , S1 , AKGEO , SGEO , NMLARG , Erreur )
               if( Erreur%Numero /= 0 ) then
                  return
               endif
            else if (ITYP == 1) then
               !                                     XXXX
               !                      DEBIT IMPOSE A L'AVAL
               !                      CALCUL DE Y A PARTIR DE L INVARIANT
               !                      DE RIEMANN REMONTE ET DU DEBIT IMPOSE
               S11 = SNODE(IS)
               M   = 0
               !                      BOUCLE SUR LES ITERATIONS DE NEWTON
               do while( M == 0.or.( ( ERROR > EPS6 ) .and. ( M < 20 ) ) )
                  S0     = S11
                  FS0    = QFIX + S0 * AKIV( IS , S0 , AKGEO , SGEO , NMLARG , Erreur ) - S0 * FP
                  if( Erreur%Numero /= 0 ) then
                     return
                  endif
                  FPRMS0 = + AKIV( IS , S0 , AKGEO , SGEO , NMLARG , Erreur ) + CELE( IS , S0 , SGEO , ALGEO , Erreur ) - FP
                  if( Erreur%Numero /= 0 ) then
                     return
                  endif
                  S11   = S0 - FS0 / FPRMS0
                  ERROR = dabs( S11 - S0 ) / S0
                  M     = M + 1
               end do

               S1 = S0
               Y1 = CSURM1( S1 , DZ(NBSECT) , SGEO(NBSECT,:) , Erreur )
               C1 = CELE( NBSECT , S0 , SGEO , ALGEO , Erreur )
               Q1 = QFIX
            endif
            !                                      XXXXX
            !                 VERIFICATION DE LA COHERENCE DES CL
            if( S1 < EPS6 ) then
               ALAMBM = QFIX / EPS6 - C1
            else
               ALAMBM = QFIX / S1   - C1
            endif

            if( ALAMBM > 0._DOUBLE ) then
              ! PRINT*,'SSP LIMITE: FLUVIAL-DEBIT AVAL DONNE'
              ! PRINT*,'L''ECOULEMENT DEVIENT TORRENTIEL A L'' AVAL'
              ! WRITE (UniteListing,*)'ALAMBM POSITIF ',ALAMBM
            endif

            if( S1 < EPS6 ) then
               SNO = EPS6
               FM  = QFIX / EPS6 - AKIV( IS , SNO , AKGEO , SGEO , NMLARG , Erreur )
               if( Erreur%Numero /= 0 ) then
                  return
               endif
            else
               FM  = QFIX / S1   - AKIV( NBSECT , S1 , AKGEO , SGEO , NMLARG , Erreur )
               if( Erreur%Numero /= 0 ) then
                  return
               endif
            endif
            ! EST-ON TOUJOURS EN FLUVIAL?
            ! SI PIED DE LA CARACTERISTIQUE CPLUS TORRENTIEL
            !     --> FMD TORRENTIEL
            if( ( UP - CP ) >= 0._DOUBLE ) then
               ! SPD N'EST PAS VRAIMENT EN NBSECT MAIS CFL<1
               FM = UP - AKIV( IS , SP , AKGEO , SGEO , NMLARG , Erreur )
               if( Erreur%Numero /= 0 ) then
                  return
               endif
               if( Impression ) write(UniteListing,*) 'FMD TORRENTIEL' , FM
            endif
         else label_froude2
            !                      ----------
            !                      TORRENTIEL
            !                      ----------
            !                      CALCUL DE FPD
            !                                ---
            !                      CONVECTION DE L'INVARIANT FPLUS
            IPOINT = IS
            TCAR   = 1
            !                      NVAL NOMBRE DE POINTS PRIS EN COMPTE
            NVAL  = min( 10 , NB )
            NSECG = IS - NVAL
            NSECD = IS

            do I = NSECG , NSECD
               call FROTTE (   &
                   FRNODE(I), &
                   I        , &
                   SNODE(I) , &
                   QNODE(I) , &
                   DEBGEO   , &
                   SGEO     , &
                   NMLARG   , &
                   Erreur )
               if( Erreur%Numero /= 0 ) then
                  return
               endif

               AKNODE(I) = AKIV( I , SNODE(I) , AKGEO , SGEO , NMLARG , Erreur )

               if( Erreur%Numero /= 0 ) then
                  return
               endif
            end do

            call CARAC3 ( &
                UP     , &
                SP     , &
                CP     , &
                FP     , &
                IPOINT , &
                UNODE  , &
                SNODE  , &
                CNODE  , &
                AKNODE , &
                TCAR   , &
                X      , &
                DT     , &
                NSECG  , &
                NSECD  , &
                COTR   , &
                FRNODE , &
                SGEO   , &
                AIGEO  , &
                DYGEO  , &
                NBSECT , &
                NMLARG , &
                Impression  , &
                UniteListing, &
                Erreur      &
                )
            if (Erreur%Numero /= 0) then
               return
            endif

            ! CALCUL DE FM
            !      ---
            ! CONVECTION DE L'INVARIANT FMOINS
            IPOINT= IS
            TCAR  = -1
            NVAL  = min( 10 , NB )
            NSECG = IS - NVAL
            NSECD = IS

            call CARAC3 ( &
                UM     , &
                SM     , &
                CM     , &
                FM     , &
                IPOINT , &
                UNODE  , &
                SNODE  , &
                CNODE  , &
                AKNODE , &
                TCAR   , &
                X      , &
                DT     , &
                NSECG  , &
                NSECD  , &
                COTR   , &
                FRNODE , &
                SGEO   , &
                AIGEO  , &
                DYGEO  , &
                NBSECT , &
                NMLARG , &
                Impression  , &
                UniteListing, &
                Erreur      &
                )

            if( Erreur%Numero /= 0 ) then
              return
            endif

            !                      CONTROLE : EVITE BLOCAGE EN CRITIQUE A L'AVAL
            !                      IF (FROUDE <= 1.05) THEN
            if( FROUDE <= 1.5_DOUBLE ) then
               if( ( UP - CP ) >= 0._DOUBLE ) then
                  AKP = AKIV(IS,SP,AKGEO,SGEO,NMLARG,Erreur)
                  if( Erreur%Numero /= 0 ) then
                     return
                  endif
                  FM = UP - AKP
               endif
            endif
            !                      EST-ON TOUJOURS EN TORRENTIEL ?
            AK1 = ( FP - FM ) / 2._DOUBLE
            S1  = AKIVM1( IS , AK1 , AKGEO , SGEO , NMLARG , Erreur )
            if( Erreur%Numero /= 0 ) then
               return
            endif

            C1 = CELE( IS , S1 , SGEO , ALGEO , Erreur )
            if( Erreur%Numero /= 0 ) then
               return
            endif

            Q1    = S1 * ( FP + FM ) / 2._DOUBLE
            UDMCD = ( FP + FM ) / 2._DOUBLE - C1
            if( UDMCD < 0._DOUBLE ) then
               ! CONDITION IMPOSEE IMPOSSIBLE
            endif
         endif label_froude2
      else label_ityp
         !                     SORTIE LIBRE
         !          CONVECTION DES VARIABLES (S,Q) PAR LA VITESSE U
         IPOINT = IS
         !                      NVAL NOMBRE DE POINTS PRIS EN COMPTE
         NVAL  = min( 10 , NB )
         NSECG = IS - NVAL
         NSECD = IS
         INDIC = 0

         do I = NSECG , NSECD
            call FROTTE ( &
                  FRNODE(I) , &
                          I , &
                   SNODE(I) , &
                   QNODE(I) , &
                     DEBGEO , &
                       SGEO , &
                     NMLARG , &
                     Erreur )
            if( Erreur%Numero /= 0 ) then
               return
            endif

         end do

         call CARAC  ( &
             QP     , &
             INDIC  , &
             IPOINT , &
             UNODE  , &
             SNODE  , &
             QNODE  , &
             X      , &
             DT     , &
             NSECG  , &
             NSECD  , &
             COTR   , &
             FRNODE , &
             SGEO   , &
             AIGEO  , &
             DYGEO  , &
             NBSECT , &
             NMLARG , &
             Erreur   &
                    )
         if( Erreur%Numero /= 0 ) then
            return
         endif

         !                      CALCUL DE SM

         INDIC = 0
         call CARAC  ( &
             SP     , &
             INDIC  , &
             IPOINT , &
             UNODE  , &
             SNODE  , &
             SNODE  , &
             X      , &
             DT     , &
             NSECG  , &
             NSECD  , &
             COTR   , &
             FRNODE , &
             SGEO   , &
             AIGEO  , &
             DYGEO  , &
             NBSECT , &
             NMLARG , &
             Erreur   &
                    )

         if( Erreur%Numero /= 0 ) then
            return
         endif

         S1 = SP
         !                        S1 = SNODE(NBSECT)+DT*(QNODE(NBSECT-1)-QNODE(NBSECT))
         !    *                        /(X(NBSECT)-X(NBSECT-1))
         Y1 = CSURM1( S1 , DZ(IS) , SGEO(IS,:) , Erreur )
         if( Erreur%Numero /= 0 ) then
            return
         endif

         Q1 = QP

      endif label_ityp

   endif label_amont

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old

  return

end subroutine LIMITE
