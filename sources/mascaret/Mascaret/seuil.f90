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

subroutine SEUIL( &
               COTR , & 
              QFIXG , &
              SGAUC , &
              SDROI , &
              ZVRAI , &
              SVRAI , &
              QVRAI , &
              YVRAI , &
              ITEMP , &
             IVALID , &
             NBSECT , &
         Impression , & ! Flag d'impression
       UniteListing , & ! Unite logique fichier listing
             Erreur &
                    )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!    FONCTION : SOLUTION ANALYTIQUE DU PROBLEME DU SEUIL
!
!-----------------------------------------------------------------------

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C ! GPES, EPS4, EPS6
   use M_ERREUR_T    ! ERREUR
   use M_FH_I        ! Interface de la fonction FH
   use M_FCONJ_I     ! Interface de la fonction FCONJ
   use M_ZBRENT_I    ! Interface du sous-programme ZBRENT

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------

   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(in)    :: COTR
   real(DOUBLE),                   intent(in)    :: QFIXG
   real(DOUBLE),                   intent(in)    :: SGAUC,SDROI
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(  out) :: ZVRAI,SVRAI,QVRAI
   real(DOUBLE), dimension(:)    , intent(  out) :: YVRAI
   integer     ,                   intent(in)    :: ITEMP,IVALID
   integer     ,                   intent(in)    :: NBSECT
   logical                       , intent(in   ) :: Impression
   integer     ,                   intent(in   ) :: UniteListing
   Type (ERREUR_T)               , intent(inout) :: ERREUR

   !.. Communs ..
   !-------------
   Integer       :: ALPHA
   real (double) :: A1,A2,A3

   common / COEFS  / ALPHA
   common / COEFH  / A1,A2,A3 

   !.. Variables locales ..
   !-----------------------
   integer      :: NOEUD,NOEUDF,ISEUIL,IL,MAXFN,INOEUD
   integer      :: BETA
   real(DOUBLE) :: YGAUC,YDROI,ALARG,FRIN,FROUT,H0,HMIN
   real(DOUBLE) :: COTMIN,YCRIT,HCHAR,EPS,HSMIN,HSAVAL,YA,YB
   real(DOUBLE) :: SCRIT,YCONJ,SCONJ,CSTE
   real(DOUBLE) :: CDROI,CGAUC,HAMONT,HAVAL,YCRITD,RES,SFLU
   real(DOUBLE) :: SVF(2*NBSECT),QVF(2*NBSECT),FRVF(2*NBSECT)
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>SEUIL'

   ALARG = 1._DOUBLE
   BETA  = 1
   ALPHA = 1

   ! PASSAGE UNIQUE DANS LA SUBROUTINE SEUIL
   ! =======================================
   if( Impression ) then
      write(UniteListing,1000)
      if( IVALID == 2 ) then
         write(UniteListing,1010)
      else
         write(UniteListing,1020)
      endif
      write(UniteListing,1030) ALARG
   endif

   !     MINIMUM DE LA COTE DU RADIER 
   COTMIN = COTR(1)
   do NOEUD = 2 , NBSECT
      if( COTR(NOEUD) < COTMIN ) COTMIN = COTR(NOEUD)
   end do

   !    SOLUTION ANALYTIQUE CALCULEE EXACTEMENT
   !    VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
   YGAUC = SGAUC / ALARG
   YDROI = SDROI / ALARG
   CGAUC = dsqrt( GPES * YGAUC )
   CDROI = dsqrt( GPES * YDROI )

   !    NOMBRE DE FROUDE A L'ENTREE
   FRIN  = QFIXG /( SGAUC * CGAUC )
   FROUT = QFIXG /( SDROI * CDROI )

   !    CHARGE DE L'ECOULEMENT NON TRANSCRITIQUE EVENTUEL
   if( FRIN <= 1._DOUBLE ) then
      H0 = QFIXG**2 / ( 2._DOUBLE * GPES * SDROI**2 ) + ( YDROI + COTR(NBSECT) - COTMIN )
   else
      H0 = QFIXG**2 / ( 2._DOUBLE * GPES * SGAUC**2 ) + ( YGAUC + COTR(1) - COTMIN )
   endif

   !     CAS D'UN ECOULEMENT TRANSCRITIQUE
   if( ( FRIN <= 1._DOUBLE ).and.( FROUT > 1._DOUBLE ) ) then
      H0 = QFIXG**2 / ( 2._DOUBLE * GPES * SGAUC**2 ) + ( YGAUC + COTR(1) - COTMIN )
   endif

   !     EST-ELLE SUFFISANTE ?
   !     IL FAUT COMPARER A LA PLUS GRANDE CHARGE MINIMUM
   HMIN = EPS6
   do NOEUD = 1 , NBSECT
      YCRIT = ( ( ( QFIXG**2 ) * ALPHA ) / &
              ( GPES * ( BETA**2 ) ) )**( 1._DOUBLE / ( 2._DOUBLE * ALPHA + 1._DOUBLE ) )
      HCHAR = ( ( 0.5_DOUBLE / ALPHA ) + 1._DOUBLE ) * YCRIT + COTR(NOEUD) - COTMIN
      if( HCHAR > HMIN ) ISEUIL = NOEUD
      if( HCHAR > HMIN ) HMIN   = HCHAR
   end do

   IL = mod( ITEMP , 100 )
   if( IL == 0 .and. Impression ) then
      write(UniteListing,*) 'FROUDE ANALYTIQUE ENTREE' , FRIN
      write(UniteListing,*) 'FROUDE ANALYTIQUE SORTIE' , FROUT
      write(UniteListing,*) 'CHARGE EN ENTREE' , H0
      write(UniteListing,*) 'HMIN' , HMIN
   endif

   if( ( H0 - HMIN ) / HMIN >= EPS2 ) then
      !                                 XXXX
      !     OUI: ECOULEMENT SOIT FLUVIAL SOIT TORRENTIEL
      if( FRIN <= 1._DOUBLE ) then
         !                        ----
         if( Impression ) write(UniteListing,*) 'ECOULEMENT FLUVIAL'
         do NOEUD = 1 , NBSECT
            A1    = 1._DOUBLE
            A2    = COTR(NOEUD) - COTMIN - H0
            A3    = QFIXG**2 / ( 2._DOUBLE * GPES * ( BETA**2 ) )
            YCRIT = ( ( ( QFIXG**2 ) * ALPHA ) &
                      / ( GPES * ( BETA**2 ) ) )**( 1._DOUBLE / ( 2._DOUBLE * ALPHA + 1._DOUBLE ) )
            EPS   = EPS6
            YA    = dmax1( YCRIT , 1.D-4 )
            YB    = 1.E+4_DOUBLE
            MAXFN = 100

            call ZBRENT( FH , EPS , YA , YB , MAXFN , Impression , Erreur )
            if( Erreur%Numero /= 0 ) then
               return
            endif

            RES = YB
            SVRAI(NOEUD) = RES
            YVRAI(NOEUD) = SVRAI(NOEUD) / ALARG
            ZVRAI(NOEUD) = YVRAI(NOEUD) + COTR(NOEUD)
            QVRAI(NOEUD) = QFIXG
         end do
      else
         !        ----
         if( Impression ) write(UniteListing,*) 'ECOULEMENT TORRENTIEL'
         do NOEUD = 1 , NBSECT
            A1    = 1._DOUBLE
            A2    = COTR(NOEUD) - COTMIN - H0
            A3    = QFIXG**2 / ( 2._DOUBLE * GPES * ( BETA**2 ) )
            YCRIT = ( ( (QFIXG**2) * ALPHA) &
                    / ( GPES * ( BETA**2 )) )**( 1._DOUBLE / ( 2._DOUBLE * ALPHA + 1._DOUBLE ) )
            EPS   = EPS6
            YA    = EPS4
            YB    = YCRIT
            MAXFN = 100

            call ZBRENT (FH,EPS,YA,YB,MAXFN, Impression, Erreur)
            if( Erreur%Numero /= 0 ) then
               return
            endif

            RES = YB
            SVRAI(NOEUD) = RES
            YVRAI(NOEUD) = SVRAI(NOEUD) / ALARG
            ZVRAI(NOEUD) = YVRAI(NOEUD) + COTR(NOEUD)        
            QVRAI(NOEUD) = QFIXG
        end do
     endif
     !         -----
   else
   !      XXXX
      !      NON : ECOULEMENT TRANSCRITIQUE RESSAUT EVENTUEL
      write(UniteListing,*) 'ECOULEMENT TRANSCRITIQUE'
      HAMONT = HMIN
      !         LIGNE FLUVIALE
      do NOEUD = 1 , ISEUIL
         A1    = 1._DOUBLE
         A2    = COTR(NOEUD) - COTMIN - HAMONT
         A3    = QFIXG**2 / ( 2._DOUBLE * GPES * ( BETA**2 ) )
         YCRIT = (( (QFIXG**2) * ALPHA) &
                  / ( GPES * (BETA**2) ))**( 1._DOUBLE / ( 2._DOUBLE * ALPHA + 1._DOUBLE ) )
         EPS   = EPS6
         YA    = YCRIT
         YB    = 1.E+4_DOUBLE
         MAXFN = 100

         if( dabs( FH(YA) ) < EPS6 ) then
            !            SI NUMERIQUEMENT NUL EN YA -> YA SOLUTION
            YB  = YA
         else
            call ZBRENT( FH , EPS , YA , YB , MAXFN , Impression , Erreur )
            if( Erreur%Numero /= 0 ) then
               return
            endif
         endif
         if( Erreur%Numero /= 0 ) then
            return
         endif

         RES = YB
         SVRAI(NOEUD) = RES
         YVRAI(NOEUD) = SVRAI(NOEUD) / ALARG
         ZVRAI(NOEUD) = YVRAI(NOEUD) + COTR(NOEUD)
         QVRAI(NOEUD) = QFIXG
      end do
      !         LIGNE TORRENTIELLE
      do NOEUD = ISEUIL + 1 , NBSECT
         A1    = 1._DOUBLE
         A2    = COTR(NOEUD) - COTMIN - HAMONT
         A3    = QFIXG**2 / ( 2._DOUBLE * GPES * ( BETA**2 ) )
         YCRIT = ( ( (QFIXG**2)*ALPHA ) / &
                 ( GPES*( BETA**2 ) ) ) **( 1._DOUBLE / ( 2._DOUBLE *ALPHA + 1._DOUBLE ) )

         EPS   = EPS6
         YA    = EPS4
         YB    = YCRIT
         MAXFN = 100

         call ZBRENT( FH , EPS , YA , YB , MAXFN , Impression , Erreur )
         if( Erreur%Numero /= 0 ) then
            return
         endif

         RES = YB
         SVRAI(NOEUD) = RES
         YVRAI(NOEUD) = SVRAI(NOEUD) / ALARG
         ZVRAI(NOEUD) = YVRAI(NOEUD) + COTR(NOEUD)
         QVRAI(NOEUD) = QFIXG
      end do

      if( IVALID == 3 ) then
         if( Impression ) write(UniteListing,*) 'PAS DE RECHERCHE D UN RESSAUT A L'' AVAL  '
         !Erreur%arbredappel = !arbredappel_old
         return
      endif

      YCRITD = ( ( (QFIXG**2) * ALPHA ) / &
               ( GPES * ( BETA**2 ) ) ) **( 1._DOUBLE / ( 2._DOUBLE * ALPHA + 1._DOUBLE ) )
      if( YDROI <= YCRITD ) then
         if( Impression ) write(UniteListing,*) 'PAS DE RECHERCHE D UN RESSAUT A L', &
              'AVAL : COTE TORRENTIELLE'       
         !Erreur%arbredappel = !arbredappel_old
         return
      endif

       !          RECHERCHE D'UN RESSAUT

      if (Impression) write(UniteListing,*) 'RECHERCHE D''UN RESSAUT'

      !          LIGNE D'EAU FLUVIALE A PARTIR DE L'AVAL

      HAVAL = QFIXG**2 / ( 2._DOUBLE * GPES * SDROI**2 ) + ( YDROI + COTR(NBSECT) - COTMIN )

      NOEUDF = NBSECT + 1
      label_boucle_noeud : do NOEUD = NBSECT , ISEUIL + 1 , -1
         YCRIT  = ( ( (QFIXG**2) * ALPHA ) / &
                  ( GPES * ( BETA**2 ) ) ) **( 1._DOUBLE / ( 2._DOUBLE * ALPHA + 1._DOUBLE ) )
         HSMIN  = ( 1._DOUBLE / ( 2._DOUBLE * ALPHA ) + 1._DOUBLE ) * YCRIT
         HSAVAL = HAVAL - COTR(NOEUD) + COTMIN
         if( HSAVAL < HSMIN ) then
            NOEUDF = NOEUD + 1
            !                RESSAUT ENTRE NOEUDF ET IM
            if( Impression ) write(UniteListing,*) 'NOEUDF' , NOEUDF
            exit label_boucle_noeud
         endif

         A1    = 1._DOUBLE
         A2    = COTR(NOEUD) - COTMIN - HAVAL
         A3    = QFIXG**2 / ( 2._DOUBLE * GPES * ( BETA**2 ) )
         YCRIT = ( ( (QFIXG**2) * ALPHA ) / &
                 ( GPES * ( BETA**2 ) ) ) **( 1._DOUBLE / ( 2._DOUBLE * ALPHA + 1._DOUBLE ) )
         EPS   = EPS6
         YA    = YCRIT
         YB    = 1.E+4_DOUBLE
         MAXFN = 100

         call ZBRENT( FH , EPS , YA , YB , MAXFN , Impression , Erreur )
         if( Erreur%Numero /= 0 ) then
            return
         endif

         RES         = YB
         SVF(NOEUD)  = RES
         QVF(NOEUD)  = QFIXG
         FRVF(NOEUD) = QVF(NOEUD) / ( SVF(NOEUD) * dsqrt( GPES * SVF(NOEUD) ) )
      end do label_boucle_noeud

      if( NOEUDF <= NBSECT ) then
         !                           YYYY
         !          LIGNE D EAU TORRENTIELLE
         !          DE ISEUIL+1 A NOEUDF
         !          RESSAUT ENTRE NOEUDF+1 ET IM
         !          POSITION DU RESSAUT:
         do NOEUD = NOEUDF , NBSECT
            !              HAUTEUR CONJUGUEE DU FLUVIAL
            SFLU  = SVF(NOEUD)
            CSTE  = QFIXG**2 / SFLU + 0.5_DOUBLE * GPES * (SFLU)**2
            A1    = GPES * BETA * ( ALPHA + 1._DOUBLE )
            A2    = -CSTE
            A3    = QFIXG**2 / BETA
            YCRIT = ( ( ( QFIXG**2 ) * ALPHA ) /  &
                    ( GPES * (BETA**2) ) )**( 1._DOUBLE / ( 2._DOUBLE * ALPHA + 1._DOUBLE ) )
            EPS   = EPS6
            YA    = EPS6
            YB    = YCRIT
            MAXFN = 100

            call ZBRENT( FCONJ , EPS , YA , YB , MAXFN , Impression , Erreur )
            if (Erreur%Numero /= 0) then
               return
            endif

            YCONJ = YB
            !              WRITE(UniteListing,*) 'YCONJ ',YCONJ
            SCONJ = YCONJ
            if( SCONJ <= SVRAI(NOEUD) ) then
               !              RESSAUT AU NOEUD NOEUD:
               do INOEUD = NOEUD + 1 , NBSECT
                  SVRAI(INOEUD) = SVF(INOEUD)
                  YVRAI(INOEUD) = SVRAI(INOEUD) / ALARG
                  ZVRAI(INOEUD) = YVRAI(INOEUD) + COTR(INOEUD)
                  QVRAI(INOEUD) = QVF(INOEUD)
               end do
               if( Impression ) write(UniteListing,*) 'RESSAUT EN NOEUD',NOEUD
               SCRIT = YCRIT
               if( Impression ) write(UniteListing,*) 'SCRIT,STORR,SFLU',SCRIT,SVRAI(NOEUD),SVF(NOEUD)
               !Erreur%arbredappel = !arbredappel_old
               return
            endif
         end do
      endif
      !         YYYYY
      if (Impression) write(UniteListing,*) 'PAS DE RESSAUT'
   endif
   !      XXXXX

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old
   return

   !*****************************************************************************
   ! FORMATS D'IMPRESSION
   !*****************************************************************************

1000 format(/,2X,'===================================',/ &
       2X,'TEST DE VALIDATION DU CODE MASCARET',/ &
       ,2X,'===================================',// &
       ,2X,'ECOULEMENT AU DESSUS D UN SEUIL : ' &
       ,'SOLUTION ANALYTIQUE',/) 
1010 format(/,2X,'CAS FLUVIAL OU TORRENTIEL AVEC RESSAUT')
1020 format(/,2X,'CAS TORRENTIEL SANS RESSAUT')
1030 format(/,2X,'RAPPEL LARGEUR DU CANAL : ALARG = ',F5.2,/)    

end subroutine SEUIL
