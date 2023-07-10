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

subroutine CALCON( &
                   W , &
               NSEGP , &
               NSEGR , &
                AIRS , &
                 BXY , &
                  ZF , &
                ST2D , &
              NELMIN , &
              NFRELM , &
               VNOIN , &
               VNOFR , &
                  DT , &
                 EPS , &
              Erreur )

!***********************************************************************
! PROGICIEL : MASCARET        F. MAUREL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!   FONCTION: RESOLUTION DE SAINT VENANT-2D PAR UNE METHODE VOLUME FINI
!                   (SOLVEUR DE ROE) POUR LE CALCUL DES CONFLUENTS
!                   PRISE EN COMPTE DU FROTTEMENT DE MANIERE IMPLICITE
!
!                  SOUS PROGRAMME APPELANT : CONFLU
!                  SOUS PROGRAMME APPELE   : FLUSRC
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  W        ! TR !  M ! VARIABLE D'ETAT DANS LE CONFLUENT            !
! !  NSEGP    !  I !  M ! NOMBRE DE SEGMENT TOTAL                      !
! !  NSEGR    !  I !  M ! NOMBRE DE SEGMENT INTERIEURS                 !
! !  AIRS     ! TR !  D ! SURFACE DES CELLULES 2D                      !
! !  BXY      ! TR !  D ! BARYCENTRE DES CELLULES 2D                   !
! !  ZF       ! TR !  D ! COTE DU FOND DES CELLULES 2D                 !
! !  ST2D     !  R !  D ! STRICKLER UNIQUE DANS LE 2D                  !
! !  NELMIN   ! TI !  D ! TABLEAU DES CELLULES FONCTION DES SEGMENTS   ! 
! !  NFRELM   ! TI !  D ! CELLULE FONCTION DU SEGMENT FRONTIERE        !
! !  VNOIN    ! TR !  D ! VECTEUR NORMAL AUX CELLULES INTERIEURES       !
! !  VNOFR    ! TR !  D ! VECTEUR NORMAL AUX CELLULES FRONTIERES       !
! !  DT       !  R !  D ! PAS DE TEMPS                                 !
! !  EPS      !  R !  D ! HAUTEUR D'EAU MINIMALE                       !
! !___________!____!____!______________________________________________!  
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.  
! !  FLUX     ! TR !  A ! FLUX ASSEMBLES                               !
! !  FLULOC   ! TR !  A ! FLUX DE ROE                                  !
! !  FLUSCE   ! TR !  A ! FLUX LIES AUX TERMES SOURCES                 !
! !  T,TW,TR  ! TR !  A ! VECTEURS PROPRES                             !
! !  RLAMBX   !  R !  A ! VALEUR PROPRE                                !
! !  KFROT    !  R !  A ! COEFFICIENT POUR CALCUL DU FROTTEMENT        !
! !  DELTA    !  R !  A ! DISCRIMANT EQ 2EME DEGRE SUR FROTTEMENT       !
! !___________!____!____!______________________________________________!  
!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************

   !============================= Declarations ===========================
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C ! GPES, EPS8
   use M_ERREUR_T    ! ERREUR
   use M_FLUSRC_I    ! Interface du sous-programme FLUSRC

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   ! .. Arguments ..
   !----------------
   ! 1ere dimension 3, 2nde dimension 12
   real(DOUBLE), dimension(:,:), intent(inout) :: W
   integer     ,                 intent(inout) :: NSEGP,NSEGR
   ! 1ere dimension 12 (2nde dimension 3 pour BXY)
   real(DOUBLE), dimension(:)  , intent(in)    :: AIRS
   real(DOUBLE), dimension(:,:), intent(in)    :: BXY
   real(DOUBLE), dimension(:)  , intent(in)    :: ZF
   real(DOUBLE),                 intent(in)    :: ST2D
   ! 1ere dimension 12, 2nde dimension 2
   integer     , dimension(:,:), intent(in)    :: NELMIN
   ! 1ere dimension 18
   integer     , dimension(:)  , intent(in)    :: NFRELM
   ! 1ere dimension 3, 2nde dimension 12
   real(DOUBLE), dimension(:,:), intent(in)    :: VNOIN
   ! 1ere dimension 3, 2nde dimension 18
   real(DOUBLE), dimension(:,:), intent(in)    :: VNOFR
   real(DOUBLE),                 intent(in)    :: DT,EPS
   Type (ERREUR_T)             , intent(inout) :: ERREUR

   !.. Varaibles locales ..
   !-----------------------
   real(DOUBLE), dimension(3)    :: FLULOC
   real(DOUBLE), dimension(12,3) :: FLUX
   real(DOUBLE), dimension(3,12) :: FLUSCE
   real(DOUBLE), dimension(3)    :: T,TW,TR
   real(DOUBLE)   :: HI,UI,VI,HJ,UJ,VJ,CT,CT2,UT,VT
   real(DOUBLE)   :: XN,YN,RNORM,CI,CI2,CJ,CJ2,ALPHA,RI,RJ
   real(DOUBLE)   :: RLAMB0,RLAMBM,RLAMBI,RLAMBJ,RLAMBP
   real(DOUBLE)   :: PRI,PRII,PRIJ,USN
   real(DOUBLE)   :: PS,SA
   real(DOUBLE)   :: KFROT,ALPHAF,DELTA
   integer        :: I,IEL,IEL1,IEL2,ISEGIN,IFON,ISEGP
   integer        :: NVPID1,INDIC
   integer        :: NSEGIN,NFON
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>CALCON'

   !------
   ! 1. INITIALISATIONS
   !------
   do I = 1 , 3
     FLULOC(I) = 0._DOUBLE
   end do

   do IEL = 1 , 12
      FLUX(IEL,1)   = 0._DOUBLE 
      FLUX(IEL,2)   = 0._DOUBLE
      FLUX(IEL,3)   = 0._DOUBLE
      FLUSCE(1,IEL) = 0._DOUBLE
      FLUSCE(2,IEL) = 0._DOUBLE
      FLUSCE(3,IEL) = 0._DOUBLE
   end do

   NVPID1 = 0

   !------
   ! 2. CALCUL DES FLUX INTERNES
   !------

   ! BOUCLE SUR LES SEGMENTS INTERIEURS
   ! ----------------------------------
   NSEGIN = 12
   NFON   = 3

   label_boucle_ISEGIN : do ISEGIN = 1 , NSEGIN

      IEL1  = NELMIN(ISEGIN,1)
      IEL2  = NELMIN(ISEGIN,2)
      INDIC = 0
      !   --->    QUELQUES CALCULS INTERMEDIAIRES
      !           ------------------------------
      HI = W(1,IEL1)
      if( HI > EPS ) then
         UI = W(2,IEL1) / HI
         VI = W(3,IEL1) / HI
      else
         HI        = EPS
         UI        = 0._DOUBLE
         VI        = 0._DOUBLE
         W(2,IEL1) = 0._DOUBLE
         W(3,IEL1) = 0._DOUBLE
         INDIC     = INDIC + 1
      endif

      HJ = W(1,IEL2)
      if( HJ > EPS ) then
         UJ = W(2,IEL2) / HJ
         VJ = W(3,IEL2) / HJ
      else
         HJ        = EPS
         UJ        = 0._DOUBLE
         VJ        = 0._DOUBLE
         W(2,IEL2) = 0._DOUBLE
         W(3,IEL2) = 0._DOUBLE
         INDIC     = INDIC + 1
      endif

      if( INDIC < 2 ) then
         XN    = VNOIN(1,ISEGIN)
         YN    = VNOIN(2,ISEGIN)
         RNORM = VNOIN(3,ISEGIN)

         !   --->    CALCUL DES MOYENNES DE ROE DE U,V,H,C**2 ET C
         !           ---------------------------------------------
         RI  = dsqrt( HI )
         RJ  = dsqrt( HJ )
         UT  = ( RI * UI + RJ * UJ ) /(RI + RJ)
         VT  = ( RI * VI + RJ * VJ ) /(RI + RJ)
         CT2 = GPES * ( HI + HJ ) / 2._DOUBLE
         CT  = dsqrt( CT2 )

         !   --->  TEST SUR LE SIGNE DE LA VALEUR PROPRE LAMB0 = <UT,N>
         !         ----------------------------------------------------------
         RLAMB0 = UT * XN + VT * YN
         if( RLAMB0 > 0._DOUBLE ) then
            ! ---- SEGMENT SORTIE---------
            ! --->    PETITS CALCULS
            RLAMBM = RLAMB0 - CT
            PRII   = GPES * (HI**2) / 2._DOUBLE
            PRIJ   = GPES * (HJ**2) / 2._DOUBLE
            ALPHA  = UI * XN + VI * YN

            !TBTB DEBUT : MODIFICATION DE RLAMBM SI RLAMBM < D1
            CI2    =  2._DOUBLE * PRII / HI
            CI     = dsqrt( CI2 )
            CJ2    =  2._DOUBLE * PRIJ / HJ
            CJ     = dsqrt( CJ2 )
            RLAMBI = ALPHA - CI
            RLAMBJ = UJ * XN + VJ * YN - CJ

            !TBTB : MODIF UNIQUEMENT DANS LA DETENTE :
            if( RLAMBI  <  0._DOUBLE .and. RLAMBJ  >  0._DOUBLE ) then
               RLAMBM = dmin1( 0._DOUBLE , RLAMBM ) - dabs( RLAMBI - RLAMBJ ) /4._DOUBLE
               NVPID1 = NVPID1 + 1
            endif

            ! --->    CALCUL DU FLUX 1
            NFON = 3
            do IFON = 1 , NFON
               FLULOC(IFON) = ALPHA * W(IFON,IEL1)
            end do

            FLULOC(2) = FLULOC(2) + PRII * XN
            FLULOC(3) = FLULOC(3) + PRII * YN

            !   --->    TEST SUR LE SIGNE DE LAMBDAM
            !           ----------------------------
            if( RLAMBM < 0._DOUBLE ) then
               T(1) = 1._DOUBLE
               T(2) = UT - CT * XN
               T(3) = VT - CT * YN

               do IFON = 1 , NFON
                  TR(IFON) = W(IFON,IEL2) - W(IFON,IEL1)
               end do

               TW(1) = ( UT * XN + VT * YN ) * CT + CT2
               TW(2) = -XN * CT
               TW(3) = -YN * CT

               PS    = TR(1) * TW(1) + TR(2) * TW(2) + TR(3) * TW(3)

               !   --->    CALCUL DU FLUX LOCAL TOTAL
               !           --------------------------
               SA       = PS * RLAMBM / (2._DOUBLE * CT2 )
               FLULOC(1)= FLULOC(1) + SA * T(1)
               FLULOC(2)= FLULOC(2) + SA * T(2)
               FLULOC(3)= FLULOC(3) + SA * T(3)

            endif
         else
            !   --->    PETITS CALCULS
            !           --------------
            RLAMBP = RLAMB0 + CT
            PRIJ   = GPES * (HJ**2) / 2._DOUBLE
            ALPHA  = UJ * XN + VJ* YN
            CI2    = GPES * HI
            CI     = dsqrt(CI2)
            CJ2    = GPES * HJ
            CJ     = dsqrt(CJ2)
           RLAMBI  = UI * XN + VI * YN + CI
           RLAMBJ  = ALPHA + CJ

            if( RLAMBI  <  0._DOUBLE .and. RLAMBJ  >  0._DOUBLE ) then
               RLAMBP = dmax1(0._DOUBLE,RLAMBP) + dabs(RLAMBI - RLAMBJ) / 4._DOUBLE
               NVPID1 = NVPID1 + 1
            endif

            !   --->    CALCUL DU FLUX 1
            !           ----------------
            do IFON = 1 , NFON
               FLULOC(IFON)= ALPHA * W(IFON,IEL2)
            end do

            FLULOC (2) = FLULOC(2) + PRIJ * XN
            FLULOC (3) = FLULOC(3) + PRIJ * YN

            !   --->    TEST SUR LE SIGNE DE LAMBDAP
            !           ----------------------------
            if( RLAMBP > 0._DOUBLE ) then
               T(1) = 1._DOUBLE
               T(2) = UT + CT * XN
               T(3) = VT + CT * YN

               do IFON = 1 , NFON
                  TR(IFON) = W(IFON,IEL2) - W(IFON,IEL1)
               end do

               TW(1) = (-UT * XN - VT * YN) * CT +CT2
               TW(2) = CT * XN
               TW(3) = CT * YN

               PS = TR(1)*TW(1)+TR(2)*TW(2)+TR(3)*TW(3)

               !   --->    CALCUL DU FLUX LOCAL TOTAL
               !           --------------------------
               SA        = - PS * RLAMBP / (2._DOUBLE * CT2 )
               FLULOC(1) = FLULOC(1) + SA *T(1)
               FLULOC(2) = FLULOC(2) + SA *T(2)
               FLULOC(3) = FLULOC(3) + SA *T(3)
            endif
         endif
      else
         RNORM     = 0._DOUBLE
         FLULOC(1) = 0._DOUBLE
         FLULOC(2) = 0._DOUBLE
         FLULOC(3) = 0._DOUBLE
      endif

      ! CALCUL DES FLUX LIES AUX TERMES SOURCES
      call FLUSRC( IEL1 , IEL2 , ISEGIN , VNOIN , W , FLUSCE , &
                   BXY , ZF , EPS , Erreur )

      if( Erreur%Numero /= 0 ) then
         return
      endif

      ! ASSEMBLAGE DES FLUX
      do IFON = 1 , NFON
         FLUX(IEL1,IFON) = FLUX(IEL1,IFON) + FLULOC(IFON) * RNORM + FLUSCE(IFON,IEL1)
         FLUX(IEL2,IFON) = FLUX(IEL2,IFON) - FLULOC(IFON) * RNORM + FLUSCE(IFON,IEL2)
      end do

   ! XXX    FIN DE LA BOUCLE SUR LES SEGMENTS
   !        ---------------------------------
   end do label_boucle_ISEGIN

   !------
   ! 5. CALCUL DES FLUX DE PAROI (technique de l'etat miroir pour evaluer les flux)
   !------
   NSEGP = 18
   do ISEGP = NSEGR + 1 , NSEGP
      IEL = NFRELM(ISEGP)
      if( W(1,IEL) > EPS ) then
         PRI = GPES * ( W(1,IEL)**2) / 2._DOUBLE
         USN = ( W(2,IEL) * VNOFR(1,ISEGP) + W(3,IEL) * VNOFR(2,ISEGP) ) / W(1,IEL)
         CT = dsqrt( GPES * W(1,IEL) )
         IF( ( USN + 2.D0 * CT ).LE.0.D0 ) THEN
            !GODUNOV
            PRI = 0.D0
         ELSEIF( ( ( USN + 2.D0 * CT ) .GE. 0. ) .AND. ( USN.LE.0. ) ) THEN
            !GODUNOV
            PRI = PRI * ( 1.D0 +  USN / 2.D0 / CT )   &
                 * ( 1.D0 +  USN / 2.D0 / CT )   &
                 * ( 1.D0 +  USN / 2.D0 / CT )   &
                 * ( 1.D0 +  USN / 2.D0 / CT )
         ELSEIF(USN.GE.0.D0) THEN
            ! VFROE
            PRI = PRI * ( 1.D0 + 2.D0 * USN / CT )
         ENDIF
         FLUX(IEL,2) = FLUX(IEL,2) + VNOFR(1,ISEGP) * VNOFR(3,ISEGP) * PRI
         FLUX(IEL,3) = FLUX(IEL,3) + VNOFR(2,ISEGP) * VNOFR(3,ISEGP) * PRI
      else
         FLUX(IEL,2) = FLUX(IEL,2)
         FLUX(IEL,3) = FLUX(IEL,3)
      endif
   end do

   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! CALCUL DES VARIABLES CONFLUENTS
   do IEL = 1 , 12
      ! PRISE EN COMPTE DES FLUX 
      W(1,IEL) = W(1,IEL) - DT * FLUX(IEL,1) / AIRS(IEL)
      W(2,IEL) = W(2,IEL) - DT * FLUX(IEL,2) / AIRS(IEL)
      W(3,IEL) = W(3,IEL) - DT * FLUX(IEL,3) / AIRS(IEL)
      ! PRISE EN COMPTE DU FROTTEMENT DE MANIERE IMPLICITE
      if( W(1,IEL) > EPS ) then
         KFROT = GPES * DT * dsqrt( W(2,IEL)**2 + W(3,IEL)**2 ) / &
                 ( ST2D * ST2D * W(1,IEL)**( 7._DOUBLE / 3._DOUBLE ) )
         if( KFROT > EPS8 ) then
            DELTA  = ( 1._DOUBLE + 4._DOUBLE * KFROT )
            ALPHAF = ( -1._DOUBLE + dsqrt(DELTA) ) / ( 2._DOUBLE * KFROT )
         else
            ALPHAF = 1._DOUBLE - KFROT
         endif
         W(2,IEL) = ALPHAF * W(2,IEL)
         W(3,IEL) = ALPHAF * W(3,IEL)
      endif
   end do

   !------------------
   ! Fin du traitement
   !------------------

  !Erreur%arbredappel = !arbredappel_old

   return

end subroutine CALCON
