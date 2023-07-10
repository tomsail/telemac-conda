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

subroutine CONFLU( &
                  S1 , &
                  Q1 , &
                   W , &
                AIRS , &
           Confluent , &
                 X1D , &
               QNODE , &
               SNODE , &
               ZNODE , &
                COTR , &
                  ST , &
                  DZ , &
                SGEO , &
               ICONF , &
               ITEMP , &
                  DT , &
                 EPS , &
              NMLARG , &
              ERREUR )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!
!   FONCTION : CALCUL  DES CONDITIONS LIMITES 1D AUX EXTREMITES
!                           D'UN CONFLUENT PAR UN COUPLAGE AVEC UNE
!                           GEOMETRIE 2D SIMPLIFIEE
!
!                    SOUS PROGRAMME APPELANT : CALCUL
!                    SOUS PROGRAMME APPELLE  : INIGEO
!                                              CFL2D1
!                                              CALCON
!                                              CFL2D
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  S1,Q1    ! TR !  R ! ETAT LIMITE RECHERCHE                        !
! !  W        ! TR !  M ! VARIABLE D'ETAT DANS LE CONFLUENT            !
! !  AIRS     ! TR !  D ! SURFACE DES CELLULES 2D                      !
! !XCONF,YCONF! TR !  D ! COORDONEES DES EXTREMEITES DU CONFLUENT      !
! !  TETACO   ! TR !  D ! DIRECTION DE CHAQUE BIEF AU CONFLUENT        !
! !  X1D      ! TR !  D ! COORDONEES MAILLAGE 1D                       !
! !  QNODE    ! TR !  D ! DEBIT (1D)                                   !
! !  SNODE    ! TR !  D ! SURFACE MOUILLEE (1D)                        !
! !  ZNODE    ! TR !  D ! COTE DE LA SURFACE LIBRE (1D)                !
! !  COTR     ! TR !  D ! COTE DU FOND (1D)                            !
! !  ST       ! TR !  D ! COEFF. DE STRICKLER (1D)                     !
! !  DZ       ! TR !  D ! PAS DE PLANIMETRAGE                          !
! !  SGEO     ! TR !  D ! SURFACE MOUILLEE PLANIMETREE                 !
! !  ISEC     ! TI !  D ! NUMERO SECTION CALCUL 1D LIMITE A UN CONFL.  !
! !  ISECVO   ! TI !  D ! NUMERO DE SA VOISINE                         !
! !  FINBIE   ! TI !  D ! INDICATEUR DE DEBUT ET FIN DE BIEF           !
! !  ICONF    !  I !  D ! NUMERO DU CONFLUENT                          !
! !  ITEMP    !  I !  D !                                              !
! !  DT       !  R !  D ! PAS DE TEMPS (1D)                            !
! !  EPS      !  R !  D ! HAUTEUR D'EAU MINIMALE                       !
! !  NMLARG   !    !  D !                                              !
! !___________!____!____!______________________________________________!  
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.  
! !  DT2D     !  R !  A ! PAS DE TEMPS DANS LE MODELE 2D               !
! !  T2D      !  R !  A ! TEMPS ECOULE DANS LE MODELE 2D               !
! !  BXY      ! TR !  A ! BARYCENTRE DES CELLULES 2D                   !
! !  NELMIN   ! TI !  A ! TABLEAU DES CELLULES FONCTION DES SEGMENTS   !
! !  VNOIN    ! TR !  A ! VECTEUR NORMAL AUX CELLULES INTERIEURS       !
! !  AIRSM1   ! TR !  A ! SURFACE DES CELLULES 2D AU PDT PRECEDENT     !
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
   use M_ERREUR_T    ! ERREUR 
   use M_CONFLUENT_T !Confluent 
   use M_CONSTANTES_CALCUL_C ! Phase du calcul
   use M_CALCON_I  ! Interface du sous-programme CALCON
   use M_CFL2D_I   ! Interface du sous-programme CFL2D
   use M_CFL2D1_I  ! Interface du sous-programme CFL2D1
   use M_CSUR_I    ! Interface de la fonction    CSUR
   use M_INIGEO_I  ! Interface du sous-programme INIGEO

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(  out) :: S1,Q1
   ! 1ere dimension 3, 2nde dimension 12
   real(DOUBLE), dimension(:,:)  , intent(inout) :: W
   ! 1ere dimension 12
   real(DOUBLE), dimension(:)    , intent(inout) :: AIRS
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(in)    :: X1D,QNODE,SNODE,ZNODE,COTR
   real(DOUBLE), dimension(:)    , intent(in)    :: ST,DZ
   ! 1ere dimension IM, 2nde dimension NMLARG
   real(DOUBLE), dimension(:,:)  , intent(in)    :: SGEO
   integer     ,                   intent(in)    :: ICONF,ITEMP
   real(DOUBLE),                   intent(in)    :: DT,EPS
   integer     ,                   intent(in)    :: NMLARG
   Type (ERREUR_T)                ,intent(inout) :: ERREUR
   Type (CONFLUENT_T), dimension(:),intent(in  ) :: CONFLUENT

   !.. Variables locales ..
   !-----------------------
   integer                       :: IEL,IS,IFONC,IAFFLU
   integer                       :: IFIN,ICOMPT
   integer     , dimension(12,2) :: NELMIN
   integer     , dimension(18)   :: NFRELM
   integer                       :: NSEGP,NSEGR
   real(DOUBLE)                  :: ST2D
   real(DOUBLE)                  :: DT2D,T2D
   real(DOUBLE), dimension(12,2) :: BXY
   real(DOUBLE), dimension(12)   :: ZF,AIRSM1
   real(DOUBLE), dimension(3,12) :: VNOIN
   real(DOUBLE), dimension(3,18) :: VNOFR
   ! 1ere dimension 3
   real(DOUBLE), dimension(3)    :: XCONF,YCONF,TETACO
   ! 1ere dimension 3
   integer     , dimension(3)    :: ISEC,ISECVO,FINBIE
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>CONFLU'

   !Affectation des variables  liees au structure Confluent
   XCONF(:)  = Confluent(Iconf)%AbscisseAfflu(:)
   YCONF(:)  = Confluent(Iconf)%OrdonneeAfflu(:)
   TETACO(:) = Confluent(Iconf)%AngleAfflu(:)
   ISEC  (:) = Confluent(Iconf)%Isec(:)
   ISECVO(:) = Confluent(Iconf)%Isecvo(:)
   FINBIE(:) = Confluent(Iconf)%Finbie(:)

   if( ITEMP > 1 ) then 
      do IEL = 1 , 12
         AIRSM1(IEL) = AIRS(IEL)
      end do
   endif

   call INIGEO (  &
       AIRS    , &
       BXY     , &
       ZF      , &
       NELMIN  , &
       NFRELM  , &
       NSEGP   , &
       NSEGR   , &
       VNOIN   , &
       VNOFR   , &
       XCONF   , &
       YCONF   , &
       TETACO  , &
       SNODE   , &
       ZNODE   , &
       COTR    , &
       X1D     , &
       DZ      , &
       SGEO    , &
       ISEC    , &
       ISECVO  , &
       ICONF   , &
       NMLARG  , &
       ERREUR    &
       )

   if( Erreur%Numero /= 0 ) then
     return
   endif

   if( ITEMP > 1 ) then
      ! PRISE EN COMPTE DES MODIFICATIONS DE GEOMETRIE DANS LE CONFLUENT
      do IEL = 1 , 3
         if( W(1,IEL) > 10._DOUBLE * EPS ) then
            do IFONC = 1 , 3
               W(IFONC,IEL) = W(IFONC,IEL) * AIRSM1(IEL) / AIRS(IEL)
            end do
         endif
      end do

      do IEL = 7 , 12
         !            IF (W(1,IEL) > 10.D0*EPS) THEN
         do IFONC = 1 , 3
            W(IFONC,IEL) = W(IFONC,IEL) * AIRSM1(IEL) / AIRS(IEL)
         end do
         !            ENDIF           
      end do
   endif

   ST2D = ( ST(ISEC(1)) + ST(ISEC(2)) + ST(ISEC(3)) ) / 3._DOUBLE

   T2D    = 0._DOUBLE
   IFIN   = 0
   ICOMPT = 0

   ! BOUCLE EN TEMPS DANS LE DOMAINE 2D
   ! ==================================
   do while( IFIN == 0 )
      call CFL2D1 ( &
           DT2D   , &
           W      , &
           BXY    , &
           VNOIN  , &
           NELMIN , &
           DT     , &
           EPS    , &
           Erreur   &
           )

      if( Erreur%Numero /= 0 ) then
         return
      endif

      if( T2D + DT2D > DT ) then
         DT2D = DT - T2D
         IFIN = 1
      endif

      T2D    = T2D + DT2D
      ICOMPT = ICOMPT + 1

      ! CONDITIONS AUX LIMITES IMPOSEES POUR LE 2D
      ! ------------------------------------------

      do IEL = 1 , 3
         IS         = ISECVO(IEL)
         W(1,IEL+3) = ZNODE(IS) - COTR(IS)
         if( FINBIE(IEL) == 0 ) then
            W(2,IEL+3) = QNODE(IS) * VNOFR(1,IEL) / VNOFR(3,IEL)
            W(3,IEL+3) = QNODE(IS) * VNOFR(2,IEL) / VNOFR(3,IEL)
         else
            W(2,IEL+3) = -QNODE(IS) * VNOFR(1,IEL) / VNOFR(3,IEL)
            W(3,IEL+3) = -QNODE(IS) * VNOFR(2,IEL) / VNOFR(3,IEL)
         endif
      end do

      call CALCON( &
           W       , &
           NSEGP   , &
           NSEGR   , &
           AIRS    , &
           BXY     , &
           ZF      , &
           ST2D    , &
           NELMIN  , &
           NFRELM  , &
           VNOIN   , &
           VNOFR   , &
           DT2D    , &
           EPS     , &
           Erreur    &
           )

      if( Erreur%Numero /= 0 ) then
         return
      endif

      call CFL2D( &
          W     , &
          BXY   , &
          NELMIN, &
          DT2D  , &
          EPS   , &
          ICONF , &
          ERREUR  &
          )

      if( Erreur%Numero /= 0 ) then
         return
      endif

   end do


   ! FIN DE LA BOUCLE EN TEMPS
   ! -------------------------
   do IAFFLU = 1 , 3
      W(1,IAFFLU)      = dmax1( EPS /10.d0 , W(1,IAFFLU) )
      S1(ISEC(IAFFLU)) = CSUR( ISEC(IAFFLU) , W(1,IAFFLU) , DZ , SGEO , NMLARG , ERREUR )

      if( Erreur%Numero /= 0 ) then
         return
      endif

      if( FINBIE(IAFFLU) == 0 ) then
         Q1(ISEC(IAFFLU)) = ( +W(2,IAFFLU) * VNOFR(1,IAFFLU) &
                            + W(3,IAFFLU) * VNOFR(2,IAFFLU) ) &
                            * VNOFR(3,IAFFLU)
      else
         Q1(ISEC(IAFFLU)) = ( -W(2,IAFFLU) * VNOFR(1,IAFFLU) &
                            - W(3,IAFFLU) * VNOFR(2,IAFFLU)) &
                            * VNOFR(3,IAFFLU)
      endif
   end do

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine CONFLU
