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

subroutine SOURCE(          &
                       SOTILD , &
                       SOPRIM , &
                       SOFROT , &
                           ZG , &
                           ZD , &
                          PRG , &
                          PRD , &
                        CTILD , &
                         BETA , &
                            X , &
                        SNODE , &
       QNODE , UNODE , JDNODE , &
                          QIN , &
                       PCSing , &
                         COTR , &
                         SGEO , &
                        SGEOD , &
                       PRGEOD , &
                       DEBGED , &
                       FRTIMP , &
                          DZD , &
                           DZ , &
                        NOEUD , &
                        NSECG , &
                       NMLARG , &
      PerteElargissementTrans , &
                         CQMV , &
                       Erreur  &
                               )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!   FONCTION : RESOLUTION DES EQUATIONS DE SAINT VENANT
!                      PAR UN SCHEMA DE ROE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  SOTILD   ! TD !  A ! T. SOURCE  INTERFACE                         !
! !  SOPRIM   !  D !  A ! T. SOURCE (CENTRE DE LA CELLULE)             !
! !  SOFROT   ! TD !  A ! FROTTEMENT INTERFACE                         !
! !  ZG       !  D !  A ! COTE SURFACE LIBRE CELLULE DE GAUCHE         !
! !  ZD       !  D !  A ! COTE SURFACE LIBRE CELLULE DE DROITE         !
! !  CTILD    !  D !  A ! CELERITE MOYENNE DE ROE                      !
! !  X        ! TR !  D ! ABSCISSES DES POINTS DU MAILLAGE             !
! !  SNODE    ! TR !    !                                              !
! !  QNODE    ! TR !    !                                              !
! !  QIN      ! TR !  D ! DEBIT D'APPORT                               !
! !  COTR     ! TR !  D ! COTE DU RADIER                               !
! !  SGEO     ! TR !  D ! SURFACES PLANIMETREES                        !
! !  SGEOD    ! TR !  D ! SURFACES PLANIMETREES(MAILLAGE DECALE)       !
! !  PRGEOD   ! TR !  D ! PRESSION PLANIMETREE (MAILLAGE DECALE)       !
! !  DEBGED   ! TR !    !                                              !
! !  FRTIMP   !  L !    !                                              !
! !  DZD      !    !    !                                              !
! !  DZ       !    !    !                                              !
! !  NOEUD    !    !    !                                              !
! !  NSECG    !    !    !                                              !
! !  NMLARG   !  I !  D !                                              !
! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  SMIL     ! TR !  A ! SECTION MOYENNE ENTRE DEUX CELLULES          !
! !  QMIL     ! TR !  A ! DEBIT MOYEN ENTRE DEUX CELLULES              !
! !  FROT     ! TR !  A ! FROTTEMENT A L'INTERFACE ENTRE DEUX CELLULES !
  !  SAP      ! TR !  A ! TERME SOURCE DANS QDM : APPORTS DE DEBIT     !
! !  SGPRI    ! TR !  A ! SECTION MOUILLE A GAUCHE POUR Z=ZD           !
! !  SDPRI    ! TR !  A ! SECTION MOUILLE A DROITE POUR Z=ZG           !
! !  HDPRI    ! TR !  A ! TIRANT D'EAU A DROITE POUR Z=ZG              !
! !  HGPRI    ! TR !  A ! TIRANT D'EAU A GAUCHE POUR Z=ZD              !
! !  DSDX     !  D !  A ! DERIVEE SECTION PAR RAPPORT A X A Z CONSTANT !
! !  PREPIG(D)!  D !  A ! PRESSION LIEE A LA CELLULE                   !
! !___________!____!____!______________________________________________!
!
!     TYPE : E (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C ! GPES
   use M_ERREUR_T  ! ERREUR
   use M_CSUR_I      ! Interface de la fonction    CSUR
   use M_FROTTD_I    ! Interface du sous-programme FROTTD
   use M_PRESD_I     ! Interface de la fonction    PRESD

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   ! 1ere dimension 2
   real(DOUBLE), dimension(:)    , intent(  out) :: SOTILD,SOPRIM,SOFROT
   real(DOUBLE),                   intent(in)    :: ZG,ZD,PRG,PRD
   real(DOUBLE),                   intent(in)    :: CTILD
   real(DOUBLE),                  intent(in)     :: BETA
   real(DOUBLE), dimension(:)    , intent(in)    :: X
   real(DOUBLE), dimension(:)    , intent(in)    :: SNODE,QNODE,QIN,COTR
   integer     , dimension(:)    , intent(in)    :: JDNODE
   real(DOUBLE), dimension(:)    , intent(in)    :: UNODE
   real(DOUBLE), dimension(:)    , intent(in)    :: PCSing
   ! 1ere dimension IM
   real(DOUBLE), dimension(:,:)  , intent(in)    :: SGEO
   ! 1ere dimension IM1
   real(DOUBLE), dimension(:,:)  , intent(in)    :: SGEOD,PRGEOD
   real(DOUBLE), dimension(:,:)  , intent(in)    :: DEBGED
   logical     ,                   intent(in)    :: FRTIMP
   logical     ,                   intent(in)    :: PerteElargissementTrans
   real(DOUBLE), dimension(:)    , intent(in)    :: DZD,DZ
   integer     ,                   intent(in)    :: NOEUD
   integer     ,                   intent(in)    :: NSECG
   integer     ,                   intent(in)    :: NMLARG
   integer     ,                   intent(in)    :: CQMV
   Type (ERREUR_T)               , intent(inout) :: ERREUR

   !.. Variables locales ..
   !-----------------------
   real(DOUBLE)   :: HDPRI,HGPRI,SDPRI,SGPRI,DSDX,VG,VD,DY,SG,SD
   real(DOUBLE)   :: SMIL,QMIL,FROT,SPERT,UMIL,Z,SAPP
   Integer        ::JG,JD
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0

   Z = 2._DOUBLE / ( X(NOEUD+1) - X(NOEUD) )
   SAPP = 0.D0

   ! CALCUL DES APPORTS DE DEBIT
   ! ---------------------------

   SOTILD(1) = -2._DOUBLE * QIN(NOEUD+1)

   ! POUR LES TERMES SOURCE : FORMULATION N UTILISANT QUE LA PRESSION
   ! ----------------------------------------------------------------
   HDPRI = ZG - COTR(NOEUD+1)
   HGPRI = ZD - COTR(NOEUD)

   IF( HDPRI < 0._DOUBLE ) then
      SDPRI =0._DOUBLE
   ELSE
      JG = int(HDPRI/DZ(NOEUD+1)) + 1
      JD = JG+1
      IF( JD.GT.NMLARG ) then
         Erreur%Numero = 1
         Erreur%Message = 'The number of steps for the vertical discretisation of the cross-sections is insufficient : &
                         &non-convergent algorithm (check the Courant Number)'
         return
      Endif

      DY = HDPRI - real( JG - 1 , DOUBLE ) * DZ(NOEUD+1)

      !     LA SECTION MOUILLEE ETANT COMPRISE ENTRE JD ET JG
      !     ON CALCULE LA SECTION MOUILLEE PAR INTERPOLATION
      SG    = SGEO(NOEUD+1,JG)
      SD    = SGEO(NOEUD+1,JD)
      SDPRI = ( SD * DY + SG * (DZ(NOEUD+1) - DY) ) / DZ(NOEUD+1)
   endif

   IF( HGPRI < 0._DOUBLE ) then
      SGPRI = 0._DOUBLE
   ELSE
      JG = int( HGPRI / DZ(NOEUD) ) + 1
      JD = JG+1
      IF( JD.GT.NMLARG ) then
         Erreur%Numero = 1
         Erreur%Message = 'The number of steps for the vertical discretisation of the cross-sections is insufficient : &
                        &non-convergent algorithm (check the Courant Number)'
         return
      Endif
      DY = HGPRI - real( JG - 1 , DOUBLE ) * DZ(NOEUD)
      !
      !     LA SECTION MOUILLEE ETANT COMPRISE ENTRE JD ET JG
      !     ON CALCULE LA SECTION MOUILLEE PAR INTERPOLATION
      SG    = SGEO(NOEUD,JG)
      SD    = SGEO(NOEUD,JD)
      SGPRI = ( SD * DY + SG * ( DZ(NOEUD) - DY ) ) / DZ(NOEUD)
   endif

   if( HGPRI < 0._DOUBLE ) then
      DSDX = Z * ( SDPRI - SNODE(NOEUD) )
   else if( HDPRI < 0._DOUBLE ) then
      DSDX = Z * ( SNODE(NOEUD+1) - SGPRI )
   else
      DSDX = Z * ( SNODE(NOEUD+1) + SDPRI - SNODE(NOEUD) - SGPRI ) / 2._DOUBLE
   endif

   SOTILD(2) = -DSDX * CTILD * CTILD

   !      TERME SOURCE CENTRE
   if( NOEUD > NSECG ) then
      SOPRIM(1) = 0._DOUBLE
      SOPRIM(2) = -PRG + PRD
   endif

   ! SOURCE DUE AU FROTTEMENT
   ! ------------------------
   if( FRTIMP ) then
      SOFROT(1) = 0._DOUBLE
      SOFROT(2) = 0._DOUBLE
   else
      SMIL = ( SNODE(NOEUD) + SNODE(NOEUD+1) ) / 2._DOUBLE
      QMIL = ( QNODE(NOEUD) + QNODE(NOEUD+1) ) / 2._DOUBLE
      call FROTTD( &
          FROT   , &
          NOEUD  , &
          SMIL   , &
          QMIL   , &
          DEBGED , &
          SGEOD  , &
   JDNODE(NOEUD) , &
          NMLARG , &
          Erreur   &
          )

      SOFROT(1) = 0._DOUBLE
      SOFROT(2) = 2._DOUBLE * GPES * SMIL * FROT

      if (Erreur%Numero /= 0) then
         !arbredappel_old    = trim(!Erreur%arbredappel)
         !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>SOURCE'
         return
      endif
   endif

   !   PRISE EN COMPTE DES PERTES DE CHARGE SINGULIERES
   SMIL  = ( SNODE(NOEUD) + SNODE(NOEUD+1) ) / 2._DOUBLE
   UMIL  = BETA * ( UNODE(NOEUD) + UNODE(NOEUD+1) ) / 2._DOUBLE
   SPERT = PCSing(NOEUD) * (UMIL*UMIL) * SMIL

   ! PERTES DE CHARGES AUTOMATIQUES
   VG = UNODE(NOEUD)
   VD = UNODE(NOEUD+1)
   If( PerteElargissementTrans ) then
      IF( (VG.GT.VD) .and. abs(PCSing(NOEUD)).LT.EPS6 ) THEN
         SPERT = 0.3D0 * SMIL * ( ( BETA * ( VG - VD ) )**2 )
      endif
   ENDIF
   !
   ! Si le debit est negatif
   !
   IF( UMIL.LT.0.D0) THEN
      SPERT = -SPERT
   ENDIF
   !
   !  Ajout des termes dus aux apports de debit dans la quantite de mvt
   !
   !
   if( CQMV.EQ.1 ) then
      SAPP = UMIL*SOTILD(1)
   endif
   SOFROT(2) = SOFROT(2) + Z * SPERT / 2._DOUBLE + SAPP

   !------------------
   ! Fin du traitement
   !------------------

   !  !Erreur%arbredappel = !arbredappel_old

   return

end subroutine SOURCE
