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

subroutine CALCFL( &
                  DT , &
                 DTI , &
               IFIGE , &
              ICOMPT , &
                 Opt , &
               UNODE , &
               CNODE , &
                   X , &
                  I1 , &
                  I2 , &
                 CFL , &
              NBBIEF , &
      Phase_Post_Imp , & ! Flag d'impression
        UniteListing , & ! Unite logique fichier listing
               DTVAR , &
              Erreur )   ! Erreur

!***********************************************************************
! PROGICIEL : MASCARET        F. MAUREL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!
!  FONCTION :
!  --------
!
!      AJUSTEMENT DU PAS DE TEMPS EN FONCTION DE LA CONDITION CFL
!
!-----------------------------------------------------------------------
!
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  DT       !  R !  M ! PAS DE TEMPS                                 !
! !  UNODE    ! TR !  D ! VITESSE                                      !
! !  CNODE    ! TR !  D ! CELERITE                                     !
! !  X        ! TR !  D ! ABSCISSES DES POINTS DU MAILLAGE             !
! !  I1,I2    ! TI !  D ! INDICE DES EXTREMITES DE CHAQUE BIEF         !
! !  CFL      !  R !  M ! NOMBRE DE COURANT DESIRE                     !
! !  NBBIEF   !  I !    ! Nombre de biefs                              !
! !  DTVAR    !  L !  D ! LOGIQUE D'AJUSTEMENT DU PAS DE TEMPS         !
! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  CMAX     !  R !  M ! NOMBRE DE COURANT MAXIMAL                    !
! !  NMAX     !  I !  M ! POINT OU LE NB DE CORANT EST MAXIMAL         !
! !  CNODE1   !  R !  A ! VITESSE + CELERITE (U+C)                     !
! !  CNODE2   !  R !  A ! VITESSE - CELERITE (U-C)                     !
! !  COUR     !  R !  A ! NB. DE COURANT DANS LA CELLULE               !
! !  COU      !  R !  A ! (NB. DE COURANT DANS LA CELLULE)*DT          !
! !  CMA      !  R !  A ! (NB. DE COURANT MAX)*DT                      !
! !  NOEUD    !  I !  A ! POINT COURANT DU MAILLAGE                    !
! !  IBIEF    !  I !  A ! NUMERO DU BIEF COURANT                       !
! !___________!____!____!______________________________________________!
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
   use M_CONSTANTES_CALCUL_C ! phase du calcul
   use M_ERREUR_T  ! ERREUR

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE),               intent(inout) ::  DT
   real(DOUBLE), dimension(:), intent(inout) ::  DTI
   ! 1ere dimension IM
   real(DOUBLE), dimension(:), intent(in)    :: UNODE,CNODE,X
   integer     , dimension(:), intent(in)    :: I1,I2
   integer     , dimension(:),intent(inout)  :: IFIGE
   real(DOUBLE),               intent(in)    :: CFL
   integer     ,               intent(in)    :: NBBIEF
   integer     ,               intent(inout) :: Icompt
   Integer     ,               intent(in)    :: Phase_Post_Imp
   integer     ,               intent(in   ) :: UniteListing
   logical     ,               intent(in)    :: DTVAR,Opt
   Type (ERREUR_T)           , intent(inout) :: ERREUR

   !.. Variables locales ..
   !-----------------------
   integer        :: NMAX,NOEUD,IBIEF
   real(DOUBLE)   :: CMAX
   real(DOUBLE)   :: CMA,COU,COUR,CNODE1,CNODE2
   integer        :: K,KMAX
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================
   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>CALCFL'
   CMAX = 0._DOUBLE
   CMA  = 0._DOUBLE
   NMAX = 0
   KMAX = 1

   ! DETERMINATION DU NOMBRE DE COURANT MAXIMAL
   ! ------------------------------------------
   do IBIEF=1,NBBIEF

      do NOEUD = I1(IBIEF) , I2(IBIEF)

         CNODE1 = UNODE(NOEUD) - CNODE(NOEUD)
         CNODE2 = UNODE(NOEUD) + CNODE(NOEUD)

         if( CNODE1 < 0._DOUBLE.and.abs(CNODE1) > CNODE2 ) then

            !  CELERITE MAX : CNODE1-CELERITE NEGATIVE
            if( NOEUD > I1(IBIEF) ) then
               COUR = CNODE1 * DT / ( X(NOEUD) - X(NOEUD-1) )
               COU  = CNODE1 / ( X(NOEUD) - X(NOEUD-1) )
            else
               COUR = CNODE1*DT / ( X(NOEUD+1)-X(NOEUD  ) )
               COU  = CNODE1 / ( X(NOEUD+1)-X(NOEUD  ) )
            endif

            COUR       = dabs(COUR)
            COU        = dabs(COU)
            DTI(NOEUD) = CFL / COU

         else

           ! CELERITE MAX : CNODE2-CELERITE POSITIVE
           if( NOEUD < I2(IBIEF) ) then
              COUR = CNODE2 * DT / ( X(NOEUD+1) - X(NOEUD) )
              COU  = CNODE2 / ( X(NOEUD+1) - X(NOEUD) )
           else
              COUR       = CNODE2 * DT / ( X(NOEUD) - X(NOEUD-1) )
              COU        = CNODE2 / ( X(NOEUD) - X(NOEUD-1) )
              DTI(NOEUD) = CFL / COU
           endif
           DTI(NOEUD) = CFL / COU

         endif

         if( COUR > CMAX ) then
            CMAX = COUR
            CMA  = COU
            NMAX = NOEUD
         endif

      end do

   end do

   if( DTVAR ) then
      ! AJUSTEMENT DU PAS DE TEMPS
      ! --------------------------
      DT = CFL / CMA
      !
      ! Evaluation du rapport K
      !
      do IBIEF = 1 , NBBIEF
         do NOEUD = I1(IBIEF) , I2(IBIEF)
            K = NINT(DTI(Noeud) / DT)
            if( K > KMAX ) then
               KMAX = K
            endif
         enddo
      enddo
   endif

   !
   !!  Determination de la frequence de mise a jour des flux
   !
 !  if( Opt ) then
 !     if( KMAX <= 6 ) then
 !        KMAX = KMAX /2
 !     else
 !        KMAX = 4
 !     endif
 !  else
  !    KMAX = 1
  ! endif

   !
   ! DETERMINATION DES CELLULES FIGEES PENDANT KDT
   !      Annulation de cette fonstionnalite
   ! IFIGE EST BLOQUE A ZERO - DECONNECTE DANS SOLVRO
   !  print *,'icompt',icompt
   !  If( Icompt.GE.kmax ) Icompt = 1

   do Ibief = 1 , NBBief
      DO NOEUD = I1(IBIEF) , I2(IBIEF)
 !        IF( ( DTI(NOEUD) >= 2 * DT ) ) then
 !           IFIGE(NOEUD) = 1
 !        else
 !           if( NOEUD.LT.I2(IBIEF) ) IFIGE(NOEUD+1) = 0
 !           If( NOEUD.GT.I1(IBIEF) ) IFIGE(NOEUD-1) = 0
 !           IFIGE(NOEUD)                            = 0
 !        endif
 !       if( icompt==1 ) then
            IFIGE( NOEUD ) = 0
 !       endif
      enddo
   enddo

 ! DO i = 1 , 3
 !     IFIGE(i) = 0
 !  enddo

   ! IMPRESSION DU NOMBRE DE COURANT
   ! -------------------------------
   if (UniteListing > 0) then
       if( ( Phase_post_imp == PHASE_CALCUL ).OR.( Phase_post_imp == PHASE_INITIALISATION ) ) then
          write(UniteListing,1000) CMAX , X(NMAX) , KMAX
          write(UniteListing,1001)
          ! write (UniteListing,1002) (DTI(NOEUD),X(NOEUD),NOEUD=1,I2(1))
       endif
   endif

   !------------------
   ! Fin du traitement
   !------------------
   !Erreur%arbredappel = !arbredappel_old
   return

   ! FORMATS D'IMPRESSION
   !---------------------

1000 format(1X,'NOMBRE DE COURANT MAXIMAL : ',F6.2,' en X = ',F11.3,I5)
1001 format(1X,'NOMBRE DE COURANT LOCAL : ')
!1002 format(1X,'                          ',F6.2,' en X = ',F11.3)

end subroutine CALCFL
