!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET-TRACER.
!
!   MASCARET-TRACER is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET-TRACER is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET-TRACER.  If not, see <http://www.gnu.org/licenses/>
!

module M_BILMAS_I
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   SUBROUTINE BILMAS( MASSE , FLUMAS , FLUENT , FLUSOR , FLUSRC , &
                      Source_ajoutee , itrac  ,                     &
                      ibief , Nb_sect, DT     , NSCMP  , Constrac , &
                      C , A , X , B , U , &
                      RK , IPASS )

   !***********************************************************************
   !
   !  FONCTION :
   !  --------
   !
   !
   !    SOUS PROGRAMME CALCULANT LA MASSE TOTALE
   !    DE CHAQUE TRACEUR PRESENT DANS LE DOMAINE
   !    AINSI QUE LES MASSES DE TRACEURS PASSEES AU TRAVERS D'UNE
   !    SECTION DE CALCUL CHOISIE PAR L'UTILISATEUR
   !
   !___________________________________________________________________
   !   NOM   !TYPE!MODE!                      ROLE                     !
   !_________!____!____!_______________________________________________!
   !                                                                   !
   !                              PARAMETRES D'APPEL                   !
   !___________________________________________________________________!
   !  NBTRA  ! E  ! >  ! Nbre de traceurs                              !
   !  DT     ! R  ! >  ! Pas de temps                                  !
   ! IM     ! R  ! >  ! Nombre de sections de calcul                   !
   ! NSCMP  ! E  ! >  ! Section de calcul ou on calcul la masse passee !
   ! CONV   ! C  ! >  ! Convection des traceur                         !
   !   C     ! TR ! >  ! Concentration de traceur                      !
   !   A     ! TR ! >  ! Section mouillee                              !
   !   X     ! TR ! >  ! Abscisse de la section de calcul              !
   !   B     ! TR ! >  ! Compteur du Numero du pas de LIDO (en NP)     !
   !   U     ! TR ! >  ! Vitesse de l'eau                              !
   !   RK    ! TR ! >  ! Coefficient de dispersion                     !
   ! MASSE   ! TR !    ! Masse du traceur j dans le domaine            !
   ! FLUMAS  ! TR !    ! Masse du traceur j passant au pt NSCMP du     !
   !                                                          maillage !
   ! FLUENT  ! TR !    ! Masse du traceur j entrant dans le domaine    !
   ! FLUSOR  ! TR !    ! Masse du traceur j sortant du domaine         !
   !_________!____!____!_______________________________________________!
   !                                                                   !
   !                            VARIABLES INTERNES                     !
   !___________________________________________________________________!
   ! RMANN   ! R  !    ! Masse de traceur au point i du maillage       !
   ! NSCMP2  ! E  !    ! NSCMP pour calcul du gradient                 !
   !-------------------------------------------------------------------
   !                                                                   !
   !                            VARIABLES EN COMMON                    !
   !___________________________________________________________________!
   ! NFM     ! E  !    ! Numero du fichier de stockage des masses      !
   ! NFM2    ! E  !    ! Num du fich de stock masse passee en 1 section!
   !-------------------------------------------------------------------
   !  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
   !               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
   !-----------------------------------------------------------------------
   !
   !   FICHIERS ENTREE/SORTIE :
   !   ----------------------
   !
   !   SOUS-PROGRAMMES APPELANT : TRACER
   !   ------------------------
   !
   !   SOUS-PROGRAMMES APPELES :
   !   -----------------------
   !
   !   COMMENTAIRES :
   !   ------------
   !
   !***********************************************************************

   use M_PRECISION
   use M_CONSTANTES_TRACER_T

   implicit none

   Type (CONSTANTES_TRACER_T), dimension(:),intent(inout) :: ConsTrac
   real (DOUBLE) ,                  intent(inout) :: MASSE
   real (DOUBLE) ,                  intent(inout) :: FLUMAS,FLUENT,FLUSOR,FLUSRC
   real (DOUBLE) , dimension(:)   , intent(in)  :: C, Source_ajoutee
   real (DOUBLE) , dimension(:)   , intent(in)  :: A,B,U,X,RK
   real (DOUBLE) DT
   integer  NSCMP, IPASS
   integer  itrac, ibief, Nb_sect

   end subroutine BILMAS

   end interface

end module M_BILMAS_I
