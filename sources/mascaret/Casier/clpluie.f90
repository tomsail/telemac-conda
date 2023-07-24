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

subroutine CLPLUIE( &
                    ApportPluie , & ! resultat, debit d apport du temps T-DT au temps T
                    T , DT      , & ! variable temps et pas de temps du calcul
                    Loi         , & ! hydrogramme de pluie
                    Erreur )        ! erreur

! ********************************************************************
! PROGICIEL : MASCARET                     A. LEBOSSE 
!                                          C. RISSOAN
!
! VERSION : V8P4R0                  EDF-CEREMA
!
!TRAITEMENT DES DEBITS D'APPORT DANS UN CASIER, REPRESENTANT UNE PLUIE
!*********************************************************************
!   FICHIERS ENTREE/SORTIE :  --
!   ----------------------      
!   SOUS PROGRAMMES APPELANTS : - principal
!   ---------------------------
!   SOUS PROGRAMMES APPELES :    - INTERPOLATION_S
!   -------------------------
!
!
!   commentaire : le calcul effectue la moyenne entre le temps T - DT 
!                 et le temps T
!
!

   !========================== Declarations ==============================
   use M_LOI_T           ! type LOI
   use M_APPORT_PLUIE_T  ! type APPORT PLUIE
   use M_ERREUR_T        ! type ERREUR
   use M_INTERPOLATION_S ! sous-programme INTEREPOLATION
   use M_PRECISION       ! type DOUBLE

   implicit none

   !.. Arguments ..
   type(APPORT_PLUIE_T) , dimension(:) , pointer       :: ApportPluie
   type(ERREUR_T)                      , intent(inout) :: Erreur
   type(LOI_T)          , dimension(:) , pointer       :: Loi
   real(DOUBLE)                        , intent(in   ) :: T , DT

   !.. Variables locales ..
   real(DOUBLE) :: q1 , & ! debit d apport au temps T-DT
                   q2 , & ! debit d apport au temps T
                   t1 , & ! T-DT
                   t2     ! T
   integer :: iapport, num_loi

   !========================== Instructions ==============================
   t1 = T-DT
   t2 = T

   do iapport = 1, size( ApportPluie )

      num_loi = ApportPluie(iapport)%NumeroLoi

      call INTERPOLATION_S( q1 , t1 , 1 , Loi(num_loi)%Temps , Loi(num_loi)%Debit , size( Loi(num_loi)%Temps ) , Erreur )
      if( Erreur%Numero /= 0 ) then
         return
      end if  

      call INTERPOLATION_S( q2 , t2 , 1 , Loi(num_loi)%Temps , Loi(num_loi)%Debit , size( Loi(num_loi)%Temps ) , Erreur )
      if( Erreur%Numero /= 0 ) then
         return
      end if

      ApportPluie(iapport)%Debit = ( q1 + q2 ) / 2._DOUBLE

   end do

   return

end subroutine CLPLUIE
