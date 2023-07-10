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

module M_INTERPOLATION_S
!***********************************************************************
! PROGICIEL : MASCARET
!                             A. LEBOSSE
!                             P. CHERUBINI
!                             S. PERON
!                             S. MANDELKERN
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!
!----------------------------------------------------------
!  Interface du sous systeme de protection PROTLIB
!  pour un code Fortran90.
!
!----------------------------------------------------------

   !
   ! Global Data
   INTEGER IFVP,ICVP,IVVP
   DOUBLE PRECISION RVVP
   COMMON/INFOS/RVVP,ICVP,IFVP,IVVP
   !
   ! Functions
   !
   INTERFACE
      INTEGER FUNCTION ISQRT0 (IVAL)
         INTEGER  , INTENT(IN) :: IVAL
      END FUNCTION
   END INTERFACE
   !
   INTERFACE
      INTEGER FUNCTION ISQRT (IVAL)
         INTEGER  , INTENT(IN) :: IVAL
      END FUNCTION
   END INTERFACE
   !
   INTERFACE
      DOUBLE PRECISION FUNCTION RSQRT (RVAL)
         DOUBLE PRECISION , INTENT(IN) :: RVAL
      END FUNCTION
   END INTERFACE
   !
   INTERFACE
      INTEGER FUNCTION ISQRTF (IVAL)
         INTEGER , INTENT(IN) :: IVAL
      END FUNCTION
   END INTERFACE

   !***********************************************************************
   !
   ! MODULE REALISANT UNE INTERPOLATION
   ! POUR UNE ABSCISSE DONNEE, IL RENVOIT L'ORDONNEE CALCULEE A PARTIR
   ! D'UNE LISTE DE POINTS CONSTITUANT LA FONCTION D"INTERPOLATION
   !
   !***********************************************************************

   integer, parameter :: PREMIER_ORDRE_INTERPOLATION = 1

   contains

   subroutine    INTERPOLATION_S    ( &
                             YT     , & ! Resultats
                             XT     , & ! Abscisse pour laquelle on veut YT
                             N      , & ! Ordre d'interpolation
                             X      , & ! Tableau des abscisses
                             Y      , & ! tableau des ordonnees
                             IX     , & ! dimension des tableaux X et Y
                             Erreur   & ! Erreur
                                    )

!***********************************************************************
!
!  FONCTION :
!  --------
!
!           INTERPOLATION DE LAGRANGE D'ORDRE N
!
!-----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ----------------------
!
!   SOUS PROGRAMMES APPELANTS :  - QCL
!   ---------------------------  - LEC_LIGNE
!
!
!
!   SOUS PROGRAMMES APPELES :    ---
!   -------------------------
!
!   COMMENTAIRES :        LE POLYNOME OPTIMAL D ORDRE N PASSANT PAR LES
!   ------------          POINTS (X ,Y ) EST DONNE PAR LA FORMULE:
!                                  I  I
!                         P(X) = SOMME(Y *PRODUIT( (X-X )/(X -X ) )
!                                J=1,N  J  I=1,N       I    J  I
!                                          I.NE.J
!                         EN EFFET P(X ) = Y QUELQUE SOIT J
!                                     J     J
!
   !============================= Declarations =============================
   use M_PRECISION
   use M_PARAMETRE_C
   use M_FICHIER_T
   use M_ERREUR_T
   use M_TRAITER_ERREUR_I
   use M_MESSAGE_C

   implicit none

   !.. Arguments ..
   real(DOUBLE), intent(  out)  :: YT
   real(DOUBLE), intent(in   )  :: XT
   integer,      intent(in   )  :: N , IX
   real(DOUBLE), dimension(:), intent(in) :: X , Y
   type(ERREUR_T), intent(inout) :: Erreur

   !.. Scalairs locaux ..
   integer        :: I,IMAX,IMIN,IMOY,IXM1,J
   real(DOUBLE)   :: COEFF,XMAX,XMIN
   !character(132) :: !arbredappel_old

   !.. Fonctions intrinseques ..
   intrinsic ABS, INT, REAL

   !============================ Instructions =================
   Erreur%Numero = 0
   !arbredappel_old = trim(Erreur%Arbredappel)
   Erreur%Arbredappel = trim(Erreur%Arbredappel)//'=>INTERPOLATION'

   !     TESTS SUR LES DONNEES
   !     --------------------
   !---------------------------------
   ! test sur l'ordre d'interpolation
   !---------------------------------
   if( N > IX ) then
      Erreur%Numero = 17
      Erreur%ft   = err_17
      Erreur%ft_c = err_17c
      call TRAITER_ERREUR (Erreur, N, IX)
      return
   end if

   !-----------------------------------
   ! test sur l'appartenance au domaine
   !-----------------------------------
   if( XT < X(1) .or. XT > X(IX) ) then
      Erreur%Numero   = 20
      Erreur%ft   = err_20
      Erreur%ft_c = err_20c
      call TRAITER_ERREUR (Erreur, XT, X(1), X(IX))
      return
   end if

   do I = 1 , IX
      if( DABS( XT - X(I) ) <= EPS10 ) then
         YT = Y(I)
         !Erreur%Arbredappel = !arbredappel_old
         return
      end if
   end do

   !     SELECTION DES N POINTS SERVANT A L INTERPOLATION
   !     ------------------------------------------------
   IXM1 = IX - 1
   do I = 1 , IXM1
      if( XT > X(I) .and. XT < X(I+1) ) exit
   end do

   if( DABS( Y(I) - Y(I+1) ) <= EPS10 ) then
      YT = Y(I)
   else
      !     SINON INTERPOLER
      IMOY = I
      XMIN = REAL( IMOY ) - REAL( N )/2.
      XMAX = REAL( IMOY ) + REAL( N )/2.
      IMIN = INT( XMIN ) + 1
      IMAX = INT( XMAX ) + 1

      !     INTERPOLATION DE LAGRANGE
      !     -------------------------
      YT = 0.
      do J = IMIN , IMAX
         COEFF = 1.
         do I = IMIN , IMAX
            if( I /= J ) then
               COEFF = COEFF * ( XT - X(I) ) / ( X(J) - X(I) )
            end if
         end do
         YT = YT + Y(J) * COEFF
      end do
   end if

   !Erreur%Arbredappel = !arbredappel_old

   return

   end subroutine INTERPOLATION_S

end module M_INTERPOLATION_S
