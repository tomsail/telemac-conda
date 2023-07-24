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

module M_KSING_I
!***********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE   S. PERON   S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine  KSING(           &
      ASING,BSING,CSING,DSING , & ! Coefficients du seuil
                        QSING , & ! Debit au seuil
                     NumSeuil , & ! Numero de la singularite
                  Singularite , & ! Singularites
                        Appel , & ! Flag
                          ZAM , & ! Cote amont
                          ZAV , & ! Cote aval
                          QAM , & ! Debit amont
                          QAV , & ! Debit aval
                          BAM , & ! largeur au miroir amont
                          VAM , & ! Vitesse amont
                      Connect , & ! Table de connectivite du reseau
                            X , & ! Maillage
                         ZREF , & ! Cote de reference aux sections
                          IDT , & ! Profils amont des sections
                          XDT , & ! Position des sections/profils
                       Profil , & ! Profils geometriques
                       B1Plan , & ! Largeurs mineurs planimetrees
                   Impression , & ! Flag d'impression
                 UniteListing , & ! Unite logique du fichier listing
                       Erreur   & ! Erreur
                                )

! **********************************************************************
!   FONCTION :
!   --------
!
!   . CALCUL DES COEFFICIENTS A,B,C,D, DE L'EQUATION DISCRETISEE D'UNE
!     SINGULARITE :
!
!     A*DQ + B*DZAMONT + C*DZAVAL = D
!
!     Appel = 1 : LES COEFFICIENTS DE L'EQUATION
!                  DE LA SINGULARITE SONT CALCULES ET LE DEBIT
!     Appel = 2 : SEUL LE DEBIT THEORIQUE EST CALCULE
!_____________________________________________________________________________
!
!   FICHIERS ENTREE/SORTIE :     - UniteListing : IMPRESSION DES RESULTATS GLOBAUX
!
!   SOUS-PROGRAMMES APPELANTS :  - REZODT 
!   -------------------------
!
!   SOUS-PROGRAMMES APPELES :
!   -----------------------
!
! . SING2  : CALCUL DES COEFFICIENTS DE L'EQUATION DISCRETISEE D'UNE
!            SINGULARITE EN UTILISANT LA LOI EN REGIME DENOYE
!            (SINGULARITE DE TYPE 2)
!
! . SING3  : CALCUL DES COEFFICIENTS DE L'EQUATION DISCRETISEE D'UNE
!            SINGULARITE DEFINIE DE MANIERE STANDARD
!            (SINGULARITE DE TYPE 3)
!
! . SING10 : CALCUL DES COEFFICIENTS DE L'EQUATION DISCRETISEE D'UNE
!            VANNE OU D'UN ORIFICE 
!            (SINGULARITE DE TYPE 10)
!
! . INTERPOLATION_S : SOUS-PROGRAMME D'INTERPOLATION
!
!
!   COMMENTAIRES :
!   ------------
!
! . LES CALCULS DEPENDENT DU TYPE DE LA SINGULARITE
! . SI LA SINGULARITE EST EST DEFINIE AU MOYEN D'UNE LOI
!   Q = F ( ZAMONT , ZAVAL) ALORS :
!   A=-1.   B=DF/DZAMONT   C=DF/DZAVAL D=0.
! . SI CETTE SINGULARITE EST DEFINIE AU MOYEN D'UNE FAMILLE DE COURBES
!   (SINGULARITE DE TYPE 1) ALORS F EST ECRITE SOUS LA FORME :
!   Q= F (X=DELZAV,Y=DELZAM) = ALPHA*X + BETA*Y + GAMMA*X*Y + DELTA
!   OU DELZAV,DELZAM SONT LES VARIATIONS DE COTES A PARTIR D'UN ETAT
!   DE REFERENCE
! . IL FAUT NOTER QUE DANS LE CAS D'UN SEUIL ET SELON LE SENS DE
!   L'ECOULEMENT, LES SECTIONS  AMONT ET AVAL SONT INVERSEES :
!   IL FAUT EN TENIR COMPTE ENSUITE POUR L'UTILISATION DES
!   COEFFICIENTS B,C
!
!------------------------------------------------------------------------

   !============================ Declarations ==============================
   use M_PRECISION       ! Type DOUBLE
   use M_PARAMETRE_C     ! Parametres de calcul
   use M_MESSAGE_C       ! Liste des messages d'erreur
   use M_CONNECT_T       ! Definition du type CONNECT_T
   use M_ERREUR_T        ! Definition du type ERREUR_T
   use M_PROFIL_T        ! Definition du type PROFIL_T
   use M_SINGULARITE_T   ! Definition du type SINGULARITE_T
   use M_INTERPOLATION_S  ! Interpolation
   use M_TRAITER_ERREUR_I ! Traitement des erreurs
   use M_NUM_BIEF_S       ! Calcul du num d'un bief d'apres num section calcul
   use M_RHSBP_S          ! Sous programme
   use M_SING2_I          ! Interface de sous-programme
   use M_SING3_I          ! Interface de sous-programme
   use M_SING10_I         ! Interface de sous-programme

   implicit none

   !.. Arguments .. 
   real(DOUBLE)                     , intent(inout) :: ASING, BSING
   real(DOUBLE)                     , intent(inout) :: CSING, DSING
   real(DOUBLE)                     , intent(  out) :: QSING
   integer                          , intent(in   ) :: NumSeuil
   TYPE(SINGULARITE_T), dimension(:), intent(inout) :: Singularite
   integer                          , intent(in   ) :: Appel
   real(DOUBLE)                     , intent(in   ) :: ZAM, ZAV
   real(DOUBLE)                     , intent(inout) :: QAM, QAV
   real(DOUBLE)                     , intent(in   ) :: BAM, VAM
   logical                          , intent(in   ) :: Impression
   integer                          , intent(in   ) :: UniteListing
   type(CONNECT_T)                  , intent(in   ) :: Connect
   real(DOUBLE), dimension(:)       , intent(in   ) :: X
   real(DOUBLE), dimension(:)       , intent(in   ) :: ZREF
   integer       , dimension(:)     , intent(in   ) :: IDT
   real(DOUBLE)  , dimension(:)     , intent(in   ) :: XDT
   type(PROFIL_T), dimension(:)     , intent(in   ) :: Profil
   real(DOUBLE)  , dimension(:,:)   , intent(in   ) :: B1Plan
   type(ERREUR_T)                   , intent(inout) :: Erreur

   end subroutine KSING

   end interface

end module M_KSING_I
