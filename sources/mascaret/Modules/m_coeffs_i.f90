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

module M_COEFFS_I
!***********************************************************************
! PROGICIEL : MASCARET
!                       A. LEBOSSE     P. CHERUBINI    S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
interface

subroutine      COEFFS                  ( &
        ! Resultats
        FA, FB, FC              , & ! Coefficients de la relation de
        FD, FE, FF              , & ! transfert   i <- i-1
        GA, GB, GC              , & ! Coefficients de la relation de
        GD, GE, GF              , & ! transfert i-1 <- i
        ! Donnees non modifiees
        I1, I2                  , & ! Sections origine et fin du bief
        X, Z, Q                 , & ! Maillage, cote et debit
        P1, P2                  , & ! perimetres mouillees
        B1, B2 , BS             , & ! Largeurs au miroir
        S1, S2                  , & ! Sections mouillees
        R1, R2                  , & ! Rayons hydrauliques
        DPDZ1, DPDZ2            , & ! Gradients de P
        ST1, ST2                , & ! Coefficients de Strickler
        QINJEC                  , & ! Debits d'apport
        PCSing                  , & ! Pertes de charge singuliere
        SectionSing             , & ! Num Section precedant la sing
        ASING, BSING            , & ! Coeff de l'equ discretisee
        CSING, DSING            , & ! de la singularite
        ModeleLit               , & ! Modele du lit (Debord/Crugos)
        DT                      , & ! Pas de temps
        Erreur                    & ! Erreur
                                )
!
! **********************************************************************

!
!   FONCTION :
!   --------
!
!   CALCUL DES COEFFICIENTS DES RELATIONS DE TRANSFERT :
!
!   DQ(I)  = FA(I-1)*DQ(I-1)+FB(I-1)*DZ(I-1)+FC(I-1)
!   DZ(I)  = FD(I-1)*DQ(I-1)+FE(I-1)*DZ(I-1)+FF(I-1)
!   DQ(I-1)= GA(I-1)*DQ(I)  +GB(I-1)*DZ(I)  +GC(I-1)
!   DZ(I-1)= GD(I-1)*DQ(I)  +GE(I-1)*DZ(I)  +GF(I-1)
!
!   A PARTIR DES COEFFICIENTS DES EQUATIONS DE SAINT-VENANT
!   DISCRETISEES
!   TRAITEMENT DES LITS COMPOSES ( HYPOTHESES DEBORD)
!   TRAITEMENT DES SINGULARITES
!   PRISE EN COMPTE DU TERME DE QUANTITE DE MOUVEMENT (CQMV)
!   POUR LES APPORTS
!
! ----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ----------------------
!
!   SOUS PROGRAMMES APPELANTS :	HYDRAU
!   ---------------------------
!   SOUS PROGRAMMES APPELES :    ---
!   -------------------------
!
!   COMMENTAIRES :
!   ------------
!
! . LES CALCULS SONT EXPLICITES DANS LE RAPPORT HE-43/92.64
!
! . SONT CALCULES SUCESSIVEMENT :
!     - LES COEFFICIENTS AG,AH,AI,AJ,AK, DE LA DISCRETISATION DE
!       L'EQUATION DE CONTINUITE
!     - LES COEFFICIENTS AL,AM,AN,AO,AP, DE LA DISCRETISATION DE
!       L'EQUATION DYNAMIQUE
!     - LES COEFFICIENTS FA,FB,FC,FD,FE,FF DES RELATIONS DE TRANSFERT
!       AVAL-->AMONT DANS LE CAS GENERAL
!     - LES COEFFICIENTS GA,GB,GC,GD,GE,GF DES RELATIONS DE TRANSFERT
!       AMONT-->AVAL DANS LE CAS GENERAL
!     - LES COEFFICIENTS DES RELATIONS DE TRANSFERT AU PASSAGE DE LA
!       SINGULARITE SITUEE SUR LE BIEF TRAITE : ILS SONT OBTENUS A
!       PARTIR DES COEFFICIENTS A,B,C,D, DE SON EQUATION DISCRETISEE ,
!       ETABLIE DANS LE SOUS-PROGRAMME KSING
!
! .  LES PERTES DE CHARGE SINGULIERES PRISES EN COMPTE SONT :
!     -  RALENTISSEMENT : JS=0.3*(WBETAJ*V(J)-WBETAI*V(I)**2/2./G
!     -  OBSTACLES EN I : JS=PCSing(I)*WBETAJ*V(J)**2/2./G
!
!   DOCUMENTATION EXTERNE :
!   ---------------------
!
!   NOTE D'UTILISATION       : RAPPORT HE-43/92.19
!   NOTE DE PRINCIPE         : RAPPORT HE-43/92.64
!
!========================================================================

   ! ...Declarations...
   use M_PRECISION        ! Type DOUBLE
   use M_PARAMETRE_C      ! Parametres de calcul
   use M_MESSAGE_C        ! Liste des messages d'erreur
   use M_ERREUR_T         ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   implicit none

   !.. Arguments .. 
   real(DOUBLE), dimension(:), intent(  out) :: FA, FB, FC, FD, FE, FF
   real(DOUBLE), dimension(:), intent(  out) :: GA, GB, GC, GD, GE, GF
   real(DOUBLE), dimension(:), intent(in   ) :: X, Z, Q
   real(DOUBLE), dimension(:), intent(in   ) :: P1, P2, B1, B2, S1, S2, R1, R2
   real(DOUBLE), dimension(:), intent(in   ) :: DPDZ1, DPDZ2
   real(DOUBLE), dimension(:), intent(in   ) :: ST1, ST2
   real(DOUBLE), dimension(:), intent(in   ) :: BS, QINJEC
   real(DOUBLE), dimension(:), intent(in   ) :: PCSing
   real(DOUBLE)              , intent(in   ) :: ASING, BSING, CSING, DSING
   real(DOUBLE)              , intent(in   ) :: DT
   integer                   , intent(in   ) :: ModeleLit
   integer                   , intent(in   ) :: I1, I2
   integer                   , intent(in   ) :: SectionSing
   type(ERREUR_T)            , intent(inout) :: Erreur

   end subroutine COEFFS

   end interface

end module M_COEFFS_I
