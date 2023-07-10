!== Copyright (C) 2000-2017 EDF-CEREMA-ARTELIA ==
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
module M_KLIAISON_I
! *********************************************************************
! PROGICIEL : MASCARET        S. DELMAS    C. COULET
!                             
! VERSION : V8P4R0         EDF-CEREMA-ARTELIA
! *********************************************************************
    interface



    subroutine  KLIAISON                    ( &
                ALiai,BLiai,CLiai,DLiai     , & ! Coefficients du seuil
                QLiai                       , & ! Debit au seuil
                Liaison                     , & ! Singularites
                Appel                       , & ! Flag
                ZAM                         , & ! Cote amont
                ZAV                         , & ! Cote aval
                ZfAM                        , & ! Cote Fond amont
                ZfAV                        , & ! Cote Fond aval
                Connect                     , & ! Table de connectivite du reseau
                Impression                  , & ! Flag d'impression
                UniteListing                , & ! Unite logique du fichier listing
                Erreur                        & ! Erreur
                                            )
    !_____________________________________________________________________________
    !   FONCTION :
    !   --------
    !
    !   . CALCUL DES COEFFICIENTS A,B,C,D, DE L'EQUATION DISCRETISEE D'UNE
    !     LIAISON :
    !
    !     A*DQ_liaison + B*DZAMONT + C*DZAVAL = D
    !
    !     Appel = 1 : LES COEFFICIENTS DE L'EQUATION 
    !                 DE LA LIAISON ET LE DEBIT SONT CALCULES 
    !     Appel = 2 : SEUL LE DEBIT THEORIQUE EST CALCULE
    !_____________________________________________________________________________
    !
    !   FICHIERS ENTREE/SORTIE :    - UniteListing : IMPRESSION DES RESULTATS GLOBAUX
    !   SOUS-PROGRAMMES APPELANT :  - REZODT
    !   ------------------------
    !   SOUS-PROGRAMMES APPELES :
    !   -----------------------
    ! . INTERPOLATION_S : SOUS-PROGRAMME D'INTERPOLATION
    !
    !   COMMENTAIRES :
    !   ------------
    ! . LES CALCULS DEPENDENT DU TYPE DE LA LIAISON
    ! . SI LA LIAISON EST EST DEFINIE AU MOYEN D'UNE LOI
    !   Q = F ( ZAMONT , ZAVAL) ALORS :
    !   A=-1.   B=DF/DZAMONT   C=DF/DZAVAL D=0.
    ! . SI CETTE LIAISON EST DEFINIE AU MOYEN D'UNE FAMILLE DE COURBES
    !   (LIAISON DE TYPE 1) ALORS F EST ECRITE SOUS LA FORME :
    !   Q= F (X=DELZAV,Y=DELZAM) = ALPHA*X + BETA*Y + GAMMA*X*Y + DELTA
    !   OU DELZAV,DELZAM SONT LES VARIATIONS DE COTES A PARTIR D'UN ETAT
    !   DE REFERENCE
    !
    !------------------------------------------------------------------------

       !============================ Declarations ==============================
   use M_PRECISION        ! Type DOUBLE
   use M_PARAMETRE_C      ! Parametres de calcul
   use M_MESSAGE_C        ! Liste des messages d'erreur
   use M_CONNECT_T        ! Definition du type CONNECT_T
   use M_LIAISON_T        ! Definition du type SINGULARITE_T
   use M_ERREUR_T         ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs   
   use M_TRAITER_ERREUR_CASIER_I ! traitement des erreurs
   use M_MESSAGE_CASIER_C        ! messages d erreur propres a CASIER
   use M_NUM_BIEF_S       ! Calcul du num d'un bief d'apres num section calcul
     
   use M_INTERPOLATION_S  ! Interpolation
   use M_RHSBP_S          ! Sous programme
   
   use M_LIAISON_SEUIL_I    ! Interface de sous-programme
   use M_LIAISON_CHENAL_I   ! Interface de sous-programme
   use M_LIAISON_SIPHON_I   ! Interface de sous-programme
   use M_LIAISON_ORIFICE_I  ! Interface de sous-programme
       

       implicit none

       !.. Arguments ..
       real(DOUBLE)                     , intent(  out) :: Aliai, Bliai
       real(DOUBLE)                     , intent(  out) :: Cliai, Dliai
       real(DOUBLE)                     , intent(  out) :: Qliai
       TYPE(LIAISON_T)                  , intent(inout) :: Liaison
       integer                          , intent(in   ) :: Appel
       real(DOUBLE)                     , intent(in   ) :: ZAM, ZAV, ZfAM, ZfAV
       logical                          , intent(in   ) :: Impression
       integer                          , intent(in   ) :: UniteListing
       type(CONNECT_T)                  , intent(in   ) :: Connect
       type(ERREUR_T)                   , intent(inout) :: Erreur
       
       
    end subroutine KLIAISON

    end interface

end module M_KLIAISON_I