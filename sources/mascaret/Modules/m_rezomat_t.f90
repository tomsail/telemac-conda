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

module M_REZOMAT_T
!***********************************************************************
! PROGICIEL : MASCARET      F. ZAOUI
!
! VERSION : V8P4R0             EDF-CEREMA
!***********************************************************************

!=========================== Declarations ==============================

use M_PRECISION

TYPE REZOMAT_T

  sequence

  integer                          :: SOLV          ! Choix du solveur lineaire : LAPACK-DGBSV (1) ou Y12M (2)
  integer                          :: N             ! Ordre de la matrice
  integer                          :: NNZ           ! Nombre d'elements non nuls de la matrice
  integer                          :: NN            ! dimension tableau de travail SNR de Y12M
  integer                          :: NN1           ! dimension tableau de travail RNR de Y12M
  integer                          :: IHA           ! dimension tableau de travail HA de Y12M
  integer                          :: KL            ! Largeur de demi-bande inferieure LAPACK
  integer                          :: KU            ! Largeur de demi-bande superieure LAPACK
  integer                          :: LDAB          ! Calcul d'un espace de travail LAPACK
  integer                          :: IFAIL         ! Diagnostic d'erreur
  integer , dimension(:) , pointer :: ipiv          => null()  ! Permutation numerique LAPACK
  integer , dimension(:) , pointer :: IFLAG         => null()  ! pilotage de Y12M
  integer , dimension(:) , pointer :: rowA          => null()  ! Indices de ligne de chaque element non nul
  integer , dimension(:) , pointer :: colA          => null()  ! Indices de colonne de chaque element non nul
  integer , dimension(:) , pointer :: snr           => null()  ! Indices de ligne de chaque element non nul
  integer , dimension(:) , pointer :: rnr           => null()  ! Indices de colonne de chaque element non nul
  integer , dimension(:,:),pointer :: ha            => null()  ! Tableau de travail  Y12M
  integer , dimension(:) , pointer :: noVarDQ       => null()  ! Table de correspondance avec le vecteur solution DQ
  integer , dimension(:) , pointer :: noVarDZ       => null()  ! Table de correspondance avec le vecteur solution DZ
  integer , dimension(:) , pointer :: noVarDQl      => null()  ! Table de correspondance avec le vecteur solution DQl dans les liaisons
  integer , dimension(:) , pointer :: noVarDZc      => null()  ! Table de correspondance avec le vecteur solution DZc dans les casiers
  integer , dimension(:) , pointer :: typSec        => null()  ! Typage des sections de calcul : 0 = quelconque , -1 = debit impose , -2 = cote imposee, >0 = confluent
  integer , dimension(:) , pointer :: headConflu    => null()  ! Pour chaque confluent, indique la premiere section de la liste avec signe + ou - suivant origine ou extremite
  integer , dimension(:) , pointer :: nextSecConflu => null() ! Section suivante de la liste avec signe + ou - suivant origine ou extremite
  integer , dimension(:) , pointer :: SecSin        => null() ! Pour chaque section, donne le numero de la singularite presente immediatement apres
  integer , dimension(:) , pointer :: SecLiai       => null() ! Pour chaque section donne le nombre des liaisons
  integer , dimension(:) , pointer :: LiaiSec       => null() ! Pour chaque liaison donne la section Amont renvoit 0 si la liaison est de type C-C
  real(DOUBLE) , dimension(:) , pointer :: AFLAG         => null() ! parametres de calcul de Y12M
  real(DOUBLE) , dimension(:) , pointer :: valA          => null() ! Valeurs des elements non nuls de la matrice
  real(DOUBLE) , dimension(:) , pointer :: b             => null() ! Second membre du systeme d'equations lineaires
  real(DOUBLE) , dimension(:) , pointer :: pivot         => null() ! Valeurs des pivots
  real(DOUBLE) , dimension(:,:) , pointer :: AB          => null() ! Matrice bande LAPACK

END TYPE REZOMAT_T

contains

! Retourne les noms des champs du type ainsi qu'une description
subroutine GET_TAB_VAR_REZOMAT(i, tabNomVar, tabDescriptionVar)
  integer , intent(inout)                                  :: i                 ! indiceTableaux
  character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele ou de l'etat
  character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele ou de l'etat

  tabNomVar(i)         ="State.Rezomat.SOLV"
  tabDescriptionVar(i) ="Linear solver (REZO - subcritical kernel)"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.N"
  tabDescriptionVar(i) ="Matrix rank (REZO - subcritical kernel)"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.NNZ"
  tabDescriptionVar(i) ="Number of non-zero elements (REZO - subcritical kernel)"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.NN"
  tabDescriptionVar(i) ="Work aray dimension SNR (Y12M solver)"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.NN1"
  tabDescriptionVar(i) ="Work aray dimension RNR (Y12M solver)"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.IHA"
  tabDescriptionVar(i) ="DWork aray dimension HA (Y12M solver)"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.KL"
  tabDescriptionVar(i) ="Lower bandwidth (LAPACK-DGBSV solver)"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.KU"
  tabDescriptionVar(i) ="Upper bandwidth (LAPACK-DGBSV solver)"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.LDAB"
  tabDescriptionVar(i) ="Workspace (LAPACK-DGBSV solver)"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.IFAIL"
  tabDescriptionVar(i) ="Error indicator"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.ipiv"
  tabDescriptionVar(i) ="Permutation matrix (LAPACK-DGBSV solver)"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.IFLAG"
  tabDescriptionVar(i) ="Numerical options and statistics (Y12M solver)"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.RowA"
  tabDescriptionVar(i) ="Row number of a non-zero element"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.ColA"
  tabDescriptionVar(i) ="Column number of a  non-zero element"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.snr"
  tabDescriptionVar(i) ="Work array SNR"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.rnr"
  tabDescriptionVar(i) ="Work array RNR"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.ha"
  tabDescriptionVar(i) ="Work array HA"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.noVarDQ"
  tabDescriptionVar(i) ="Pointers to DQ solutions (discharge)"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.noVarDZ"
  tabDescriptionVar(i) ="Pointers to DZ solutions (level)"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.noVarDQl"
  tabDescriptionVar(i) ="Pointers to DQl solutions (link)"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.noVarDZc"
  tabDescriptionVar(i) ="Pointers to DZc solutions (Storage area)"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.KdNode"
  tabDescriptionVar(i) ="Kind of nodes : 0->undefined, -1 ->discharge, -2 ->level, >0 ->junction"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.headJunction"
  tabDescriptionVar(i) ="For each junction, first element (node number)"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.nxtNdJunction"
  tabDescriptionVar(i) ="Next element in the list of nodes of a junction"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.WeirNd"
  tabDescriptionVar(i) ="Positions of weirs --- nodes"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.LinkNd"
  tabDescriptionVar(i) ="Positions of links --- nodes"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.NdLink"
  tabDescriptionVar(i) ="For each link, gives the upstream node or 0 if the link is not connected to the river"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.AFLAG"
  tabDescriptionVar(i) ="Numerical parameters (Y12M solver)"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.valA"
  tabDescriptionVar(i) ="Values of the non-zero elements"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.b"
  tabDescriptionVar(i) ="RHS vector of the linear system : A.x = b"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.pivot"
  tabDescriptionVar(i) ="Pivot value of the Gauss elimination"
  i=i+1
  tabNomVar(i)         ="State.Rezomat.AB"
  tabDescriptionVar(i) ="Band matrix (LAPACK-DGBSV solver)"
  i=i+1
  return

end subroutine GET_TAB_VAR_REZOMAT

! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
function GET_TYPE_VAR_REZOMAT(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
  implicit none

  integer                          :: GET_TYPE_VAR_REZOMAT ! different de 0 si erreur
  character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation pointe)
  character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
  character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
  logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
  integer          , intent(out)   :: dimVar                   ! dimension (c'est a dire le nombre d'indexe de 0 a 3)
  character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

  GET_TYPE_VAR_REZOMAT = 0
  TypeVar               = ""
  Categorie             = "STATE"
  Modifiable            = .FALSE.
  dimVar                = 0
  MessageErreur         = ""

  if ( index(NomVar, 'State.Rezomat.SOLV') > 0) then
     TypeVar = 'INT'
     dimVar                = 0
  else if ( index(NomVar, 'State.Rezomat.NNZ') > 0) then
     TypeVar = 'INT'
     dimVar                = 0
  else if ( index(NomVar, 'State.Rezomat.NN1') > 0) then
     TypeVar = 'INT'
     dimVar                = 0
  else if ( index(NomVar, 'State.Rezomat.NN') > 0) then
     TypeVar = 'INT'
     dimVar                = 0
  else if ( index(NomVar, 'State.Rezomat.N') > 0) then
     TypeVar = 'INT'
     dimVar                = 0
  else if ( index(NomVar, 'State.Rezomat.IHA') > 0) then
     TypeVar = 'INT'
     dimVar                = 0
  else if ( index(NomVar, 'State.Rezomat.KL') > 0) then
     TypeVar = 'INT'
     dimVar                = 0
  else if ( index(NomVar, 'State.Rezomat.KU') > 0) then
     TypeVar = 'INT'
     dimVar                = 0
  else if ( index(NomVar, 'State.Rezomat.LDAB') > 0) then
     TypeVar = 'INT'
     dimVar                = 0
  else if ( index(NomVar, 'State.Rezomat.IFAIL') > 0) then
     TypeVar = 'INT'
     dimVar                = 0
  else  if ( index(NomVar, 'State.Rezomat.ipiv') > 0) then
     TypeVar = 'TABINT'
     dimVar                = 1
  else  if ( index(NomVar, 'State.Rezomat.IFLAG') > 0) then
     TypeVar = 'TABINT'
     dimVar                = 1
  else  if ( index(NomVar, 'State.Rezomat.RowA') > 0) then
     TypeVar = 'TABINT'
     dimVar                = 1
  else  if ( index(NomVar, 'State.Rezomat.ColA') > 0) then
     TypeVar = 'TABINT'
     dimVar                = 1
  else  if ( index(NomVar, 'State.Rezomat.snr') > 0) then
     TypeVar = 'TABINT'
     dimVar                = 1
  else  if ( index(NomVar, 'State.Rezomat.rnr') > 0) then
     TypeVar = 'TABINT'
     dimVar                = 1
  else  if ( index(NomVar, 'State.Rezomat.ha') > 0) then
     TypeVar = 'TABINT'
     dimVar                = 2
  else  if ( index(NomVar, 'State.Rezomat.noVarDQ') > 0) then
     TypeVar = 'TABINT'
     dimVar                = 1
  else  if ( index(NomVar, 'State.Rezomat.noVarDZ') > 0) then
     TypeVar = 'TABINT'
     dimVar                = 1
  else  if ( index(NomVar, 'State.Rezomat.noVarDQl') > 0) then
     TypeVar = 'TABINT'
     dimVar                = 1
  else  if ( index(NomVar, 'State.Rezomat.noVarDZc') > 0) then
     TypeVar = 'TABINT'
     dimVar                = 1
  else  if ( index(NomVar, 'State.Rezomat.KdNode') > 0) then
     TypeVar = 'TABINT'
     dimVar                = 1
  else  if ( index(NomVar, 'State.Rezomat.headJunction') > 0) then
     TypeVar = 'TABINT'
     dimVar                = 1
  else  if ( index(NomVar, 'State.Rezomat.nxtNdJunction') > 0) then
     TypeVar = 'TABINT'
     dimVar                = 1
  else  if ( index(NomVar, 'State.Rezomat.WeirNd') > 0) then
     TypeVar = 'TABINT'
     dimVar                = 1
  else  if ( index(NomVar, 'State.Rezomat.LinkNd') > 0) then
     TypeVar = 'TABINT'
     dimVar                = 1
  else  if ( index(NomVar, 'State.Rezomat.NdLink') > 0) then
     TypeVar = 'TABINT'
     dimVar                = 1
  else  if ( index(NomVar, 'State.Rezomat.AFLAG') > 0) then
     TypeVar = 'TABDOUBLE'
     dimVar                = 1
  else  if ( index(NomVar, 'State.Rezomat.valA') > 0) then
     TypeVar = 'TABDOUBLE'
     dimVar                = 1
  else  if ( index(NomVar, 'State.Rezomat.b') > 0) then
     TypeVar = 'TABDOUBLE'
     dimVar                = 1
  else  if ( index(NomVar, 'State.Rezomat.pivot') > 0) then
     TypeVar = 'TABDOUBLE'
     dimVar                = 1
  else  if ( index(NomVar, 'State.Rezomat.AB') > 0) then
     TypeVar = 'TABDOUBLE'
     dimVar                = 2
  else
    GET_TYPE_VAR_REZOMAT = 1
    TypeVar = "?"
    Categorie             = "STATE"
    Modifiable            = .FALSE.
    dimVar                = -1
    MessageErreur         = "GET_TYPE_VAR_REZOMAT - Unknown variable name"
  end if

end function GET_TYPE_VAR_REZOMAT

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_REZOMAT(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_REZOMAT         ! different de 0 si erreur
      type(REZOMAT_T),        intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur

      GET_TAILLE_VAR_REZOMAT = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( index(NomVar, 'State.Rezomat.SOLV') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.NNZ') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.NN1') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.NN') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.N') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.IHA') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.KL') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.KU') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.LDAB') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
     else if ( index(NomVar, 'State.Rezomat.IFAIL') > 0) then
         taille1 = 0
         taille2 = 0
         taille3 = 0
     else if ( index(NomVar, 'State.Rezomat.ipiv') > 0) then
         if (ASSOCIATED(Instance%ipiv)) then
            taille1 = size(Instance%ipiv)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
     else if ( index(NomVar, 'State.Rezomat.IFLAG') > 0) then
         if (ASSOCIATED(Instance%IFLAG)) then
            taille1 = size(Instance%IFLAG)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.RowA') > 0) then
         if (ASSOCIATED(Instance%rowA)) then
            taille1 = size(Instance%rowA)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.ColA') > 0) then
         if (ASSOCIATED(Instance%colA)) then
            taille1 = size(Instance%colA)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.snr') > 0) then
         if (ASSOCIATED(Instance%snr)) then
            taille1 = size(Instance%snr)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.rnr') > 0) then
         if (ASSOCIATED(Instance%rnr)) then
            taille1 = size(Instance%rnr)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.ha') > 0) then
         if (ASSOCIATED(Instance%ha)) then
            taille1 = size(Instance%ha, 1)
            taille2 = size(Instance%ha, 2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.noVarDQ') > 0) then
         if (ASSOCIATED(Instance%noVarDQ)) then
            taille1 = size(Instance%noVarDQ)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.noVarDZ') > 0) then
         if (ASSOCIATED(Instance%noVarDZ)) then
            taille1 = size(Instance%noVarDZ)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.noVarDQl') > 0) then
         if (ASSOCIATED(Instance%noVarDQl)) then
            taille1 = size(Instance%noVarDQl)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.noVarDZc') > 0) then
         if (ASSOCIATED(Instance%noVarDZc)) then
            taille1 = size(Instance%noVarDZc)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.KdNode') > 0) then
         if (ASSOCIATED(Instance%typSec)) then
            taille1 = size(Instance%typSec)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.headJunction') > 0) then
         if (ASSOCIATED(Instance%headConflu)) then
            taille1 = size(Instance%headConflu)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.nxtNdJunction') > 0) then
         if (ASSOCIATED(Instance%nextSecConflu)) then
            taille1 = size(Instance%nextSecConflu)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.WeirNd') > 0) then
         if (ASSOCIATED(Instance%SecSin)) then
            taille1 = size(Instance%SecSin)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.LinkNd') > 0) then
         if (ASSOCIATED(Instance%SecLiai)) then
            taille1 = size(Instance%SecLiai)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.NdLink') > 0) then
         if (ASSOCIATED(Instance%LiaiSec)) then
            taille1 = size(Instance%LiaiSec)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.AFLAG') > 0) then
         if (ASSOCIATED(Instance%AFLAG)) then
            taille1 = size(Instance%AFLAG)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.valA') > 0) then
         if (ASSOCIATED(Instance%valA)) then
            taille1 = size(Instance%valA)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.b') > 0) then
         if (ASSOCIATED(Instance%b)) then
            taille1 = size(Instance%b)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.pivot') > 0) then
         if (ASSOCIATED(Instance%pivot)) then
            taille1 = size(Instance%pivot)
         else
            taille1 = 0
         endif
         taille2 = 0
         taille3 = 0
      else if ( index(NomVar, 'State.Rezomat.AB') > 0) then
         if (ASSOCIATED(Instance%AB)) then
            taille1 = size(Instance%AB,1)
            taille2 = size(Instance%AB,2)
         else
            taille1 = 0
            taille2 = 0
         endif
         taille3 = 0
      else
         GET_TAILLE_VAR_REZOMAT = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_REZOMAT - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_REZOMAT

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_REZOMAT(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_REZOMAT         ! different de 0 si erreur
      type(REZOMAT_T),        intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur

      integer t1, t2, t3, err

      SET_TAILLE_VAR_REZOMAT = 0
      t1                     = -1
      t2                     = -1
      t3                     = -1
      MessageErreur          = ""
      err                    = 0

      !----------------------------------------------------------
      ! Modification de la taille des pointers de types primitifs
      !----------------------------------------------------------
      if ( index(NomVar, 'State.Rezomat.ipiv') > 0) then
        if (ASSOCIATED(Instance%ipiv)) then
           t1 = size(Instance%ipiv)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%ipiv, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_REZOMAT = err
                 MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to deallocate REZOMAT_T.ipiv'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%ipiv) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%ipiv(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_REZOMAT = err
              MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to allocate REZOMAT_T.ipiv'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Rezomat.IFLAG') > 0) then
        if (ASSOCIATED(Instance%IFLAG)) then
           t1 = size(Instance%IFLAG)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%IFLAG, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_REZOMAT = err
                 MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to deallocate REZOMAT_T.IFLAG'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%IFLAG) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%IFLAG(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_REZOMAT = err
              MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to allocate REZOMAT_T.IFLAG'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Rezomat.RowA') > 0) then
        if (ASSOCIATED(Instance%rowA)) then
           t1 = size(Instance%rowA)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%rowA, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_REZOMAT = err
                 MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to deallocate REZOMAT_T.rowA'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%rowA) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%rowA(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_REZOMAT = err
              MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to allocate REZOMAT_T.rowA'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Rezomat.ColA') > 0) then
        if (ASSOCIATED(Instance%colA)) then
           t1 = size(Instance%colA)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%colA, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_REZOMAT = err
                 MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to deallocate REZOMAT_T.colA'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%colA) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%colA(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_REZOMAT = err
              MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to allocate REZOMAT_T.colA'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Rezomat.snr') > 0) then
        if (ASSOCIATED(Instance%snr)) then
           t1 = size(Instance%snr)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%snr, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_REZOMAT = err
                 MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to deallocate REZOMAT_T.snr'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%snr) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%snr(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_REZOMAT = err
              MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to allocate REZOMAT_T.snr'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Rezomat.rnr') > 0) then
        if (ASSOCIATED(Instance%rnr)) then
           t1 = size(Instance%rnr)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%rnr, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_REZOMAT = err
                 MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to deallocate REZOMAT_T.rnr'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%rnr) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%rnr(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_REZOMAT = err
              MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to allocate REZOMAT_T.rnr'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Rezomat.ha') > 0) then
        if (ASSOCIATED(Instance%ha)) then
           t1 = size(Instance%ha, 1)
           t2 = size(Instance%ha, 2)
           if ( (t1 /= NewT1).OR.(t2 /= NewT2) ) then
              DEALLOCATE(Instance%ha, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_REZOMAT = err
                 MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to deallocate REZOMAT_T.ha'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%ha).OR.(t1/=NewT1).OR.(t2/=NewT2)) then
           ALLOCATE(Instance%ha(NewT1, NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_REZOMAT = err
              MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to allocate REZOMAT_T.ha'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Rezomat.noVarDQ') > 0) then
        if (ASSOCIATED(Instance%noVarDQ)) then
           t1 = size(Instance%noVarDQ)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%noVarDQ, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_REZOMAT = err
                 MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to deallocate REZOMAT_T.noVarDQ'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%noVarDQ) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%noVarDQ(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_REZOMAT = err
              MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to allocate REZOMAT_T.noVarDQ'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Rezomat.noVarDZ') > 0) then
        if (ASSOCIATED(Instance%noVarDZ)) then
           t1 = size(Instance%noVarDZ)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%noVarDZ, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_REZOMAT = err
                 MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to deallocate REZOMAT_T.noVarDZ'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%noVarDZ) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%noVarDZ(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_REZOMAT = err
              MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to allocate REZOMAT_T.noVarDZ'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Rezomat.noVarDQl') > 0) then
        if (ASSOCIATED(Instance%noVarDQl)) then
           t1 = size(Instance%noVarDQl)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%noVarDQl, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_REZOMAT = err
                 MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to deallocate REZOMAT_T.noVarDQl'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%noVarDQl) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%noVarDQl(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_REZOMAT = err
              MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to allocate REZOMAT_T.noVarDQl'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Rezomat.noVarDZc') > 0) then
        if (ASSOCIATED(Instance%noVarDZc)) then
           t1 = size(Instance%noVarDZc)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%noVarDZc, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_REZOMAT = err
                 MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to deallocate REZOMAT_T.noVarDZc'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%noVarDZc) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%noVarDZc(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_REZOMAT = err
              MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to allocate REZOMAT_T.noVarDZc'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Rezomat.KdNode') > 0) then
        if (ASSOCIATED(Instance%typSec)) then
           t1 = size(Instance%typSec)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%typSec, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_REZOMAT = err
                 MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to deallocate REZOMAT_T.typSec'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%typSec) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%typSec(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_REZOMAT = err
              MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to allocate REZOMAT_T.typSec'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Rezomat.headJunction') > 0) then
        if (ASSOCIATED(Instance%headConflu)) then
           t1 = size(Instance%headConflu)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%headConflu, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_REZOMAT = err
                 MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to deallocate REZOMAT_T.headConflu'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%headConflu) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%headConflu(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_REZOMAT = err
              MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to allocate REZOMAT_T.headConflu'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Rezomat.nxtNdJunction') > 0) then
        if (ASSOCIATED(Instance%nextSecConflu)) then
           t1 = size(Instance%nextSecConflu)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%nextSecConflu, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_REZOMAT = err
                 MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to deallocate REZOMAT_T.nextSecConflu'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%nextSecConflu) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%nextSecConflu(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_REZOMAT = err
              MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to allocate REZOMAT_T.nextSecConflu'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Rezomat.WeirNd') > 0) then
        if (ASSOCIATED(Instance%SecSin)) then
           t1 = size(Instance%SecSin)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%SecSin, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_REZOMAT = err
                 MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to deallocate REZOMAT_T.SecSin'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%SecSin) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%SecSin(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_REZOMAT = err
              MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to allocate REZOMAT_T.SecSin'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Rezomat.LinkNd') > 0) then
        if (ASSOCIATED(Instance%SecLiai)) then
           t1 = size(Instance%SecLiai)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%SecLiai, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_REZOMAT = err
                 MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to deallocate REZOMAT_T.SecLiai'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%SecLiai) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%SecLiai(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_REZOMAT = err
              MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to allocate REZOMAT_T.SecLiai'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Rezomat.NdLink') > 0) then
        if (ASSOCIATED(Instance%LiaiSec)) then
           t1 = size(Instance%LiaiSec)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%LiaiSec, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_REZOMAT = err
                 MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to deallocate REZOMAT_T.LiaiSec'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%LiaiSec) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%LiaiSec(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_REZOMAT = err
              MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to allocate REZOMAT_T.LiaiSec'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Rezomat.AFLAG') > 0) then
        if (ASSOCIATED(Instance%AFLAG)) then
           t1 = size(Instance%AFLAG)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%AFLAG, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_REZOMAT = err
                 MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to deallocate REZOMAT_T.AFLAG'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%AFLAG) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%AFLAG(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_REZOMAT = err
              MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to allocate REZOMAT_T.AFLAG'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Rezomat.valA') > 0) then
        if (ASSOCIATED(Instance%valA)) then
           t1 = size(Instance%valA)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%valA, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_REZOMAT = err
                 MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to deallocate REZOMAT_T.valA'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%valA) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%valA(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_REZOMAT = err
              MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to allocate REZOMAT_T.valA'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Rezomat.b') > 0) then
        if (ASSOCIATED(Instance%b)) then
           t1 = size(Instance%b)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%b, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_REZOMAT = err
                 MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to deallocate REZOMAT_T.b'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%b) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%b(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_REZOMAT = err
              MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to allocate REZOMAT_T.b'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Rezomat.pivot') > 0) then
        if (ASSOCIATED(Instance%pivot)) then
           t1 = size(Instance%pivot)
           if (t1 /= NewT1) then
              DEALLOCATE(Instance%pivot, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_REZOMAT = err
                 MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to deallocate REZOMAT_T.pivot'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%pivot) .OR. (t1 /= NewT1)) then
           ALLOCATE(Instance%pivot(NewT1), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_REZOMAT = err
              MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to allocate REZOMAT_T.pivot'
              return
           endif
        endif
      else if ( index(NomVar, 'State.Rezomat.AB') > 0) then
        if (ASSOCIATED(Instance%AB)) then
           t1 = size(Instance%AB,1)
           t2 = size(Instance%AB,2)
           if ((t1 /= NewT1).OR.(t2 /= NewT2)) then
              DEALLOCATE(Instance%AB, STAT=err)
              if (err /= 0) then
                 SET_TAILLE_VAR_REZOMAT = err
                 MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to deallocate REZOMAT_T.AB'
                 return
              endif
           endif
        endif
        if (.not.ASSOCIATED(Instance%AB).OR.(t1 /= NewT1).OR.(t2 /= NewT2)) then
           ALLOCATE(Instance%AB(NewT1,NewT2), STAT=err)
           if (err /= 0) then
              SET_TAILLE_VAR_REZOMAT = err
              MessageErreur = 'SET_TAILLE_VAR_REZOMAT : Unable to allocate REZOMAT_T.AB'
              return
           endif
        endif
      !--------------------------------------------------------------------
      ! Fin de la modification de la taille des pointers de types primitifs
      !--------------------------------------------------------------------

      else
         SET_TAILLE_VAR_REZOMAT = 1
         MessageErreur         = "SET_TAILLE_VAR_REZOMAT - Unknown variable name"
      end if
   end function SET_TAILLE_VAR_REZOMAT

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_DOUBLE_REZOMAT(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_REZOMAT         ! different de 0 si erreur
      type(REZOMAT_T),        intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_DOUBLE_REZOMAT = 0
      valeur                = -9999999.9999
      MessageErreur          = ""

      if ( index(NomVar, 'State.Rezomat.AFLAG') > 0) then
         valeur = Instance%AFLAG(index1)
      else if ( index(NomVar, 'State.Rezomat.valA') > 0) then
         valeur = Instance%valA(index1)
      else if ( index(NomVar, 'State.Rezomat.b') > 0) then
         valeur = Instance%b(index1)
      else if ( index(NomVar, 'State.Rezomat.pivot') > 0) then
         valeur = Instance%pivot(index1)
      else if ( index(NomVar, 'State.Rezomat.AB') > 0) then
         valeur = Instance%AB(index1,index2)
      else
         GET_DOUBLE_REZOMAT = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_REZOMAT - Unknown variable name"
      end if
   end function GET_DOUBLE_REZOMAT


   function GET_INT_REZOMAT(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_INT_REZOMAT            ! different de 0 si erreur
      type(REZOMAT_T),        intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(out):: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_INT_REZOMAT = 0
      valeur                = -9999
      MessageErreur          = ""

      if ( index(NomVar, 'State.Rezomat.SOLV') > 0) then
         valeur = Instance%SOLV
      else if ( index(NomVar, 'State.Rezomat.NNZ') > 0) then
         valeur = Instance%NNZ
      else if ( index(NomVar, 'State.Rezomat.NN1') > 0) then
         valeur = Instance%NN1
      else if ( index(NomVar, 'State.Rezomat.NN') > 0) then
         valeur = Instance%NN
      else if ( index(NomVar, 'State.Rezomat.N') > 0) then
         valeur = Instance%N
      else if ( index(NomVar, 'State.Rezomat.IHA') > 0) then
         valeur = Instance%IHA
      else if ( index(NomVar, 'State.Rezomat.KL') > 0) then
         valeur = Instance%KL
      else if ( index(NomVar, 'State.Rezomat.KU') > 0) then
         valeur = Instance%KU
      else if ( index(NomVar, 'State.Rezomat.LDAB') > 0) then
         valeur = Instance%LDAB
      else if ( index(NomVar, 'State.Rezomat.IFAIL') > 0) then
         valeur = Instance%IFAIL
      else if ( index(NomVar, 'State.Rezomat.ipiv') > 0) then
         valeur = Instance%ipiv(index1)
      else if ( index(NomVar, 'State.Rezomat.IFLAG') > 0) then
         valeur = Instance%IFLAG(index1)
      else if ( index(NomVar, 'State.Rezomat.RowA') > 0) then
         valeur = Instance%rowA(index1)
      else if ( index(NomVar, 'State.Rezomat.ColA') > 0) then
         valeur = Instance%colA(index1)
      else if ( index(NomVar, 'State.Rezomat.snr') > 0) then
         valeur = Instance%snr(index1)
      else if ( index(NomVar, 'State.Rezomat.rnr') > 0) then
         valeur = Instance%rnr(index1)
      else if ( index(NomVar, 'State.Rezomat.ha') > 0) then
         valeur = Instance%ha(index1, index2)
      else if ( index(NomVar, 'State.Rezomat.noVarDQ') > 0) then
         valeur = Instance%noVarDQ(index1)
      else if ( index(NomVar, 'State.Rezomat.noVarDZ') > 0) then
         valeur = Instance%noVarDZ(index1)
      else if ( index(NomVar, 'State.Rezomat.noVarDQl') > 0) then
         valeur = Instance%noVarDQl(index1)
      else if ( index(NomVar, 'State.Rezomat.noVarDZc') > 0) then
         valeur = Instance%noVarDZc(index1)
      else if ( index(NomVar, 'State.Rezomat.KdNode') > 0) then
         valeur = Instance%typSec(index1)
      else if ( index(NomVar, 'State.Rezomat.headJunction') > 0) then
         valeur = Instance%headConflu(index1)
      else if ( index(NomVar, 'State.Rezomat.nxtNdJunction') > 0) then
         valeur = Instance%nextSecConflu(index1)
      else if ( index(NomVar, 'State.Rezomat.WeirNd') > 0) then
         valeur = Instance%SecSin(index1)
      else if ( index(NomVar, 'State.Rezomat.LinkNd') > 0) then
         valeur = Instance%SecLiai(index1)
      else if ( index(NomVar, 'State.Rezomat.NdLink') > 0) then
         valeur = Instance%LiaiSec(index1)
      else
         GET_INT_REZOMAT = 1
         valeur                = -9999
         MessageErreur         = "GET_INT_REZOMAT - Unknown variable name"
      end if
   end function GET_INT_REZOMAT



! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_DOUBLE_REZOMAT(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_REZOMAT         ! different de 0 si erreur
      type(REZOMAT_T),        intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_DOUBLE_REZOMAT = 0
      MessageErreur          = ""

      if ( index(NomVar, 'State.Rezomat.AFLAG') > 0) then
         Instance%AFLAG(index1) = valeur
      else if ( index(NomVar, 'State.Rezomat.valA') > 0) then
         Instance%valA(index1) = valeur
      else if ( index(NomVar, 'State.Rezomat.b') > 0) then
         Instance%b(index1) = valeur
      else if ( index(NomVar, 'State.Rezomat.pivot') > 0) then
         Instance%pivot(index1) = valeur
      else if ( index(NomVar, 'State.Rezomat.AB') > 0) then
         Instance%AB(index1,index2) = valeur
      else
         SET_DOUBLE_REZOMAT = 1
         MessageErreur         = "SET_DOUBLE_REZOMAT - Unknown variable name"
      end if
   end function SET_DOUBLE_REZOMAT


   function SET_INT_REZOMAT(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_INT_REZOMAT            ! different de 0 si erreur
      type(REZOMAT_T),        intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(in) :: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_INT_REZOMAT = 0
      MessageErreur          = ""

      if ( index(NomVar, 'State.Rezomat.SOLV') > 0) then
         Instance%SOLV = valeur
      else if ( index(NomVar, 'State.Rezomat.NNZ') > 0) then
         Instance%NNZ = valeur
      else if ( index(NomVar, 'State.Rezomat.NN1') > 0) then
         Instance%NN1 = valeur
      else if ( index(NomVar, 'State.Rezomat.NN') > 0) then
         Instance%NN = valeur
      else if ( index(NomVar, 'State.Rezomat.N') > 0) then
         Instance%N = valeur
      else if ( index(NomVar, 'State.Rezomat.IHA') > 0) then
         Instance%IHA = valeur
      else if ( index(NomVar, 'State.Rezomat.KL') > 0) then
         Instance%KL = valeur
      else if ( index(NomVar, 'State.Rezomat.KU') > 0) then
         Instance%KU = valeur
      else if ( index(NomVar, 'State.Rezomat.LDAB') > 0) then
         Instance%LDAB = valeur
      else if ( index(NomVar, 'State.Rezomat.IFAIL') > 0) then
         Instance%IFAIL = valeur
      else if ( index(NomVar, 'State.Rezomat.ipiv') > 0) then
         Instance%ipiv(index1) = valeur
      else if ( index(NomVar, 'State.Rezomat.IFLAG') > 0) then
         Instance%IFLAG(index1) = valeur
      else if ( index(NomVar, 'State.Rezomat.RowA') > 0) then
         Instance%rowA(index1) = valeur
      else if ( index(NomVar, 'State.Rezomat.ColA') > 0) then
         Instance%colA(index1) = valeur
      else if ( index(NomVar, 'State.Rezomat.snr') > 0) then
         Instance%snr(index1) = valeur
      else if ( index(NomVar, 'State.Rezomat.rnr') > 0) then
         Instance%rnr(index1) = valeur
      else if ( index(NomVar, 'State.Rezomat.ha') > 0) then
         Instance%ha(index1, index2) = valeur
      else if ( index(NomVar, 'State.Rezomat.noVarDQ') > 0) then
         Instance%noVarDQ(index1) = valeur
      else if ( index(NomVar, 'State.Rezomat.noVarDZ') > 0) then
         Instance%noVarDZ(index1) = valeur
      else if ( index(NomVar, 'State.Rezomat.noVarDQl') > 0) then
         Instance%noVarDQl(index1) = valeur
      else if ( index(NomVar, 'State.Rezomat.noVarDZc') > 0) then
         Instance%noVarDZc(index1) = valeur
      else if ( index(NomVar, 'State.Rezomat.KdNode') > 0) then
         Instance%typSec(index1) = valeur
      else if ( index(NomVar, 'State.Rezomat.headJunction') > 0) then
         Instance%headConflu(index1) = valeur
      else if ( index(NomVar, 'State.Rezomat.nxtNdJunction') > 0) then
         Instance%nextSecConflu(index1) = valeur
      else if ( index(NomVar, 'State.Rezomat.WeirNd') > 0) then
         Instance%SecSin(index1) = valeur
      else if ( index(NomVar, 'State.Rezomat.LinkNd') > 0) then
         Instance%SecLiai(index1) = valeur
      else if ( index(NomVar, 'State.Rezomat.NdLink') > 0) then
         Instance%LiaiSec(index1) = valeur
      else
         SET_INT_REZOMAT = 1
         MessageErreur         = "SET_INT_REZOMAT - Unknown variable name"
      end if
   end function SET_INT_REZOMAT



! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_REZOMAT(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_REZOMAT          ! different de 0 si erreur
      type(REZOMAT_T),        intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: taille
      integer                            :: err
      DESALLOUE_REZOMAT = 0
      MessageErreur     = ""
      err               = 0

      !----------------------------------------------
      ! Desallocation des pointers de types primitifs
      !----------------------------------------------
       if (ASSOCIATED(Instance%ipiv)) then
          taille = SIZE(Instance%ipiv)
          if (taille > 0) then
              DEALLOCATE(Instance%ipiv, STAT=err)
              if (err /= 0) then
                  DESALLOUE_REZOMAT = err
                  MessageErreur = 'Unable to deallocate REZOMAT_T.ipiv'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%ipiv)
      if (ASSOCIATED(Instance%IFLAG)) then
          taille = SIZE(Instance%IFLAG)
          if (taille > 0) then
              DEALLOCATE(Instance%IFLAG, STAT=err)
              if (err /= 0) then
                  DESALLOUE_REZOMAT = err
                  MessageErreur = 'Unable to deallocate REZOMAT_T.IFLAG'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%IFLAG)
      if (ASSOCIATED(Instance%rowA)) then
          taille = SIZE(Instance%rowA)
          if (taille > 0) then
              DEALLOCATE(Instance%rowA, STAT=err)
              if (err /= 0) then
                  DESALLOUE_REZOMAT = err
                  MessageErreur = 'Unable to deallocate REZOMAT_T.rowA'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%rowA)
      if (ASSOCIATED(Instance%colA)) then
          taille = SIZE(Instance%colA)
          if (taille > 0) then
              DEALLOCATE(Instance%colA, STAT=err)
              if (err /= 0) then
                  DESALLOUE_REZOMAT = err
                  MessageErreur = 'Unable to deallocate REZOMAT_T.colA'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%colA)
      if (ASSOCIATED(Instance%snr)) then
          taille = SIZE(Instance%snr)
          if (taille > 0) then
              DEALLOCATE(Instance%snr, STAT=err)
              if (err /= 0) then
                  DESALLOUE_REZOMAT = err
                  MessageErreur = 'Unable to deallocate REZOMAT_T.snr'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%snr)
      if (ASSOCIATED(Instance%rnr)) then
          taille = SIZE(Instance%rnr)
          if (taille > 0) then
              DEALLOCATE(Instance%rnr, STAT=err)
              if (err /= 0) then
                  DESALLOUE_REZOMAT = err
                  MessageErreur = 'Unable to deallocate REZOMAT_T.rnr'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%rnr)
      if (ASSOCIATED(Instance%ha)) then
          taille = SIZE(Instance%ha, 1)
          if (taille > 0) then
              DEALLOCATE(Instance%ha, STAT=err)
              if (err /= 0) then
                  DESALLOUE_REZOMAT = err
                  MessageErreur = 'Unable to deallocate REZOMAT_T.ha'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%ha)
      if (ASSOCIATED(Instance%noVarDQ)) then
          taille = SIZE(Instance%noVarDQ)
          if (taille > 0) then
              DEALLOCATE(Instance%noVarDQ, STAT=err)
              if (err /= 0) then
                  DESALLOUE_REZOMAT = err
                  MessageErreur = 'Unable to deallocate REZOMAT_T.noVarDQ'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%noVarDQ)
      if (ASSOCIATED(Instance%noVarDZ)) then
          taille = SIZE(Instance%noVarDZ)
          if (taille > 0) then
              DEALLOCATE(Instance%noVarDZ, STAT=err)
              if (err /= 0) then
                  DESALLOUE_REZOMAT = err
                  MessageErreur = 'Unable to deallocate REZOMAT_T.noVarDZ'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%noVarDZ)
      if (ASSOCIATED(Instance%noVarDQl)) then
          taille = SIZE(Instance%noVarDQl)
          if (taille > 0) then
              DEALLOCATE(Instance%noVarDQl, STAT=err)
              if (err /= 0) then
                  DESALLOUE_REZOMAT = err
                  MessageErreur = 'Unable to deallocate REZOMAT_T.noVarDQl'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%noVarDQl)
      if (ASSOCIATED(Instance%noVarDZc)) then
          taille = SIZE(Instance%noVarDZc)
          if (taille > 0) then
              DEALLOCATE(Instance%noVarDZc, STAT=err)
              if (err /= 0) then
                  DESALLOUE_REZOMAT = err
                  MessageErreur = 'Unable to deallocate REZOMAT_T.noVarDZc'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%noVarDZc)
      if (ASSOCIATED(Instance%typSec)) then
          taille = SIZE(Instance%typSec)
          if (taille > 0) then
              DEALLOCATE(Instance%typSec, STAT=err)
              if (err /= 0) then
                  DESALLOUE_REZOMAT = err
                  MessageErreur = 'Unable to deallocate REZOMAT_T.typSec'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%typSec)
      if (ASSOCIATED(Instance%headConflu)) then
          taille = SIZE(Instance%headConflu)
          if (taille > 0) then
              DEALLOCATE(Instance%headConflu, STAT=err)
              if (err /= 0) then
                  DESALLOUE_REZOMAT = err
                  MessageErreur = 'Unable to deallocate REZOMAT_T.headConflu'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%headConflu)
      if (ASSOCIATED(Instance%nextSecConflu)) then
          taille = SIZE(Instance%nextSecConflu)
          if (taille > 0) then
              DEALLOCATE(Instance%nextSecConflu, STAT=err)
              if (err /= 0) then
                  DESALLOUE_REZOMAT = err
                  MessageErreur = 'Unable to deallocate REZOMAT_T.nextSecConflu'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%nextSecConflu)
      if (ASSOCIATED(Instance%SecSin)) then
          taille = SIZE(Instance%SecSin)
          if (taille > 0) then
              DEALLOCATE(Instance%SecSin, STAT=err)
              if (err /= 0) then
                  DESALLOUE_REZOMAT = err
                  MessageErreur = 'Unable to deallocate REZOMAT_T.SecSin'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%SecSin)
      if (ASSOCIATED(Instance%SecLiai)) then
          taille = SIZE(Instance%SecLiai)
          if (taille > 0) then
              DEALLOCATE(Instance%SecLiai, STAT=err)
              if (err /= 0) then
                  DESALLOUE_REZOMAT = err
                  MessageErreur = 'Unable to deallocate REZOMAT_T.SecLiai'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%SecLiai)
      if (ASSOCIATED(Instance%LiaiSec)) then
          taille = SIZE(Instance%LiaiSec)
          if (taille > 0) then
              DEALLOCATE(Instance%LiaiSec, STAT=err)
              if (err /= 0) then
                  DESALLOUE_REZOMAT = err
                  MessageErreur = 'Unable to deallocate REZOMAT_T.LiaiSec'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%LiaiSec)
      if (ASSOCIATED(Instance%AFLAG)) then
          taille = SIZE(Instance%AFLAG)
          if (taille > 0) then
              DEALLOCATE(Instance%AFLAG, STAT=err)
              if (err /= 0) then
                  DESALLOUE_REZOMAT = err
                  MessageErreur = 'Unable to deallocate REZOMAT_T.AFLAG'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%AFLAG)
      if (ASSOCIATED(Instance%valA)) then
          taille = SIZE(Instance%valA)
          if (taille > 0) then
              DEALLOCATE(Instance%valA, STAT=err)
              if (err /= 0) then
                  DESALLOUE_REZOMAT = err
                  MessageErreur = 'Unable to deallocate REZOMAT_T.valA'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%valA)
      if (ASSOCIATED(Instance%b)) then
          taille = SIZE(Instance%b)
          if (taille > 0) then
              DEALLOCATE(Instance%b, STAT=err)
              if (err /= 0) then
                  DESALLOUE_REZOMAT = err
                  MessageErreur = 'Unable to deallocate REZOMAT_T.b'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%b)
      if (ASSOCIATED(Instance%pivot)) then
          taille = SIZE(Instance%pivot)
          if (taille > 0) then
              DEALLOCATE(Instance%pivot, STAT=err)
              if (err /= 0) then
                  DESALLOUE_REZOMAT = err
                  MessageErreur = 'Unable to deallocate REZOMAT_T.pivot'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%pivot)
      if (ASSOCIATED(Instance%AB)) then
          taille = SIZE(Instance%AB,1)
          if (taille > 0) then
              DEALLOCATE(Instance%AB, STAT=err)
              if (err /= 0) then
                  DESALLOUE_REZOMAT = err
                  MessageErreur = 'Unable to deallocate REZOMAT_T.AB'
                  return
              endif
          endif
      endif
      NULLIFY(Instance%AB)
      !--------------------------------------------------------
      ! Fin de la desallocation des pointers de types primitifs
      !--------------------------------------------------------

   end function DESALLOUE_REZOMAT

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_REZOMAT(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_REZOMAT          ! different de 0 si erreur
      type(REZOMAT_T),        intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      NULLIFIER_REZOMAT = 0
      MessageErreur          = ""

      !----------------------------------------------
      ! Nullifie des pointers de types primitifs
      !----------------------------------------------
      NULLIFY(Instance%ipiv)
      NULLIFY(Instance%IFLAG)
      NULLIFY(Instance%rowA)
      NULLIFY(Instance%colA)
      NULLIFY(Instance%snr)
      NULLIFY(Instance%rnr)
      NULLIFY(Instance%ha)
      NULLIFY(Instance%noVarDQ)
      NULLIFY(Instance%noVarDZ)
      NULLIFY(Instance%noVarDQl)
      NULLIFY(Instance%noVarDZc)
      NULLIFY(Instance%typSec)
      NULLIFY(Instance%headConflu)
      NULLIFY(Instance%nextSecConflu)
      NULLIFY(Instance%SecSin)
      NULLIFY(Instance%SecLiai)
      NULLIFY(Instance%LiaiSec)
      NULLIFY(Instance%AFLAG)
      NULLIFY(Instance%valA)
      NULLIFY(Instance%b)
      NULLIFY(Instance%pivot)
      NULLIFY(Instance%AB)
      !--------------------------------------------------------
      ! Fin de la Nullification des pointers de types primitifs
      !--------------------------------------------------------

   end function NULLIFIER_REZOMAT

end module M_REZOMAT_T
