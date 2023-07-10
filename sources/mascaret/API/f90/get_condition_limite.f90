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

! *********************************************************************
! PROGICIEL : MASCARET       J.-M. LACOMBE
!
! VERSION : V8P4R0              EDF-CEREMA
! *********************************************************************
!.................................................................................................................................
! Recupere le nombre de condition limite dans le modele
!.................................................................................................................................
subroutine GET_NB_CONDITION_LIMITE_MASCARET(Erreur, Identifiant, NbCL)
   use M_APIMASCARET_STATIC
   use M_LOI_T               ! Type LOI_T
   use M_MODELE_MASCARET_T   ! Type MODELE_MASCARET_T

   implicit none
   integer, intent(out)                         :: Erreur             ! different de 0 si erreur
   integer , intent(in)                         :: Identifiant        ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   integer , intent(out)                        :: NbCL               ! Nombre de condition limite dans le modele

   !-- Scalaires locaux
   type(MODELE_MASCARET_T)  :: Model
   integer nbLoi, i, t

   Erreur = TEST_INIT_AND_ID(Identifiant, 'GET_NB_CONDITION_LIMITE_MASCARET')
   if (Erreur > 0 ) then
      RETURN
   end if

   Model = ptrTabMascaret(Identifiant)%ModeleMascaret

   if (ASSOCIATED(Model%LoisHydrau)) then
      nbLoi = size(Model%LoisHydrau)
   else
      NbCL = 0
      RETURN
   endif

   NbCL = 0
   do i=1, nbLoi
      t = Model%LoisHydrau(i)%Type
      if ( (t == 1) .OR. (t == 2) .OR. (t == 3) .OR. (t == 7) ) then
         NbCL = NbCL +1
      endif
   end do
   RETURN

end subroutine GET_NB_CONDITION_LIMITE_MASCARET

!.................................................................................................................................
! Recupere le nom de la condition limite dans le modele
!.................................................................................................................................
subroutine GET_NOM_CONDITION_LIMITE_MASCARET(Erreur, Identifiant, NumCL, NomCL, NumLoi)
   use M_APIMASCARET_STATIC
   use M_LOI_T               ! Type LOI_T
   use M_MODELE_MASCARET_T   ! Type MODELE_MASCARET_T

   implicit none
   integer, intent(out)                         :: Erreur             ! different de 0 si erreur
   integer , intent(in)                         :: Identifiant        ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   integer , intent(in)                         :: NumCL              ! Numero de la condition limite dans le modele
   character(LEN=30), intent(out)               :: NomCL              ! Nom de la condition limite identifie par son numero
   integer , intent(out)                        :: NumLoi             ! Numero de la Loi correspondant a la condition limite dans le modele

   !-- Scalaires locaux
   type(MODELE_MASCARET_T)  :: Model
   integer nbLoi, i, t, nbCL


   Erreur = 0
   NomCL = ""
   NumLoi = 0
   if (Identifiant < 0) then
      MsgErreur = 'GET_NOM_CONDITION_LIMITE - Identifiant negatif : pas autorise'
      Erreur = 2
      RETURN
   end if
   if (Identifiant > NB_MAX_MASCARET) then
      MsgErreur = 'GET_NOM_CONDITION_LIMITE - Identifiant trop grand : pas autorise'
      Erreur = 2
      RETURN
   end if

   if (mascaretCree(Identifiant) == 0) then
      ptrMsgsErreurs(Identifiant) = 'GET_NOM_CONDITION_LIMITE - Mascaret n''est pas cree'
      Erreur = 2
      RETURN
   end if
   Model = ptrTabMascaret(Identifiant)%ModeleMascaret

   if (ASSOCIATED(Model%LoisHydrau)) then
      nbLoi = size(Model%LoisHydrau)
   else
      ptrMsgsErreurs(Identifiant) = 'GET_NOM_CONDITION_LIMITE - Aucune loi n''est presente dans le modele'
      Erreur = 2
      RETURN
   endif

   nbCL = 0
   do i=1, nbLoi
      t = Model%LoisHydrau(i)%Type
      if ( (t == 1) .OR. (t == 2) .OR. (t == 3) .OR. (t == 7) ) then
         NbCL = NbCL +1
         if (NbCL==NumCL) then
            NomCL = Model%LoisHydrau(i)%Nom
            NumLoi = i
            RETURN
         endif
      endif
   end do

   ptrMsgsErreurs(Identifiant) = 'GET_NOM_CONDITION_LIMITE - La condition limite demandee n''existe pas'
   Erreur = 2
   RETURN
end subroutine GET_NOM_CONDITION_LIMITE_MASCARET





