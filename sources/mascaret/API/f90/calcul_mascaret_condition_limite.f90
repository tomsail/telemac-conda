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
   ! Calcul d'un nouvel etat au "TpsFinal" en utilisant le modele courant et l'etat precedent
   ! .................................................................................................................................
subroutine CALCUL_MASCARET_CONDITION_LIMITE(Erreur, Identifiant, TpsInitial, TpsFinal, PasTps, &
                                            TpsCl, TailleTpsCL, Cl1, CL2, Impression)
   use M_ERREUR_T            ! Type ERREUR_T
   use M_LOI_T               ! Type LOI_T
   use M_MODELE_MASCARET_T   ! Type MODELE_MASCARET_T
   use M_ETAT_MASCARET_T     ! Type ETAT_MASCARET_T

   use M_APPORT_T            ! Type APPORT_T
   use M_BARRAGE_T           ! Type BARRAGE_T
   use M_CONFLUENT_T         ! Type CONFLUENT_T
   use M_CONNECT_T           ! Type CONNECT_T : connectivite du reseau
   use M_DEVERSOIR_T         ! Type DEVERSOIR_T
   use M_EXTREMITE_T         ! Type EXTREMITE_T
   use M_FICHIER_T           ! Type FICHIER_T
   use M_PROFIL_T            ! Type PROFIL_T
   use M_PROFIL_PLAN_T       ! Type PROFIL_PLAN_T
   use M_SECTION_T           ! Type SECTION_PLAN_T
   use M_SECTION_PLAN_T      ! Type SECTION_T
   use M_SINGULARITE_T       ! Type SINGULARITE_T
   use M_ZONE_SECHE_T        ! Type ZONE_SECHE_T
   use M_SAUVE_T             ! Type SAUVE_T
   use M_CASIER_T            ! Type CASIER_T
   use M_LIAISON_T           ! Type LIAISON_T
   use M_APPORT_PLUIE_T      ! Type APPORT_PLUIE_T


   use M_INDEX_VARIABLE_C    ! Index des variables
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_MESSAGE_C           ! Messages d'erreur
   use M_PARAMETRE_C         ! EPS2,SEPS

   use M_INTERSECT_I
   use M_APIMASCARET_STATIC
   use M_PLANIM_I
   use M_PLANMA_I
   use M_POST_I
   use M_POST_IMP_I
   use M_PRETRAIT_INTERFACE_I
   use M_QCL_I
   use M_REZO_INTERFACE_I
   use M_SARAP_I
   use M_CLPLUIE_I
   use M_POST_CASIER_I
   use M_POST_IMP_CASIER_I
   use M_STOCK_I
   use M_STOCK_REP_I
   use M_TRAITER_ERREUR_I    ! Traitement de l'errreur
   use M_TRAITER_ERREUR_CASIER_I
   use M_MASCARET_INTERFACE_I

   implicit none

   integer, intent(out)                        :: Erreur              ! different de 0 si erreur
   integer, intent(in )                        :: Identifiant         ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   real(8), intent(in )                        :: TpsInitial          ! Temps initial du calcul
   real(8), intent(in )                        :: TpsFinal            ! Temps final du calcul
   real(8), intent(in )                        :: PasTps              ! Pas de temps interne du calcul
   real(8), dimension(*),   intent(in)         :: TpsCl               ! Le vecteur temps commun aux conditons limites - Dimension : TailleTpsCL
   integer, intent(in )                        :: TailleTpsCL         ! Nombre de pas de temps pour les conditions limites
   real(8), dimension(TailleTpsCL,*), intent(in) :: Cl1               ! Composante 1 de la condition limite (type1->Q, type2->Z, type3->Q, type7->Zinf
                                                                      ! Dimension : (TailleTpsCL x NbCL)
   real(8), dimension(TailleTpsCL,*), intent(in) :: Cl2               ! Composante 2 de la condition limite (type1 et type2->Pas utilise, type3->Z, type7->Zsup
                                                                      ! Dimension : (TailleTpsCL x NbCL)
   integer, intent(in )                        :: Impression          ! impression sur les fichiers listing (1-> Vrai 0-> Faux)

   integer nbLoi, nbCL, i, j, t

   Erreur = 0
   
    if (ASSOCIATED(ptrTabMascaret(Identifiant)%ModeleMascaret%LoisHydrau)) then
      nbLoi = size(ptrTabMascaret(Identifiant)%ModeleMascaret%LoisHydrau)
   else
      ptrMsgsErreurs(Identifiant) = 'COMPUTE_MASCARET_BOUNDARY_CONDITION - No graph found'
      Erreur = 2
      RETURN
   endif

! Modification de la loi par les donnes des conditions limites passee en argument
   nbCL = 0
   do i=1, nbLoi
      t = ptrTabMascaret(Identifiant)%ModeleMascaret%LoisHydrau(i)%Type
      if ( (t == 1) .OR. (t == 2) .OR. (t == 3) .OR. (t == 7) ) then
         NbCL = NbCL +1
         call ALLOCATE_LOI_CL(Erreur, ptrTabMascaret(Identifiant)%ModeleMascaret%LoisHydrau(i), &
                             t, TailleTpsCL)
         if(erreur.ne.0) then
            ptrMsgsErreurs(Identifiant) = 'Problem with the allocation of variables for the boundary conditions'
            return
         endif
         do j=1, TailleTpsCL
            ptrTabMascaret(Identifiant)%ModeleMascaret%LoisHydrau(i)%Temps(j) = TpsCl(j)

            if (t == 1) then
               ptrTabMascaret(Identifiant)%ModeleMascaret%LoisHydrau(i)%Debit(j) = Cl1(j,NbCL)
            endif
            if (t == 2) then
              ptrTabMascaret(Identifiant)%ModeleMascaret%LoisHydrau(i)%Cote(j) = Cl1(j,NbCL)
            endif
            if (t == 3) then
               ptrTabMascaret(Identifiant)%ModeleMascaret%LoisHydrau(i)%Debit(j) = Cl1(j,NbCL)
               ptrTabMascaret(Identifiant)%ModeleMascaret%LoisHydrau(i)%Cote(j) = Cl2(j,NbCL)
            endif
            if (t == 7) then
               ptrTabMascaret(Identifiant)%ModeleMascaret%LoisHydrau(i)%CoteInf(j) = Cl1(j,NbCL)
               ptrTabMascaret(Identifiant)%ModeleMascaret%LoisHydrau(i)%CoteSup(j) = Cl2(j,NbCL)
            endif
         end do
      endif
   end do
! FIN Modification de la loi par les donnes des conditions limites passee en argument

   call CALCUL_MASCARET(Erreur, Identifiant, TpsInitial, TpsFinal, PasTps, Impression)

   return
   
end subroutine CALCUL_MASCARET_CONDITION_LIMITE


!==============================================================
subroutine ALLOCATE_LOI_CL(Erreur, Loi, TypeCl, TailleTpsCL)
   use M_LOI_T               ! Type LOI_T
   implicit none

   integer,                         intent(out)   :: Erreur        ! different de 0 si erreur
   type(LOI_T),                     intent(inout) :: Loi           ! Loi a modifier par la nouvelle condition limite
   integer,                         intent(in)    :: TypeCl        ! Nouveau type de loi : 1->Q=f(T) 2->Z=f(T) 3->Q,Z=f(T) 7->Zinf,Zsup=f(T)
   integer,                         intent(in)    :: TailleTpsCL   !

   Erreur = 0
   
!--- Desallocation et reallocation si necessaire des pointeurs de la loi
   if (size(Loi%Temps) /= TailleTpsCL) then
      DEALLOCATE(Loi%Temps, STAT=Erreur)
      if (Erreur /= 0) then
         return
      endif
      ALLOCATE(Loi%Temps(TailleTpsCL), STAT=Erreur)
      if (Erreur /= 0) then
         return
      endif
   end if

   if ((TypeCl == 1) .or. (TypeCl == 3)) then
      if (size(Loi%Debit) /= TailleTpsCL) then
         DEALLOCATE(Loi%Debit, STAT=Erreur)
         if (Erreur /= 0) then
            return
         endif
         ALLOCATE(Loi%Debit(TailleTpsCL), STAT=Erreur)
         if (Erreur /= 0) then
            return
         endif
      endif
   endif
   if ((TypeCl == 2) .or. (TypeCl == 3)) then
      if (size(Loi%Cote) /= TailleTpsCL) then
         DEALLOCATE(Loi%Cote, STAT=Erreur)
         if (Erreur /= 0) then
            return
         endif
         ALLOCATE(Loi%Cote(TailleTpsCL), STAT=Erreur)
         if (Erreur /= 0) then
            return
         endif
      endif
   endif
   if (TypeCl == 7) then
      if (size(Loi%CoteInf) /= TailleTpsCL) then
         DEALLOCATE(Loi%CoteInf, STAT=Erreur)
         if (Erreur /= 0) then
            return
         endif
         ALLOCATE(Loi%CoteInf(TailleTpsCL), STAT=Erreur)
         if (Erreur /= 0) then
            return
         endif
      endif
      if (size(Loi%CoteSup) /= TailleTpsCL) then
         DEALLOCATE(Loi%CoteSup, STAT=Erreur)
         if (Erreur /= 0) then
            return
         endif
         ALLOCATE(Loi%CoteSup(TailleTpsCL), STAT=Erreur)
         if (Erreur /= 0) then
            return
         endif
      endif
   endif
!--- Fin Desallocation et realocation si necessaire
   return
   
end subroutine ALLOCATE_LOI_CL

!=========================================================================




