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
   ! Importation de l'etat Mascaret a partir du fichier natif contenant la ligne d'eau initiale
   ! .................................................................................................................................
subroutine INIT_ETAT_MASCARET(Erreur, Identifiant, NomFichier, Impression)
   use M_APIMASCARET_STATIC
   use M_MODELE_MASCARET_T   ! Type MODELE_MASCARET_T
   use M_ETAT_MASCARET_T     ! Type ETAT_MASCARET_T
   use M_CONSTANTES_CALCUL_C
   use M_LEC_LIGNE_INTERFACE_I

   implicit none

   integer, intent(out)                        :: Erreur         ! different de 0 si erreur
   integer, intent(in )                        :: Identifiant    ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   character(LEN=255), intent(in)              :: NomFichier     ! Nom du fichier natif contenant la ligne d'eau initiale
   integer, intent(in )                        :: Impression     ! impression sur les fichiers listing (1-> Vrai 0-> Faux)



   !-- Scalaires locaux

   type(MODELE_MASCARET_T)  :: Model
   type(ETAT_MASCARET_T)    :: Etat
   integer :: retour
   integer :: nb_sect
   integer :: nbCasier
   integer :: nbLiaison
   integer :: i
   type(FICHIER_T) :: FichierLigne ! fichier l.e. initial
   integer :: FormatLigne          ! format LIDO/OPTHYCA
   logical :: ImpressionLigne      ! test d'impression
   integer :: UniteListing         ! Unite logique fichier listing
   type(ERREUR_T)  :: ErreurLecLigne


   Erreur = TEST_INIT_AND_ID(Identifiant, 'INIT_ETAT_MASCARET')
   if (Erreur > 0 ) then
      RETURN
   end if
   Model = ptrTabMascaret(Identifiant)%ModeleMascaret
   Etat  = ptrTabMascaret(Identifiant)%EtatMascaret

! Initialisation a null des pointeurs de l'etat
   if(associated(Etat%DPDZ2)) then
       deallocate(Etat%DPDZ2)
       nullify(Etat%DPDZ2)
   endif
   if(associated(Etat%DPDZ1)) then
       deallocate(Etat%DPDZ1)
       nullify(Etat%DPDZ1)
   endif
   if(associated(Etat%QDeverse)) then
       deallocate(Etat%QDeverse)
       nullify(Etat%QDeverse)
   endif
   if(associated(Etat%Qinjec)) then
       deallocate(Etat%Qinjec)
       nullify(Etat%Qinjec)
   endif
   if(associated(Etat%Airs)) then
       deallocate(Etat%Airs)
       nullify(Etat%Airs)
   endif
   if(associated(Etat%W)) then
       deallocate(Etat%W)
       nullify(Etat%W)
   endif
   if(associated(Etat%YNode)) then
       deallocate(Etat%YNode)
       nullify(Etat%YNode)
   endif
   if(associated(Etat%CNode)) then
       deallocate(Etat%CNode)
       nullify(Etat%CNode)
   endif
   if(associated(Etat%UNode)) then
       deallocate(Etat%UNode)
       nullify(Etat%UNode)
   endif
   if(associated(Etat%XFron)) then
       deallocate(Etat%XFron)
       nullify(Etat%XFron)
   endif
   if(associated(Etat%RH2)) then
       deallocate(Etat%RH2)
       nullify(Etat%RH2)
   endif
   if(associated(Etat%RH1)) then
       deallocate(Etat%RH1)
       nullify(Etat%RH1)
   endif
   if(associated(Etat%BS)) then
       deallocate(Etat%BS)
       nullify(Etat%BS)
   endif
   if(associated(Etat%B2)) then
       deallocate(Etat%B2)
       nullify(Etat%B2)
   endif
   if(associated(Etat%B1)) then
       deallocate(Etat%B1)
       nullify(Etat%B1)
   endif
   if(associated(Etat%P2)) then
       deallocate(Etat%P2)
       nullify(Etat%P2)
   endif
   if(associated(Etat%P1)) then
       deallocate(Etat%P1)
       nullify(Etat%P1)
   endif
   if(associated(Etat%Froude)) then
       deallocate(Etat%Froude)
       nullify(Etat%Froude)
   endif
   if(associated(Etat%Beta)) then
       deallocate(Etat%Beta)
       nullify(Etat%Beta)
   endif
   if(associated(Etat%S2)) then
       deallocate(Etat%S2)
       nullify(Etat%S2)
   endif
   if(associated(Etat%S1)) then
       deallocate(Etat%S1)
       nullify(Etat%S1)
   endif
   if(associated(Etat%SS)) then
       deallocate(Etat%SS)
       nullify(Etat%SS)
   endif
   if(associated(Etat%Q2)) then
       deallocate(Etat%Q2)
       nullify(Etat%Q2)
   endif
   if(associated(Etat%Q1)) then
       deallocate(Etat%Q1)
       nullify(Etat%Q1)
   endif
   if(associated(Etat%V1)) then
       deallocate(Etat%V1)
       nullify(Etat%V1)
   endif
   if(associated(Etat%V2)) then
       deallocate(Etat%V2)
       nullify(Etat%V2)
   endif
   if(associated(Etat%Y)) then
       deallocate(Etat%Y)
       nullify(Etat%Y)
   endif
   if(associated(Etat%VOL)) then
       deallocate(Etat%VOL)
       nullify(Etat%VOL)
   endif
   if(associated(Etat%VOLS)) then
       deallocate(Etat%VOLS)
       nullify(Etat%VOLS)
   endif
   if(associated(Etat%Q)) then
       deallocate(Etat%Q)
       nullify(Etat%Q)
   endif
   if(associated(Etat%Z)) then
       deallocate(Etat%Z)
       nullify(Etat%Z)
   endif
   if(associated(Etat%Liaisons)) then
       deallocate(Etat%Liaisons)
       nullify(Etat%Liaisons)
   endif
   if(associated(Etat%Casiers)) then
       deallocate(Etat%Casiers)
       nullify(Etat%Casiers)
   endif
   if(associated(Etat%JGNODE)) then
       deallocate(Etat%JGNODE)
       nullify(Etat%JGNODE)
   endif
   if(associated(Etat%JDNODE)) then
       deallocate(Etat%JDNODE)
       nullify(Etat%JDNODE)
   endif
   if(associated(Etat%IFIGE)) then
       deallocate(Etat%IFIGE)
       nullify(Etat%IFIGE)
   endif
   if(associated(Etat%FLUX)) then
       deallocate(Etat%FLUX)
       nullify(Etat%FLUX)
   endif
   if(associated(Etat%DebitFlux)) then
       deallocate(Etat%DebitFlux)
       nullify(Etat%DebitFlux)
   endif
   if(associated(Etat%MatriceRezo%ipiv)) then
       deallocate(Etat%MatriceRezo%ipiv)
       nullify(Etat%MatriceRezo%ipiv)
   endif
   if(associated(Etat%MatriceRezo%IFLAG)) then
       deallocate(Etat%MatriceRezo%IFLAG)
       nullify(Etat%MatriceRezo%IFLAG)
   endif
   if(associated(Etat%MatriceRezo%rowA)) then
       deallocate(Etat%MatriceRezo%rowA)
       nullify(Etat%MatriceRezo%rowA)
   endif
   if(associated(Etat%MatriceRezo%colA)) then
       deallocate(Etat%MatriceRezo%colA)
       nullify(Etat%MatriceRezo%colA)
   endif
   if(associated(Etat%MatriceRezo%snr)) then
       deallocate(Etat%MatriceRezo%snr)
       nullify(Etat%MatriceRezo%snr)
   endif
   if(associated(Etat%MatriceRezo%rnr)) then
       deallocate(Etat%MatriceRezo%rnr)
       nullify(Etat%MatriceRezo%rnr)
   endif
   if(associated(Etat%MatriceRezo%ha)) then
       deallocate(Etat%MatriceRezo%ha)
       nullify(Etat%MatriceRezo%ha)
   endif
   if(associated(Etat%MatriceRezo%noVarDQ)) then
       deallocate(Etat%MatriceRezo%noVarDQ)
       nullify(Etat%MatriceRezo%noVarDQ)
   endif
   if(associated(Etat%MatriceRezo%noVarDZ)) then
       deallocate(Etat%MatriceRezo%noVarDZ)
       nullify(Etat%MatriceRezo%noVarDZ)
   endif
   if(associated(Etat%MatriceRezo%noVarDQl)) then
       deallocate(Etat%MatriceRezo%noVarDQl)
       nullify(Etat%MatriceRezo%noVarDQl)
   endif
   if(associated(Etat%MatriceRezo%noVarDZc)) then
       deallocate(Etat%MatriceRezo%noVarDZc)
       nullify(Etat%MatriceRezo%noVarDZc)
   endif
   if(associated(Etat%MatriceRezo%typSec)) then
       deallocate(Etat%MatriceRezo%typSec)
       nullify(Etat%MatriceRezo%typSec)
   endif
   if(associated(Etat%MatriceRezo%headConflu)) then
       deallocate(Etat%MatriceRezo%headConflu)
       nullify(Etat%MatriceRezo%headConflu)
   endif
   if(associated(Etat%MatriceRezo%nextSecConflu)) then
       deallocate(Etat%MatriceRezo%nextSecConflu)
       nullify(Etat%MatriceRezo%nextSecConflu)
   endif
   if(associated(Etat%MatriceRezo%SecSin)) then
       deallocate(Etat%MatriceRezo%SecSin)
       nullify(Etat%MatriceRezo%SecSin)
   endif
   if(associated(Etat%MatriceRezo%SecLiai)) then
       deallocate(Etat%MatriceRezo%SecLiai)
       nullify(Etat%MatriceRezo%SecLiai)
   endif
   if(associated(Etat%MatriceRezo%LiaiSec)) then
       deallocate(Etat%MatriceRezo%LiaiSec)
       nullify(Etat%MatriceRezo%LiaiSec)
   endif
   if(associated(Etat%MatriceRezo%AFLAG)) then
       deallocate(Etat%MatriceRezo%AFLAG)
       nullify(Etat%MatriceRezo%AFLAG)
   endif
   if(associated(Etat%MatriceRezo%valA)) then
       deallocate(Etat%MatriceRezo%valA)
       nullify(Etat%MatriceRezo%valA)
   endif
   if(associated(Etat%MatriceRezo%b)) then
       deallocate(Etat%MatriceRezo%b)
       nullify(Etat%MatriceRezo%b)
   endif
   if(associated(Etat%MatriceRezo%pivot)) then
       deallocate(Etat%MatriceRezo%pivot)
       nullify(Etat%MatriceRezo%pivot)
   endif
   if(associated(Etat%MatriceRezo%AB)) then
       deallocate(Etat%MatriceRezo%AB)
       nullify(Etat%MatriceRezo%AB)
   endif
   if(associated(Etat%IDEB)) then
       deallocate(Etat%IDEB)
       nullify(Etat%IDEB)
   endif
   if(associated(Etat%IFIN)) then
       deallocate(Etat%IFIN)
       nullify(Etat%IFIN)
   endif
   if(associated(Etat%ITEM0)) then
       deallocate(Etat%ITEM0)
       nullify(Etat%ITEM0)
   endif
   if(associated(Etat%ZINIT)) then
       deallocate(Etat%ZINIT)
       nullify(Etat%ZINIT)
   endif

   Etat%tempsPrecedent = 0.
   Etat%numPasTps = 0
   Etat%DT = 0.
   Etat%DTRezo = 0.
   Etat%NBARAD = 0

   FichierLigne%Unite = 19
   FichierLigne%Nom = NomFichier

   ImpressionLigne = Model%ImpressionCalcul
   if (ImpressionLigne) then
      UniteListing = 22
      open(unit=UniteListing, file=Model%FichierListing%Nom, access='SEQUENTIAL', &
           action='WRITE'           , form='FORMATTED'       , iostat=RETOUR      , &
           position='append')
      if (RETOUR /= 0) then
         if(impression.ne.0) then
             print *,'Error while opening the listing file!'
         endif
         Erreur = 1
         return
      end if
   else
      UniteListing = -1
   endif

   FormatLigne = FORMAT_STO_PERMANENT
   call LEC_LIGNE_INTERFACE( &
                 Etat%Z , & ! Cote initiale
                 Etat%Q , & ! debit initial
              Model%CF1 , & ! Coefficient de frottement mineur
              Model%CF2 , & ! Coefficient de frottement majeur
                Model%X , & ! maillage
           FichierLigne , & ! fichier l.e. initial
            FormatLigne , & ! format LIDO/OPTHYCA
     Model%TypeMaillage , & ! Mode de calcul du maillage
        ImpressionLigne , & ! test d'impression
           UniteListing , & ! Unite logique fichier listing
          Model%Profils , & ! Profils geometriques
          Model%ProfAbs , & ! Abscisse absolue
      Model%ProfDebBief , & ! Premiers profils des biefs
      Model%ProfFinBief , & ! Derniers profils des biefs
      Model%absc_rel_ext_deb_bief , & ! Abscisse relative de debut de bief
      Model%absc_rel_ext_fin_bief , & ! Abscisse relative de fin de bief
           ErreurLecLigne ) ! Erreur
      if (ErreurLecLigne%Numero /= 0) then
         Erreur = ErreurLecLigne%Numero
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - LEC_LIGNE_INTERFACE - '//ErreurLecLigne%Message
         if (ImpressionLigne) then
            rewind(UniteListing)
            close(UniteListing)
         endif
         return
      endif

   nb_sect = size(Model%X)
! ALLOCATIONS reprises de rezo
!======================================================
   retour = 0
   if(.not.associated(Etat%DPDZ1)) allocate (Etat%DPDZ1(nb_sect), STAT = retour)
   if (retour /= 0) then
      Erreur = retour
      ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.DPDZ1'
      return
   endif
   Etat%DPDZ1(:) = W0    ! W0 = 0._DOUBLE
   if(.not.associated(Etat%DPDZ2)) allocate (Etat%DPDZ2(nb_sect), STAT = retour)
   if (retour /= 0) then
      Erreur = retour
      ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.DPDZ2'
      return
   endif
   Etat%DPDZ2(:) = W0    ! W0 = 0._DOUBLE

   !==============================================================
   ! A PARTIR DE MAINTENANT, LE MAILLAGE EST CONNU,
   ! Z ET Q SONT EVENTUELLEMENT ALLOUE ET SONT CONNUS SI L.E. INI,
   ! SI ON N'A PAS DE LE INI (CAS PERMANENTS) => IL FAUT ALLOUER
   ! Z, Q ET LES INITIALISER
   !==============================================================
   if( .not. associated(Etat%Z) ) then
      allocate( Etat%Z(nb_sect) , STAT = retour )
      if( retour /= 0 ) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.Z'
          return
      end if
      Etat%Z(:) = W0    ! W0 = 0._DOUBLE

      allocate( Etat%Q(nb_sect) , STAT = retour )
      if( retour /= 0 ) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.Q'
         return
      end if
      Etat%Q(:) = W0    ! W0 = 0._DOUBLE
  endif

! ALLOCATIONS ET INITIALISATIONS repris du superviseur
!======================================================

! Allocation des variables conmunes a LIDO et MASCARET
      if(.not.associated(Etat%Q1)) allocate (Etat%Q1(nb_sect), STAT = retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.Q1'
         return
      endif
      Etat%Q1(:) = Etat%Q(:)    ! permet d'entrer le debit de la ligne d'eau initiale au premier pas de temps

      if(.not.associated(Etat%V1)) allocate (Etat%V1(nb_sect), STAT = retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.V1'
         return
      endif
      Etat%V1(:) = W0    ! W0 = 0._DOUBLE

      if(.not.associated(Etat%Qdeverse)) allocate (Etat%Qdeverse(nb_sect), STAT = retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.Qspilled'
         return
      endif
      Etat%Qdeverse(:) = W0    ! W0 = 0._DOUBLE

      if(.not.associated(Etat%Qinjec)) allocate (Etat%Qinjec(nb_sect), STAT = retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.Qinflow'
         return
      endif
      Etat%Qinjec(:) = W0    ! W0 = 0._DOUBLE

      if(.not.associated(Etat%Q2)) allocate (Etat%Q2(nb_sect), STAT = retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.Q2'
         return
      endif
      Etat%Q2(:) = W0    ! W0 = 0._DOUBLE

      if(.not.associated(Etat%V2)) allocate (Etat%V2(nb_sect), STAT = retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.V2'
         return
      endif
      Etat%V2(:) = W0    ! W0 = 0._DOUBLE

      if(.not.associated(Etat%Y)) allocate (Etat%Y(nb_sect), STAT = retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.Y'
         return
      endif
      Etat%Y(:) = W0    ! W0 = 0._DOUBLE

      if(.not.associated(Etat%S1)) allocate (Etat%S1(nb_sect), STAT = retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.S1'
         return
      endif
      Etat%S1(:) = W0    ! W0 = 0._DOUBLE

      if(.not.associated(Etat%S2)) allocate (Etat%S2(nb_sect), STAT = retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.S2'
         return
      endif
      Etat%S2(:) = W0    ! W0 = 0._DOUBLE

      if(.not.associated(Etat%BETA)) allocate (Etat%BETA(nb_sect), STAT = retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.BETA'
         return
      endif
      Etat%BETA(:) = W0    ! W0 = 0._DOUBLE

      if(.not.associated(Etat%Froude)) allocate (Etat%Froude(nb_sect), STAT = retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.Froude'
         return
      endif
      Etat%Froude(:) = W0    ! W0 = 0._DOUBLE

      if(.not.associated(Etat%FLUX)) allocate(Etat%FLUX(nb_sect,2) , STAT = retour )
      if( retour /= 0 ) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.FLUX'
         return
      end if
      Etat%Flux(:,:) = W0

      if(.not.associated(Etat%DebitFlux)) allocate(Etat%DebitFlux(nb_sect) , STAT = retour )
      if( retour /= 0 ) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.Flow'
         return
      end if
      Etat%DebitFlux (:) = W0

      ! Allocation des variables specifiques a LIDO

      if(.not.associated(Etat%P1)) allocate (Etat%P1(nb_sect), STAT = retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.P1'
         return
      endif
      Etat%P1(:) = W0    ! W0 = 0._DOUBLE

      if(.not.associated(Etat%P2)) allocate (Etat%P2(nb_sect), STAT = retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.P2'
         return
      endif
      Etat%P2(:) = W0    ! W0 = 0._DOUBLE

      if(.not.associated(Etat%B1)) allocate (Etat%B1(nb_sect), STAT = retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.B1'
         return
      endif
      Etat%B1(:) = W0    ! W0 = 0._DOUBLE

      if(.not.associated(Etat%B2)) allocate (Etat%B2(nb_sect), STAT = retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.B2'
         return
      endif
      Etat%B2(:) = W0    ! W0 = 0._DOUBLE

      if(.not.associated(Etat%BS)) allocate (Etat%BS(nb_sect), STAT = retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.BS'
         return
      endif
      Etat%BS(:) = W0    ! W0 = 0._DOUBLE

      if(.not.associated(Etat%RH1)) allocate (Etat%RH1(nb_sect), STAT = retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.RH1'
         return
      endif
      Etat%RH1(:) = W0    ! W0 = 0._DOUBLE

      if(.not.associated(Etat%RH2)) allocate (Etat%RH2(nb_sect), STAT = retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.RH2'
         return
      endif
      Etat%RH2(:) = W0    ! W0 = 0._DOUBLE

   ! Allocation des variables specifiques a MASCARET
      if (Model%Noyau == NOYAU_MASCARET) then
         if(.not.associated(Etat%XFRON)) allocate (Etat%XFRON(nb_sect), STAT = retour)
         if (retour /= 0) then
            Erreur = retour
            ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.XFRON'
            return
         endif
         Etat%XFRON(:) = W0    ! W0 = 0._DOUBLE
      else
         if(associated(Etat%XFRON)) deallocate(Etat%XFRON)
         nullify(Etat%XFRON)
      endif

      if(.not.associated(Etat%UNODE)) allocate (Etat%UNODE(nb_sect), STAT = retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.UNODE'
         return
      endif
      Etat%UNODE(:) = W0    ! W0 = 0._DOUBLE

      if(.not.associated(Etat%CNODE)) allocate (Etat%CNODE(nb_sect), STAT = retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.CNODE'
         return
      endif
      Etat%CNODE(:) = W0    ! W0 = 0._DOUBLE

      if(.not.associated(Etat%YNODE)) allocate (Etat%YNODE(nb_sect), STAT = retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.YNODE'
         return
      endif
      Etat%YNODE(:) = W0    ! W0 = 0._DOUBLE

      if(.not.associated(Etat%JGNODE)) allocate (Etat%JGNODE(nb_sect), STAT = retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.JGNODE'
         return
      endif
      Etat%JGNODE(:) = W0    ! W0 = 0._DOUBLE

      if(.not.associated(Etat%JDNODE)) allocate (Etat%JDNODE(nb_sect), STAT = retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.JDNODE'
         return
      endif
      Etat%JDNODE(:) = W0    ! W0 = 0._DOUBLE

      if(.not.associated(Etat%IFIGE)) allocate (Etat%IFIGE(nb_sect), STAT = retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.IFIGE'
         return
      endif
      Etat%IFIGE(:) = W0    ! W0 = 0._DOUBLE

      if(.not.associated(Etat%W)) allocate (Etat%W(3,12,size(Model%Connect%OrigineBief)), STAT= retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.W'
         return
      endif
      Etat%W(:,:,:) = W0    ! W0 = 0._DOUBLE

      if(.not.associated(Etat%AIRS)) allocate (Etat%AIRS(12,size(Model%Connect%OrigineBief)), STAT= retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.AIRS'
         return
      endif
      Etat%AIRS(:,:) = W0    ! W0 = 0._DOUBLE

      if (Model%Noyau == NOYAU_MASCARET) then
        if(.not.associated(Etat%IDEB)) allocate (Etat%IDEB(size(Model%Connect%OrigineBief)), STAT = retour)
        if (retour /= 0) then
           Erreur = retour
           ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.IDEB'
           return
        endif

        if(.not.associated(Etat%IFIN)) allocate (Etat%IFIN(size(Model%Connect%OrigineBief)), STAT = retour)
        if (retour /= 0) then
           Erreur = retour
           ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.IFIN'
           return
        endif

        if(.not.associated(Etat%ITEM0)) allocate (Etat%ITEM0(size(Model%Connect%OrigineBief)), STAT = retour)
        if (retour /= 0) then
           Erreur = retour
           ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.ITEM0'
           return
        endif

        if(.not.associated(Etat%ZINIT)) allocate (Etat%ZINIT(nb_sect), STAT = retour)
        if (retour /= 0) then
            Erreur = retour
            ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.ZINIT'
            return
        endif
        Etat%ZINIT(:) = W0    ! W0 = 0._DOUBLE
      else
        if(associated(Etat%ZINIT)) deallocate(Etat%ZINIT)
        nullify(Etat%ZINIT)
      endif

   ! Allocation des variables specifiques a Casier

      if (ASSOCIATED(Model%Casiers)) then
         nbCasier = size(Model%Casiers)
      else
         nbCasier = 0
      endif

      Etat%PhaseSimulation = PHASE_INITIALISATION

      if (ASSOCIATED(Model%Liaisons)) then
         nbLiaison = size(Model%Liaisons)
      else
         nbLiaison = 0
      endif

      if(.not.associated(Etat%Liaisons)) allocate (Etat%Liaisons(nbLiaison), STAT = retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.Link'
         return
      endif

      if(.not.associated(Etat%Casiers)) allocate (Etat%Casiers(nbCasier), STAT = retour)
      if (retour /= 0) then
         Erreur = retour
         ptrMsgsErreurs(Identifiant) = 'INIT_ETAT_MASCARET - Unable to allocate State.StoArea'
         return
      endif

      DO i = 1, nbCasier
         Etat%Casiers(i)%Cote        = Model%Casiers(i)%Cote
         Etat%Casiers(i)%Surface     = Model%Casiers(i)%Surface
         Etat%Casiers(i)%Volume      = Model%Casiers(i)%Volume
         Etat%Casiers(i)%VolumeIni   = Model%Casiers(i)%VolumeIni
         Etat%Casiers(i)%Bilan       = Model%Casiers(i)%Bilan
         Etat%Casiers(i)%BilanErreur = Model%Casiers(i)%BilanErreur
         Etat%Casiers(i)%DzCas       = Model%Casiers(i)%DzCas
         Etat%Casiers(i)%CoteMax     = Model%Casiers(i)%CoteMax
         Etat%Casiers(i)%TempsMax    = Model%Casiers(i)%TempsMax
      END DO

      DO i = 1, nbLiaison
         Etat%Liaisons(i)%DebitEchange    = Model%Liaisons(i)%DebitEchange
         Etat%Liaisons(i)%DebitPrecedent  = Model%Liaisons(i)%DebitPrecedent
         Etat%Liaisons(i)%DebitMax        = Model%Liaisons(i)%DebitMax
         Etat%Liaisons(i)%TempsDebitMax   = Model%Liaisons(i)%TempsDebitMax

         Etat%Liaisons(i)%VitesseEchange  = Model%Liaisons(i)%VitesseEchange
         Etat%Liaisons(i)%VitesseMax      = Model%Liaisons(i)%VitesseMax
         Etat%Liaisons(i)%TempsVitesseMax = Model%Liaisons(i)%TempsVitesseMax

         Etat%Liaisons(i)%DQDZamont   = Model%Liaisons(i)%CaracCC%DQDZamont
         Etat%Liaisons(i)%DQDZaval    = Model%Liaisons(i)%CaracCC%DQDZaval
         Etat%Liaisons(i)%DQDZcasier  = Model%Liaisons(i)%CaracRC%DQDZcasier
         Etat%Liaisons(i)%DQDZriviere = Model%Liaisons(i)%CaracRC%DQDZriviere
      END DO

      ptrTabMascaret(Identifiant)%EtatMascaret = Etat

      if (ImpressionLigne) then
          rewind(UniteListing)
          close(UniteListing)
      endif

      Erreur = 0
      RETURN


end subroutine INIT_ETAT_MASCARET



