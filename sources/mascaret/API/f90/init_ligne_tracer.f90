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
subroutine INIT_LIGNE_TRACER(Erreur, Identifiant, C, Taille, NbTrac, Impression)
   use M_MASCARET_T
   use M_APIMASCARET_STATIC
   use M_MODELE_MASCARET_T   ! Type MODELE_MASCARET_T
   use M_ETAT_MASCARET_T     ! Type ETAT_MASCARET_T
   use M_CONSTANTES_CALCUL_C
   use M_PRECISION                 ! Definition de la precision DOUBLE ou SIMPLE
   use M_PARAMETRE_C
   use M_LEC_CONC_INI_TRACER_INTERFACE_I

   implicit none

   integer, intent(out)                         :: Erreur         ! different de 0 si erreur
   integer, intent(in )                         :: Identifiant    ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   integer, intent(in )                         :: Taille         ! Taille de la ligne
   integer, intent(in )                         :: NbTrac         ! Nombre de traceurs dans l'entree
   real(8), dimension(Taille,NbTrac), intent(in):: C              ! Tableau des concentrations initiales
   integer, intent(in )                         :: Impression     ! impression sur les fichiers listing (1-> Vrai 0-> Faux)

   !-- Scalaires locaux
   type(MODELE_MASCARET_T)   :: Modele
   type(MASCARET_T), pointer :: Masc
   integer                   :: ult ! Unite listing
   integer                   :: ultrac ! Unite listing tracer
   integer :: nb_sect, nb_bief, nb_trac
   character(LEN=256)        :: MessageErreur

   Erreur = TEST_INIT_AND_ID(Identifiant, 'INIT_LIGNE_TRACER')
   if (Erreur > 0 ) then
      return
   end if
   Masc => ptrTabMascaret(Identifiant)
   Modele = Masc%ModeleMascaret

   if (.not.Modele%OptionTracer) then
       Erreur = 1
       ptrMsgsErreurs(Identifiant) = 'Tracer option is not set'
       return
   endif

   if (impression .ne. 0) then
      ult = Modele%FichierListing%Unite
      ultrac = Modele%Tracer%FichierListingTracer%Unite
      open(unit=ult, file=Modele%FichierListing%Nom, access='SEQUENTIAL', &
         action='WRITE'           , form='FORMATTED'       , iostat=Erreur      , &
         position='append')
         if (Erreur /= 0) then
             ptrMsgsErreurs(Identifiant) = "Error opening the listing files"
             return
         endif
      open(unit=ultrac, file=Modele%Tracer%FichierListingTracer%Nom, access='SEQUENTIAL', &
         action='WRITE'           , form='FORMATTED'       , iostat=Erreur      , &
         position='append')
         if (Erreur /= 0) then
             ptrMsgsErreurs(Identifiant) = "Error opening the Tracer listing files"
             return
         endif
   else
      Modele%Tracer%ImpressionConcListing = .false.
      Modele%Tracer%ImpressionConcIni     = .false.
      ult = -1
      ultrac = -1
   endif

   nb_sect = size(Modele%X(:))
   nb_bief = size(Modele%Connect%OrigineBief)
   nb_trac = Modele%Tracer%nbtrac

   if (taille /= nb_sect) then
      Erreur = 2
      ptrMsgsErreurs(Identifiant) = 'INIT_LIGNE_TRACER - The numbers of nodes are different'
      return
   endif

   if (nbtrac /= nb_trac) then
      Erreur = 2
      ptrMsgsErreurs(Identifiant) = 'INIT_LIGNE_TRACER - The numbers of tracers are different'
      return
   endif


   Erreur = DESALLOUE_ETAT_TRACER(Masc%EtatMascaret%Tracer, MessageErreur)
   if (Erreur > 0 ) then
       ptrMsgsErreurs(Identifiant) = MessageErreur
       return
   end if

   allocate( Masc%EtatMascaret%Tracer%Ctraceur(nb_sect,Modele%Tracer%Nbtrac) , STAT = Erreur )
   if( Erreur /= 0 ) return
   Masc%EtatMascaret%Tracer%Ctraceur(:,:) = C(:,:)

   allocate( Masc%EtatMascaret%Tracer%QT(nb_sect) , STAT = Erreur )
   if( Erreur /= 0 ) call err_alloc_tr('QT')
   Masc%EtatMascaret%Tracer%QT(:) = W0    ! W0 = 0._DOUBLE

   allocate( Masc%EtatMascaret%Tracer%ST(nb_sect) , STAT = Erreur )
   if( Erreur /= 0 ) call err_alloc_tr('ST')
   Masc%EtatMascaret%Tracer%ST(:) = W0    ! W0 = 0._DOUBLE

   allocate( Masc%EtatMascaret%Tracer%BT(nb_sect) , STAT = Erreur )
   if( Erreur /= 0 ) call err_alloc_tr('BT')
   Masc%EtatMascaret%Tracer%BT(:) = W0    ! W0 = 0._DOUBLE

   allocate( Masc%EtatMascaret%Tracer%QT_ANT(nb_sect) , STAT = Erreur )
   if( Erreur /= 0 ) call err_alloc_tr('QT_ANT')
   Masc%EtatMascaret%Tracer%QT_ANT(:) = W0    ! W0 = 0._DOUBLE

   allocate( Masc%EtatMascaret%Tracer%ST_ANT(nb_sect) , STAT = Erreur )
   if( Erreur /= 0 ) call err_alloc_tr('ST_ANT')
   Masc%EtatMascaret%Tracer%ST_ANT(:) = W0    ! W0 = 0._DOUBLE

   allocate( Masc%EtatMascaret%Tracer%BT_ANT(nb_sect) , STAT = Erreur )
   if( Erreur /= 0 ) call err_alloc_tr('BT_ANT')
   Masc%EtatMascaret%Tracer%BT_ANT(:) = W0    ! W0 = 0._DOUBLE

   allocate( Masc%EtatMascaret%Tracer%NbCourant(nb_bief) , STAT = Erreur )
   if( Erreur /= 0 ) call err_alloc_tr('NbCourant')
   Masc%EtatMascaret%Tracer%NbCourant(:) = W0    ! W0 = 0._DOUBLE

   allocate( Masc%EtatMascaret%Tracer%MASS(nb_bief,Modele%Tracer%Nbtrac) , STAT = Erreur )
   if( Erreur /= 0 ) call err_alloc_tr('MASS')
   Masc%EtatMascaret%Tracer%MASS(:,:) = W0    ! W0 = 0._DOUBLE

   allocate( Masc%EtatMascaret%Tracer%FLUMAS(nb_bief,Modele%Tracer%Nbtrac) , STAT = Erreur )
   if( Erreur /= 0 ) call err_alloc_tr('FLUMAS')
   Masc%EtatMascaret%Tracer%FLUMAS(:,:) = W0    ! W0 = 0._DOUBLE

   allocate( Masc%EtatMascaret%Tracer%FLUENT(nb_bief,Modele%Tracer%Nbtrac) , STAT = Erreur )
   if( Erreur /= 0 ) call err_alloc_tr('FLUENT')
   Masc%EtatMascaret%Tracer%FLUENT(:,:) = W0    ! W0 = 0._DOUBLE

   allocate( Masc%EtatMascaret%Tracer%FLUSOR(nb_bief,Modele%Tracer%Nbtrac) , STAT = Erreur )
   if( Erreur /= 0 ) call err_alloc_tr('FLUSOR')
   Masc%EtatMascaret%Tracer%FLUSOR(:,:) = W0    ! W0 = 0._DOUBLE

   allocate( Masc%EtatMascaret%Tracer%FLUSRC(nb_bief,Modele%Tracer%Nbtrac) , STAT = Erreur )
   if( Erreur /= 0 ) call err_alloc_tr('FLUSRC')
   Masc%EtatMascaret%Tracer%FLUSRC(:,:) = W0    ! W0 = 0._DOUBLE


   if (Impression .ne. 0) then
      close(ult)
      close(ultrac)
   endif

   Erreur = 0
   return

contains

   subroutine err_alloc_tr(name)
      use M_ERREUR_T               ! Type ERREUR_T
      use M_MESSAGE_C              ! Messages d'erreur
      use M_TRAITER_ERREUR_I       ! Traitement de l'errreur
      character(len=*) :: name
      type(ERREUR_T)   :: Erreur
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , name )
      write(*,321)
      print * , Erreur%Message
      stop 1
      return
321   format(/,"===========",/,"=> ERROR <=",/,"===========",/)
   end subroutine err_alloc_tr


end subroutine INIT_LIGNE_TRACER
