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

subroutine LEC_LIGNE_LIDO              ( &

           x_ini              , & ! Tableau des abscisses initiales
           z_ini              , & ! Tableau des cotes     initiales
           q_ini              , & ! Tableau des debits    initiaux
           cf1_ini            , & ! Tableau des coefficients de frottement initiales
           cf2_ini            , & ! Tableau des coefficients de frottement initiaux
           PresenceCoeffFrott , & ! test de presence de coefficients de frottement
           FichierLigne       , & ! fichier de la ligne d'eau initiale
           Erreur               &
                                       )

! *********************************************************************
! PROGICIEL : MASCARET       A. LEBOSSE
!                            P. CHERUBINI
!                            S. MANDELKERN
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
!  FONCTION :
!  --------
!
!         LECTURE DE LA LIGNE D'EAU INITIALE ,
!         DES COEFFICIENTS DE FROTTEMENT SI PresenceCoeffFrott est vrai
!
!----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ----------------------
!   SOUS PROGRAMME APPELANT :  LEC_LIGNE
!   -------------------------
!   SOUS PROGRAMMES APPELES :
!   -------------------------
!
!                 - INTERP * INTERPOLATION DE LAGRANGE D'ORDRE N .
!
!   COMMENTAIRES :
!   ------------
!
!
!   **  TypeMaillage = 4 **   LES SECTIONS ONT ETE DEFINIES DANS LE CALCUL
!                       PERMANENT ET SONT LUES DANS LE FICHIER
!                       CONTENANT LA LIGNE D'EAU INITIALE .
!
!***********************************************************************

   !============================= declarations ============================
   use M_PRECISION
   use M_PARAMETRE_C      ! EPS5
   use M_MESSAGE_C        ! messages d'erreur
   use M_ERREUR_T         ! Definition du type ERREUR_T
   use M_FICHIER_T        !                    FICHIER_T
   use M_TRAITER_ERREUR_I

   ! Arguments
   real(DOUBLE), dimension(:), pointer       :: x_ini   ! abscisse lue sur le fichier
   real(DOUBLE), dimension(:), pointer       :: z_ini   ! cote lue sur le fichier
   real(DOUBLE), dimension(:), pointer       :: q_ini   ! debit lu sur le fichier
   real(DOUBLE), dimension(:), pointer       :: cf1_ini ! coeff de frottement mineur lu sur le fichier
   real(DOUBLE), dimension(:), pointer       :: cf2_ini ! coeff de frottement majeur lu sur le fichier
   logical                   , intent(  out) :: PresenceCoeffFrott ! presence dans la ligne d'eau initiale
   type(FICHIER_T)           , intent(in   ) :: FichierLigne
   type(ERREUR_T)            , intent(inout) :: Erreur
   ! Constantes
   character(LEN=4), parameter :: cx    = 'X   '   ! chaine en-tete
   character(LEN=4), parameter :: cz    = 'Z   '   ! chaine en-tete
   character(LEN=4), parameter :: cqtot = 'Q   '   ! chaine en-tete
   character(LEN=4), parameter :: cqmin = 'QMIN'   ! chaine en-tete
   character(LEN=4), parameter :: cqmaj = 'QMAJ'   ! chaine en-tete
   character(LEN=4), parameter :: ccf1  = 'CF1 '   ! chaine en-tete
   character(LEN=4), parameter :: cst1  = 'ST1 '   ! chaine en-tete
   character(LEN=4), parameter :: ccf2  = 'CF2 '   ! chaine en-tete
   character(LEN=4), parameter :: cst2  = 'ST2 '   ! chaine en-tete
   character(LEN=4), parameter :: cfin  = 'FIN '   ! chaine en-tete
   character(LEN=1), parameter :: cegal = '='      ! chaine en-tete
   integer, parameter          :: ORDRE_INTERP = 1 ! ordre d'interpolation
   ! Variables locales
   character(132) :: date
   character(132) :: titcal(10)
   character(132) :: ligne
   integer           :: RETOUR            ! code de retour des fonctions intrinseques
   !character(132)    :: !arbredappel_old   ! arbre dappel precedent
   integer           :: nb_sect          ! nombre de sections de calcul lues
   integer           :: nb_bief          ! nombre de biefs
   integer           :: nb_ligne_entete  ! nombre de lignes d'en-tete
   integer           :: nb_ligne         ! nombre de lignes des tableaux de valeurs
   integer           :: i                ! compteur
   integer           :: ul               ! unite logique du fichier
   character(80)     :: chaine           ! chaine
   logical           :: presence_debit_total
   real(DOUBLE), dimension(:), allocatable :: qmin_ini ! debit mineur lu sur le fichier
   real(DOUBLE), dimension(:), allocatable :: qmaj_ini ! debit majeur lu sur le fichier

   !============================= Instructions ============================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>LEC_LIGNE_LIDO'

   ul = FichierLigne%Unite

   PresenceCoeffFrott = .false.

   !============================
   ! ouverture du fichier a lire
   !============================
   open(unit=ul          , file=FichierLigne%Nom, access='SEQUENTIAL', &
        action='READ'    , form='FORMATTED', iostat=RETOUR      , &
        position='rewind', status='OLD'    )
   if( RETOUR /= 0 ) then
      Erreur%Numero = 3
      Erreur%ft     = err_3
      Erreur%ft_c   = err_3c
      call TRAITER_ERREUR( Erreur , FichierLigne%Nom )
      return
   end if

   ! LECTURE DE LA LIGNE D'EAU INITIALE
   ! ----------------------------------
   read(ul,'(A)',iostat = RETOUR) date
   read(ul,'(A)',iostat = RETOUR) (titcal(i),i=1,2)

   if( RETOUR /= 0 ) then
      Erreur%Numero = 70
      Erreur%ft     = err_70
      Erreur%ft_c   = err_70c
      call TRAITER_ERREUR( Erreur , FichierLigne%Nom )
      return
   end if

   read(ul, '(2(8X,I5))'  ,iostat = RETOUR)  nb_sect , nb_bief
   if( RETOUR /= 0 ) then
      Erreur%Numero = 71
      Erreur%ft     = err_71
      Erreur%ft_c   = err_71c
      call TRAITER_ERREUR( Erreur , FichierLigne%Nom )
      return
   end if

   ! Allocation des tableaux x_ini, z_ini, q_ini, cf1_ini et cf2_ini a nb_sect
   ! -------------------------------------------------------------------------
   if(.not.associated(x_ini)) allocate( x_ini(nb_sect) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'x_ini' )
      return
   end if

   if(.not.associated(z_ini)) allocate( z_ini(nb_sect) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'z_ini' )
      return
   end if

   if(.not.associated(q_ini)) allocate( q_ini(nb_sect) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'q_ini' )
      return
   end if

   if(.not.associated(cf1_ini)) allocate( cf1_ini(nb_sect) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'cf1_ini' )
      return
   end if

   if(.not.associated(cf2_ini)) allocate( cf2_ini(nb_sect) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'cf2_ini' )
      return
   end if

   ! Allocation des tableaux locaux qmin_ini et qmaj_ini a nb_sect
   ! -------------------------------------------------------------
   allocate( qmin_ini(nb_sect) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'qmin_ini' )
      return
   end if

   allocate( qmaj_ini(nb_sect) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'qmaj_ini' )
      return
   end if

   !---------------------
   ! Lecture de l'en-tete
   !---------------------
   nb_ligne_entete = int(( nb_bief - 1 ) / 5 ) + 1

   i = 1
   do while( RETOUR == 0 )
      read(ul,'(A)',iostat = RETOUR) ligne
      if( i == nb_ligne_entete ) then
         exit
      endif
      i = i + 1
   end do

   if( RETOUR /= 0 ) then
      Erreur%Numero = 72
      Erreur%ft     = err_72
      Erreur%ft_c   = err_72c
      call TRAITER_ERREUR( Erreur , i - 1 )
      return
   end if

   ! -----------------------------------------
   ! Calcul du nombre de lignes de chaque bloc
   ! -----------------------------------------
   nb_ligne = int( ( nb_sect - 1 ) / 5 ) + 1

   q_ini(:) = 0._DOUBLE

   presence_debit_total = .false.

   read(ul,'(A4)',iostat = RETOUR) chaine

   !------------------
   ! Boucle de lecture
   !------------------
   do while( RETOUR == 0 )

      if( index(chaine,cfin) /= 0 ) then
         exit
      else if ( index(chaine,cx) /= 0 ) then
         read(ul,*, iostat = RETOUR) ( x_ini(i) , i = 1 , nb_sect )
         if( RETOUR /= 0 ) then
            Erreur%Numero = 73
            Erreur%ft     = err_73
            Erreur%ft_c   = err_73c
            call TRAITER_ERREUR( Erreur , 'abscisses' )
            return
         endif
      else if( index(chaine,cz) /= 0 ) then
         read(ul,*,iostat = RETOUR) ( z_ini(i) , i = 1 , nb_sect )
         if( RETOUR /= 0 ) then
            Erreur%Numero = 73
            Erreur%ft     = err_73
            Erreur%ft_c   = err_73c
            call TRAITER_ERREUR( Erreur , 'cotes' )
            return
         endif
      else if( index(chaine,cqtot) /= 0 ) then
         read(ul,*,iostat = RETOUR) ( q_ini(i) , i = 1 , nb_sect )
         if( RETOUR /= 0 ) then
            Erreur%Numero = 73
            Erreur%ft     = err_73
            Erreur%ft_c   = err_73c
            call TRAITER_ERREUR( Erreur , 'debits totaux' )
            return
         endif

         presence_debit_total = .true.
      else if( index(chaine,cqmin) /= 0 .and. .not. presence_debit_total ) then
         read(ul,*,iostat = RETOUR) (qmin_ini(i),i=1,nb_sect)
         if( RETOUR /= 0 ) then
            Erreur%Numero = 73
            Erreur%ft     = err_73
            Erreur%ft_c   = err_73c
            call TRAITER_ERREUR( Erreur , 'debits mineurs' )
            return
         endif
         q_ini(:) = q_ini(:) + qmin_ini(:)
      else if( index(chaine,cqmaj) /= 0 .and. .not. presence_debit_total ) then
         read(ul,*,iostat = RETOUR) (qmaj_ini(i),i=1,nb_sect)
         if( RETOUR /= 0 ) then
            Erreur%Numero = 73
            Erreur%ft     = err_73
            Erreur%ft_c   = err_73c
            call TRAITER_ERREUR( Erreur , 'debits majeurs' )
            return
         endif
         q_ini(:) = q_ini(:) + qmaj_ini(:)
      else if( index(chaine,cst1) /= 0 .or. index(chaine,ccf1) /= 0 ) then
         PresenceCoeffFrott = .true.
         read(ul,*,iostat = RETOUR) ( cf1_ini(i) , i = 1 , nb_sect )
         if( RETOUR /= 0 ) then
            Erreur%Numero = 73
            Erreur%ft   = err_73
            Erreur%ft_c = err_73c
            call TRAITER_ERREUR( Erreur , 'Coeff de frottement mineurs' )
            return
         endif
      else if( index(chaine,cst2) /= 0 .or. index(chaine,ccf2) /= 0 ) then
         read(ul,*,iostat = RETOUR) ( cf2_ini(i) , i = 1 , nb_sect )
         if( RETOUR /= 0 ) then
            Erreur%Numero = 73
            Erreur%ft     = err_73
            Erreur%ft_c   = err_73c
            call TRAITER_ERREUR( Erreur , 'Coeff de frottement majeurs' )
            return
        endif
      else
         do i = 1 , nb_ligne
            read(ul,'(A)',iostat = RETOUR) ligne
            if( RETOUR /= 0 ) then
               Erreur%Numero = 74
               Erreur%ft   = err_74
               Erreur%ft_c = err_74c
               call TRAITER_ERREUR  (Erreur, chaine, i)
               return
            endif
         end do
      endif

      read(ul,'(A)',iostat = RETOUR) chaine

   end do

   if( RETOUR /= 0 ) then
      Erreur%Numero = 75
      Erreur%ft     = err_75
      Erreur%ft_c   = err_75c
      call TRAITER_ERREUR( Erreur , i )
      return
   endif

   ! De-allocation des tableaux cf1_ini et cf2_ini si PresenceCoeffFrott est faux
   ! ----------------------------------------------------------------------------
   if( .not. PresenceCoeffFrott ) then
      if(associated(cf1_ini)) deallocate( cf1_ini , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 6
         Erreur%ft     = err_6
         Erreur%ft_c   = err_6c
         call TRAITER_ERREUR( Erreur , 'cf1_ini' )
         return
      end if

      if(associated(cf2_ini)) deallocate( cf2_ini , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 6
         Erreur%ft     = err_6
         Erreur%ft_c   = err_6c
         call TRAITER_ERREUR( Erreur , 'cf2_ini' )
         return
      end if
   endif

   ! De-allocation des tableaux locaux qmin_ini et qmaj_ini
   ! ------------------------------------------------------
   deallocate( qmin_ini , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 6
      Erreur%ft     = err_6
      Erreur%ft_c   = err_6c
      call TRAITER_ERREUR( Erreur , 'qmin_ini' )
      return
   end if

  deallocate( qmaj_ini , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 6
      Erreur%ft     = err_6
      Erreur%ft_c   = err_6c
      call TRAITER_ERREUR( Erreur , 'qmaj_ini' )
      return
   end if

   !----------------------
   ! Fin du sous-programme
   !----------------------

   close(ul)

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine LEC_LIGNE_LIDO
