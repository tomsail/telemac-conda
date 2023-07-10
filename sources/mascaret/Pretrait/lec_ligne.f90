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

subroutine LEC_LIGNE( &
                      Z , & ! Cote initiale
                      Q , & ! debit initial
                    CF1 , & ! Coefficient de frottement mineur
                    CF2 , & ! Coefficient de frottement majeur
                      X , & ! maillage
        TypeEntreeLigne , & ! format LIDO/OPTHYCA
           FichierLigne , & ! fichier l.e. initial
            FormatLigne , & ! format LIDO/OPTHYCA
           TypeMaillage , & ! Mode de calcul du maillage
        ImpressionLigne , & ! test d'impression
           UniteListing , & ! Unite logique fichier listing
                 Profil , & ! Profils geometriques
               Prof_Abs , & ! Abscisse absolue
            ProfDebBief , & ! Premiers profils des biefs
            ProfFinBief , & ! Derniers profils des biefs
          AbsRelDebBief , & ! Abscisse relative de debut de bief
          AbsRelFinBief , & ! Abscisse relative de fin de bief
                 Erreur ) ! Erreur

! *********************************************************************
! PROGICIEL : MASCARET       S. MANDELKERN
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
!
!
!  FONCTION : LECTURE DU FICHIER DE LA LIGNE D'EAU INITIALE
!  --------
!             INTERPOLATION AUX SECTIONS DE CALCUL SI NECESSAIRE
!             SI TypeMaillage = TYPE_MAILLAGE_PRECEDENT :
!             DEFINITION DES ABSCISSES DES SECTIONS DE CALCUL .
!
!----------------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :       - Fichier listing (UniteListing)
!   ----------------------
!
!   SOUS-PROGRAMME(S) APPELANT(S) :  - LECDON
!   -----------------------------
!   SOUS-PROGRAMME(S) APPELE(S)   :  - LEC_LIGNE_OPT
!   ---------------------------      - LEC_LIGNE_LIDO
!                                    - INTERPOLATION_S
!                                    - ABS_ABS_S
!
!   COMMENTAIRES :
!   ------------
!
!   DOCUMENTATION EXTERNE :
!   ---------------------
!
!***********************************************************************

   !============================= Declarations ============================
   use M_PRECISION
   use M_FICHIER_T            ! Definition du type FICHIER_T et UniteListing
   use M_ERREUR_T             ! Definition du type ERREUR_T
   use M_PROFIL_T             ! Type  PROFIL_T
   use M_PARAMETRE_C
   use M_MESSAGE_C            ! Definition des messages d'erreur
   use M_CONSTANTES_CALCUL_C  ! Definition des formats de fichiers de sortie
   use M_LEC_LIGNE_LIDO_I
   use M_LEC_LIGNE_OPT_I
   use M_INTERPOLATION_S       ! Interface du sous programme INTERPOLATION_S
   use M_ABS_ABS_S             ! Calcul de l'abscisse absolue
   use M_TRAITER_ERREUR_I      ! Traitement des erreurs

   !.. Declarations explicites ..
   implicit none

   type SECTION_REL_T
      sequence
      integer      :: Branche     ! Numero de branche
      real(DOUBLE) :: AbscisseRel ! Abscisse relative
   end type SECTION_REL_T

   !.. Arguments ..
   real(DOUBLE), dimension(:)   , pointer       :: X
   real(DOUBLE), dimension(:)   , pointer       :: Z
   real(DOUBLE), dimension(:)   , pointer       :: Q
   real(DOUBLE), dimension(:)   , pointer       :: CF1
   real(DOUBLE), dimension(:)   , pointer       :: CF2
   integer                      , intent(in   ) :: TypeEntreeLigne
   type(FICHIER_T)              , intent(in   ) :: FichierLigne
   integer                      , intent(in   ) :: FormatLigne
   integer                      , intent(in   ) :: TypeMaillage
   logical                      , intent(in   ) :: ImpressionLigne
   logical                      , intent(in   ) :: Prof_Abs
   integer                      , intent(in   ) :: UniteListing
   type(PROFIL_T), dimension(:) , intent(in   ) :: Profil
   integer       , dimension(:) , intent(in   ) :: ProfDebBief
   integer       , dimension(:) , intent(in   ) :: ProfFinBief
   real(DOUBLE)  , dimension(:) , intent(in   ) :: AbsRelDebBief
   real(DOUBLE)  , dimension(:) , intent(in   ) :: AbsRelFinBief
   type(ERREUR_T)               , intent(inout) :: Erreur
   integer, parameter :: ORDRE_INTERP = 1  ! ordre d'interpolation
   ! Variables locales
   real(DOUBLE)    , dimension(:), pointer :: x_ini => null()     ! maillage lu
   real(DOUBLE)    , dimension(:), pointer :: x_ini_abs => null() ! maillage lu en absolu
   real(DOUBLE)    , dimension(:), pointer :: z_ini => null()     ! cote lu
   real(DOUBLE)    , dimension(:), pointer :: q_ini => null()     ! debit lu
   real(DOUBLE)    , dimension(:), pointer :: cf1_ini => null()   ! Coeff frottement majeur lu
   real(DOUBLE)    , dimension(:), pointer :: cf2_ini => null()   ! Coeff frottement majeur lu
   !character(132) :: !arbredappel_old
   integer      :: i,j, ideb_bief, ifin_bief  ! compteurs
   integer      :: nb_sect              ! nombre de sections de calculs
   logical      :: interpoler           ! test necessite interpolatione
   integer      :: RETOUR               ! code de retour des fonctions intrinseques
   real(DOUBLE) :: sigma                ! somme des differences entre deux maillages
   logical      :: presence_coeff_frott ! test de presence du coeff de frottement

   !============================ Instructions ==============================
   ! INITIALISATION
   ! --------------
   Erreur%Numero      = 0
   retour = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>LEC_LIGNE'

   !====================================
   ! LECTURE OU SAISIE DE LA LIGNE D'EAU
   !====================================
   if( TypeEntreeLigne == SAISIE_PAR_FICHIER ) then
      if( FormatLigne == FORMAT_STO_OPTHYCA ) then
         call LEC_LIGNE_OPT            (   &
                    x_ini                , & ! Tableau des abscisses initiales
                    z_ini                , & ! Tableau des cotes     initiales
                    q_ini                , & ! Tableau des debits    initiaux
                    cf1_ini              , & ! Tableau des coeff de frottement
                    cf2_ini              , & ! Tableau des coeff de frottement
                    presence_coeff_frott , & ! test de presence du coefficient de frottement
                    FichierLigne         , & ! fichier de la ligne d'eau initiale
                    Erreur               &
                                       )
         if( Erreur%Numero /= 0 ) then
            return
         end if

         if(.not.associated(x_ini_abs)) allocate( x_ini_abs(size(x_ini)) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'x_ini_abs' )
            return
         end if

         if( (.not.Prof_Abs) .and. ( TypeMaillage /= TYPE_MAILLAGE_PRECEDENT ) ) then

            ideb_bief = 1

            do j = 1 , size(AbsRelDebBief)

               i= ideb_bief

               do while ( abs(x_ini(i)-AbsRelFinBief(j)).GT.EPS6)
                  i = i + 1
               end do

               ifin_bief = i

               do i = ideb_bief, ifin_bief

                  x_ini_abs(i) =  ABS_ABS_S     ( &
                                 j              , &
                                 x_ini(i)       , &
                                 Profil         , &
                                 ProfDebBief    , &
                                 ProfFinBief    , &
                                 Erreur           )

                  if( Erreur%Numero /= 0 ) then
                     return
                  end if

               end do

               ideb_bief = ifin_bief + 1

            end do

            do i = 1, size(x_ini)
               x_ini(i) = x_ini_abs(i)
            end do

         end if

      else if( FormatLigne == FORMAT_STO_PERMANENT ) then

         call LEC_LIGNE_LIDO                ( &
                       x_ini                , & ! Tableau des abscisses initiales
                       z_ini                , & ! Tableau des cotes     initiales
                       q_ini                , & ! Tableau des debits    initiaux
                       cf1_ini              , & ! Tableau des coeff de frottement
                       cf2_ini              , & ! Tableau des coeff de frottement
                       presence_coeff_frott , & ! test de presence du coeff de frottement
                       FichierLigne         , & ! fichier de la ligne d'eau initiale
                       Erreur               &
                                           )
         if( Erreur%Numero /= 0 ) then
            return
         end if

         if(.not.associated(x_ini_abs)) allocate( x_ini_abs(size(x_ini)) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'x_ini_abs' )
            return
         end if

         if( (.not.Prof_Abs) .and. ( TypeMaillage /= TYPE_MAILLAGE_PRECEDENT ) ) then

            ideb_bief = 1

            do j = 1 , size(AbsRelDebBief)

               i = ideb_bief

               do while( dabs( x_ini(i) - AbsRelFinBief(j) ) >= 0.0001_DOUBLE )
                  i = i + 1
               end do

               ifin_bief = i

               do i = ideb_bief, ifin_bief

                  x_ini_abs(i) =  ABS_ABS_S     ( &
                                 j              , &
                                 x_ini(i)       , &
                                 Profil         , &
                                 ProfDebBief    , &
                                 ProfFinBief    , &
                                 Erreur           )

                  if( Erreur%Numero /= 0 ) then
                     return
                  end if

               end do

               ideb_bief = ifin_bief + 1

            end do

            do i = 1 , size(x_ini)
               x_ini(i) = x_ini_abs(i)
            end do

         end if

      end if

   endif

   !===============================================
   ! ALLOCATION DES POINTEURS AUX BONNES DIMENSIONS
   !===============================================
   ! -----------------------------------------
   ! SI TypeMaillage = TYPE_MAILLAGE_PRECEDENT
   ! -----------------------------------------
   if( TypeMaillage == TYPE_MAILLAGE_PRECEDENT ) then
      !----------------------------------------------------
      ! la taille des tableaux est celle lue sur le fichier
      !----------------------------------------------------
      nb_sect = size(x_ini)

      if(.not.associated(X)) allocate( X(nb_sect) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'X' )
         return
      end if
   else     ! de if typeMaillage
     !-----------------------------------------------
     ! sinon la taille est celle du tableau X initial
     !-----------------------------------------------
      nb_sect = size(X)
   end if   ! de if TypeMaillage

   !-----------------------------------------------------
   ! Allocation des tableaux Z, Q et CF1 et CF2 a nb_sect
   !-----------------------------------------------------
   if(.not.associated(Z)) allocate( Z(nb_sect) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'Z' )
      return
   end if

   if(.not.associated(Q)) allocate( Q(nb_sect) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'Q' )
      return
   end if

   ! Allocation eventuelle des tableaux CF1 et CF2
   !----------------------------------------------
   if( TypeMaillage == TYPE_MAILLAGE_PRECEDENT .and. presence_coeff_frott ) then

      if(.not.associated(CF1)) allocate( CF1(nb_sect) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'CF1' )
         return
      end if

      if(.not.associated(CF2)) allocate( CF2(nb_sect) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'CF2' )
         return
      end if

   endif

   !========================
   ! AFFECTATION DES VALEURS
   !========================
   ! -----------------------------------------
   ! SI TypeMaillage = TYPE_MAILLAGE_PRECEDENT
   ! -----------------------------------------
   if( TypeMaillage == TYPE_MAILLAGE_PRECEDENT ) then
      !-------------------------------------------
      ! Affectation des valeurs au tableaux finaux
      !-------------------------------------------

      X(:) = x_ini(:)
      Z(:) = z_ini(:)
      Q(:) = q_ini(:)

      if( presence_coeff_frott ) then

         CF1(:) = cf1_ini(:)
         CF2(:) = cf2_ini(:)

      endif

      !----------------------------------------------
      ! ECRITURE DES ABSCISSES DES SECTIONS DE CALCUL
      !----------------------------------------------
      write (UniteListing ,2000) nb_sect
      write (UniteListing ,2001)
      write (UniteListing ,'(10f8.2)') (X(i),i=1,nb_sect)

      ! -------------------------------------------------
      ! SI TypeMaillage n'est pas TYPE_MAILLAGE_PRECEDENT
      ! -------------------------------------------------
   else    ! de if typeMaillage

      !---------------------------------------------------
      ! TEST SI BESOIN D'INTERPOLER AUX SECTIONS DE CALCUL
      !---------------------------------------------------
      nb_sect = size(X)

      interpoler = .false.

      if( nb_sect == size(x_ini) ) then

         sigma = 0._DOUBLE

         do i = 1 , nb_sect
            sigma = sigma + ( X(i) - x_ini(i) )
         end do

         if( dabs(sigma) >= EPS5 ) then
            !PRINT * , 'abs(sigma)' , dabs(sigma)
            ! Il faut interpoler
            interpoler = .true.
         endif

      else
         ! Il faut interpoler
         interpoler = .true.
      endif

      !-----------------------------------------------------
      ! LE MAILLAGE EST DIFFERENT DE CELUI DE LA LIGNE D'EAU
      ! NECESSITE D'INTERPOLER AUX SECTIONS DE CALCUL
      !-----------------------------------------------------
      if( interpoler ) then

         boucle_d_interpolation : do i = 1 , nb_sect

            !         ====================
            CALL INTERPOLATION_S                 ( &
            !         ====================
                                     Z(I)         , &
                                     X(I)         , &
                                     ORDRE_INTERP , &
                                     x_ini        , &
                                     z_ini        , &
                                     size(x_ini)  , & ! taille de x_ini
                                     Erreur       )
            if( Erreur%Numero /= 0 ) then
               return
            end if

            !         ====================
            CALL INTERPOLATION_S                 ( &
            !         ====================
                                    Q(I)         , &
                                    X(I)         , &
                                    ORDRE_INTERP , &
                                    x_ini        , &
                                    q_ini        , &
                                    size(x_ini)  , & ! taille de x_ini
                                    Erreur       )

            if( Erreur%Numero /= 0 ) then
               return
            end if

         end do boucle_d_interpolation

         !----------------------------------------------------
         ! LE MAILLAGE EST LE MEME QUE CELUI DE LA LIGNE D'EAU
         ! PAS BESOIN D'INTERPOLER AUX SECTIONS DE CALCUL
         !----------------------------------------------------
      else  ! de if interpoler

         Z(:) = z_ini(:)
         Q(:) = q_ini(:)
         if( presence_coeff_frott.and.TypeMaillage == TYPE_MAILLAGE_PRECEDENT ) then
            CF1(:) = cf1_ini(:)
            CF2(:) = cf2_ini(:)
         endif

      endif  ! de if interpoler

   endif    ! de if typeMaillage

   !------------------------------------
   ! ECRITURE DE LA LIGNE D'EAU INITIALE
   !------------------------------------
   if( ImpressionLigne ) then

      write( UniteListing , 2003 )

      if( interpoler ) then
         write(UniteListing ,2005) nb_sect
      endif

      write(UniteListing ,2006)
      write(UniteListing ,2007) ( Z(i) , i = 1 , nb_sect )

      write(UniteListing ,2008)
      write(UniteListing ,2009) ( Q(i) , i = 1 , nb_sect )

      if( presence_coeff_frott .and. TypeMaillage == TYPE_MAILLAGE_PRECEDENT ) then
        write(UniteListing ,2010)
        write(UniteListing ,2011)
        write(UniteListing ,2009) ( CF1(i) , i = 1 , nb_sect )
        write(UniteListing ,2012)
        write(UniteListing ,2009) ( CF2(i) , i = 1 , nb_sect )
      endif

   endif

   ! ------------------------------------------------------
   ! De-allocation des tableaux locaux x_ini, z_ini, q_ini,
   ! cf1_ini et cf2_ini a nb_sect
   ! ------------------------------------------------------
   if(associated(x_ini)) deallocate( x_ini , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 6
      Erreur%ft     = err_6
      Erreur%ft_c   = err_6c
      call TRAITER_ERREUR( Erreur , 'x_ini' )
      return
   end if

   if(associated(z_ini)) deallocate( z_ini , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 6
      Erreur%ft     = err_6
      Erreur%ft_c   = err_6c
      call TRAITER_ERREUR( Erreur , 'z_ini' )
      return
   end if

   if(associated(q_ini)) deallocate( q_ini , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 6
      Erreur%ft     = err_6
      Erreur%ft_c   = err_6c
      call TRAITER_ERREUR( Erreur , 'q_ini' )
      return
   end if

   ! De-allocation des tableaux cf1_ini et cf2_ini si presence_coeff_frott est vrai
   ! ----------------------------------------------------------------------------
   if( presence_coeff_frott ) then

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

   end if

   !----------------------
   ! Fin du sous-programme
   !----------------------

   !Erreur%arbredappel = !arbredappel_old

   return

   !---------------------
   ! FORMATS DES MESSAGES
   !---------------------

 2000 FORMAT(/,'NOMBRE DE SECTIONS DE CALCUL = ',I5,/)
 2001 FORMAT('ABSCISSES DES SECTIONS DE CALCUL :')

 2003 FORMAT(/,'LIGNE D''EAU INITIALE ',/,20('-'),/)
 2005 FORMAT('INTERPOLATION DE LA LIGNE D''EAU A PARTIR DES',       &
             I5,' POINTS DE DONNEES',/)
 2006 FORMAT('COTES DU PLAN D''EAU , ( Z(I) , I=1,nb_sect )')
 2007 FORMAT(10F8.3)
 2008 FORMAT(/,'DEBITS               , ( Q(I) , I=1,nb_sect )')
 2009 FORMAT(10F8.2)
 2010 FORMAT(/, &
  'ATTENTION: SEULS LES COEFFICIENTS DE FROTTEMENT LUS DANS LE FICHIER',/,&
             'DE LIGNE D''EAU INITIALE SONT PRIS EN COMPTE.',/)
 2011 FORMAT('COEFFICIENTS DE FROTTEMENT DU LIT MINEUR : ( CF1(I) ,' &
               ,  ' I=1,nb_sect ) ')
 2012 FORMAT('COEFFICIENTS DE FROTTEMENT DU LIT MAJEUR : ( CF2(I) ,' &
               ,  ' I=1,nb_sect ) ')

end subroutine LEC_LIGNE
