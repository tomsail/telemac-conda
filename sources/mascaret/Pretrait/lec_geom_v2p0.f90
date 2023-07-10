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

subroutine LEC_GEOM_V2P0( &
                     Profil , & ! Profil de donnees geometriques
        FrottParoiVerticale , & ! Flag
                    Fichier , & ! Fichier des profils
               UniteListing , & ! Unite logique fichier listing
                     Erreur ) ! Erreur

! *********************************************************************
! PROGICIEL : MASCARET        P. CHERUBINI
!                             A. LEBOSSE
!                             S. MANDELKERN
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
!  FONCTION :
!  --------
!
!       LECTURE DES DONNEES GEOMETRIQUES (PROFILS EN TRAVERS)
!       AU FORMAT LIDO V2P0
!       CONTROLES
!-----------------------------------------------------------------------
!
!   FICHIERS  ENTREE/SORTIE :
!   ------------------------
!
!                UL       : FICHIER GEOMETRIE
!                UniteListing   : IMPRESSION DU LISTING
!
!   SOUS PROGRAMME APPELANT :  LEC_GEOM
!   -------------------------
!   SOUS PROGRAMMES APPELES :  ---
!   -------------------------
!
!   COMMENTAIRES :
!   --------------
!
!***********************************************************************

   !=========================== Declarations ================================
   use M_PRECISION
   use M_PARAMETRE_C ! INFINI
   use M_PROFIL_T    ! Format PROFIL_T
   use M_FICHIER_T   ! Format FICHIER_T et UniteListing
   use M_ERREUR_T    ! Format ERREUR_T
   use M_TRAITER_ERREUR_I
   use M_MESSAGE_C

   !.. Implicit Declarations ..
   implicit none

   !.. Formal Arguments ..
   type(PROFIL_T), dimension(:), pointer       :: Profil   ! profils
   type(FICHIER_T)             , intent(in   ) :: Fichier  ! fichier des profils
   logical                     , intent(in   ) :: FrottParoiVerticale ! test de frott<ement
   integer                     , intent(in   ) :: UniteListing
   type(ERREUR_T)              , intent(inout) :: Erreur   ! Erreur
   !.. Local Scalars ..
   character(LEN=60) :: LIBEL
   character(LEN=72) :: LIGNE
   integer           :: reponse
   integer       :: nb_point     ! nombre de points d'un profil
   integer       :: i,j,k        ! Compteurs
   integer       :: ibief        ! Compteur
   integer       :: nb_bief      ! Nombre de biefs
   integer       :: nb_profil    ! nombre de profils
   integer       :: prof_deb         ! profil de debut de bief
   integer       :: prof_fin         ! profil de fin de bief
   integer       :: ul               ! numero de l'unite logique du fichier geometrie
   real(DOUBLE)  :: x_max, x_min     ! points extremes d'un profil
   integer       :: type_profil      ! type d'entree des profil : point/largeur
   integer       :: retour           ! code de retour des fonctions intrinseques
   logical       :: paroi_verticale  ! test de presence de parois verticales
   integer       :: iparoi           ! compteur de parois verticales
   real(DOUBLE), dimension(2)  :: limite_min       ! limite gauche lit mineur
   real(DOUBLE), dimension(2)  :: limite_maj       ! limite droite lit mineur
   real(DOUBLE)  :: dbief            ! dbief
   !character(132) :: !arbredappel_old ! ancien arbre
   !.. Constantes ..
   integer :: TYPE_PROFIL_POINT   = 1
   integer :: TYPE_PROFIL_LARGEUR = 2
   !.. Local Arrays ..
   integer          , dimension(50) :: profil_1 ! premier profil d'un bief

   !.. Intrinsic Functions ..
   intrinsic MAX, MIN

   !========================== Instructions =============================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   retour = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>LEC_GEOM_V2P0'

   !  OUVERTURE DU FICHIER
   !  --------------------
   ! Ouverture du fichier a lire
   open(unit=Fichier%Unite, file=Fichier%Nom, access='SEQUENTIAL', &
        action='READ'      , form='FORMATTED' , iostat=RETOUR    , &
        position='rewind'  , status='OLD'     )
   if( RETOUR /= 0 ) then
      Erreur%Numero = 3
      Erreur%ft     = err_3
      Erreur%ft_c   = err_3c
      call TRAITER_ERREUR( Erreur , fichier%Nom )
      return
   end if

   ul = Fichier%Unite

   !  LECTURE DU TITRE
   !  ----------------
   read( ul,'(A)',iostat = RETOUR ) ( LIGNE , I = 1,6 )
   read(ul,*,iostat = RETOUR) LIGNE , type_profil
   read(ul,*,iostat = RETOUR) LIGNE , reponse
   read(ul,*,iostat = RETOUR) LIGNE , nb_profil
   if( RETOUR /= 0 ) then
      Erreur%Numero = 50
      Erreur%ft     = err_50
      Erreur%ft_c   = err_50c
      call TRAITER_ERREUR( Erreur , fichier%Nom )
      return
   end if

   if(.not.associated(Profil)) allocate(Profil(nb_profil),STAT=retour)
   if( RETOUR /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'Profil' )
      return
   end if

   read(ul,'(A)',iostat = RETOUR) LIGNE

   !===================================================
   !        L E C T U R E   D E S   P R O F I L S
   !===================================================
   if( type_profil == TYPE_PROFIL_POINT ) then
      !  =========================
      !  ProfilS ENTRES PAR POINTS
      !  =========================
      loop_1 : do J = 1 , nb_profil
         read(ul,*,iostat = RETOUR) Profil(J)%Nom(1:12) , LIBEL , Profil(J)%AbsRel
         read(ul,*,iostat = RETOUR) LIBEL , (limite_min(K),K=1,2) , LIBEL , Profil(J)%CoeffFrottMin
         read(ul,*,iostat = RETOUR) LIBEL , (limite_maj(K),K=1,2) , LIBEL , Profil(J)%CoeffFrottMaj
         read(ul,*,iostat = RETOUR) LIBEL , Profil(J)%ZRive(1) , Profil(J)%ZRive(2)
         read(ul,*,iostat = RETOUR) LIBEL , nb_point
         if( RETOUR /= 0 ) then
            Erreur%Numero = 51
            Erreur%ft     = err_51
            Erreur%ft_c   = err_51c
            call TRAITER_ERREUR( Erreur , fichier%Nom , J )
            return
         end if

         if(.not.associated(Profil(J)%X)) allocate( Profil(J)%X(nb_point) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'Profil%X' )
            return
         end if

         if(.not.associated(Profil(J)%Y)) allocate( Profil(J)%Y(nb_point) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'Profil%Y' )
            return
         end if

         read(ul,'(A)',iostat = RETOUR) LIBEL
         read(ul,*,iostat = RETOUR) ( Profil(J)%X(I) , I = 1 , nb_point )

         if( retour /= 0 ) then
            Erreur%Numero = 52
            Erreur%ft     = err_52
            Erreur%ft_c   = err_52c
            call TRAITER_ERREUR( Erreur , 'abscisses' , J )
            return
         end if

         read(ul,'(A)',iostat = RETOUR) LIBEL
         read(ul,*,iostat = RETOUR) ( Profil(J)%Y(I) , I = 1 , nb_point )

         if( retour /= 0 ) then
            Erreur%Numero = 52
            Erreur%ft     = err_52
            Erreur%ft_c   = err_52c
            call TRAITER_ERREUR( Erreur , 'ordonnees' , J )
            return
         end if

         read( ul ,'(A)' ) LIGNE

         !------------------------------------------
         ! Calcul des points limites des lits mineur
         !------------------------------------------
         do i = 2 , nb_point - 1
            if( abs(Profil(j)%X(i)-limite_min(1)).lt.EPS15 ) then
               Profil(j)%LimiteMin(1) = i
               exit
            end if
         end do

         do i = 2 , nb_point - 1
            if( abs(Profil(j)%X(i)-limite_min(2)).lt.EPS15 ) then
               Profil(j)%LimiteMin(2) = i
            end if
         end do

         !------------------------------------------
         ! Calcul des points limites des lits majeur
         !------------------------------------------
         do i = 2 , nb_point - 1
            if( abs(Profil(j)%X(i)-limite_maj(1)).lt.EPS15 ) then
               Profil(j)%LimiteMaj(1) = i
               exit
            end if
         end do

         do i = 2 , nb_point - 1
            if( abs(Profil(j)%X(i)-limite_maj(2)).lt.EPS15 ) then
               Profil(j)%LimiteMaj(2) = i
            end if
         end do

         ! CONTROLE DES COEFFICIENTS DE RUGOSITE
         ! -------------------------------------
         if( Profil(J)%CoeffFrottMin <= 0._DOUBLE .or. Profil(J)%CoeffFrottMaj <= 0._DOUBLE ) then
            Erreur%Numero = 53
            Erreur%ft     = err_53
            Erreur%ft_c   = err_53c
            call TRAITER_ERREUR( Erreur , J )
            return
         end if

         x_min = INFINI
         x_max = - INFINI

         do I = 1 , nb_point
            x_min = DMIN1( x_min , Profil(J)%X(I) )
            x_max = DMAX1( x_max , Profil(J)%X(I) )
         end do

         ! CONTROLE DES EXTREMITES DU PROFIL
         ! ---------------------------------
         if( abs(Profil(J)%Y(1)-Profil(J)%Y(nb_point)).gt.EPS15 ) then
            Erreur%Numero = 54
            Erreur%ft     = err_54
            Erreur%ft_c   = err_54c
            call TRAITER_ERREUR( Erreur , J )
            return
         end if

         ! CONTROLE DES LIMITES DU LIT MINEUR ==> LIT MAJEUR
         ! -------------------------------------------------
         if( limite_min(1) >= limite_min(2) ) then
            Erreur%Numero = 55
            Erreur%ft     = err_55
            Erreur%ft_c   = err_55c
            call TRAITER_ERREUR( Erreur , J , 'mineur' )
            return
         end if

         if( limite_maj(1) >= limite_maj(2) ) then
            Erreur%Numero = 55
            Erreur%ft     = err_55
            Erreur%ft_c   = err_55c
            call TRAITER_ERREUR( Erreur , J , 'majeur' )
            return
         end if

         if( ( limite_min(1) < limite_maj(1) ) .or. ( limite_min(2) > limite_maj(2) ) ) then
            Erreur%Numero = 56
            Erreur%ft     = err_56
            Erreur%ft_c   = err_56c
            call TRAITER_ERREUR( Erreur , J )
            return
         end if

         ! CONTROLE DES LIMITES DU LIT MINEUR
         ! ----------------------------------
         do i = 1 , nb_point
            if( abs(Profil(J)%X(I)-limite_min(1)).lt.EPS15 ) then
               exit
            elseif( I == nb_point ) then
               Erreur%Numero = 57
               Erreur%ft     = err_57
               Erreur%ft_c   = err_57c
               call TRAITER_ERREUR( Erreur , J , 'gauche du lit mineur' )
               return
            end if
         end do

         do i = 1 , nb_point
            if( abs(Profil(J)%X(I)-limite_min(2)).lt.EPS15 ) then
               exit
            elseif( i == nb_point ) then
               Erreur%Numero = 57
               Erreur%ft     = err_57
               Erreur%ft_c   = err_57c
               call TRAITER_ERREUR( Erreur , J , 'droite du lit mineur' )
               return
            end if
         end do

         if( x_min > limite_min(1) ) then
            Erreur%Numero = 58
            Erreur%ft     = err_58
            Erreur%ft_c   = err_58c
            call TRAITER_ERREUR( Erreur , J , 'gauche du lit mineur' )
            return
         end if

         if( x_max < limite_min(2) ) then
            Erreur%Numero = 58
            Erreur%ft     = err_58
            Erreur%ft_c   = err_58c
            call TRAITER_ERREUR( Erreur , J , 'droite du lit mineur' )
            return
         end if

         ! CONTROLE DES LIMITES DU LIT MAJEUR
         ! ----------------------------------
         do i = 1 , nb_point
            if( abs(Profil(J)%X(I)-limite_maj(1)).lt.EPS15 ) then
               exit
            elseif( I == nb_point ) then
               Erreur%Numero = 57
               Erreur%ft     = err_57
               Erreur%ft_c   = err_57c
               call TRAITER_ERREUR( Erreur , J , 'gauche du lit majeur' )
               return
            end if
         end do

         do i = 1 , nb_point
            if( abs(Profil(J)%X(I)-limite_maj(2)).lt.EPS15 ) then
               exit
            elseif( i == nb_point ) then
               Erreur%Numero = 57
               Erreur%ft     = err_57
               Erreur%ft_c   = err_57c
               call TRAITER_ERREUR( Erreur , J , 'droite du lit majeur' )
               return
            end if
         end do

         if( x_min > limite_maj(1) ) then
            Erreur%Numero = 58
            Erreur%ft     = err_58
            Erreur%ft_c   = err_58c
            call TRAITER_ERREUR( Erreur , J , 'gauche du lit majeur' )
            return
         end if

         if( x_max < limite_maj(2) ) then
            Erreur%Numero = 58
            Erreur%ft     = err_58
            Erreur%ft_c   = err_58c
            call TRAITER_ERREUR( Erreur , J , 'droite du lit majeur' )
            return
         end if

         !------------------------------------------
         ! Calcul des points limites des lits mineur
         !------------------------------------------
         do i = 2 , nb_point - 1
            if( abs(Profil(j)%X(i)-limite_min(1)).lt.EPS15 ) then
               Profil(j)%LimiteMin(1) = i
               exit
            end if
         end do

         do i = 2 , nb_point - 1
            if( abs(Profil(j)%X(i)-limite_min(2)).lt.EPS15 ) then
               Profil(j)%LimiteMin(2) = i
            end if
         end do

         !------------------------------------------
         ! Calcul des points limites des lits majeur
         !------------------------------------------
         do i = 2 , nb_point - 1
            if( abs(Profil(j)%X(i)-limite_maj(1)).lt.EPS15 ) then
               Profil(j)%LimiteMaj(1) = i
               exit
            end if
         end do

         do i = 2 , nb_point - 1
            if( abs(Profil(j)%X(i)-limite_maj(2)).lt.EPS15 ) then
               Profil(j)%LimiteMaj(2) = i
            end if
         end do
      end do loop_1     ! sur les profils

   ! ----------------------------------------------------------------------
   elseif( type_profil == TYPE_PROFIL_LARGEUR ) then
      Erreur%numero = 59
      Erreur%ft     = err_59
      Erreur%ft_c   = err_59c
      call TRAITER_ERREUR( Erreur )
      return
   end if

   !=====================================
   !  LECTURE DU NUMERO DU PREMIER Profil
   !  DE CHAQUE RIVIERE OU BIEF
   !=====================================
   profil_1(:)     = 0
   ibief           = 1
   profil_1(ibief) = ibief
   ibief           = ibief + 1

   read(ul,*,iostat = RETOUR) LIBEL , profil_1(ibief) , LIBEL , dbief

   do while( RETOUR == 0 )
      !----------
      ! Controles
      !----------
      if( profil_1(ibief) <= profil_1(ibief - 1) ) then
         Erreur%Numero = 61
         Erreur%ft     = err_61
         Erreur%ft_c   = err_61c
         call TRAITER_ERREUR( Erreur , ibief , ibief - 1 )
         return
      end if

      if( profil_1(ibief) > nb_profil ) then
         nb_bief = ibief - 1
         exit
      endif

      !-----------------------------
      ! Lecture d'une nouvelle ligne
      !-----------------------------
      ibief = ibief + 1
      read (ul,*,iostat = RETOUR) LIBEL , profil_1(ibief) , LIBEL , dbief
   end do

   if( RETOUR /= 0 ) then
      Erreur%Numero = 60
      Erreur%ft     = err_60
      Erreur%ft_c   = err_60c
      call TRAITER_ERREUR( Erreur , ibief )
      return
   endif

   !  ==============================================
   !  VERIFICATION DE L'ORDRE DES PROFILS, EFFECTUEE
   !  D' APRES L'ABSCISSE DES PROFILS
   !  ==============================================
   do ibief = 1 , nb_bief
      prof_deb = profil_1(ibief) + 1      ! second  Profil du bief ibief
      prof_fin = profil_1(ibief + 1) - 1  ! dernier Profil du bief ibief
      if( prof_fin > nb_profil ) then
         Erreur%Numero = 62
         Erreur%ft     = err_62
         Erreur%ft_c   = err_62c
         call TRAITER_ERREUR( Erreur , ibief , prof_fin , nb_profil )
         return
      end if

      !-----------------------
      ! boucle sur les profils
      !-----------------------
      do i = prof_deb , prof_fin
         j = i - 1
         if( Profil(i)%AbsRel <= Profil(j)%AbsRel ) then
            Erreur%Numero = 63
            Erreur%ft     = err_63
            Erreur%ft_c   = err_63c
            call TRAITER_ERREUR( Erreur , i , Profil(i)%AbsRel , j , Profil(j)%AbsRel )
            return
         end if
      end do
   end do

   !-----------------------------------------------------------
   ! Affectation des numeros de branches aux structures profils
   !-----------------------------------------------------------
   do ibief = 1 , nb_bief
      do j = profil_1(ibief) , profil_1(ibief+1) - 1
         Profil(j)%NumBief = ibief
      end do
   end do

   ! ==========================================================
   ! TEST AFIN DE DETECTER LES PAROIS VERTICALES SUR LESQUELLES
   ! AUCUN FROTTEMENT NE SERA PRIS EN COMPTE
   ! IMPRESSION SYSTEMATIQUE SUR LE LISTING
   ! ==========================================================
   paroi_verticale = .false.
   do i = 1 , nb_profil
      iparoi = 0
      do j = 1 , size(Profil(i)%X) - 1
         if( abs(Profil(i)%X(j)-Profil(i)%X(j+1)).lt.EPS15 ) then
            iparoi = iparoi + 1
            if( .not.paroi_verticale .and. .not.FrottParoiVerticale ) then
               if (UniteListing >0) write(UniteListing,10012)
            else if( .not.paroi_verticale .and. FrottParoiVerticale ) then
               if (UniteListing >0) write (UniteListing,10013)
            end if
            paroi_verticale = .true.
            if( iparoi == 1 ) then
               if (UniteListing >0) write(UniteListing,10014) i , Profil(i)%Nom
            end if
            if (UniteListing >0) write (UniteListing,10015) j, j+1
         end if
      end do
   end do

   !------------------
   ! Fin du traitement
   !------------------
   close (ul)

   !Erreur%arbredappel = !arbredappel_old

   return

   ! Formats d ecriture WARNINGS
   ! ---------------------------
   10012 format (/                                                          , &
           '<<ATTENTION>> : DETECTION DE PAROIS VERTICALES SUR LESQUELLES',/, &
           ' AUCUN FROTTEMENT NE SERA PRIS EN COMPTE',/)
   10013 format (/                                                          , &
           '<<ATTENTION>> : DETECTION DE PAROIS VERTICALES SUR LESQUELLES',/, &
           ' LE FROTTEMENT SERA PRIS EN COMPTE',/)
   10014 format ('   SUR LE PROFIL NUMERO   ',i5,'   (',a12,')')
   10015 format ('     ENTRE LES POINTS   ',2i5)

end subroutine LEC_GEOM_V2P0
