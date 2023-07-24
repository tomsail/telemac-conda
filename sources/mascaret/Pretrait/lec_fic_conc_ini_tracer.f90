!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET-TRACER.
!
!   MASCARET-TRACER is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET-TRACER is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET-TRACER.  If not, see <http://www.gnu.org/licenses/>
!

subroutine LEC_FIC_CONC_INI_TRACER( &
                            x_ini , & ! Tableau des abscisses initiales
                            c_ini , & ! Tableau des concentrations initiales
                          nb_trac , & ! nombre de traceurs
                  FichierConcInit , & ! fichier des concentrations initiales
                           Erreur   &
                                  )

!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN - M. LUCK
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!
!  FONCTION :   LECTURE DU FICHIER D'UNE LIGNE D'EAU INITIALE
!  --------     AU FORMAT OPTHYCA
!
!  SOUS PROGRAMMES APPELANT(S) : LEC_CONC_INI_TRACER
!  ---------------------------
!  SOUS PROGRAMMES APPELE(S) :   - LIRE_CHAINE_S
!  -------------------------     - OPT2FORT
!
!***********************************************************************

   !============================= Declarations ===========================
   use M_PRECISION
   use M_PARAMETRE_C
   use M_MESSAGE_C        ! messages d'erreur
   use M_MESSAGE_TRACER_C
   use M_ERREUR_T         ! Definition du type ERREUR_T
   use M_FICHIER_T        !                    FICHIER_T
   use M_TRAITER_ERREUR_I ! traitement des erreurs
   use M_LIRE_CHAINE_S    ! lecture d'une chaine de caracteres
   use M_OPT2FORT_I       ! interface de sous-programme

   !.. Declarations implicites ..
   implicit none

   !.. Arguments ..
   real(DOUBLE), dimension(:), pointer       :: x_ini   ! abscisse lue sur le fichier
   real(DOUBLE), dimension(:,:), pointer     :: c_ini   ! cote lue sur le fichier
   integer                   , intent(in   ) :: nb_trac  ! nombre de traceurs
   type(FICHIER_T)           , intent(in   ) :: FichierConcInit
   type(ERREUR_T)            , intent(inout) :: Erreur
   ! Constantes
   integer     , parameter :: LEN_CHAINE = 200
   character(2), parameter :: CHAINE_COMMENTAIRE = '/*'
   character(2), dimension(10) :: j_in_letter
   data j_in_letter / '1','2','3','4','5','6','7','8','9','10'/
   !.. Variables locales ..
   integer :: ul                     ! Unite logique du fichier de stockage
                                     !  des fonctions d'E/S
   integer :: rang                   ! position du mot cle sur la ligne
   character(LEN_CHAINE) :: chaine   ! chaine lue sur le fichier
   character(LEN_CHAINE) :: chaine_opt  ! chaine au format opthyca
   character(LEN_CHAINE) :: chaine_fort ! chaine convertie au format fortran
   integer :: ivar                   ! compteur
   integer, dimension(10):: pos_c    ! position de la colonne concentration
   character(LEN=30) :: nom_bief     ! nom du bief lu
   character(LEN=30) :: nom_sect     ! nom de la section lu
   real(DOUBLE)      :: temps        ! temps lu
   real(DOUBLE)      :: temps_ini    ! temps initial
   integer           :: RETOUR       ! code de retour des fonctions d'e/s
   integer           :: i,j          ! compteur
   integer           :: num_sect     ! compteur des sections
   integer           :: nb_sect      ! nombre de sections
   integer           :: nb_var_stocke! nombre de variables stockees
   real(DOUBLE), dimension(:), allocatable :: var
   real(DOUBLE)      :: x_temp       ! abscisse temporaire
   character(2) :: CU
   character(3) :: CD


   !============================ Instructions ==============================
   !INITIALISATION
   !==============
   Erreur%Numero = 0
   !arbredappel_old = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>LEC_FIC_CONC_INI_TRACER'

   ul = FichierConcInit%Unite

   !----------------------------
   ! ouverture du fichier a lire
   !----------------------------
   open( unit = ul           , file = FichierConcInit%Nom , access = 'SEQUENTIAL' , &
         action = 'READ'     , form = 'FORMATTED'     , iostat = RETOUR      , &
         position = 'rewind' , status = 'OLD'    )

   if( RETOUR /= 0 ) then
      Erreur%Numero = 3
      Erreur%ft     = err_3
      Erreur%ft_c   = err_3c
      call TRAITER_ERREUR_1C( Erreur , FichierConcInit%Nom )
      return
   end if

   ! LECTURE DE L'EN-TETE
   ! ====================
   call LIRE_CHAINE_S( chaine , FichierConcInit , CHAINE_COMMENTAIRE , RETOUR )

   ! si probleme en lecture de la ligne
   ! ----------------------------------
   if( RETOUR /= 0 ) then
      Erreur%Numero = 64
      Erreur%ft     = err_64
      Erreur%ft_c   = err_64c
      call TRAITER_ERREUR( Erreur )
      return
   end if

   rang = 0
   rang = index( chaine , '[variables]' )

   !--------------------------------------------
   ! si le mot-cle '[variables]' n'est pas trouve
   !--------------------------------------------

   if( rang == 0 ) then
      Erreur%Numero = 65
      Erreur%ft     = err_65
      Erreur%ft_c   = err_65c
      call TRAITER_ERREUR_1C( Erreur , '[variables]' )
      return
   end if

   ! initialisation des indices
   ! --------------------------
   ivar = 0
   do j = 1 , nb_trac
      pos_c(j) = 0
   enddo

   ! lecture d'une ligne
   ! -------------------
   call LIRE_CHAINE_S( chaine , FichierConcInit , CHAINE_COMMENTAIRE , RETOUR )

   do while( RETOUR == 0 )

      ! test de la presence du mot cle "resultat"
      ! -----------------------------------------
      rang = 0
      rang = index( chaine , 'resultats' )
      if( rang /= 0 ) then
         exit
      else
         ! on incremente la position
         ivar = ivar + 1
         ! on recherche la position des colonnes Cj
         ! la colonne X etant toujours placee a la meme position
         !------------------------------------------------------
         do j = 1 , nb_trac
            if( j.lt.10 ) then
               CU = 'C'//j_in_letter(j)(1:1)
               rang = index( chaine ,CU)
            else
               CD = 'C'//j_in_letter(j)
               rang = index( chaine ,CD)
            endif
            if( rang /= 0 ) then
               pos_c(j) = ivar
            endif
         enddo

         call LIRE_CHAINE_S( chaine , FichierConcInit , CHAINE_COMMENTAIRE , RETOUR )

      end if

   end do

   ! si probleme de lecture
   !-----------------------
   if( RETOUR /= 0 ) then
      Erreur%Numero = 66
      Erreur%ft     = err_66
      Erreur%ft_c   = err_66c
      call TRAITER_ERREUR_1I( Erreur , ivar + 1 )
      return
   end if

   ! s'il n'y a pas de variable dans l'en-tete
   !-------------------------------------------
   if( ivar == 0 ) then
      Erreur%Numero = 67
      Erreur%ft     = err_67
      Erreur%ft_c   = err_67c
      call TRAITER_ERREUR( Erreur )
      return
   end if

   ! s'il n'y a pas de c
   !---------------------
   do j = 1 , nb_trac
      if( pos_c(j) == 0 ) then
         Erreur%Numero = 568
         Erreur%ft     = err_568
         Erreur%ft_c   = err_568c
         call TRAITER_ERREUR_1C( Erreur , 'C'//j_in_letter(j) )
         return
      end if
   enddo

   ! mise en memoire du nombre total de variables stockees en plus
   ! de x
   nb_var_stocke = ivar

   ! allocation du tableau var en fonction de l'indice le plus grand
   !----------------------------------------------------------------
   allocate( var(nb_var_stocke) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR_1C( Erreur , 'var' )
      return
   end if

   !==============================================================
   ! PREMIERE LECTURE POUR COMPTER LE NOMBRE DE SECTIONS DE CALCUL
   !==============================================================
   num_sect = 1

   !--------------------
   ! lecture d'une ligne
   !--------------------
   call LIRE_CHAINE_S( chaine_opt , FichierConcInit , CHAINE_COMMENTAIRE , RETOUR )

   ! si probleme de lecture
   if( RETOUR /= 0 ) then
      Erreur%Numero = 76
      Erreur%ft     = err_76
      Erreur%ft_c   = err_76c
      call TRAITER_ERREUR_1I( Erreur , num_sect )
      return
   end if

   !-----------------------
   ! conversion de la ligne
   !-----------------------
   call OPT2FORT( chaine_fort , chaine_opt , Erreur )
   if( Erreur%Numero /= 0 ) then
      return
   endif

   !--------------------------------------
   ! lecture des informations sur la ligne
   !--------------------------------------
   read( chaine_fort , * , iostat = RETOUR ) temps , nom_bief , nom_sect , x_temp , (var(i),i=1,nb_var_stocke)
   ! si probleme de lecture
   if( RETOUR /= 0 ) then
      Erreur%Numero = 69
      Erreur%ft     = err_69
      Erreur%ft_c   = err_69c
      call TRAITER_ERREUR_1I( Erreur , num_sect )
      return
   end if

   !-----------------------------
   ! affectation du temps initial
   !-----------------------------
   temps_ini = temps

   ! Boucle de lecture
   !==================
   do while( RETOUR == 0 )

      num_sect = num_sect + 1

      !-----------------------------
      ! lecture de la ligne suivante
      !-----------------------------

      call LIRE_CHAINE_S( chaine_opt , FichierConcInit , CHAINE_COMMENTAIRE , RETOUR )
      ! si fin du fichier
      if( RETOUR < 0 ) then
         exit
      ! si probleme de lecture
      else if( RETOUR > 0 ) then
         Erreur%Numero = 76
         Erreur%ft     = err_76
         Erreur%ft_c   = err_76c
         call TRAITER_ERREUR_1C1I( Erreur , FichierConcInit%Nom , num_sect )
         return
      end if

      !-----------------------
      ! conversion de la ligne
      !-----------------------
      call OPT2FORT( chaine_fort , chaine_opt , Erreur )
      if( Erreur%Numero /= 0 ) then
         return
      endif

      !--------------------------------------
      ! lecture des informations sur la ligne
      !--------------------------------------
      read( chaine_fort , * , iostat = RETOUR ) temps , nom_bief , nom_sect , x_temp , (var(i),i=1,nb_var_stocke)

      ! si probleme de lecture
      !-----------------------
      if( RETOUR /= 0 ) then
         Erreur%Numero = 69
         Erreur%ft     = err_69
         Erreur%ft_c   = err_69c
         call TRAITER_ERREUR_1I( Erreur , num_sect )
         return
      end if

      !-----------------------------------------------------
      ! si on arrive a une deuxieme ligne d'eau, on s'arrete
      ! car on ne prend en compte que la premiere du fichier
      !-----------------------------------------------------
      if( abs(temps-temps_ini).gt.EPS6 ) then
         exit
      endif

   ! fin de la boucle de lecture
   !============================
   end do

   nb_sect = num_sect - 1

   ! allocation des tableaux x_ini, z_ini et q_ini
   !----------------------------------------------
   allocate( x_ini(nb_sect) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR_1C( Erreur , 'x_ini' )
      return
   end if

   do j = 1 , nb_trac

      allocate( c_ini(nb_sect,j) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR_1C( Erreur , 'z_ini' )
         return
      end if

   enddo

   !===========================================
   ! DEUXIEME PASSAGE : LECTURE PROPREMENT DITE
   !===========================================
   ! Repositionnement au debut des resultats (apres l'en-tete)
   ! qui prend nb_var_stocke + 2 lignes
   !----------------------------------------------------------
   rewind(ul)

   do i = 1 , nb_var_stocke + 2
      call LIRE_CHAINE_S( chaine , FichierConcInit , CHAINE_COMMENTAIRE , RETOUR )
   end do

   ! Boucle de lecture des donnees
   !==============================
   do num_sect = 1 , nb_sect
      !--------------------
      ! lecture d'une ligne
      !--------------------
      call LIRE_CHAINE_S( chaine_opt , FichierConcInit , CHAINE_COMMENTAIRE , RETOUR )
      ! si probleme de lecture
      if( RETOUR /= 0 ) then
         Erreur%Numero = 76
         Erreur%ft     = err_76
         Erreur%ft_c   = err_76c
         call TRAITER_ERREUR_1C1I( Erreur , FichierConcInit%Nom , num_sect )
         return
      end if

      !-----------------------
      ! conversion de la ligne
      !-----------------------
      call OPT2FORT( chaine_fort , chaine_opt , Erreur )
      if( Erreur%Numero /= 0 ) then
         return
      endif

      !--------------------------------------
      ! lecture des informations sur la ligne
      !--------------------------------------
      read( chaine_fort , * , iostat = RETOUR ) temps , nom_bief , nom_sect , x_ini(num_sect) , (var(i),i=1,nb_var_stocke)
      ! si probleme de lecture
      if( RETOUR /= 0 ) then
         Erreur%Numero = 69
         Erreur%ft     = err_69
         Erreur%ft_c   = err_69c
         call TRAITER_ERREUR_1I( Erreur , num_sect )
         return
      end if

      !----------------------------------------------------------
      ! affectation des tableaux c_ini
      !----------------------------------------------------------
      do j = 1 , nb_trac
         c_ini(num_sect,j) = var(pos_c(j))
      enddo

   ! fin de la boucle de lecture
   !============================
   end do

   ! de-allocation du tableau local var
   !-----------------------------------
   deallocate(var)
   if( retour /= 0 ) then
      Erreur%Numero = 6
      Erreur%ft     = err_6
      Erreur%ft_c   = err_6c
      call TRAITER_ERREUR_1C( Erreur , 'var' )
      return
   end if

   ! fermeture du fichier
   !---------------------
   close(ul)

   !Erreur%arbredappel = arbredappel_old

   return

end subroutine LEC_FIC_CONC_INI_TRACER
