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

subroutine LEC_HYDRAU( &
                       ! Resultats
                     loi , & ! loi hydraulique
              UniteTemps , & ! unite de temps des chroniques temporelles
                       ! Donnees
                 Fichier , & ! Fichier contenant la loi
       impression_hydrau , & ! Flag d'impression de la loi
            UniteListing , & ! Unite logique fichier listing
                  Erreur   & ! Erreur
                           )

! *********************************************************************
! PROGICIEL : MASCARET       S. MANDELKERN
!                            D. POIZAT
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
!
!  FONCTION : LECTURE DES FICHIERS DE DONNEES HYDRAULIQUE
!  --------
!
!----------------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :       - Fichier listing (UniteListing)
!   ----------------------         - Fichier des lois
!
!   SOUS-PROGRAMME(S) APPELANT(S) : - LEC_LOI
!   -----------------------------
!   SOUS-PROGRAMME(S) APPELE(S)   : - LIRE_CHAINE_S
!   ---------------------------
!   COMMENTAIRES :
!   ------------
!
!   DOCUMENTATION EXTERNE :
!   ---------------------
!
!***********************************************************************

   !============================= Declarations ============================
   use M_PRECISION
   use M_PARAMETRE_C
   use M_LOI_T               ! Definition du type LOI_T
   use M_ERREUR_T            ! Definition du type ERREUR_T
   use M_FICHIER_T           ! Definition du type FICHIER_T
   use M_MESSAGE_C           ! Definition des messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes LIMNIGRAMME, HYDROGRAMME, etc.
   use M_TRAITER_ERREUR_I    ! Inteface generique de gestion des erreurs
   use M_LIRE_CHAINE_S       ! Lecture des lignes commentaires

   !.. Declarations explicites ..
   implicit none

   !.. Arguments ..
   type(LOI_T)                  , intent(inout) :: loi
   integer                      , intent(  out) :: UniteTemps
   type(FICHIER_T)              , intent(in   ) :: fichier
   logical                      , intent(in   ) :: impression_hydrau
   integer                      , intent(in   ) :: UniteListing
   type(ERREUR_T) ,               intent(inout) :: Erreur
   !.. Constantes ..
   ! Chaine caractere pour identifier les lignes de commentaire
   ! dans l'en-tete du fichier
   character(1), parameter :: CHAINE_COMMENTAIRE = '#'
   integer     , parameter :: LEN_CHAINE = 80
   !.. Variables locales ..
   integer       :: ul                 ! Unite logique du fichier
   integer       :: retour             ! Code de retour de la fonction read
   integer       :: ipoint             ! Compteur sur les points d'une loi
   integer       :: jpoint             ! Compteur sur les points d'une loi
   integer       :: indice_abscisse    ! Index des valeurs d'abscisse
   integer       :: nb_point           ! Nombre d'abscisses dans une loi
   integer       :: nb_point_z         ! Dimension du tableau Zaval
   integer       :: nb_point_q         ! Dimension du tableau Q
   real(DOUBLE)  :: debit              !
   real(DOUBLE)  :: debit_a            !
   real(DOUBLE)  :: z_aval             !
   real(DOUBLE)  :: z_amont            !
   character(LEN_CHAINE) :: chaine     ! Chaine contenant une ligne du fichier
   integer          :: rang
   character(LEN=1) :: chaine_unite_temps
   !character(132):: !arbredappel_old    ! Arbredappel initial

   !============================ Commentaires ============================
   ! Description du fichier lu :
   !----------------------------
   ! Toute ligne de commentaire commence par le caractere "#"
   !
   !    Exemple :
   !
   !    #ESSAI
   !    #18/12/1995
   !    # courbe de tarage
   !    0.00     10.000
   !    4.00     12.876
   !    # commentaire
   !    20.00    13.500
   !
   !    Exemple :
   !
   !    #ESSAI
   !    #18/12/1995
   !    # loi Zam=f(Q,Zav)
   !    #    Q      Zav     Zam
   !     10.00     1.00    1.00
   !     10.00     1.10    1.20
   !     10.00     1.20    1.21
   !     20.00     1.00    1.10
   !     20.00     1.10    1.12
   !     20.00     1.20    1.14
   !    ...
   !============================ Instructions ==============================

   Erreur%Numero      = 0
   retour = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>LEC_HYDRAU'

   ! ===============================================================
   ! Premiere lecture du fichier afin de connaitre le nombre de lois
   ! ===============================================================
   ! et de deceler les erreurs
   ! ===============================================================
   ! Ouverture du fichier a lire
   ! ---------------------------
   open(unit=fichier%Unite, file=fichier%Nom, access='SEQUENTIAL', &
        action='READ'      , form='FORMATTED' , iostat=RETOUR    , &
        position='rewind'  , status='OLD'     )
   if( RETOUR /= 0 ) then
      Erreur%Numero = 3
      Erreur%ft   = err_3
      Erreur%ft_c = err_3c
      call TRAITER_ERREUR  (Erreur, fichier%Nom)
      return
   end if

   ul = fichier%Unite

   ! ===============================================================
   ! 1 ere lecture du fichier pour dimensionner la taille des lois
   ! ===============================================================
   ! dans le cas de chronique temporelle, verification que la premiere
   ! ligne non commentaire contient l'unite de temps
   if( Loi%Type == LOI_TYPE_LIMNIGRAMME     .or.  &
       Loi%Type == LOI_TYPE_HYDROGRAMME     .or.  &
       Loi%Type == LOI_TYPE_LIMNHYDROGRAMME .or.  &
       Loi%Type == LOI_TYPE_ZINF_ZSUP_T) then

      call LIRE_CHAINE_S( chaine , Fichier , CHAINE_COMMENTAIRE , retour )
      if( RETOUR /= 0 ) then
         Erreur%Numero = 380
         Erreur%ft     = err_380
         Erreur%ft_c   = err_380c
         call TRAITER_ERREUR( Erreur , Fichier%Nom )
         return
      end if

      rang = scan( chaine ,'SsMmHhJj' )
      if( rang == 0 ) then
         Erreur%Numero = 379
         Erreur%ft     = err_379
         Erreur%ft_c   = err_379c
         call TRAITER_ERREUR( Erreur , fichier%Nom )
         return
      endif
   endif

   indice_abscisse = 0

   label_dimension_lois : do while( RETOUR == 0 )
      indice_abscisse = indice_abscisse + 1
      call LIRE_CHAINE_S( chaine , Fichier , CHAINE_COMMENTAIRE , retour )
   end do label_dimension_lois

   !====================================
   ! La lecture du fichier est terminee
   !====================================
   label_retour : if( RETOUR > 0 ) then
      ! Le code de retour de la fonction READ informe d'une erreur
      !============================================================
      Erreur%Numero = 11
      Erreur%ft     = err_11
      Erreur%ft_c   = err_11c
      call TRAITER_ERREUR( Erreur , fichier%Nom , indice_abscisse )
      return
   else label_retour
      ! Le code de retour de la fonction READ informe de la fin du fichier
      !===================================================================
      ! On stocke le nombre d'abscisses lues
      ! Et on alloue les tableaux T, Z ou Q
      nb_point     = indice_abscisse - 1

      ! Controle du nombre de points minimum
      !-------------------------------------
      if( Loi%Type == LOI_TYPE_TARAGE_Q_Z .or. Loi%Type == LOI_TYPE_TARAGE_Z_Q ) then
         if( nb_point <= 1 ) then
            Erreur%Numero = 21
            Erreur%ft     = err_21
            Erreur%ft_c   = err_21c
            call TRAITER_ERREUR( Erreur , Loi%Nom )
            return
         end if
      endif

      ! Allocations
      !------------
      select case( Loi%Type )
         case( LOI_TYPE_HYDROGRAMME )

            if(.not.associated(Loi%Temps)) allocate( Loi%Temps(nb_point) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Loi%Temps' )
               return
            end if

            if(.not.associated(Loi%Debit)) allocate( Loi%Debit(nb_point) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Loi%Debit' )
               return
            end if

            if(.not.associated(Loi%Cote)) allocate( Loi%Cote(0) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft   = err_5
               Erreur%ft_c = err_5c
               call TRAITER_ERREUR( Erreur , 'Loi%Debit' )
               return
            end if

         case( LOI_TYPE_LIMNIGRAMME )

            if(.not.associated(Loi%Temps)) allocate( Loi%Temps(nb_point) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Loi%Temps' )
               return
            end if

            if(.not.associated(Loi%Debit)) allocate( Loi%Debit(0) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Loi%Debit' )
               return
            end if

            if(.not.associated(Loi%Cote)) allocate( Loi%Cote (nb_point) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Loi%Debit' )
               return
            end if

         case( LOI_TYPE_LIMNHYDROGRAMME )

            if(.not.associated(Loi%Temps)) allocate( Loi%Temps(nb_point) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Loi%Temps' )
               return
            end if

            if(.not.associated(Loi%Debit)) allocate( Loi%Debit(nb_point) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Loi%Debit' )
               return
            end if

            if(.not.associated(Loi%Cote)) allocate( Loi%Cote(nb_point) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Loi%Debit' )
               return
            end if

         case( LOI_TYPE_TARAGE_Z_Q , LOI_TYPE_TARAGE_Q_Z )

            if(.not.associated(Loi%Temps)) allocate( Loi%Temps(0) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Loi%Temps' )
               return
            end if

            if(.not.associated(Loi%Debit)) allocate( Loi%Debit(nb_point) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Loi%Debit' )
               return
            end if

            if(.not.associated(Loi%Cote)) allocate( Loi%Cote(nb_point) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Loi%Debit' )
               return
            end if

         case( LOI_TYPE_ZINF_ZSUP_T )

            if(.not.associated(Loi%Temps)) allocate( Loi%Temps(nb_point) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Loi%Cote' )
               return
            end if

            if(.not.associated(Loi%CoteSup)) allocate( Loi%CoteSup(nb_point) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Loi%CoteSup' )
               return
            end if

            if(.not.associated(Loi%CoteInf)) allocate( Loi%CoteInf(nb_point) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Loi%CoteInf' )
               return
            end if

         case( LOI_TYPE_ZAMONT_ZAVAL_Q )

            ! On relit le fichier pour dimensionner Zam, Zav et Q
            rewind (ul)

            do ipoint = 1 , nb_point
               call LIRE_CHAINE_S( chaine , Fichier , CHAINE_COMMENTAIRE , retour )
               read(chaine,*,IOSTAT = retour) debit , z_aval , z_amont
               if( ipoint == 1 ) then
                  debit_a = debit
               else if( abs(debit-debit_a).gt.EPS15 ) then
                  nb_point_z = ipoint - 1
                  exit
               endif
            end do

            nb_point_q = int(real(nb_point) / real(nb_point_z))

            ! Controle du nombre de points
            if( abs((real(nb_point)/real(nb_point_z))-real(nb_point_q)).gt.EPS15 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Loi%Nom' )
               return
            end if

            if(.not.associated(Loi%CoteAval)) allocate( Loi%CoteAval(nb_point_z) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Loi%CoteAval' )
               return
            end if

            if(.not.associated(Loi%Debit)) allocate( Loi%Debit(nb_point_q) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Loi%Debit' )
               return
            end if

            if(.not.associated(Loi%CoteAmont)) allocate( Loi%CoteAmont(nb_point_q, nb_point_z) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Loi%CoteAmont' )
               return
            end if
      end select

      ! ===============================================================
      !             2eme lecture - Lecture effective des lois
      ! ===============================================================

      ! Retour au debut du fichier
      rewind (ul)

      ! Le cas echeant, lecture de l'unite de temps
      if(Loi%Type == LOI_TYPE_LIMNIGRAMME     .or.  &
         Loi%Type == LOI_TYPE_HYDROGRAMME     .or.  &
         Loi%Type == LOI_TYPE_LIMNHYDROGRAMME .or.  &
         Loi%Type == LOI_TYPE_ZINF_ZSUP_T) then

         call LIRE_CHAINE_S (chaine, Fichier, CHAINE_COMMENTAIRE, retour)

         read(chaine(rang:rang+1),*,IOSTAT = retour) chaine_unite_temps

         if( RETOUR /= 0 ) then
            Erreur%Numero = 380
            Erreur%ft     = err_380
            Erreur%ft_c   = err_380c
            call TRAITER_ERREUR( Erreur , Fichier%Nom )
            return
         end if

         select case( chaine_unite_temps )

            case( "S" , "s" )

               UniteTemps = 1
               if( impression_hydrau ) write(UniteListing,10780) 'SECONDE'

            case( "M" , "m" )

               UniteTemps = 2
               if( impression_hydrau ) write(UniteListing,10780) 'MINUTE'

            case( "H" , "h" )

               UniteTemps = 3
               if( impression_hydrau ) write(UniteListing,10780) 'HEURE'

            case( "J" , "j" )

               UniteTemps = 4
               if( impression_hydrau ) write(UniteListing,10780) 'JOUR'

         end select

      endif

      ! Lecture des points de la loi
      select case( Loi%Type )

         case( LOI_TYPE_HYDROGRAMME )

            if (UniteListing >0) write(UniteListing,10785) size(Loi%Temps)
            if( impression_hydrau ) then
               write(UniteListing,10790) '            Temps   Debit'
            endif

            do ipoint = 1 , size(Loi%Temps)

               call LIRE_CHAINE_S( chaine , Fichier , CHAINE_COMMENTAIRE , retour )
               read(chaine,*,IOSTAT = retour) loi%Temps(ipoint) , loi%Debit(ipoint)

               if( RETOUR /= 0 ) then
                  Erreur%Numero = 11
                  Erreur%ft     = err_11
                  Erreur%ft_c   = err_11c
                  call TRAITER_ERREUR( Erreur , Fichier%Nom , ipoint )
                  return
               end if

               if( impression_hydrau ) then
                  write(UniteListing,10800) ipoint , Loi%Temps(ipoint) , Loi%Debit(ipoint)
               endif

            end do

         case( LOI_TYPE_LIMNIGRAMME )

            if (UniteListing >0) write(UniteListing,10785) size(Loi%Temps)
            if( impression_hydrau ) then
               write(UniteListing,10790) '            Temps   Cote'
            endif

            do ipoint = 1 , size(Loi%Temps)
               call LIRE_CHAINE_S( chaine , Fichier , CHAINE_COMMENTAIRE , retour )
               read(chaine,*,IOSTAT = retour) loi%Temps(ipoint) , loi%Cote(ipoint)
               if( RETOUR /= 0 ) then
                  Erreur%Numero = 11
                  Erreur%ft     = err_11
                  Erreur%ft_c   = err_11c
                  call TRAITER_ERREUR( Erreur , Fichier%Nom , ipoint )
                  return
               end if

               if( impression_hydrau ) then
                  write(UniteListing,10800) ipoint , Loi%Temps(ipoint) , Loi%Cote(ipoint)
               endif

            end do

         case( LOI_TYPE_TARAGE_Z_Q )

            if (UniteListing >0) write(UniteListing,10785) size(Loi%Cote)
            if( impression_hydrau ) then
               write(UniteListing,10790) '            Debit   Cote'
            endif

            do ipoint = 1 , size(Loi%Debit)

               call LIRE_CHAINE_S( chaine , Fichier , CHAINE_COMMENTAIRE , retour )
               read(chaine,*,IOSTAT = retour) loi%Debit(ipoint) , loi%Cote(ipoint)

               if( RETOUR /= 0 ) then
                  Erreur%Numero = 11
                  Erreur%ft     = err_11
                  Erreur%ft_c   = err_11c
                  call TRAITER_ERREUR( Erreur , Fichier%Nom , ipoint )
                  return
               end if

               if( impression_hydrau ) then
                  write(UniteListing,10800) ipoint , Loi%Debit(ipoint) , Loi%Cote(ipoint)
               endif

            end do

         case( LOI_TYPE_TARAGE_Q_Z )

            if (UniteListing >0) write(UniteListing,10785) size(Loi%Cote)
            if( impression_hydrau ) then
               write(UniteListing,10790) '            Cote   Debit'
            endif

            do ipoint = 1 , size(Loi%Debit)

               call LIRE_CHAINE_S( chaine , Fichier , CHAINE_COMMENTAIRE , retour )
               read(chaine,*,IOSTAT = retour) loi%Cote(ipoint) , loi%Debit(ipoint)

               if( RETOUR /= 0 ) then
                  Erreur%Numero = 11
                  Erreur%ft     = err_11
                  Erreur%ft_c   = err_11c
                  call TRAITER_ERREUR( Erreur , Fichier%Nom , ipoint )
                  return
               end if

               if( impression_hydrau ) then
                  write(UniteListing,10800) ipoint , Loi%Cote(ipoint) , Loi%Debit(ipoint)
               endif

            end do

         case( LOI_TYPE_LIMNHYDROGRAMME )

            if (UniteListing >0) write(UniteListing,10785) size(Loi%Temps)

            if( impression_hydrau ) then
               write(UniteListing,10790) 'Temps   Cote   Debit'
            endif

            do ipoint = 1 , size(Loi%Debit)

               call LIRE_CHAINE_S (chaine, Fichier, CHAINE_COMMENTAIRE, retour)
               read(chaine,*,IOSTAT = retour) loi%Temps(ipoint), loi%Cote(ipoint), loi%Debit(ipoint)
               if( RETOUR /= 0 ) then
                  Erreur%Numero = 11
                  Erreur%ft     = err_11
                  Erreur%ft_c   = err_11c
                  call TRAITER_ERREUR( Erreur , Fichier%Nom , ipoint )
                  return
               end if

               if( impression_hydrau ) then
                  write(UniteListing,10805) ipoint , Loi%Temps(ipoint) , Loi%Cote(ipoint) , Loi%Debit(ipoint)
               endif

            end do

         case( LOI_TYPE_ZINF_ZSUP_T )

            if (UniteListing >0) write(UniteListing,10785) size(Loi%Temps)

            if( impression_hydrau ) then
               write(UniteListing,10790) 'Temps      Cote inf   Cote sup'
            endif

            do ipoint = 1 , size(Loi%Temps)

               call LIRE_CHAINE_S (chaine, Fichier, CHAINE_COMMENTAIRE, retour)
               read(chaine,*,IOSTAT = retour) loi%Temps(ipoint), loi%CoteInf(ipoint), loi%CoteSup(ipoint)
               if( RETOUR /= 0 ) then
                  Erreur%Numero = 11
                  Erreur%ft     = err_11
                  Erreur%ft_c   = err_11c
                  call TRAITER_ERREUR( Erreur , Fichier%Nom , ipoint )
                  return
               end if

               if( impression_hydrau ) then
                  write(UniteListing,10805) ipoint, Loi%Temps(ipoint),   &
                                         Loi%CoteInf(ipoint), &
                                         Loi%CoteSup(ipoint)
               endif
            end do

         case( LOI_TYPE_ZAMONT_ZAVAL_Q )

            if (UniteListing >0) write(UniteListing,10785) size(Loi%Debit)

            if( impression_hydrau ) then
               write(UniteListing,10790) 'Debit   Cote aval   Cote amont'
            endif

            do ipoint = 1 , nb_point_q

               do jpoint = 1 , nb_point_z

                  call LIRE_CHAINE_S (chaine, Fichier, CHAINE_COMMENTAIRE, retour)
                  read(chaine,*,IOSTAT = retour) loi%Debit(ipoint), loi%CoteAval(jpoint), loi%CoteAmont(ipoint,jpoint)
                  if( RETOUR /= 0 ) then
                     Erreur%Numero = 11
                     Erreur%ft     = err_11
                     Erreur%ft_c   = err_11c
                     call TRAITER_ERREUR( Erreur , Fichier%Nom , (ipoint-1)*nb_point_q + jpoint )
                     return
                  end if

                  if( impression_hydrau ) then
                     write(UniteListing,10805) Loi%Debit(ipoint), Loi%CoteAval(jpoint), Loi%CoteAmont(ipoint,jpoint)
                  endif

               end do

            end do

      end select

   endif label_retour

   close(ul)

   !Erreur%arbredappel = !arbredappel_old

! Formats

  10780 format ('Unite de temps     = ',A)
  10785 format ('Nombre de points   = ',i3)
  10790 format (A)
  10800 format (i5,2f12.3)
  10805 format (i5,3f12.3)

end subroutine LEC_HYDRAU
