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

subroutine ALGOP( &
         Algorithme , &
             NbSect , &
         NbExtNoeud , &
            Connect , &
   ImpressionReseau , &
       UniteListing , & ! Unite logique fichier listing
             Erreur   &
                   )

! *********************************************************************
! PROGICIEL : MASCARET         A. LEBOSSE
!                              S. MANDELKERN
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
!  FONCTION :
!  --------
!                          SARA PERMANENT
!                          >>>>>>><<<<<<<
!         DEFINITION DE L'ALGORITHME DE RESOLUTION :
!         CONSTITUTION DU TABLEAU D'APPELS AUX SOUS-PROGRAMMES
!         RESOLVANT LE CALCUL DE LA LIGNE D'EAU EN REGIME PERMANENT .
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____._______________________________________________
! !    NOM    !TYPE!MODE!                   ROLE
! !___________!____!____!_______________________________________________
! ! Algorithme! I  !<-- ! TABLEAUX DES APPELS AUX S.P. RESOLVANT LE
! !           !    !    ! CALCUL DE LIGNES D'EAU :
! !           !    !    !  Algorithme = ISUB + I
! !           !    !    !   AVEC  ISUB = 100  ==>  CALL QBIEF
! !           !    !    !         ISUB = 200  ==>  CALL PERMAT
! !           !    !    !         ISUB = 300  ==>  CALL QNODE (1ER PAS)
! !           !    !    !         ISUB = 400  ==>  CALL QNODE (2EM PAS)
! !           !    !    !         ISUB = 500  ==>  CALL QREPAR(1ER PAS)
! !           !    !    !         ISUB = 600  ==>  CALL QREPAR(2EM PAS)
! !           !    !    !     ET   I = LE NUMERO DU NOEUD OU DU BIEF
! ! ical      ! I  ! -- ! ical(L) = +1 SI ON A TRAITE LA LIEME
! !           !    !    ! SECTION EXTREME ( ORIGINE )
! !           !    !    ! ical(L) = -1 SI ON A TRAITE LA LIEME
! !           !    !    ! SECTION EXTREME ( EXTREMITE )
! !           !    !    ! ical(L) =  0 SI C'EST UNE LIMITE LIBRE
! !           !    !    ! ical(L) =  1 SI LA LIEME SECTION EXTREME
! !           !    !    ! EST RELIEE A UN NOEUD
! !___________!____!____!_____________________________________________
!----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ----------------------
!                UniteListing  : IMPRESSION LISTING
!
!   SOUS PROGRAMMES APPELANTS :  PRETRAIT
!   ---------------------------
!   SOUS PROGRAMMES APPELES :    ---
!   -------------------------
!***********************************************************************

   !============================= Declarations ===========================
   use M_MESSAGE_C        ! Liste des messages d'erreur
   use M_CONNECT_T        ! Definition du type CONNECT_T
   use M_ERREUR_T         ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   implicit none

   ! Arguments
   integer, dimension(:), pointer       :: Algorithme
   integer              , intent(in   ) :: NbSect
   integer, dimension(:), intent(in   ) :: NbExtNoeud
   type(CONNECT_T)      , intent(in   ) :: Connect
   logical              , intent(in   ) :: ImpressionReseau
   integer              , intent(in   ) :: UniteListing
   type(ERREUR_T)       , intent(inout) :: Erreur
   ! Variables locales
   integer, dimension(NbSect)                         :: ical
   logical, dimension(size(Connect%NbBiefConfluence)) :: noeud_traite
   integer :: nb_noeud
   integer :: nb_bief
   integer :: nb_noeud_non_traite ! Compteur des noeuds non traites
   integer :: num_bief
   integer :: num_section
   integer :: num_bief_aval
   integer :: num_section_aval
   integer :: nb_branche_aval
   integer :: iext             ! Compteur sur les extremites libres
   integer :: inoeud           ! Compteur sur les noeuds
   integer :: iafflu           ! Compteur sur les affluents
   integer :: iextremite       ! Compteur sur les extremites
   integer :: i, isub
   logical :: traiter_defluence  ! indicateur de traitement des noeuds
                                     ! avec plusieurs branches aval
   integer :: icompt       ! Compteur sur les iterations
   integer :: lindam
   integer :: nb_element   ! Nombre d'elements du tableau Algorithme
   logical :: origine_bief ! test de positionnement d'une section
   integer :: retour       ! Code de retour des fonctions intrinseques
   !character(132) :: !arbredappel_old

   !============================= Instructions ===========================
   ! ===============
   ! INITIALISATIONS
   ! ===============
   Erreur%Numero = 0
   retour = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>ALGOP'

   nb_bief  = size(Connect%OrigineBief)
   nb_noeud = size(Connect%NbBiefConfluence)

   if(.not.associated(Algorithme)) allocate( Algorithme( 2 * ( nb_bief + nb_noeud ) ) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'Algorithme' )
      return
   end if

   iextremite      = 0
   noeud_traite(:) = .false.
   ical(:)         = 0
   Algorithme(:)   = 0

   ! ===================================
   ! UN SEUL BIEF ==> ON FAIT COMME LIDO
   ! ===================================
   if( nb_bief == 1 ) then
      Algorithme(1) = 101
      iextremite    = 1
   else
   ! =================================================
   ! TRAITEMENT DES BIEFS AYANT UNE LIMITE LIBRE AMONT
   ! =================================================
   ! INITIALISATIONS DU TABLEAU ical
   ! -------------------------------
      iextremite = 0
      ! BOUCLE SUR LES LIMITES LIBRES
      ! -----------------------------
      do iext = 1 , size(Connect%NumSectionExtLibre)
         ! Calcul du numero de bief
         ! et du numero de section correspondant
         !--------------------------------------
         num_bief    = Connect%NumBiefExtLibre(iext)
         num_section = Connect%NumSectionExtLibre(iext)
         !----------------------------------------
         ! Est-ce une origine ou une fin de bief ?
         !----------------------------------------
         if( Connect%OrigineBief(num_bief) == num_section ) then
            origine_bief = .true.
         else if (Connect%FinBief(num_bief) == num_section) then
            origine_bief = .false.
         endif
         !--------------------------------------
         ! ON PART DES EXTREMITES DE TYPE AMONT
         ! OU LE DEBIT EST CONNU (PAR HYPOTHESE)
         !--------------------------------------
         ! Si c'est une section amont
         if( origine_bief ) then
            ISUB = 100
            ical(num_section)               =  1
            ical(Connect%FinBief(num_bief)) = -1
            iextremite                      = iextremite + 1
            Algorithme(iextremite)          = ISUB + num_bief
         endif
      end do
      ! On a parcouru toutes les extremites libres amont de biefs
      ! on a implemente Algorithme et positionne ical
      ! ===========================================
      ! TRAITEMENT DES NOEUDS ET DES AUTRES BIEFS
      ! ===========================================
      ! ITERATIONS TANT QU'IL RESTE DES NOEUDS NON TRAITES
      ! --------------------------------------------------
      nb_noeud_non_traite = nb_noeud
      traiter_defluence   = .false.
      icompt              = 0
      30  CONTINUE
      !********
      icompt = icompt + 1

      boucle_noeud: do inoeud = 1 , nb_noeud
         ! Si le noeud n'est pas encore traite
         if( .not.noeud_traite(inoeud) ) then
            nb_branche_aval = 0
            LINDAM          = 0
            iafflu          = 1
            boucle_afflu : do while( Connect%NumSectionConfluence(inoeud,iafflu) /= 0 )
               num_section = Connect%NumSectionConfluence(inoeud,iafflu)
               num_bief    = Connect%NumBiefConfluence(inoeud,iafflu)
               !-----------------------------------------------------------
               ! ical = 0 si la branche est aval (dans le cas contraire
               ! le traitement des limites libres l'a positionne a 1 ou -1)
               !-----------------------------------------------------------
               if( ical(num_section) == 0 ) then
                  num_section_aval = num_section
                  num_bief_aval    = num_bief
                  if( num_section_aval == Connect%FinBief(num_bief_aval) ) then
                     ! il ne faudra pas traiter ce noeud
                     LINDAM = 1
                  endif
                  nb_branche_aval = nb_branche_aval + 1
               endif
               iafflu = iafflu + 1
               if( iafflu > NbExtNoeud(inoeud) ) then
                  exit boucle_afflu
               endif
            end do boucle_afflu

         ! CHOIX DES NOEUDS A TRAITER
         select case( nb_branche_aval )
            case( 0 )
               !-----------------------------------
               ! IL Y A PROBLEME , ON NE TROUVE PAS
               ! DE BRANCHES AVAL
               !-----------------------------------
               Erreur%Numero = 352
               Erreur%ft     = err_352
               Erreur%ft_c   = err_352c
               call TRAITER_ERREUR( Erreur , inoeud )
               return
            case(1)
               !---------------------------------
               ! IL N'EXISTE QU'UNE BRANCHE AVAL,
               ! ON Y CONNAIT DONC LE DEBIT
               !---------------------------------
               iextremite             = iextremite + 1
               Algorithme(iextremite) = 300 + inoeud
               noeud_traite(inoeud)   = .true.
               nb_noeud_non_traite    = nb_noeud_non_traite - 1
               ! PUIS ON TRAITE LE BIEF AVAL
               !--------------------------------------------
               ! (ON VERIFIE TOUT DE MEME QUE L'EXTREMITE DE
               ! LA BRANCHE EN QUESTION EST DU TYPE AMONT  )
               !----------------------------------------------------------
               ! Comme on n'a qu'une branche aval, num_bief_aval et num_section_aval
               ! sont uniques
               !----------------------------------------------------------
               !--------------------------
               ! SI C'EST UNE BRANCHE AVAL
               !--------------------------
               if( num_section_aval == Connect%OrigineBief(num_bief_aval) ) then
                  ISUB = 100
                  ical(Connect%OrigineBief(num_bief_aval)) =  1
                  ical(Connect%FinBief(num_bief_aval))     = -1
                  iextremite                               = iextremite + 1
                  Algorithme(iextremite)                   = ISUB + num_bief_aval
                  GO TO 30
                  !********
               else
                  !-----------------------
                  ! SI C'EST UN POINT AVAL
                  ! CE N'EST PAS NORMAL
                  !-----------------------
                  Erreur%Numero = 353
                  Erreur%ft     = err_353
                  Erreur%ft_c   = err_353c
                  call TRAITER_ERREUR (Erreur, inoeud)
                  return
               endif
            case default
               !-------------------------------
               ! IL Y A PLUSIEURS BRANCHES AVAL
               !-------------------------------
               if( traiter_defluence .and. LINDAM == 0 ) then
                  ! IL FAUT TRAITER LE NOEUD
                  !-------------------------
                  iextremite             = iextremite + 1
                  Algorithme(iextremite) = 500 + inoeud
                  noeud_traite(inoeud)   = .true.
                  nb_noeud_non_traite    = nb_noeud_non_traite - 1
                  ! PUIS ON TRAITE LES BRANCHES AVAL --
                  iafflu = 1
                  boucle_afflu_2 : do while( Connect%NumBiefConfluence(inoeud,iafflu) /= 0 )
                     num_section = Connect%NumSectionConfluence(inoeud,iafflu)
                     num_bief    = Connect%NumBiefConfluence(inoeud,iafflu)
                     if( ical(num_section) == 0 ) then
                        ! SI C'EST UNE BRANCHE AVAL
                        !--------------------------
                        if( Connect%OrigineBief(num_bief) == num_section ) then
                           ISUB                                = 100
                           ical(Connect%OrigineBief(num_bief)) = 1
                           ical(Connect%FinBief(num_bief))     = -1
                           iextremite                          = iextremite + 1
                           Algorithme(iextremite)              = ISUB + num_bief
                        else
                        ! C'EST UNE BRANCHE AMONT
                        ! CE N'EST PAS NORMAL
                        !------------------------
                           Erreur%Numero = 354
                           Erreur%ft     = err_354
                           Erreur%ft_c   = err_354c
                           call TRAITER_ERREUR (Erreur, inoeud)
                           return
                        endif
                     endif
                     iafflu = iafflu + 1
                     if( iafflu > size(Connect%NumSectionConfluence(1,:)) ) then
                        exit boucle_afflu_2
                     endif
                  end do boucle_afflu_2
                  traiter_defluence = .false.
                  GO TO 30
                  !********
               endif
            end select  ! de if nb_branche_aval = 0
         endif         ! de if noeud non traite
      end do boucle_noeud

      if( icompt > 500 ) then
         Erreur%Numero = 355
         Erreur%ft     = err_355
         Erreur%ft_c   = err_355c
         call TRAITER_ERREUR( Erreur, inoeud )
         return
      endif

      if( nb_noeud_non_traite > 0 ) then
         traiter_defluence = .true.
         GO TO 30
         !********
      endif

      ! ============================
      ! FIN DU TRAITEMENT DES NOEUDS
      ! ============================
   endif     ! de if nb_bief = 1

   ! ==================
   ! TRAITEMENT INVERSE
   ! ==================
   nb_element = iextremite
   do i = 1 , iextremite
      iextremite             = iextremite + 1
      Algorithme(iextremite) = Algorithme(nb_element + 1 - i) + 100
   end do

   ! ==============================
   ! IMPRESSION DU TABLEAU D'APPELS
   ! ==============================
   if (ImpressionReseau) then
      WRITE (UniteListing,2000)
      WRITE (UniteListing,2001)  iextremite
      WRITE (UniteListing,2010) (Algorithme(i),i=1,iextremite)
   endif

   ! Fin des traitements
   !--------------------

   !Erreur%Arbredappel = !arbredappel_old

   return


   ! FORMATS D'ECRITURE
   2000 format (/,'ALGORITHME DE PARCOURS DES BIEFS EN PERMANENT',/, &
             &  '--------------------------------------------',/)
   2001 FORMAT('Nombre d''appels : ',I10,/,                       &
             'Tableau des appels : IAPPEL = ISUB + num bief ou noeud ',/,&
             '             AVEC  ISUB = 100  ==>  CALL QBIEF  ',/,       &
             '                   ISUB = 200  ==>  CALL PERMAT ',/,       &
             '                   ISUB = 300  ==>  CALL QNODE (1) ',/,    &
             '                   ISUB = 400  ==>  CALL QNODE (2) ',/,    &
             '                   ISUB = 500  ==>  CALL QREPAR(1) ',/,    &
             '                   ISUB = 600  ==>  CALL QREPAR(2) ',/,    &
             '  (IAPPEL(I),I=1,iextremite) : ',/)
  2010 FORMAT(10X,10I5)

end subroutine ALGOP
