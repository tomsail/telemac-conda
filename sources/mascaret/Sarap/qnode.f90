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

subroutine QNODE (   &
     Q             , & !/DONNEES MODIFIEES/
     Z             , &
     NumConfluence , & !/DONNEES NON MODIFIEES/
     NumPassage    , &
     Connect       , &
     Erreur          & !/ERREUR/
     )

! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             S. MANDELKERN
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
!
!  FONCTION :
!  --------
!
!        REPARTITION DES DEBITS A UN NOEUD AVEC EGALITE DES COTES :
!
!          CAS AVEC UNE SEULE BRANCHE AVAL
!                   ---------
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! ._______________.____.____._______________________________________________
! !    NOM        !TYPE!MODE!                   ROLE
! !_______________!____!____!______________________________________________
! ! Q             ! R  !<-->! TABLEAU DES DEBITS
! ! Z             ! R  !<-->! TABLEAU DES COTES DE LA SURFACE LIBRE
! ! NumConfluence ! I  ! -->! Numero de la confluence a traiter
! ! NumPassage    ! I  ! -->! VARIABLE INDICATRICE :
! !               !    !    !  = 1  CALCUL DU DEBIT DANS LA BRANCHE AVAL
! !               !    !    !  = 2  CALCUL DES COTES DANS LES BRANCHES AMONT
! ! Connect       ! T  ! -->! Structure contenant la table de connectivite
! ! Erreur        ! T  !<-->! ERREUR
! !_______________!____!____!______________________________________________
!
!                         VARIABLES LOCALES
! ._______________.____.____.______________________________________________
! !               !    !    !
! !_______________!____!____!______________________________________________
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ----------------------
!   SOUS PROGRAMME APPELANT :  PERSAR
!   -------------------------
!   SOUS PROGRAMMES APPELES :  ---
!   -------------------------
!
!***********************************************************************

   !============================= Declarations ===========================
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_MESSAGE_C        ! Messages d'erreur
   use M_CONNECT_T        ! Type CONNECT_T
   use M_ERREUR_T         ! Type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement de l'erreur
   !.. Declarations explicites ..
   !-----------------------------
   implicit none
   !.. Arguments ..
   !---------------
   ! TABLEAUX DIMENSIONNES A NbSection
   real(DOUBLE)       , dimension(:)  , intent(inout) :: Q
   real(DOUBLE)       , dimension(:)  , intent(inout) :: Z
   integer            ,                 intent(in   ) :: NumConfluence
   integer            ,                 intent(in   ) :: NumPassage
   type(CONNECT_T)    ,                 intent(in   ) :: Connect
   type(ERREUR_T)     ,                 intent(inout) :: Erreur
   !.. Variables locales ..
   !-----------------------
   integer        :: ibief
   integer        :: isec
   integer        :: II1
   integer        :: num_sect
   logical        :: type_origine
   real(DOUBLE)   :: QAMONT
   !character(132) :: !arbredappel_old

   !============================= Instructions ===========================
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>QNODE'

   label_NumPassage : if ( NumPassage == 1 ) then
      !=====================================
      !  PREMIER PASSAGE :
      ! CALCUL DU DEBIT DANS LA BRANCHE AVAL
      !=====================================
      QAMONT = 0._DOUBLE

      do ibief = 1 , Connect%NbBiefConfluence(NumConfluence)

         num_sect = Connect%NumSectionConfluence(NumConfluence, ibief)

         ! On cherche si la section num_sect est une origine ou une fin du bief
         type_origine = .false.
         do isec = 1 , size(Connect%OrigineBief)
            if( Connect%OrigineBief(isec) == num_sect ) then
               type_origine = .true.
               exit
            end if
         end do

         if( type_origine ) then          ! Il s'agit de la branche aval unique
            II1 = num_sect
         else                            ! c'est une branche amont :
            QAMONT=QAMONT+Q(num_sect)    ! on somme les debits
         end if

      end do

      Q(II1)=QAMONT                      ! debit aval = somme des debits amont

   else if( NumPassage == 2 ) then label_NumPassage
      !========================================
      !  SECOND PASSAGE :
      ! ON ECRIT L'EGALITE DES COTES AUX NOEUDS
      !========================================

      !    PREMIER PARCOURS DU NOEUD POUR DETERMINER LA BRANCHE AVAL
      do ibief = 1 , Connect%NbBiefConfluence(NumConfluence)
         num_sect = Connect%NumSectionConfluence(NumConfluence, ibief)
         ! On cherche si la section num_sect est une origine ou une fin du bief
         type_origine = .false.
         do isec = 1 , size(Connect%OrigineBief)
            if( Connect%OrigineBief(isec) == num_sect ) then
               type_origine = .true.
               exit
            end if
         end do
         if( type_origine ) then
            ! -- BRANCHE AVAL --
            II1 = num_sect
         end if
      end do

      !    SECOND PARCOURS DU NOEUD , ON REPORTE LA COTE A L'AMONT
      do ibief = 1 , Connect%NbBiefConfluence(NumConfluence)
         num_sect = Connect%NumSectionConfluence(NumConfluence, ibief)
         ! On cherche si la section num_sect est une origine ou une fin du bief
         type_origine = .false.
         do isec = 1, size(Connect%OrigineBief)
            if( Connect%OrigineBief(isec) == num_sect ) then
               type_origine = .true.
               exit
            end if
         end do

         if( .not. type_origine ) then
            ! -- BRANCHE AMONT --
            Z(num_sect) = Z(II1)
         end if
      end do

   else label_NumPassage
      Erreur%Numero = 34
      Erreur%ft     = err_34
      Erreur%ft_c   = err_34c
      call TRAITER_ERREUR( Erreur , NumPassage )
      return
   endif label_NumPassage

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine QNODE
