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
!                            F. ZAOUI
!
! VERSION : V8P4R0              EDF-CEREMA
! *********************************************************************
subroutine LEC_LOI_INTERFACE    ( &

     LoiHydrau        , & ! Tableau des lois hydrauliques
     FichiersLois     , & ! Fichiers des lois hydrauliques
     impression_hydrau, & ! Flag d'impression des lois
     UniteListing     , & ! Unite logique fichier listing
     CritereArret     , & ! Critere d'arret du calcul
     TempsMaximum     , & ! Temps maximum du calcul
     unitNum          , & ! Unite logique .xcas
     Erreur             & ! Erreur
                      )
! *********************************************************************
! PROGICIEL : MASCARET       J.-M. LACOMBE
!
! VERSION : V8P4R0              EDF-CEREMA
! *********************************************************************

   !========================= Declarations ===========================
   use M_PRECISION
   use M_ERREUR_T            ! Type ERREUR_T
   use M_FICHIER_T           ! UniteListing
   use M_LOI_T               ! Types LOI_T
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_TRAITER_ERREUR_I    ! Traitement de l'errreur
   use M_LEC_HYDRAU_I        ! Interface de sous-programme
   use M_XCAS_S

  implicit none

! Arguments

  type(LOI_T)    , dimension(:), pointer       :: LoiHydrau
  type(FICHIER_T), dimension(:), pointer       :: FichiersLois
  logical                      , intent(in   ) :: impression_hydrau
  integer                      , intent(in   ) :: UniteListing
  integer                      , intent(in   ) :: CritereArret
  real(DOUBLE)                 , intent(in   ) :: TempsMaximum
  integer, intent(in)                          :: unitNum
! Variables locales

  integer :: nb_loi   ! nombre de lois
  integer :: nb_point ! nombre de points
  integer :: iloi     ! compteur sur les lois
  integer :: iFichiersloi ! compteur sur les lois

  integer :: i        ! compteur sur les points
  integer :: retour   ! code de retour des fonctions intrinseques
  integer :: mode_entree_loi ! type d'entree clavier/fichier
  integer :: unite_temps     ! unite de temps des lois entres par clavier
  character(132) :: arbredappel_old
  character(len=256)  :: pathNode
  character(len=8192) :: line
! Traitement des erreurs

  type(ERREUR_T), intent(inout) :: Erreur

!========================= Instructions ===========================

! INITIALISATION
! --------------

  Erreur%Numero = 0
!   arbredappel_old = trim(Erreur%arbredappel)
!   Erreur%arbredappel = trim(Erreur%arbredappel)//'=>LEC_LOI'

  if (UniteListing >0) write(UniteListing,10000)

! Nombre de lois
!---------------

  pathNode = 'parametresLoisHydrauliques/nb'
  line = xcasReader(unitNum, pathNode)
  read(unit=line, fmt=*) nb_loi
  if (nb_loi < 2) then
    Erreur%Numero = 305
    Erreur%ft   = err_305
    Erreur%ft_c = err_305c
    call TRAITER_ERREUR  (Erreur, 'Nombre de lois', 'superieurs ou egaux a 2')
    return
  end if

  if (impression_hydrau) then
    write(UniteListing,10010) nb_loi
  endif


! Allocation des lois
!--------------------
  retour = 0
  if(.not.associated(LoiHydrau)) allocate (LoiHydrau(nb_loi), STAT = retour)
  if (retour /= 0) then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR  (Erreur, 'LoiHydrau')
    return
  end if

  iFichiersloi = 1

  pathNode = 'parametresLoisHydrauliques/lois'
  line = xcasReader(unitNum, pathNode)
  if(len(trim(line)).eq.0) then
     call xerror(Erreur)
     return
  endif

  do iloi = 1, nb_loi


    nullify(LoiHydrau(iloi)%CoteInf)
    nullify(LoiHydrau(iloi)%CoteSup)
    nullify(LoiHydrau(iloi)%Temps)
    nullify(LoiHydrau(iloi)%Debit)
    nullify(LoiHydrau(iloi)%Cote)
    nullify(LoiHydrau(iloi)%CoteAval)
    nullify(LoiHydrau(iloi)%CoteAmont)

    if(iloi.eq.1) then
      pathNode = 'parametresLoisHydrauliques/lois/structureParametresLoi/nom'
      LoiHydrau(iloi)%Nom = xcasReader(unitNum, pathNode)
    else
      pathNode = 'structureParametresLoi/nom'
      LoiHydrau(iloi)%Nom = xcasReader(unitNum, pathNode, 1)
    endif

    if(iloi.eq.1) then
      pathNode = 'parametresLoisHydrauliques/lois/structureParametresLoi/type'
      line = xcasReader(unitNum, pathNode)
    else
      pathNode = 'type'
      line = xcasReader(unitNum, pathNode, 0)
    endif
    read(unit=line, fmt=*) LoiHydrau(iloi)%Type

    if (LoiHydrau(iloi)%Type < 1 .or. LoiHydrau(iloi)%Type > LOI_TYPE_NB_MAX) then
      Erreur%Numero = 317
      Erreur%ft   = err_317
      Erreur%ft_c = err_317c
      call TRAITER_ERREUR  (Erreur, iloi)
      return
    end if

    if(iloi.eq.1) then
      pathNode = 'parametresLoisHydrauliques/lois/structureParametresLoi/donnees/modeEntree'
      line = xcasReader(unitNum, pathNode)
    else
      pathNode = 'donnees/modeEntree'
      line = xcasReader(unitNum, pathNode, 0)
    endif
    read(unit=line, fmt=*) mode_entree_loi

    if (mode_entree_loi /= SAISIE_PAR_FICHIER .and. &
        mode_entree_loi /= SAISIE_PAR_CLAVIER) then
      Erreur%Numero = 318
      Erreur%ft   = err_318
      Erreur%ft_c = err_318c
      call TRAITER_ERREUR  (Erreur, iloi)
      return
    end if

    if (impression_hydrau) then
      write(UniteListing,10020) iloi, LoiHydrau(iloi)%Nom, LoiHydrau(iloi)%Type
    endif


    if (mode_entree_loi == SAISIE_PAR_FICHIER) then

!     FichierLoiHydrau%Nom = MOTCAR(ADRESS(4,25 +2*(iloi-1)))

      if (impression_hydrau) then
        write(UniteListing,10030) 'PAR FICHIER', FichiersLois(iFichiersloi)%Nom
      endif

      call LEC_HYDRAU          ( &
           LoiHydrau(iloi)                , &
           unite_temps                    , &
           FichiersLois(iFichiersloi )    , &
           impression_hydrau              , &
           UniteListing                   , &
           Erreur                &
                               )
      iFichiersloi  = iFichiersloi +1
      if (Erreur%Numero /= 0) then
        return
      endif


!    else if (mode_entree_loi == SAISIE_PAR_CLAVIER) then
!
!      if (impression_hydrau) then
!        write(UniteListing,10040) 'PAR CLAVIER'
!      endif
!
!      unite_temps = MOTINT(ADRESS(1,230 + iloi - 1))
!
!      if (unite_temps <= 0 .or. unite_temps > LOI_UNITE_NB_MAX) then
!        Erreur%Numero = 316
!        Erreur%ft   = err_316
!        Erreur%ft_c = err_316c
!        call TRAITER_ERREUR  (Erreur, iloi, LOI_UNITE_NB_MAX)
!        return
!      end if
!
!      if (impression_hydrau) then
!        select case (unite_temps)
!        case(LOI_UNITE_SECONDE)
!          write(UniteListing,10045) 'SECONDE'
!        case(LOI_UNITE_MINUTE)
!          write(UniteListing,10045) 'MINUTE'
!        case(LOI_UNITE_HEURE)
!          write(UniteListing,10045) 'HEURE'
!        case(LOI_UNITE_JOUR)
!          write(UniteListing,10045) 'JOUR'
!        end select
!      endif
!
!      nb_point = MOTINT(ADRESS(1,61 +3*(iloi-1)))
!
!      if (nb_point <= 0) then
!        Erreur%Numero = 319
!        Erreur%ft   = err_319
!        Erreur%ft_c = err_319c
!        call TRAITER_ERREUR  (Erreur, iloi)
!        return
!      end if
!
!      if (impression_hydrau) then
!        write(UniteListing,10050) nb_point
!      endif
!
!      ! Allocations
!      !------------
!
!      Select case (LoiHydrau(iloi)%Type)
!
!      case (LOI_TYPE_HYDROGRAMME)
!
!        allocate(LoiHydrau(iloi)%Temps(nb_point), STAT = retour)
!
!        if (retour /= 0) then
!          Erreur%Numero = 5
!          Erreur%ft   = err_5
!          Erreur%ft_c = err_5c
!          call TRAITER_ERREUR  (Erreur, 'LoiHydrau%Temps')
!          return
!        end if
!
!        allocate(LoiHydrau(iloi)%Debit (nb_point), STAT = retour)
!
!        if (retour /= 0) then
!          Erreur%Numero = 5
!          Erreur%ft   = err_5
!          Erreur%ft_c = err_5c
!          call TRAITER_ERREUR  (Erreur, 'LoiHydrau%Debit')
!          return
!        end if
!
!        allocate(LoiHydrau(iloi)%Cote (0), STAT = retour)
!
!
!      case (LOI_TYPE_LIMNIGRAMME)
!
!        allocate(LoiHydrau(iloi)%Temps(nb_point), STAT = retour)
!
!        if (retour /= 0) then
!          Erreur%Numero = 5
!          Erreur%ft   = err_5
!          Erreur%ft_c = err_5c
!          call TRAITER_ERREUR  (Erreur, 'LoiHydrau%Temps')
!          return
!        end if
!
!        allocate(LoiHydrau(iloi)%Cote(nb_point), STAT = retour)
!
!        if (retour /= 0) then
!          Erreur%Numero = 5
!          Erreur%ft   = err_5
!          Erreur%ft_c = err_5c
!          call TRAITER_ERREUR  (Erreur, 'LoiHydrau%Cote')
!          return
!        end if
!
!        allocate(LoiHydrau(iloi)%Debit (0), STAT = retour)
!
!      case (LOI_TYPE_LIMNHYDROGRAMME)
!
!        allocate(LoiHydrau(iloi)%Temps(nb_point), STAT = retour)
!
!        if (retour /= 0) then
!          Erreur%Numero = 5
!          Erreur%ft   = err_5
!          Erreur%ft_c = err_5c
!          call TRAITER_ERREUR  (Erreur, 'LoiHydrau%Temps')
!          return
!        end if
!
!        allocate(LoiHydrau(iloi)%Debit(nb_point), STAT = retour)
!
!        if (retour /= 0) then
!          Erreur%Numero = 5
!          Erreur%ft   = err_5
!          Erreur%ft_c = err_5c
!          call TRAITER_ERREUR  (Erreur, 'LoiHydrau%Debit')
!          return
!        end if
!
!        allocate(LoiHydrau(iloi)%Cote(nb_point), STAT = retour)
!
!        if (retour /= 0) then
!          Erreur%Numero = 5
!          Erreur%ft   = err_5
!          Erreur%ft_c = err_5c
!          call TRAITER_ERREUR  (Erreur, 'LoiHydrau%Cote')
!          return
!        end if
!
!      case (LOI_TYPE_TARAGE_Z_Q, LOI_TYPE_TARAGE_Q_Z)
!
!        allocate(LoiHydrau(iloi)%Temps(0), STAT = retour)
!
!        allocate(LoiHydrau(iloi)%Debit(nb_point), STAT = retour)
!
!        if (retour /= 0) then
!          Erreur%Numero = 5
!          Erreur%ft   = err_5
!          Erreur%ft_c = err_5c
!          call TRAITER_ERREUR  (Erreur, 'LoiHydrau%Debit')
!          return
!        end if
!
!        allocate(LoiHydrau(iloi)%Cote(nb_point), STAT = retour)
!
!        if (retour /= 0) then
!          Erreur%Numero = 5
!          Erreur%ft   = err_5
!          Erreur%ft_c = err_5c
!          call TRAITER_ERREUR  (Erreur, 'LoiHydrau%Cote')
!          return
!        end if
!
!      case (LOI_TYPE_ZAMONT_ZAVAL_Q)
!
!       nb_point_q = MOTINT(ADRESS(1,282 +(iloi-1)))
!	   nb_point_z = nb_point/nb_point_q
!
!	   allocate(LoiHydrau(iloi)%CoteAval(nb_point_z), STAT = retour)
!         if (retour /= 0) then
!          Erreur%Numero = 5
!          Erreur%ft   = err_5
!          Erreur%ft_c = err_5c
!          call TRAITER_ERREUR  (Erreur, 'Loi%CoteAval')
!          return
!        end if
!
!       allocate(LoiHydrau(iloi)%Debit(nb_point_q), STAT = retour)
!         if (retour /= 0) then
!          Erreur%Numero = 5
!          Erreur%ft   = err_5
!          Erreur%ft_c = err_5c
!          call TRAITER_ERREUR  (Erreur, 'Loi%Debit')
!          return
!        end if
!
!		allocate(LoiHydrau(iloi)%CoteAmont(nb_point_q,nb_point_z), STAT = retour)
!
!         if (retour /= 0) then
!          Erreur%Numero = 5
!          Erreur%ft   = err_5
!          Erreur%ft_c = err_5c
!          call TRAITER_ERREUR  (Erreur, 'Loi%CoteAmont')
!          return
!        end if
!
!		case(LOI_TYPE_ZINF_ZSUP_T)
!
!		allocate(LoiHydrau(iloi)%Temps(nb_point), STAT = retour)
!         if (retour /= 0) then
!          Erreur%Numero = 5
!          Erreur%ft   = err_5
!          Erreur%ft_c = err_5c
!          call TRAITER_ERREUR  (Erreur, 'Loi%Temps')
!          return
!        end if
!
!       allocate(LoiHydrau(iloi)%CoteSup(nb_point), STAT = retour)
!         if (retour /= 0) then
!          Erreur%Numero = 5
!          Erreur%ft   = err_5
!          Erreur%ft_c = err_5c
!          call TRAITER_ERREUR  (Erreur, 'Loi%CoteSup')
!          return
!        end if
!
!		allocate(LoiHydrau(iloi)%CoteInf(nb_point), STAT = retour)
!         if (retour /= 0) then
!          Erreur%Numero = 5
!          Erreur%ft   = err_5
!          Erreur%ft_c = err_5c
!          call TRAITER_ERREUR  (Erreur, 'Loi%CoteInf')
!          return
!        end if
!      end select
!
!
!      ! Affectations
!      !-------------
!
!      select case(LoiHydrau(iloi)%Type)
!
!      case (LOI_TYPE_HYDROGRAMME)
!
!        if (impression_hydrau) then
!          write(UniteListing,10060) '            Temps   Debit'
!        endif
!
!        do i=1,nb_point
!          LoiHydrau(iloi)%Temps(i) = MOTREA(ADRESS(2,25+3*(iloi-1))+i-1)
!          LoiHydrau(iloi)%Debit(i) = MOTREA(ADRESS(2,27+3*(iloi-1))+i-1)
!          if (impression_hydrau) then
!            write(UniteListing,10070) i, LoiHydrau(iloi)%Temps(i), &
!                                   LoiHydrau(iloi)%Debit(i)
!          endif
!        end do
!
!
!      case (LOI_TYPE_LIMNIGRAMME)
!
!        if (impression_hydrau) then
!          write(UniteListing,10060) '            Temps   Cote'
!        endif
!
!        do i=1,nb_point
!          LoiHydrau(iloi)%Temps(i) = MOTREA(ADRESS(2,25+3*(iloi-1))+i-1)
!          LoiHydrau(iloi)%Cote(i)  = MOTREA(ADRESS(2,26+3*(iloi-1))+i-1)
!          if (impression_hydrau) then
!            write(UniteListing,10070) i, LoiHydrau(iloi)%Temps(i), &
!                                         LoiHydrau(iloi)%Cote(i)
!          endif
!        end do
!
!
!      case (LOI_TYPE_TARAGE_Z_Q)
!
!        ! Controle du nombre de points minimum
!        !-------------------------------------
!
!        if (nb_point <= 1) then
!          Erreur%Numero = 21
!          Erreur%ft   = err_21
!          Erreur%ft_c = err_21c
!          call TRAITER_ERREUR  (Erreur, LoiHydrau(iloi)%Nom)
!          return
!        end if
!
!        if (impression_hydrau) then
!          write(UniteListing,10060) '            Debit   Cote'
!        endif
!
!        do i=1,nb_point
!          LoiHydrau(iloi)%Debit(i) = MOTREA(ADRESS(2,27+3*(iloi-1))+i-1)
!          LoiHydrau(iloi)%Cote(i)  = MOTREA(ADRESS(2,26+3*(iloi-1))+i-1)
!          if (impression_hydrau) then
!            write(UniteListing,10070) i, LoiHydrau(iloi)%Debit(i), &
!                                         LoiHydrau(iloi)%Cote(i)
!          endif
!        end do
!
!
!      case (LOI_TYPE_TARAGE_Q_Z)
!        ! Controle du nombre de points minimum
!        !-------------------------------------
!
!        if (nb_point <= 1) then
!          Erreur%Numero = 21
!          Erreur%ft   = err_21
!          Erreur%ft_c = err_21c
!          call TRAITER_ERREUR  (Erreur, LoiHydrau(iloi)%Nom)
!          return
!        end if
!
!        if (impression_hydrau) then
!          write(UniteListing,10060) '            Cote   Debit'
!        endif
!
!        do i=1,nb_point
!          LoiHydrau(iloi)%Cote(i)  = MOTREA(ADRESS(2,26+3*(iloi-1))+i-1)
!          LoiHydrau(iloi)%Debit(i) = MOTREA(ADRESS(2,27+3*(iloi-1))+i-1)
!          if (impression_hydrau) then
!            write(UniteListing,10070) i, LoiHydrau(iloi)%Cote(i), &
!                                         LoiHydrau(iloi)%Debit(i)
!          endif
!        end do
!
!
!      case (LOI_TYPE_LIMNHYDROGRAMME)
!
!        if (impression_hydrau) then
!          write(UniteListing,10060) 'Temps   Cote   Debit'
!        endif
!
!        do i=1,nb_point
!          LoiHydrau(iloi)%Temps(i) = MOTREA(ADRESS(2,25+3*(iloi-1))+i-1)
!          LoiHydrau(iloi)%Cote(i)  = MOTREA(ADRESS(2,26+3*(iloi-1))+i-1)
!          LoiHydrau(iloi)%Debit(i) = MOTREA(ADRESS(2,27+3*(iloi-1))+i-1)
!          if (impression_hydrau) then
!            write(UniteListing,10080) i, LoiHydrau(iloi)%Temps(i), &
!                                   LoiHydrau(iloi)%Cote(i),  &
!                                   LoiHydrau(iloi)%Debit(i)
!          endif
!        end do
!
!
!		case (LOI_TYPE_ZAMONT_ZAVAL_Q)
!
!		do i=1,nb_point_q
!
!		   do j=1,nb_point_z
!		   k = (i-1)*nb_point_z+ j
!		   LoiHydrau(iloi)%Debit(i)    = MOTREA(ADRESS(2,27+3*(iloi-1))+k-1)
!		   LoiHydrau(iloi)%CoteAval(j) = MOTREA(ADRESS(2,351 +(iloi-1))+k-1)
!		   LoiHydrau(iloi)%CoteAmont(i,j) = MOTREA(ADRESS(2,26+3*(iloi-1))+k-1)
!
!		   end do
!		end do
!
!        case (LOI_TYPE_ZINF_ZSUP_T)
!
!        do i=1,nb_point
!          LoiHydrau(iloi)%Temps(i) = MOTREA(ADRESS(2,25+3*(iloi-1))+i-1)
!          LoiHydrau(iloi)%CoteSup(i)  = MOTREA(ADRESS(2,351+3*(iloi-1))+i-1)
!          LoiHydrau(iloi)%CoteInf(i) = MOTREA(ADRESS(2,26+3*(iloi-1))+i-1)
!          if (impression_hydrau) then
!            write(UniteListing,10080) i, LoiHydrau(iloi)%Temps(i), &
!                                         LoiHydrau(iloi)%CoteSup(i),  &
!                                         LoiHydrau(iloi)%CoteInf(i)
!          endif
!        end do
!      end select
!
    endif ! de mode de saisie


    !---------------------------------------------------------
    ! Controles communs des lois quelquesoit le mode de saisie
    !---------------------------------------------------------


    ! Controles sur les courbes de tarage
    !------------------------------------

    if(LoiHydrau(iloi)%Type == LOI_TYPE_TARAGE_Z_Q     .or.  &
       LoiHydrau(iloi)%Type == LOI_TYPE_TARAGE_Q_Z) then

      nb_point = size(LoiHydrau(iloi)%Cote(:))

      ! Detection de points de rebroussements

      do i = 2,nb_point

        if ((LoiHydrau(iloi)%Cote(i) <= LoiHydrau(iloi)%Cote(i-1)) .or. &
            (LoiHydrau(iloi)%Debit(i) <= LoiHydrau(iloi)%Debit(i-1))) then
          Erreur%Numero = 22
          Erreur%ft   = err_22
          Erreur%ft_c = err_22c
          call TRAITER_ERREUR  (Erreur, LoiHydrau(iloi)%Nom, i)
          return
        end if

      end do

    endif

    ! Controles sur les Lois temporelles
    !-----------------------------------

    if(LoiHydrau(iloi)%Type == LOI_TYPE_LIMNIGRAMME     .or.  &
       LoiHydrau(iloi)%Type == LOI_TYPE_HYDROGRAMME     .or.  &
       LoiHydrau(iloi)%Type == LOI_TYPE_LIMNHYDROGRAMME .or.  &
       LoiHydrau(iloi)%Type == LOI_TYPE_ZINF_ZSUP_T) then

      nb_point = size(LoiHydrau(iloi)%Temps(:))

      ! Detection de points de rebroussements

      do i = 2,nb_point

        if (LoiHydrau(iloi)%Temps(i) <= &
          LoiHydrau(iloi)%Temps(i-1)) then
          Erreur%Numero = 22
          Erreur%ft   = err_22
          Erreur%ft_c = err_22c
          call TRAITER_ERREUR  (Erreur, LoiHydrau(iloi)%Nom, i)
          return
        end if

      end do

    ! Passage du temps en secondes

      if (unite_temps /= LOI_UNITE_SECONDE) then

        select case (unite_temps)

        case(LOI_UNITE_MINUTE)
          do i = 1,nb_point
            LoiHydrau(iloi)%Temps(i) = LoiHydrau(iloi)%Temps(i) * 60.
          end do
        case(LOI_UNITE_HEURE)
          do i = 1,nb_point
            LoiHydrau(iloi)%Temps(i) = LoiHydrau(iloi)%Temps(i) * 3600.
          end do
        case(LOI_UNITE_JOUR)
          do i = 1,nb_point
            LoiHydrau(iloi)%Temps(i) = LoiHydrau(iloi)%Temps(i) * 86400.
          end do

        end select

      endif  ! de unite de temps

      ! Coherence avec le nombre de pas de temps de la simulation

      if(CritereArret == TEMPS_MAXIMUM            .and. &
        LoiHydrau(iloi)%Temps(nb_point) < TempsMaximum) then
        Erreur%Numero = 303
        Erreur%ft   = err_303
        Erreur%ft_c = err_303c
        call TRAITER_ERREUR  (Erreur, iloi, trim(LoiHydrau(iloi)%Nom))
        return
      end if

    endif    ! de lois temporelles

  end do

  Erreur%Arbredappel = arbredappel_old

  return

  10000 format (/,'LOIS HYDRAULIQUES',/, &
               &  '-----------------',/)
  10010 format ('Nombre de lois = ',i3)
  10020 format (/,'Loi ',i3,' : Nom = ',A,' Type =',i2)
  10030 format ('Mode d''entree      = ',A,' Nom du fichier = ',A)

   contains

   subroutine xerror(Erreur)

       use M_MESSAGE_C
       use M_ERREUR_T            ! Type ERREUR_T

       type(ERREUR_T)                   , intent(inout) :: Erreur

       Erreur%Numero = 704
       Erreur%ft     = err_704
       Erreur%ft_c   = err_704c
       call TRAITER_ERREUR( Erreur )

       return

   end subroutine xerror

  end subroutine LEC_LOI_INTERFACE
