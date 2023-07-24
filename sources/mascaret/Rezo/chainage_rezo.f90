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

subroutine CHAINAGE_REZO( Connect , Extremite , Matrice , OptionCasier, Liaison , Casier , Erreur )

! *********************************************************************
! PROGICIEL : MASCARET        F. ZAOUI
!                             S. DELMAS     C. COULET
!
! VERSION : V8P4R0               EDF-CEREMA-ARTELIA
! *********************************************************************
! - FONCTION :
!              DETERMINATION DE LA TOPOLOGIE DE LA MATRICE CREUSE
!
!   PROGRAMME APPELANT                : Rezo
!
!   COMMENTAIRES :
!
!      Les termes non-nuls de la matrice sont stockes au format coordonnees
!         'Matrice%rowA' et 'Matrice%colA' sont les tables des indices respectivement
!          de ligne et colonne des elements non-nuls
!

   use M_CONNECT_T           ! Definition du type CONNECT_T
   use M_EXTREMITE_T         ! Definition du type EXTREMITE_T
   use M_REZOMAT_T           ! Definition du type REZOMAT_T
   use M_ERREUR_T            ! Definition du type ERREUR_T
   use M_MESSAGE_C           ! Messages d'erreur pre-definis
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs
   use M_LIAISON_T           ! Definition du type LIAISON_T
   use M_CASIER_T            ! Definition du type CASIER_T
   use M_CONSTANTES_CASIER_C

   implicit none             ! Pas de type implicite par defaut

   type(ERREUR_T) , intent(out)  :: Erreur                       ! Gestion des erreurs
   type(CONNECT_T) , intent(in)  :: Connect                      ! Table descriptive du reseau de biefs
   type(EXTREMITE_T) , dimension(:), intent(inout) :: Extremite  ! Extremites libres
   type(REZOMAT_T) , intent(inout) :: Matrice                      ! Description de la matrice du probleme
   logical                           , intent(in   ) :: OptionCasier
   type(LIAISON_T)  , dimension(:) , pointer , intent(in   ) :: Liaison    ! Liaisons
   type(CASIER_T)   , dimension(:) , pointer , intent(in   ) :: Casier     ! Casiers

   logical test              ! Test des sections des liaisons
   integer i,j,k,iliaison    ! Indices de boucle
   integer incr              ! Compteur d'increment
   integer element           ! Compteur d'elements
   integer ori,ext           ! Origine et extremite d'un bief
   integer retour            ! Code retour suite a erreur lors de l'allocation dynamique
   integer nbEqConflu        ! Nombre d'equations pour modeliser les confluents du reseau
   integer nbEqHydrau        ! Nombre d'equations issues de la discretisation DF SVT
   integer nbExtLibre        ! Nombre d'extremites libres
   integer nbBief            ! Nombre de biefs
   integer nbExtLib          ! Nombre d'extremites libres
   integer nbConflu          ! Nombre de confluents
   integer nbSection         ! Nombre de sections de calcul
   integer nrow              ! Indice pour le parcours des lignes de la matrice
   integer ncol              ! Indice pour le parcours de colonnes de la matrice
   integer nval              ! Indice pour la parcours des valeurs de la matrice
   integer NvalA             ! Taille de valA
   integer nbLiaison         ! Nombre de Liaisons
   integer nbCasier          ! Nombre de Casier
   integer nbEqLiaison       ! Nombre d'equations de liaison
   integer nbEqCasier        ! Nombre d'equation de casier
   integer, dimension(size(liaison)):: mark_liaison
   integer                          :: id_section, compt

   !
   ! Initialisations
   !
   Erreur%Numero = 0
   retour        = 0
   nbEqConflu    = 0
   nbEqHydrau    = 0
   nbEqLiaison   = 0
   nbEqCasier    = 0
   nbExtLibre    = 0
   nbSection     = 0
   nrow          = 0
   ncol          = 0
   nval          = 0
   nbBief        = size( Connect%ORIGINEBIEF )
   nbExtLib      = size( Connect%NUMSECTIONEXTLIBRE )
   nbConflu      = size( Connect%NBBIEFCONFLUENCE )
   nbLiaison     = size( Liaison )
   nbCasier      = size( Casier )
   mark_liaison  = 0
   Matrice%NNZ   = 0

   !
   ! Verification si les liaisons Casiers-Riviere sont sur les noeuds
   !
   if( OptionCasier ) then
      test = .false.
      do i = 1, size(Liaison)
         if ( liaison(i)%NatureLiaison == LIAISON_TYPE_RIVIERE_CASIER ) then
            do j = 1, nbBief
                if ( Connect%ORIGINEBIEF(j) == liaison(i)%CaracRC%Section ) then
                    k = Connect%ORIGINEBIEF(j)
                    test = .true.
                elseif ( Connect%FINBIEF(j)     == liaison(i)%CaracRC%Section ) then
                    k = Connect%FINBIEF(j)
                    test = .true.
                elseif ( Connect%ORIGINEBIEF(j)+1 == liaison(i)%CaracRC%Section ) then
                    k = Connect%ORIGINEBIEF(j)+1
                    test = .true.
                elseif ( Connect%FINBIEF(j)-1     == liaison(i)%CaracRC%Section ) then
                    k = Connect%FINBIEF(j)-1
                    test = .true.
                endif
                if ( test ) then
                    Erreur%Numero = 1
                    Erreur%ft     = err_703
                    Erreur%ft_c   = err_703c
                    call TRAITER_ERREUR( Erreur , i, liaison(i)%CaracRC%Abscisse , j )
                    return
                endif
            enddo
         endif
      enddo
   endif

   !
   ! Determination de l'ordre (N) et du nombre d'elements non nuls (NNZ) de la matrice
   !
   PASS0 : do i = 1,nbBief
      nbEqHydrau  = nbEqHydrau  + 2 * ( Connect%FINBIEF(i) - Connect%ORIGINEBIEF(i) )
      Matrice%NNZ = Matrice%NNZ + 8 * ( Connect%FINBIEF(i) - Connect%ORIGINEBIEF(i) )
      nbSection   = nbSection   + Connect%FINBIEF(i) - Connect%ORIGINEBIEF(i) + 1
   end do PASS0

   Matrice%NNZ = Matrice%NNZ - 2 * nbExtLib

   if( nbConflu > 0 ) then
      PASS1 : do i = 1 , nbConflu
         nbEqConflu  = nbEqConflu  + Connect%NBBIEFCONFLUENCE(i)
         Matrice%NNZ = Matrice%NNZ + Connect%NBBIEFCONFLUENCE(i) + ( Connect%NBBIEFCONFLUENCE(i) - 1 ) * 2
      end do PASS1
   endif

   !
   ! Equation Couplage Casiers
   !
   if( OptionCasier ) then
      do i = 1, size(Liaison)
        if ( mark_liaison(i) .ne. 1 ) then
            if ( liaison(i)%NatureLiaison == LIAISON_TYPE_RIVIERE_CASIER ) then
                id_section = liaison(i)%CaracRC%Section
                compt = 0
                do j = i, size(Liaison) ! optimisation possible en ne reparcourant pas toutes les liaisons
                    if ( liaison(j)%NatureLiaison == LIAISON_TYPE_RIVIERE_CASIER ) then
                        if ( liaison(j)%CaracRC%Section == id_section ) then
                            compt = compt + 1
                            mark_liaison(j) = 1
                        endif
                    endif
                enddo
                Matrice%NNZ = Matrice%NNZ - 8               ! enlevement de 8 elements des 2 eq
                Matrice%NNZ = Matrice%NNZ + 2 + 2 + compt   ! Zam = Zav (+2); Qam - Qav -EQ = (+2 +nbQ)
            endif
        endif
      enddo

      if ( nbLiaison > 0) then    ! equations et elements non-nuls pour les liaisons
          nbEqLiaison = nbLiaison
          Matrice%NNZ = Matrice%NNZ + 3 * (nbLiaison) ! Elements : A, B et C || D va dans le vect b
      endif

      if ( nbCasier > 0) then     ! euqations et elements non-nuls pour les casiers
        nbEqCasier = nbCasier
        do i = 1, nbCasier
            element = 1 + size( Casier(i)%LiaisonRC(:,1) ) + size( Casier(i)%LiaisonCC(:,1) )
            Matrice%NNZ = Matrice%NNZ + element
        enddo
      endif
   endif

   Matrice%N = nbEqHydrau + nbEqConflu + nbEqLiaison + nbEqCasier

   if(Matrice%SOLV.EQ.2) then
      !
      ! Dimensions de travail Y12M
      !
      Matrice%NN  = 4 * Matrice%NNZ
      Matrice%NN1 = Matrice%NN
      Matrice%IHA = Matrice%N
      NvalA       = Matrice%NN

      !
      ! Allocation des tableaux de la structure 'Matrice'
      !
      if(.not.associated(Matrice%IFLAG)) allocate( Matrice%IFLAG( 10 ) , STAT = retour )
      if( retour.ne.0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Matrice_IFLAG' )
         return
      end if
      Matrice%IFLAG(:) = 0
      Matrice%IFLAG(4) = 1

      if(.not.associated(Matrice%AFLAG)) allocate( Matrice%AFLAG( 8 ) , STAT = retour )
      if( retour.ne.0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Matrice_AFLAG' )
         return
      end if
      Matrice%AFLAG(:) = 0._DOUBLE

      if(.not.associated(Matrice%RNR)) allocate( Matrice%RNR( Matrice%NN1 ) , STAT = retour )
      if( retour.ne.0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Matrice_rnr' )
         return
      end if
      Matrice%RNR(:) = 0

      if(.not.associated(Matrice%SNR)) allocate ( Matrice%SNR( Matrice%NN ), STAT = retour)
      if( retour.ne.0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Matrice_snr' )
         return
      end if
      Matrice%SNR(:) = 0

      if(.not.associated(Matrice%pivot)) allocate( Matrice%pivot( Matrice%N ) , STAT = retour )
      if( retour.ne.0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR  (Erreur, 'Matrice_pivot')
         return
      end if
      Matrice%pivot(:) = 0._DOUBLE

      if(.not.associated(Matrice%ha)) allocate( Matrice%ha( Matrice%IHA , 11 ) , STAT = retour )
      if( retour.ne.0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Matrice_ha' )
         return
      end if
      Matrice%ha(:,:) = 0

   else
      !
      ! Dimensions de travail LAPACK
      !
      Matrice%KL   = 2
      Matrice%KU   = 2
      Matrice%LDAB = 2 * Matrice%KL + Matrice%KU + 1
      NvalA        = Matrice%NNZ

      !
      ! Allocation des tableaux de la structure 'Matrice' propre a LAPACK
      !
      if(.not.associated(Matrice%AB)) allocate ( Matrice%AB( Matrice%LDAB , Matrice%N ), STAT = retour)
      if( retour.ne.0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR  (Erreur, 'Matrice_AB')
         return
      end if

      if(.not.associated(Matrice%ipiv)) allocate ( Matrice%ipiv( Matrice%N ), STAT = retour)
      if( retour.ne.0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR  (Erreur, 'Matrice_ipiv')
         return
      end if

   endif

   if(.not.associated(Matrice%noVarDQ)) allocate( Matrice%noVarDQ( nbSection ) , STAT = retour )
   if( retour.ne.0 ) then
     Erreur%Numero = 5
     Erreur%ft     = err_5
     Erreur%ft_c   = err_5c
     call TRAITER_ERREUR( Erreur , 'Matrice_noVarDQ' )
     return
   end if
   Matrice%noVarDQ(:) = 0

   if(.not.associated(Matrice%noVarDZ)) allocate( Matrice%noVarDZ( nbSection ) , STAT = retour )
   if( retour.ne.0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'Matrice_noVarDZ' )
      return
   end if
   Matrice%noVarDZ(:) = 0

   if( OptionCasier ) then
       if(.not.associated(Matrice%noVarDQl)) allocate( Matrice%noVarDQl( nbLiaison ) , STAT = retour )
       if( retour.ne.0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Matrice_noVarDQl' )
         return
       end if
       Matrice%noVarDQl(:) = 0

       if(.not.associated(Matrice%noVarDZc)) allocate( Matrice%noVarDZc( nbCasier ) , STAT = retour )
       if( retour.ne.0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'Matrice_noVarDZc' )
          return
       end if
       Matrice%noVarDZc(:) = 0

       if(.not.associated(Matrice%LiaiSec)) allocate( Matrice%LiaiSec( nbLiaison ) , STAT = retour )
       if( retour.ne.0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'Matrice_LiaiSec' )
            return
       end if
       Matrice%LiaiSec(:) = 0
   endif

   if(.not.associated(Matrice%SecLiai)) allocate( Matrice%SecLiai( nbSection ) , STAT = retour )
   if( retour.ne.0 ) then
        Erreur%Numero = 5
        Erreur%ft     = err_5
        Erreur%ft_c   = err_5c
        call TRAITER_ERREUR( Erreur , 'Matrice_SecLiai' )
        return
   end if
   Matrice%SecLiai(:)   = 0

   if(.not.associated(Matrice%rowA)) allocate( Matrice%rowA( Matrice%NNZ ) , STAT = retour )
   if( retour.ne.0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'Matrice_rowA' )
      return
   end if
   Matrice%rowA(:) = 0



   if(.not.associated(Matrice%colA)) allocate( Matrice%colA( Matrice%NNZ ) , STAT = retour )
   if( retour.ne.0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'Matrice_colA' )
      return
   end if
   Matrice%colA(:) = 0
   if(.not.associated(Matrice%valA)) allocate( Matrice%valA( NvalA ) , STAT = retour )
   if( retour.ne.0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'Matrice_valA' )
      return
   end if
   Matrice%valA(:) = 0._DOUBLE

   if(.not.associated(Matrice%typSec)) allocate( Matrice%typSec( nbSection ) , STAT = retour )
   if( retour.ne.0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'Matrice_typSec' )
      return
   end if
   Matrice%typSec(:)    = 0

   if( nbConflu >= 0 ) then
      if(.not.associated(Matrice%headConflu)) allocate( Matrice%headConflu( nbConflu ) , STAT = retour )
      if( retour.ne.0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Matrice_headConflu' )
         return
      end if
      Matrice%headConflu(:) = 0

      if(.not.associated(Matrice%nextSecConflu)) allocate( Matrice%nextSecConflu( nbSection ) , STAT = retour )
      if( retour.ne.0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Matrice_nextSecConflu' )
         return
      end if
      Matrice%nextSecConflu(:) = 0
   endif

   if(.not.associated(Matrice%b)) allocate( Matrice%b( Matrice%N ) , STAT = retour )
   if( retour.ne.0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'Matrice_b' )
      return
   end if
   Matrice%b(:) = 0._DOUBLE

   !
   ! Determination du type des sections
   !
   PASS2 : do i = 1 , nbConflu
      PASS3 : do j = 1 , Connect%NBBIEFCONFLUENCE(i)
         Matrice%typSec(Connect%NUMSECTIONCONFLUENCE(i,j)) = i
      end do PASS3
   end do PASS2

   PASS4 : do i = 1 , nbExtLib
      select case( Extremite(i)%Type )
         case( CONDITION_TYPE_DEBIT_IMPOSE )
            Matrice%typSec(Connect%NUMSECTIONEXTLIBRE(i)) = -1
         case( CONDITION_TYPE_COTE_IMPOSE )
            Matrice%typSec(Connect%NUMSECTIONEXTLIBRE(i)) = -2
         case( CONDITION_TYPE_COTE_DEBIT , CONDITION_TYPE_DEBIT_COTE )
            Matrice%typSec(Connect%NUMSECTIONEXTLIBRE(i)) = -3
         case( CONDITION_TYPE_ZAVAL_QAMONT )
            Matrice%typSec(Connect%NUMSECTIONEXTLIBRE(i)) = -4
         case( CONDITION_TYPE_NORMALE )
            Matrice%typSec(Connect%NUMSECTIONEXTLIBRE(i)) = -5
      end select
   end do PASS4

   if( OptionCasier ) then
       PASS20 : do i = 1, size(Liaison)
            if ( liaison(i)%NatureLiaison == LIAISON_TYPE_RIVIERE_CASIER ) then
                Matrice%typSec( liaison(i)%CaracRC%Section ) = - 10
                Matrice%SecLiai( liaison(i)%CaracRC%Section )= Matrice%SecLiai( liaison(i)%CaracRC%Section ) + 1
                Matrice%LiaiSec(i) = liaison(i)%CaracRC%Section
            endif
       enddo PASS20
   endif

   !
   ! Determination du type origine (-) ou extremite (+) des sections pour chaque confluent
   !
   if( nbConflu > 0 ) then
      Matrice%headConflu(:)    = 0
      Matrice%nextSecConflu(:) = 0
      PASS10 : do i = 1 , nbConflu
         PASS11 : do j = 1 , Connect%NBBIEFCONFLUENCE(i)
            incr = Connect%NUMSECTIONCONFLUENCE(i,j)
            PASS12 : do k = 1 , nbBief
               ori = Connect%ORIGINEBIEF(k)
               ext = Connect%FINBIEF(k)
               if( ori.eq.incr ) then
                  Matrice%nextSecConflu(incr) = Matrice%headConflu(i)
                  Matrice%headConflu(i)       = -ori
                  exit PASS12
               elseif( ext.eq.incr ) then
                  Matrice%nextSecConflu(incr) = Matrice%headConflu(i)
                  Matrice%headConflu(i)       = ext
                  exit PASS12
               endif
            enddo PASS12
         end do PASS11
      end do PASS10
   endif

   !
   ! Chainage de la matrice
   !
   ! 1ere partie : Equations de Saint-Venant discretisees
   PASS5 : do k = 1 , nbBief
      ori = Connect%ORIGINEBIEF(k)
      ext = Connect%FINBIEF(k)
      PASS6 : do j = ori , ext - 1
         i    = j + 1
         incr = 0

         if( Matrice%typSec(j).ne.-10 ) then ! non reliee a une liaison
             ! I et N !
             if( Matrice%typSec(j).ne.-1 ) then
                incr               = incr + 1
                nval               = nval + 1
                Matrice%noVarDQ(j) = ncol + incr
                Matrice%rowA(nval) = nrow + 1
                Matrice%colA(nval) = ncol + incr
                nval               = nval + 1
                Matrice%rowA(nval) = nrow + 2
                Matrice%colA(nval) = ncol + incr
             endif
             ! J et O !
             if( Matrice%typSec(j).gt.-2 ) then
                incr               = incr + 1
                nval               = nval + 1
                Matrice%noVarDZ(j) = ncol + incr
                Matrice%rowA(nval) = nrow + 1
                Matrice%colA(nval) = ncol + incr
                nval               = nval + 1
                Matrice%rowA(nval) = nrow + 2
                Matrice%colA(nval) = ncol + incr
             endif
             ! G et L !
             if( Matrice%typSec(i).ne.-1 ) then
                incr               = incr + 1
                nval               = nval + 1
                Matrice%noVarDQ(i) = ncol + incr
                Matrice%rowA(nval) = nrow + 1
                Matrice%colA(nval) = ncol + incr
                nval               = nval + 1
                Matrice%rowA(nval) = nrow + 2
                Matrice%colA(nval) = ncol + incr
             endif

             ! H et M !
             if( Matrice%typSec(i).gt.-2.or. Matrice%typSec(i).eq. -10 ) then
                incr               = incr + 1
                nval               = nval + 1
                Matrice%noVarDZ(i) = ncol + incr
                Matrice%rowA(nval) = nrow + 1
                Matrice%colA(nval) = ncol + incr
                nval               = nval + 1
                Matrice%rowA(nval) = nrow + 2
                Matrice%colA(nval) = ncol + incr
             endif

         elseif ( Matrice%typSec(j).eq.-10 ) then ! Reliee a une liaison

             ! positionnnement des variables
             Matrice%noVarDQ(j) = ncol + 1
             Matrice%noVarDZ(j) = ncol + 2
             Matrice%noVarDQ(i) = ncol + 3
             Matrice%noVarDZ(i) = ncol + 4

             nval               = nval + 1
             Matrice%rowA(nval) = nrow + 1
             Matrice%colA(nval) = Matrice%noVarDQ(j)

             nval               = nval + 1
             Matrice%rowA(nval) = nrow + 1
             Matrice%colA(nval) = Matrice%noVarDQ(i)

             ! parcours sur les Ql :
             do iliaison = 1, NbLiaison
                if ( liaison(iliaison)%NatureLiaison == LIAISON_TYPE_RIVIERE_CASIER ) then
                    if ( liaison(iliaison)%CaracRC%Section == j ) then
                        nval               = nval + 1
                        Matrice%rowA(nval) = nrow + 1
                        Matrice%colA(nval) = nbEqHydrau + nbEqConflu + iliaison
                    endif
                endif
             enddo

             ! egalite des cotes
             nval                = nval + 1
             Matrice%rowA(nval)  = nrow + 2
             Matrice%colA(nval)  = Matrice%noVarDZ(j)
             nval                = nval + 1
             Matrice%rowA(nval)  = nrow + 2
             Matrice%colA(nval)  = Matrice%noVarDZ(i)

          endif

         nrow = nrow + 2

         if( j.eq.ori.and.Matrice%typSec(j) < 0 ) then
            ncol = ncol + 1
         elseif( i.eq.ext.and.Matrice%typSec(i) > 0 ) then
            ncol = ncol + 4
         elseif( i.eq.ext.and.Matrice%typSec(i) < 0 ) then
            ncol = ncol + 3
         elseif( Matrice%typSec(j) == -10 ) then
            ncol = ncol + 2
         else
            ncol = ncol + 2
         endif
      end do PASS6
   end do PASS5

   ! 2eme partie : Traitement des confluents (egalite des cotes et repartition des debits)

   if( nbConflu > 0 ) then
      PASS7 : do k = 1 , nbConflu
         ! Une equation pour la repartition conservative des debits
         nrow = nrow + 1
         j    = iabs( Matrice%headConflu(k) )
         do while( j.ne.0)
            nval               = nval + 1
            Matrice%rowA(nval) = nrow
            Matrice%colA(nval) = Matrice%noVarDQ(j)
            j                  = iabs( Matrice%nextSecConflu(j) )
         end do
         ! (nbBief(k)-1) equations pour l'egalite des cotes
         j = iabs( Matrice%headConflu(k) )
         i = iabs( Matrice%nextSecConflu(j) )
         do while( i.ne.0 )
            nrow               = nrow + 1
            nval               = nval + 1
            Matrice%rowA(nval) = nrow
            Matrice%colA(nval) = Matrice%noVarDZ(j)
            nval               = nval + 1
            Matrice%rowA(nval) = nrow
            Matrice%colA(nval) = Matrice%noVarDZ(i)
            j                  = i
            i                  = iabs( Matrice%nextSecConflu(i) )
         end do
      end do PASS7
   endif

   if( OptionCasier ) then
       ! Determination des positions des variables Ql et Zc :
        do i = 1, Nbliaison
            Matrice%noVarDQl(i) = max ( maxval(Matrice%noVarDQ(:)) , maxval(Matrice%noVarDZ(:)) ) + i
        enddo
        do i = 1, NbCasier
            Matrice%noVarDZc(i) = maxval( Matrice%noVarDQl(:) ) + i
        enddo

        ! 3eme partie : traitement des liaisons
        if ( NbLiaison > 0 ) then
            PASS21 : do i = 1, NbLiaison
                nrow                = nrow + 1
                nval                = nval + 1
                Matrice%rowA(nval)  = nrow
                Matrice%colA(nval)  = Matrice%noVarDQl(i)


                if ( Liaison(i)%NatureLiaison == LIAISON_TYPE_RIVIERE_CASIER ) then
                    nval                = nval + 1
                    Matrice%rowA(nval)  = nrow
                    Matrice%colA(nval)  = Matrice%noVarDZ( Liaison(i)%CaracRC%Section )
                    nval                = nval + 1
                    Matrice%rowA(nval)  = nrow
                    Matrice%colA(nval)  = Matrice%noVarDZc( Liaison(i)%CaracRC%NumCasier )
                else
                    nval                = nval + 1
                    Matrice%rowA(nval)  = nrow
                    Matrice%colA(nval)  = Matrice%noVarDZc( Liaison(i)%CaracCC%CasierOrigine )
                    nval                = nval + 1
                    Matrice%rowA(nval)  = nrow
                    Matrice%colA(nval)  = Matrice%noVarDZc( Liaison(i)%CaracCC%CasierFin )
                endif
            end do PASS21
        endif

        ! 4eme partie : traitement des casiers
        if ( NbCasier > 0 ) then
            PASS22 : do i = 1, NbCasier
                nrow               = nrow + 1
                nval               = nval + 1
                Matrice%rowA(nval)  = nrow
                Matrice%colA(nval)  = Matrice%noVarDZc(i)

                do j = 1, size( Casier(i)%LiaisonRC(:,1) )
                    nval                = nval + 1
                    Matrice%rowA(nval)  = nrow
                    Matrice%colA(nval)  = Matrice%noVarDQl( Casier(i)%LiaisonRC(j,1) )
                enddo
                do j = 1, size( Casier(i)%LiaisonCC(:,1) )
                    nval                = nval + 1
                    Matrice%rowA(nval)  = nrow
                    Matrice%colA(nval)  = Matrice%noVarDQl( Casier(i)%LiaisonCC(j,1) )
                enddo
            end do PASS22
        endif

   endif

   return

end subroutine
