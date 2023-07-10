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

SUBROUTINE CALAGE_N2QN1( &
                        z , &   ! Cote de la surface libre
                       q1 , &   ! Debit mineur
                       q2 , &   ! Debit majeur
                   qinjec , &   ! Qinjecte
                   pcsing , &   ! Pertes de charge singulieres
               impression   &   ! Flag d'impression
                           )
!
! *********************************************************************
! PROGICIEL : MASCARET         F. DEMANGEON
!                              F. ZAOUI
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
! FONCTION :
! --------
!
! SOUS-PROGRAMME DE BASE : ESTIMATION ITERATIVE DES COEFFICIENTS DE
! RUGOSITE MINEURS OU MAJEURS
!
! *********************************************************************
!
!
! COMMENTAIRES : LE PRINCIPE EST DE MINIMISER LA FONCTION DE COUT
! ------------   EGALE A LA SOMME DES CARRES DE (ZMES-ZCAL) , OU
!                ZMES EST UNE COTE MESUREE , ET ZCAL LA COTE
!                CALCULEE ASSOCIE
!
!                LA METHODE RETENUE EST UNE METHODE DE QUASI-NEWTON BFGS
!                --- SOLVEUR D'OPTIMISATION : N2QN1 - INRIA ---
!                       AUTEURS : C. LEMARECHAL et J.-Ch. GILBERT
!
!                LES DERIVEES dZ/dCF12 SONT CALCULEES PAR DERIVATION AUTOMATIQUE
!                --- DIFFERENTIATEUR : TAPENADE - INRIA ---
!                       AUTEURS : L. HASCOET et V. PASCUAL
!
! *********************************************************************
!
! DEFINITION DES VARIABLES
! ------------------------
!.. Modules importes ..
!----------------------
   USE M_PRECISION
! Constantes nommees
! GPES
   USE M_PARAMETRE_C
! Messages d'erreur
   USE M_MESSAGE_C
! Numero de bief d'une section
   USE M_NUM_BIEF_S
! Calcul du nombre de Froude
   USE M_FROUDE_S
! Interface generique de traitement des erreurs
   USE M_TRAITER_ERREUR_I
   USE M_XINDIC_S
   USE M_STRICK_I
   USE M_DIRECT_I
   USE M_SIMUL_I
   USE M_SHARE_VAR

   IMPLICIT NONE
!.. Arguments ..
!---------------
! TABLEAU  DIMENSIONNE  A NbSect
   DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: z
! TABLEAU  DIMENSIONNE  A NbSect
   DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: q1, q2
! TABLEAUX DIMENSIONNES A NMSCAL
   DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: pcsing
   DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: qinjec
   LOGICAL, INTENT(IN) :: impression
!
!  Variables locales
!
   INTEGER :: retour ! Code de retour d'erreur des fonctions intrinseques
   INTEGER :: iter, ic
   INTEGER :: km, indice, j, k
   INTEGER :: indic, n
   INTEGER :: imp
   INTEGER :: io
   INTEGER :: mode
   INTEGER :: nsim
   INTEGER :: izs
   INTEGER, DIMENSION(:), POINTER :: iz
   REAL :: rzs
   DOUBLE PRECISION :: dzs(size(z))
   DOUBLE PRECISION ::zinit
   DOUBLE PRECISION :: f
   DOUBLE PRECISION :: df1, dk, ftemp
   DOUBLE PRECISION :: epsabs, eps
   DOUBLE PRECISION, DIMENSION(:), POINTER :: z_calc
   DOUBLE PRECISION, DIMENSION(:), POINTER :: cfzone
   DOUBLE PRECISION, DIMENSION(:), POINTER :: g
   DOUBLE PRECISION, DIMENSION(:), POINTER :: gnext
   DOUBLE PRECISION, DIMENSION(:), POINTER :: dxmin
   DOUBLE PRECISION, DIMENSION(:), POINTER  :: binf
   DOUBLE PRECISION, DIMENSION(:), POINTER  :: bsup
   DOUBLE PRECISION, DIMENSION(:), POINTER :: rz

! ALLOCATION DES VARAIBLES LIEES AU CALAGE
   ALLOCATE( z_mesu(nb_crue,max_mes) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'z_mesu' )
      return
   end if
   ALLOCATE( pond(nb_crue,max_mes) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'pond' )
      return
   end if
   ALLOCATE( i_mesu(nb_crue,max_mes) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'i_mesu' )
      return
   end if
   ALLOCATE( nb_mes(nb_crue) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'nb_mes' )
      return
   end if

! ALLOCATION DES VARAIBLES LIEES AU SOLVEUR N2QN1
   ALLOCATE( g(2*nb_zone_frottement) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'g' )
      return
   end if
   ALLOCATE( gnext(2*nb_zone_frottement) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'gnext' )
      return
   end if
   ALLOCATE( cfzone(2*nb_zone_frottement) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'cfzone' )
      return
   end if
   ALLOCATE( z_calc(max_mes) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'z_calc' )
      return
   end if
   ALLOCATE( dxmin(2*nb_zone_frottement) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'dxmin' )
      return
   end if
   ALLOCATE( bsup(2*nb_zone_frottement) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'bsup' )
      return
   end if
   ALLOCATE( binf(2*nb_zone_frottement) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'binf' )
      return
   end if
   ALLOCATE( iz(4*nb_zone_frottement+1) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'iz' )
      return
   end if
   ALLOCATE( rz(2*nb_zone_frottement*(2*nb_zone_frottement+9)/2) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'rz' )
      return
   end if
!
!
! INITIALISATIONS
!
   g      = 0.D0
   gnext  = 0.D0
   f      = 0.D0
   indic  = 4
   n      = 2*nb_zone_frottement
   iz     = 0
   rz     = 0.D0
   izs    = 0       ! Enregistre le numero de l'iteration
   if(impression.eqv..true.) then   ! Flag d'impression
      rzs = 1.
   else
      rzs = -1.
   endif
   dzs    = 0.D0
   imp    = 0
   io     = 6
   mode   = 2
   dk     = 1.D-2
   eps    = 1.D-6
   dxmin  = 1.D-2
   dzs(:) = pcsing(:)
   epsabs = constantes_calage%precis
   iter   = constantes_calage%niter
   nsim   = 10*iter

!
! CONSTANTES
! ----------
!
   iestim = constantes_calage%iestim
!
! CALCUL DU NOMBRE TOTAL DE MESURES
!
   DO ic=1,nb_crue
      nb_mes(ic) = calage_crues%nmes(ic)
      DO km=1,calage_crues%nmes(ic)
         z_mesu(ic, km) = calage_crues%zmesu(ic, km)
         pond(ic, km)   = calage_crues%pond(ic, km)
         CALL XINDIC_S(indice, calage_crues%xmesu(ic, km), x,erreur)
         if(Erreur%Numero.ne.0) return
         i_mesu(ic, km) = indice
      END DO
   END DO

!
! Ko INITIAL et bornes
!
   DO k=1, n/2
       cfzone(k)     = calage_frott(k)%valeur_coeff_min
       cfzone(k+n/2) = calage_frott(k)%valeur_coeff_maj
       binf(k)       = calage_frott(k)%valeur_coeff_min_binf
       bsup(k)       = calage_frott(k)%valeur_coeff_min_bsup
       binf(k+n/2)   = calage_frott(k)%valeur_coeff_maj_binf
       bsup(k+n/2)   = calage_frott(k)%valeur_coeff_maj_bsup
       ! Test sur la coherence des bornes Lit Mineur/Lit Majeur
       if(binf(k).lt.bsup(k+n/2)) then
          Erreur%Numero = 1
          Erreur%Message = "Overlap of the bounds for the coefficients of friction between the main channel and floodplain"
          return
       endif
   ENDDO

!
! COEFFICIENTS DE FROTTEMENT
!
   CALL STRICK()
   if(Erreur%Numero.ne.0) return
!
! 1er APPEL AU SIMULATEUR
!
   CALL SIMUL(indic, n, cfzone, f, g, izs, rzs, dzs)

!
! CALCUL DE LA DIAGONAL DE LA HESSIENNE PAR DIFFERENCES FINIES
!
   j = 1
   DO k = 1, n
      cfzone(k) = cfzone(k) + dk
      CALL SIMUL(indic, n, cfzone, ftemp, gnext, izs, rzs, dzs)
      IF (gnext(k) .LT. 0.D0+eps.AND. gnext(k) .GT. 0.D0-eps)  THEN
         rz(j) = 1.D0
      ELSE
         rz(j) = ABS(gnext(k)-g(k))/dk
      ENDIF
      j = j + n - (k-1)
      cfzone(k) = cfzone(k) - dk
   ENDDO

!
! APPEL AU SOLVEUR QUASI-NEWTON BFGS
!
   if(rzs.gt.0.) then
      print *
      write(*,'(a)') "  iters       f      "
      write(*,'(a)') "  ^^^^^  ^^^^^^^^^^^^"
   endif
   CALL N2QN1 (simul, n, cfzone, f, g, dxmin, df1, epsabs, imp, io, &
     &                  mode, iter, nsim, binf, bsup, iz, rz, izs, rzs, &
     &                  dzs)
  if(rzs.gt.0.) print *

   if(mode.ne.1) then
      Erreur%numero = mode
      select case(mode)
         case(0)
            Erreur%numero = 1
            Erreur%Message = 'N2QN1 : Problem with the simulator'
         case(2)
            Erreur%Message = 'N2QN1 : Problem with the initialisation'
         case(3)
            Erreur%Message = 'N2QN1 : The Newton matrix is not positive'
         case(4)
            Erreur%Message = 'N2QN1 : Maximal number of iterations reached'
         case(5)
            Erreur%Message = 'N2QN1 : Maximal number of simulations reached'
         case(6)
            Erreur%Message = 'N2QN1 : dxmin precision reached'
         case(7)
            Erreur%Message = 'N2QN1 : Problem with the Hessian decomposition'
      end select
      return
   endif
!
! RECOPIE DE LA SOLUTION
!
   DO k=1,n/2
       calage_frott(k)%valeur_coeff_min = cfzone(k)
       calage_frott(k)%valeur_coeff_maj = cfzone(k+n/2)
   ENDDO

   CALL STRICK()
   if(Erreur%Numero.ne.0) return
!
! CALCUL DE LA COTE FINALE (CRUE No. 1)
!
   CALL DIRECT(f, z_calc, z, q1, qinjec, pcsing, zinit, 1)

!
! DESALLOCATIONS
!
   DEALLOCATE(z_mesu)
   DEALLOCATE(pond)
   DEALLOCATE(i_mesu)
   DEALLOCATE(nb_mes)
   DEALLOCATE(z_calc)
   DEALLOCATE(g)
   DEALLOCATE(gnext)
   DEALLOCATE(cfzone)
   DEALLOCATE(dxmin)
   DEALLOCATE(bsup)
   DEALLOCATE(binf)
   DEALLOCATE(iz)
   DEALLOCATE(rz)

   RETURN

END SUBROUTINE CALAGE_N2QN1

