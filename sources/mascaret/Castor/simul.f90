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

SUBROUTINE SIMUL( &
             indic , &   ! Flag de pilotage
                 n , &   ! Nombre de variables a optimiser
            cfzone , &   ! Variables d'optimisation
                 f , &   ! Valeur de la fonction cout
                 g , &   ! Gradient de la fonction cout
               izs , &   ! Tableaux de travail d'entiers, de reels simples et doubles
               rzs , &
               dzs   &
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
! CALCUL DE LA FONCTION COUT ET DU GRADIENT
!
! *********************************************************************
!
! DEFINITION DES VARIABLES
! ------------------------
!.. Modules importes ..
!----------------------
   USE M_PRECISION
   USE M_PARAMETRE_C
   USE M_MESSAGE_C
   USE M_RHSBP_S
   USE M_NUM_BIEF_S
   USE M_FROUDE_S
   USE M_TRAITER_ERREUR_I
   USE M_CQINJ_I
   USE M_DIFF_JCOUT_I
   USE M_COUT_I  
   USE M_XINDIC_S
   USE M_SHARE_VAR
   USE M_STOCK_CALAGE_I
   USE M_STRICK_I
   USE M_DIRECT_I
   IMPLICIT NONE

!
!.. Arguments ..
!---------------
!
   INTEGER,INTENT(INOUT) :: indic
   INTEGER,INTENT(IN) :: n                                     
   DOUBLE PRECISION, INTENT(INOUT), DIMENSION(n) :: cfzone
   DOUBLE PRECISION, INTENT(INOUT) :: f                                   
   DOUBLE PRECISION, INTENT(INOUT), DIMENSION(n) :: g       
   INTEGER, INTENT(INOUT) :: izs
   REAL, INTENT(INOUT)  :: rzs
   DOUBLE PRECISION,DIMENSION(size(x)),INTENT(INOUT)  :: dzs
!
! VARIABLES LOCALES
!
   DOUBLE PRECISION, DIMENSION(:), POINTER :: z, q1
   DOUBLE PRECISION, DIMENSION(:), POINTER :: z_calc
   DOUBLE PRECISION, DIMENSION(:), POINTER :: grmaj
   DOUBLE PRECISION, DIMENSION(:), POINTER :: grmin
   DOUBLE PRECISION, DIMENSION(:), POINTER :: gtemp
   DOUBLE PRECISION, DIMENSION(:), POINTER :: qinjec
   DOUBLE PRECISION, DIMENSION(:), POINTER :: pcsing
   DOUBLE PRECISION :: fc, zinit, dk
   INTEGER :: j, k, ic
   INTEGER :: retour

!
! ALLOCATION DES VARIABLES LIEES AU SIMULATEUR
!
   ALLOCATE( z(size(x)) , STAT = retour )
   if( retour /= 0 ) then
      indic = 0
      return
   end if
   ALLOCATE(q1(size(x)) , STAT = retour )
   if( retour /= 0 ) then
      indic = 0
      return
   end if
   ALLOCATE(z_calc(max_mes) , STAT = retour )
   if( retour /= 0 ) then
      indic = 0
      return
   end if
   ALLOCATE(qinjec(size(x)) , STAT = retour )
   if( retour /= 0 ) then
      indic = 0
      return
   end if
   ALLOCATE(pcsing(size(x)) , STAT = retour )
   if( retour /= 0 ) then
      indic = 0
      return
   end if
   ALLOCATE(grmaj(n/2) , STAT = retour )
   if( retour /= 0 ) then
      indic = 0
      return
   end if
   ALLOCATE(grmin(n/2) , STAT = retour )
   if( retour /= 0 ) then
      indic = 0
      return
   end if
   ALLOCATE(gtemp(n) , STAT = retour )
   if( retour /= 0 ) then
      indic = 0
      return
   end if
!
! ENREGISTREMENT DES RESULTATS
!
   if( indic.EQ.1 ) then
      !
      ! Impression d'ecran des iterations
      !
      if(rzs.gt.0.) then
         write (*,1019) izs, f
      endif
 1019 format (i6,2x,1pe12.5)
      !
      ! Recopie de la solution courante
      !
      DO k=1,n/2  
         calage_frott(k)%valeur_coeff_min = cfzone(k) 
         calage_frott(k)%valeur_coeff_maj = cfzone(k+n/2)
      ENDDO

      CALL STRICK()
      !
      ! Calcul de la cote courante (pour chaque crue)
      !
      pcsing(:) = dzs(:)

      DO ic = 1, nb_crue
         extremite(1)%ptq(1) = calage_crues%debit(ic)
         extremite(2)%ptz(1) = calage_crues%zaval(ic)

         DO j = 1, SIZE(x)
            qinjec(j) = calage_crues%apport(ic, j)
         ENDDO

         CALL DIRECT(fc, z_calc, z, q1, qinjec, pcsing, zinit, ic)
         !
         ! Impression sur fichiers
         !
         CALL STOCK_CALAGE( X , CF1 , CF2 , Z , Q1 , f , FichierResultatCalage , &
                         FichierResultatCalage1 , izs , TEMPS , nb_crue, ic, Erreur )
      ENDDO
      !
      ! Modifie le numero d'enregistrement de l'iteration
      ! 
      izs = izs + 1
      return
   else
!
! CALCUL DES FONCTIONS
!
      dk     = 0.01D0
      z      = 0.D0
      q1     = 0.D0
      zinit  = 0.D0
      qinjec = 0.D0
      z_calc = 0.D0
      grmin  = 0.D0
      grmaj  = 0.D0
      f      = 0.D0
      fc     = 0.D0
      g      = 0.D0
      j      = 0
      DO k = 1,n/2
         calage_frott(k)%valeur_coeff_min = cfzone(k)
         calage_frott(k)%valeur_coeff_maj = cfzone(k+n/2)
      ENDDO
      pcsing(:) = dzs(:)

      DO ic = 1, nb_crue
         extremite(1)%ptq(1) = calage_crues%debit(ic)
         extremite(2)%ptz(1) = calage_crues%zaval(ic)
         DO j = 1, SIZE(x)
            qinjec(j) = calage_crues%apport(ic, j)
         ENDDO
         !
         ! CALCUL DU GRADIENT
         !
         CALL DIFF_JCOUT(grmin, grmaj, z, q1, qinjec, pcsing, &
&          zinit, dk, z_calc, ic)
         DO k=1,n/2
            gtemp(k)     = grmin(k)
            gtemp(k+n/2) = grmaj(k)
         END DO
         !
         ! CONTRUCTION DE LA FONCTION COUT ET DU GRADIENT TOTAL S'IL Y A PLUSIEURS CRUES
         !
         CALL COUT( fc , nb_mes(ic) , z_mesu(ic, :) , z_calc , pond(ic, :) )
         f = f + fc
         DO k = 1, n
            g(k) = g(k) + gtemp(k)
         ENDDO
      ENDDO
   endif

!
! DESALLOCATIONS
!
    DEALLOCATE(z)
    DEALLOCATE(q1)
    DEALLOCATE(z_calc)
    DEALLOCATE(qinjec)
    DEALLOCATE(pcsing)
    DEALLOCATE(grmin)
    DEALLOCATE(grmaj)
    DEALLOCATE(gtemp)

    RETURN

END SUBROUTINE SIMUL

