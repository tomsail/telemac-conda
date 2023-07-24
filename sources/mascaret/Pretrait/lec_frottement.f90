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

subroutine LEC_FROTTEMENT ( &
                          CF1 , & ! Coefficient de frottement Mineur
                          CF2 , & ! Coefficient de frottement Majeur
                            X , & ! Abscisse des sections de calcul
                      ZoneFrot, & ! Zone de frottement
                          XDT , &
                       Profil , & ! Profils geometriques
                  ProfDebBief , & ! Premiers profils des biefs
                  ProfFinBief , & ! Derniers profils des biefs
            AbscRelExtDebBief , & ! Abscisse de l'extremite debut du bief
            AbscRelExtFinBief , & ! Abscisse de l'extremite debut du bief
          InterpLinCoeffFrott , &
                 UniteListing , & ! Unite logique fichier listing
                      unitNum , & ! Pointeur vers document XML
                       Erreur & ! Erreur
                             )

! *********************************************************************
! PROGICIEL : MASCARET        S. MANDELKERN
!                             F. ZAOUI
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   !========================= Declarations ===========================
   use M_PRECISION
   use M_ERREUR_T            ! Type ERREUR_T
   use M_PARAMETRE_C
   use M_PROFIL_T            ! Type  PROFIL_T
   use M_ZONE_FROT_T         ! Type Zone frottement
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_TRAITER_ERREUR_I    ! Traitement de l'errreur
   use M_XINDIC_S            ! Calc de l'indice corresp a une absc
   use M_ABS_ABS_S           ! Calcul de l'abscisse absolue
   use M_XCAS_S

   implicit none

   ! Arguments
   real(DOUBLE)      , dimension(:)  , pointer       :: CF1
   real(DOUBLE)      , dimension(:)  , pointer       :: CF2
   real(DOUBLE)      , dimension(:)  , intent(in   ) :: X
   real(DOUBLE)      , dimension(:)  , intent(in   ) :: XDT
   type(PROFIL_T)    , dimension(:)  , intent(in   ) :: Profil
   type(ZONE_FROT_T) , dimension(:)  , pointer       :: ZoneFrot
   integer           , dimension(:)  , intent(in   ) :: ProfDebBief
   integer           , dimension(:)  , intent(in   ) :: ProfFinBief
   real(DOUBLE)      , dimension(:)  , intent(in   ) :: AbscRelExtDebBief
   real(DOUBLE)      , dimension(:)  , intent(in   ) :: AbscRelExtFinBief
   logical                           , intent(in   ) :: InterpLinCoeffFrott
   integer                           , intent(in   ) :: UniteListing
   integer, intent(in)                               :: unitNum
   type(ERREUR_T)                    , intent(inout) :: Erreur
   ! Variables locales
   integer      :: izone              ! compteur sur les zones de frottement
   integer      :: isect              ! compteur sur les sections
   integer      :: indice             ! indice de debut de zone
   integer      :: indice2            ! indice de fin de zone
   integer      :: indice2_prec       ! indice de fin de zone precedente
   integer      :: nb_zone_frottement
   real(DOUBLE) :: valeur_coeff_min
   real(DOUBLE) :: valeur_coeff_maj
   real(DOUBLE) :: valeur_coeff_min_prec
   real(DOUBLE) :: valeur_coeff_maj_prec
   integer      :: branche_zone_frott
   real(DOUBLE) :: abscdeb_zone_frott
   real(DOUBLE) :: abscfin_zone_frott
   real(DOUBLE) :: abscfin_zone_frott_prec
   integer      :: branche_zone_frott_prec
   logical      :: chevauchement
   integer      :: retour              ! code de retour des fonctions
                                       ! intrinseques
   integer, allocatable :: itab(:)
   real(double), allocatable :: rtab1(:),rtab2(:),rtab3(:),rtab4(:)
   character(len=256)  :: pathNode
   character(len=8192) :: line
   !character(132) :: !arbredappel_old

   !========================= Instructions ===========================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   retour = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>LEC_FROTTEMENT'
   chevauchement = .TRUE.

   if (UniteListing >0) write(UniteListing,10020)

   if(.not.associated(CF1)) allocate( CF1(size(X)) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'CF1' )
      return
   end if

   if(.not.associated(CF2)) allocate( CF2(size(X)) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'CF2' )
      return
   end if

   ! Nombre de zones de frottement
   !-----------------------------
   pathNode = 'parametresCalage/frottement/nbZone'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) nb_zone_frottement

   if( nb_zone_frottement <= 0 ) then
      Erreur%Numero = 306
      Erreur%ft     = err_306
      Erreur%ft_c   = err_306c
      call TRAITER_ERREUR( Erreur , 'Nombre de zone de frottement' )
      return
   end if

   allocate( itab(nb_zone_frottement) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'itab' )
       return
   end if
   allocate( rtab1(nb_zone_frottement) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'rtab1' )
       return
   end if
   allocate( rtab2(nb_zone_frottement) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'rtab2' )
       return
   end if
   allocate( rtab3(nb_zone_frottement) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'rtab3' )
       return
   end if
   allocate( rtab4(nb_zone_frottement) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'rtab4' )
       return
   end if
   if( nb_zone_frottement > 0 ) then

      allocate( ZoneFrot(nb_zone_Frottement) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'ZoneFrot' )
         return
      end if

   endif
   if (UniteListing >0) write(UniteListing,10000) nb_zone_frottement

   branche_zone_frott_prec = 0
   abscfin_zone_frott_prec = 0._DOUBLE

   pathNode = 'parametresCalage/numBranche'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) itab

   pathNode = 'parametresCalage/coefLitMin'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) rtab1

   pathNode = 'parametresCalage/coefLitMaj'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) rtab2

   pathNode = 'parametresCalage/absDebZone'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) rtab3

   pathNode = 'parametresCalage/absFinZone'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) rtab4

   do izone = 1 , nb_zone_frottement
      valeur_coeff_min   =  rtab1(izone)
      if( valeur_coeff_min <= 0 ) then
         Erreur%Numero = 311
         Erreur%ft     = err_311
         Erreur%ft_c   = err_311c
         call TRAITER_ERREUR( Erreur , 'of the main channel coefficient' , izone )
         return
      end if

      valeur_coeff_maj = rtab2(izone)
      if( valeur_coeff_maj <= 0 ) then
         Erreur%Numero = 311
         Erreur%ft     = err_311
         Erreur%ft_c   = err_311c
         call TRAITER_ERREUR( Erreur , 'of the floodplain coefficient' , izone )
         return
      end if

      branche_zone_frott = itab(izone)
      if( branche_zone_frott < 0 ) then
         Erreur%Numero = 367
         Erreur%ft   = err_367
         Erreur%ft_c = err_367
         call TRAITER_ERREUR( Erreur , 'Reach friction' , branche_zone_frott , izone )
         return
      end if

      abscdeb_zone_frott = rtab3(izone)
      if( abscdeb_zone_frott < AbscRelExtDebBief(branche_zone_frott) .or. &
          abscdeb_zone_frott > AbscRelExtFinBief(branche_zone_frott) ) then
         Erreur%Numero = 334
         Erreur%ft     = err_334
         Erreur%ft_c   = err_334c
         call TRAITER_ERREUR( Erreur ,'begin' , izone , branche_zone_frott )
         return
      end if

      abscfin_zone_frott = rtab4(izone)
      if( abscfin_zone_frott < AbscRelExtDebBief(branche_zone_frott) .or. &
          abscfin_zone_frott > AbscRelExtFinBief(branche_zone_frott) ) then
         Erreur%Numero = 334
         Erreur%ft     = err_334
         Erreur%ft_c   = err_334c
         call TRAITER_ERREUR( Erreur , 'end' , izone , branche_zone_frott )
         return
      end if

      if( izone /= 1 ) then
         if( abs(abscdeb_zone_frott-abscfin_zone_frott_prec).GT.EPS6 ) then
            chevauchement = .FALSE.
         end if
      end if

      if (UniteListing >0) write(UniteListing,10010) &
                        izone,                       &
                        branche_zone_frott,          &
                        abscdeb_zone_frott,          &
                        abscfin_zone_frott,          &
                        valeur_coeff_min,            &
                        valeur_coeff_maj

      branche_zone_frott_prec = branche_zone_frott
      abscfin_zone_frott_prec = abscfin_zone_frott

      ! Passage en coordonnees absolues
      !--------------------------------
      abscdeb_zone_frott = ABS_ABS_S ( &
              branche_zone_frott     , &
              abscdeb_zone_frott     , &
              Profil                 , &
              ProfDebBief            , &
              ProfFinBief            , &
              Erreur                   &
                             )
      if( Erreur%Numero /= 0 ) then
         return
      end if

      abscfin_zone_frott = ABS_ABS_S ( &
              branche_zone_frott     , &
              abscfin_zone_frott     , &
              Profil                 , &
              ProfDebBief            , &
              ProfFinBief            , &
              Erreur                   &
                             )
      if( Erreur%Numero /= 0 ) then
         return
      end if

      !------------------------------
      ! calcul des indices de section
      ! correspondant aux abscisses
      !------------------------------
      call XINDIC_S( indice , abscdeb_zone_frott , X , Erreur )
      if( Erreur%Numero /= 0 ) then
         return
      endif
      ZoneFrot(izone)%Sectiondeb = indice

      call XINDIC_S( indice2 , abscfin_zone_frott , X , Erreur )
      if( Erreur%Numero /= 0 ) then
         return
      endif
      ZoneFrot(izone)%Sectionfin= indice2

      do isect = indice , indice2
         CF1(isect) = valeur_coeff_min
         CF2(isect) = valeur_coeff_maj
      end do

      if( chevauchement .eqv. .FALSE. ) then
         if( InterpLinCoeffFrott ) then
            do isect = indice2_prec , indice
               CF1(isect) = valeur_coeff_min_prec + ( valeur_coeff_min - valeur_coeff_min_prec ) * XDT(isect)
               CF2(isect) = valeur_coeff_maj_prec + ( valeur_coeff_maj - valeur_coeff_maj_prec ) * XDT(isect)
            end do
         else
            do isect = indice2_prec , indice-1
               CF1(isect) = valeur_coeff_min_prec
               CF2(isect) = valeur_coeff_maj_prec
            end do
         end if
      end if

      valeur_coeff_min_prec = valeur_coeff_min
      valeur_coeff_maj_prec = valeur_coeff_maj
      indice2_prec = indice2
   end do

   !Erreur%arbredappel = !arbredappel_old

   deallocate(itab)
   deallocate(rtab1)
   deallocate(rtab2)
   deallocate(rtab3)
   deallocate(rtab4)

   return

   10020 format (/,'FROTTEMENT',/, &
                &  '----------',/)
   10000 format ('Nombre de zones de Frottement : ',i3)
   10010 format (/,'Zones ',i3,' Branche n0 ',i3,                     &
                ' Abscisse debut : ',f12.3,' Abscisse fin : ',f12.3,/, &
                'Coefficient mineur : ',f12.3,' Coefficient majeur : ',f12.3)

end subroutine LEC_FROTTEMENT
