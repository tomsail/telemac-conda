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

subroutine  SING2       ( &
            AS,BS,CS    , & ! Coeff de l'equation discretisee de la singularite
            ZAM,ZAV     , & ! Cotes amont et aval
            CoteCrete   , & ! cote de crete de la singularite
            NbQRef      , & ! Nombre de points definissant la loi en regime denoye
            ZAMONT      , & ! points Z de la loi
            QAMONT      , & ! Points Q de la loi
            Erreur        & ! Erreur
                        )

! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P. CHERUBINI
!                             S. PERON
!                             S. MANDELKERN
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
!   FONCTION :
!   --------
!
!   CALCUL DES COEFFICIENTS DE L'EQUATION DISCRETISEE D'UNE SINGULARITE
!   EN UTILISANT LA LOI EN REGIME DENOYE (SINGULARITE DE TYPE 2) .
!
!   Q = AS * DHAMONT + BS * DHAVAL + CS
!
! ----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ----------------------
!
!   SOUS PROGRAMME APPELANT :  - KSING
!   -------------------------
!   SOUS PROGRAMMES APPELES :  - INTERPOLATION_S : INTERPOLATION DE LAGRANGE D'ORDRE N
!   -------------------------
!
!   COMMENTAIRES :
!   ------------
!
! . LA LOI EN REGIME DENOYE EST DONNEE PAR LES TABLEAUX ZAMONT,QAMONT
!
! . Q EST > 0 DE L'INDICE 1 VERS L'INDICE 2
!   EN REGIME NOYE , LA CORRECTION EST DONNEE PAR LE COEFFICIENT C :
!
! . RH=(HAVAL-CoteCrete)/(HAMONT-CoteCrete)
!      ---          RH < 0.8   C= +1
!      ---   0.8  < RH < 1.0   C= C1*RH**3 + C2*RH**2 + C3*RH + C4
!
! . AS,BS ET CS SONT LES COEFFICIENTS DE L'EQUATION DISCRETISEE :
!   Q = AS*DHAMONT + BS*DHAVAL + CS
!   DHAMONT REPRESENTE DZ1 SI L'ECOULEMENT A LIEU DANS LE SENS 1->2
!   DHAMONT REPRESENTE DZ2 SI L'ECOULEMENT A LIEU DANS LE SENS 2->1
!   DHAVAL  REPRESENTE DZ2 SI L'ECOULEMENT A LIEU DANS LE SENS 1->2
!   DHAVAL  REPRESENTE DZ1 SI L'ECOULEMENT A LIEU DANS LE SENS 2->1
!
!------------------------------------------------------------------------

   !============================ Declarations ==============================
   use M_PRECISION       ! Type DOUBLE
   use M_PARAMETRE_C     ! Parametres de calcul
   use M_MESSAGE_C       ! Liste des messages d'erreur
   use M_ERREUR_T        ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs
   use M_INTERPOLATION_S  ! Interpolation

   implicit none

   !.. Formal Arguments ..
   real(DOUBLE)              , intent(  out) :: AS, BS, CS
   type(ERREUR_T)            , intent(inout) :: Erreur
   real(DOUBLE)              , intent(in   ) :: ZAM, ZAV
   real(DOUBLE)              , intent(in   ) :: CoteCrete
   integer                   , intent(in   ) :: NbQRef
   real(DOUBLE), dimension(:), intent(in   ) :: ZAMONT, QAMONT
   !.. Constantes
   real(DOUBLE) :: C1 =   0._DOUBLE ! Coefficients de la fonction
   real(DOUBLE) :: C2 = -25._DOUBLE ! permettant le passage
   real(DOUBLE) :: C3 =  40._DOUBLE ! du regime denoye
   real(DOUBLE) :: C4 = -15._DOUBLE ! au regime noye
   ! Variables locales
   real(DOUBLE)   :: C,DCDRH,DQDZ,H1,H2,HAMONT,Q,Q1,RH
   !character(132) :: arbredappel_old

   !.. Intrinsic Functions ..
   intrinsic DMAX1

   !============================= Instructions =============================
   ! INITIALISATIONS
   ! ---------------
   Erreur%Numero      = 0
   !arbredappel_old    = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>SING2'
   AS = 0._DOUBLE
   BS = 0._DOUBLE
   CS = 0._DOUBLE

   HAMONT = DMAX1(ZAM,ZAV)
   if( HAMONT - CoteCrete <= 0._DOUBLE ) then
      !Erreur%arbredappel = arbredappel_old
      return
   endif

   H1 = ZAM - CoteCrete
   H2 = ZAV - CoteCrete

   if( H1 <= 0._DOUBLE ) then
      H1 = 0._DOUBLE
   end if

   if( H2 <= 0._DOUBLE ) then
      H2 = 0._DOUBLE
   end if

   ! CALCUL DU DEBIT
   ! ---------------
   call INTERPOLATION_S ( &
        Q               , &
        HAMONT          , &
        1               , &
        ZAMONT          , &
        QAMONT          , &
        NbQRef          , &
        Erreur            &
                        )
   if( Erreur%Numero /= 0 ) then
      return
   end if

   call INTERPOLATION_S ( &
        Q1              , &
        HAMONT+EPS2     , &
        1               , &
        ZAMONT          , &
        QAMONT          , &
        NbQRef          , &
        Erreur            &
                        )
   if( Erreur%Numero /= 0 ) then
      return
   end if

   DQDZ = ( Q1 - Q ) / EPS2

   ! CALCUL DES COEFFICIENTS
   ! -----------------------
   if( ZAM >= ZAV ) then
      RH = H2 / H1
      if( RH < 0.8_DOUBLE ) then
         ! ECOULEMENT DENOYE DE (1) VERS (2)
         C  = 1._DOUBLE
         AS = C * DQDZ
         BS = 0._DOUBLE
         CS = C * Q
      else
         ! ECOULEMENT NOYE DE (1) VERS (2)
         C     = C1 * ( RH**3 ) + C2 * ( RH**2 ) + C3 * RH + C4
         DCDRH = 3._DOUBLE * C1 * ( RH**2 ) + 2._DOUBLE * C2 * RH + C3
         AS    = C * DQDZ + Q * DCDRH * ( -H2 / ( H1**2 ) )
         BS    = Q * DCDRH * (1._DOUBLE / H1 )
         CS    = C * Q
      end if

   elseif( ZAV >= ZAM ) then

      RH = H1 / H2

      if( RH < 0.8_DOUBLE ) then

         ! ECOULEMENT DENOYE DE (2) VERS (1)
         C  = -1._DOUBLE
         AS = C * DQDZ
         BS = 0._DOUBLE
         CS = C * Q

      else

         ! ECOULEMENT NOYE DE (2) VERS (1)
         C     = -1._DOUBLE * (C1 * ( RH**3 ) + C2 * ( RH**2 ) + C3 * RH + C4 )
         DCDRH = -1._DOUBLE * ( 3._DOUBLE * C1 * ( RH**2 ) + 2._DOUBLE * C2 * RH + C3 )
         AS    = C * DQDZ + Q * DCDRH * ( -H1 / ( H2**2 ) )
         BS    = Q * DCDRH * (1._DOUBLE / H2 )
         CS    = C * Q
      end if

   end if

   !Erreur%arbredappel = arbredappel_old

   return

end subroutine SING2
