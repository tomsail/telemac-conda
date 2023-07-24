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

subroutine LEC_SORTIES( &
     VarSto              , &
     VarCalc             , &
     unitNum             , & ! Unite logique .xcasL
     Erreur                & ! Erreur
                      )

! *********************************************************************
! PROGICIEL : MASCARET       S. MANDELKERN
!                            F. ZAOUI
!
! VERSION : V8P4R0              EDF-CEREMA
! *********************************************************************

   !========================= Declarations ===========================
   use M_PRECISION
   use M_ERREUR_T            ! Type ERREUR_T
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_INDEX_VARIABLE_C    ! Numeros des variables a sortir
   use M_TRAITER_ERREUR_I    ! Traitement de l'errreur
   use M_XCAS_S

   implicit none

   ! Arguments
   logical           , dimension(:)   , intent(  out) :: VarCalc
   logical           , dimension(:)   , intent(  out) :: VarSto
   integer, intent(in)                                :: unitNum
   type(ERREUR_T)                     , intent(inout) :: Erreur
   ! Variables locales
   integer :: retour       ! code de retour des fonctions intrinseques
   logical :: reponse_logique
   integer :: nb_deversoir
   logical, allocatable :: ltab1(:),ltab2(:)
   character(len=256)  :: pathNode
   character(len=8192) :: line
   !character(132) :: !arbredappel_old

   !========================= Instructions ===========================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>LEC_SORTIES'
   VarSto(:) = .false.
   VarCalc(:) = .false.

    allocate( ltab1(42) , STAT = retour )
    if( retour /= 0 ) then
        Erreur%Numero = 5
        Erreur%ft     = err_5
        Erreur%ft_c   = err_5c
        call TRAITER_ERREUR( Erreur , 'ltab1' )
        return
    end if
    ltab1(:) = .false.
    allocate( ltab2(15) , STAT = retour )
    if( retour /= 0 ) then
        Erreur%Numero = 5
        Erreur%ft     = err_5
        Erreur%ft_c   = err_5c
        call TRAITER_ERREUR( Erreur , 'ltab1' )
        return
    end if
    ltab2(:) = .false.

   pathNode = 'parametresVariablesStockees/variablesStockees'
   line = xcasReader(unitNum, pathNode)
   if(len(trim(line)).ne.0) read(unit=line, fmt=*) ltab1

   pathNode = 'parametresVariablesStockees/variablesCalculees'
   line = xcasReader(unitNum, pathNode)
   if(len(trim(line)).ne.0) read(unit=line, fmt=*) ltab2

   ! Variables stockees
   VarSto(VAR_X)    = .true.

   VarSto(VAR_ZREF) = ltab1(1)
   VarSto(VAR_RGC)  = ltab1(2)
   VarSto(VAR_RDC)  = ltab1(3)
   VarSto(VAR_CF1)  = ltab1(4)
   VarSto(VAR_CF2)  = ltab1(5)
   VarSto(VAR_Z)    = ltab1(6)
   VarSto(VAR_Q1)   = ltab1(7)
   VarSto(VAR_Q2)   = ltab1(8)
   VarSto(VAR_S1)   = ltab1(9)
   VarSto(VAR_S2)   = ltab1(10)
   VarSto(VAR_FR)   = ltab1(11)
   VarSto(VAR_BETA) = ltab1(12)
   VarSto(VAR_B1)   = ltab1(13)
   VarSto(VAR_B2)   = ltab1(14)
   VarSto(VAR_BS)   = ltab1(15)
   VarSto(VAR_P1)   = ltab1(16)
   VarSto(VAR_P2)   = ltab1(17)
   VarSto(VAR_RH1)  = ltab1(18)
   VarSto(VAR_RH2)  = ltab1(19)
   VarSto(VAR_V1)   = ltab1(20)
   VarSto(VAR_V2)   = ltab1(21)
   VarSto(VAR_TAUF) = ltab1(22)
   VarSto(VAR_Y)    = ltab1(23)
   VarSto(VAR_HMOY) = ltab1(24)
   VarSto(VAR_Q2G)  = ltab1(25)
   VarSto(VAR_Q2D)  = ltab1(26)
   VarSto(VAR_SS)   = ltab1(27)
   VarSto(VAR_VOL)  = ltab1(28)
   VarSto(VAR_VOLS) = ltab1(29)
   VarSto(VAR_CHARG)= ltab1(30)
   VarSto(VAR_ZMAX) = ltab1(31)
   VarSto(VAR_TZMAX)= ltab1(32)
   VarSto(VAR_VZMAX)= ltab1(33)
   VarSto(VAR_ZMIN) = ltab1(34)
   VarSto(VAR_TZMIN)= ltab1(35)
   VarSto(VAR_V1MIN)= ltab1(36)
   VarSto(VAR_V1MAX)= ltab1(37)
   VarSto(VAR_BMAX) = ltab1(38)
   VarSto(VAR_TOND) = ltab1(39)
   VarSto(VAR_QMAX) = ltab1(40)
   VarSto(VAR_TQMAX)= ltab1(41)
   VarSto(VAR_EMAX) = ltab1(42)


   VarSto(VAR_QDEV) = .true.
   VarSto(VAR_Q)    = .true.
   VarSto(VAR_Debi) = .false.
   VarSto(VAR_YVRAI) = .false.
   VarSto(VAR_QVRAI) = .false.

   pathNode = 'parametresGeneraux/validationCode'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) reponse_logique

   if(reponse_logique) then
       VarSto(VAR_YVRAI) = .true.
       VarSto(VAR_QVRAI) = .true.
   endif

   pathNode = 'parametresGeneraux/calcOndeSubmersion'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) reponse_logique

   if(reponse_logique) then
      VarSto(VAR_ZMAX)  = .true.
      VarSto(VAR_TZMAX) = .true.
      VarSto(VAR_VZMAX) = .true.
      VarSto(VAR_TOND)  = .true.
      VarSto(VAR_QMAX)  = .true.
      VarSto(VAR_V1MAX) = .true.
   Endif
   If( VarSto(VAR_VZMAX) ) VarSto(Var_V1) = .true.

   pathNode = 'parametresApportDeversoirs/deversLate'
   line = xcasReader(unitNum, pathNode)

   if(len(trim(line)).eq.0) then
       nb_deversoir = 0
   else
       pathNode = 'parametresApportDeversoirs/deversLate/nbDeversoirs'
       line = xcasReader(unitNum, pathNode)
       read(unit=line, fmt=*) nb_deversoir
   endif
   If( Nb_deversoir == 0 ) then
      VarSto(VAR_QDEV) = .false.
   endif

   ! Variables calculees
   VarCalc(VAR_X)    = .true.
   VarCalc(VAR_ZREF) = .true.
   VarCalc(VAR_RGC)  = .true.
   VarCalc(VAR_RDC)  = .true.
   VarCalc(VAR_CF1)  = .true.
   VarCalc(VAR_CF2)  = .true.
   VarCalc(VAR_Z)    = .true.
   VarCalc(VAR_Q1)   = .true.
   VarCalc(VAR_Q2)   = .true.
   VarCalc(VAR_S1)   = .true.
   VarCalc(VAR_S2)   = .true.
   VarCalc(VAR_FR)   = .true.
   VarCalc(VAR_BETA) = .false.

   reponse_logique = ltab2(1)
   if( reponse_logique .or. VarSto(VAR_B1) ) VarCalc(VAR_B1) = .true.

   reponse_logique = ltab2(2)
   if( reponse_logique .or. VarSto(VAR_B2) ) VarCalc(VAR_B2) = .true.

   reponse_logique = ltab2(3)
   if( reponse_logique .or. VarSto(VAR_BS) ) VarCalc(VAR_BS) = .true.

   reponse_logique = ltab2(4)
   if( reponse_logique .or. VarSto(VAR_P1) ) VarCalc(VAR_P1) = .true.

   reponse_logique = ltab2(5)
   if( reponse_logique .or. VarSto(VAR_P2) ) VarCalc(VAR_P2) = .true.

   reponse_logique = ltab2(6)
   if( reponse_logique .or. VarSto(VAR_RH1) ) then
      VarCalc(VAR_RH1) = .true.
      VarCalc(VAR_P1)  = .true.
   endif

   reponse_logique = ltab2(7)
   if( reponse_logique .or. VarSto(VAR_RH2) ) then
      VarCalc(VAR_RH2) = .true.
      VarCalc(VAR_P2)  = .true.
   endif

   reponse_logique = ltab2(8)
   if( reponse_logique .or. VarSto(VAR_V1) ) VarCalc(VAR_V1) = .true.

   reponse_logique = ltab2(9)
   if( reponse_logique .or. VarSto(VAR_V2) ) VarCalc(VAR_V2) = .true.

   reponse_logique = ltab2(10)
   if( reponse_logique .or. VarSto(VAR_TAUF) ) then
      VarCalc(VAR_TAUF) = .true.
      VarCalc(VAR_B1)   = .true.
      VarCalc(VAR_B2)   = .true.
      VarCalc(VAR_P1)   = .true.
      VarCalc(VAR_P2)   = .true.
   endif

   reponse_logique = ltab2(11)
   if( reponse_logique .or. VarSto(VAR_Y) ) VarCalc(VAR_Y) = .true.

   reponse_logique = ltab2(12)
   if( reponse_logique .or. VarSto(VAR_HMOY) ) then
      VarCalc(VAR_HMOY) = .true.
      VarCalc(VAR_B1)   = .true.
      VarCalc(VAR_B2)   = .true.
   endif

   reponse_logique = ltab2(13)
   if( reponse_logique .or. VarSto(VAR_Q2G) ) VarCalc(VAR_Q2G) = .true.

   reponse_logique = ltab2(14)
   if( reponse_logique .or. VarSto(VAR_Q2D) ) then
      VarCalc(VAR_Q2D) = .true.
      VarCalc(VAR_Q2G) = .true.
   endif

   !----------------------------------------
   ! VOL et VOLS (donc SS) toujours calcules
   ! afin de controler le volume
   !----------------------------------------
   VarCalc(VAR_VOL)  = .true.
   VarCalc(VAR_SS)   = .true.
   VarCalc(VAR_VOLS) = .true.

   reponse_logique = ltab2(15)
   if( reponse_logique .or. VarSto(VAR_CHARG) ) VarCalc(VAR_CHARG) = .true.

   if( VarSto(VAR_ZMAX)  ) VarCalc(VAR_ZMAX)  = .true.
   if( VarSto(VAR_TZMAX) ) VarCalc(VAR_TZMAX) = .true.
   if( VarSto(VAR_VZMAX) ) VarCalc(VAR_VZMAX) = .true.
   if( VarSto(VAR_ZMIN)  ) VarCalc(VAR_ZMIN)  = .true.
   if( VarSto(VAR_TZMIN) ) VarCalc(VAR_TZMIN) = .true.
   if( VarSto(VAR_V1MIN) ) VarCalc(VAR_V1MIN) = .true.
   if( VarSto(VAR_V1MAX) ) VarCalc(VAR_V1MAX) = .true.
   if( VarSto(VAR_BMAX)  ) VarCalc(VAR_BMAX)  = .true.
   if( VarSto(VAR_TOND)  ) VarCalc(VAR_TOND)  = .true.
   if( VarSto(VAR_QMAX)  ) VarCalc(VAR_QMAX)  = .true.
   if( VarSto(VAR_TQMAX) ) VarCalc(VAR_TQMAX) = .true.
   if( VarSto(VAR_EMAX)  ) VarCalc(VAR_EMAX)  = .true.

   if( VarCalc(VAR_TZMAX).or.VarCalc(VAR_VZMAX) ) then
       VarCalc(VAR_ZMAX)  = .true.
   endif
   if (VarCalc(VAR_Q2G)) then
        VarCalc(VAR_V2) = .TRUE.
   endif
   if (VarCalc(VAR_TQMAX)) then
      VarCalc(VAR_QMAX) = .TRUE.
   endif
   if (VarCalc(VAR_QMAX)) then
      VarCalc(VAR_TQMAX) = .TRUE.
   endif

   ! Fin des traitements

   deallocate(ltab1)
   deallocate(ltab2)

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine LEC_SORTIES
