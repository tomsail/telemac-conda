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

subroutine POST_IMP_CASIER( &
                            Casier                , &
                            FichierListingCasier  , &
                            Liaison               , &
                            FichierListingLiaison , &
                            TEMPS                 , &
                            PhaseSimulation       , &
                            Erreur                  )

!***********************************************************************
! PROGICIEL : MASCARET
!
!
! VERSION : V8P4R0                  EDF-CEREMA
!
!  FONCTION :   IMPRESSION DES VALEURS DES VARIABLES CASIER ET LIAISON
!  --------     SUR LISTING
!
!  SOUS PROGRAMMES APPELANT(S) : SUPERVISEUR
!  ---------------------------
!  SOUS PROGRAMMES APPELE(S) :   Neant
!  -------------------------
!***********************************************************************

!============================= Declarations ===========================
   use M_PRECISION
   use M_CASIER_T
   use M_LIAISON_T
   use M_ERREUR_T
   use M_FICHIER_T
   use M_CONSTANTES_CALCUL_C
   use M_MESSAGE_CASIER_C
   use M_TRAITER_ERREUR_CASIER_I

   implicit none

!.. Arguments ..
   type(CASIER_T)  , dimension(:) , pointer       :: Casier
   type(LIAISON_T) , dimension(:) , pointer       :: Liaison
   type(ERREUR_T)  ,                intent(inout) :: Erreur
   type(FICHIER_T) ,                intent(in   ) :: FichierListingCasier
   type(FICHIER_T) ,                intent(in   ) :: FichierListingLiaison
   integer         ,                intent(in   ) :: PhaseSimulation
   real(DOUBLE)    ,                intent(in   ) :: TEMPS

!.. Variables locales
   !character(132) :: arbredappel_old
   integer :: ull, ulc
   integer :: icasier, nb_casier, iliaison, nb_liaison

!============================ Instructions ==============================

   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   !arbredappel_old = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>POST_IMP_CASIER'
   ulc = FichierListingCasier%Unite
   ull = FichierListingLiaison%Unite

   ! ECRITURE DES DONNEES INITIALES CASIER
   ! =====================================
   if( PhaseSimulation == PHASE_INITIALISATION ) then
      write(ulc,'(A)') "/* caracteristiques des casiers : resultats */"
      write(ulc,*) ! saut de ligne
      write(ulc,'(A)') "[variables]"
      write(ulc,*) ! saut de ligne
      write(ulc,'(A)') '"cote surface libre casier";"ZCAS";"m";2'
      write(ulc,'(A)') '"surface casier";"SURCAS";"m2";2'
      write(ulc,'(A)') '"volume casier";"VOLCAS";"m3";2'
      write(ulc,*) ! saut de ligne
      write(ulc,'(A)') "[resultats]"
      write(ulc,*) ! saut deligne
      write(ulc,'(A,";",A,";",A,";",A)') "numero casier", "cote", "surafce", "volume"
      write(ulc,*) ! saut de ligne
   end if

   ! ECRITURE DES DONNEES CASIER AU COURS DU TEMPS (et au pas de temps 0)
   ! =============================================
   nb_casier = size( Casier )
   write (ulc,"('******** TEMPS ******** = ',f12.1,' s')") TEMPS
   write(ulc,*)
   do icasier = 1 , nb_casier
      write(ulc,"('""CAS',i3,'""',';',f9.5,';',f12.2,';',f12.2)") &
         icasier, Casier(icasier)%Cote, Casier(icasier)%Surface , Casier(icasier)%Volume
      write(ulc,"(A,i3,f12.3)") "BILAN VOLUME CASIER", icasier , Casier(icasier)%Bilan
      write(ulc,"(A,i3,g15.9)") "ERREUR VOLUME CASIER", icasier, Casier(icasier)%BilanErreur
   end do
   write(ulc,*) ! saut de ligne

   ! ECRITURE DES DONNEES INITIALES LIAISON
   ! ======================================
   if( PhaseSimulation == PHASE_INITIALISATION ) then
      write(ull,'(A)') "/* caracteristiques des liaisons : resultats */"
      write(ull,*) ! saut de ligne
      write(ull,'(A)') "[variables]"
      write(ull,*) ! saut de ligne
      write(ull,'(A)') '"debit echange";"QECH";"m3/s";3'
      write(ull,'(A)') '"vitesse echange";"VECH";"m/s";3'
      write(ull,*) ! saut de ligne
      write(ull,'(A)') "[resultats]"
      write(ull,*) ! saut de ligne
      write(ull,'(A,";",A,";",A)') "code liaison","debit", "vitesse"
      write(ull,*) ! saut de ligne
   end if

   ! ECRITURE DES DONNEES LIAISON AU COURS DU TEMPS (et au pas de temps 0)
   ! ==============================================
   nb_liaison = size( Liaison )
   write(ull,"('******** TEMPS ******** = ',f12.1,' s')") TEMPS
   write(ull,*)
   do iliaison = 1 , nb_liaison
      if( Liaison(iliaison)%NatureLiaison == LIAISON_TYPE_RIVIERE_CASIER ) then
         write(ull,"('""NUM',i3,'/RIV-CAS""',';',f10.3,';',f15.3)") &
               iliaison , Liaison(iliaison)%DebitEchange , &
               Liaison(iliaison)%VitesseEchange
      else
         write(ull,"('""NUM',i3,'/CAS-CAS""',';',f10.3,';',f15.3)") &
               iliaison, Liaison(iliaison)%DebitEchange , &
               Liaison(iliaison)%VitesseEchange
      end if
   end do
   write(ull,*) ! saut de ligne
   !fin des traitements d erreur
   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine POST_IMP_CASIER
