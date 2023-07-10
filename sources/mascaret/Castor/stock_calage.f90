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

subroutine STOCK_CALAGE( &
                         X , &
                       CF1 , &
                       CF2 , &
                         Z , &
                         Q , &
                    FCOUTM , &
           FichierResultat , &
          FichierResultat1 , &
                      iter , &
                     TEMPS , &
                    NbCrue , &
                      Crue , &
                    Erreur )

!***********************************************************************
!  PROGICIEL : MASCARET
!
!  VERSION : V8P4R0           EDF-CEREMA
!
!  FONCTION :  STOCKAGE SUR FICHIER AU FORMAT `LIDO NON PERMANENT'
!  --------    DES RESULTATS PROPRES A CASIER
!
!  SOUS PROGRAMMES APPELANT(S) : STOCK
!  ---------------------------
!  SOUS PROGRAMMES APPELE(S)   : Neant
!  -------------------------
!
!***********************************************************************

   !============================= Declarations ===========================
   use M_ERREUR_T
   use M_FICHIER_T
   use M_PRECISION
   use M_CONSTANTES_CALCUL_C

   implicit none

   !.. Arguments ..
   type(ERREUR_T),                      intent(inout) :: Erreur
   real(DOUBLE)       , dimension(:)  , intent(in)    :: X
   real(DOUBLE)       , dimension(:)  , intent(in)    :: Z,Q 
   real(DOUBLE)       , dimension(:)  , intent(in)    :: CF1
   real(DOUBLE)       , dimension(:)  , intent(in)    :: CF2 
   real(DOUBLE)                       , intent(in)    :: FCOUTM
   real(DOUBLE),                        intent(in   ) :: TEMPS
   integer,                             intent(in   ) :: Iter, NbCrue, Crue
   type(FICHIER_T),                     intent(in   ) :: FichierResultat,FichierResultat1
   REAL(DOUBLE)                                       :: Xiter

   !.. Constantes ..
   character(LEN=4), parameter :: NOM_FIN       = "FIN "
   character(LEN=4), parameter :: NOM_IDEB_BIEF = "I1  "
   character(LEN=4), parameter :: NOM_IFIN_BIEF = "I2  "

   !.. Variables locales ..
   character(LEN=72), dimension(3) :: TITRE
   !character(132) :: arbredappel_old
   integer :: ul, ul1, nb_sect, retour, isect

   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   !arbredappel_old = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>STO_CALAGE'
   TITRE(1) = 'Convergence du calage'
   TITRE(2) = ' '
   TITRE(3) = ' '
   ul  = FichierResultat%Unite
   ul1 = FichierResultat1%Unite
   nb_sect = size( X )

   if( Iter == 0 .and. Crue == 1 ) then
      open( unit = ul , file = FichierResultat%Nom , access = 'SEQUENTIAL' , &
            action = 'WRITE' , form = 'FORMATTED' , iostat = RETOUR , &
            position = 'append' , status = 'REPLACE' )
      open( unit = ul1 , file = FichierResultat1%Nom , access = 'SEQUENTIAL' , &
            action= 'WRITE' , form = 'FORMATTED' , iostat = RETOUR , &
            position = 'append' , status = 'REPLACE' )
   else  ! PhaseSimulation == PHASE_CALCUL
      open( unit = ul1 , file = FichierResultat1%Nom , access = 'SEQUENTIAL' , &
            action = 'WRITE' , form = 'FORMATTED' , iostat = RETOUR , &
            position = 'append' , status = 'OLD' )
      open( unit = ul , file = FichierResultat%Nom , access = 'SEQUENTIAL' , &
            action = 'WRITE' , form = 'FORMATTED' , iostat = RETOUR , &
            position = 'append' , status = 'OLD' )
   end if

   if( Iter == 0 .and. Crue == 1 ) then
      ! ECRITURE DES DONNEES INITIALES
      ! ------------------------------
      ! TITRE
      !   write (ul,'(A72)') TITRE(1) 
      write(ul,'(A)') "[variables]"
      ! pas de variable initiale

      ! DONNEES INDEPENDANTES DU PAS DE TEMPS
      ! -------------------------------------
      !   write (ul,'(A4)') 'X   '
      !   write (ul) 'ZMAX'
      !   write (ul) 'TMAX'
      !   write (ul,'(A4)') NOM_FIN
      !   write (ul,'(I8)') nb_sect
      !   write(ul) (REAL(isect, kind = SIMPLE), isect = 1, nb_sect)
      !   write(ul,'(F10.2)') (REAL(X(isect), kind = SIMPLE), isect = 1, nb_sect)

      ! NOMS DES VARIABLES DEPENDANTES DU TEMPS
      write( ul , "('""',A,'""',';','""',A,'""',';','""',A,'""',';',I1)" ) &
                  'COTE DE L EAU' , 'Z' , 'm' , 3
      write( ul , "('""',A,'""',';','""',A,'""',';','""',A,'""',';',I1)" ) &
                  'DEBIT TOTAL','QTOT','m3/s',3
      write( ul , "('""',A,'""',';','""',A,'""',';','""',A,'""',';',I1)" ) &
                  'COEFFICIENT DE FROTTEMENT MINEUR','KMIN','m1/3/s',0
      write( ul , "('""',A,'""',';','""',A,'""',';','""',A,'""',';',I1)" ) &
                  'COEFFICIENT DE FROTTEMENT MAJEUR','KMAJ','m1/3/s',0 
      write( ul , "('""',A,'""',';','""',A,'""',';','""',A,'""',';',I1)" ) &
                  'FONCTION COUT','FCOUT','m',5 
      write(ul,'(A)') "[resultats]"
   end if

   ! ECRITURE DES DONNEES DEPENDANTES DU TEMPS
   ! -----------------------------------------
   do isect = 1 , nb_sect
      Xiter = iter
      write( ul , "(F12.1,';','""',i2,'""',';','""',i4,'""',';')" , advance = 'NO' ) &
          XITER , Crue , isect
      write( ul , "(F10.2,';',F10.2,';',F10.2,';',F10.2,';',F10.2,';',F10.4)" ) &
          X(Isect) , Z(isect) , Q(Isect) , CF1(Isect) , CF2(isect) , FCOUTM
   enddo

   if(Crue.eq.NbCrue) write( ul1 , '(I3,F10.5)' ) Iter , FCOUTM

   close( ul )
   close( ul1 )

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine STOCK_CALAGE
