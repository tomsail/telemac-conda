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

module M_TRAITER_ERREUR_CASIER_I
!***********************************************************************
! PROGICIEL : MASCARET       A. LEBOSSE     C. RISSOAN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
interface TRAITER_ERREUR_CASIER

  module procedure TRAITER_ERREUR_CASIER

  module procedure TRAITER_ERREUR_CASIER_1C

  module procedure TRAITER_ERREUR_CASIER_2C

  module procedure TRAITER_ERREUR_CASIER_1I

  module procedure TRAITER_ERREUR_CASIER_1I1C

  module procedure TRAITER_ERREUR_CASIER_1I2C

  module procedure TRAITER_ERREUR_CASIER_2I

  module procedure TRAITER_ERREUR_CASIER_3I

  module procedure TRAITER_ERREUR_CASIER_1I1R

  module procedure TRAITER_ERREUR_CASIER_1I3R

  module procedure TRAITER_ERREUR_CASIER_1C1R

  module procedure TRAITER_ERREUR_CASIER_3I1C1R

  module procedure TRAITER_ERREUR_CASIER_1I1C1I

  module procedure TRAITER_ERREUR_CASIER_1R1C

end interface TRAITER_ERREUR_CASIER


!===========================================================
!                         SOUS-PROGRAMMES
!===========================================================

contains

subroutine TRAITER_ERREUR_CASIER (Erreur)

! Declarations ----------------------------------------

  use M_MESSAGE_CASIER_C   ! Liste des messages d'erreur
  use M_ERREUR_T    ! Definition du type ERREUR_T
  use M_FICHIER_T   ! UL_LST_CAS

  implicit none

  !... Arguments ...

  type(ERREUR_T), intent(inout) :: Erreur  ! Erreur

! Instructions ----------------------------------------
  ! Message long
  if (UL_LST_CAS > 0) then
      call preliminaire(Erreur)

      write(UL_LST_CAS,Erreur%ft)
      write(UL_LST_CAS,banniere)
  endif
  ! Message court
  write(Erreur%message,Erreur%ft_c)

end subroutine TRAITER_ERREUR_CASIER

! =======================================================

subroutine TRAITER_ERREUR_CASIER_1C (Erreur   , &
                              chaine   )

! Declarations ----------------------------------------

  use M_MESSAGE_CASIER_C   ! Liste des messages d'erreur
  use M_ERREUR_T    ! Definition du type ERREUR_T
  use M_FICHIER_T   ! UL_LST_CAS

  implicit none

  !... Arguments ...

  type(ERREUR_T), intent(inout) :: Erreur  ! Erreur
  character(*),   intent(in   ) :: chaine  ! argument d'entree

! Instructions ----------------------------------------
  ! Message long
  if (UL_LST_CAS > 0) then
      call preliminaire(Erreur)

      write(UL_LST_CAS,Erreur%ft)chaine
      write(UL_LST_CAS,banniere)
  endif
  ! Message court
  write(Erreur%message,Erreur%ft_c)chaine

end subroutine TRAITER_ERREUR_CASIER_1C

! =======================================================

subroutine TRAITER_ERREUR_CASIER_2C (Erreur   , &
                              chaine_1 , &
                              chaine_2 )

! Declarations ----------------------------------------

  use M_MESSAGE_CASIER_C   ! Liste des messages d'erreur
  use M_ERREUR_T    ! Definition du type ERREUR_T
  use M_FICHIER_T   ! UL_LST_CAS

  implicit none

  !... Arguments ...

  type(ERREUR_T), intent(inout) :: Erreur    ! Erreur
  character(*),   intent(in   ) :: chaine_1  ! argument d'entree
  character(*),   intent(in   ) :: chaine_2  ! argument d'entree

! Instructions ----------------------------------------
  ! Message long
  if (UL_LST_CAS > 0) then
      call preliminaire(Erreur)

      write(UL_LST_CAS,Erreur%ft)chaine_1, chaine_2
      write(UL_LST_CAS,banniere)
  endif
  ! Message court
  write(Erreur%message,Erreur%ft_c)chaine_1, chaine_2

end subroutine TRAITER_ERREUR_CASIER_2C

! =======================================================

subroutine TRAITER_ERREUR_CASIER_1I (Erreur  , &
                              entier  )

! Declarations ----------------------------------------

  use M_MESSAGE_CASIER_C   ! Liste des messages d'erreur

  use M_ERREUR_T    ! Definition du type ERREUR_T
  use M_FICHIER_T   ! UL_LST_CAS

  implicit none

  !... Arguments ...

  type(ERREUR_T),intent(inout) :: Erreur   ! Erreur
  integer,       intent(in   ) :: entier   ! argument d'entree

! Instructions ----------------------------------------

  if (UL_LST_CAS > 0) then
      call preliminaire (Erreur)

      write(UL_LST_CAS,Erreur%ft) entier
      write(UL_LST_CAS,banniere)
  endif
  ! Message court
  write(Erreur%message,Erreur%ft_c) entier

end subroutine TRAITER_ERREUR_CASIER_1I

! =======================================================

subroutine TRAITER_ERREUR_CASIER_1I1C (Erreur,      &
                                entier_1,    &
                                chaine_2     )

! Declarations ----------------------------------------

  use M_MESSAGE_CASIER_C   ! Liste des messages d'erreur

  use M_ERREUR_T    ! Definition du type ERREUR_T
  use M_FICHIER_T   ! UL_LST_CAS

  implicit none

  !... Arguments ...

  type(ERREUR_T),intent(inout) :: Erreur        ! Erreur
  integer,       intent(in   ) :: entier_1      ! argument d'entree
  character(*),  intent(in   ) :: chaine_2      ! argument d'entree

! Instructions ----------------------------------------

  if (UL_LST_CAS > 0) then
      call preliminaire (Erreur)

      write(UL_LST_CAS,Erreur%ft) entier_1 , chaine_2
      write(UL_LST_CAS,banniere)
  endif
  ! Message court
  write(Erreur%message,Erreur%ft_c) entier_1 , chaine_2

end subroutine TRAITER_ERREUR_CASIER_1I1C

! =======================================================

subroutine TRAITER_ERREUR_CASIER_1I2C (Erreur,      &
                                entier_1,    &
                                chaine_1,    &
                                chaine_2     )

! Declarations ----------------------------------------

  use M_MESSAGE_CASIER_C   ! Liste des messages d'erreur

  use M_ERREUR_T    ! Definition du type ERREUR_T
  use M_FICHIER_T   ! UL_LST_CAS

  implicit none

  !... Arguments ...

  type(ERREUR_T),intent(inout) :: Erreur              ! Erreur
  integer,       intent(in   ) :: entier_1            ! argument d'entree
  character(*),  intent(in   ) :: chaine_2, chaine_1  ! argument d'entree

! Instructions ----------------------------------------

  if (UL_LST_CAS > 0) then
      call preliminaire (Erreur)

      write(UL_LST_CAS,Erreur%ft) entier_1 , chaine_1, chaine_2
      write(UL_LST_CAS,banniere)
  endif
  ! Message court
  write(Erreur%message,Erreur%ft_c) entier_1 , chaine_1, chaine_2

end subroutine TRAITER_ERREUR_CASIER_1I2C


!================================================================

subroutine TRAITER_ERREUR_CASIER_2I (Erreur    , &
                              entier_1  , &
                              entier_2  )

! Declarations ----------------------------------------

  use M_MESSAGE_CASIER_C   ! Liste des messages d'erreur

  use M_ERREUR_T    ! Definition du type ERREUR_T
  use M_FICHIER_T   ! UL_LST_CAS

  implicit none

  !... Arguments ...

  type(ERREUR_T),intent(inout) :: Erreur    ! Erreur
  integer,       intent(in   ) :: entier_1  ! 1er argument d'entree
  integer,       intent(in   ) :: entier_2  ! 2emeargument d'entree

! Instructions ----------------------------------------

  if (UL_LST_CAS > 0) then
      call preliminaire (Erreur)

      write(UL_LST_CAS,Erreur%ft)entier_1, entier_2
      write(UL_LST_CAS,banniere)
  endif
  ! Message court
  write(Erreur%message, Erreur%ft_c) entier_1, entier_2

end subroutine TRAITER_ERREUR_CASIER_2I

!================================================================

subroutine TRAITER_ERREUR_CASIER_3I (Erreur    , &
                              entier_1  , &
                              entier_2  , &
                              entier_3  )

! Declarations ----------------------------------------

  use M_MESSAGE_CASIER_C   ! Liste des messages d'erreur

  use M_ERREUR_T    ! Definition du type ERREUR_T
  use M_FICHIER_T   ! UL_LST_CAS

  implicit none

  !... Arguments ...

  type(ERREUR_T),intent(inout) :: Erreur    ! Erreur
  integer,       intent(in   ) :: entier_1  ! 1er argument d'entree
  integer,       intent(in   ) :: entier_2  ! 2emeargument d'entree
  integer,       intent(in   ) :: entier_3  ! 3emeargument d'entree

! Instructions ----------------------------------------

  if (UL_LST_CAS > 0) then
      call preliminaire (Erreur)

      write(UL_LST_CAS,Erreur%ft)entier_1, entier_2, entier_3
      write(UL_LST_CAS,banniere)
  endif
  ! Message court
  write(Erreur%message, Erreur%ft_c) entier_1, entier_2, entier_3

end subroutine TRAITER_ERREUR_CASIER_3I


!=============================================================

subroutine TRAITER_ERREUR_CASIER_1I1R    (Erreur   , &
                                   entier_1 , &
                                   reel_1     &
                                              )

! Declarations ----------------------------------------

  use M_PRECISION

  use M_MESSAGE_CASIER_C   ! Liste des messages d'erreur

  use M_ERREUR_T    ! Definition du type ERREUR_T
  use M_FICHIER_T   ! UL_LST_CAS

  implicit none

  !... Arguments ...

  type(ERREUR_T)            , intent(inout) :: Erreur    ! Erreur
  integer                   , intent(in   ) :: entier_1  ! Argument d'entree
  real(DOUBLE)              , intent(in   ) :: reel_1  ! Argument d'entree

! Instructions ----------------------------------------

  if (UL_LST_CAS > 0) then
      call preliminaire (Erreur)

      write(UL_LST_CAS,banniere)
      write(UL_LST_CAS,Erreur%ft)entier_1, reel_1

      write(UL_LST_CAS,banniere)
  endif
  ! Message court
  write(Erreur%message,Erreur%ft_c)entier_1,reel_1

end subroutine TRAITER_ERREUR_CASIER_1I1R


!=============================================================

subroutine TRAITER_ERREUR_CASIER_1I3R    (Erreur   , &
                                   entier_1 , &
                                   reel_1   , &
                                   reel_2   , &
                                   reel_3     &
                                              )

! Declarations ----------------------------------------

  use M_PRECISION

  use M_MESSAGE_CASIER_C   ! Liste des messages d'erreur

  use M_ERREUR_T    ! Definition du type ERREUR_T
  use M_FICHIER_T   ! UL_LST_CAS

  implicit none

  !... Arguments ...

  type(ERREUR_T)            , intent(inout) :: Erreur    ! Erreur
  integer                   , intent(in   ) :: entier_1  ! Argument d'entree
  real(DOUBLE)              , intent(in   ) :: reel_1  ! Argument d'entree
  real(DOUBLE)              , intent(in   ) :: reel_2    ! Argument d'entree
  real(DOUBLE)              , intent(in   ) :: reel_3    ! Argument d'entree

! Instructions ----------------------------------------

  if (UL_LST_CAS > 0) then
      call preliminaire (Erreur)

      write(UL_LST_CAS,Erreur%ft)entier_1, reel_1, reel_2, reel_3
      write(UL_LST_CAS,banniere)
  endif
  ! Message court
  write(Erreur%message,Erreur%ft_c)entier_1,reel_1, reel_2, reel_3

end subroutine TRAITER_ERREUR_CASIER_1I3R


!=============================================================

subroutine TRAITER_ERREUR_CASIER_1C1R    (Erreur   , &
                                   chaine_1 , &
                                   reel_1    )


! Declarations ----------------------------------------

  use M_PRECISION

  use M_MESSAGE_CASIER_C   ! Liste des messages d'erreur

  use M_ERREUR_T    ! Definition du type ERREUR_T
  use M_FICHIER_T   ! UL_LST_CAS

  implicit none

  !... Arguments ...

  type(ERREUR_T)            , intent(inout) :: Erreur    ! Erreur
  character(*)              , intent(in   ) :: chaine_1  ! Argument d'entree
  real(DOUBLE)              , intent(in   ) :: reel_1  ! Argument d'entree

! Instructions ----------------------------------------

  if (UL_LST_CAS > 0) then
      call preliminaire (Erreur)

      write(UL_LST_CAS,Erreur%ft)chaine_1, reel_1
      write(UL_LST_CAS,banniere)
  endif
  ! Message court
  write(Erreur%message,Erreur%ft_c)chaine_1,reel_1

end subroutine TRAITER_ERREUR_CASIER_1C1R

!=============================================================

subroutine TRAITER_ERREUR_CASIER_3I1C1R   (Erreur   , &
                                   entier_1 , &
                                   entier_2 , &
                                   entier_3 , &
                                   chaine_1 , &
                                   reel_1   )

! Declarations ----------------------------------------

  use M_PRECISION

  use M_MESSAGE_CASIER_C   ! Liste des messages d'erreur

  use M_ERREUR_T    ! Definition du type ERREUR_T
  use M_FICHIER_T   ! UL_LST_CAS

  implicit none

  !... Arguments ...

  type(ERREUR_T)            , intent(inout) :: Erreur    ! Erreur
  integer                   , intent(in   ) :: entier_1, entier_2, entier_3  ! Argument d'entree
  real(DOUBLE)              , intent(in   ) :: reel_1  ! Argument d'entree
  character(*)              , intent(in   ) :: chaine_1


! Instructions ----------------------------------------

  if (UL_LST_CAS > 0) then
      call preliminaire (Erreur)

      write(UL_LST_CAS,Erreur%ft)entier_1, entier_2, entier_3, chaine_1, reel_1
      write(UL_LST_CAS,banniere)
  endif
  ! Message court
  write(Erreur%message,Erreur%ft_c)entier_1,entier_2,entier_3,chaine_1,reel_1

end subroutine TRAITER_ERREUR_CASIER_3I1C1R

!=============================================================

subroutine TRAITER_ERREUR_CASIER_1I1C1I   (Erreur   , &
                                   entier_1 , &
                                   chaine_1 , &
                                   entier_2 )

! Declarations ----------------------------------------

  use M_PRECISION

  use M_MESSAGE_CASIER_C   ! Liste des messages d'erreur

  use M_ERREUR_T    ! Definition du type ERREUR_T
  use M_FICHIER_T   ! UL_LST_CAS

  implicit none

  !... Arguments ...

  type(ERREUR_T)            , intent(inout) :: Erreur    ! Erreur
  integer                   , intent(in   ) :: entier_1, entier_2 ! Argument d'entree
  character(*)              , intent(in   ) :: chaine_1


! Instructions ----------------------------------------

  if (UL_LST_CAS > 0) then
      call preliminaire (Erreur)

      write(UL_LST_CAS,Erreur%ft)entier_1, chaine_1, entier_2
      write(UL_LST_CAS,banniere)
  endif
  ! Message court
  write(Erreur%message,Erreur%ft_c) entier_1, chaine_1, entier_2

end subroutine TRAITER_ERREUR_CASIER_1I1C1I

!=============================================================

subroutine TRAITER_ERREUR_CASIER_1R1C    (Erreur   , &
                                   reel   , &
                                   chaine   )

! Declarations ----------------------------------------

  use M_PRECISION

  use M_MESSAGE_CASIER_C   ! Liste des messages d'erreur

  use M_ERREUR_T    ! Definition du type ERREUR_T
  use M_FICHIER_T   ! UL_LST_CAS

  implicit none

  !... Arguments ...

  type(ERREUR_T)            , intent(inout) :: Erreur    ! Erreur
  real(DOUBLE)              , intent(in   ) :: reel    ! Argument d'entree
  character(*)              , intent(in   ) :: chaine


! Instructions ----------------------------------------

  if (UL_LST_CAS > 0) then
      call preliminaire (Erreur)

      write(UL_LST_CAS,Erreur%ft)reel, chaine
      write(UL_LST_CAS,banniere)
  endif
  ! Message court
  write(Erreur%message,Erreur%ft_c) reel, chaine

end subroutine TRAITER_ERREUR_CASIER_1R1C

!==========================================================

subroutine preliminaire (Erreur)

use M_FICHIER_T  ! UL_LST_CAS
use M_MESSAGE_CASIER_C  ! banniere
use M_ERREUR_T   ! Erreur_T

! argument
  type(ERREUR_T), intent(in   ) :: Erreur


!================== Instructions =========================

! Ecritures d'en-tete

  write(UL_LST_CAS,banniere)

  write(UL_LST_CAS,'("Erreur n0 ",i4)')      Erreur%numero
  write(UL_LST_CAS,'("Arbre d''appel : ",A)')!Erreur%arbredappel

  write(UL_LST_CAS,banniere)

end subroutine preliminaire



end module M_TRAITER_ERREUR_CASIER_I
