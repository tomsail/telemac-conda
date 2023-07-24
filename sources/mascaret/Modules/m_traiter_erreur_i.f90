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

module M_TRAITER_ERREUR_I
!***********************************************************************
! PROGICIEL : MASCARET     N. GOUTAL
!
! VERSION : V8P4R0            EDF-CEREMA
!***********************************************************************
interface TRAITER_ERREUR

  module procedure TRAITER_ERREUR

  module procedure TRAITER_ERREUR_1C

  module procedure TRAITER_ERREUR_2C

  module procedure TRAITER_ERREUR_1I

  module procedure TRAITER_ERREUR_1C1I

  module procedure TRAITER_ERREUR_1C1R

  module procedure TRAITER_ERREUR_1C2I

  module procedure TRAITER_ERREUR_1C3I

  module procedure TRAITER_ERREUR_1I1C

  module procedure TRAITER_ERREUR_1R1I1C

  module procedure TRAITER_ERREUR_2I1C

  module procedure TRAITER_ERREUR_2I2C

  module procedure TRAITER_ERREUR_2I

  module procedure TRAITER_ERREUR_3I

  module procedure TRAITER_ERREUR_4I

  module procedure TRAITER_ERREUR_1R1I

  module procedure TRAITER_ERREUR_1R2I

  module procedure TRAITER_ERREUR_1R3I

  module procedure TRAITER_ERREUR_1R2I1R

  module procedure TRAITER_ERREUR_1I1R1I1R

  module procedure TRAITER_ERREUR_1R1I2R

  module procedure TRAITER_ERREUR_1R

  module procedure TRAITER_ERREUR_2R1I

  module procedure TRAITER_ERREUR_3R

  module procedure TRAITER_ERREUR_2I1R

  module procedure TRAITER_ERREUR_1R2I3R

  module procedure TRAITER_ERREUR_1I1R

  module procedure TRAITER_ERREUR_1I1R1I

  module procedure TRAITER_ERREUR_1I3R

  module procedure TRAITER_ERREUR_1I4R

end interface TRAITER_ERREUR

!===========================================================
!                         SOUS-PROGRAMMES
!===========================================================
contains

   subroutine TRAITER_ERREUR (Erreur)

      ! Declarations ----------------------------------------
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T), intent(inout) :: Erreur  ! Erreur

      ! Instructions ----------------------------------------
      ! Message long

      if (UL_LST > 0) then
          call preliminaire(Erreur)

          write(UL_LST,Erreur%ft)
          write(UL_LST,banniere)
      end if

      ! Message court
      write(Erreur%message,Erreur%ft_c)

   end subroutine TRAITER_ERREUR

   ! =======================================================

   subroutine TRAITER_ERREUR_1C ( Erreur   , chaine   )

   ! Declarations ----------------------------------------

      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T), intent(inout) :: Erreur  ! Erreur
      character(*),   intent(in   ) :: chaine  ! argument d'entree

      ! Instructions ----------------------------------------
      ! Message long
      if (UL_LST > 0) then
          call preliminaire(Erreur)

          write(UL_LST,Erreur%ft)chaine
          write(UL_LST,banniere)
      end if

      ! Message court
      write(Erreur%message,Erreur%ft_c)chaine

   end subroutine TRAITER_ERREUR_1C

   ! =======================================================
   subroutine TRAITER_ERREUR_2C (Erreur   , &
                              chaine_1 , &
                              chaine_2 )

   ! Declarations ----------------------------------------
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none
      !... Arguments ...
      type(ERREUR_T), intent(inout) :: Erreur    ! Erreur
      character(*),   intent(in   ) :: chaine_1  ! argument d'entree
      character(*),   intent(in   ) :: chaine_2  ! argument d'entree

      ! Instructions ----------------------------------------
      ! Message long
      if (UL_LST > 0) then
          call preliminaire(Erreur)

          write(UL_LST,Erreur%ft)chaine_1, chaine_2
          write(UL_LST,banniere)
      end if
      ! Message court
      write(Erreur%message,Erreur%ft_c)chaine_1, chaine_2

   end subroutine TRAITER_ERREUR_2C

   ! =======================================================

   subroutine TRAITER_ERREUR_1I (Erreur  , &
                              entier  )

      ! Declarations ----------------------------------------
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T),intent(inout) :: Erreur   ! Erreur
      integer,       intent(in   ) :: entier   ! argument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
          call preliminaire (Erreur)

          write(UL_LST,Erreur%ft) entier
          write(UL_LST,banniere)
      end if

      ! Message court
      write(Erreur%message,Erreur%ft_c) entier

   end subroutine TRAITER_ERREUR_1I

   ! =======================================================
   subroutine TRAITER_ERREUR_1C1I (Erreur,    &
                                   chaine,    &
                                   entier     )

      ! Declarations ----------------------------------------
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T),intent(inout) :: Erreur      ! Erreur
      character(*),  intent(in   ) :: chaine      ! argument d'entree
      integer,       intent(in   ) :: entier      ! argument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
          call preliminaire( Erreur )

          write(UL_LST,Erreur%ft) chaine , entier
          write(UL_LST,banniere)
      end if

      ! Message court
      write(Erreur%message,Erreur%ft_c) chaine, entier

   end subroutine TRAITER_ERREUR_1C1I

   ! =======================================================
   subroutine TRAITER_ERREUR_1C1R (Erreur,    &
                                   chaine,    &
                                   reel     )

      ! Declarations ----------------------------------------
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T),intent(inout) :: Erreur      ! Erreur
      character(*),  intent(in   ) :: chaine      ! argument d'entree
      real(DOUBLE)  ,intent(in   ) :: reel        ! argument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
          call preliminaire( Erreur )

          write(UL_LST,Erreur%ft) chaine , reel
          write(UL_LST,banniere)
      end if

      ! Message court
      write(Erreur%message,Erreur%ft_c) chaine, reel

   end subroutine TRAITER_ERREUR_1C1R

   ! =======================================================
   subroutine TRAITER_ERREUR_1C2I (Erreur,    &
                                   chaine,    &
                                   entier_1,  &
                                   entier_2   )

      ! Declarations ----------------------------------------
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T),intent(inout) :: Erreur      ! Erreur
      character(*),  intent(in   ) :: chaine      ! 1er argument d'entree
      integer,       intent(in   ) :: entier_1    ! 2eme argument d'entree
      integer,       intent(in   ) :: entier_2    ! 3eme argument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
          call preliminaire( Erreur )

          write(UL_LST,Erreur%ft) chaine , entier_1, entier_2
          write(UL_LST,banniere)
      end if

      ! Message court
      write(Erreur%message,Erreur%ft_c) chaine, entier_1, entier_2

   end subroutine TRAITER_ERREUR_1C2I

   ! =======================================================
   subroutine TRAITER_ERREUR_1C3I (Erreur,    &
                                   chaine,    &
                                   entier_1,  &
                                   entier_2,  &
                                   entier_3   )

      ! Declarations ----------------------------------------
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T),intent(inout) :: Erreur      ! Erreur
      character(*),  intent(in   ) :: chaine      ! 1er argument d'entree
      integer,       intent(in   ) :: entier_1    ! 2eme argument d'entree
      integer,       intent(in   ) :: entier_2    ! 3eme argument d'entree
      integer,       intent(in   ) :: entier_3    ! 4eme argument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
          call preliminaire( Erreur )

          write(UL_LST,Erreur%ft) chaine , entier_1, entier_2, entier_3
          write(UL_LST,banniere)
      end if
      ! Message court
      write(Erreur%message,Erreur%ft_c) chaine, entier_1, entier_2,entier_3

   end subroutine TRAITER_ERREUR_1C3I

   ! =======================================================
   subroutine TRAITER_ERREUR_2I1C (Erreur,      &
                                   entier_1,    &
                                   entier_2,    &
                                   chaine_3     )

      ! Declarations ----------------------------------------
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T),intent(inout) :: Erreur        ! Erreur
      integer,       intent(in   ) :: entier_1      ! argument d'entree
      integer,       intent(in   ) :: entier_2      ! argument d'entree
      character(*),  intent(in   ) :: chaine_3      ! argument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
          call preliminaire (Erreur)

          write(UL_LST,Erreur%ft) entier_1 , entier_2 , chaine_3
          write(UL_LST,banniere)
      end if
      ! Message court
      write(Erreur%message,Erreur%ft_c) entier_1 , entier_2 , chaine_3

   end subroutine TRAITER_ERREUR_2I1C

   ! =======================================================
   subroutine TRAITER_ERREUR_2I2C (Erreur,      &
                                   entier_1,    &
                                   entier_2,    &
                                   chaine_3,    &
                                   chaine_4     )

      ! Declarations ----------------------------------------
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T),intent(inout) :: Erreur        ! Erreur
      integer,       intent(in   ) :: entier_1      ! argument d'entree
      integer,       intent(in   ) :: entier_2      ! argument d'entree
      character(*),  intent(in   ) :: chaine_3      ! argument d'entree
      character(*),  intent(in   ) :: chaine_4      ! argument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
          call preliminaire( Erreur )

          write(UL_LST,Erreur%ft) entier_1 , entier_2 , chaine_3 , chaine_4
          write(UL_LST,banniere)
      end if

      ! Message court
      write(Erreur%message,Erreur%ft_c) entier_1 , entier_2 , chaine_3 , chaine_4

   end subroutine TRAITER_ERREUR_2I2C

   ! =======================================================
   subroutine TRAITER_ERREUR_1I1C (Erreur,      &
                                   entier_1,    &
                                   chaine_2     )

      ! Declarations ----------------------------------------
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T),intent(inout) :: Erreur        ! Erreur
      integer,       intent(in   ) :: entier_1      ! argument d'entree
      character(*),  intent(in   ) :: chaine_2      ! argument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
          call preliminaire( Erreur )

          write(UL_LST,Erreur%ft) entier_1 , chaine_2
          write(UL_LST,banniere)
      end if
      ! Message court
      write(Erreur%message,Erreur%ft_c) entier_1 , chaine_2

   end subroutine TRAITER_ERREUR_1I1C

   ! =======================================================
   subroutine TRAITER_ERREUR_1R1I1C (Erreur,      &
                                     reel_1,      &
                                     entier_2,    &
                                     chaine_3     )

      ! Declarations ----------------------------------------
      use M_PRECISION
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T),intent(inout) :: Erreur        ! Erreur
      real(DOUBLE)  ,intent(in   ) :: reel_1        ! argument d'entree
      integer,       intent(in   ) :: entier_2      ! argument d'entree
      character(*),  intent(in   ) :: chaine_3      ! argument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
          call preliminaire( Erreur )

          write(UL_LST,Erreur%ft) reel_1, entier_2 , chaine_3
          write(UL_LST,banniere)
      endif
      ! Message court
      write(Erreur%message,Erreur%ft_c) reel_1, entier_2 , chaine_3

   end subroutine TRAITER_ERREUR_1R1I1C

   !================================================================
   subroutine TRAITER_ERREUR_2I (Erreur    , &
                                 entier_1  , &
                                 entier_2  )

      ! Declarations ----------------------------------------
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T),intent(inout) :: Erreur    ! Erreur
      integer,       intent(in   ) :: entier_1  ! 1er argument d'entree
      integer,       intent(in   ) :: entier_2  ! 2emeargument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
          call preliminaire( Erreur )

          write(UL_LST,Erreur%ft)entier_1, entier_2
          write(UL_LST,banniere)
      endif
      ! Message court
      write(Erreur%message, Erreur%ft_c) entier_1, entier_2

   end subroutine TRAITER_ERREUR_2I

   !================================================================
   subroutine TRAITER_ERREUR_3I (Erreur    , &
                                 entier_1  , &
                                 entier_2  , &
                                 entier_3  )

      ! Declarations ----------------------------------------
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T),intent(inout) :: Erreur    ! Erreur
      integer,       intent(in   ) :: entier_1  ! 1er argument d'entree
      integer,       intent(in   ) :: entier_2  ! 2emeargument d'entree
      integer,       intent(in   ) :: entier_3  ! 3emeargument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
          call preliminaire( Erreur )

          write(UL_LST,Erreur%ft)entier_1, entier_2, entier_3
          write(UL_LST,banniere)
      endif
      ! Message court
      write(Erreur%message, Erreur%ft_c) entier_1, entier_2, entier_3

   end subroutine TRAITER_ERREUR_3I

   !================================================================
   subroutine TRAITER_ERREUR_4I (Erreur    , &
                                 entier_1  , &
                                 entier_2  , &
                                 entier_3  , &
                                 entier_4  )

      ! Declarations ----------------------------------------
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T),intent(inout) :: Erreur    ! Erreur
      integer,       intent(in   ) :: entier_1  ! 1er argument d'entree
      integer,       intent(in   ) :: entier_2  ! 2emeargument d'entree
      integer,       intent(in   ) :: entier_3  ! 3emeargument d'entree
      integer,       intent(in   ) :: entier_4  ! 4emeargument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
          call preliminaire( Erreur )

          write(UL_LST,Erreur%ft)entier_1, entier_2, entier_3, entier_4
          write(UL_LST,banniere)
      endif
      ! Message court
      write(Erreur%message, Erreur%ft_c) entier_1, entier_2, entier_3, entier_4

   end subroutine TRAITER_ERREUR_4I

   !================================================================
   subroutine TRAITER_ERREUR_1R1I (Erreur    , &
                                   reel      , &
                                   entier    )

      ! Declarations ----------------------------------------
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T),intent(inout) :: Erreur    ! Erreur
      real(DOUBLE),  intent(in   ) :: reel      ! 1er  argument d'entree
      integer,       intent(in   ) :: entier    ! 2eme argument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
          call preliminaire( Erreur )

          write(UL_LST,Erreur%ft) reel, entier
          write(UL_LST,banniere)
      endif
      ! Message court
      write(Erreur%message, Erreur%ft_c) reel, entier

   end subroutine TRAITER_ERREUR_1R1I

   !================================================================
   subroutine TRAITER_ERREUR_1R2I (Erreur    , &
                                   reel      , &
                                   entier_1  , &
                                   entier_2  )

      ! Declarations ----------------------------------------
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T),intent(inout) :: Erreur    ! Erreur
      real(DOUBLE),  intent(in   ) :: reel      ! 1er  argument d'entree
      integer,       intent(in   ) :: entier_1  ! 2eme argument d'entree
      integer,       intent(in   ) :: entier_2  ! 3eme argument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
          call preliminaire( Erreur )

          write(UL_LST,Erreur%ft) reel, entier_1, entier_2
          write(UL_LST,banniere)
      endif
      ! Message court
      write(Erreur%message, Erreur%ft_c) reel, entier_1, entier_2

   end subroutine TRAITER_ERREUR_1R2I

   !================================================================
   subroutine TRAITER_ERREUR_1R3I (Erreur    , &
                                   reel      , &
                                   entier_1  , &
                                   entier_2  , &
                                   entier_3  )

      ! Declarations ----------------------------------------
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T),intent(inout) :: Erreur    ! Erreur
      real(DOUBLE),  intent(in   ) :: reel      ! 1er  argument d'entree
      integer,       intent(in   ) :: entier_1  ! 2eme argument d'entree
      integer,       intent(in   ) :: entier_2  ! 3eme argument d'entree
      integer,       intent(in   ) :: entier_3  ! 3eme argument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
          call preliminaire( Erreur )

          write(UL_LST,Erreur%ft) reel, entier_1, entier_2, entier_3
          write(UL_LST,banniere)
      endif
      ! Message court
      write(Erreur%message, Erreur%ft_c) reel, entier_1, entier_2, entier_3

   end subroutine TRAITER_ERREUR_1R3I

   !================================================================
   subroutine TRAITER_ERREUR_1R2I1R (Erreur    , &
                                     reel_1    , &
                                     entier_2  , &
                                     entier_3  , &
                                     reel_4      &
                                              )

      ! Declarations ----------------------------------------
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T),intent(inout) :: Erreur    ! Erreur
      real(DOUBLE),  intent(in   ) :: reel_1    ! 1er  argument d'entree
      integer,       intent(in   ) :: entier_2  ! 2eme argument d'entree
      integer,       intent(in   ) :: entier_3  ! 3eme argument d'entree
      real(DOUBLE),  intent(in   ) :: reel_4    ! 4eme argument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
          call preliminaire (Erreur)

          write(UL_LST,Erreur%ft) reel_1, entier_2, entier_3, reel_4
          write(UL_LST,banniere)
      endif
      ! Message court
      write(Erreur%message, Erreur%ft_c) reel_1, entier_2, entier_3, reel_4

   end subroutine TRAITER_ERREUR_1R2I1R


   !================================================================
   subroutine TRAITER_ERREUR_1I1R1I1R (Erreur    , &
                                       entier_1  , &
                                       reel_2    , &
                                       entier_3  , &
                                       reel_4      &
                                                   )

      ! Declarations ----------------------------------------
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T),intent(inout) :: Erreur    ! Erreur
      integer,       intent(in   ) :: entier_1  ! 2eme argument d'entree
      real(DOUBLE),  intent(in   ) :: reel_2    ! 1er  argument d'entree
      integer,       intent(in   ) :: entier_3  ! 3eme argument d'entree
      real(DOUBLE),  intent(in   ) :: reel_4    ! 4eme argument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
          call preliminaire( Erreur )

          write(UL_LST,Erreur%ft) entier_1, reel_2, entier_3, reel_4
          write(UL_LST,banniere)
      endif
      ! Message court
      write(Erreur%message, Erreur%ft_c) entier_1, reel_2, entier_3, reel_4

   end subroutine TRAITER_ERREUR_1I1R1I1R

   !================================================================
   subroutine TRAITER_ERREUR_1R1I2R (Erreur    , &
                                     reel_1    , &
                                     entier    , &
                                     reel_2    , &
                                     reel_3      &
                                              )

      ! Declarations ----------------------------------------
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T),intent(inout) :: Erreur    ! Erreur
      real(DOUBLE),  intent(in   ) :: reel_1    ! 1er  argument d'entree
      integer,       intent(in   ) :: entier    ! 2eme argument d'entree
      real(DOUBLE),  intent(in   ) :: reel_2    ! 3eme argument d'entree
      real(DOUBLE),  intent(in   ) :: reel_3    ! 4eme argument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
          call preliminaire (Erreur)

          write(UL_LST,Erreur%ft) reel_1, entier, reel_2, reel_3
          write(UL_LST,banniere)
      endif
      ! Message court
      write(Erreur%message, Erreur%ft_c) reel_1, entier, reel_2, reel_3

   end subroutine TRAITER_ERREUR_1R1I2R

   !=============================================================
   subroutine TRAITER_ERREUR_1R (Erreur   , &
                                 reel       &
                                            )

      ! Declarations ----------------------------------------
      use M_PRECISION
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T), intent(inout) :: Erreur  ! Erreur
      real(DOUBLE),   intent(in   ) :: reel    ! argument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
          call preliminaire (Erreur)

          write(UL_LST,Erreur%ft)reel
          write(UL_LST,banniere)
      endif
      ! Message court
      write(Erreur%message,Erreur%ft_c)reel

   end subroutine TRAITER_ERREUR_1R

   !=============================================================
   subroutine TRAITER_ERREUR_2R1I (Erreur   , &
                                   reel_1   , &
                                   reel_2   , &
                                   entier     &
                                           )

      ! Declarations ----------------------------------------
      use M_PRECISION
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T), intent(inout) :: Erreur  ! Erreur
      real(DOUBLE),   intent(in   ) :: reel_1  ! 1er argument d'entree
      real(DOUBLE),   intent(in   ) :: reel_2  ! 2em argument d'entree
      integer     ,   intent(in   ) :: entier  ! 3em argument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
          call preliminaire( Erreur )

          write(UL_LST,Erreur%ft)reel_1, reel_2, entier
          write(UL_LST,banniere)
      endif
      ! Message court
      write(Erreur%message,Erreur%ft_c)reel_1, reel_2, entier

   end subroutine TRAITER_ERREUR_2R1I

   !=============================================================
   subroutine TRAITER_ERREUR_3R (Erreur   , &
                                 reel_1   , &
                                 reel_2   , &
                                 reel_3     &
                                         )

      ! Declarations ----------------------------------------
      use M_PRECISION
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T), intent(inout) :: Erreur  ! Erreur
      real(DOUBLE),   intent(in   ) :: reel_1  ! 1er argument d'entree
      real(DOUBLE),   intent(in   ) :: reel_2  ! 2em argument d'entree
      real(DOUBLE),   intent(in   ) :: reel_3  ! 3em argument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
          call preliminaire (Erreur)

          write(UL_LST,Erreur%ft)reel_1, reel_2, reel_3
          write(UL_LST,banniere)
      endif
      ! Message court
      write(Erreur%message,Erreur%ft_c)reel_1, reel_2, reel_3

   end subroutine TRAITER_ERREUR_3R

   !=============================================================
   subroutine TRAITER_ERREUR_2I1R  (Erreur   , &
                                    entier_1 , &
                                    entier_2 , &
                                    reel_s     &
                                            )

      ! Declarations ----------------------------------------
      use M_PRECISION
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T)            , intent(inout) :: Erreur    ! Erreur
      integer                   , intent(in   ) :: entier_1  ! Argument d'entree
      integer                   , intent(in   ) :: entier_2  ! Argument d'entree
      real(DOUBLE)              , intent(in   ) :: reel_s    ! Argument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
          call preliminaire( Erreur )

          write(UL_LST,Erreur%ft)entier_1, entier_2, reel_s
          write(UL_LST,banniere)
      endif
      ! Message court
      write(Erreur%message,Erreur%ft_c)entier_1, entier_2, reel_s

   end subroutine TRAITER_ERREUR_2I1R

   !=============================================================
   subroutine TRAITER_ERREUR_1R2I3R  (Erreur   , &
                                      reel_1   , &
                                      entier_1 , &
                                      entier_2 , &
                                      reel_2   , &
                                      reel_3   , &
                                      reel_4     &
                                          )

      ! Declarations ----------------------------------------
      use M_PRECISION
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T)            , intent(inout) :: Erreur    ! Erreur
      real(DOUBLE)              , intent(in   ) :: reel_1    ! Argument d'entree
      integer                   , intent(in   ) :: entier_1  ! Argument d'entree
      integer                   , intent(in   ) :: entier_2  ! Argument d'entree
      real(DOUBLE)              , intent(in   ) :: reel_2    ! Argument d'entree
      real(DOUBLE)              , intent(in   ) :: reel_3    ! Argument d'entree
      real(DOUBLE)              , intent(in   ) :: reel_4    ! Argument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
         call preliminaire (Erreur)

         write(UL_LST,Erreur%ft)reel_1, entier_1, entier_2, reel_2, reel_3, reel_4
         write(UL_LST,banniere)
      endif
      ! Message court
      write(Erreur%message,Erreur%ft_c)reel_1, entier_1, entier_2, reel_2, reel_3, reel_4

   end subroutine TRAITER_ERREUR_1R2I3R

   !=============================================================
   subroutine TRAITER_ERREUR_1I1R    (Erreur   , &
                                      entier_1 , &
                                      reel_1     &
                                                 )

      ! Declarations ----------------------------------------
      use M_PRECISION
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T)            , intent(inout) :: Erreur    ! Erreur
      integer                   , intent(in   ) :: entier_1  ! Argument d'entree
      real(DOUBLE)              , intent(in   ) :: reel_1  ! Argument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
          call preliminaire (Erreur)

          write(UL_LST,banniere)
          write(UL_LST,Erreur%ft)entier_1, reel_1

          write(UL_LST,banniere)
      endif
      ! Message court
      write(Erreur%message,Erreur%ft_c)entier_1,reel_1

   end subroutine TRAITER_ERREUR_1I1R

   !=============================================================
   subroutine TRAITER_ERREUR_1I1R1I  (Erreur   , &
                                      entier_1 , &
                                      reel_1   , &
                                      entier_2   &
                                              )

      ! Declarations ----------------------------------------
      use M_PRECISION
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T)            , intent(inout) :: Erreur    ! Erreur
      integer                   , intent(in   ) :: entier_1  ! 1er Argument d'entree
      real(DOUBLE)              , intent(in   ) :: reel_1    ! 2eme Argument d'entree
      integer                   , intent(in   ) :: entier_2  ! 3eme Argument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
          call preliminaire (Erreur)

          write(UL_LST,Erreur%ft)entier_1, reel_1, entier_2
          write(UL_LST,banniere)
      endif
      ! Message court
      write(Erreur%message,Erreur%ft_c)entier_1,reel_1, entier_2

   end subroutine TRAITER_ERREUR_1I1R1I

   !=============================================================
   subroutine TRAITER_ERREUR_1I3R    (Erreur   , &
                                      entier_1 , &
                                      reel_1   , &
                                      reel_2   , &
                                      reel_3     &
                                              )

      ! Declarations ----------------------------------------
      use M_PRECISION
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T)            , intent(inout) :: Erreur    ! Erreur
      integer                   , intent(in   ) :: entier_1  ! Argument d'entree
      real(DOUBLE)              , intent(in   ) :: reel_1  ! Argument d'entree
      real(DOUBLE)              , intent(in   ) :: reel_2    ! Argument d'entree
      real(DOUBLE)              , intent(in   ) :: reel_3    ! Argument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
          call preliminaire( Erreur )

          write(UL_LST,Erreur%ft)entier_1, reel_1, reel_2, reel_3
          write(UL_LST,banniere)
      endif
      ! Message court
      write(Erreur%message,Erreur%ft_c)entier_1,reel_1, reel_2, reel_3

   end subroutine TRAITER_ERREUR_1I3R

   !=============================================================
   subroutine TRAITER_ERREUR_1I4R    (Erreur   , &
                                      entier_1 , &
                                      reel_1   , &
                                      reel_2   , &
                                      reel_3   , &
                                      reel_4     &
                                              )

      ! Declarations ----------------------------------------
      use M_PRECISION
      use M_MESSAGE_C   ! Liste des messages d'erreur
      use M_ERREUR_T    ! Definition du type ERREUR_T
      use M_FICHIER_T   ! UL_LST

      implicit none

      !... Arguments ...
      type(ERREUR_T)            , intent(inout) :: Erreur    ! Erreur
      integer                   , intent(in   ) :: entier_1  ! Argument d'entree
      real(DOUBLE)              , intent(in   ) :: reel_1    ! Argument d'entree
      real(DOUBLE)              , intent(in   ) :: reel_2    ! Argument d'entree
      real(DOUBLE)              , intent(in   ) :: reel_3    ! Argument d'entree
      real(DOUBLE)              , intent(in   ) :: reel_4    ! Argument d'entree

      ! Instructions ----------------------------------------
      if (UL_LST > 0) then
          call preliminaire( Erreur )

          write(UL_LST,Erreur%ft)entier_1, reel_1, reel_2, reel_3, reel_4
          write(UL_LST,banniere)
      endif
      ! Message court
      write(Erreur%message,Erreur%ft_c)entier_1,reel_1, reel_2, reel_3, reel_4

   end subroutine TRAITER_ERREUR_1I4R

   !==========================================================
   subroutine preliminaire (Erreur)

      use M_FICHIER_T  ! U_LST
      use M_MESSAGE_C  ! banniere
      use M_ERREUR_T   ! Erreur_T

      ! argument
      type(ERREUR_T), intent(in   ) :: Erreur

      !================== Instructions =========================
      ! Ecritures d'en-tete
      write(UL_LST,banniere)
      write(UL_LST,'("Erreur n0 ",i3)')      Erreur%numero
      write(UL_LST,'("Arbre d''appel : ",A)')!Erreur%arbredappel
      write(UL_LST,banniere)

   end subroutine preliminaire

end module M_TRAITER_ERREUR_I
