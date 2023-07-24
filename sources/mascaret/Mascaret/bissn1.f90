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

SUBROUTINE BISSN1( &
                   X , &
                   A , &
                   B , &
                   C , &
                  KM , &
                 NFU , &
              ERREUR )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!
!                 RESOLUTION DE SYSTEMES TRIDIAGONAUX
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !   X       ! TR ! M  ! ENTREE: SECOND MEMBRES                       !
! !           !    !    ! SORTIE: RESULTATS                            !
! ! A,B,C     ! TR ! M  ! MATRICE TRIDIAGONALE PAR BLOC                !
! !  KM       ! E  ! D  ! DIMENSION DU SYSTEME                         !
! !  NFU      ! E  ! D  ! NOMBRE DE FONCTIONS                          !
! !___________!____!____!______________________________________________!
!***********************************************************************

   !
   ! DECLARATION DES VARIABLES
   !
   use M_PRECISION
   use M_ERREUR_T
   use M_PROMAT_I
   use M_PROMVT_I
   use M_INVMAT_I

   !
   Implicit none

   Real(DOUBLE) , dimension(:,:)    , intent(inout) :: A , B , C , X
   Integer                          , intent(in   ) :: NFU , KM
   Type( ERREUR_T )                 , intent(inout) :: ERREUR

   !
   !     variables locales
   !
   Real(DOUBLE)                     :: FK(4) , EK(4) , IBD(4) , NORM
   Real(DOUBLE)                     :: DET(4) , YD(4) , ZD(4) , IDET(4)
   ! Array used for Memory optimization (from intel debug)
   Real(DOUBLE)                     :: TMP1(4) , TMP2(4)
   Integer                          :: KM1 , K , J , KM2

   KM1 = KM - 1

   IF( KM1.LT.1 ) THEN
      !PRINT * , ' **************'
      !PRINT * , ' * SSP BISSND *'
      !PRINT * , ' **************'
      !PRINT * , ' STOP KM1=' , KM1
      !STOP
    Erreur%Numero  = 1
    Erreur%Message = 'Error in the linear solver BISSN1 (variable KM1)'
    return
   ENDIF

   IF( NFU.NE.1 ) THEN
      !PRINT * , ' **************'
      !PRINT * , ' * SSP BISSND *'
      !PRINT * , ' **************'
      !PRINT * , ' STOP NFU=' , NFU
      !STOP
    Erreur%Numero  = 1
    Erreur%Message = 'Error in the linear solver BISSN1 (variable NFU)'
    return
   ENDIF

   DO 30 K = 2 , KM1

      KM2 = K - 1
      TMP1 = B(1:,KM2)
      CALL INVMAT( IBD , TMP1 , 2 , K , Erreur )
      B(1:,KM2) = TMP1
      IF(Erreur%Numero.ne.0) RETURN
      TMP1 = A(1:,K)
      CALL PROMAT( EK , TMP1 , IBD , 2 )
      A(1:,K) = TMP1
      TMP1 = C(1:,K - 1)
      CALL PROMAT( FK , EK , TMP1 , 2 )
      C(1:,K - 1) = TMP1

      DO 31 J = 1 , 4
         B(J,K) = B(J,K) - FK(J)
      31 CONTINUE

      NORM = DSQRT( X(1,K)**2 + X(2,K)**2 )

      TMP1(1:2) = X(1:,K - 1)
      CALL PROMVT( ZD , EK , TMP1 , 2 )
      X(1:,K - 1) = TMP1(1:2)

      X(1,K) = X(1,K) - ZD(1)
      X(2,K) = X(2,K) - ZD(2)

   30 CONTINUE

   TMP1 = B(1:,KM - 1)
   TMP2 = B(1:,KM)
   CALL PROMAT( EK , TMP1 , TMP2 , 2 )
   B(1:,KM - 1) = TMP1
   B(1:,KM) = TMP2
   TMP1 = A(1:,KM)
   TMP2 = C(1:,KM - 1)
   CALL PROMAT( FK , TMP1 , TMP2 , 2 )
   A(1:,KM) = TMP1
   C(1:,KM - 1) = TMP2

   DET(1) = FK(1) - EK(1)
   DET(2) = FK(2) - EK(2)
   DET(3) = FK(3) - EK(3)
   DET(4) = FK(4) - EK(4)

   CALL INVMAT( IDET , DET , 2 , KM , Erreur )
   IF(Erreur%Numero.ne.0) RETURN

   IF( DABS( X(1,KM) ).LE.1.D-15 ) GOTO 40

   TMP1 = A(1:,KM)
   TMP2(1:2) = X(1:,KM1)
   CALL PROMVT( YD , TMP1 , TMP2 , 2 )
   A(1:,KM) = TMP1
   X(1:,KM1) = TMP2(1:2)
   TMP1 = B(1:,KM - 1)
   TMP2(1:2) = X(1:,KM)
   CALL PROMVT( ZD , TMP1 , TMP2 , 2 )
   B(1:,KM - 1) = TMP1
   X(1:,KM) = TMP2(1:2)

   ZD(1) = YD(1) - ZD(1)
   ZD(2) = YD(2) - ZD(2)

   TMP1(1:2) = X(1:,KM)
   CALL PROMVT( TMP1 , IDET , ZD , 2 )
   X(1:,KM) = TMP1(1:2)

   40 K = KM

   50 K = K - 1

   NORM = DSQRT( X(1,K)**2 + X(2,K)**2 )

   TMP1 = B(1:,K)
   CALL INVMAT( IBD , TMP1 , 2 , K , ERREUR )
   B(1:,K) = TMP1
   IF(Erreur%Numero.ne.0) RETURN
   TMP1 = C(1:,K)
   TMP2(1:2) = X(1:,K + 1)
   CALL PROMVT( YD , TMP1 , TMP2 , 2 )
   C(1:,K) = TMP1
   X(1:,K + 1) = TMP2(1:2)

   ZD(1) = X(1,K) - YD(1)
   ZD(2) = X(2,K) - YD(2)

   TMP1(1:2) = X(1:,K)
   CALL PROMVT( TMP1 , IBD , ZD , 2 )
   X(1:,K) = TMP1(1:2)

   IF( K.GT.1 ) GO TO 50

   RETURN

END SUBROUTINE BISSN1
