!                   ************************
                    SUBROUTINE USER_CONDIN_H
!                   ************************
!
!
!***********************************************************************
! TELEMAC2D   V7P3
!***********************************************************************
!
!brief    USER INITIALISES THE PHYSICAL PARAMETERS U, V
!
!history  J-M HERVOUET (LNHE)
!+        30/08/2007
!+        V6P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE TPXO
      USE OKADA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I, NSEC,NFO1
!
!-----------------------------------------------------------------------
!
      NFO1=T2D_FILES(T2DFO1)%LU
!jaj free surface initialisation from a file and using surfini

      READ(NFO1,*)
      READ(NFO1,*) NSEC
      WRITE(LU,*) 'CONDIN: READING FREE SURFACE INITIALISATION FILE'
      WRITE(LU,*) 'CONDIN: NSEC = ',NSEC
      WRITE(LU,*) ' '
      WRITE(LU,'(5(1X,A15))')
     &    'XLEFT', 'YLEFT', 'XRIGHT', 'YRIGHT', 'WATER_LEVEL'
      DO I=1,NSEC
        READ(NFO1,*) T1%R(I), T2%R(I), T4%R(I), T5%R(I), T3%R(I)
        T6%R(I) = T3%R(I)
        WRITE(LU,'(5(1X,G15.6))')
     &     T1%R(I), T2%R(I), T4%R(I), T5%R(I), T3%R(I)
      END DO
      WRITE(LU,*) ' '

      WRITE(LU,*) 'CONDIN: COTINI = ',COTINI
      CALL OS( 'X=C     ' , H , H  , H , COTINI )

      CALL SURFINI
     & (T1%R, T2%R, T3%R, T4%R, T5%R, T6%R,
     &  T7%R, T8%R, T9%R,
     &  X, Y, H%R, ZF%R,
     &  IT1%I, IT2%I, NSEC, NPOIN)

      CALL OS( 'X=X-Y   ' , H , ZF , ZF , 0.D0 )
!
!-----------------------------------------------------------------------
!
      RETURN
      END
