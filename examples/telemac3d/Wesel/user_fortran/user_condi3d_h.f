!                   *************************
                    SUBROUTINE USER_CONDI3D_H
!                   *************************
!
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    USER INITIALISES DEPTH
!
!history  C.-T. PHAM (LNHE)
!+        24/03/2017
!+        V7P3
!+   Creation from not splitted CONDIM
!+   Called by CONDIM
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_USER_CONDI3D_H => USER_CONDI3D_H
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER I,NSEC,NFO1
!
!-----------------------------------------------------------------------
!
!     JAJ FREE SURFACE INITIALISATION FROM A FILE AND USING SURFINI
      NFO1=T3D_FILES(T3DFO1)%LU
!
      READ(NFO1,*)
      READ(NFO1,*) NSEC
      WRITE(LU,*) 'CONDI3D_H: READING FREE SURFACE INITIALISATION FILE'
      WRITE(LU,*) 'USER_CONDI3D_H: NSEC = ',NSEC
      WRITE(LU,*) ' '
      WRITE(LU,'(5(1X,A15))') 'XLEFT','YLEFT','XRIGHT','YRIGHT',
     &                        'WATER_LEVEL'
      DO I=1,NSEC
        READ(NFO1,*) T3_01%R(I), T3_02%R(I), T3_04%R(I),
     &               T3_05%R(I), T3_03%R(I)
        T3_06%R(I) = T3_03%R(I)
        WRITE(LU,'(5(1X,G15.6))') T3_01%R(I), T3_02%R(I), T3_04%R(I),
     &                            T3_05%R(I), T3_03%R(I)
      ENDDO
      WRITE(LU,*) ' '
!
      WRITE(LU,*) 'USER_CONDI3D_H: COTINI = ',COTINI
      CALL OS( 'X=C     ' , X=H, C=COTINI)
!
      CALL SURFINI(T3_01%R,T3_02%R,T3_03%R,T3_04%R,T3_05%R,T3_06%R,
     &             T3_07%R,T3_08%R,T3_09%R,MESH3D%X%R,MESH3D%Y%R,
     &             H%R,ZF%R,IT1%I,IT2%I,NSEC,NPOIN2)
!
      CALL OS( 'X=X-Y   ' , H , ZF , ZF , 0.D0 )
!
!-----------------------------------------------------------------------
!
      RETURN
      END
