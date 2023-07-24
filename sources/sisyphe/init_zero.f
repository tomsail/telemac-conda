!                   ********************
                    SUBROUTINE INIT_ZERO
!                   ********************
!
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    INITIALISES VARIABLES.
!
!history  JMH
!+        04/06/2008
!+
!+   INITIALISATION OF MASDEP
!
!history  C. VILLARET
!+
!+        V5P9
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SISYPHE
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
!========================================================================
!                         INITIALISES
! =======================================================================
!
!---- THE WORKING ARRAYS
!
      IF(NPRIV > 0) CALL OS ('X=0     ', X=PRIVE)
      CALL OS('X=0     ', X=T12   )
      CALL OS('X=0     ', X=COEFPN)
!
!---- THE SEDIMENT VARIABLES :
!
      CALL OS('X=0     ', X=QS)
      CALL OS('X=0     ', X=QSX)
      CALL OS('X=0     ', X=QSY)
      CALL OS('X=0     ', X=QSCLXC )
      CALL OS('X=0     ', X=QSCLYC )
      CALL OS('X=0     ', X=QSCLXS )
      CALL OS('X=0     ', X=QSCLYS )
!
! PROVISIONAL INITIALISATION FOR FIRST OUTPUT IN RESULTS FILE
!
      CALL OS('X=0     ', X=QSCL )  ! BLOCK OF SIZE NSICLA
      CALL OS('X=0     ', X=QS_S )
      CALL OS('X=0     ', X=QSXS )
      CALL OS('X=0     ', X=QSYS )
      CALL OS('X=0     ', X=QS_C )
      CALL OS('X=0     ', X=QSXC )
      CALL OS('X=0     ', X=QSYC )
!
! PROBABLY USEFUL ONLY IF(CHARR) AND WITH FINITE ELEMENTS
!
      CALL OS('X=0     ', X=ZFCL_C )
!
!---- THE DEPOSITION MASSES FOR EVERY CLASS IN SUSPENSION
!
      DO I=1,NSICLA
        MASDEP(I)=0.D0
      ENDDO
!
      CALL OS('X=0     ', X=E    )
      CALL OS('X=0     ', X=ESOMT)
      CALL OS('X=0     ', X=CS   )
!
!---- THE HYDRODYNAMIC VARIABLES :
!
      CALL OS('X=0     ', X=QU )
      CALL OS('X=0     ', X=QV )
      CALL OS('X=0     ', X=U2D )
      CALL OS('X=0     ', X=V2D )
      CALL OS('X=0     ', X=HN )
      CALL OS('X=0     ', X=Q  )
      CALL OS('X=0     ', X=TOB)
!
!---- THE WAVE PARAMETERS IF NEED BE
!
!     ALL INITIALISATIONS OF THE WAVES ARE TO BE REMOVED
!     WHEN ALL CHECKS WILL BE DONE
!     SEE BEDLOAD_BAILARD, DIBWAT, BIJKER AND SOULSBY
!
!
!     FW=0.3 CORRESPONDS TO NO WAVES, 0 WOULD DO A LOG(0)
      CALL OS('X=C     ', X=FW ,C=0.3D0   )   !
      CALL OS('X=0     ', X=HW    )   !
      CALL OS('X=0     ', X=TW    )   !
      CALL OS('X=C     ', X=THETAW, C=90.D0)  !
      CALL OS('X=0     ', X=UW    )   !
      CALL OS('X=0     ', X=TOBW)     !
!
!-----------------------------------------------------------------------
!
      RETURN
      END
