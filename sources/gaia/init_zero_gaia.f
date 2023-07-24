!                   ********************
                    SUBROUTINE INIT_ZERO_GAIA
!                   ********************
!
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Initialises variables.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_GAIA
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER I,K
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
      CALL OS('X=0     ', X=QSCL_C)
      CALL OS('X=0     ', X=FLUER)
      CALL OS('X=0     ', X=FLUDP)
      CALL OS('X=0     ', X=FLUDPT)
!
! 7 FOLLOWING LINES ADDED BY JMH 22/04/2005
! PROVISIONAL INITIALISATION FOR FIRST OUTPUT IN RESULTS FILE
!
      CALL OS('X=0     ', X=QSCL )  ! BLOCK OF SIZE NSICLA
      CALL OS('X=0     ', X=QS_C )
      CALL OS('X=0     ', X=QSXC )
      CALL OS('X=0     ', X=QSYC )
!
      CALL OS('X=0     ', X=MUDB )
      CALL OS('X=0     ', X=F_MUDB )
!
      CALL OS('X=0     ', X=EVCL_MB )
      CALL OS('X=0     ', X=FLBCLA )
!
!---- BED EVOLUTION, CUMULATED BED EVOLUTION
!     IN KG/M2 AND IN M
!
      CALL OS('X=0     ', X=E    )
      CALL OS('X=0     ', X=ESOMT)
      CALL OS('X=0     ', X=CUMBE)
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
!     SEE BEDLOAD_BAILARD_GAIA, DIBWAT, BIJKER AND SOULSBY
!
!
!     FW=0.3 CORRESPONDS TO NO WAVES, 0 WOULD DO A LOG(0)
      CALL OS('X=C     ', X=FW ,C=0.3D0   )   !
      CALL OS('X=0     ', X=HW    )   !
      CALL OS('X=0     ', X=TW    )   !
      CALL OS('X=C     ', X=THETAW, C=0.D0)  !
      CALL OS('X=C     ', X=THETAC, C=0.D0)  !
      CALL OS('X=0     ', X=UW    )   !
      CALL OS('X=0     ', X=TOBW)     !
!
      IF(NMUD.EQ.0) THEN
        DO I=1,NPOIN
          DO K=1,NOMBLAY
            CONC_MUD(K,I)=0.D0
          ENDDO
        ENDDO
      ENDIF
!-----------------------------------------------------------------------
!
      RETURN
      END
