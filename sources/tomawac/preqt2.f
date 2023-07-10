!                   *****************
                    SUBROUTINE PREQT2
!                   *****************
!
!***********************************************************************
! TOMAWAC   V8P0                                   32/12/2018
!***********************************************************************
!
!brief    SOURCE TERM RELATED TO NON-LINEAR INTERACTIONS
!+                BETWEEN FREQUENCY TRIPLETS.
!+                DEVELOPED FROM THE BOUSSINESQ EQUATIONS.
!
!history  EDF/DER/LNH
!+        11/06/98
!+        V5P0
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
!history  G.MATTAROLO (EDF - LNHE)
!+        23/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
      USE DECLARATIONS_TOMAWAC
      USE INTERFACE_TOMAWAC, EX_PREQT2 => PREQT2
      IMPLICIT NONE
!
!.....VARIABLES FROM MODULE TOMAWAC
!     """""""""""""""""""""""""""""
!| QINDI          |<--| CONFIGURATION INDEX
!| NBD            |-->| NUMBER OF TRIAD CONFIGURATIONS
!| BDISPB         |-->| LOWER DIRECTIONAL BOUND. OF SPB TRIAD MODEL
!| BDSSPB         |-->| UPPER DIRECTIONAL BOUND. OF SPB TRIAD MODEL
!| KSPB           |-->| COEFFICIENT K OF SPB TRIAD INTERACTION MODEL

!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER           IPL
      DOUBLE PRECISION  AP2 , DTETA
!
      INTEGER           NBPL , NBPU, NB1
!
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
      IF(.NOT.DEJA_QT2) THEN
        ALLOCATE(QINDI(NDIRE))
        DEJA_QT2=.TRUE.
      ENDIF
!
      DTETA = TETA(2)-TETA(1)
      IF(BDSSPB.GE.BDISPB) THEN
        AP2  = (BDISPB-TETA(1))/DTETA
        NBPL = NINT(AP2) + 1
        AP2  = (BDSSPB-TETA(1))/DTETA
        NBPU = NINT(AP2)
        NBD=NBPU-NBPL+1
        DO IPL=1,NBD
          QINDI(IPL)=NBPL+IPL-1
        END DO
      ELSE
        AP2  = (BDSSPB-TETA(1))/DTETA
        NBPU = NINT(AP2)  + 1
        AP2  = (BDISPB-TETA(1))/DTETA
        NBPL = NINT(AP2)
        IF(NBPL.GT.NDIRE) THEN
          NBPL = 1
          QINDI(1) = 1
          NBD  = NBPU - NBPL + 1
          DO IPL = 2,NBD
            QINDI(IPL)=IPL
          END DO
        ELSE
          NB1 = NDIRE - NBPL + 1
          NBD = NB1 + NBPU
          DO IPL = 1,NB1
            QINDI(IPL)=NBPL+IPL-1
          END DO
          DO IPL = 1,NBPU
            QINDI(IPL+NB1)=IPL
          END DO
        ENDIF
      ENDIF
!
      RETURN
      END
