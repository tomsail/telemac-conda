      SUBROUTINE MONOCRO
     &(F, NPOIN2, NF, NDIRE, IFREQ, JDIR)
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!.....Variables transmises
!     """"""""""""""""""""
      INTEGER NPOIN2, NF, NDIRE, IFREQ, JDIR
      DOUBLE PRECISION F(NPOIN2,NDIRE,NF)
!
!.....Variables locales
!     """"""""""""""""""""
      INTEGER IP, JF, JD
      DOUBLE PRECISION AUX
!
!
      DO IP=1,NPOIN2
        AUX=F(IP,JDIR,IFREQ)
        DO JF=1,NF
          DO JD=1,NDIRE
            F(IP,JD,JF)=0.0D0
          END DO
        END DO
        F(IP,JDIR,IFREQ)=AUX
      END DO
!
      RETURN
      END

