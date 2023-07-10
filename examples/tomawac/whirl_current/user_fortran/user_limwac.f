!                   **********************
                    SUBROUTINE USER_LIMWAC
!                   **********************
!
     &(F     , FBOR  , NPTFR , NDIRE , NF    , NPOIN2,
     & KENT  , PRIVE , NPRIV , IMP_FILE)
!
!***********************************************************************
! TOMAWAC
!***********************************************************************
!
!brief    USER LIMIT SPECTRUM.
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F              |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FBOR           |<->| SPECTRAL VARIANCE DENSITY AT THE BOUNDARIES
!| FPICL          |-->| BOUNDARY PEAK FREQUENCY
!| FRA            |<--| DIRECTIONAL SPREADING FUNCTION VALUES
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| IMP_FILE       |-->| MESH FILE WITH THE IMPOSED SPECTRA
!| KENT           |-->| B.C.: A SPECTRUM IS PRESCRIBED AT THE BOUNDARY
!| KSORT          |-->| B.C.: FREE BOUNDARY: NO ENERGY ENTERING THE DOMAIN
!| LIFBOR         |-->| TYPE OF BOUNDARY CONDITION ON F
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPRIV          |-->| NUMBER OF PRIVATE ARRAYS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| PRIVE          |-->| USER WORK TABLE
!| SPEC           |<--| VARIANCE DENSITY FREQUENCY SPECTRUM
!| TETA           |-->| DISCRETIZED DIRECTIONS
!| TETA1L         |-->| BOUNDARY MAIN DIRECTION 1
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TOMAWAC, EX_USER_LIMWAC => USER_LIMWAC
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI, FPICL , TETA1L,
     &    TETA, FREQ, SPEC, FRA, LIFBOR, NBOR

      USE DECLARATIONS_SPECIAL
      USE BND_SPECTRA
      USE BIEF_DEF, ONLY : BIEF_FILE
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
      INTEGER, INTENT(IN)            :: NPTFR,NDIRE,NF,NPOIN2,NPRIV
      INTEGER, INTENT(IN)            :: KENT
      DOUBLE PRECISION, INTENT(IN)   :: PRIVE(NPOIN2,NPRIV)
      TYPE(BIEF_FILE), INTENT(IN)    :: IMP_FILE
      DOUBLE PRECISION, INTENT(INOUT):: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT):: FBOR(NPTFR,NDIRE,NF)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
      INTEGER IPLAN,IPTFR
!
      DOUBLE PRECISION, ALLOCATABLE :: TRAV(:)
!
      INTEGER IFREQ
!
      DOUBLE PRECISION S00, DTETA
!
!-----------------------------------------------------------------------
!
      ALLOCATE(TRAV(1:NF))
      S00=0.4538D0
      DO IFREQ=1,NF
        IF (FREQ(IFREQ).LT.FPICL) THEN
          TRAV(IFREQ)=S00*(FREQ(IFREQ)/FPICL)**(-2.03D0)
        ELSE
          TRAV(IFREQ)=S00*(FREQ(IFREQ)/FPICL)**(1.04D0)
        ENDIF
      ENDDO
!
      DO IPTFR=1,NPTFR
        IF (LIFBOR(IPTFR).EQ.KENT) THEN
          DO IPLAN=1,NDIRE
            DTETA=TETA(IPLAN)-TETA1L
            IF ((TETA(IPLAN)-TETA1L).GT.DEUPI/2) THEN
              DTETA=DEUPI-DTETA
            ENDIF
            DO IFREQ=1,NF
              FRA(IPLAN)=1.D0/SQRT(DEUPI)*TRAV(IFREQ)*
     &        EXP(-DTETA**2/(2.D0*TRAV(IFREQ)**2))
              FBOR(IPTFR,IPLAN,IFREQ)=SPEC(IFREQ)*FRA(IPLAN)
              F(NBOR(IPTFR),IPLAN,IFREQ)=FBOR(IPTFR,IPLAN,IFREQ)
            ENDDO
          ENDDO
        ENDIF
      ENDDO
!
      DEALLOCATE(TRAV)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
