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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TOMAWAC, EX_USER_LIMWAC => USER_LIMWAC
      USE DECLARATIONS_TOMAWAC, ONLY : FB_CTE, DEUPI, FREQ, LIFBOR, NBOR
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
      DOUBLE PRECISION DTETAR, DF3, AUX
      INTEGER IFF, IPLAN, IPTFR
!
!-----------------------------------------------------------------------
!
!======================================================================
!MJTS MODIF POUR CAS MONOCHROMATIQUE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!======================================================================
!     Permet d'avoir Hm0 = 3.54 m qui est le Hm0 cible; freq=1 Hz
      DTETAR=DEUPI/DBLE(NDIRE)
      DF3=0.5D0*(FREQ(4)/FREQ(3)-1.0D0)*(FREQ(4)+FREQ(3))
      AUX=(3.54D0)**2/(16.0D0*DTETAR*DF3)
!
      DO IFF=1,NF
        DO IPLAN=1,NDIRE
          FB_CTE(IPLAN,IFF)=0.0D0
          DO IPTFR=1,NPTFR
            FBOR(IPTFR,IPLAN,IFF)=0.0D0
          ENDDO
        ENDDO
      ENDDO
!     Put energy on a single bin : 10th direction (=90 deg for NDIRE=36
!     and 4rd frequency
      FB_CTE(10,4)=AUX
      DO IPTFR=1,NPTFR
        FBOR(IPTFR,10,4)=AUX
      ENDDO
      DO IPTFR=1,NPTFR
        IF(LIFBOR(IPTFR).EQ.KENT) THEN
          DO IFF=1,NF
            DO IPLAN=1,NDIRE
              F(NBOR(IPTFR),IPLAN,IFF)=FBOR(IPTFR,IPLAN,IFF)
            ENDDO
          ENDDO
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
