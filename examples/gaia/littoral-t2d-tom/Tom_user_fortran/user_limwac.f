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
      USE DECLARATIONS_TOMAWAC, ONLY :  LIFBOR, NBOR
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
      INTEGER IFF, IPLAN,IPTFR
      INTEGER IP,IMIL(40),IDRO(40),IGAU(40)
!
!-----------------------------------------------------------------------
!     MODIFICATION M. BENOIT (12/03/2002) POUR METTRE SUR LES LIMITES
!     LATERALES LE SPECTRE CALCULE SUR L'AXE DU DOMAINE
!     (ATTENTION : CECI N'EST VALABLE QUE POUR LE MAILLAGE "COURANT
!      LITTORAL" ; LES NUMEROS DE POINTS SONT CODES EN DUR)
!-----------------------------------------------------------------------
!
      DO IPTFR=1,NPTFR
        IF(LIFBOR(IPTFR).EQ.KENT) THEN
          DO IFF=1,NF
            DO IPLAN=1,NDIRE
              F(NBOR(IPTFR),IPLAN,IFF)=FBOR(IPTFR,IPLAN,IFF)
            ENDDO
          ENDDO
        ENDIF
      ENDDO
      DO IP=1,40
        IMIL(IP)=1117+IP-1
        IF (IMIL(IP).EQ.1156) IMIL(IP)=116
        IGAU(IP)=180-IP+1
        IDRO(IP)= 52+IP-1
      ENDDO
!
      IF(NCSIZE.GT.1) THEN
        DO IP=1,40
          CALL BORD_WAC(F,NDIRE,NF,NPOIN2,IP)
        ENDDO
      ENDIF
!
      IF(NCSIZE.LE.1) THEN
        DO IP=1,40
          DO IFF=1,NF
            DO IPLAN = 1,NDIRE
              F(IGAU(IP),IPLAN,IFF) = F(IMIL(IP),IPLAN,IFF)
              F(IDRO(IP),IPLAN,IFF) = F(IMIL(IP),IPLAN,IFF)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
