!                   *********************
                    SUBROUTINE EROSION_FX
!                   *********************
!
     &(SEDERO,TAUB,SF,TAUR,ERO,ZZERO,NPOIN)
!
!***********************************************************************
! WAQTEL   V8P2
!***********************************************************************
!
!brief    COMPUTES EROSION FLUX
!
!
!history  R. ATA (LNHE)
!+        02/09/2015
!+        V7P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ERO            |-->| EROSION RATE
!| NPOIN          |-->| TOTAL NUMBER OF MESH NODES
!| SEDERO         |<--| EROSION FLUX
!| SF             |-->| BED SEDIMENTS
!| TAUB           |-->| BED SHEAR STRESS
!| TAUR           |-->| CRITICAL STRESS OF RESUSPENSION
!| ZZERO          |-->| EPS UNDER WHICH VALUE IS CONSIDERED ZERO
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_WAQTEL, EX_EROSION_FX => EROSION_FX
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)     :: NPOIN
      DOUBLE PRECISION, INTENT(IN)     :: TAUR,ERO,ZZERO
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TAUB,SF
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: SEDERO
      INTRINSIC MAX
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!     LOCAL VARIABLES
      INTEGER I
      DOUBLE PRECISION  CC
!
      IF (ABS(TAUR).LT.ZZERO)THEN
        WRITE(LU,*) 'EROSION_FX: CRITICAL STRESS OF SEDIMENTATION  '
        WRITE(LU,*) '          TAUR VERY SMALL OR NIL - VERIFY !!!'
        WRITE(LU,*) '          TAUR = ',TAUR
        CALL PLANTE(1)
        STOP
      ENDIF
!     THIS WAY WORKS WELL FOR 2D AND FOR 3D AS WELL SINCE BED LAYER IS
!     FOR I=1 TO NPOIN
      DO I=1,NPOIN
        IF(TAUB%R(I).GT.TAUR)THEN
          CC=ERO*(TAUB%R(I)/TAUR -1.D0)
!     TONY C (AKA HYDROENVIRONMENTAL) SUGGESTION
!     IN WETTING AND DRYING MUD FLATS, THE USE OF ANTI DIRAC
!     SEEMS TO GROW ERROR WITH NEGATIVE SEDIMENT FRACTION TOLERATED AND
!     RETURNING AS A SOURCE TO THE SUSPENDED SOILIDS 
!     BETTER TO HAVE NO EROSION OF A NEGATIVE SEDIMENT MASS WITH MAX
!          SEDERO%R(I)=CC*ANTI_DIRAC(SF%R(I),ZZERO)
          SEDERO%R(I)=CC*MAX(SF%R(I),ZZERO)
        ELSE
          SEDERO%R(I)=0.D0
        ENDIF
      ENDDO
!
      RETURN
      END
!
!-----------------------------------------------------------------------
!
