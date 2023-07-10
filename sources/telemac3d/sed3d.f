!                   ****************
                    SUBROUTINE SED3D
!                   ****************
!
     &(S3D_MASBED, S3D_MASBED0, S3D_MASDEP,S3D_EPAI,
     & S3D_CONC,TRA02,
     & NPOIN2,S3D_NCOUCH,
     & AT,VOLU2D,
     & S3D_CFDEP,S3D_EPAICO,S3D_EPAINCO,S3D_MIXTE)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE RELATIVE MASS BALANCE FOR THE
!+                SEDIMENT DURING A TIMESTEP.
!
!history  C.LE NORMANT(LNH)
!+        26/08/92
!+        V5P1
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
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
!history  J-M HERVOUET (LNHE)
!+        18/03/2011
!+        V6P1
!+   Call to massed replaces the old (and duplicated) formula.
!
!history  C. VILLARET & T. BENSON (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments for sediment, merged on 25/02/2014.
!
!history  G. ANTOINE & M. JODEAU & J.M. HERVOUET (EDF - LNHE)
!+        13/10/2014
!+        V7P0
!+   New developments in sediment for mixed sediment transport
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| S3D_CFDEP      |-->| CONCENTRATION OF MUD DEPOSIT (G/L)
!| S3D_CONC       |-->| CONCENTRATION OF MUD BED LAYER
!|                |   | (MULTILAYER MODEL)
!| S3D_EPAI       |-->| THICKNESS OF SOLID BED LAYER
!|                |   | (S3D_EPAI=DZ/(1+S3D_IVIDE), DZ TOTAL BED THICKNESS)
!| S3D_EPAICO     |-->| THICKNESS OF COHESIVE SUB-LAYER
!| S3D_EPAINCO    |-->| THICKNESS OF NON-COHESIVE SUB-LAYER
!| S3D_MIXTE      |-->| LOGICAL, MIXED SEDIMENTS OR NOT
!| S3D_MASBED     |<->| MASS OF BED
!| S3D_MASBED0    |-->| INITIAL MASS OF BED
!| S3D_MASDEP     |<->| TOTAL DEPOSITED MASS
!| S3D_NCOUCH     |-->| NUMBER OF LAYERS WITHIN THE BED
!|                |   | (MULTILAYER S3D_GIBSONSETTLING MODEL)
!| TRA02          |<->| WORK ARRAY (RESULT)
!| VOLU2D         |-->| INTEGRAL OF TEST FUNCTIONS IN 2D
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_SED3D => SED3D
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: S3D_NCOUCH,NPOIN2
!
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_MASBED,S3D_MASDEP
      DOUBLE PRECISION, INTENT(IN) :: VOLU2D(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: S3D_EPAI(NPOIN2,S3D_NCOUCH)
      DOUBLE PRECISION, INTENT(IN) :: S3D_EPAICO(*), S3D_EPAINCO(*)
      DOUBLE PRECISION, INTENT(IN) :: S3D_CONC(NPOIN2,S3D_NCOUCH)
!
      DOUBLE PRECISION, INTENT(INOUT) :: TRA02(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN)    :: AT,S3D_CFDEP
      DOUBLE PRECISION, INTENT(IN)    :: S3D_MASBED0
!
      LOGICAL, INTENT(IN)             :: S3D_MIXTE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION ERROR
!
!=======================================================================
!
! COMPUTES THE MASS OF ERODED SEDIMENTS (MASSE3)
! DURING THE TIMESTEP
!
!=======================================================================
!
!      FLUX=0.D0
!
!      DO I=1,NPOIN2
!        FLUX=FLUX+S3D_FLUER(I)*VOLU2D(I)
!      ENDDO
!
!      MASSE3=FLUX*DT
!      IF(NCSIZE.GT.1) MASSE3=P_SUM(MASSE3)
!
!=======================================================================
!
! COMPUTES THE MASS OF DEPOSITED SEDIMENTS (MASSE4) DURING THE TIMESTEP
!
!=======================================================================
!
!      FLUX=0.D0
!      S3D_FLUDPcalculated in FONVAS (or MURD3D_POS) S3D_FLUDP>0
!      DO I=1,NPOIN2
!        FLUX=FLUX+S3D_FLUDP(I)*VOLU2D(I)
!      ENDDO
!
!      MASSE4=FLUX*DT
!      IF(NCSIZE.GT.1) MASSE4=P_SUM(MASSE4)
!
!=======================================================================
!
! COMPUTES THE MASS OF SEDIMENT EXCHANGED (MASSE5)
! BETWEEN THE MUDDY BED AND THE FLUID DURING THE TIMESTEP
!
!=======================================================================
!
!      MASSE5=MASSE4-MASSE3
!
!=======================================================================
!
! CUMULATED MASS OF SEDIMENT DEPOSITED (S3D_MASDEP=0 t=0)
!
!=======================================================================
!      S3D_MASDEPcalculated in  fonvas
!      S3D_MASDEP= S3D_MASDEP+ MASSE5
!=======================================================================
!
! COMPUTES THE MASS OF SEDIMENT BED
!
!=======================================================================
!
      CALL MASSED(S3D_MASBED,S3D_EPAI,S3D_CONC,TRA02,NPOIN2,
     &            S3D_NCOUCH,
     &            VOLU2D,S3D_CFDEP,S3D_EPAICO,S3D_EPAINCO,S3D_MIXTE)
!
!=======================================================================
! PRINTOUT
!=======================================================================
!
      ERROR = S3D_MASBED-(S3D_MASBED0+ S3D_MASDEP)
!
      WRITE(LU,*) 'SEDIMENT BED MASS BALANCE AT TIME=',AT
      WRITE(LU,*) 'MASS OF BED                        : ',S3D_MASBED
      IF(S3D_MASDEP.GT.0) THEN
        WRITE(LU,*) 'TOTAL DEPOSITED MASS               :', S3D_MASDEP
      ELSE
        WRITE(LU,*) 'TOTAL ERODED MASS                : ',-S3D_MASDEP
      ENDIF
      WRITE(LU,*) 'SEDIMENT BED MASS BALANCE  (GAIN>0 LOSS<0):', ERROR
      WRITE(LU,*) 'SEDIMENT BED MASS BALANCE  (GAIN>0 LOSS<0):', ERROR
!
!-----------------------------------------------------------------------
!
      RETURN
      END
