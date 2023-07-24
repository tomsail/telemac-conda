!                   *****************
                    SUBROUTINE MASSED
!                   *****************
!
     &(S3D_MASBED,S3D_EPAI,S3D_CONC,TRA02,NPOIN2,
     & S3D_NCOUCH,VOLU2D,
     & S3D_CFDEP,S3D_EPAICO,S3D_EPAINCO,S3D_MIXTE)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    PERFORMS INITIAL RELATIVE MASS BALANCE FOR
!+                THE SEDIMENT.
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
!+        17/03/2011
!+        V6P1
!+   Rewritten (formula changed, parallelism,...)
!
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments in sediment merged on 25/02/2014.
!
!history  G. ANTOINE & M. JODEAU & J.M. HERVOUET (EDF - LNHE)
!+        13/10/2014
!+        V7P0
!+   New developments in sediment for mixed sediment transport
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| S3D_CFDEP      |-->| CONCENTRATION OF MUD DEPOSIT (G/L)
!| S3D_CONC       |-->| CONCENTRATION OF MUD BED LAYER
!|                |   | (MULTILAYER MODEL)
!| S3D_EPAI       |-->| THICKNESS OF SOLID BED LAYER
!|                |   | (S3D_EPAI=DZ/(1+S3D_IVIDE), DZ TOTAL BED THICKNESS)
!| S3D_EPAICO     |-->| THICKNESS OF COHESIVE SUB-LAYER
!| S3D_EPAINCO    |-->| THICKNESS OF NON-COHESIVE SUB-LAYER
!| S3D_MASBED     |<->| MASS OF SEDIMENT BED
!| S3D_MIXTE      |-->| LOGICAL, MIXED SEDIMENTS OR NOT
!| S3D_NCOUCH     |-->| NUMBER OF LAYERS WITHIN THE BED
!|                |   | (S3D_GIBSONMODEL)
!| TRA02          |<->| WORK ARRAY
!| VOLU2D         |-->| INTEGRAL OF TEST FUNCTIONS IN 2D
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_MASSED => MASSED
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_SUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN2,S3D_NCOUCH
      DOUBLE PRECISION, INTENT(IN)    :: S3D_CFDEP
      DOUBLE PRECISION, INTENT(INOUT) :: S3D_MASBED
      DOUBLE PRECISION, INTENT(IN)    :: S3D_EPAI(NPOIN2,S3D_NCOUCH)
      DOUBLE PRECISION, INTENT(IN)    :: S3D_EPAICO(*), S3D_EPAINCO(*)
      DOUBLE PRECISION, INTENT(IN)    :: VOLU2D(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: S3D_CONC(NPOIN2,S3D_NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA02(NPOIN2)
      LOGICAL, INTENT(IN)             :: S3D_MIXTE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN,IPF
      DOUBLE PRECISION MASSE6
!
!=======================================================================
!
! MASS OF MUDDY DEPOSITS ON THE RIGID BED (MASSE6)
!
!=======================================================================
!
      IF(S3D_MIXTE) THEN

        DO IPOIN=1,NPOIN2
          TRA02(IPOIN)=0.D0
          TRA02(IPOIN)=TRA02(IPOIN)+S3D_CONC(IPOIN,1)*S3D_EPAICO(IPOIN)
     &      + S3D_CFDEP*S3D_EPAINCO(IPOIN)
        ENDDO

      ELSE

        DO IPOIN=1,NPOIN2
          TRA02(IPOIN)=0.D0
          DO IPF=1,S3D_NCOUCH
            TRA02(IPOIN)=TRA02(IPOIN)+
     &                   S3D_CONC(IPOIN,IPF)*S3D_EPAI(IPOIN,IPF)
          ENDDO
        ENDDO
!
      ENDIF

      MASSE6=0.D0
      DO IPOIN=1,NPOIN2
        MASSE6=MASSE6+VOLU2D(IPOIN)*TRA02(IPOIN)
      ENDDO
      IF(NCSIZE.GT.1) MASSE6=P_SUM(MASSE6)
      S3D_MASBED= MASSE6
!
!-----------------------------------------------------------------------
!
      RETURN
      END

