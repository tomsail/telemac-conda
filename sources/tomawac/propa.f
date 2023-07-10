!                   ****************
                    SUBROUTINE PROPA
!                   ****************
!
     &(F,     B,  ELT,    ETA,   FRE,   NPOIN3, NPOIN2,
     & NDIRE, NF, COURAN, TRA01)
!
!***********************************************************************
! TOMAWAC   V7P0
!***********************************************************************
!
!brief    ADVECTION STEP.
!+                INTERPOLATES AT THE FOOT OF THE CHARACTERISTICS.
!
!history  F. MARCOS (LNH)
!+        05/12/95
!+        V1P0
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        25/11/2014
!+        V7P0
!+   Bug corrected: size of array WSHZ for POST_INTERP was not correct
!+   in the call, as TRA01(1,2), TRA01(1,4) was needed.
!+   Moreover the real size of TRA01 is (NPOIN3,6), not (NPOIN3,8), see
!+   point_tomawac.f. Intent completed.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| B              |-->| JACOBIAN TO TRANSFORM N(KX,KY) INTO F(FR,TETA)
!| COURAN         |-->| LOGICAL INDICATING IF THERE IS A CURRENT
!| ELT            |-->| NUMBERS OF THE ELEMENTS 2D OF THE
!|                |   | POINTS TO BE ADVECTED
!| ETA            |-->| NUMBERS OF THE LAYERS OF THE
!|                |   | POINTS TO BE ADVECTED
!| F              |<->| WAVE ACTION DENSITY OR VARIANCE DENSITY
!|                |   | DIRECTIONAL SPECTRUM
!| FRE            |-->| NUMBER OF THE FREQUENCIES OF THE
!|                |   | POINTS TO BE ADVECTED
!| IKLE_EXT       |-->| TRANSITION BETWEEN LOCAL AND GLOBAL NUMBERING
!|                |   | OF THE 2D MESH (IN AN EXTENDED FORM)
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPOIN3         |-->| NPOIN2*NDIRE
!| SHF            |-->| BARYCENTRIC COORDINATES ALONG F OF THE
!|                |   | NODES IN THEIR ASSOCIATED FREQUENCIES "FRE"
!| SHP            |-->| BARYCENTRIC COORDINATES OF THE NODES IN
!|                |   | THEIR ASSOCIATED 2D ELEMENT "ELT"
!| SHZ            |-->| BARYCENTRIC COORDINATES ALONG TETA OF THE
!|                |   | NODES IN THEIR ASSOCIATED LAYER "ETA"
!| TRA01          |<->| WORK TABLE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TOMAWAC, EX_PROPA => PROPA
      USE STREAMLINE, ONLY : POST_INTERP
      USE DECLARATIONS_TELEMAC, ONLY : NAMECODE
      USE DECLARATIONS_TOMAWAC, ONLY : SSHP1, SSHZ, SSHF, T3_01, T3_02,
     &     STSTOT, ITR01, IKLE_EXT, ISUB, MESH3D
!     STSTOT IS HERE A WORK TABLE
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN3,NPOIN2,NDIRE,NF
!
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: B(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPOIN3,6)
      INTEGER, INTENT(INOUT) :: ELT(NPOIN3,NF),ETA(NPOIN3,NF)
      INTEGER, INTENT(INOUT) :: FRE(*)
      LOGICAL, INTENT(IN)    :: COURAN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IFF,I,I3,IDIRE
      INTEGER :: SIZ_ISUB, SIZ_FRE, JF_ISUB, JF_FRE
      INTEGER, ALLOCATABLE :: TMP_ISUB(:)
!
!----------------------------------------------------------------------
!
!     WITH CURRENT ALL FUNCTION MUST BE BUILT BEFORE INTERPOLATION
!     AS ALL FREQUENCIES WILL BE USED TOGETHER
!
      IF(COURAN) THEN
        DO IFF=1,NF
          DO IDIRE=1,NDIRE
            DO I=1,NPOIN2
              I3=I+(IDIRE-1)*NPOIN2+(IFF-1)*NPOIN3
              STSTOT%R(I3)=F(I,IDIRE,IFF)*B(I,IFF)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
!
!     NOW FREQUENCY PER FREQUENCY
!
      DO IFF=1,NF
!
!       COPY OF F*B INTO T3_01=TRA02 ?
!
        IF(.NOT.COURAN) THEN
          DO IDIRE=1,NDIRE
            DO I=1,NPOIN2
              I3=I+(IDIRE-1)*NPOIN2
              STSTOT%R(I3)=F(I,IDIRE,IFF)*B(I,IFF)
            ENDDO
          ENDDO
        ENDIF
!
        IF(NCSIZE.GT.1) THEN
          SIZ_ISUB = NPOIN3
          JF_ISUB = IFF
        ELSE
          SIZ_ISUB = 1
          JF_ISUB = 1
        ENDIF
        IF(COURAN.OR.NAMECODE(1:7).EQ.'TELEMAC') THEN
          SIZ_FRE = NPOIN3
          JF_FRE = IFF
        ELSE
          SIZ_FRE = 1
          JF_FRE = 1
        ENDIF
        ! Memory optimisation (intel debug)
        ALLOCATE(TMP_ISUB(SIZ_ISUB))
        TMP_ISUB = ISUB((JF_ISUB-1)*SIZ_ISUB+1:JF_ISUB*SIZ_ISUB)
        CALL POST_INTERP(STSTOT,T3_02,SSHP1%ADR(IFF)%P%R,
     &                   SSHZ%ADR(IFF)%P%R,SSHF%ADR(IFF)%P%R,
     &                   IKLE_EXT%I,IKLE_EXT%DIM1,1,
     &                   NPOIN2,ELT(1,IFF),ETA(1,IFF),
     &                   FRE((JF_FRE-1)*SIZ_FRE+1:JF_FRE*SIZ_FRE),
     &                   TMP_ISUB,
     &                   3,NDIRE,41,NPOIN3,
     &                   NPOIN2,TRA01,TRA01(1,4),
     &                   T3_01%R,ITR01(1:NPOIN3),
     &                   ITR01(NPOIN3+1:2*NPOIN3),
     &                   ITR01(2*NPOIN3+1:3*NPOIN3),
     &                   NPOIN3,
     &                   .TRUE.,
!                      PERIODICITY
     &                   COURAN)
!                        4D
        DEALLOCATE(TMP_ISUB)
!
        IF(NCSIZE.GT.1) CALL PARCOM(T3_02,1,MESH3D)
!
!       FINAL COMPUTATION OF F
!
        DO IDIRE=1,NDIRE
          DO I=1,NPOIN2
            I3=I+(IDIRE-1)*NPOIN2
!           MAX(..,0.D0) SEEMS NECESSARY, WHERE F BECOMES < 0 ??
            F(I,IDIRE,IFF)=MAX(T3_02%R(I3)/B(I,IFF),0.D0)
          ENDDO
        ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

