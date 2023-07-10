!                   ****************************
                    SUBROUTINE PREPARE_ADVECTION
!                   ****************************
!
     & (FN,S0F,FBORL,LIFBOL,FLUXF,
     &  SCHCF,CALFLU,MESH3D,MASKEL,NPTFR3,VOLUNPAR,FLUEXT,FLUEXTPAR,
     &  NBOR3,DT,MSK,IELM3,NUMLIQ,DIRFLU,NFRLIQ)
!
!***********************************************************************
! TELEMAC3D   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    PREPARES ADVECTION FOR ADVECTED VARIABLES
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/1999
!+
!+   FORTRAN95 VERSION
!
!history  J.M. HERVOUET (LNHE)
!+        18/12/2009
!+        V6P0
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CALFLU         |-->| INDICATE IF FLUX IS CALCULATED FOR BALANCE
!| DIRFLU         |-->| TYPE OF BOUNDARY CONDITION FOR FLUXES
!| DT             |-->| TIME STEP
!| FBORL          |<->| DIRICHLET CONDITIONS
!| FLUEXT         |-->| OUTPUT FLUX BY NODE
!| FLUEXTPAR      |-->| OUTPUT FLUX BY NODE IN PARALLEL
!| FLUXF          |<->| GLOBAL FLUX GLOBAL TO UPDATE
!| FN             |<->| VARIABLE F AT TIME N
!| IELM3          |-->| TYPE OF 3D DISCRETISATION
!| LIFBOL         |<->| PHYSICAL BOUNDARY CONDITIONS TYPE
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH3D         |<->| 3D MESH
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NBOR3          |-->| GLOBAL NUMBER OF 3D BOUNDARY POINTS
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NPTFR3         |-->| NUMBER OF LATERAL BOUNDARY POINTS IN 3D
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| S0F            |<->| EXPLICIT SOURCE TERM (DIM=F/T)
!| SCHCF          |-->| ADVECTION SCHEME FOR F
!| VOLUNPAR       |-->| VOLUME AROUND POINTS AT TIME N IN PARALLEL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE INTERFACE_TELEMAC3D, EX_PREPARE_ADVECTION => PREPARE_ADVECTION
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FN,S0F,LIFBOL,FBORL
      TYPE(BIEF_OBJ), INTENT(IN)      :: FLUEXT,FLUEXTPAR
      DOUBLE PRECISION, INTENT(IN)    :: DT
      DOUBLE PRECISION, INTENT(INOUT) :: FLUXF
      INTEGER, INTENT(IN)             :: SCHCF,NPTFR3,NFRLIQ,IELM3
      INTEGER, INTENT(IN)             :: NUMLIQ(*),DIRFLU(0:NFRLIQ)
      LOGICAL, INTENT(IN)             :: CALFLU,MSK
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKEL,NBOR3,VOLUNPAR
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH3D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IP,K,IPTFR,IS,I
!
      DOUBLE PRECISION LAMBDA
!
      LOGICAL YADIRFLU,VELOCITY
!
!***********************************************************************
!
      VELOCITY=.FALSE.
      IF(FN%NAME(1:1).EQ.'U'.OR.
     &   FN%NAME(1:1).EQ.'V'.OR.
     &   FN%NAME(1:1).EQ.'W') VELOCITY=.TRUE.
!
!     EVEN IF.NOT.CALFLU
!
      FLUXF = 0.D0
!
!     WITH DISTRIBUTIVE SCHEMES : COMPUTES PRESCRIBED VALUES THAT
!     WILL ENSURE THE CORRECT FLUX (REAL PRESCRIBED VALUES DISCARDED)
!     THESE CORRECTED PRESCRIBED VALUES ARE SET BEFORE ADVECTION
!
!     YADIRFLU=.TRUE. : THERE IS AT LEAST ONE BOUNDARY WITH
!                       TREATMENT OF FLUXES AT BOUNDARIES = 2
      YADIRFLU=.FALSE.
!     DIRFLU DISCARDED FOR VELOCITIES
      IF(NFRLIQ.GT.0.AND..NOT.VELOCITY) THEN
        DO K=1,NFRLIQ
          IF(DIRFLU(K).EQ.2) YADIRFLU=.TRUE.
        ENDDO
      ENDIF
!
!=======================================================================
!
!     FOR TRACERS (=NOT VELOCITY) : DIRICHLET VALUES ARE NOT RESPECTED IF EXIT
!     THERE IS NO NEED TO TEST KENTU OR KADH FOR TRACERS
!
      IF(NPTFR3.GT.0.AND.NFRLIQ.GT.0.AND..NOT.VELOCITY) THEN
        DO IPTFR=1,NPTFR3
          IF(LIFBOL%I(IPTFR).EQ.KENT) THEN
!           EXITS ARE TREATED AS FREE BOUNDARIES
            IP=NBOR3%I(IPTFR)
            IF(FLUEXTPAR%R(IP).GE.0.D0) LIFBOL%I(IPTFR)=KSORT
          ENDIF
        ENDDO
      ENDIF
!
!=======================================================================
!
!     VELOCITIES ARE ADVECTED FOR THE NEXT TIME STEP
!     HENCE NO CHANGE OF U, V AND W AND BOUNDARY CONDITIONS
!
      IF(.NOT.VELOCITY) THEN
!
!     A PRIORI CORRECTION OF FN FOR REAL ENTRANCES
!     I.E. LIFBOL STILL KENT DESPITE ABOVE CHANGE
!
!     IF((SCHCF.EQ.ADV_SUP   .OR.SCHCF.EQ.ADV_NSC    .OR.
!    &    SCHCF.EQ.ADV_PSI   .OR.SCHCF.EQ.ADV_LPO    .OR.
!    &    SCHCF.EQ.ADV_LPO_TF.OR.SCHCF.EQ.ADV_NSC_TF)
!    &                                              .AND.YADIRFLU) THEN
!
      IF(YADIRFLU) THEN
!
        IF(NPTFR3.GT.0) THEN
!
        DO IP=1,NPTFR3
          IF(DIRFLU(NUMLIQ(IP)).EQ.2.AND.LIFBOL%I(IP).EQ.KENT) THEN
            I=NBOR3%I(IP)
            LAMBDA=-FLUEXTPAR%R(I)*DT/
     &      (MAX(VOLUNPAR%R(I),1.D-10)-FLUEXTPAR%R(I)*DT)
            FN%R(I)=FN%R(I)+LAMBDA*(FBORL%R(IP)-FN%R(I))
!           CORRECTION OF FLUX
!           IN THE PROOF OF MASS-CONSERVATION, FLUEXT IS MULTIPLIED
!           BY FN INSTEAD OF FBOR, TO INTERPRET THE ADDED MASS AS
!           A FLUX THIS CORRECTION IS NECESSARY
!           HERE IT IS THE FN MODIFIED ABOVE
!           EVEN IF NOT CALFLU (CHEAPER)
            FLUXF=FLUXF+(FBORL%R(IP)-FN%R(I))*FLUEXT%R(I)*DT
!           AVOIDS A DIRICHLET TREATMENT HEREAFTER AND BY DIFF3D -
!           WILL BE RESTORED AFTER DIFF3D
            LIFBOL%I(IP)=KSORT
          ENDIF
        ENDDO
!
        ENDIF
!
      ENDIF
!
!=======================================================================
!
!     PUTS DIRICHLET VALUES IN FN
!     MAY HAVE NO EFFECT IF TREATMENT OF FLUXES AT THE BOUNDARIES=2
!     BECAUSE LIFBOL CHANGED ABOVE
!
      IF(NPTFR3.GT.0) THEN
        DO IPTFR=1,NPTFR3
          IF(LIFBOL%I(IPTFR).EQ.KENT .OR.
     &       LIFBOL%I(IPTFR).EQ.KENTU.OR.
     &       LIFBOL%I(IPTFR).EQ.KADH) THEN
            FN%R(NBOR3%I(IPTFR)) = FBORL%R(IPTFR)
          ENDIF
        ENDDO
      ENDIF
!
!     HERE BOTTOM AND FREE SURFACE SHOULD BE TREATED AS WELL
!
!
!     END OF IF(.NOT.VELOCITY)
      ENDIF
!
!=======================================================================
!
!     3D ADVECTION (OTHER THAN SUPG)
!
!=======================================================================
!
!     WITH DISTRIBUTIVE SCHEMES, RIGHT-HAND SIDE MUST BE
!     IN INTEGRATED FORM (BEWARE, ORIGINAL S0F THUS MODIFIED)
!
      IF(SCHCF.EQ.ADV_NSC.OR.SCHCF.EQ.ADV_PSI.OR.SCHCF.EQ.ADV_LPO.OR.
     &   SCHCF.EQ.ADV_NSC_TF.OR.SCHCF.EQ.ADV_LPO_TF) THEN
!
        IF(S0F%TYPR.NE.'0') THEN
!
          CALL VECTOR(S0F,'=','MASVEC          ',IELM3,1.D0,
     &                S0F,S0F,S0F,S0F,S0F,S0F,MESH3D,MSK,MASKEL)
          IF(NCSIZE.GT.1) CALL PARCOM(S0F,2,MESH3D)
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
