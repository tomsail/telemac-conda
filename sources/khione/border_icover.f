!                     ************************
                      SUBROUTINE BORDER_ICOVER
!                     ************************
!
     &( U,V, MESH )
!
!***********************************************************************
! KHIONE   V8P0
!***********************************************************************
!
!brief    COMPUTES PRESENCE OF STATIC BORDER ICE
!
!+  Use of ICETYPE:
!+    1 - open water
!+    2 - static border ice (within or at the edge of the cover)
!+    3 - border ice (both static and dynamic, and including the edge)
!+  Use of ICELOC:
!+    1 - open water
!+    2 - domain boundary node
!+    3 - edge of the border ice where dynamic border ice accumulates
!+  Note that dynamic border ice (where ice is being accumulated),
!+  TODO: This method still need to be updated to account for the
!+    melting down of border ice (in patches if possible)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| U              |-->| X-COMPONENT OF THE VELOCITY
!| V              |-->| Y-COMPONENT OF THE VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_KHIONE, ONLY:BD_ICE,ANFEM,THIFEMS,
     &                              ANFEM0,IT1,IT2,T1,
     &                              ICETYPE,VCRBOR,VCRBOM,TCR,VZ,
     &                              TC,TMELT,ICELOC,ICETYPEP,T2
      USE INTERFACE_KHIONE, EX_BORDER_ICOVER => BORDER_ICOVER
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: U,V
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          I,J,K,N1,N2,IT,NPOIN,NELEM,NELMAX,IL,ITP
      DOUBLE PRECISION USTAR,VMAG,VB
      DOUBLE PRECISION THICK
!
!-----------------------------------------------------------------------
!
      NPOIN = MESH%NPOIN
      NELEM = MESH%NELEM
      NELMAX = MESH%NELMAX
!
!
!=======================================================================
!
!     7 - STATIC BORDER ICE GROWTH
!
!-----------------------------------------------------------------------
!
      IF( BD_ICE ) THEN
!
!-----------------------------------------------------------------------
!
!     MAYBE THIS SHOULD BE DONE ONCE /!\
!     TODO: T1 CAN BE REPLACED BY VOLUPAR (ALREADY COMPUTED)
!
        CALL VECTOR(T1,'=','MASBAS          ',U%ELM,1.D0,
     &              T2,T2,T2,T2,T2,T2,MESH,.FALSE.,T2)
!       ASSEMBLING THE SUM OF ALL VALUES
        IF( NCSIZE.GT.1 ) CALL PARCOM( T1,2,MESH )
!
!-----------------------------------------------------------------------
!
!     LOOKING FOR NEW BORDER ICE NODES (TO BE SWITCHED))
!
        DO I = 1,NPOIN
          IT = ICETYPE%I(I)
          IL = ICELOC%I(I)
          ITP = ICETYPEP%I(I)
!
!         CURRENT VELOCITY
          VMAG = SQRT( U%R(I)**2 + V%R(I)**2 )
!         BUOYANT VELOCITY
          VB = MAX( 0.D0, -0.025D0*TCR%R(I) + 0.005D0 )
!         BORDER ICE THICKNESS /!\ TODO: DOUBLE CHECK VALIDITY
          THICK = THIFEMS%R(I)
!
!         VELOCITY CRITERIA FOR DYNAMIC BORDER ICE GROWTH
          USTAR = MAX( 0.175D0, VMAG / MAX(VCRBOM,1.D-12) )
!
!         IT1 IS USED AS A STATIC ICE MASK DEFINING NODES FOR WHICH
!         THE THRESHOLDS ARE TRUE (I.E. SUBJECT TO BE BODER ICE NODE).
!         - IT1 = 0 IF THRESHOLDS ARE --NOT-- TRUE
!         - IT1 = 1 IF THRESHOLDS ARE TRUE
!         - IT1 = 2 IF THRESHOLD ARE TRUE AND IS ALSO ON THE DOMAIN
!         BORDER IT IS NOTED THAT IF IT1 = 2, THEN THE NODE IS
!         --NOT-- BORDER ICE ALREADY, BUT ABOUT TO BE SWITCHED.
          IT1%I(I) = 0
!
!       > THRESHOLDS FOR STATIC BORDER ICE FORMATION
          IF( TCR%R(I).LE.(TC-TMELT%R(I)) .AND. ! THERMAL PROPERTY
     &        VB.GT.1.1D0*VZ%R(I) .AND.         ! BUOYANT VS. TURBULENCE
     &        VMAG.LT.VCRBOR ) THEN             ! CRITICAL VELOCITY
            IF( IT .NE. 2 ) THEN
!           NODE OUTSIDE STATIC BORDER ICE COVER (NOT MULTIPLIER OF 3)
              IT1%I(I) = 1
!             BORDER NODE ABOUT TO BE SWITCH TO STATIC BORDER ICE
              IF( IL.EQ.2.OR.IL.EQ.4 ) IT1%I(I) = 2
            ENDIF
          ENDIF
!
!         IT2 IS USED AS A DYNAMIC ICE MASK DEFINING NODES FOR WHICH
!         THE THRESHOLDS ARE TRUE (I.E. SUBJECT TO BE BODER ICE NODE).
!         - IT2 = 0 IF THRESHOLDS ARE --NOT-- TRUE
!         - IT2 = 1 IF THRESHOLDS ARE TRUE
!         - IT2 = 2 IF THRESHOLD ARE TRUE AND IS ALSO ON THE DOMAIN
!         BORDER IT IS NOTED THAT IF IT2 = 2, THEN THE NODE IS
!         --NOT-- BORDER ICE ALREADY, BUT ABOUT TO BE SWITCHED.
          IT2%I(I) = 0
!
!       > THRESHOLDS FOR DYNAMIC BORDER ICE FORMATION
          IF( !THICK.GE.0.0D0 .AND.              ! CRITICAL THICKNESS
     &        USTAR.LE.1.D0 ) THEN              ! CRITICAL VELOCITY
            IF( IL .LT. 3.AND.ITP.NE.2  ) THEN
              IT2%I(I) = 1
!             BORDER NODE ABOUT TO BE SWITCH TO STATIC BORDER ICE
              IF( IL.EQ.2 ) IT2%I(I) = 2
!
!            ELSE !IF( IL.GE.3 ) THEN
!        > FURTHER GROWING DYNAMIC ICE
!              R = 14.1 * USTAR ** (-0.93) * ANFEM%R(I) ** 1.08
!              DPHI = LIN_WATAIR*( TWAT%R(I) - TAIR%R(I) )
!              DW = DPHI * R / ( ROEAU * LH_ICE )
!              DWB%R(I) = MIN( 1.D0, DWB%R(I) + DW / SQRT( T1%R(I) ) )
!
            ENDIF
          ENDIF
!
        ENDDO
!
!-----------------------------------------------------------------------
!
!     CHECK BORDER NODES ABOUT TO BE SWITCHED INTO BORDER ICE NODES
!
        DO J = 1,NELEM
!
          DO K = 1,3
! RA: why we compute again node indices ? They are stored in IKLE (NELEM,3)
!      1- introduce IKLE as an argument like IKLE(NELEM,3)
!      2- nodes of element IELEM are :
!              I1 = IKLE(IELEM,1)
!              I2 = IKLE(IELEM,2)
!              I3 = IKLE(IELEM,3)
!
            I = MESH%IKLE%I( J + (K-1)*NELMAX )
!
            IT = ICETYPE%I(I)
            ITP = ICETYPEP%I(I)
            IL = ICELOC%I(I)
!           NODES ABOUT TO BE SWITCHED NEED TO BE BELOW THRESHOLDS AND
!           --NOT-- ALREADY BODER ICE NODES. THEY ALSO NEED TO BE CLOSE
!           TO TWO OTHER (SUFFICIENTLY THICK) BORDER ICE NODES.
            IF( IT1%I(I).EQ.1 ) THEN
              N1 = ICETYPE%I( MESH%IKLE%I( J + MOD(K,3)*NELMAX ) )
              N2 = ICETYPE%I( MESH%IKLE%I( J + MOD(K+1,3)*NELMAX ) )
              IF( N1 .EQ. 2 .AND. N2 .EQ. 2 ) THEN
                IT1%I(I) = 2
              ENDIF
            ENDIF
            IF( IT2%I(I).EQ.1 ) THEN
              N1 = ICETYPEP%I( MESH%IKLE%I( J + MOD(K,3)*NELMAX ) )
              N2 = ICETYPEP%I( MESH%IKLE%I( J + MOD(K+1,3)*NELMAX ) )
              IF( N1 .EQ. 2 .AND. N2 .EQ. 2 ) THEN
                IT2%I(I) = 2
              ENDIF
            ENDIF
          ENDDO
!
        ENDDO
!
!       ASSEMBLING THE MAX OF ALL VALUES - THE TWOS WILL BE SWITCHED
!       CONVERSION BETWEEN INTEGER AND DOUBLE IS NECESSARY FOR PARCOM
        IF(NCSIZE.GT.1)CALL PARCOM2I(IT1%I,IT2%I,IT2%I,NPOIN,1,3,2,MESH)
!
!-----------------------------------------------------------------------
!
!       SWITCHES BORDER NODES INTO STATIC BORDER ICE NODES
!
        DO I = 1,NPOIN
          IT = ICETYPE%I(I)
          ITP = ICETYPEP%I(I)
          IL = ICELOC%I(I)
!
!       > STATIC BORDER ICE
          IF( IT1%I(I).EQ.2 ) THEN
            ICETYPE%I(I) = 2    !=> INSTANT SWITCH
            THIFEMS%R(I) = 0.D0
            ANFEM%R(I) = ANFEM0
!
!           SWITCH FROM SOLID DYNAMIC TO STATIC BORDER ICE
            IF( ITP .EQ. 2 ) THEN
!
!           SWITCH FROM GROWING DYNAMIC TO STATIC BORDER ICE
            ELSEIF( IL .GE. 3 ) THEN
!           USE DWB TO COMPUTE THIFEMS AND ANFEM ?
              IF(IL.EQ.3) ICELOC%I(I) = 1         !=> INSTANT SWITCH
              IF(IL.EQ.4) ICELOC%I(I) = 2         !=> INSTANT SWITCH
              ICETYPEP%I(I) = 2
!
!           NEW STATIC BORDER ICE
            ELSE
!             NOT PART OF THE DYNAMIC ICE COVER
              ICETYPEP%I(I) = 2     !=> INSTANT SWITCH
              THIFEMS%R(I) = 0.D0
              ANFEM%R(I) = ANFEM0
!
            ENDIF
!
!       > DYNAMIC BORDER ICE
          ELSEIF( IT2%I(I).EQ.2 ) THEN
            IF( IL .LT. 3 ) THEN
              THIFEMS%R(I) = 0.D0
              ANFEM%R(I) = ANFEM0
              IF(IL.EQ.1) ICELOC%I(I) = 3         !=> INSTANT SWITCH
              IF(IL.EQ.2) ICELOC%I(I) = 4         !=> INSTANT SWITCH
            ENDIF
          ENDIF
!
        ENDDO
!
      ENDIF
!
!
!=======================================================================
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE BORDER_ICOVER
