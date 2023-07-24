!                   ****************
                    SUBROUTINE PHBOR
!                   ****************
!
!
!***********************************************************************
! ARTEMIS   V7P2                                     Nov 2016
!***********************************************************************
!
!brief    TRANSLATES THE BOUNDARY CONDITIONS SPECIFIED
!+                BY THE USER,
!+                I.E. COMPUTES THE COEFFICIENTS
!+                APHIR, APHII, ... FOR EACH BOUNDARY SEGMENT.
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        21/08/2000
!+        V5P1
!+
!
!history  C. DENIS (SINETICS)
!+        18/03/2010
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
!history  C.PEYRARD (LNHE)
!+        01/06/2012
!+        V6P0
!+   KSORT   : end of application to neighbours
!+   KINC    : end of application to the I+1 node
!+   SEGMENT : If a segment links a solid node to a liquid node,
!+             this segment is regarded as solid.
!+   BUG correction         : Full KINC boundaries taken into account
!+   NEW boundary condition : incident potential can be given by the user
!+   Parallel correction    : - End of HBT,CGT,CTT,KT,XT,YT tables
!+                              only   HB%R,CG%R, etc... are used.
!+                            - No use of NCSIZE variable.
!
!history  C.PEYRARD (LNHE)
!+        18/03/2014
!+        V7P0
!+   KSORT   : application to neighbours (best for automatic angles)
!
!history  N.DURAND (HRW)
!+        November 2016
!+        V7P2
!+   Phase calculation brought back inside PHBOR
!+   (automatic in the sense of applicable to general case, different
!+    from that using KPHREF)
!
!history  S.BOURBAN (HRW)
!+        23/01/2017
!+        V7P3
!+   Implementing a cumulative sorting of each boundary allowing for
!+   varying bathymetry along each boundary segment.
!
!history  N.DURAND (HRW)
!+        August 2017
!+        V7P3
!+   PI now defined in DECLARATIONS_ARTEMIS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER I,I0,IZ1,IZ2,IG,N
!
!
      DOUBLE PRECISION GRE, GIM
      DOUBLE PRECISION DDXGRE,DDYGRE,DDXGIM,DDYGIM
!

      DOUBLE PRECISION PHASEOI,X0,Y0
      DOUBLE PRECISION AUXI1,AUXIC,AUXIS
      DOUBLE PRECISION SDIST,MINDIST
!     DOUBLE PRECSION AUXI2
      INTRINSIC COS,SIN
!
      DOUBLE PRECISION, PARAMETER :: CHOUIA = 1.D-9
!
!-----------------------------------------------------------------------
!
      PI = 4.D0 * ATAN( 1.D0 )
      DEGRAD = PI / 180.D0
      RADDEG = 180.D0 / PI
!
!-----------------------------------------------------------------------
!
! INITIALISES LIDIR TO KSORT (A DIFFERENT VALUE FROM KENT)
! IN ORDER NOT TO TAKE NODES IMPOSED IN PRIDIH INTO ACCOUNT,
! WHEN IT HAS NOT BEEN REQUESTED.
!
      DO I=1,NPTFR
        LIDIR%I(I) = KSORT
!       BEWARE: IT IS ASSUMED HERE THAT NPTFRX=NPTFR
        LIDIR%I(I+NPTFR) = KSORT
        IF (LIHBOR%I(I).EQ.KENT) THEN
          LIHBOR%I(I) = KINC
        ENDIF
        APHI1B%R(I) = 0.D0
        BPHI1B%R(I) = 0.D0
        CPHI1B%R(I) = 0.D0
        DPHI1B%R(I) = 0.D0
        APHI2B%R(I) = 0.D0
        BPHI2B%R(I) = 0.D0
        CPHI2B%R(I) = 0.D0
        DPHI2B%R(I) = 0.D0
        APHI3B%R(I) = 0.D0
        BPHI3B%R(I) = 0.D0
        CPHI3B%R(I) = 0.D0
        DPHI3B%R(I) = 0.D0
        APHI4B%R(I) = 0.D0
        BPHI4B%R(I) = 0.D0
        CPHI4B%R(I) = 0.D0
        DPHI4B%R(I) = 0.D0
        CGRX1B%R(I) = 0.D0
        CGRY1B%R(I) = 0.D0
        DGRX1B%R(I) = 0.D0
        DGRY1B%R(I) = 0.D0
      ENDDO
!
!       -------------------------------------------------
!             COEFFICIENTS FOR A INCIDENT WAVE
!       -------------------------------------------------
!
!-----------------------------------------------------------------------
!
!     PROCESSING EACH BOUNDARY SEGMENT INDEPEDENTLY,
!     STARTING WITH THE FIRST NODE HIT BY THE INCOMING WAVE
!
      DO N = 1,NFRLIQ
!
!     ***********************************************************
!     LOOKING FOR AN APPROPRIATE STARTING POINT: I0 (AND END POINTS IZS)
!
        I0 = 0
        DO I=1,NPTFR
          IF( NUMLIQ%I(I).EQ.N ) THEN
!     SIGNED DISTANCE BETWEEN THE CREST OF THE WAVE AND THE BOUNDARY
!     ( TETAB%R(I) DEFINED AS WAVE PROPAGATING TOWARDS )
            IG   = MESH%NBOR%I(I)
            SDIST = COS(TETAB%R(I)*DEGRAD)*( X_PHREF*Y_PHREF-X(IG) )
     &            + SIN(TETAB%R(I)*DEGRAD)*( X_PHREF*Y_PHREF-Y(IG) )
            IF( I0.EQ.0 ) THEN
              I0 = I
              MINDIST = SDIST
            ELSE
              IF( SDIST.GT.MINDIST ) THEN
                I0 = I
                MINDIST = SDIST
              ENDIF
            ENDIF
          ENDIF
        ENDDO
!
        IF( I0.EQ.0 ) CYCLE
!
        IZ1 = MESH%KP1BOR%I(I0)
        IZ2 = MESH%KP1BOR%I(I0+NPTFR)
        DO WHILE( NUMLIQ%I(IZ1).EQ.N .AND. NUMLIQ%I(IZ2).EQ.N )
          IZ1 = MESH%KP1BOR%I(IZ1)
          IZ2 = MESH%KP1BOR%I(IZ2+NPTFR)
          IF( IZ1.EQ.IZ2 ) EXIT
        ENDDO
        IF( NUMLIQ%I(IZ1).NE.N ) THEN
          DO WHILE( NUMLIQ%I(IZ2).EQ.N )
            IZ2 = MESH%KP1BOR%I(IZ2+NPTFR)
          ENDDO
        ENDIF
        IF( NUMLIQ%I(IZ2).NE.N ) THEN
          DO WHILE( NUMLIQ%I(IZ1).EQ.N )
            IZ1 = MESH%KP1BOR%I(IZ1)
          ENDDO
        ENDIF
!
!       CASE OF A BOUNDARY SPLIT OVER MORE THAN ONE PROCESSOR
! /!\   THIS SHOULD NOT HAPPEN AGAIN BECAUSE ARTEMIS'S SPECIAL TREATMENT
!        IF( NCSIZE.GT.1 ) THEN
!          SDIST = MDIST
!          MDIST = P_MIN( MDIST )
!          IG = 0
!          IF( ABS(SDIST-MDIST).LT.CHOUIA ) IG = I0
!          I0 = P_MAX( I0 )
!        ENDIF
!
!     ***********************************************************
!     DUAL WAVE PROPAGATION ALONG THE BOUNDARY, STARTING FROM I0
!
        IF (LIHBOR%I(I0).EQ.KINC) THEN
!
          WRITE(LU,112) N,MESH%NBOR%I(I0),
     &      X(MESH%NBOR%I(I0)),Y(MESH%NBOR%I(I0))
 112      FORMAT(/,1X,'PHBOR: BOUNDARY ',I3,
     &      ' IS INCIDENT - FIRST IMPACT NODE ',I6,
     &      '[',F12.4,',',F12.4,']')
!
!     1.  FOLLOWING THE BOUNDARY NODES GOING ONE WAY (INCL. I0)
          I  = I0
          IG = MESH%NBOR%I(I)
          X0 = X_PHREF
          Y0 = Y_PHREF
          PHASEOI = 0.D0
          DO WHILE( NUMLIQ%I(I).EQ.N )
!           -- LOCAL CONSTANTS
            AUXIC = COS(TETAB%R(I)*DEGRAD)
            AUXIS = SIN(TETAB%R(I)*DEGRAD)
            AUXI1 = GRAV/OMEGA * HB%R(I)/2.D0
!           -- AUTOMATIC PHASE CALCULATION (TETAB HAVE TO BE UNIFORM ON THE BOUNDARY)
            IF(LPHASEAUTO) THEN
              PHASEOI =
     &          KPHREF*AUXIC*( X(IG)-X0 ) + KPHREF*AUXIS*( Y(IG)-Y0 )
            ELSE
              PHASEOI = PHASEOI +
     &          K%R(IG)*AUXIC*( X(IG)-X0 ) + K%R(IG)*AUXIS*( Y(IG)-Y0 )
!             -- INCREMENT WITHIN
              X0=X(IG)
              Y0=Y(IG)
            ENDIF
!           -- INCIDENT WAVE --> INCIDENT POTENTIAL (REAL, IMAGINAR)
            GRE= AUXI1*SIN( ALFAP%R(I)*DEGRAD + PHASEOI)
            GIM=-AUXI1*COS( ALFAP%R(I)*DEGRAD + PHASEOI)
!           -- INCIDENT WAVE --> GRADIENTS          (REAL, IMAGINAR)
            DDXGRE= AUXI1*COS(ALFAP%R(I)*DEGRAD+PHASEOI)*AUXIC*K%R(IG)
            DDYGRE= AUXI1*COS(ALFAP%R(I)*DEGRAD+PHASEOI)*AUXIS*K%R(IG)
            DDXGIM= AUXI1*SIN(ALFAP%R(I)*DEGRAD+PHASEOI)*AUXIC*K%R(IG)
            DDYGIM= AUXI1*SIN(ALFAP%R(I)*DEGRAD+PHASEOI)*AUXIS*K%R(IG)
!           -- MATRIX AM AND BM COEFFICIENTS
            APHI1B%R(I) = - K%R(IG) * C%R(IG) * CG%R(IG)
     &                   * COS(TETAP%R(I)*DEGRAD)
            BPHI1B%R(I) = 0.D0
!           -- SECOND MEMBER CV1 AND CV2 COEFFICIENTS
!           i * K * Gamma (multiplied by "- cos THETAP" in BERKHO)
            CPHI1B%R(I)  = ( -GIM*K%R(IG) ) *C%R(IG)*CG%R(IG)
            DPHI1B%R(I)  = (  GRE*K%R(IG) ) *C%R(IG)*CG%R(IG)
!           -- GRAD(Gamma) will be used in BERKHO ...
            CGRX1B%R(I)=   ( DDXGRE ) *C%R(IG)*CG%R(IG)
            CGRY1B%R(I)=   ( DDYGRE ) *C%R(IG)*CG%R(IG)
            DGRX1B%R(I)=   ( DDXGIM ) *C%R(IG)*CG%R(IG)
            DGRY1B%R(I)=   ( DDYGIM ) *C%R(IG)*CG%R(IG)
!           -- WHILE INCREMENT
            IF( I.EQ.IZ1 ) EXIT
            I  = MESH%KP1BOR%I(I)
            IG = MESH%NBOR%I(I)
          ENDDO
!
!     2.  FOLLOWING THE BOUNDARY GOING THE OTHER WAY (INCL. I0)
          I  = I0
          IG = MESH%NBOR%I(I)
          X0 = X_PHREF
          Y0 = Y_PHREF
          PHASEOI = 0.D0
          DO WHILE( NUMLIQ%I(I).EQ.N )
!           -- LOCAL CONSTANTS
            AUXIC = COS(TETAB%R(I)*DEGRAD)
            AUXIS = SIN(TETAB%R(I)*DEGRAD)
            AUXI1 = GRAV/OMEGA * HB%R(I)/2.D0
!           -- AUTOMATIC PHASE CALCULATION (TETAB HAVE TO BE UNIFORM ON THE BOUNDARY)
            IF(LPHASEAUTO) THEN
              PHASEOI =
     &          KPHREF*AUXIC*( X(IG)-X0 ) + KPHREF*AUXIS*( Y(IG)-Y0 )
            ELSE
              PHASEOI = PHASEOI +
     &          K%R(IG)*AUXIC*( X(IG)-X0 ) + K%R(IG)*AUXIS*( Y(IG)-Y0 )
!             -- INCREMENT WITHIN
              X0=X(IG)
              Y0=Y(IG)
            ENDIF
!           -- INCIDENT WAVE --> INCIDENT POTENTIAL (REAL, IMAGINAR)
            GRE= AUXI1*SIN( ALFAP%R(I)*DEGRAD + PHASEOI)
            GIM=-AUXI1*COS( ALFAP%R(I)*DEGRAD + PHASEOI)
!           -- INCIDENT WAVE --> GRADIENTS          (REAL, IMAGINAR)
            DDXGRE= AUXI1*COS(ALFAP%R(I)*DEGRAD+PHASEOI)*AUXIC*K%R(IG)
            DDYGRE= AUXI1*COS(ALFAP%R(I)*DEGRAD+PHASEOI)*AUXIS*K%R(IG)
            DDXGIM= AUXI1*SIN(ALFAP%R(I)*DEGRAD+PHASEOI)*AUXIC*K%R(IG)
            DDYGIM= AUXI1*SIN(ALFAP%R(I)*DEGRAD+PHASEOI)*AUXIS*K%R(IG)
!           -- MATRIX AM AND BM COEFFICIENTS
            APHI1B%R(I) = - K%R(IG) * C%R(IG) * CG%R(IG)
     &                   * COS(TETAP%R(I)*DEGRAD)
            BPHI1B%R(I) = 0.D0
!           -- SECOND MEMBER CV1 AND CV2 COEFFICIENTS
!           i * K * Gamma (multiplied by "- cos THETAP" in BERKHO)
            CPHI1B%R(I)  = ( -GIM*K%R(IG) ) *C%R(IG)*CG%R(IG)
            DPHI1B%R(I)  = (  GRE*K%R(IG) ) *C%R(IG)*CG%R(IG)
!           -- GRAD(Gamma) will be used in BERKHO ...
            CGRX1B%R(I)=   ( DDXGRE ) *C%R(IG)*CG%R(IG)
            CGRY1B%R(I)=   ( DDYGRE ) *C%R(IG)*CG%R(IG)
            DGRX1B%R(I)=   ( DDXGIM ) *C%R(IG)*CG%R(IG)
            DGRY1B%R(I)=   ( DDYGIM ) *C%R(IG)*CG%R(IG)
!           -- WHILE INCREMENT
            IF( I.EQ.IZ2 ) EXIT
            I  = MESH%KP1BOR%I(I+NPTFR)
            IG = MESH%NBOR%I(I)
          ENDDO

        ENDIF
!
!     ***********************************************************
!
      ENDDO
!
!     INITIALISATION OF PHASE VARIABLES FOR AUTOMATIC CALCULATION
      X0      = X_PHREF
      Y0      = Y_PHREF
      PHASEOI = 0D0
!
      DO I=1,NPTFR
!
!       ********************************
!       GLOBAL NUMBER OF THE BOUNDARY NODE I
!       ********************************
!
        IG  = MESH%NBOR%I(I)
!
!       ******************************************
!       GLOBAL NUMBER OF THE BOUNDARY NODE PRECEDING I
!       ******************************************
!
!       IGN = MESH%NBOR%I(MESH%KP1BOR%I(I+NPTFR))
!
!       ****************************************
!       GLOBAL NUMBER OF THE BOUNDARY NODE FOLLOWING I
!       ****************************************
!
!       IGP = MESH%NBOR%I(MESH%KP1BOR%I(I))

!       -------------------------------------------------
!             COEFFICIENTS FOR AN INCIDENT WAVE
!       -------------------------------------------------
!             ALREADY TREATED ABOVE
!
!        IF (LIHBOR%I(I).EQ.KINC) THEN
!
!        ENDIF
!
!        -------------------------------------------------
!             COEFFICIENTS FOR AN INCIDENT POTENTIAL
!        -------------------------------------------------
!
        IF (LIHBOR%I(I).EQ.KPOT) THEN
!
!------------ POTENTIAL (REAL, IMAGINAR)
          GRE=PRB%R(I)
          GIM=PIB%R(I)
! ----------- GRADIENTS (REAL, IMAGINAR)
          DDXGRE= DDXPRB%R(I)
          DDYGRE= DDYPRB%R(I)
          DDXGIM= DDXPIB%R(I)
          DDYGIM= DDYPIB%R(I)
! --------COEFFICIENTS
! --      MATRIX AM AND BM COEFFICIENTS
          APHI1B%R(I) = - K%R(IG) * C%R(IG) * CG%R(IG)
     &                  * COS(TETAP%R(I)*DEGRAD)
          BPHI1B%R(I) = 0.D0
! --      SECOND MEMBER CV1 AND CV2 COEFFICIENTS
! ----    i * K * Gamma (multiplied by "- cos THETAP" in BERKHO)
          CPHI1B%R(I)  = ( -GIM*K%R(IG) ) *C%R(IG)*CG%R(IG)
          DPHI1B%R(I)  = (  GRE*K%R(IG) ) *C%R(IG)*CG%R(IG)
! ----    GRAD(Gamma) will be used in BERKHO...
          CGRX1B%R(I)=   ( DDXGRE ) *C%R(IG)*CG%R(IG)
          CGRY1B%R(I)=   ( DDYGRE ) *C%R(IG)*CG%R(IG)
          DGRX1B%R(I)=   ( DDXGIM ) *C%R(IG)*CG%R(IG)
          DGRY1B%R(I)=   ( DDYGIM ) *C%R(IG)*CG%R(IG)
        ENDIF
!         -------------------------------------------------
!         COEFFICIENTS FOR A FREE EXIT BOUNDARY SEGMENT
!         -------------------------------------------------
!
        IF(LIHBOR%I(I).EQ.KSORT) THEN
!
          APHI2B%R(I)  = - K%R(IG) * C%R(IG) * CG%R(IG)
     &                   * COS(TETAP%R(I)*DEGRAD)
          BPHI2B%R(I)  = 0.D0
          CPHI2B%R(I)  = 0.D0
          DPHI2B%R(I)  = 0.D0
!
        ELSEIF (LIHBOR%I(MESH%KP1BOR%I(I)).EQ.KSORT) THEN
          APHI2B%R(I)  = - K%R(IG) * C%R(IG) * CG%R(IG)
     &                   * COS(TETAP%R(MESH%KP1BOR%I(I))*DEGRAD)
          BPHI2B%R(I)  = 0.D0
          CPHI2B%R(I)  = 0.D0
          DPHI2B%R(I)  = 0.D0
!
        ELSEIF (LIHBOR%I(MESH%KP1BOR%I(I+NPTFR)).EQ.KSORT) THEN
          APHI2B%R(I)  = - K%R(IG) * C%R(IG) * CG%R(IG)
     &                  * COS(TETAP%R(MESH%KP1BOR%I(I+NPTFR))*DEGRAD)
          BPHI2B%R(I)  = 0.D0
          CPHI2B%R(I)  = 0.D0
          DPHI2B%R(I)  = 0.D0
!
        ELSE
          APHI2B%R(I)  = 0.D0
          BPHI2B%R(I)  = 0.D0
          CPHI2B%R(I)  = 0.D0
          DPHI2B%R(I)  = 0.D0
        ENDIF
!
!       -------------------------------------------
!       COEFFICIENTS FOR A SOLID BOUNDARY SEGMENT
!       -------------------------------------------
!
        IF (LIHBOR%I(I).EQ.KLOG) THEN
!
          AUXI1 = K%R(IG) * C%R(IG) * CG%R(IG) *
     &             COS(TETAP%R(I)*DEGRAD) /
     &             (1.D0 + RP%R(I)*RP%R(I) +
     &             2.D0*RP%R(I)*COS(ALFAP%R(I)*DEGRAD))
!
          APHI3B%R(I) = - (1.D0 - RP%R(I) * RP%R(I) ) * AUXI1
!
          BPHI3B%R(I) = 2.D0*RP%R(I)*SIN(ALFAP%R(I)*DEGRAD) * AUXI1
!
          CPHI3B%R(I)  = 0.D0
!
          DPHI3B%R(I)  = 0.D0
!
        ELSEIF (LIHBOR%I(MESH%KP1BOR%I(I)).EQ.KLOG) THEN
          AUXI1 = K%R(IG) * C%R(IG) * CG%R(IG) *
     &      COS(TETAP%R(MESH%KP1BOR%I(I))*DEGRAD) /
     &      (1.D0 + RP%R(MESH%KP1BOR%I(I))*RP%R(MESH%KP1BOR%I(I))
     &      +2.D0 * RP%R(MESH%KP1BOR%I(I))*
     &       COS(ALFAP%R(MESH%KP1BOR%I(I))*DEGRAD))
!
          APHI3B%R(I) = - (1.D0-RP%R(MESH%KP1BOR%I(I))*
     &      RP%R(MESH%KP1BOR%I(I))) * AUXI1
!
          BPHI3B%R(I) = 2.D0*RP%R(MESH%KP1BOR%I(I))
     &                * SIN(ALFAP%R(MESH%KP1BOR%I(I))*DEGRAD) * AUXI1
!
          CPHI3B%R(I)  = 0.D0
!
          DPHI3B%R(I)  = 0.D0
!
        ELSEIF (LIHBOR%I(MESH%KP1BOR%I(I+NPTFR)).EQ.KLOG) THEN
          AUXI1 = K%R(IG) * C%R(IG) * CG%R(IG) *
     &     COS(TETAP%R(MESH%KP1BOR%I(I+NPTFR))*DEGRAD) /
     &     (1.D0 + RP%R(MESH%KP1BOR%I(I+NPTFR))
     &      *RP%R(MESH%KP1BOR%I(I+NPTFR))
     &      +2.D0 * RP%R(MESH%KP1BOR%I(I+NPTFR))*
     &       COS(ALFAP%R(MESH%KP1BOR%I(I+NPTFR))*DEGRAD))
!
          APHI3B%R(I) = - (1.D0-RP%R(MESH%KP1BOR%I(I+NPTFR))*
     &       RP%R(MESH%KP1BOR%I(I+NPTFR))) * AUXI1
!
          BPHI3B%R(I) = 2.D0*RP%R(MESH%KP1BOR%I(I+NPTFR))
     &      * SIN(ALFAP%R(MESH%KP1BOR%I(I+NPTFR))*DEGRAD) * AUXI1
!
          CPHI3B%R(I)  = 0.D0
!
          DPHI3B%R(I)  = 0.D0
!
        ELSE
          APHI3B%R(I)  = 0.D0
!
          BPHI3B%R(I)  = 0.D0
!
          CPHI3B%R(I)  = 0.D0
!
          DPHI3B%R(I)  = 0.D0
!
        ENDIF
!
!        -------------------------------------------------
!        COEFFICIENTS FOR AN IMPOSED WAVE BOUNDARY SEGMENT
!        -------------------------------------------------
!DA      -----------------------------------
!DA      KEPT FOR MEMORY !
!DA      -----------------------------------
!DA
!DA         IF (LIHBOR(I).EQ.KENT) THEN
!DA         AUXIC      = COS(TETAB(I)*DEGRAD)
!DA         AUXIS      = SIN(TETAB(I)*DEGRAD)
!DA         AUXI1      = GRAV/OMEGA * HB(I)/2.D0 * C(IG) * CG(IG) *
!DA     *                K(IG) * ( AUXIC *XSGBOR(I) +
!DA     *                          AUXIS *YSGBOR(I) )
!DA         AUXI2      = K(IG) * ( X(IG)*AUXIC +
!DA     *                          Y(IG)*AUXIS )
!DA
!DA         APHI4B(I)  = 0.D0
!DA
!DA         BPHI4B(I)  = 0.D0
!DA
!DA         CPHI4B(I)  = AUXI1 * COS( AUXI2 )
!DA
!DA         DPHI4B(I)  = AUXI1 * SIN( AUXI2 )
!DA
!DA       VALUES IMPOSED AT THE NODES OF A KENT SEGMENT
!DA         LIDIR(I)         = KENT
!DA
!DA         AUXI1 = GRAV/OMEGA * HB(I)/2.D0
!DA         AUXI2 = K(IG) * (X(IG)*AUXIC +
!DA     *                    Y(IG)*AUXIS )
!DA
!DA            PHIRB(I) =   AUXI1 * SIN( AUXI2 )
!DA            PHIIB(I) = - AUXI1 * COS( AUXI2 )
!DA         ENDIF
!
!
      ENDDO
!-----------------------------------------------------------------------
!
!
      RETURN
      END SUBROUTINE
