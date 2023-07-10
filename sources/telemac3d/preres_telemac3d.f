!                   ***************************
                    SUBROUTINE PRERES_TELEMAC3D
!                   ***************************
!
!
!***********************************************************************
! TELEMAC3D   V8P2
!***********************************************************************
!
!brief    PREPARES THE VARIABLES WHICH WILL BE WRITTEN TO
!+                THE RESULTS FILE OR TO THE LISTING.
!
!history  J-M HERVOUET (LNH)
!+        30/03/04
!+        V5P7
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
!history  J-M HERVOUET (LNHE)
!+        14/10/2011
!+        V6P2
!+   ADDING MAXIMUM ELEVATION AND ASSOCIATED TIME
!
!history  J-M HERVOUET (LNHE)
!+        02/04/2012
!+        V6P2
!+   DH and HN added in a 3D array for a clean restart.
!
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        14/03/2014
!+        V7P0
!+   New developments in sediment merged on 14/03/2014.
!
!history  J-M HERVOUET (LNHE)
!+        10/09/2015
!+        V7P1
!+   With the decision to always issue the initial condition, this
!+   subroutine must be changed.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        21/01/2016
!+        V7P1
!+   Variables for clean restart mode must be built for initial
!+   conditions.
!
!history  Y. AUDOUIN (LNHE)
!+        22/10/18
!+        V8P1
!+   Call of USER_PRERES_TELEMAC3D User Fortran where the modifications
!+   are done
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| LT             |-->| ITERATION NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_PRERES_TELEMAC3D => PRERES_TELEMAC3D
      USE DECLARATIONS_TELEMAC3D
      USE DECLARATIONS_TELEMAC, ONLY : COUPLING
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL LEO
!
      INTEGER LTT,I,IPLAN,I3,J
      DOUBLE PRECISION DELTAZ,U_0,U_1,V_0,V_1,C_0,C_1,XMAX
!
      LOGICAL :: DEJA = .FALSE.
!
      INTRINSIC SQRT,MAX
!
!-----------------------------------------------------------------------
!
! 1)  PART WHICH MUST BE DONE EVEN IF THERE IS NO OUTPUT FOR THIS TIMESTEP
!     BUT ONLY AFTER FIRST TIMESTEP FOR GRAPHIC PRINTOUTS
!
!-----------------------------------------------------------------------
!
      IF(LT.GE.GRADEB) THEN
!
!=======================================================================
! COMPUTES THE MAXIMUM ELEVATION AND ASSOCIATED TIME
!=======================================================================
!
        IF(SORG2D(35)) THEN
          IF(.NOT.DEJA) THEN
            CALL OS('X=Y     ',X=MAXZ ,Y=ZF)
            CALL OS('X=C     ',X=TMAXZ,C=AT)
            DEJA=.TRUE.
          ELSE
            DO I=1,NPOIN2
              XMAX=H%R(I)+ZF%R(I)
!             DRY LAND EXCLUDED (TO AVOID RANDOM TIMES)
              IF(XMAX.GT.MAXZ%R(I).AND.H%R(I).GT.0.01D0) THEN
                MAXZ%R(I)=XMAX
                IF(SORG2D(36)) TMAXZ%R(I)=AT
              ENDIF
            ENDDO
          ENDIF
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
!       CASE WHERE OUTINI=.TRUE. : PRIORITY ON PTINIG, VALUES FOR LT=0
!       OTHERWISE THEY WOULD NOT BE INITIALISED
        IF(SORG2D(35)) CALL OS('X=Y     ',X=MAXZ ,Y=ZF)
        IF(SORG2D(36)) CALL OS('X=C     ',X=TMAXZ,C=AT)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
! 2)  PART WHICH MUST BE DONE ONLY IF THERE IS AN OUTPUT FOR THIS TIMESTEP
!
!-----------------------------------------------------------------------
!
!     IS THERE AN OUTPUT OR NOT AT THIS TIMESTEP ?
!
      LEO=.FALSE.
      LTT=(LT/GRAPRD)*GRAPRD
      IF((LT.EQ.LTT.OR.LT.EQ.NIT).AND.
     &   (LT.GE.GRADEB.OR.LT.EQ.0)) THEN
        LEO=.TRUE.
      ENDIF
!
!     NO PRINT OUT, NO OUTPUT TO FILE: EXITS
      IF(.NOT.LEO) GO TO 1000
!
!
!-----------------------------------------------------------------------
!
! VARIABLES FOR 2D OUTPUTS
!
!-----------------------------------------------------------------------
!
!
!=======================================================================
! CELERITY OF WAVES = SQRT(GH) : PUT INTO T2_10
!=======================================================================
!
      IF(LEO.AND.SORG2D(3)) THEN
        DO I=1,NPOIN2
          T2_10%R(I)=SQRT(GRAV*MAX(H%R(I),0.D0))
        ENDDO
      ENDIF
!
!=======================================================================
! FREE SURFACE (= H + ZF) : PUT INTO T2_01
!=======================================================================
!
      IF(LEO.AND.SORG2D(5)) THEN
        CALL OS('X=Y+Z   ', X=T2_01, Y=H, Z=ZF)
      ENDIF
!
!=======================================================================
! FROUDE NUMBER = U/SQRT(GH) : PUT INTO T2_02
!=======================================================================
!
      IF(LEO.AND.SORG2D(7)) THEN
        DO I=1,NPOIN2
          T2_02%R(I)=SQRT(  (U2D%R(I)**2+V2D%R(I)**2)
     &                     /(GRAV*MAX(H%R(I),1.D-3))   )
        ENDDO
      ENDIF
!
!=======================================================================
! SCALAR DISCHARGE : PUT INTO T2_03
!=======================================================================
!
      IF(LEO.AND.SORG2D(8)) THEN
        CALL OS( 'X=N(Y,Z)' , X=T2_03 , Y=U2D, Z=V2D )
        CALL OS( 'X=XY    ' , X=T2_03 , Y=H )
      ENDIF
!
!=======================================================================
! DISCHARGE ALONG X : PUT INTO T2_04
!=======================================================================
!
      IF(LEO.AND.SORG2D(13)) THEN
        CALL OS( 'X=YZ    ' , X=T2_04 , Y=H , Z=U2D )
      ENDIF
!
!=======================================================================
! DISCHARGE ALONG Y : PUT INTO T2_05
!=======================================================================
!
      IF(LEO.AND.SORG2D(14)) THEN
        CALL OS( 'X=YZ    ' , X=T2_05 , Y=H , Z=V2D )
      ENDIF
!
!=======================================================================
! NORM OF VELOCITY : PUT INTO T2_06
!=======================================================================
!
      IF(LEO.AND.SORG2D(15)) THEN
        CALL OS( 'X=N(Y,Z)' , X=T2_06 , Y=U2D , Z=V2D )
      ENDIF
!
!=======================================================================
! SEDIMENT RELATED VARIABLES THAT ARE ONLY KNOWN AT THE END OF THE
! FIRST TIMESTEP : SETS HERE TO 0 IF LT=0
!=======================================================================
!
!     IF(LEO.AND.SORG2D(24).AND.LT.EQ.0) THEN
!       CALL OS( 'X=0     ' , X=S3D_EPAI)
!     ENDIF
      IF(LEO.AND.SORG2D(25).AND.LT.EQ.0) THEN
        CALL OS( 'X=0     ' , X=S3D_FLUER)
      ENDIF
      IF(LEO.AND.SORG2D(26).AND.LT.EQ.0) THEN
        CALL OS( 'X=0     ' , X=S3D_FLUDP)
      ENDIF
!
!=======================================================================
! FRICTION VELOCITY
!=======================================================================
!
      IF(LEO.AND.SORG2D(31)) THEN
        CALL OS( 'X=SQR(Y)' , X=T2_07 , Y=UETCAR )
      ENDIF
!
!=======================================================================
! SOLID DISCHARGE (ALL 3 DONE IF ONE ASKED)
!=======================================================================
!
      IF(LEO.AND.(SORG2D(32).OR.SORG2D(33).OR.SORG2D(34))) THEN
!       QS IN T2_11, QSX IN T2_12, QSY IN T2_13
!       GIVING A STRUCTURE OF LINEAR 2D VECTOR, LIKE H
        CALL CPSTVC(H,T2_11)
        CALL CPSTVC(H,T2_12)
        CALL CPSTVC(H,T2_13)
!       INITIALISES QSX AND QSY
        CALL OS('X=0     ',X=T2_12)
        CALL OS('X=0     ',X=T2_13)
        DO IPLAN=1,NPLAN-1
          DO I=1,NPOIN2
            I3=I+NPOIN2*(IPLAN-1)
            DELTAZ=Z(I3+NPOIN2)-Z(I3)
!           INTEGRATES U*C ON THE VERTICAL
            U_0=U%R(I3)
            U_1=U%R(I3+NPOIN2)
            V_0=V%R(I3)
            V_1=V%R(I3+NPOIN2)
            C_0=TA%ADR(NTRAC)%P%R(I3)
            C_1=TA%ADR(NTRAC)%P%R(I3+NPOIN2)
            T2_12%R(I)=T2_12%R(I)+DELTAZ*((U_0*C_1+U_1*C_0)/2.D0
     &                                   +(U_1-U_0)*(C_1-C_0)/3.D0)
            T2_13%R(I)=T2_13%R(I)+DELTAZ*((V_0*C_1+V_1*C_0)/2.D0
     &                                   +(V_1-V_0)*(C_1-C_0)/3.D0)
          ENDDO
        ENDDO
!       SOLID DISCHARGE IN M2/S (AS IN SISYPHE, FOR COMPARISON)
        CALL OS('X=CX    ',X=T2_12,C=1.D0/S3D_RHOS)
        CALL OS('X=CX    ',X=T2_13,C=1.D0/S3D_RHOS)
!       QS AS NORM OF QSX AND QSY
        IF(SORG2D(32)) THEN
          CALL OS( 'X=N(Y,Z)' , X=T2_11 , Y=T2_12 , Z=T2_13 )
        ENDIF
      ENDIF
!
!=======================================================================
! DEPTH-AVERAGED TRACERS (VARIABLES ADR_TRAC_2D TO ADR_TRAC_2D+NTRAC-1)
!=======================================================================
!
      IF(NTRAC.GT.0) THEN
        DO I=1,NTRAC
          IF(LEO.AND.SORG2D(ADR_TRAC_2D+I-1)) THEN
            CALL VERMOY(TRAV2%ADR(ADR_TRAV2+I)%P%R,
     &                  TRAV2%ADR(ADR_TRAV2+I)%P%R,
     &                  TA%ADR(I)%P%R,TA%ADR(I)%P%R,1,Z,
     &                  T3_01%R,T3_02%R,T3_03%R,1,NPLAN,NPOIN2,
     &                  NPLAN,OPTBAN)
          ENDIF
        ENDDO
      ENDIF
!
!=======================================================================
! KHIONE RESULTS
!=======================================================================
!
      IF(INCLUS(COUPLING,'KHIONE')) THEN
        CALL PRERES_TELEMAC3D_KHIONE
      ENDIF
!
!=======================================================================
! VELOCITY COMPONENTS AT THE SURFACE
!=======================================================================
!
      IF(LEO.AND.SORG2D(39)) THEN
        DO I=1,NPOIN2
          T2_14%R(I) = U%R(I+NPOIN3-NPOIN2)
        ENDDO
      ENDIF
!
      IF(LEO.AND.SORG2D(40)) THEN
        DO I=1,NPOIN2
          T2_15%R(I) = V%R(I+NPOIN3-NPOIN2)
        ENDDO
      ENDIF
!
      IF(LEO.AND.SORG2D(41)) THEN
        DO I=1,NPOIN2
          T2_16%R(I) = W%R(I+NPOIN3-NPOIN2)
        ENDDO
      ENDIF
!
      IF(LEO.AND.SORG2D(42)) THEN
        DO I=1,NPOIN2
          T2_17%R(I) = SQRT(T2_14%R(I)**2+T2_15%R(I)**2+T2_16%R(I)**2)
        ENDDO
      ENDIF
!
!=======================================================================
! TRACERS AT THE SURFACE (VARIABLES ADR_TRAC_2D TO ADR_TRAC_2D+NTRAC-1)
!=======================================================================
!
      IF(NTRAC.GT.0) THEN
        DO I=1,NTRAC
          IF(LEO.AND.SORG2D(ADR_TRAC_2D+NTRAC+I-1)) THEN
            DO J=1,NPOIN2
              TRAV2%ADR(ADR_TRAV2+NTRAC+I)%P%R(J) =
     &        TA%ADR(I)%P%R(J+NPOIN3-NPOIN2)
            ENDDO
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
! VARIABLES FOR 3D OUTPUTS
!
!-----------------------------------------------------------------------
!
!
!=======================================================================
! HYDROSTATIC PRESSURE
!=======================================================================
!
      IF(NONHYD.AND.LEO.AND.SORG3D(12)) THEN
        CALL PHSTAT(PH%R,DELTAR%R,Z,T3_01%R,T3_02%R,RHO0,GRAV,
     &              NPOIN3,NPOIN2,NPLAN)
      ENDIF
!
!=======================================================================
! FOR RESTARTS, STORAGE OF DH AND HN IN A 3D ARRAY
!=======================================================================
!
      IF(LEO.AND.(SORG3D(19).OR.(SOREST(19).AND.
!        INITIAL CONDITIONS NOW ALWAYS WANTED, THOUGH NOT USED
     &   (LT.EQ.0.OR.LT.EQ.NIT)))) THEN
        DO I=1,NPOIN2
          DHHN%R(I       )=DH%R(I)
          DHHN%R(I+NPOIN2)=HN%R(I)
        ENDDO
        IF(NPLAN.GT.2) THEN
          DO I=2*NPOIN2+1,NPLAN*NPOIN2
            DHHN%R(I)=0.D0
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
!
      ! USER FUNCTION
      CALL USER_PRERES_TELEMAC3D(LEO)
1000  CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END

