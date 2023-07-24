!                   *********************
                    SUBROUTINE BED_FLUXES
!                   *********************
!
!
!***********************************************************************
! TELEMAC3D   V7P1                                   27/04/2015
!***********************************************************************
!
!brief    BUILDS THE SOURCE TERMS CALCULATED FROM THE BED FLUXES
!+                TO ADD IN THE 3D CONTINUITY EQUATIONS.
!
!history  A JOLY (LNHE)
!+        27/04/2015
!+        V7P1
!+  First version
!
!history  A JOLY (LNHE)
!+        07/01/2016
!+        V7P1
!+  Correction of a bug: SMH IS ASSEMBLED IN PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION :: XNB,YNB,ZNB
!
!-----------------------------------------------------------------------
!
!     SETTING BEDFLU TO 0 AND DEFINING TEMP WORK ARRAYS
!
      CALL OS('X=0     ',X=BEDFLU)
      CALL CPSTVC(BEDFLU,T2_01)
      CALL CPSTVC(BEDFLU,T2_02)
      DO I = 1,NPOIN2
        IF(LIWBOF%I(I).EQ.KENT.OR.
     &     LIWBOF%I(I).EQ.KENTU) THEN
          XNB=GRADZF%ADR(1)%P%R(I)
          YNB=GRADZF%ADR(2)%P%R(I)
          ZNB=-SQRT(1-XNB**2-YNB**2)
          T2_01%R(I) = -UBORF%R(I)*XNB-VBORF%R(I)*YNB-WBORF%R(I)*ZNB
        ELSE
          T2_01%R(I) = 0.D0
        ENDIF
      ENDDO
!
      CALL VECTOR(BEDFLU,'=','FLUBOR          ',
     &            IELM2H,1.D0,T2_01,SVIDE,SVIDE,SVIDE,
     &            SVIDE,SVIDE,MESH2D,.FALSE.,MASKEL)
!
!     SMH IS ASSEMBLED IN PARALLEL
      CALL OS('X=Y     ',X=T2_02,Y=BEDFLU)
      IF(NCSIZE.GT.1) CALL PARCOM(T2_02,2,MESH2D)
      CALL OS('X=X+Y   ',X=SMH,Y=T2_02)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
