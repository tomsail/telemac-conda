!                   *********************
                    SUBROUTINE USER_UTIMP
!                   *********************
!
     & (GRADEBL,GRAPRDL,LISDEBL,LISPRDL)
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    PRINTS OUT SPECIFIC RESULTS
!+               (SEE THE LIST OF VARIABLES IN DECLARATIONS_TELEMAC3D).
!+
!+            FOR BIEF_OBJ STRUCTURES, THE DOUBLE PRECISION ARRAY
!+                IS IN COMPONENT R, E.G. U%R FOR THE VELOCITY.
!
!history  C LE NORMANT(LNH)    ; F LEPEINTRE (LNH)
!+        25/11/97
!+        V5P2
!+
!
!history  Y. AUDOUIN (LNHE)
!+        22/10/18
!+        V8P1
!+   Creation from UTIMP
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| GRADEBL        |-->| NUMBER OF FIRST TIME STEP FOR GRAPHIC PRINTOUTS
!| GRAPRDL        |-->| GRAPHIC PRINTOUT PERIOD
!| LISDEBL        |-->| NUMBER OF FIRST TIME STEP FOR LISTING PRINTOUTS
!| LISPRDL        |-->| LISTING PRINTOUT PERIOD
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: GRADEBL,GRAPRDL,LISDEBL,LISPRDL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,NODE,N
      DOUBLE PRECISION DELTAZ,ULOG,AUX
!
!-----------------------------------------------------------------------
!
!     SPECIFIC TO ROUSE'S PROFILE
!
!     BEGINNING OF ESSAI JMH
      N=46
      IF(NCSIZE.GT.1) THEN
        NODE=0
        DO I=1,NPOIN2
          IF(MESH2D%KNOLG%I(I).EQ.N) NODE=I
        ENDDO
      ELSE
        NODE=N
      ENDIF
      IF(LT.EQ.NIT.AND.NODE.GT.0) THEN
        WRITE(LU,*)
     &          '        Z                      U                  ULOG'
        DO I=1,NPLAN
          IF(I.EQ.1) THEN
            DELTAZ=(MESH3D%Z%R(NODE+NPOIN2)-MESH3D%Z%R(NODE))
     &              /2.71828182845D0**2
          ELSE
            DELTAZ=MESH3D%Z%R(NODE+(I-1)*NPOIN2)-MESH3D%Z%R(NODE)
          ENDIF
          AUX=MAX(30.D0*DELTAZ/0.0162D0,1.D0)
          ULOG=(0.0703D0/0.41D0)*LOG(AUX)
          WRITE(LU,*) MESH3D%Z%R(NODE+(I-1)*NPOIN2),
     &                U%R(NODE+(I-1)*NPOIN2), ULOG
        ENDDO
        WRITE(LU,*) '         Z                  NUT VIT '
        DO I=1,NPLAN
          WRITE(LU,*) MESH3D%Z%R(NODE+(I-1)*NPOIN2),
     &                VISCVI%ADR(3)%P%R(NODE+(I-1)*NPOIN2)
        ENDDO
        IF(NTRAC.GT.0) THEN
          WRITE(LU,*)
     &         '         Z                      C               NU TRAC'
          DO I=1,NPLAN
            WRITE(LU,*) MESH3D%Z%R(NODE+(I-1)*NPOIN2),
     &                  TA%ADR(NTRAC)%P%R(NODE+(I-1)*NPOIN2),
     &                 VISCTA%ADR(NTRAC)%P%ADR(3)%P%R(NODE+(I-1)*NPOIN2)
          ENDDO
        ENDIF
      ENDIF
!     END ESSAI JMH
!
!-----------------------------------------------------------------------
!
      RETURN
      END
