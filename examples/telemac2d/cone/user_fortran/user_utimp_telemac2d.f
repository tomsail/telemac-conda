!                   *******************************
                    SUBROUTINE USER_UTIMP_TELEMAC2D
!                   *******************************
!
     &(LTL,ATL,GRADEBL,GRAPRDL,LISDEBL,LISPRDL)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    WRITES OUT ADDITIONAL OUTPUT REQUIRED BY THE USER.
!
!note     THIS SUBROUTINE IS CALLED IN THE SAME PLACES AS THE
!+                MAIN TELEMAC2D OUTPUT SUBROUTINE (NAMED DESIMP),
!+                I.E. CALLED TWICE:
!+
!note   (1) ONCE PER RUN, WHEN LTL==0, INDEPENDENTLY OF WHETHER
!+             'OUTPUT OF INITIAL CONDITIONS : YES' IS SET OR NOT
!note   (2) EACH TIME STEP JUST AFTER DESIMP-OUTPUT
!
!history  JACEK A. JANKOWSKI PINXIT, BAW KARLSRUHE, JACEK.JANKOWSKI@BAW.DE
!+        **/08/2003
!+        V5P4
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ATL            |-->| TIME OF TIME STEP, IN SECONDS
!| GRADEBL        |-->| FIRST TIME STEP FOR GRAPHIC OUTPUTS
!| GRAPRDL        |-->| PERIOD OF GRAPHIC OUTPUTS
!| LISDEBL        |-->| FIRST TIME STEP FOR LISTING OUTPUTS
!| LISPRDL        |-->| PERIOD OF LISTING OUTPUTS
!| LTL            |-->| CURRENT TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: ATL
      INTEGER, INTENT(IN) :: LTL,GRADEBL,GRAPRDL,LISDEBL,LISPRDL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!     CUSTOM PRINTOUT PERIOD
      LOGICAL USE_CUSTOM_LEOPRD
      INTEGER CUSTOM_LEOPRD, FID
      PARAMETER (USE_CUSTOM_LEOPRD=.TRUE.)
!
      TYPE(BIEF_OBJ) ONES, MASSM
      TYPE(BIEF_OBJ) SVIDE
!
      DOUBLE PRECISION MASSTOT
!
      ALLOCATE(SVIDE%R(NPOIN))
      ALLOCATE(ONES%R(NPOIN))
      ALLOCATE(MASSM%R(NPOIN))
!
      CALL BIEF_ALLVEC(1, SVIDE,'SVIDE   ',IELMH, 1,1,MESH)
      CALL BIEF_ALLVEC(1, ONES,'ONES   ',IELMH, 1,1,MESH)
      CALL BIEF_ALLVEC(1, MASSM,'MASSM   ',IELMH, 1,1,MESH)
!
      SVIDE%R(:) = 0.D0
      ONES%R(:) = 1.D0
      MASSM%R(:) = 0.D0
!
      SVIDE%TYPE = 2
      ONES%TYPE = 2
      MASSM%TYPE = 2
!
!-----------------------------------------------------------------------
!
!     DEFINING CUSTOM PRINTOUT PERIOD
      IF(USE_CUSTOM_LEOPRD) THEN
        CUSTOM_LEOPRD=1
      ELSE
        CUSTOM_LEOPRD=LEOPRD
      ENDIF
!
!-----------------------------------------------------------------------
! MASS MATRIX AT FINAL TIME
!-----------------------------------------------------------------------
!
!     COMPUTE MASS MATRIX
      IF(MOD(LT,CUSTOM_LEOPRD).EQ.0) THEN
!
        CALL VECTOR(MASSM,'=     ','MASVEC          ',
     &        IELMH,
     &        1.D0,ONES,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &        MESH,MSK,MASKEL)
!
!       COMPUTE TOTAL MASS
        MASSTOT=0.D0
        DO I=1,MESH%NPOIN
          MASSTOT = MASSTOT + MASSM%R(I)
        ENDDO
      ENDIF
!
!     WRITE MASS AT FINAL TIME
      IF(LT.EQ.NIT) THEN
        IF(IPID.EQ.0) THEN
          FID = T2D_FILES(T2DRFO)%LU
          DO I=1,MESH%NPOIN
            WRITE(FID,*) MASSM%R(I)
          ENDDO
        ENDIF
      ENDIF
!
      DEALLOCATE(MASSM%R)
      CALL BIEF_DEALLVEC(MASSM)
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
