!                     **************************
                      SUBROUTINE PRERES3D_KHIONE
!                     **************************
!
     &(LT,TN,NPOIN3,MESH)
!
!***********************************************************************
! KHIONE   V7P3
!***********************************************************************
!
!brief    PREPARES THE VARIABLES WHICH WILL BE WRITTEN TO
!+         THE RESULTS FILE OR TO THE LISTING.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| LT             |-->| CURRENT NUMBER OF OF TIME STEP
!| MESH           |-->| MESH STRUCTURE
!| NPOIN          |-->| NUMBER OF NODES
!| TN             |-->| TELEMAC3D TRACER VALUES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_KHIONE
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_KHIONE, EX_PRERES3D_KHIONE => PRERES3D_KHIONE
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,               INTENT(IN) :: LT
      TYPE(BIEF_OBJ),        INTENT(IN) :: TN
      INTEGER,               INTENT(IN) :: NPOIN3
      TYPE(BIEF_MESH),       INTENT(IN) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J
      LOGICAL IMP,LEO
!
!-----------------------------------------------------------------------
!
      IMP = .FALSE.
      LEO = .FALSE.
      IF( LT.EQ.(LT/LISPRD)*LISPRD ) IMP = .TRUE.
      IF( LT.EQ.(LT/LEOPRD)*LEOPRD ) LEO = .TRUE.
!
!     NO PRINTOUT REQUIRED (LISTING OR RESULT FILE): EXITS
      IF( .NOT.(LEO.OR.IMP) ) RETURN
!
!-----------------------------------------------------------------------
!
!     FOR EVERY TIME STEP
!
!=======================================================================
!     VARIABLES OF THERMAL BUDGET FROM TRACER
!=======================================================================
!
      IF( (LEO.AND.SORLEO3(2)).OR.(IMP.AND.SORIMP3(2)) ) THEN
        DO I = 1,NPOIN3
          CTOT3%R(I) = TN%ADR(IND_FRA)%P%R(I)
        ENDDO
        IF(NC_FRA.GT.1) THEN
          DO I = 2,NC_FRA
            DO J = 1,NPOIN3
              CTOT3%R(J) = CTOT3%R(J) + TN%ADR(IND_FRA+I-1)%P%R(J)
            ENDDO
          ENDDO
        ENDIF
      ENDIF
!
      IF( (LEO.AND.SORLEO3(1)).OR.(IMP.AND.SORIMP3(1)) ) THEN
        DO I = 1,NPOIN3
          NTOT3%R(I) = TN%ADR(IND_FRA)%P%R(I) / VK_FRZL(1)
        ENDDO
        IF(NC_FRA.GT.1) THEN
          DO I = 2,NC_FRA
            DO J = 1,NPOIN3
              NTOT3%R(J) = NTOT3%R(J) + TN%ADR(IND_FRA+I-1)%P%R(J) /
     &                    VK_FRZL(I)
            ENDDO
          ENDDO
        ENDIF
      ENDIF

      IF( (LEO.AND.SORLEO3(3)).OR.(IMP.AND.SORIMP3(3)) ) THEN
        DO I = 1,NPOIN3
          ELEZ%R(I) = MESH%Z%R(I)
        ENDDO
      ENDIF

!
      DO I=1,NC_FRA
        IF( (LEO.AND.SORLEO3(3+I)).OR.(IMP.AND.SORIMP3(3+I)) ) THEN
          DO J = 1, NPOIN3
            FRZL3%ADR(I)%P%R(J) = TN%ADR(IND_FRA+I-1)%P%R(J)
          ENDDO
        ENDIF
      ENDDO
!
      DO I=1,NC_FRA
        IF( (LEO.AND.SORLEO3(3+I+NC_FRA))
     &       .OR.(IMP.AND.SORIMP3(3+I+NC_FRA)) ) THEN
          DO J = 1,NPOIN3
            NBP3%ADR(I)%P%R(J) = TN%ADR(IND_FRA+I-1)%P%R(J) /
     &                            VK_FRZL(I)
          ENDDO
        ENDIF
      ENDDO
!
      IF( (LEO.AND.SORLEO3(3+2*NC_FRA+1))
     &     .OR.(IMP.AND.SORIMP3(3+2*NC_FRA+1)) ) THEN
        DO I = 1,NPOIN3
          TEMP3%R(I) = TN%ADR(IND_T)%P%R(I)
        ENDDO
      ENDIF
!
      IF(SALINITY) THEN
        IF( (LEO.AND.SORLEO3(3+2*NC_FRA+2))
     &       .OR.(IMP.AND.SORIMP3(3+2*NC_FRA+2)) ) THEN
          DO I = 1,NPOIN3
            SAL3%R(I) = TN%ADR(IND_S)%P%R(I)
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
