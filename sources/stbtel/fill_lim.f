!                       *******************
                        SUBROUTINE FILL_LIM
!                       *******************
!
     & (NPTFR,NPTFRX,NTRAC,LIHBOR,LIUBOR,LIVBOR,LITBOR,
     &  HBOR,UBOR,VBOR,CHBORD,TBOR,ATBOR,BTBOR, NBOR, OLD_NBOR, KP1BOR)
!
!***********************************************************************
! PROGICIEL : STBTEL  V5.2                 J-M JANIN   (LNH) 30 87 72 84
! ORIGINE   : TELEMAC
!***********************************************************************
!
!     FUNCTION  :  FILLS THE BOUNDARY CONDITIONS ARRAYS BASED ON THE
!                  COARSER MESH INFORMATION
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! | HBOR           | <->| PRESCRIBED DEPTH
! | LIHBOR         | -->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
! | LITBOR         | -->| TYPE OF BOUNDARY CONDITIONS ON TRACERS
! | LIUBOR         | -->| TYPE OF BOUNDARY CONDITIONS ON VELOCITY
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!----------------------------------------------------------------------
! APPELE PAR : STBTEL
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE

      INTEGER, INTENT(IN)    :: NPTFR,NPTFRX,NTRAC
      INTEGER,INTENT(INOUT) :: LIHBOR(NPTFRX),LIUBOR(NPTFRX)
      INTEGER,INTENT(INOUT) :: LIVBOR(NPTFRX)
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: LITBOR
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(NPTFRX,2),VBOR(NPTFRX,2)
      DOUBLE PRECISION, INTENT(INOUT) :: HBOR(NPTFRX)
      DOUBLE PRECISION, INTENT(INOUT) :: CHBORD(NPTFRX)
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: TBOR, ATBOR, BTBOR
      INTEGER, INTENT(IN) :: NBOR(NPTFRX), OLD_NBOR(NPTFRX)
      INTEGER, INTENT(IN) :: KP1BOR(NPTFRX)

      INTEGER I, P1, P2
      INTEGER VAL_P1, VAL_P2, IDX

      LOGICAL :: REORDER, FOUND
      INTEGER, ALLOCATABLE :: CONV(:)
      INTEGER J, NODE1, NODE2

      INTEGER :: OLD_LIHBOR(NPTFR),OLD_LIUBOR(NPTFR)
      INTEGER :: OLD_LIVBOR(NPTFR)
      INTEGER :: OLD_LITBOR(NPTFR)
      DOUBLE PRECISION :: OLD_UBOR(NPTFR,2),OLD_VBOR(NPTFR,2)
      DOUBLE PRECISION :: OLD_HBOR(NPTFR)
      DOUBLE PRECISION :: OLD_CHBORD(NPTFR)
      DOUBLE PRECISION :: OLD_TBOR(NPTFR), OLD_ATBOR(NPTFR)
      DOUBLE PRECISION :: OLD_BTBOR(NPTFR)


      ! Check if the nbor of the original mesh has the same oredering as
      ! the one in the refine mesh that went through ranbo
      REORDER = .FALSE.
      DO I=1,NPTFR
        IF (OLD_NBOR(I) .NE. NBOR(2*(I-1)+1)) THEN
          REORDER = .TRUE.
          EXIT
        ENDIF
      ENDDO

      ALLOCATE(CONV(NPTFR))

      IF (REORDER) THEN
        DO I=1,NPTFR
          NODE1 = NBOR(2*I)
          NODE2 = NBOR(2*(I-1)+1)
          FOUND = .FALSE.
          DO J=1,NPTFR
            IF (OLD_NBOR(J).EQ.NODE1.OR.OLD_NBOR(J).EQ.NODE2) THEN
              FOUND = .TRUE.
              EXIT
            ENDIF
          ENDDO
          IF(.NOT.FOUND) THEN
            WRITE(LU,*) 'COULD NOT FIND', J, NODE1, NODE2
            CALL PLANTE(1)
          ENDIF
          CONV(I) = J
        ENDDO
      ELSE
        DO I=1,NPTFR
          CONV(I) = I
        ENDDO
      ENDIF

      LIHBOR(1:NPTFR*2:2)=LIHBOR(CONV)
      LIUBOR(1:NPTFR*2:2)=LIUBOR(CONV)
      LIVBOR(1:NPTFR*2:2)=LIVBOR(CONV)

      CHBORD(1:NPTFR*2:2)=CHBORD(CONV)
      HBOR(1:NPTFR*2:2)=HBOR(CONV)
      UBOR(1:NPTFR*2:2,1)=UBOR(CONV,1)
      VBOR(1:NPTFR*2:2,1)=VBOR(CONV,1)
      UBOR(1:NPTFR*2:2,2)=UBOR(CONV,2)
      VBOR(1:NPTFR*2:2,2)=VBOR(CONV,2)

      IF (NTRAC.GT.0) THEN
        LITBOR%ADR(1)%P%I(1:NPTFR*2:2)=LITBOR%ADR(1)%P%I(CONV)
        TBOR%ADR(1)%P%R(1:NPTFR*2:2)=TBOR%ADR(1)%P%R(CONV)
        ATBOR%ADR(1)%P%R(1:NPTFR*2:2)=ATBOR%ADR(1)%P%R(CONV)
        BTBOR%ADR(1)%P%R(1:NPTFR*2:2)=BTBOR%ADR(1)%P%R(CONV)
      ENDIF
!
      DEALLOCATE(CONV)
!
!
      ! Filling the point in the middle of each segment
      DO I=1,NPTFR

        P1 = 2*I-1
        ! Using kp1bor to identify the next point (not 2*i+1 for i=nptfr)
        P2 = KP1BOR(2*I)
!
        VAL_P1 = LIHBOR(P1)*1000 +
     &           LIUBOR(P1)*100  +
     &           LIVBOR(P1)*10
        IF(NTRAC.GT.0) THEN
          VAL_P1 = VAL_P1 + LITBOR%ADR(1)%P%I(P1)*1
        ENDIF

        VAL_P2 = LIHBOR(P2)*1000 +
     &           LIUBOR(P2)*100  +
     &           LIVBOR(P2)*10
        IF(NTRAC.GT.0) THEN
          VAL_P2 = VAL_P2 + LITBOR%ADR(1)%P%I(P2)*1
        ENDIF

        ! If same type on each point apply the same type
        IF (VAL_P1.EQ.VAL_P2) THEN
          IDX = P1
        ! If one of the points is a solid point taking that one
        ELSE IF(LIHBOR(P1).EQ.2) THEN
          IDX = P1
        ELSEIF(LIHBOR(P2).EQ.2) THEN
          IDX = P2
        ! Otherwise taking the smallest one
        ELSEIF(VAL_P1.LT.VAL_P2) THEN
          IDX = P1
        ELSE
          IDX = P2
        ENDIF

        LIHBOR(2*I)=LIHBOR(IDX)
        LIUBOR(2*I)=LIUBOR(IDX)
        LIVBOR(2*I)=LIVBOR(IDX)
        CHBORD(2*I)=CHBORD(IDX)
        HBOR(2*I)=HBOR(IDX)
        UBOR(2*I,1)=UBOR(IDX,1)
        VBOR(2*I,1)=VBOR(IDX,1)
        UBOR(2*I,2)=UBOR(IDX,2)
        VBOR(2*I,2)=VBOR(IDX,2)
!
        IF(NTRAC.GT.0) THEN
          LITBOR%ADR(1)%P%I(2*I)=LITBOR%ADR(1)%P%I(IDX)
          ATBOR%ADR(1)%P%R(2*I)=ATBOR%ADR(1)%P%R(IDX)
          BTBOR%ADR(1)%P%R(2*I)=BTBOR%ADR(1)%P%R(IDX)
        ENDIF
!
      ENDDO
!
      END SUBROUTINE
