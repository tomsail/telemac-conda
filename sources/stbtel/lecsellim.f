!                   *****************
                    SUBROUTINE LECSELLIM
!                   *****************
!
     &(NLIM,LIHBOR,LIUBOR,LIVBOR,HBOR,UBOR,VBOR,
     & CHBORD,NBOR,NPMAX,NPTFR,NCOLOR)
!
!***********************************************************************
! STBTEL
!***********************************************************************
!
!brief    READS THE BOUNDARY CONDITIONS FILE AND
!+                STORES IN ARRAYS THE DATA READ.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NLIM           |-->| LOGICAL UNIT OF BOUNDARY CONDITIONS FILE
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON U
!| LIVBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON V
!| HBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON DEPTH
!| UBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY U
!| VBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY V
!| CHBORD         |<--| FRICTION COEFFICIENT AT BOUNDARY
!| NBORD          |<--| BOUNDARY NUMBERING
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_STBTEL, ONLY: FFORMAT, TYP_BND_ELEM
      USE INTERFACE_HERMES
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NLIM
      INTEGER, INTENT(IN)    :: NPMAX
      INTEGER, INTENT(INOUT) :: LIUBOR(NPMAX),LIVBOR(NPMAX)
      INTEGER, INTENT(INOUT) :: LIHBOR(NPMAX)
      INTEGER, INTENT(INOUT) :: NBOR(NPMAX)
      INTEGER, INTENT(OUT) :: NPTFR
      DOUBLE PRECISION,  INTENT(INOUT) :: UBOR(NPMAX,2),VBOR(NPMAX,2)
      DOUBLE PRECISION,  INTENT(INOUT) :: HBOR(NPMAX),CHBORD(NPMAX)
      INTEGER, INTENT(INOUT) :: NCOLOR(NPMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: I, IERR
!
      CALL GET_BND_NPOIN(FFORMAT,NLIM,TYP_BND_ELEM,NPTFR,IERR)

      CALL GET_BND_VALUE(FFORMAT, NLIM, TYP_BND_ELEM, NPTFR, LIHBOR,
     &                   LIUBOR, LIVBOR, HBOR, UBOR(:,1), VBOR(:,1),
     &                   CHBORD, .FALSE., LIHBOR, HBOR, HBOR, HBOR,
     &                   NPTFR, IERR)
      WRITE(LU,*) 'NPTFR FROM LECSELLIM ', NPTFR
      CALL CHECK_CALL(IERR, 'LECSELIM:GET_BND_VALUE')

      CALL GET_BND_NUMBERING(FFORMAT, NLIM, TYP_BND_ELEM, NPTFR,
     &                       NBOR, IERR)
      CALL CHECK_CALL(IERR, 'LECSELIM:GET_BND_NUMBERING')

      DO I=1,NPTFR
        NCOLOR(I) = I
      ENDDO
      UBOR(:,2) = UBOR(:, 1)
      VBOR(:,2) = VBOR(:, 1)

      END SUBROUTINE LECSELLIM
