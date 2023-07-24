!                   ****************
                    SUBROUTINE CHECK
!                   ****************
!
     &(IKLE2,NBOR,NELBOR,IKLBOR,NELEB,NELEBX,IKLE3,NELBO3,NULONE,
     & DIM1NUL,DIM2NUL,NBOR3,NELMAX2,NPTFR,NELMAX,NPTFR3,INFO)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CHECKS FOR COMMON ERRORS.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+        V5P1
!+   FORTRAN95 VERSION
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
!| IKLBOR         |-->| CONNECTIVITY TABLE OF BOUNDARY ELEMENTS
!| IKLE2          |-->| GLOBAL NUMBERS OF POINTS IN 2D ELEMENTS
!| IKLE3          |-->| GLOBAL NUMBERS OF POINTS IN 3D ELEMENTS
!| INFO           |-->| LISTING PRINTOUT OR NOT
!| NBOR           |-->| GLOBAL NUMBER OF 2D BOUNDARY POINTS
!| NBOR3          |-->| GLOBAL NUMBER OF 3D BOUNDARY POINTS
!| NELBO3         |-->| ASSOCIATION OF EACH BOUNDARY EDGE
!|                |   | TO THE CORRESPONDING 3D ELEMENT
!| NELBOR         |-->| NUMBER OF THE ADJACENT ELEMENT AT THE K TH
!|                |   | BOUNDARY SEGMENT
!| NELMAX2        |-->| MAXIMUM NUMBER OF ELEMENTS IN 2D MESH
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN 3D MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS IN 2D
!| NPTFR3         |-->| NUMBER OF BOUNDARY POINTS IN 3D
!| NULONE         |-->| GOES WITH ARRAY NELBOR. NELBOR GIVES THE
!|                |   | ADJACENT ELEMENT, NULONE GIVES THE LOCAL
!|                |   | NUMBER OF THE FIRST NODE OF THE BOUNDARY EDGE
!|                |   | I.E. 1, 2 OR 3 FOR TRIANGLES.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER, INTENT(IN) :: NELMAX2,NPTFR,NELMAX
      INTEGER, INTENT(IN) :: NPTFR3,NELEB,NELEBX,DIM1NUL,DIM2NUL
      INTEGER, INTENT(IN) :: NELBOR(NPTFR),NBOR(NPTFR)
      INTEGER, INTENT(IN) :: IKLE2(NELMAX2,3)
      INTEGER, INTENT(IN) :: IKLBOR(NELEBX,*),IKLE3(NELMAX,*)
      INTEGER, INTENT(IN) :: NELBO3(NELEBX)
      INTEGER, INTENT(IN) :: NULONE(DIM1NUL,DIM2NUL)
      INTEGER, INTENT(IN) :: NBOR3(NPTFR3)
      LOGICAL, INTENT(IN) :: INFO
!
!-----------------------------------------------------------------------
!
      INTEGER IERR,IEL,N1,N2,IPTFR,ILOC,IELEB
!
!***********************************************************************
!
!     INITIALISES FATAL ERROR COUNT
!
      IERR  = 0
!
!-----------------------------------------------------------------------
!
!     CHECKS 2D ARRAY NELBOR: GLOBAL POINT ON THE BOUNDARY MUST BE ONE
!                             POINT OF THE ELEMENT GIVEN BY NELBOR
!
      IF(NCSIZE.LE.1) THEN
!
        DO IPTFR = 1,NPTFR
          IEL = NELBOR(IPTFR)
          N1  = NBOR(IPTFR)
          IF (N1.NE.IKLE2(IEL,1).AND.N1.NE.IKLE2(IEL,2).AND.
     &        N1.NE.IKLE2(IEL,3)) THEN
            WRITE(LU,12) IEL,IPTFR
            IERR = IERR + 1
          ENDIF
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     CHECKS 3D ARRAYS IKLBOR,MESH3D%NELBOR=NELBO3 HERE,NULONE
!
      IF(NCSIZE.LE.1) THEN
!
        DO ILOC = 1,DIM2NUL
          DO IELEB=1,NELEB
              N1=NBOR3(IKLBOR(IELEB,ILOC))
              N2=IKLE3(NELBO3(IELEB),NULONE(IELEB,ILOC))
              IF (N1.NE.N2) THEN
                WRITE(LU,52) IELEB,ILOC,N1,N2
                IERR = IERR + 1
              ENDIF
          ENDDO
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
! PRINTS OUT THE RESULTS
!
      IF(IERR.EQ.0) THEN
        IF(INFO) WRITE(LU,112)
      ELSEIF(IERR.EQ.1) THEN
        WRITE(LU,122)
        CALL PLANTE(1)
        STOP
      ELSE
        WRITE(LU,132) IERR
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
12    FORMAT(' CHECK: ELEMENT',
     &   I6,' IS NOT ADJACENT TO BOUNDARY NODE',I5)
52    FORMAT(' CHECK: ERROR ON DATA STRUCTURE',/,
     &       'IELEB,ILOC,N1,N2 :',4I5)
112   FORMAT(' CHECK: NO ERROR HAS BEEN DETECTED',////)
122   FORMAT(' CHECK: 1 FATALE ERROR . BREAK IN THE PROGRAM',////)
132   FORMAT(' CHECK: ',I4,' FATALE ERRORS . BREAK IN THE PROGRAM',////)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
