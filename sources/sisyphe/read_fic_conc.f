!                   ************************
                    SUBROUTINE READ_FIC_CONC
!                   ************************
!
     &(CGL , WHAT , AT , NFIC , FOUND )
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    READS AND INTERPOLATES VALUES FROM THE LIQUID BOUNDARY FILE.
!
!history  J-M HERVOUET (LNHE)
!+        10/08/2009
!+        V6P0
!+
!
!history  J-M HERVOUET (LNHE)
!+        28/06/2010
!+        V6P0
!+   SIZE OF LIGN PARAMETERIZED (SEE SIZELIGN)
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
!| AT             |-->| TIME_RFC IN SECONDS
!| NFIC           |-->| LOGICAL UNIT OF FILE
!| CGL            |<--| VARIABLE READ AND INTERPOLATED
!| FOUND          |<--| IF FALSE: VARIABLE NOT FOUND
!| WHAT           |-->| VARIABLE TO LOOK FOR IN 8 CHARACTERS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SISYPHE, ONLY : DEJA_RFC,INFIC_RFC,TIME_RFC,
     &                                 CHOIX_RFC,IL1_RFC,IL2_RFC,
     &                                 TL1_RFC,TL2_RFC,NVALUE_RFC,
     &                                 LASTWHAT_RFC,LASTAT_RFC,NLIG_RFC,
     &                                 MAXVAL_RFC
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8)     , INTENT(IN)       :: WHAT
      DOUBLE PRECISION, INTENT(IN)       :: AT
      DOUBLE PRECISION, INTENT(INOUT)    :: CGL
      INTEGER         , INTENT(IN)       :: NFIC
      LOGICAL         , INTENT(OUT)      :: FOUND
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     MAXIMUM NUMBER OF CHARACTERS PER LIGN (MAY BE CHANGED)
!
      INTEGER, PARAMETER :: SIZELIGN = 3000
!
      INTEGER IVALUE,ILIG,OK,J,IWHAT,IDEB,IFIN
      DOUBLE PRECISION TETA
      DOUBLE PRECISION, PARAMETER :: TOL = 1.D-3
!
      CHARACTER(LEN=SIZELIGN) :: LIGNE
!
!-----------------------------------------------------------------------
!
!     1) (AT FIRST CALL)
!        READS THE LIQUID BOUNDARY FILE
!        INITIALISES CURRENT LINES AND INTERVAL OF TIME_RFC
!
      IF(.NOT.DEJA_RFC) THEN
        REWIND(NFIC)
!       SKIPS COMMENTS
1       READ(NFIC,FMT='(A)',ERR=10) LIGNE
        GO TO 20
10      CONTINUE
        WRITE(LU,*) 'READ ERROR IN THE'
        WRITE(LU,*) 'LIQUID BOUNDARIES FILE FOR CONC'
        WRITE(LU,*) 'PROBABLY A PROBLEM OF FORMAT'
        WRITE(LU,*) 'ANY WINDOWS CARRIAGE RETURNS ON UNIX OR LINUX'
        WRITE(LU,*) 'GUILTY LINE:'
        WRITE(LU,*) LIGNE
        CALL PLANTE(1)
        STOP
20      CONTINUE
        IF(LIGNE(1:1).EQ.'#') GO TO 1
!
!       FINDS OUT WHAT AND HOW MANY VALUES ARE GIVEN IN THE FILE
!
        NVALUE_RFC = -1
        IFIN = 1
40      IDEB = IFIN
!
!       IDENTIFIES FIRST CHARACTER OF NAME
50      IF(LIGNE(IDEB:IDEB).EQ.' '.AND.IDEB.LT.SIZELIGN) THEN
          IDEB=IDEB+1
          GO TO 50
        ENDIF
!       IDENTIFIES LAST CHARACTER OF NAME
        IFIN = IDEB
60      IF(LIGNE(IFIN:IFIN).NE.' '.AND.IFIN.LT.SIZELIGN) THEN
          IFIN=IFIN+1
          GO TO 60
        ENDIF
!
        IF(IDEB.EQ.IFIN) GO TO 4
!
        NVALUE_RFC = NVALUE_RFC + 1
        IF(NVALUE_RFC.EQ.0) THEN
          IF(LIGNE(IDEB:IFIN-1).NE.'T') THEN
          WRITE(LU,*) 'FIRST VALUE MUST BE TIME_RFC, DENOTED T'
          WRITE(LU,*) 'IN FILE OF LIQUID BOUNDARIES'
          CALL PLANTE(1)
          STOP
          ENDIF
        ELSEIF(NVALUE_RFC.LE.MAXVAL_RFC) THEN
          CHOIX_RFC(NVALUE_RFC)='        '
          CHOIX_RFC(NVALUE_RFC)(1:IFIN-IDEB+1)=LIGNE(IDEB:IFIN-1)
        ELSE
          WRITE(LU,*) 'INCREASE MAXVAL_RFC IN DECLARATIONS_SISYPHE'
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(IFIN.LT.SIZELIGN) GO TO 40
!
!       SKIPS THE LINE WITH UNITS OR NAMES
4       READ(NFIC,FMT='(A)',ERR=10) LIGNE
        IF(LIGNE(1:1).EQ.'#') GO TO 4
!
!       COUNTS LINES OF DATA
        NLIG_RFC = 0
998     READ(NFIC,*,END=1000,ERR=999) LIGNE
        IF(LIGNE(1:1).NE.'#') NLIG_RFC=NLIG_RFC+1
        GO TO 998
999     CONTINUE
        WRITE(LU,*) 'READING ERROR ON THE LIQUID BOUNDARIES FILE'
        WRITE(LU,*) 'AT LINE OF DATA : ',NLIG_RFC
        WRITE(LU,*) '(COMMENTS EXCLUDED)'
        CALL PLANTE(1)
        STOP
1000    CONTINUE
!
!       DYNAMICALLY ALLOCATES TIME_RFC AND INFIC_RFC
!
        ALLOCATE(TIME_RFC(NLIG_RFC),STAT=OK)
        IF(OK.NE.0) WRITE(LU,*) 'MEMORY ALLOCATION ERROR FOR TIME_RFC'
        ALLOCATE(INFIC_RFC(NVALUE_RFC,NLIG_RFC),STAT=OK)
        IF(OK.NE.0) WRITE(LU,*) 'MEMORY ALLOCATION ERROR FOR INFIC_RFC'
!
!       FINAL READ OF TIME_RFC AND INFIC_RFC
!
        REWIND(NFIC)
!       SKIPS COMMENTS AND FIRST TWO MANDATORY LINES
2       READ(NFIC,FMT='(A)') LIGNE
        IF(LIGNE(1:1).EQ.'#') GO TO 2
        READ(NFIC,FMT='(A)') LIGNE
!
        DO ILIG=1,NLIG_RFC
3         READ(NFIC,FMT='(A)') LIGNE
          IF(LIGNE(1:1).EQ.'#') THEN
            GO TO 3
          ELSE
            BACKSPACE(NFIC)
            READ(NFIC,*) TIME_RFC(ILIG),
     &                  (INFIC_RFC(IVALUE,ILIG),IVALUE=1,NVALUE_RFC)
          ENDIF
        ENDDO
!
        CLOSE(NFIC)
        DEJA_RFC = .TRUE.
!
        IL1_RFC = 1
        IL2_RFC = 2
        TL1_RFC = TIME_RFC(1)
        TL2_RFC = TIME_RFC(2)
!
        WRITE(LU,*) 'THE LIQUID BOUNDARIES FILE CONTAINS'
        WRITE(LU,*) NLIG_RFC,' LINES WITH:'
        WRITE(LU,*) (CHOIX_RFC(IVALUE),IVALUE=1,NVALUE_RFC)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     2) INTERPOLATES THE DATA TO GET THE CORRECT TIME_RFC
!
!     2.A) FINDS THE ADDRESS IN THE ARRAY OF STORED DATA
!
!     2.B) INTERPOLATES DATA FROM THE ARRAY INFIC_RFC
!
!-----------------------------------------------------------------------
!
!
!     WHICH VARIABLE ?
      IWHAT = 0
      DO J=1,NVALUE_RFC
        IF(WHAT.EQ.CHOIX_RFC(J)) IWHAT=J
      ENDDO
      IF(IWHAT.EQ.0) THEN
        FOUND=.FALSE.
        RETURN
      ENDIF
!
!
!
70    IF(AT.GE.TL1_RFC-TOL.AND.AT.LE.TL2_RFC+TOL) THEN
        TETA = (AT-TL1_RFC)/(TL2_RFC-TL1_RFC)
      ELSE
        DO J=1,NLIG_RFC-1
          IF(AT.GE.TIME_RFC(J)-TOL.AND.AT.LE.TIME_RFC(J+1)+TOL) THEN
            TL1_RFC=TIME_RFC(J)
            TL2_RFC=TIME_RFC(J+1)
            IL1_RFC=J
            IL2_RFC=J+1
            GO TO 70
          ENDIF
        ENDDO
        IL1_RFC=IL2_RFC
        IL2_RFC=IL2_RFC+1
        IF(IL2_RFC.GT.NLIG_RFC) THEN
          WRITE(LU,*) 'T=',AT,' OUT OF RANGE'
          WRITE(LU,*) 'OF THE FILE OF LIQUID BOUNDARIES'
          CALL PLANTE(1)
          STOP
        ENDIF
        TL1_RFC=TIME_RFC(IL1_RFC)
        TL2_RFC=TIME_RFC(IL2_RFC)

        GO TO 70
      ENDIF
!
      CGL = (1.D0-TETA)*INFIC_RFC(IWHAT,IL1_RFC)
     &  +       TETA *INFIC_RFC(IWHAT,IL2_RFC)
!
      FOUND=.TRUE.

      LASTAT_RFC=AT
      LASTWHAT_RFC=WHAT
!
!-----------------------------------------------------------------------
!
      RETURN
      END
