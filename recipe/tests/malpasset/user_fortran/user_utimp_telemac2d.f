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
      INTEGER J,NRFO,INUM(14),I
      INTEGER IELEM,I1,I2,I3
!
      DOUBLE PRECISION XMES(14),YMES(14),HAUT(14),ARR_TIME(3),FON(14)
!
      DOUBLE PRECISION PERDUE,RELATI
      DOUBLE PRECISION X1,Y1,X2,Y2,X3,Y3
      DOUBLE PRECISION DETM
!
      LOGICAL FIND_ARR_TIME(3)
      LOGICAL DEJA_UTIMP
!
      DATA    DEJA_UTIMP /.FALSE./
!
      SAVE INUM,HAUT,ARR_TIME,FIND_ARR_TIME,DEJA_UTIMP, FON
      INTEGER ID
!
!-----------------------------------------------------------------------
!
      IF(.NOT.DEJA_UTIMP) THEN
!
        XMES( 1) = 5550.D0
        XMES( 2) = 11900.D0
        XMES( 3) = 13000.D0
        XMES( 4) = 11900.D0
        XMES( 5) = 13000.D0
        XMES( 6) = 4947.D0
        XMES( 7) = 5717.D0
        XMES( 8) = 6775.D0
        XMES( 9) = 7128.D0
        XMES(10) = 8585.D0
        XMES(11) = 9674.D0
        XMES(12) = 10939.D0
        XMES(13) = 11724.D0
        XMES(14) = 12723.D0
!
        YMES( 1) = 4400.D0
        YMES( 2) = 3250.D0
        YMES( 3) = 2700.D0
        YMES( 4) = 3250.D0
        YMES( 5) = 2700.D0
        YMES( 6) = 4289.D0
        YMES( 7) = 4407.D0
        YMES( 8) = 3869.D0
        YMES( 9) = 3162.D0
        YMES(10) = 3443.D0
        YMES(11) = 3085.D0
        YMES(12) = 3044.D0
        YMES(13) = 2810.D0
        YMES(14) = 2485.D0
!
        DO J=1,14
          INUM(J) = 0
          HAUT(J) = 0.D0
          FON(J) = 0.0D0
        ENDDO
!
        CALL PROXIM(INUM,XMES,YMES,X,Y,14,NPOIN,IKLE%I,NELEM,NELEM)
!
        DO J=1,3
          FIND_ARR_TIME(J) = .FALSE.
          ARR_TIME(J)      = 0.D0
        ENDDO
!
        DEJA_UTIMP = .TRUE.
!
      ENDIF
!
!-----------------------------------------------------------------------

      DO J=1,14
        IF(INUM(J).NE.0) THEN
          HAUT(J) = MAX(H%R(INUM(J)),HAUT(J))
          FON(J) = ZF%R(INUM(J))
        ELSE
          HAUT(J) = 0.D0
          FON(J) = 0.D0
        ENDIF
      ENDDO
!
      DO J=1,3
        IF(INUM(J).NE.0) THEN
          IF(H%R(INUM(J)).GT.1.D-4.AND..NOT.FIND_ARR_TIME(J)) THEN
            ARR_TIME(J) = AT
            FIND_ARR_TIME(J) = .TRUE.
          ENDIF
        ELSE
          ARR_TIME(J) = 0.D0
          FIND_ARR_TIME(J) = .FALSE.
        ENDIF
      ENDDO
!
      IF(NCSIZE.GT.1) THEN
        DO J=1,14
          HAUT(J) = P_MAX(HAUT(J))+P_MIN(HAUT(J))
          FON(J) = P_MAX(FON(J))+P_MIN(FON(J))
        ENDDO
        DO J=1,3
          ARR_TIME(J) = P_MAX(ARR_TIME(J))+P_MIN(ARR_TIME(J))
        ENDDO
      ENDIF
!
      IF(IPID.EQ.0) THEN
        IF(LT.EQ.1) THEN
          NRFO = T2D_FILES(T2DRFO)%LU
          WRITE(NRFO,*) "VOLUMES"
          WRITE(NRFO,1003) 'TIME',INT(0),' S = ',MASSE0,' M3'
        ENDIF
        IF(MOD(LT,LEOPRD).EQ.0) THEN
          NRFO = T2D_FILES(T2DRFO)%LU
          IF(LT.NE.0) THEN
              WRITE(NRFO,1003) 'TIME',INT(AT),' S = ',MASSE2,' M3'
          ENDIF
        ENDIF
        IF(LT.EQ.NIT) THEN
          NRFO = T2D_FILES(T2DRFO)%LU
          PERDUE = MASSE0+MASSET+MASENT-MASSE2
          WRITE(NRFO,*) "VOLUME LOST = ", PERDUE
          RELATI = PERDUE/(MASSE0+MASSET+MASENT)
          WRITE(NRFO,*) "RELATIVE ERROR = ", RELATI
        ENDIF
!
        IF(LT.EQ.NIT) THEN
          NRFO = T2D_FILES(T2DRFO)%LU
          WRITE(NRFO,*)
          WRITE(NRFO,*) 'MAXIMUM WATER DEPTHS'
          DO J=6,14
            WRITE(NRFO,1001) 'MEASUREMENT POINT',J,' = ',
     &                       HAUT(J)+FON(J),' M'
          ENDDO
!
          WRITE(NRFO,*)
          WRITE(NRFO,1002)'ARRIVAL TIME AT A :',ARR_TIME(1),' S'
          WRITE(NRFO,1002)'TIME FROM A TO B  :',ARR_TIME(2)-ARR_TIME(1),
     &                    ' S'
          WRITE(NRFO,1002)'TIME FROM A TO C  :',ARR_TIME(3)-ARR_TIME(1),
     &                    ' S'
        ENDIF
      ENDIF

      IF(LT.EQ.NIT) THEN
!     MASS FILE
        !MASS MATRIX CALCULATION
        DO I=1,MESH%NPOIN
          MESH%M%D%R(I)=0.D0
        ENDDO
        DO IELEM=1,MESH%NELEM
          I1 = MESH%IKLE%I(              IELEM)
          I2 = MESH%IKLE%I(  MESH%NELMAX+IELEM)
          I3 = MESH%IKLE%I(2*MESH%NELMAX+IELEM)
!     GET THE COORDINATES OF EACH POINT OF THE COARSE ELEMENT
          X1 = MESH%X%R(I1)
          X2 = MESH%X%R(I2)
          X3 = MESH%X%R(I3)
          Y1 = MESH%Y%R(I1)
          Y2 = MESH%Y%R(I2)
          Y3 = MESH%Y%R(I3)
          DETM  = ((X2-X1)*(Y3-Y1)-(X3-X1)*(Y2 -Y1))/3.D0
          MESH%M%D%R(I1)=MESH%M%D%R(I1)+DETM
          MESH%M%D%R(I2)=MESH%M%D%R(I2)+DETM
          MESH%M%D%R(I3)=MESH%M%D%R(I3)+DETM
        ENDDO
        IF(NCSIZE.LE.1) THEN
          ID = T2D_FILES(T2DRF1)%LU
          DO I=1,MESH%NPOIN
            WRITE(ID,*) MESH%M%D%R(I)
          ENDDO
        ENDIF
      ENDIF

!
 1001 FORMAT((A,I3,A,F6.2,A))
 1002 FORMAT((A,F7.1,A))
 1003 FORMAT((A,I5,A,E25.17,A))
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
