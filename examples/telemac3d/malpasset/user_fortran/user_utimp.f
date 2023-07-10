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
      USE INTERFACE_PARALLEL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: GRADEBL,GRAPRDL,LISDEBL,LISPRDL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER J,NRFO,INUM(14), FID
!
      DOUBLE PRECISION XMES(14),YMES(14),HAUT(14),ARR_TIME(3),FON(14)
!
      DOUBLE PRECISION LOST,RELATI
      LOGICAL FIND_ARR_TIME(3)
      LOGICAL DEJA_UTIMP
!
      DATA    DEJA_UTIMP /.FALSE./
!
      INTRINSIC MOD
!
      SAVE INUM,HAUT,ARR_TIME,FIND_ARR_TIME,DEJA_UTIMP,FON
!
      INTEGER I,IELEM,I1,I2,I3
      DOUBLE PRECISION X1_2D,Y1_2D,X2_2D,Y2_2D,X3_2D,Y3_2D
      DOUBLE PRECISION DETM
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
          FON(J)  = 0.0D0
        ENDDO
!
        CALL PROXIM(INUM,XMES,YMES,X2%R,Y2%R,14,NPOIN2,IKLE2%I,NELEM2,
     &              NELEM2)
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
      DO J=1,14
        IF(INUM(J).NE.0) THEN
          HAUT(J) = MAX(H%R(INUM(J)),HAUT(J))
          FON(J)  = ZF%R(INUM(J))
        ELSE
          HAUT(J) = 0.D0
          FON(J)  = 0.D0
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
          FON(J)  = P_MAX(FON(J))+P_MIN(FON(J))
        ENDDO
        DO J=1,3
          ARR_TIME(J) = P_MAX(ARR_TIME(J))+P_MIN(ARR_TIME(J))
        ENDDO
      ENDIF
!
      IF(LT.EQ.1) THEN
        NRFO = T3D_FILES(T3DRFO)%LU
        WRITE(NRFO,*) "VOLUMES"
        WRITE(NRFO,1003) 'TIME',INT(0),' S = ',MASINI_WATER,' M3'
      ENDIF
      IF(MOD(LT,GRAPRD).EQ.0) THEN
        NRFO = T3D_FILES(T3DRFO)%LU
        IF(LT.NE.0) THEN
          WRITE(NRFO,1003) 'TIME',INT(AT),' S = ',MASSE_WATER,' M3'
        ENDIF
      ENDIF
      IF(LT.EQ.NIT) THEN
        NRFO = T3D_FILES(T3DRFO)%LU
        LOST = MASINI_WATER-MASSE_WATER-DT*FLUXTOTCUM
        WRITE(NRFO,*) "VOLUME LOST = ", LOST
        RELATI = LOST/(MASINI_WATER)
        WRITE(NRFO,*) "RELATIVE ERROR = ", RELATI
      ENDIF

      IF(LT.EQ.NIT) THEN
        NRFO = T3D_FILES(T3DRFO)%LU
        WRITE(NRFO,*)
        WRITE(NRFO,*) 'MAXIMUM WATER DEPTHS'
        DO J=6,14
          WRITE(NRFO,1001) 'MEASUREMENT POINT',J,' = ',HAUT(J),' M'
        ENDDO
!
        WRITE(NRFO,*)
        WRITE(NRFO,1002)'ARRIVAL TIME AT A :',ARR_TIME(1),' S'
        WRITE(NRFO,1002)'TIME FROM A TO B  :',ARR_TIME(2)-ARR_TIME(1),
     &                  ' S'
        WRITE(NRFO,1002)'TIME FROM A TO C  :',ARR_TIME(3)-ARR_TIME(1),
     &                  ' S'
      ENDIF

      IF(LT.EQ.NIT) THEN
!     MASS FILE
        !MASS MATRIX CALCULATION
        DO I=1,MESH2D%NPOIN
          MESH2D%M%D%R(I)=0.D0
        ENDDO
        DO IELEM=1,MESH2D%NELEM
          I1 = MESH2D%IKLE%I(                IELEM)
          I2 = MESH2D%IKLE%I(  MESH2D%NELMAX+IELEM)
          I3 = MESH2D%IKLE%I(2*MESH2D%NELMAX+IELEM)
!         GET THE COORDINATES OF EACH POINT OF THE COARSE ELEMENT
          X1_2D = MESH2D%X%R(I1)
          X2_2D = MESH2D%X%R(I2)
          X3_2D = MESH2D%X%R(I3)
          Y1_2D = MESH2D%Y%R(I1)
          Y2_2D = MESH2D%Y%R(I2)
          Y3_2D = MESH2D%Y%R(I3)
          DETM  = ((X2_2D-X1_2D)*(Y3_2D-Y1_2D)-
     &             (X3_2D-X1_2D)*(Y2_2D -Y1_2D))/3.D0
          MESH2D%M%D%R(I1)=MESH2D%M%D%R(I1)+DETM
          MESH2D%M%D%R(I2)=MESH2D%M%D%R(I2)+DETM
          MESH2D%M%D%R(I3)=MESH2D%M%D%R(I3)+DETM
        ENDDO
        IF(NCSIZE.LE.1) THEN
          FID = T3D_FILES(T3DRF1)%LU
          DO I=1,MESH2D%NPOIN
            WRITE(FID,*) MESH2D%M%D%R(I)
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
      END
