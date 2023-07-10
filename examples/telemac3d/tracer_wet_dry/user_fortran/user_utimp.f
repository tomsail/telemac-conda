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
      INTRINSIC MOD
!
      INTEGER NRFO
      DOUBLE PRECISION PERDUE,RELATI,PERDUE_T,MASS_T_0
!
      INTEGER I,IELEM,I1,I2,I3
!
      DOUBLE PRECISION X1_2D,Y1_2D,X2_2D,Y2_2D,X3_2D,Y3_2D
      DOUBLE PRECISION DETM

      INTEGER ID

      SAVE MASS_T_0
!
!-----------------------------------------------------------------------
!
      IF(LT.EQ.1) THEN
        NRFO = T3D_FILES(T3DRFO)%LU
        WRITE(NRFO,*) "VOLUMES"
        WRITE(NRFO,1003) 'TIME',INT(0),'  CREATED ',
     &        DT*FLUXTOTCUM,' S = ',MASINI_WATER,' M3'
      ENDIF
      IF(MOD(LT,GRAPRD).EQ.0) THEN
        NRFO = T3D_FILES(T3DRFO)%LU
        IF(LT.NE.0) THEN
          WRITE(NRFO,1003) 'TIME',INT(AT),'  CREATED ',
     &          DT*FLUXTOTCUM,' S = ',MASSE_WATER,' M3'
        ENDIF
      ENDIF
      IF(LT.EQ.NIT) THEN
        NRFO = T3D_FILES(T3DRFO)%LU
        PERDUE = MASINI_WATER-MASSE_WATER-DT*FLUXTOTCUM
        WRITE(NRFO,*) "VOLUME LOST = ", PERDUE
        RELATI = PERDUE/(MASINI_WATER)
        WRITE(NRFO,*) "RELATIVE ERROR = ", RELATI
      ENDIF

      IF(IPID.EQ.0) THEN
        IF(LT.EQ.1) THEN
          ID = T3D_FILES(T3DRF1)%LU
          WRITE(ID,*) 'MASS'
          MASS_T_0 = MASSE%R(6) ! NOT MASINI%R(6)=0...
          WRITE(ID,1003) 'TIME',INT(0),'  CREATED ',
     &          DT*FLUCUM%R(6),' S = ',MASS_T_0,' M3'
        ELSEIF(MOD(LT,GRAPRD).EQ.0) THEN
          ID = T3D_FILES(T3DRF1)%LU
          WRITE(ID,1003) 'TIME',INT(AT),'  CREATED ',
     &          DT*FLUCUM%R(6),' S = ',MASSE%R(6),' M3'
        ENDIF
        IF(LT.EQ.NIT) THEN
          ID = T3D_FILES(T3DRF1)%LU
          PERDUE_T = MASS_T_0-MASSE%R(6)-DT*FLUCUM%R(6)
          WRITE(ID,*) "MASS LOST = ", PERDUE_T
          !RELATI_T = PERDUE_T/(MASS_T_0)
          !WRITE(1846986,*) "RELATIVE ERROR = ", RELATI_T
        ENDIF
      ENDIF

      IF(LT.EQ.NIT) THEN
        IF(IPID.EQ.0) THEN
!     MASS FILE
          ID = T3D_FILES(T3DRF2)%LU
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
     &               (X3_2D-X1_2D)*(Y2_2D -Y1_2D))/3.D0
            MESH2D%M%D%R(I1)=MESH2D%M%D%R(I1)+DETM
            MESH2D%M%D%R(I2)=MESH2D%M%D%R(I2)+DETM
            MESH2D%M%D%R(I3)=MESH2D%M%D%R(I3)+DETM
          ENDDO
          DO I=1,MESH2D%NPOIN
            WRITE(ID,*) MESH2D%M%D%R(I)
          ENDDO
        ENDIF
      ENDIF
!
 1003 FORMAT((A,I7,A,E25.17,A,E25.17,A))
!
!-----------------------------------------------------------------------
!
      RETURN
      END
