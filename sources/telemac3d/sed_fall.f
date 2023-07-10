!                   *******************
                    SUBROUTINE SED_FALL
!                   *******************
!
     &(FC,FN,WCHU,MESH3D,DT,VOLU,NPOIN2,NPOIN3,NPLAN,T1)
!
!***********************************************************************
! TELEMAC3D   V7P0                                    04/06/2014
!***********************************************************************
!
!brief    Advection of sediment with settling velocity, with a method
!+        inspired from the weak form of characteristics. The mass of
!+        sediment carried by a point is just shifted below with the
!+        settling velocity and put on nearby points. At the end it
!+        is translated into local concentration.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        29/04/2014
!+        V7P0
!+   First version.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        04/06/2014
!+        V7P0
!+   Avoiding divisions by 0 on tidal flats (VOLU limited by 1.D-6).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME STEP
!| FC             |<->| VARIABLE AFTER CONVECTION
!| FN             |<->| VARIABLE F AFTER ADVECTION WITH VELOCITY BUT WC
!| MESH3D         |<->| 3D MESH
!| NPLAN          |-->| NUMBER OF PLANES IN THE 2D MESH
!| NPOIN2         |-->| NUMBER OF POINTS IN THE 2D MESH
!| NPOIN3         |-->| NUMBER OF POINTS IN THE 3D MESH
!| T1             |<->| WORK ARRAY IN BIEF_OBJ
!| VOLU           |-->| VOLUME AROUND POINTS AT TIME N+1
!| WCHU            |-->| VELOCITY (POSITIVE IF SEDIMENT SETTLING VELOCITY)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NPOIN2,NPOIN3,NPLAN
      DOUBLE PRECISION, INTENT(IN)    :: DT
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: FN,WCHU,VOLU
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: FC,T1
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH3D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I2D,IPLAN,I3D,IINF,ISUP
      DOUBLE PRECISION ZDEP,ZARR,ZBOT,ZINF,ZSUP,ALFA
!
!-----------------------------------------------------------------------
!
!     INITIALISING FC (IF WILL BE THE FINAL MASS CARRIED BY POINTS)
!
      CALL OS('X=0     ',X=FC)
!
!     SEDIMENT ON BOTTOM PLANE WILL NOT MOVE
!
      DO I2D=1,NPOIN2
        FC%R(I2D)=FC%R(I2D)+FN%R(I2D)*VOLU%R(I2D)
      ENDDO
!
!-----------------------------------------------------------------------
!
!     LOOP ON ALL THE VERTICALS, MASSES SHIFTED DOWN FOR EVERY POINT
!
      DO I2D=1,NPOIN2
!       BOTTOM TREATED ABOVE, SO LOOP STARTS AT 2
        DO IPLAN=2,NPLAN
!         3D POINT NUMBER
          I3D=I2D+(IPLAN-1)*NPOIN2
!         BOTTOM
          ZBOT=MESH3D%Z%R(I2D)
!         DEPARTURE POINT
          ZDEP=MESH3D%Z%R(I3D)
!         ARRIVAL POINT (WCHU>0)
          ZARR=MAX(ZDEP-WCHU%R(I3D)*DT,ZBOT)
!         LOCATING THE ARRIVAL POINT
          IINF=IPLAN-1
          ISUP=IPLAN
1         CONTINUE
          ZINF=MESH3D%Z%R(I2D+(IINF-1)*NPOIN2)
          ZSUP=MESH3D%Z%R(I2D+(ISUP-1)*NPOIN2)
          IF(ZARR.LT.ZINF) THEN
            IINF=IINF-1
            ISUP=ISUP-1
            GO TO 1
          ENDIF
!         PROJECTION OF THE ORIGINAL MASS ON THE TWO POINTS
          ALFA=(ZARR-ZINF)/MAX(ZSUP-ZINF,1.D-7)
!         MASS ON IINF
          FC%R(I2D+(IINF-1)*NPOIN2)
     &   =FC%R(I2D+(IINF-1)*NPOIN2)+(1.D0-ALFA)*FN%R(I3D)*VOLU%R(I3D)
!         MASS ON ISUP
          FC%R(I2D+(ISUP-1)*NPOIN2)
     &   =FC%R(I2D+(ISUP-1)*NPOIN2)+      ALFA *FN%R(I3D)*VOLU%R(I3D)
        ENDDO
      ENDDO
!
!     INVERSION OF THE MASS MATRIX (HERE LUMPED) TO GO BACK FROM MASSES
!     TO CONCENTRATIONS
!
      CALL OS('X=Y     ',X=T1,Y=VOLU)
!
!     ASSEMBLING IN PARALLEL
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(FC,2,MESH3D)
        CALL PARCOM(T1,2,MESH3D)
      ENDIF
!
!     INVERTING THE (LUMPED) MASS MATRIX (I.E. VOLU)
!
      DO I3D=1,NPOIN3
        FC%R(I3D)=FC%R(I3D)/MAX(T1%R(I3D),1.D-6)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

