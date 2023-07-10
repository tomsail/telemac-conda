!                   ************************************
                    SUBROUTINE BYPASS_CRUSHED_POINTS_SEG
!                   ************************************
!
     &(SVOLU,SVOLUN,FLUX,TRA01,MESH3,
     & SCHCF,NPOIN2,GLOSEG,DIMGLO,NSEG,NPLAN)
!
!***********************************************************************
! TELEMAC3D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    BYPASSES FLUXES TO POINTS THAT WILL REMAIN WITH
!+                A ZERO VOLUME.
!+
!+            FLUX IS CONVEYED TO UPPER LAYER THROUGH A VERTICAL.
!+
!+            THIS AVOIDS USELESS ITERATIONS.
!
!warning  HERE FLUXES ARE FROM POINT 2 TO POINT 1.
!+            SEE FLUX3D (HORIZONTAL FLUXES BASED ON FLUINT)
!+            AND PRECON (VERTICAL FLUXES BASED ON WSCONV)
!warning  HERE EDGE-BASED STORAGE
!
!history  J-M HERVOUET (LNHE)
!+        20/04/2010
!+        V6P0
!+
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
!history  J-M HERVOUET (LNHE)
!+        28/12/2011
!+        V6P2
!+   Adaptation to prisms cut into tetrahedra
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DIMGLO         |-->| FIRST DIMENSION OF GLOSEG
!| FLUX           |<->| FLUXES TO BE CHANGED
!| GLOSEG         |-->| 3D LIST OF SEGMENTS POINTS
!| MESH3          |<->| 3D MESH
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NSEG           |-->| NUMBER OF SEGMENTS IN 2D
!| SCHCF          |-->| ADVECTION SCHEME
!| SVOLU          |-->| BIEF_OBJ STRUCTURE, WITH SVOLU%R=VOLU
!| SVOLUN         |-->| BIEF_OBJ STRUCTURE, WITH SVOLUN%R=VOLUN
!| TRA01          |<->| WORK BIEF_OBJ STRUCTURE
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
      INTEGER, INTENT(IN)             :: SCHCF,NPOIN2
      INTEGER, INTENT(IN)             :: NSEG,NPLAN,DIMGLO
      INTEGER, INTENT(IN)             :: GLOSEG(DIMGLO,2)
!
      TYPE(BIEF_OBJ), INTENT(IN)      :: SVOLU,SVOLUN
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TRA01
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH3
!
      DOUBLE PRECISION, INTENT(INOUT) :: FLUX(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NSEGH,NSEGV,IHOR
      INTEGER I1,I2,I3,I4,IPLAN,ISEG2D,ISEG3D,ICR1,ICR2,IS1
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION, PARAMETER :: EPS_VOLUME = 1.D-14
!
!-----------------------------------------------------------------------
!
      NSEGH=NSEG*NPLAN
      NSEGV=NPOIN2*(NPLAN-1)
!
!***********************************************************************
!
!     TRA01=VOLU+VOLUN=0 MEANS THAT BOTH VOLU AND VOLUN ARE EQUAL TO 0
!
      CALL OS('X=Y+Z   ',X=TRA01,Y=SVOLU,Z=SVOLUN)
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(TRA01,2,MESH3)
      ENDIF
!
!     SEE STOSEG41.F FOR NUMBERING OF CROSSED SEGMENTS
!
      IF(SCHCF.EQ.ADV_NSC_TF) THEN
!
        IF(MESH3%TYPELM.EQ.40) THEN
!
!       PRISMS
!       CASE WITH BOTH HORIZONTAL AND CROSSED FLUXES (CASE OF N-SCHEME)
!
        DO ISEG2D=1,NSEG
          DO IPLAN=1,NPLAN-1
!           CROSSED SEGMENT EXITING FROM POINT 1 OF HORIZONTAL SEGMENT
            ICR1=NSEGH+NSEGV+2*(IPLAN-1)*NSEG+ISEG2D
!           CROSSED SEGMENT EXITING FROM POINT 2 OF HORIZONTAL SEGMENT
            ICR2=ICR1+NSEG
!           HORIZONTAL SEGMENT
            ISEG3D=ISEG2D+(IPLAN-1)*NSEG
            I1=GLOSEG(ISEG3D,1)
            I2=GLOSEG(ISEG3D,2)
            I3=GLOSEG(ICR1,2)
            I4=GLOSEG(ICR2,2)
!           FIRST CROSSED SEGMENT BYPASSED
            IF(TRA01%R(I1).LT.EPS_VOLUME.OR.
     &         TRA01%R(I3).LT.EPS_VOLUME) THEN
!             FLUX ADDED TO UPPER LAYER WITH SAME ORIENTATION
              IHOR=ISEG3D+NSEG
              FLUX(IHOR)=FLUX(IHOR)+FLUX(ICR1)
!             FLUX ADDED TO VERTICAL OF I1
              FLUX(NSEGH+I1)=FLUX(NSEGH+I1)+FLUX(ICR1)
!             FLUX CANCELLED
              FLUX(ICR1)=0.D0
            ENDIF
!           SECOND CROSSED SEGMENT BYPASSED
            IF(TRA01%R(I2).LT.EPS_VOLUME.OR.
     &         TRA01%R(I4).LT.EPS_VOLUME) THEN
!             FLUX ADDED TO UPPER LAYER WITH OPPOSITE ORIENTATION
              IHOR=ISEG3D+NSEG
              FLUX(IHOR)=FLUX(IHOR)-FLUX(ICR2)
!             FLUX ADDED TO VERTICAL OF I2
              FLUX(NSEGH+I2)=FLUX(NSEGH+I2)+FLUX(ICR2)
!             FLUX CANCELLED
              FLUX(ICR2)=0.D0
            ENDIF
!           LOWER HORIZONTAL SEGMENT BYPASSED
            IF(TRA01%R(I1).LT.EPS_VOLUME.OR.
     &         TRA01%R(I2).LT.EPS_VOLUME) THEN
!             FLUX (FROM 2 TO 1) ADDED TO UPPER LAYER
!             HERE IHOR HAS ALREADY BEEN COMPUTED ABOVE
              FLUX(IHOR)=FLUX(IHOR)+FLUX(ISEG3D)
!             FLUX ADDED TO VERTICAL OF I2
!             (WITH - SIGN BECAUSE POSITIVE IS FROM UP TO DOWN)
              FLUX(NSEGH+I2)=FLUX(NSEGH+I2)-FLUX(ISEG3D)
!             FLUX REMOVED FROM VERTICAL OF I1 (WITH + SIGN)
              FLUX(NSEGH+I1)=FLUX(NSEGH+I1)+FLUX(ISEG3D)
!             FLUX CANCELLED
              FLUX(ISEG3D)=0.D0
            ELSE
              EXIT
            ENDIF
          ENDDO
        ENDDO
!
        ELSEIF(MESH3%TYPELM.EQ.50) THEN
!
!       SEE STOSEG51.F FOR NUMBERING OF CROSSED SEGMENTS
!
!       TETRAHEDRA
!       CASE WITH BOTH HORIZONTAL AND CROSSED FLUXES (CASE OF N-SCHEME)
!
        DO ISEG2D=1,NSEG
          DO IPLAN=1,NPLAN-1
!           ONLY ONE CROSSED SEGMENT, EXITING FROM POINT 1 OR 2
            ICR1=NSEGH+NSEGV+(IPLAN-1)*NSEG+ISEG2D
!           FIRST POINT OF THIS SEGMENT
            IS1=GLOSEG(ICR1,1)
!           LOWER HORIZONTAL SEGMENT
            ISEG3D=ISEG2D+(IPLAN-1)*NSEG
            I1=GLOSEG(ISEG3D,1)
            I2=GLOSEG(ISEG3D,2)
!           POINT ABOVE I2
            I3=I2+NPOIN2
!           POINT ABOVE I1
            I4=I1+NPOIN2
!           CASE OF CROSSED SEGMENT TOUCHING I1 (IS1=I1 OR IS1=I3)
            IF(TRA01%R(I1).LT.EPS_VOLUME.OR.
     &         TRA01%R(I3).LT.EPS_VOLUME) THEN
              IF(IS1.EQ.I1) THEN
!               WITH SAME ORIENTATION
!               FLUX ADDED TO UPPER LAYER WITH SAME ORIENTATION
                IHOR=ISEG3D+NSEG
                FLUX(IHOR)=FLUX(IHOR)+FLUX(ICR1)
!               FLUX ADDED TO VERTICAL OF I1
                FLUX(NSEGH+I1)=FLUX(NSEGH+I1)+FLUX(ICR1)
!               FLUX CANCELLED
                FLUX(ICR1)=0.D0
              ELSEIF(IS1.EQ.I3) THEN
!               WITH OPPOSITE ORIENTATION
!               FLUX ADDED TO UPPER LAYER WITH OPPOSITE ORIENTATION
                IHOR=ISEG3D+NSEG
                FLUX(IHOR)=FLUX(IHOR)-FLUX(ICR1)
!               FLUX ADDED TO VERTICAL OF I1
!               ALSO WITH - SIGN BECAUSE WITH OPPOSITE ORIENTATION
!               TO VERTICAL SEGMENTS ALSO (ALL ARE FROM BOTTOM TO TOP)
                FLUX(NSEGH+I1)=FLUX(NSEGH+I1)-FLUX(ICR1)
!               FLUX CANCELLED
                FLUX(ICR1)=0.D0
              ENDIF
            ENDIF
!           CASE OF CROSSED SEGMENT TOUCHING I2 (IS1=I2 OR IS1=I4)
            IF(TRA01%R(I2).LT.EPS_VOLUME.OR.
     &         TRA01%R(I4).LT.EPS_VOLUME) THEN
              IF(IS1.EQ.I2) THEN
!               WITH SAME ORIENTATION
!               FLUX ADDED TO UPPER LAYER WITH OPPOSITE ORIENTATION
                IHOR=ISEG3D+NSEG
                FLUX(IHOR)=FLUX(IHOR)-FLUX(ICR1)
!               FLUX ADDED TO VERTICAL OF I2
!               SAME ORIENTATION AS VERTICAL SEGMENTS, HENCE +
                FLUX(NSEGH+I2)=FLUX(NSEGH+I2)+FLUX(ICR1)
!               FLUX CANCELLED
                FLUX(ICR1)=0.D0
              ELSEIF(IS1.EQ.I4) THEN
!               WITH OPPOSITE ORIENTATION
!               FLUX ADDED TO UPPER LAYER WITH SAME ORIENTATION
                IHOR=ISEG3D+NSEG
                FLUX(IHOR)=FLUX(IHOR)+FLUX(ICR1)
!               FLUX ADDED TO VERTICAL OF I2
!               OPPOSITE ORIENTATION AS VERTICAL SEGMENTS, HENCE -
                FLUX(NSEGH+I2)=FLUX(NSEGH+I2)-FLUX(ICR1)
!               FLUX CANCELLED
                FLUX(ICR1)=0.D0
              ENDIF
            ENDIF
!           BYPASSING THE LOWER HORIZONTAL SEGMENT. EXACTLY LIKE PRISMS
!           BECAUSE SAME NUMBERING OF HORIZONTAL AND VERTICAL SEGMENTS
            IF(TRA01%R(I1).LT.EPS_VOLUME.OR.
     &         TRA01%R(I2).LT.EPS_VOLUME) THEN
!             FLUX (FROM 2 TO 1) ADDED TO UPPER LAYER
              FLUX(ISEG3D+NSEG)=FLUX(ISEG3D+NSEG)+FLUX(ISEG3D)
!             FLUX ADDED TO VERTICAL OF I2 (WITH - SIGN)
              FLUX(NSEGH+I2)=FLUX(NSEGH+I2)-FLUX(ISEG3D)
!             FLUX REMOVED FROM VERTICAL OF I1 (WITH + SIGN)
              FLUX(NSEGH+I1)=FLUX(NSEGH+I1)+FLUX(ISEG3D)
!             FLUX CANCELLED
              FLUX(ISEG3D)=0.D0
            ELSE
              EXIT
            ENDIF
          ENDDO
        ENDDO
!
        ELSE
          WRITE(LU,*) 'BYPASS_CRUSHED_POINTS_SEG'
          WRITE(LU,*) 'UNKNOWN TYPE OF ELEMENT: ',MESH3%TYPELM
          WRITE(LU,*) 'IN THE CASE OF SCHEME: ',SCHCF
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ELSEIF(SCHCF.EQ.ADV_LPO_TF) THEN
!
        IF(MESH3%TYPELM.NE.40) THEN
          WRITE(LU,*) 'BYPASS_CRUSHED_POINTS_SEG'
          WRITE(LU,*) 'UNKNOWN TYPE OF ELEMENT: ',MESH3%TYPELM
          WRITE(LU,*) 'IN THE CASE OF SCHEME: ',SCHCF
          CALL PLANTE(1)
          STOP
        ENDIF
!
!       CASE WITH ONLY HORIZONTAL FLUXES
!
        DO ISEG2D=1,NSEG
          DO IPLAN=1,NPLAN-1
            ISEG3D=ISEG2D+(IPLAN-1)*NSEG
            I1=GLOSEG(ISEG3D,1)
            I2=GLOSEG(ISEG3D,2)
            IF(TRA01%R(I1).LT.EPS_VOLUME) THEN
!             FLUX (FROM 2 TO 1) ADDED TO UPPER LAYER
              FLUX(ISEG3D+NSEG)=FLUX(ISEG3D+NSEG)+FLUX(ISEG3D)
!             FLUX ADDED TO VERTICAL OF I2 (WITH - SIGN)
              FLUX(NSEGH+I2)=FLUX(NSEGH+I2)-FLUX(ISEG3D)
!             FLUX REMOVED FROM VERTICAL OF I1 (WITH + SIGN)
              FLUX(NSEGH+I1)=FLUX(NSEGH+I1)+FLUX(ISEG3D)
!             FLUX CANCELLED
              FLUX(ISEG3D)=0.D0
            ELSEIF(TRA01%R(I2).LT.EPS_VOLUME) THEN
!             FLUX (FROM 2 TO 1) ADDED TO UPPER LAYER
              FLUX(ISEG3D+NSEG)=FLUX(ISEG3D+NSEG)+FLUX(ISEG3D)
!             FLUX REMOVED FROM VERTICAL OF I2
              FLUX(NSEGH+I2)=FLUX(NSEGH+I2)-FLUX(ISEG3D)
!             FLUX ADDED TO VERTICAL OF I1
              FLUX(NSEGH+I1)=FLUX(NSEGH+I1)+FLUX(ISEG3D)
!             FLUX CANCELLED
              FLUX(ISEG3D)=0.D0
            ELSE
              EXIT
            ENDIF
          ENDDO
        ENDDO
!
      ELSE
        WRITE(LU,*) 'UNKNOWN SCHEME IN BYPASS_CRUSHED_POINTS_SEG:',SCHCF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!=======================================================================
!
      RETURN
      END
