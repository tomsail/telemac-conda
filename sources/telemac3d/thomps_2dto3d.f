!                   ************************
                    SUBROUTINE THOMPS_2DTO3D
!                   ************************
!
!
!***********************************************************************
! TELEMAC3D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    GENERATING THE 3D BOUNDARY CONDITIONS IN VIEW OF THE 2D
!+
!+            CONDITIONS GIVEN BY THOMPSON METHOD
!
!warning    Only velocities treated here.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  J-M HERVOUET (LNHE)
!+
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER IPLAN,IPTFR,IPTFR3,IFRLIQ,IOPT,IPOIN2
      DOUBLE PRECISION AUX,HH,Y0,DENOM,INTEG,UCOEF,VCOEF,DZ,DELTAZ
!
!***********************************************************************
!
!     HARDCODED OPTION   1:CONSTANT   2:LOGARITHMIC PROFILE
!
      IOPT=1
!
      IF(IOPT.EQ.1) THEN
!
        DO IPTFR = 1,NPTFR2
          IFRLIQ=NUMLIQ%I(IPTFR)
          IF(IFRLIQ.NE.0) THEN
            IF(FRTYPE(IFRLIQ).EQ.2) THEN
              DO IPLAN = 1,NPLAN
                IPTFR3 = (IPLAN-1)*NPTFR2 + IPTFR
                UBORL%R(IPTFR3)  = UBOR2D%R(IPTFR)
                VBORL%R(IPTFR3)  = VBOR2D%R(IPTFR)
              ENDDO
            ENDIF
          ENDIF
        ENDDO
!
      ELSEIF(IOPT.EQ.2) THEN
!
        DO IPTFR=1,NPTFR2
          IFRLIQ=NUMLIQ%I(IPTFR)
          IF(IFRLIQ.NE.0) THEN
          IF(FRTYPE(IFRLIQ).EQ.2) THEN
!
          IPOIN2=MESH3D%NBOR%I(IPTFR)
          AUX=KARMAN*SQRT(2.D0/CF%R(IPOIN2))
          HH=MAX( MESH3D%Z%R(IPOIN2+(NPLAN-1)*NPOIN2)
     &           -MESH3D%Z%R(IPOIN2)   , 1.D-4)
!         Y0 = KS/30
          Y0=HH/EXP(1.D0+AUX)
!         EXP(1)= 2.71828182845D0
          DENOM=MAX(LOG(HH/Y0/2.71828182845D0),1.D-4)
!
          INTEG=0.D0
          DO IPLAN=1,NPLAN
            IF(IPLAN.EQ.1) THEN
              DZ=(MESH3D%Z%R(IPOIN2+NPOIN2)-MESH3D%Z%R(IPOIN2))
              AUX=2.D0
              DELTAZ=DZ/EXP(AUX)
            ELSE
              DELTAZ=MESH3D%Z%R(IPOIN2+(IPLAN-1)*NPOIN2)
     &              -MESH3D%Z%R(IPOIN2)
            ENDIF
            DELTAZ=MAX(DELTAZ,Y0)
            IF(IPLAN.EQ.1) THEN
              DZ=( MESH3D%Z%R(IPOIN2+NPOIN2)
     &            -MESH3D%Z%R(IPOIN2       ) )*0.5D0
            ELSEIF(IPLAN.EQ.NPLAN) THEN
              DZ=( MESH3D%Z%R(IPOIN2+(NPLAN-1)*NPOIN2)
     &            -MESH3D%Z%R(IPOIN2+(NPLAN-2)*NPOIN2) )*0.5D0
            ELSE
              DZ=( MESH3D%Z%R(IPOIN2+ IPLAN   *NPOIN2)
     &            -MESH3D%Z%R(IPOIN2+(IPLAN-2)*NPOIN2) )*0.5D0
            ENDIF
            INTEG=INTEG+DZ*LOG(DELTAZ/Y0)/DENOM
          ENDDO
          UCOEF=HH*UBOR2D%R(IPTFR)/MAX(INTEG,1.D-10)
          VCOEF=HH*VBOR2D%R(IPTFR)/MAX(INTEG,1.D-10)
          DO IPLAN=1,NPLAN
            IF(IPLAN.EQ.1) THEN
              DZ=(MESH3D%Z%R(IPOIN2+NPOIN2)-MESH3D%Z%R(IPOIN2))
              AUX=2.D0
              DELTAZ=DZ/EXP(AUX)
            ELSE
              DELTAZ=MESH3D%Z%R(IPOIN2+(IPLAN-1)*NPOIN2)
     &              -MESH3D%Z%R(IPOIN2)
            ENDIF
            DELTAZ=MAX(DELTAZ,Y0)
            IPTFR3 = (IPLAN-1)*NPTFR2 + IPTFR
            UBORL%R(IPTFR3) = UCOEF*LOG(DELTAZ/Y0)/DENOM
            VBORL%R(IPTFR3) = VCOEF*LOG(DELTAZ/Y0)/DENOM
          ENDDO
!
          ENDIF
          ENDIF
        ENDDO
!
      ELSE
!
        WRITE(LU,*) 'THOMPS_2DTO3D: UNKNOWN OPTION IOPT=',IOPT
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!======================================================================
!
      RETURN
      END
