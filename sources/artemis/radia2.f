!                   *****************
                    SUBROUTINE RADIA2
!                   *****************
!
     &(LISHHO)
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE RADIATION STRESSES AND DRIVING FORCES.
!
!reference  M.W. DINGEMANS, A.C. RADDER AND H.J. DE VRIEND
!+          COMPUTATION OF THE DRIVING FORCES OF WACE-INDUCED
!+          CURRENTS. COASTAL ENGINEERING, 11 (1987) PP 539-563.
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH) ; F. BECQ (LNH)
!+        04/06/1999
!+        V5P1
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
!| LISHHO         |<--| SMOOTHING FOR THE WAVE HEIGTH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
      USE INTERFACE_ARTEMIS, EX_RADIA2=> RADIA2
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: LISHHO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
! INTERNAL VARIABLES FOR RADIA2
!
      DOUBLE PRECISION COE , COCO, COSI, SISI
      INTEGER          LISRAD
!
      LOGICAL MAS
!
      INTRINSIC COS, SIN
!
!
!=======================================================================
!     RADIATION STRESSES........METHOD 2 (IDENTICAL TO THAT USED IN TOMAWAC)
!=======================================================================
!
      CALL OS('X=Y     ',X=T3,Y=HHO)
!
! -------------------------------------------------------------
! SMOOTHES THE WAVE HEIGHT TO ELIMINATE PARASITIC
! OSCILLATIONS
! -------------------------------------------------------------
!
      IF(LISHHO.GT.0) THEN
        MAS = .TRUE.
        CALL FILTER(T3,MAS,T1,T2,AM1,'MATMAS          ',
     &              1.D0,SBID,SBID,SBID,SBID,SBID,SBID,
     &              MESH,MSK,MASKEL,LISHHO)
      ENDIF
!
      CALL OS('X=Y     ',X=HHO,Y=T3)
!
! -------------------------------------------------------------
! COMPUTES STRESSES SXX, SXY AND SYY
! -------------------------------------------------------------
!
      CALL OS('X=Y/Z   ', X=T1 , Y=CG , Z=C)
      DO I=1,NPOIN
        COCO=COS(INCI%R(I))*COS(INCI%R(I))
        COSI=COS(INCI%R(I))*SIN(INCI%R(I))
        SISI=SIN(INCI%R(I))*SIN(INCI%R(I))
        COE=GRAV*HALE%R(I)*HALE%R(I)/16.D0
!
! THE COEFFICIENT 1/16 ABOVE STEMS FROM HHO REPRESENTING THE
! SIGNIFICANT WAVE HEIGHT (ENERGY) IN RANDOM SEAS
!
        SXX%R(I)= (T1%R(I)*(1.D0+COCO)-0.5D0)*COE
        SXY%R(I)= (T1%R(I)*COSI)*COE
        SYY%R(I)= (T1%R(I)*(1.D0+SISI)-0.5D0)*COE
      END DO
!
!
!=======================================================================
! SPACIAL GRADIENTS OF RADIATION STRESSES
!=======================================================================
!
!  -----------------------------------------------
!  OPTIONAL SMOOTHING(S) OF THE RADIATION STRESSES
!  -----------------------------------------------
!
      LISRAD = 3
!
      CALL OS('X=Y     ',X=T3,Y=SXX)
      IF(LISRAD.GT.0) THEN
        MAS = .TRUE.
        CALL FILTER(T3,MAS,T1,T2,AM1,'MATMAS          ',
     &              1.D0,SBID,SBID,SBID,SBID,SBID,SBID,
     &              MESH,MSK,MASKEL,LISRAD)
      ENDIF
      CALL OS('X=Y     ',X=SXX,Y=T3)
!
      CALL OS('X=Y     ',X=T3,Y=SXY)
      IF(LISRAD.GT.0) THEN
        MAS = .TRUE.
        CALL FILTER(T3,MAS,T1,T2,AM1,'MATMAS          ',
     &              1.D0,SBID,SBID,SBID,SBID,SBID,SBID,
     &              MESH,MSK,MASKEL,LISRAD)
      ENDIF
      CALL OS('X=Y     ',X=SXY,Y=T3)
!
      CALL OS('X=Y     ',X=T3,Y=SYY)
      IF(LISRAD.GT.0) THEN
        MAS = .TRUE.
        CALL FILTER(T3,MAS,T1,T2,AM1,'MATMAS          ',
     &              1.D0,SBID,SBID,SBID,SBID,SBID,SBID,
     &              MESH,MSK,MASKEL,LISRAD)
      ENDIF
      CALL OS('X=Y     ',X=SYY,Y=T3)
!
! END OF RADIATION STRESS SMOOTHING(S)
! -------------------------------------------------------
!
!=======================================================================
! DRIVING FORCES FX AND FY FOR WAVE-INDUCED CURRENTS
!=======================================================================
!
      CALL VECTOR(T1 , '=' , 'MASBAS          ' , IELM ,
     &            1.D0 , T3 , T3 , T3 , T3 , T3 , T3 ,
     &            MESH , MSK , MASKEL )
!
      CALL VECTOR
     & (T2,'=','GRADF          X',IELM,1.D0,SXX,T4,T4,T4,T4,T4,
     &  MESH , MSK , MASKEL )
      CALL OS('X=Y/Z   ',X=T2,Y=T2,Z=T1)
!
      CALL VECTOR
     & (T3,'=','GRADF          Y',IELM,1.D0,SXY,T4,T4,T4,T4,T4,
     &  MESH , MSK , MASKEL )
      CALL OS('X=Y/Z   ',X=T3,Y=T3,Z=T1)
!     ------------------------------------
!     FORCE FX = - (DSXX/DX + DSXY/DY) / H
!     ------------------------------------
      CALL OS('X=Y+Z   ',X=FX,Y=T2,Z=T3)
      CALL OS('X=CY/Z  ',X=FX,Y=FX,Z=H,C=-1.D0)
!
!     ------------------------------------
!
      CALL VECTOR(T1 , '=' , 'MASBAS          ' , IELM ,
     &            1.D0 , T3 , T3 , T3 , T3 , T3 , T3 ,
     &            MESH , MSK , MASKEL )
!
      CALL VECTOR
     & (T2,'=','GRADF          X',IELM,1.D0,SXY,T4,T4,T4,T4,T4,
     &  MESH , MSK , MASKEL )
      CALL OS('X=Y/Z   ',X=T2,Y=T2,Z=T1)
!
      CALL VECTOR
     & (T3,'=','GRADF          Y',IELM,1.D0,SYY,T4,T4,T4,T4,T4,
     &  MESH , MSK , MASKEL )
      CALL OS('X=Y/Z   ',X=T3,Y=T3,Z=T1)
!
!     ------------------------------------
!     FORCE FY = - (DSXY/DX + DSYY/DY) / H
!     ------------------------------------
      CALL OS('X=Y+Z   ',X=FY,Y=T2,Z=T3)
      CALL OS('X=CY/Z  ',X=FY,Y=FY,Z=H,C=-1.D0)
!
!=======================================================================
!
      RETURN
      END
