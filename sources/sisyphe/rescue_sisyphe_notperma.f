!                   **********************************
                    SUBROUTINE RESCUE_SISYPHE_NOTPERMA
!                   **********************************
!
     &(QU,QV,Q,U,V,H,S,ZF,HW,TW,THETAW,NPOIN,TROUVE,ALIRE,ICF,ENTET,
     & MAXVAR)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES MISSING DATA/VARIABLES FOR HYDRODYNAMIC
!+                AND/OR SEDIMENTOLOGICAL CONTINUATION RUN.
!
!note     RESCUE_SISYPHE MUST BE MODIFIED FOR UNSTEADY SIMULATIONS
!+         TO TAKE THE BED EVOLUTIONS INTO ACCOUNT.
!note  THE WATER DEPTH NEEDS TO BE COMPUTED FROM THE BED ELEVATION
!+        (SISYPHE) AND THE FREE SURFACE ELEVATION (HYDRO FILE).
!
!history  C.VILLARET
!+
!+        V5P7
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
!| ALIRE          |-->| LIST VARIABLES TO BE READ
!| H              |<->| WATER DEPTH
!| HW             |<->| WAVE DEPTH
!| ICF            |-->| BED-LOAD OR TOTAL LOAD TRANSPORT FORMULAS
!| LISTI          |-->| LOGICAL, IF YES PRINT MESSAGES
!| MAXVAR         |-->| MAXIMUM NUMBER OF OUTPUT VARIABLES
!| NPOIN          |-->| NUMBER OF MESH NODES
!| PASS           |-->| LOGICAL, IF YES BEGIN OF COMPUTATION
!| Q              |<->| LIQUID DISCHARGE
!| QU             |<->| LIQUID DISCHARGE X
!| QV             |<->| LIQUID DISCHARGE Y
!| S              |<->| WATER SURFACE ELEVATION
!| THETAW         |<->| ANGLE BETWEEN WAVE AND CURRENT
!| TROUVE         |-->| LOGIQUE INDIQUANT LES VARIABLES TROUVEES
!|                |   | DANS LE SOUS-PROGRAMME SUITE
!| TW             |<->| WAVE PERIOD
!| U              |<->| VELOCITY COMPONENT X
!| V              |<->| VELOCITY COMPONENT Y
!| ZF             |<->| BED LEVEL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE, EX_RESCUE_SISYPHE_NOTPERMA
     &           => RESCUE_SISYPHE_NOTPERMA
      USE BIEF, ONLY: OV
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: MAXVAR
      INTEGER, INTENT(IN) :: TROUVE(MAXVAR),ALIRE(MAXVAR),NPOIN,ICF
!
      DOUBLE PRECISION, INTENT(INOUT) :: QU(NPOIN), QV(NPOIN), Q(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NPOIN) , V(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: S(NPOIN) , ZF(NPOIN), H(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: HW(NPOIN), TW(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: THETAW(NPOIN)
      LOGICAL, INTENT(IN)             :: ENTET
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     THE ESSENTIAL VARIABLES NOT MODIFIED BY THE BED EVOLUTIONS
!     ARE THE FREE SURFACE AND THE FLOW
!
!     COMPUTES THE FREE SURFACE(4) FROM THE NON-MODIFIED WATER DEPTH
!     AND THE NON-MODIFIED BED ELEVATION
!
      IF(TROUVE(4).EQ.0.AND.ALIRE(4).EQ.1) THEN
        IF(TROUVE(3).EQ.1.AND.TROUVE(5).EQ.1) THEN
          CALL OV('X=Y+Z   ', X=S, Y=H, Z=ZF, DIM1=NPOIN)
        ELSE
          WRITE(LU,*) 'UNABLE TO BUILD FREE SURFACE'
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
! COMPUTES THE TELEMAC WATER DEPTH(3) FROM THE NON-MODIFIED
! BED ELEVATION AND THE FREE SURFACE
!
      IF(TROUVE(3).EQ.0.AND.ALIRE(3).EQ.1) THEN
        IF(TROUVE(5).EQ.1) THEN
          CALL OV('X=Y-Z   ', X=H, Y=S, Z=ZF, DIM1=NPOIN)
        ELSE
          WRITE(LU,*) 'MISSING BOTTOM OR WATER DEPTH'
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!  FLOWRATE ALONG X:  QU
!
      IF (ALIRE(7).EQ.1.AND.TROUVE(7).EQ.0)  THEN
        IF (TROUVE(1).EQ.1) THEN
          IF(ENTET) THEN
          WRITE(LU,151)
151       FORMAT(1X,'DISCHARGE /X COMPUTED WITH DEPTH AND VELOCITY')
          ENDIF
          CALL OV('X=YZ    ', X=QU, Y=U, Z=H, DIM1=NPOIN)
        ELSE
          IF(ENTET) THEN
          WRITE(LU,191)
191       FORMAT(1X,'PREVIOUS COMPUTATION WITHOUT VELOCITY U: U IS
     &               EQUAL TO ZERO')
          ENDIF
          CALL OV('X=C     ', X=QU, C=0.D0, DIM1=NPOIN)
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!  FLOWRATE ALONG Y :  QV
!
      IF (ALIRE(8).EQ.1.AND.TROUVE(8).EQ.0)  THEN
        IF (TROUVE(2).EQ.1) THEN
          IF(ENTET) THEN
          WRITE(LU,161)
161       FORMAT(1X,'DISCHARGE /Y COMPUTED WITH DEPTH AND VELOCITY')
          ENDIF
          CALL OV('X=YZ    ', X=QV, Y=V, Z=H,DIM1=NPOIN)
        ELSE
          IF(ENTET) THEN
          WRITE(LU,211)
211       FORMAT(1X,'PREVIOUS COMPUTATION WITHOUT VELOCITY V:
     &               FIXED TO ZERO')
          ENDIF
          CALL OV('X=C     ', X=QV, C=0.D0, DIM1=NPOIN)
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!  FLOWRATE (M2/S)
!
      IF ((ALIRE(6).EQ.1).AND.(TROUVE(6).EQ.0))  THEN
        CALL OV('X=N(Y,Z)', X=Q, Y=QU, Z=QV, DIM1=NPOIN)
      ENDIF
!
!-----------------------------------------------------------------------
!  WAVE HEIGHT AND PERIOD
!
      IF ((ICF==4).OR.(ICF==5).OR.
     &    (ICF==8).OR.(ICF==9)    ) THEN
!
        IF ((ALIRE(12).EQ.1).AND.(TROUVE(12).EQ.0)) THEN
          WRITE(LU,901)
          CALL OV('X=C     ', X=HW, C=0.D0, DIM1=NPOIN)
        ENDIF
!
901     FORMAT(1X,'PREVIOUS COMPUTATION WITHOUT WAVE HEIGHT : IT IS',
     &          ' FIXED TO ZERO')
!
        IF ((ALIRE(13).EQ.1).AND.(TROUVE(13).EQ.0)) THEN
          WRITE(LU,903)
          CALL OV('X=C     ', X=TW, C=0.D0, DIM1=NPOIN)
        ENDIF
903     FORMAT(1X,'PREVIOUS COMPUTATION WITHOUT WAVE PERIOD : IT IS',
     &          ' FIXED TO ZERO')
!
        IF ((ALIRE(14).EQ.1).AND.(TROUVE(14).EQ.0)) THEN
          WRITE(LU,910)
          CALL OV('X=C     ', X=THETAW , C=90.D0, DIM1=NPOIN)
        ENDIF
      ENDIF
910     FORMAT(1X,'PREVIOUS COMPUTATION WITHOUT WAVE ANGLE : IT IS',
     &          ' FIXED TO ZERO')
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE RESCUE_SISYPHE_NOTPERMA
