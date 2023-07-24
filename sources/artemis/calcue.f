!                   *****************
                    SUBROUTINE CALCUE
!                   *****************
!
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES AN EFFECTIVE SPEED UE FOR THE ESTIMATION
!+                OF THE FRICTION DISSIPATION COEFFICIENT FW.
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH) ; D. PAUGAM ( PLACEMENT)
!+        02/06/1999
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      CALL VECTOR(U0 , '=' , 'GRADF          X' , IELM ,
     &            1.D0 , PHIR , SBID , SBID , SBID , SBID , SBID ,
     &            MESH , MSK  , MASKEL )
!
      CALL VECTOR(V0 , '=' , 'GRADF          Y' , IELM ,
     &            1.D0 , PHIR , C , C , C , C , C ,
     &            MESH , MSK  , MASKEL )
!
!
!      THE OLD VARIABLE U1 IS STORED IN T1
!
      CALL VECTOR(T1, '=' , 'GRADF          X' , IELM ,
     &            1.D0 , PHII , C , C , C , C , C ,
     &            MESH , MSK  , MASKEL )
!
!
!      THE OLD VARIABLE V1 IS STORED IN T2
!
      CALL VECTOR(T2, '=' , 'GRADF          Y' , IELM ,
     &            1.D0 , PHII , C , C , C , C , C ,
     &            MESH , MSK  , MASKEL )
!
      CALL VECTOR(T4 , '=' , 'MASBAS          ' , IELM ,
     &            1.D0 , C , C , C , C , C , C ,
     &            MESH , MSK  , MASKEL )
!
      CALL OS('X=Y/Z   ', X=U0, Y=U0, Z=T4)
      CALL OS('X=Y/Z   ', X=V0, Y=V0, Z=T4)
      CALL OS('X=Y/Z   ', X=T1, Y=T1, Z=T4)
      CALL OS('X=Y/Z   ', X=T2, Y=T2, Z=T4)
!
!--------------------------------------------------------------
!             COMPUTES UE
!--------------------------------------------------------------
!
      CALL OS('X=C     ', X=T4 , C=0.D0)
      CALL OS('X=YZ    ', X=T4 , Y=U0 , Z=U0)
      CALL OS('X=X+YZ  ', X=T4 , Y=V0 , Z=V0)
      CALL OS('X=X+YZ  ', X=T4 , Y=T1 , Z=T1)
      CALL OS('X=X+YZ  ', X=T4 , Y=T2 , Z=T2)
!
      CALL OS('X=CX    ', X=T4 , C=0.5D0)
      CALL OS('X=SQR(Y)', X=T1 , Y=T4)
      CALL OS('X=CY    ', X=T4 , Y=T1, C=1.2D0 )
!
      RETURN
      END
