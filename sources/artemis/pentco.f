!                       *****************
                        SUBROUTINE PENTCO
!                       *****************
     &(II)
!
!***********************************************************************
! ARTEMIS   V7P3                                     Aug 2017
!***********************************************************************
!
!
!brief FUNCTION:   CALCULATE SECOND ORDER BOTTOM EFFECTS (GRADIENT&CURVATURE)
!+                  FOR EXTENDED MILD-SLOPE EQUATION
!
!code
!+        OUTPUT :
!+        1 + F = 1 + E1(KH)*grad(H)**2 + E2(KH)/K0*LAPLACIEN(H)
!+
!+      WE CAN CHOOSE TO ONTEGRATE ONLY GRADIANT EFFECTS              : II=1
!+      WE CAN CHOOSE TO ONTEGRATE ONLY CURVATURE EFFECTS             : II=2
!+      WE CAN CHOOSE TO INTEGRATE BOTH GRADIANT & CURVATURE EFFECTS  : II=3
!+      DEFAULT VALUE  : IPENTCO=0, ARTEMIS SOLVE CLASSICAL MILD-SLOPE EQUATION
!+
!+
!+     EXPRESSIONS FOR E1 and  E2 USED HERE ARE (Chamberlain & Porter 1995) :
!+
!+     (given X = 2 KH)
!+
!+          ( X**4 + 4 X**3 SH(X) - 9 SH(X)SH(2X) + 3 X (X+2SH(X))*(CH(X)**2-2CH(X)+3) )
!+ E1(KH) = -----------------------------------------------------------------------------
!+                                  3 ( X+SH(X) )**4
!+
!+
!+
!+
!+                         (TH(X)-X)* CH(X)
!+    E2(KH)/K0 = 2 H * ------------------------
!+                         X ( SH(X) + X )**2
!+
!+      K0 IS THE WAVE NUMBER FOR INFINITE DEPTH : K0 = K TH(KH)
!+      H IS THE WATER DEPTH
!+                                           ------
!+
!+  USING SUBROUTINE BERKHO NOTATIONS,
!+  AFTER VARIATIONAL FORMULATION :
!+           /
!+ AM1 =    / C*CG * GRAD(PSII)*GRAD(PSIJ) DS
!+         /S
!+
!+           /
!+       -  / OMEGA**2 * CG/C * (1+F) * PSII*PSIJ  DS
!+         /S
!+
!+           /
!+       -  /  BPHIRB * PSII*PSIJ  DB
!+         /B
!+
!+
!+ THE SECOND MEMEBER (DIFFUSION) IS MODIFIED
!
!history C.PEYRARD & E.RAZAFINDRAKOTO
!+        31/05/11
!+        V6P1
!+
!
!history  N.DURAND (HRW)
!+        August 2017
!+        V7P3
!+   Removed unnecessary references to PI, DEGRAD and RADDEG
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|    II          |-->|  OPTION FOR GRADIENT AND CURVATURE EFFECTS
!|    T2          |---|  WORK TABLE
!|    T4          |---|  WORK TABLE
!|    T5          |---|  WORK TABLE
!|    T6          |---|  WORK TABLE
!|    T7          |---|  WORK TABLE
!|    T9          |---|  WORK TABLE
!|    T8          |---|  WORK TABLE
!|    T11         |---|  WORK TABLE
!|    T12         |---|  WORK TABLE
!|    T3          |<--|  OUTPUT WORK TABLE WITH CORRECTION TERMS FOR
!|                |   |  GRADIENT AND CURVATURE EFFECTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! APPELE PAR                   :  BERKHO
! SOUS-PROGRAMMES APPELES      :  FCTE1 et FCTE2
! TABLEAUX DE TRAVAIL UTILISES :  T2 T3 T4 T5 T6 T7 T9 T8 T11 T12
!
!-----------------------------------------------------------------------
!
      USE BIEF
      USE INTERFACE_ARTEMIS,ONLY: FCTE1,FCTE2
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: II
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
!
! MASS MATRIX
!
      CALL VECTOR(T2 , '=' , 'MASBAS          ' , IELM ,
     &            1.D0 , C , C , C , C , C , C ,
     &            MESH , MSK  , MASKEL )
!
      IF(II.EQ.1.OR.II.EQ.3) THEN
! --------------
! GRADIENT EFFECTS
! --------------
!
!--->  E1*GRAD(H)**2 ---> IN T4
!
!  DX
!
      CALL VECTOR(T7 , '=' , 'GRADF          X' , IELM ,
     &            1.D0 , H , T1 , T1 , T1 , T1 , T1 ,
     &            MESH , MSK , MASKEL)
      CALL OS('X=YZ    ', X=T4, Y=T7, Z=T7)
!
!  DY
!
      CALL VECTOR(T7 , '=' , 'GRADF          Y' , IELM ,
     &            1.D0 , H , T1 , T1 , T1 , T1 , T1 ,
     &            MESH , MSK , MASKEL)
      CALL OS('X=YZ    ' , X=T5,Y=T7,Z=T7)
!
! GRAD(H)**2
!
      CALL OS( 'X=X+Y   ', X=T4,Y=T5)
!
! SQUARE MASS
!
      CALL OS( 'X=YZ    ' , X=T8,Y=T2,Z=T2)
!
! COEFF GRAD(H)**2
!
      CALL OS( 'X=Y/Z   ' , X=T4,Y=T4,Z=T8)
!
!---> FUNCTION E1
!
      DO I=1,NPOIN
        T11%R(I)=FCTE1(  K%R(I)*H%R(I) )
      END DO
!
!--->  E1*GRAD(H)**2 ---> IN T4
!
      CALL OS( 'X=YZ    ' , X=T4,Y=T4,Z=T11)
! END OF GRADIENT EFFECTS
      ENDIF
!
      IF(II.EQ.2.OR.II.EQ.3) THEN
! ------------------
! CURVATURE EFFECTS
! ------------------
!---> E2/K0*LAPLACIAN(H)   ---> IN T9
!  DX
      CALL VECTOR(T7 , '=' , 'GRADF          X' , IELM ,
     &            1.D0 , H , T1 , T1 , T1 , T1 , T1 ,
     &            MESH , MSK , MASKEL)
      CALL OS( 'X=Y/Z    ' , X=T7,Y=T7,Z=T2)

      CALL VECTOR(T5 , '=' , 'GRADF          X' , IELM ,
     &            1.D0 , T7 , T1 , T1 , T1 , T1 , T1 ,
     &            MESH , MSK , MASKEL)
!
! COEFF DX
!
      CALL OS('X=Y/Z    ' , X=T5,Y=T5,Z=T2)
!
!  DY
!
      CALL VECTOR(T7 , '=' , 'GRADF          Y' , IELM ,
     &            1.D0 , H , T1 , T1 , T1 , T1 , T1 ,
     &            MESH , MSK , MASKEL)
      CALL OS('X=Y/Z    ' , X=T7,Y=T7,Z=T2)
!
      CALL VECTOR(T6 , '=' , 'GRADF          Y' , IELM ,
     &            1.D0 , T7 , T1 , T1 , T1 , T1 , T1 ,
     &            MESH , MSK , MASKEL)
!
! COEFF DY
!
      CALL OS('X=Y/Z   ', X=T6,Y=T6,Z=T2)
!
      CALL OS('X=Y+Z   ', X=T9,Y=T5,Z=T6)
!
!---> FUNCTION E2 * 2 H
!
      DO I=1,NPOIN
        T12%R(I)=2.*H%R(I) * FCTE2(K%R(I)*H%R(I))
      END DO
!
!---> E2/K0*LAPLACIAN(H)
!
      CALL OS( 'X=YZ    ' , X=T9,Y=T9,Z=T12)
!
! END OF CURVATURE EFFECTS
      ENDIF
!
! SUM OF GRADIENT AND CURVTURE EFFECTS, DEPENDING OF OPTION "IPENTCO"
!
      IF(II.EQ.1)  THEN
!       F= E1*GRAD(H)**2
        CALL OS( 'X=Y      ' , X=T3,Y=T4)
      ENDIF
!
      IF(II.EQ.2)  THEN
!       F= E2/K0*LAPLACIAN(H)
        CALL OS( 'X=Y      ' , X=T3,Y=T9)
      ENDIF
!
      IF(II.EQ.3)  THEN
!       F =  E1*grad(H)**2 + E2/K0*LAPLACIEN(H)
        CALL OS( 'X=Y+Z    ' , X=T3,Y=T4,Z=T9)
      ENDIF
!
! ADD 1.,  T3 = 1 + F
!
      CALL OS( 'X=X+C   ', X=T3,C=1.D0)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

