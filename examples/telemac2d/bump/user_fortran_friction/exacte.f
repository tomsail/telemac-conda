!                       *****************
                        SUBROUTINE EXACTE
!                       *****************
!
     &(HN,U,ZF,X,NPOIN,ICONS)
!
!***********************************************************************
! PROGICIEL : 'TELEMAC'       12/12/88    F. LEPEINTRE
!                             10/02/92    J-M HERVOUET (REMPLACEMENT
!                             DE ZRPOLY PAR ZBRENT, IMPLICIT NONE ET
!                             DOUBLE PRECISION)
!                             02/03/92    F. LEPEINTRE
!***********************************************************************
!
!      FONCTION:    SOLUTION EXACTE DE L'ECOULEMENT TRANSCRITIQUE
!                   SUR UN BUMP. AVEC UN RESSAUT
!
!                   PAR CONVENTION, ZF=0. AU POINT CRITIQUE
!
!      ATTENTION : ON UTILISE ICI LE FAIT QUE LE MAILLAGE
!                  EST RECTANGULAIRE.
!
!
!      ICONS = 1 : HAUTEURS CONJUGUEES CLASSIQUES
!      ICONS = 2 : HAUTEURS CONJUGUEES "NON CONSERVATIVES"
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |     HN         |<-- |  HAUTEUR D'EAU.                              |
! |     U          |<-- |  VITESSE U.
! |     ZF         | -->|  COTE DU FOND.
! |     X          | -->|  ABCISSES DES POINTS DU MAILLAGE
! |     NPOIN      | -->|  NOMBRE DE POINTS DU MAILLAGE
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!**********************************************************************
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC2D, ONLY: GRAV
      IMPLICIT NONE
!
      DOUBLE PRECISION U(132,11),ZF(132,11),X(132,11),HN(132,11)
      DOUBLE PRECISION ZVF(300),ZV(300),PRAD(300),ZAVAL
      DOUBLE PRECISION QFIXG,ST,PRK,XRK,ZRK,XRKPR,ZRKPR,ZCRIT
      DOUBLE PRECISION XRKCO,ZRKCO,ZFLU,RES
!
      INTEGER IM,JM,I,J,ICRIT,INOEUD,NOEUD,NOEUDF,ND,NG,NRK,N,IC,NPOIN
      INTEGER ICONS
!
      DOUBLE PRECISION A(4)
      COMMON/FORFC1/A
      COMMON/HCRIT/ZCRIT
!
      EXTERNAL F
      DOUBLE PRECISION F
!
      INTRINSIC REAL
!
!-----------------------------------------------------------------------
!
!     THE CHANNEL WIDTH IS 1M
!
!     TODO: DO NOT ASSUME THE MESH HAS A GIVEN SHAPE
!     MESH MADE OF SQUARES SPLIT INTO TRIANGLES. THE STRUCTURE IS SUCH
!     THAT IT IS POSSIBLE TO WORK ON IT AS ON A STRUCTURED MESH.
!     BEWARE THAT THIS IS NOT A GENERIC IMPLEMENTATION: IT DOES NOT
!     WORK ON ANOTHER DISCRETISATION, NEITHER ON AN UNSTRUCTURED MESH

!     NUMBER OF SQUARES ALONG X
      IM = 132
!     NUMBER OF SQUARES ALONG Y
      JM = 11

!     INDEX OF THE POINT WHERE THE TRANSITION TO A CRITICAL
!     FLOW IS ASSUMED TO TAKE PLACE (DIFFERENT FROM THE WEIR POSITION)
!     IN THE PRESENT CASE, THE BATHYMETRY IS SUCH THAT THE CRITICAL
!     POINT IS LOCATED AT THE FIRST ELEMENT AFTER THE WEIR.
      ICRIT = 63

!     DOWNSTREAM WATER LEVEL
      ZAVAL  = 0.6D0
!     LINEIC FLOWRATE (PER M2 OF WIDTH)
      QFIXG  = 1.D0
!     STRICKLER COEFFICIENT
      ST     = 40.D0
!
!     COMPUTE THE BED SLOPE (GRAD_ZF)
      DO NOEUD=1,IM-1
        ND = NOEUD + 1
        NG = NOEUD
!       BED SLOPE (WITH OPPOSITE SIGN)
        PRAD(NOEUD) = -(ZF(ND,5)-ZF(NG,5)) / (X(ND,5)-X(NG,5))
      ENDDO
!
!     RUNGE-KUTTA ALGORITHM (METHOD OF THE IMPROVED TANGENT)
!     NUMBER OF SUBSTEPS
      NRK = 10000
!     RUNGE-KUTTA STEP SIZE
      PRK = (X(IM,5)-X(1,5))/FLOAT(NRK-1)
!
!     FIRST, COMPUTE THE SUBCRITICAL WATER LINE FROM THE
!     DOWNSTREAM BOUNDARY AS LONG AS Y IS LARGER AS ZCRITIQUE
!
      ZCRIT=(QFIXG**2/GRAV)**(1.D0/3.D0)
      ZVF(IM) = ZAVAL
      XRK = X(IM,5)
      ZRK = ZAVAL
      IC  = IM-1
!
      DO N=1,NRK
!       PREDICTION
        XRKPR = XRK - PRK
        ZRKPR = ZRK - PRK*F(ZRK,IC,QFIXG,PRAD,ST)
        IF(ZRKPR.LT.ZCRIT) THEN
!         UNSIFFICIENT HEAD TO PASS THE WEIR IN SUBCRITICAL STATE
          NOEUDF = IC+1
          GOTO 30
        ENDIF
!       CORRECTION
        XRKCO = XRKPR
        ZRKCO = ZRK - PRK*(F(ZRK  ,IC,QFIXG,PRAD,ST) +
     &                     F(ZRKPR,IC,QFIXG,PRAD,ST))*0.5D0
        IF(ZRKCO.LT.ZCRIT) THEN
!         UNSIFFICIENT HEAD TO PASS THE WEIR IN SUBCRITICAL STATE
          NOEUDF = IC+1
          GOTO 30
        ENDIF
!
!       ARE WE OUTSIDE OF THE CURRENT ELEMENT?
        IF(XRKCO.LE.X(IC,5)) THEN
!         COMPUTE Z AT THE NODE THROUGH A LINEAR INTERPOLATION
          ZVF(IC) = (ZRK-ZRKCO)/PRK*(X(IC,5)-XRK)+ ZRK
!         CHANGE THE CURRENT ELEMENT
          IC = IC-1
          IF (IC.EQ.0) GOTO 40
        ENDIF
!       UPDATE
        XRK = XRKCO
        ZRK = ZRKCO
!
        ENDDO
40      DO NOEUD=1,IM
          ZV(NOEUD) = ZVF(NOEUD)
        ENDDO
        GOTO 60
!
30      CONTINUE
!
!
!       COMPUTE THE WATER LINE FROM THE CRITICAL POINT
!
!       SUBCRITICAL FLOW
!
!       USING RUNGE-KUTTA (IMPROVED TANGENT METHOD)
!       NUMBER OF SUBSTEPS
        NRK = 10000
!       RUNGE-KUTTA STEP SIZE
        PRK = (X(ICRIT,5)-X(1,5))/REAL(NRK-1)
!
        ZV(ICRIT) = ZCRIT
        XRK = X(ICRIT,5)
        ZRK = ZV(ICRIT)
        IC  = ICRIT-1
!
        DO N=1,NRK
!
!       PREDICTION
          XRKPR = XRK - PRK
          ZRKPR = ZRK - PRK*F(ZRK,IC,QFIXG,PRAD,ST)
!       CORRECTION
          XRKCO = XRKPR
          ZRKCO = ZRK - PRK*(F(ZRK  ,IC,QFIXG,PRAD,ST)+
     &                       F(ZRKPR,IC,QFIXG,PRAD,ST))/2.D0
!
!         ARE WE OUTSIDE OF THE CURRENT ELEMENT?
          IF (XRKCO.LE.X(IC,5)) THEN
!           COMPUTE Z AT THE NODE THROUGH A LINEAR INTERPOLATION
            ZV(IC) = (ZRK-ZRKCO)/PRK*(X(IC,5)-XRK)+ ZRK
!           CHANGE THE CURRENT ELEMENT
            IC = IC-1
            IF (IC.EQ.0) GOTO 80
          ENDIF
!         ACTUALISATION
          XRK = XRKCO
          ZRK = ZRKCO
!
          ENDDO
!
!         SUPERCRITICAL FLOW
!
80        CONTINUE
!
!         USE RUNGE-KUTTA (METHOD OF THE IMPROVED TANGENT)
!         NUMBER OF SUB-STEPS
          NRK = 10000
!         RUNGE-KUTTA STEP SIZE
          PRK = (X(IM,5)-X(ICRIT,5))/REAL(NRK-1)
!
          XRK = X(ICRIT,5)
!         0.9999 TO GO ON THE RELEVANT SIDE OF THE SOLUTION
          ZRK = 0.9999D0*ZCRIT
          IC  = ICRIT + 1
!
      DO N=1,NRK
!
!       PREDICTION
        XRKPR = XRK + PRK
        ZRKPR = ZRK + PRK*F(ZRK,IC-1,QFIXG,PRAD,ST)
!       CORRECTION
        XRKCO = XRKPR
        ZRKCO = ZRK + PRK*(F(ZRK,IC-1,QFIXG,PRAD,ST)+
     &                     F(ZRKPR,IC-1,QFIXG,PRAD,ST))/2.D0
!
!       ARE WE OUTSIDE OF THE CURRENT ELEMENT?
        IF (XRKCO.GE.X(IC,5)) THEN
!         COMPUTE Z AT THE NODE THROUGH A LINEAR INTERPOLATION
          ZV(IC) = (ZRKCO-ZRK)/PRK*(X(IC,5)-XRK)+ ZRK
!         CHANGE THE CURRENT ELEMENT
          IC = IC+1
        ENDIF
!
!       ACTUALISATION
!
        XRK = XRKCO
        ZRK = ZRKCO
!
      ENDDO
!
!     LOOK FOR A HYDRAULIC JUMP
!
      IF (NOEUDF.EQ.IM) GOTO 120
      DO NOEUD=NOEUDF,IM
!       SUBCRITICAL CONJUGATE DEPTH H1*H2(H1+H2)=2HC**3
        ZFLU = ZVF(NOEUD)
        IF(ICONS.EQ.1) THEN
          RES = (-ZFLU**2+SQRT(ZFLU**4+8*ZFLU*ZCRIT**3))/(2*ZFLU)
        ELSEIF(ICONS.EQ.2) THEN
          RES = (ZCRIT**3/2.D0+SQRT(ZCRIT**6/4.D0
     &                   +2.D0*ZFLU**3*ZCRIT**3))/(2*ZFLU**2)
        ELSE
          WRITE(LU,*) 'ICONS = 1 OU 2, PAS ',ICONS
          STOP
        ENDIF
        IF (RES.LE.ZV(NOEUD)) THEN
          DO INOEUD=NOEUD+1,IM
            ZV(INOEUD)=ZVF(INOEUD)
          ENDDO
          GOTO 60
        ENDIF
      ENDDO
!     NO HYDRAULIC JUMP
120   CONTINUE
!
60    CONTINUE
!
!-----------------------------------------------------------------------
!
      DO I=1,IM
        DO J=1,JM
          HN(I,J) = ZV(I)
          U(I,J) = QFIXG/HN(I,J)
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

