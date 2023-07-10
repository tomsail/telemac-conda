      SUBROUTINE SURFINI
     &  (XLE,YLI,ZLI,XRI,YRI,ZRE,XM,YM,ZM,
     &   X,Y,ZS,ZF,IKLE,ELEM,NSEC,NPOIN2)

      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: NSEC,NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: XLE(NSEC),YLI(NSEC),ZLI(NSEC)
      DOUBLE PRECISION, INTENT(IN)    :: XRI(NSEC),YRI(NSEC),ZRE(NSEC)
      DOUBLE PRECISION, INTENT(INOUT) :: XM(2*NSEC),YM(2*NSEC)
      DOUBLE PRECISION, INTENT(INOUT) :: ZM(2*NSEC)
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN2), Y(NPOIN2),ZF(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: ZS(NPOIN2)
      INTEGER, INTENT(INOUT) :: IKLE(2*NSEC-2,3)
      INTEGER, INTENT(INOUT) :: ELEM(NPOIN2)
      DOUBLE PRECISION, ALLOCATABLE :: SHP(:,:)

      INTEGER ISEC, I, IE
      INTEGER N1, N2, N3
      DOUBLE PRECISION A1, A2, A3, SURDET

      ALLOCATE(SHP(NPOIN2,3))

      DO ISEC = 1,NSEC
        I = (ISEC-1)*2 + 1
        XM(I)   = XLE(ISEC)
        XM(I+1) = XRI(ISEC)
        YM(I)   = YLI(ISEC)
        YM(I+1) = YRI(ISEC)
        ZM(I)   = ZLI(ISEC)
        ZM(I+1) = ZRE(ISEC)
      END DO

      DO IE=1,2*NSEC-3,2
        IKLE(IE,1)   = IE
        IKLE(IE,2)   = IE+1
        IKLE(IE,3)   = IE+2
        IKLE(IE+1,1) = IE+1
        IKLE(IE+1,2) = IE+3
        IKLE(IE+1,3) = IE+2
      END DO

      DO I=1,NPOIN2
        ELEM(I) = 0
        SHP(I,1) = 0.0D0
        SHP(I,2) = 0.0D0
        SHP(I,3) = 0.0D0
        DO IE=1,2*NSEC-2
          N1 = IKLE(IE,1)
          N2 = IKLE(IE,2)
          N3 = IKLE(IE,3)
          A1 = (X(I)-XM(N3))*(YM(N2)-YM(N3))
     &       - (Y(I)-YM(N3))*(XM(N2)-XM(N3))
          A2 = (X(I)-XM(N1))*(YM(N3)-YM(N1))
     &       - (Y(I)-YM(N1))*(XM(N3)-XM(N1))
          A3 = (X(I)-XM(N2))*(YM(N1)-YM(N2))
     &       - (Y(I)-YM(N2))*(XM(N1)-XM(N2))
          IF ((A1.GE.0.).AND.(A2.GE.0.).AND.(A3.GE.0.)) THEN
            SURDET = 1.0 / ((XM(N2)-XM(N1))*(YM(N3)-YM(N1)) -
     &                      (YM(N2)-YM(N1))*(XM(N3)-XM(N1)))
            ELEM(I) = IE
            SHP(I,1) = A1 * SURDET
            SHP(I,2) = A2 * SURDET
            SHP(I,3) = A3 * SURDET
            EXIT
          ENDIF
        END DO
      END DO

      DO I=1,NPOIN2
        IF (ELEM(I)==0) THEN
          WRITE (LU,*) 'SURFINI: POINT ',I,
     &        ' IS OUTSIDE THE DOMAIN FOR FREE SURFACE INITIALISATION'
          ZS(I) = ZF(I)
        ELSE
          N1 = IKLE(ELEM(I),1)
          N2 = IKLE(ELEM(I),2)
          N3 = IKLE(ELEM(I),3)
          A1 = SHP(I,1)
          A2 = SHP(I,2)
          A3 = SHP(I,3)
          ZS(I) = A1*ZM(N1) + A2*ZM(N2) + A3*ZM(N3)
        ENDIF
      END DO

      DEALLOCATE(SHP)
      RETURN
      END

