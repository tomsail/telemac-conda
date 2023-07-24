!                       *****************
                        SUBROUTINE F1F1F1
!                       *****************
!
     &(F1SF,NF1,IQ_OM1)
!
!***********************************************************************
! TOMAWAC   V6P3                                   15/06/2011
!***********************************************************************
!
!brief   SUBROUTINE CALLED BY PRENL3
!+         COMPUTES VALUES OF RATIO F1/F AS FUNCTION OF THE IQ_OM1
!+         INDICATOR
!
!history  E. GAGNAIRE-RENOU
!+        04/2011
!+        V6P1
!+   CREATED
!
!history  G.MATTAROLO (EDF - LNHE)
!+        15/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  E. GAGNAIRE-RENOU
!+        12/03/2013
!+        V6P3
!+   Better formatted: WRITE(LU,*), etc.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F1SF           |-->|
!| IQ_OM1         |-->| SETTING FOR INTEGRATION ON OMEGA1
!| NF1            |-->|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TOMAWAC, EX_F1F1F1 => F1F1F1
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)    :: IQ_OM1
      INTEGER,          INTENT(INOUT) :: NF1
      DOUBLE PRECISION, INTENT(INOUT) :: F1SF(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,M
      DOUBLE PRECISION RAISON
!
!-----------------------------------------------------------------------
!
      IF(IQ_OM1.EQ.1) THEN
        IF(NF1.NE.14) THEN
          WRITE(LU,*) 'PROGRAM STOP IN F1F1F1 : WRONG VALUE FOR NF1'
          WRITE(LU,*) 'IQ_OM1 = ',IQ_OM1,'   AND NF1 = ',NF1
          CALL PLANTE(1)
          STOP
        ENDIF
        F1SF( 1)=0.30D0
        F1SF( 2)=0.40D0
        F1SF( 3)=0.50D0
        F1SF( 4)=0.60D0
        F1SF( 5)=0.70D0
        F1SF( 6)=0.80D0
        F1SF( 7)=0.90D0
        F1SF( 8)=1.00D0
        F1SF( 9)=1.11D0
        F1SF(10)=1.25D0
        F1SF(11)=1.42D0
        F1SF(12)=1.67D0
        F1SF(13)=2.00D0
        F1SF(14)=2.50D0
        F1SF(15)=3.30D0
      ELSEIF(IQ_OM1.EQ.2) THEN
        IF (NF1.NE.26) THEN
          WRITE(LU,*) 'PROGRAM STOP IN F1F1F1 : WRONG VALUE FOR NF1'
          WRITE(LU,*) 'IQ_OM1 = ',IQ_OM1,'   AND NF1 = ',NF1
          CALL PLANTE(1)
          STOP
        ENDIF
        F1SF( 1)=0.32D0
        F1SF( 2)=0.35D0
        F1SF( 3)=0.39D0
        F1SF( 4)=0.44D0
        F1SF( 5)=0.50D0
        F1SF( 6)=0.56D0
        F1SF( 7)=0.63D0
        F1SF( 8)=0.70D0
        F1SF( 9)=0.78D0
        F1SF(10)=0.86D0
        F1SF(11)=0.92D0
        F1SF(12)=0.97D0
        F1SF(13)=1.00D0
        F1SF(14)=1.03D0
        F1SF(15)=1.08D0
        F1SF(16)=1.13D0
        F1SF(17)=1.20D0
        F1SF(18)=1.28D0
        F1SF(19)=1.37D0
        F1SF(20)=1.48D0
        F1SF(21)=1.50D0
        F1SF(22)=1.65D0
        F1SF(23)=1.85D0
        F1SF(24)=2.10D0
        F1SF(25)=2.40D0
        F1SF(26)=2.70D0
        F1SF(27)=3.20D0
      ELSEIF(IQ_OM1.EQ.3) THEN
        IF(NF1.NE.11) THEN
          WRITE(LU,*) 'PROGRAM STOP IN F1F1F1 : WRONG VALUE FOR NF1'
          WRITE(LU,*) 'IQ_OM1 = ',IQ_OM1,'   AND NF1 = ',NF1
          CALL PLANTE(1)
          STOP
        ENDIF
        F1SF( 1)=0.30D0
        F1SF( 2)=0.48D0
        F1SF( 3)=0.64D0
        F1SF( 4)=0.78D0
        F1SF( 5)=0.90D0
        F1SF( 6)=1.00D0
        F1SF( 7)=1.12D0
        F1SF( 8)=1.28D0
        F1SF( 9)=1.50D0
        F1SF(10)=1.80D0
        F1SF(11)=2.40D0
        F1SF(12)=3.40D0
      ELSEIF(IQ_OM1.EQ.4) THEN
        IF(NF1.NE.40) THEN
          WRITE(LU,*) 'PROGRAM STOP IN F1F1F1 : WRONG VALUE FOR NF1'
          WRITE(LU,*) 'IQ_OM1 = ',IQ_OM1,'   AND NF1 = ',NF1
          CALL PLANTE(1)
          STOP
        ENDIF
        NF1=20
        M=10
        RAISON=9.D0**(1.D0/DBLE(NF1))
        F1SF(M+1)=1.0D0/3.0D0
        NF1=2*M+NF1
        DO I=M+2,NF1+1
          F1SF(I)=F1SF(I-1)*RAISON
        ENDDO
        DO I=M,1,-1
          F1SF(I)=F1SF(I+1)/RAISON
        ENDDO
      ELSEIF(IQ_OM1.EQ.5) THEN
        RAISON=9.D0**(1.D0/DBLE(NF1))
        F1SF(1)=1.D0/3.D0
        DO I=2,NF1+1
          F1SF(I)=F1SF(I-1)*RAISON
        ENDDO
      ELSEIF(IQ_OM1.EQ.6) THEN
        RAISON=(3.D0-1.D0/3.D0)/DBLE(NF1)
        F1SF(1)=1.D0/3.D0
        DO I=2,NF1+1
          F1SF(I)=F1SF(I-1)+RAISON
        ENDDO
      ELSEIF(IQ_OM1.EQ.7) THEN
        IF(NF1.NE.20) THEN
          WRITE(LU,*) 'PROGRAM STOP IN F1F1F1 : WRONG VALUE FOR NF1'
          WRITE(LU,*) 'IQ_OM1 = ',IQ_OM1,'   AND NF1 = ',NF1
          CALL PLANTE(1)
          STOP
        ENDIF
        F1SF( 1)=1.D0/3.D0
        F1SF( 2)=0.40D0
        F1SF( 3)=0.46D0
        F1SF( 4)=0.52D0
        F1SF( 5)=0.60D0
        F1SF( 6)=0.70D0
        F1SF( 7)=0.79D0
        F1SF( 8)=0.86D0
        F1SF( 9)=0.92D0
        F1SF(10)=0.97D0
        F1SF(11)=1.00D0
        F1SF(12)=1.04D0
        F1SF(13)=1.10D0
        F1SF(14)=1.18D0
        F1SF(15)=1.28D0
        F1SF(16)=1.42D0
        F1SF(17)=1.60D0
        F1SF(18)=1.84D0
        F1SF(19)=2.14D0
        F1SF(20)=2.52D0
        F1SF(21)=3.00D0
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
