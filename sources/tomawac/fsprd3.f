!                   *****************
                    SUBROUTINE FSPRD3
!                   *****************
!
     &( FRA   , NDIRE , SPRED1, TETA1 , SPRED2, TETA2 , XLAMDA)
!
!***********************************************************************
! TOMAWAC   V6P1                                   15/06/2011
!***********************************************************************
!
!brief    COMPUTES THE BIMODAL DIRECTIONAL SPREADING FUNCTION
!+                FOR A RANGE OF DIRECTIONS.
!code
!+              2S
!+           COS  ((T-T0)/2.)    (MITSUYASU)
!
!history  M. BENOIT
!+        07/11/96
!+        V1P2
!+   CREATED
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
!history  G.MATTAROLO (EDF - LNHE)
!+        15/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FRA            |<--| DIRECTIONAL SPREADING FUNCTION VALUES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| SPRED1         |-->| DIRECTIONAL SPREAD 1
!| SPRED2         |-->| DIRECTIONAL SPREAD 1
!| TETA1          |-->| MAIN DIRECTION 1
!| TETA2          |-->| MAIN DIRECTION 2
!| XLAMDA         |-->| WEIGHTING FACTOR FOR FRA
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : TETA
!
      USE INTERFACE_TOMAWAC, EX_FSPRD3 => FSPRD3
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER,INTENT(IN)             :: NDIRE
      DOUBLE PRECISION,INTENT(IN)    :: SPRED1, TETA1 , SPRED2, TETA2
      DOUBLE PRECISION,INTENT(IN)    :: XLAMDA
      DOUBLE PRECISION,INTENT(INOUT) :: FRA(NDIRE)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP
      DOUBLE PRECISION DELT1 , DELT2 , FTH   , FRA1  , FRA2  , ARGUM
      DOUBLE PRECISION ARGMI1, ARGMI2
      DELT1 = 0.5D0/DELFRA(SPRED1)
      DELT2 = 0.5D0/DELFRA(SPRED2)
      IF (SPRED1.GT.1.D-1) THEN
        ARGMI1=10.D0**(-4.D0/SPRED1)
      ELSE
        ARGMI1=0.D0
      ENDIF
      IF (SPRED2.GT.1.D-1) THEN
        ARGMI2=10.D0**(-4.D0/SPRED2)
      ELSE
        ARGMI2=0.D0
      ENDIF
!
      DO JP=1, NDIRE
        FTH = TETA(JP)
!
        ARGUM = ABS(COS(0.5D0*(FTH-TETA1)))
        IF (ARGUM.GT.ARGMI1) THEN
          FRA1=DELT1*ARGUM**(2.D0*SPRED1)
        ELSE
          FRA1=0.D0
        ENDIF
!
        ARGUM = ABS(COS(0.5D0*(FTH-TETA2)))
        IF (ARGUM.GT.ARGMI2) THEN
          FRA2=DELT2*ARGUM**(2.D0*SPRED2)
        ELSE
          FRA2=0.D0
        ENDIF
!
        FRA(JP)=XLAMDA*FRA1+(1.D0-XLAMDA)*FRA2
        IF (FRA(JP).LT.1.D-10) FRA(JP)=0.D0
      ENDDO ! JP
!
      RETURN
      END
