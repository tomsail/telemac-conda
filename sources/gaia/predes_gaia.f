!                   **********************
                    SUBROUTINE PREDES_GAIA
!                   **********************
!
     &(LLT,AAT,YAGOUT,CODE,LISTCOUNT)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief    Prepares the variables which will be written to
!!          the results file or to the listing.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in] AAT       Current time (for building solutions)
!>@param[in] CODE      Name of calling programme (telemac2d or 3d)
!>@param[in] LLT       Local lt
!>@param[in] LISTCOUNT Listing printout period
!>@param[in] YAGOUT    If yes graphic output anyway (steered by t2d)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_GAIA
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN) :: LLT,LISTCOUNT
      DOUBLE PRECISION , INTENT(IN) :: AAT
      CHARACTER(LEN=24) , INTENT(IN) :: CODE
      LOGICAL          , INTENT(IN) :: YAGOUT
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER LTT,I,J,K
      LOGICAL IMP,LEO
!
!-----------------------------------------------------------------------
!
!     THE OUTPUT VARIABLES ARE BUILT ONLY IF NECESSARY, HENCE THE
!     FOLLOWING TESTS, WHICH MUST BE THE SAME AS IN DESIMP (BIEF LIBRARY)
!
      IMP=.FALSE.
      LEO=.FALSE.
      LTT=(LLT/LISTCOUNT)*LISTCOUNT
      IF(LLT.EQ.LTT.AND.LLT.GE.PTINIL) IMP=.TRUE.
!     IF CODE =TELEMAC2D OUTOUT IS MANAGED BY T2D
      IF(CODE(8:9).EQ.'2D'.OR.CODE(8:9).EQ.'3D') LEO=YAGOUT
!
!     NO PRINTOUTS REUIRED: LEAVING
      IF (.NOT.(LEO.OR.IMP)) GO TO 1000
!
!=======================================================================
!     COMPUTES SECONDARY VARIABLES
!=======================================================================
!
!     FREE SURFACE: H+ZF
!
      IF((LEO.AND.SORLEO(4)).OR.(IMP.AND.SORIMP(4))) THEN
        CALL OS('X=Y+Z   ',X=Z,Y=HN,Z=ZF)
      ENDIF
!
!     DISCHARGE
!
      IF((LEO.AND.SORLEO(6)).OR.(IMP.AND.SORIMP(6))) THEN
        DO I=1,NPOIN
          Q%R(I)=HN%R(I)*SQRT(U2D%R(I)**2+V2D%R(I)**2)
        ENDDO
      ENDIF
!
!     DISCHARGE ALONG X
!
      IF((LEO.AND.SORLEO(7)).OR.(IMP.AND.SORIMP(7))) THEN
        CALL OS('X=YZ    ',X=QU,Y=U2D,Z=HN)
      ENDIF
!
!     DISCHARGE ALONG Y
!
      IF((LEO.AND.SORLEO(8)).OR.(IMP.AND.SORIMP(8))) THEN
        CALL OS('X=YZ    ',X=QV,Y=V2D,Z=HN)
      ENDIF
!
!=======================================================================
!
!     VARIABLES WHICH ARE NOT INITIALISED AT THE FIRST CALL OF PREDES_GAIA
!
      IF(LLT.EQ.0) THEN
!       JMH ON 27/11/2009
        IF((LEO.AND.SORLEO(19)).OR.(IMP.AND.SORIMP(19))) THEN
          CALL OS('X=0     ',X=KS)
        ENDIF
      ENDIF
!
!=======================================================================
! UPDATE THE Values of the writing blocks
!=======================================================================
!
      ! For memory optimisation (from intel debug)
      ! For RATIOS
      IF ((LEO.AND.SORLEO(NVAR_RATIOS+1)).OR.
     &    (IMP.AND.SORIMP(NVAR_RATIOS+1))) THEN
        DO K=1,NOMBLAY
          DO I=1,NSAND
            RATIOS%ADR(K+(I-1)*NOMBLAY)%P%R=RATIO_SAND(I,K,1:NPOIN)
          ENDDO
        ENDDO
      ENDIF
      ! For RATIOM
      IF((LEO.AND.SORLEO(NVAR_RATIOM+1)).OR.
     &   (IMP.AND.SORIMP(NVAR_RATIOM+1))) THEN
        DO K=1,NOMBLAY
          DO I=1,NMUD
            RATIOM%ADR(K+(I-1)*NOMBLAY)%P%R=RATIO_MUD(I,K,1:NPOIN)
          ENDDO
        ENDDO
      ENDIF
      ! For Layconc
      IF((LEO.AND.SORLEO(NVAR_LAYCONC+1)).OR.
     &   (IMP.AND.SORIMP(NVAR_LAYCONC+1))) THEN
        DO K=1,NOMBLAY
          LAYCONC%ADR(K)%P%R=CONC_MUD(K,1:NPOIN)
        ENDDO
      ENDIF
      ! For mass_s
      IF((LEO.AND.SORLEO(NVAR_MASS_S+1)).OR.
     &   (IMP.AND.SORIMP(NVAR_MASS_S+1))) THEN
        DO K=1,NOMBLAY
          DO I=1,NSAND
            MASS_S%ADR(K+(I-1)*NOMBLAY)%P%R=MASS_SAND(I,K,1:NPOIN)
          ENDDO
        ENDDO
      ENDIF
      ! For mass_m
      IF((LEO.AND.SORLEO(NVAR_MASS_M+1)).OR.
     &   (IMP.AND.SORIMP(NVAR_MASS_M+1))) THEN
        DO K=1,NOMBLAY
          DO I=1,NMUD
            MASS_M%ADR(K+(I-1)*NOMBLAY)%P%R=MASS_MUD(I,K,1:NPOIN)
          ENDDO
        ENDDO
      ENDIF
      ! For mtransfer
      IF((LEO.AND.SORLEO(NVAR_MTRANS+1)).OR.
     &   (IMP.AND.SORIMP(NVAR_MTRANS+1))) THEN
        DO K=1,NOMBLAY
          MTRANSFER%ADR(K)%P%R=TRANS_MASS(K,1:NPOIN)
        ENDDO
      ENDIF
      ! For tocemud
      IF((LEO.AND.SORLEO(NVAR_TOCEMUD+1)).OR.
     &   (IMP.AND.SORIMP(NVAR_TOCEMUD+1))) THEN
        DO K=1,NOMBLAY
          TOCEMUD%ADR(K)%P%R=TOCE_MUD(K,1:NPOIN)
        ENDDO
      ENDIF
      ! For partheniades
      IF((LEO.AND.SORLEO(NVAR_PARTHE+1)).OR.
     &   (IMP.AND.SORIMP(NVAR_PARTHE+1))) THEN
        DO K=1,NOMBLAY
          PARTHE%ADR(K)%P%R=PARTHENIADES(K,1:NPOIN)
        ENDDO
      ENDIF
!
!=======================================================================
! UPDATE THE POINTERS TO THE DIFFERENTIATED VARIABLES
!=======================================================================
!
      J = NVAR_ADVAR
      DO I = 1,NADVAR
        IF((LEO.AND.SORLEO(J)).OR.(IMP.AND.SORIMP(J))) THEN
          CALL AD_GET_GAIA(I,ADVAR%ADR(I)%P)
          J = J + 1
        ENDIF
      ENDDO
!
!=======================================================================
!
1000  CONTINUE
!
!=======================================================================
!
      RETURN
      END
