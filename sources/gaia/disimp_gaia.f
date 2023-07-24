!                   **********************
                    SUBROUTINE DISIMP_GAIA
!                   **********************
!
     &(Q,Q2BOR,NUMLIQ,IFRLIQ,NSOLDIS,WORK1,QBOR,NPTFR,MASK,MESH)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief  Imposes solid discharge boundary conditions. Q2BOR is the
!!        discharge in m2/s, the integral of Q2BOR on the boundary QBOR
!!        is multiplied by a constant to get the correct discharge Q
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     IFRLIQ  Rank of liquid boundary
!>@param[in]     MASK    Block of masks for boundary conditions
!>@param[in]     MESH    Mesh structure
!>@param[in]     NPTFR   Number of boundary points
!>@param[in]     NSOLDIS Number of solid discharges given in parameter
!!                       file
!>@param[in]     NUMLIQ  Liquid boundary number of boundary points
!>@param[in]     Q       Prescribed value of discharge
!>@param[in,out] Q2BOR   Prescribed solid discharge
!>@param[in,out] WORK1   Work bief_obj structure
!>@param[in,out] QBOR    The resulting discharge in m3/s
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_SUM
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPTFR,IFRLIQ,NSOLDIS
      INTEGER, INTENT(IN)             :: NUMLIQ(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: MASK(NPTFR),Q
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: WORK1,QBOR
      TYPE(BIEF_OBJ), INTENT(IN)      :: Q2BOR
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,IELM,IELEB
!
      DOUBLE PRECISION Q1
!
      INTRINSIC ABS
!
!=======================================================================
!     COMPUTES FLUX
!=======================================================================
!
!     IN THE FOLLOWING LOOP ONE RESTRICTS THE MASK OF DIRICHLETS SEGMENTS
!     TO THOSE OF THE LIQUID BOUNDARY NUMBER IFRLIQ. AS NUMLIQ IS
!     DEFINED AT NODES, ONE RISKS AN ERROR FOR THE SEGMENT FOLLOWING
!     THE LAST NODE ON THE BOUNDARY. IN FACT THIS SEGMENT WILL BE SOLID
!     AND WILL HAVE A MASK ALREADY SET TO ZERO.
!
      CALL OS( 'X=0     ' , X=WORK1 )
!
      DO IELEB=1,MESH%NELEB
        K=MESH%IKLBOR%I(IELEB)
        IF(NUMLIQ(K).EQ.IFRLIQ) WORK1%R(IELEB)=MASK(IELEB)
      ENDDO
!
!     Q2BOR IS INTEGRATED ALONG THE BOUNDARY
!
      IELM=11
      CALL VECTOR(QBOR,'=','MASVEC          ',IELBOR(IELM,1),
!                      USED  VOID  VOID  VOID  VOID  VOID
     &            1.D0,Q2BOR,Q2BOR,Q2BOR,Q2BOR,Q2BOR,Q2BOR,
     &            MESH,.TRUE.,WORK1)
!
!=======================================================================
!     FINAL QBOR IF Q2BOR ONLY A PROFILE
!=======================================================================
!
      IF(NSOLDIS.GE.IFRLIQ) THEN
!
!       A VALUE OF DISCHARGE HAS BEEN GIVEN IN THE PARAMETER FILE
!       FOR THIS BOUNDARY. Q2BOR IS CONSIDERED AS ONLY A PROFILE
!
!       FOR THE USER: POSITIVE DISCHARGE = ENTERING
        Q1 = BIEF_SUM(QBOR)
        IF(NCSIZE.GT.1) Q1 = P_SUM(Q1)
!
        IF(ABS(Q1).LT.1.D-10) THEN
!         ZERO FLUX: WARNING MESSAGE
          IF(ABS(Q).GT.1.D-10) THEN
            WRITE(LU,31) IFRLIQ
31          FORMAT(1X,'DISIMP_GAIA : PROBLEM ON BOUNDARY NUMBER ',1I6,/,
     &      1X,
     &     '           GIVE A SOLID DISCHARGE PROFILE  ',/,1X,
     &     '           IN THE BOUNDARY CONDITIONS FILE')
            CALL PLANTE(1)
            STOP
          ELSE
            Q1 = 1.D0
          ENDIF
        ENDIF
!
        DO K=1,NPTFR
          IF(NUMLIQ(K).EQ.IFRLIQ) THEN
            QBOR%R(K) = QBOR%R(K) * Q / Q1
          ENDIF
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
