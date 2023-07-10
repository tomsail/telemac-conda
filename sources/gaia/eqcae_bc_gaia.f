!                   ************************
                    SUBROUTINE EQCAE_BC_GAIA
!                   ************************
!
     &(LITBOR,TBOR,TN,J,KENT)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Imposes the equilibrium concentration for the inflow node
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param [in]     LITBOR   Type of boundary conditions on tracers
!>@param [in,out] TBOR     Block with prescribed values of tracers
!>@param [in,out] TN       Tracers at time n
!>@param [in]     J        Number of the sediment class considered
!>@param [in]     ROUSE    Bief obj containing the Rouse profile
!>@param [in]     KENT     Convention for prescribed value at entrance
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_GAIA
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: LITBOR(NPTFR),J,KENT
      DOUBLE PRECISION, INTENT(INOUT) :: TBOR(NPTFR),TN(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,K
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DO K = 1, NPTFR
        IF(LITBOR(K).EQ.KENT) THEN
          I = MESH%NBOR%I(K)
          IF(.NOT.SEDCO(J)) THEN
            TBOR(K) = CSTAEQ%ADR(J)%P%R(I)/CSRATIO%ADR(J)%P%R(I)
          ELSE
            TBOR(K) = FLUER%ADR(J)%P%R(I)/XWC(J)
          ENDIF
!         THIS IS THE CONDITION TO HAVE NO EVOLUTION
!         CS%R(I) MAY BE DIFFERENT FROM CBOR%R(K) IF UNSTEADY FLOW
!         OR IF DIRFLU.EQ.2 (CASE OF PRIORITY TO FLUXES)
          FLUER%ADR(J)%P%R(I) = FLUDPT%ADR(J)%P%R(I)*TN(I)
        ENDIF
      ENDDO
!
      RETURN
      END
