!                   **********************
                    SUBROUTINE USER_CONLIT
!                   **********************
!
     &(NBOR,AT)
!
!***********************************************************************
! SISYPHE
!***********************************************************************
!
!brief    ALLOWS TO IMPOSE TIME VARYING BOUNDARY CONDITIONS
!+               (CONSTANT VALUES CAN BE DIRECTLY IMPOSED IN CONDIM
!+                INPUT FILE).
!+
!+
!+            ALLOWS TO IMPOSE A SAND TRANSPORT RATE AT SOME
!+                BOUNDARY NODES (QBOR AND LIQBOR). IT IS THEN NECESSARY
!+                TO ALSO IMPOSE LIEBOR = KSORT AT THESE NODES !
!
!history  E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!+        11/09/1995
!+
!+
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINT
!| AT             |-->| TEMPS (s)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE
      USE DECLARATIONS_TELEMAC
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_MAX
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: AT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
!       EXAMPLE 1: IMPOSED SOLID DISCHARGE - FREE BED EVOLUTION
!
!       QBOR%ADR(J)%P%R(K) IS THE SOLID DISCHARGE IMPOSED AT THE BOUNDARY
!       NODE K , CLASS OF SEDIMENT J, EXCLUDING VOIDS
!
!       LIEBOR%I(K)=KSORT
!       LIQBOR%I(K)=KENT
!
!       QBOR%ADR(1)%P%R(K)=1.D-4
!       QBOR%ADR(2)%P%R(K)=1.D-4 .....
!
!       EXAMPLE 2: IMPOSED BED EVOLUTON
!
!       LIEBOR%I(K)=KENT
!       (LIQBOR%I(K)=KSORT IS DONE IN SISYPHE.F)
!       IF(LIEBOR%I(K).EQ.KENT) THEN
!         EBOR%ADR(1)%P%R(K)=1.D-4
!         EBOR%ADR(2)%P%R(K)=1.D-4.....
!       ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
