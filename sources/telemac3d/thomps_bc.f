!                   ********************
                    SUBROUTINE THOMPS_BC
!                   ********************
!
     &(OPTION)
!
!***********************************************************************
! TELEMAC3D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    SAVING, CHANGING, AND RESTORING
!+        BOUNDARY CONDITIONS FOR THOMPSON
!+
!
!history  J-M HERVOUET (LNHE)
!+        20/09/2011
!+        V6P2
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| OPTION         |-->| 1: SAVING CONDITIONS
!|                |   | 2: CHANGING CONDITIONS
!|                |   | 3: RESTORING CONDITIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN) :: OPTION
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPLAN,IPTFR,IPTFR3,IFRLIQ,N
!
!***********************************************************************
!
      IF(OPTION.EQ.1) THEN
!
!       SAVING
!
        DO IPTFR = 1 , NPTFR2
          LIHBOR_USER%I(IPTFR) = LIHBOR%I(IPTFR)
        ENDDO
        DO IPTFR3 = 1 , NPTFR3
          LIUBOL_USER%I(IPTFR3) = LIUBOL%I(IPTFR3)
          LIVBOL_USER%I(IPTFR3) = LIVBOL%I(IPTFR3)
        ENDDO
!
      ELSEIF(OPTION.EQ.2) THEN
!
!       CHANGING
!
        DO IPTFR = 1 , NPTFR2
          IFRLIQ=NUMLIQ%I(IPTFR)
          IF(IFRLIQ.GT.0) THEN
            N=MESH2D%NBOR%I(IPTFR)
            IF(FRTYPE(IFRLIQ).EQ.2.AND.H%R(N).GT.1.D-3) THEN
              LIHBOR%I(IPTFR) = KENT
              DO IPLAN=1,NPLAN
                IPTFR3 = (IPLAN-1)*NPTFR2 + IPTFR
                LIUBOL%I(IPTFR3) = KENTU
                LIVBOL%I(IPTFR3) = KENTU
!               SECOND DIMENSION, SEE CVDF3D
                LIUBOL%I(IPTFR3+NPTFR3) = KENTU
                LIVBOL%I(IPTFR3+NPTFR3) = KENTU
!               LIWBOL%I(IPTFR3) = KADH
              ENDDO
            ENDIF
          ENDIF
        ENDDO
!
      ELSEIF(OPTION.EQ.3) THEN
!
!       RESTORING BOUNDARY CONDITIONS
!
        DO IPTFR = 1 , NPTFR2
          LIHBOR%I(IPTFR)=LIHBOR_USER%I(IPTFR)
        ENDDO
        DO IPTFR3 = 1 , NPTFR3
          LIUBOL%I(IPTFR3)=LIUBOL_USER%I(IPTFR3)
          LIVBOL%I(IPTFR3)=LIVBOL_USER%I(IPTFR3)
        ENDDO
!
      ELSE
!
!       ERROR ON OPTION
!
        WRITE(LU,*) 'THOMPSON_BC: WRONG OPTION ',OPTION
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!======================================================================
!
      RETURN
      END
