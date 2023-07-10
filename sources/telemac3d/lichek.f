!                   *****************
                    SUBROUTINE LICHEK
!                   *****************
!
     &(LIMPRP,NPTFR,IKLBOR,NELEB2,NELEBX2)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    HARMONISES THE BOUNDARY CONDITIONS, INITIALISES 'IBOR'.
!
!warning  THE ORDER OF THE INDICES HAS CHANGED FOR THIS VARIABLE
!+            BECAUSE OF THE 2D ADVECTION BY CHARACTERISTICS OF H
!+            WITH TIDAL FLATS (PASSES IBOR INSTEAD OF IFABOR)
!code
!+      BEFORE :   IBOR(IELEM,IETAGE,IFACE)
!+      NOW :      IBOR(IELEM,IFACE,IETAGE)
!warning  JAJ: DOES NOT HARMONISE THE BC'S IN THE NON-HYDROSTATIC
!+            CASE. IN PRINCIPLE, BC TYPES CAN BE DIFFERENT FOR U, V
!+            AND W VELOCITY COMPONENTS
!
!history  JACEK A. JANKOWSKI - UNIVERSITAET HANNOVER
!+        **/12/98
!+
!+   NON-HYDROSTATIC VERSION
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  JMH
!+        15/09/2008
!+
!+   TREATS KP1BOR IN PARALLEL
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        19/03/2014
!+        V7P0
!+   Boundary segments have now their own numbering, independent of
!+   boundary points numbering.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IKLBOR         |-->| CONNECTIVITY OF BOUNDARY SEGMENTS IN D
!| LIMPRP         |<->| TYPES OF BOUNDARY CONDITIONS FOR PROPAGATION
!|                |   | BY  POINTS   :    .1:H  .2:U  .3:V
!|                |   | BY  EDGES    :    .4:H  .5:U  .6:V
!| NELEB2         |-->| NUMBER OF BOUNDARY ELEMENTS IN 2D.
!| NELEBX2        |-->| MAXIMUM NUMBER OF BOUNDARY ELEMENTS IN 2D.
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS IN 2D
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
      INTEGER, INTENT(IN)    :: NPTFR,NELEB2,NELEBX2
      INTEGER, INTENT(INOUT) :: IKLBOR(NELEBX2,2),LIMPRP(NELEBX2,6)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN2, IPTFR, IPTFR3, IPTFRX,I,IELEB
      DOUBLE PRECISION C
!
!***********************************************************************
!
! HARMONISES THE BOUNDARY CONDITIONS
!
!=======================================================================
!
      IF (.NOT.NONHYD) THEN
!
        DO IPOIN2 = 1,NPOIN2
          IF(LIUBOF%I(IPOIN2).EQ.KLOG.OR.
     &       LIVBOF%I(IPOIN2).EQ.KLOG.OR.
     &       LIWBOF%I(IPOIN2).EQ.KLOG) THEN
            LIUBOF%I(IPOIN2) = KLOG
            LIVBOF%I(IPOIN2) = KLOG
            LIWBOF%I(IPOIN2) = KLOG
          ENDIF
          IF(LIUBOS%I(IPOIN2).EQ.KLOG.OR.
     &       LIVBOS%I(IPOIN2).EQ.KLOG.OR.
     &       LIWBOS%I(IPOIN2).EQ.KLOG) THEN
            LIUBOS%I(IPOIN2) = KLOG
            LIVBOS%I(IPOIN2) = KLOG
            LIWBOS%I(IPOIN2) = KLOG
          ENDIF
        ENDDO
!
        DO IPTFR3 = 1,NPTFR3
          IF(LIUBOL%I(IPTFR3).EQ.KLOG .OR.
     &       LIVBOL%I(IPTFR3).EQ.KLOG .OR.
     &       LIWBOL%I(IPTFR3).EQ.KLOG) THEN
            LIUBOL%I(IPTFR3) = KLOG
            LIVBOL%I(IPTFR3) = KLOG
            LIWBOL%I(IPTFR3) = KLOG
          ENDIF
        ENDDO
!
      ENDIF  ! (IF .NOT.NONHYD)
!
!=======================================================================
!
! INITIALISES THE BOUNDARY CONDITIONS FOR PROPAGATION IN 2D:
!
!=======================================================================
!
!     NODAL VALUES
!
      DO IPTFR = 1,NPTFR
!
!   BOUNDARY CONDITIONS ON H
!
      IF(LIHBOR%I(IPTFR).EQ.KENT ) THEN
        LIMPRP(IPTFR,1) = KDIR
      ELSEIF(LIHBOR%I(IPTFR).EQ.KSORT) THEN
        LIMPRP(IPTFR,1) = KDDL
      ELSEIF(LIHBOR%I(IPTFR).EQ.KLOG ) THEN
        LIMPRP(IPTFR,1) = KDDL
      ELSE
        WRITE(LU,102) IPTFR,LIHBOR%I(IPTFR)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!   BOUNDARY CONDITIONS ON U
!  (TAKES THE BOUNDARY CONDITIONS DEFINED ON THE SECOND PLANE
!   AS MEAN 2D VELOCITY)
!
      IPTFR3 = NPTFR + IPTFR
      IPTFRX = NPTFR + MESH2D%KP1BOR%I(IPTFR)
      IF(LIUBOL%I(IPTFR3).EQ.KENT.OR.
     &  LIUBOL%I(IPTFR3).EQ.KENTU.OR.
     &  LIUBOL%I(IPTFR3).EQ.KADH) THEN
        LIMPRP(IPTFR,2) = KDIR
      ELSEIF(LIUBOL%I(IPTFR3).EQ.KSORT) THEN
        LIMPRP(IPTFR,2) = KDDL
      ELSEIF(LIUBOL%I(IPTFR3).EQ.KLOG ) THEN
        LIMPRP(IPTFR,2) = KDDL
      ELSE
        WRITE(LU,202) IPTFR3,LIUBOL%I(IPTFR3)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!   BOUNDARY CONDITIONS ON V
!  (TAKES THE BOUNDARY CONDITIONS DEFINED ON THE SECOND PLANE
!   AS MEAN 2D VELOCITY)
!
      IF(LIVBOL%I(IPTFR3).EQ.KENT.OR.
     &  LIVBOL%I(IPTFR3).EQ.KENTU.OR.
     &  LIVBOL%I(IPTFR3).EQ.KADH) THEN
        LIMPRP(IPTFR,3) = KDIR
      ELSEIF(LIVBOL%I(IPTFR3).EQ.KSORT) THEN
        LIMPRP(IPTFR,3) = KDDL
      ELSEIF(LIVBOL%I(IPTFR3).EQ.KLOG ) THEN
        LIMPRP(IPTFR,3) = KDDL
      ELSE
        WRITE(LU,302) IPTFR3,LIVBOL%I(IPTFR3)
        CALL PLANTE(1)
        STOP
      ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
!     SEGMENT VALUES
!
      DO IELEB = 1,NELEB2
!
      IPTFR =IKLBOR(IELEB,1)
      IPTFRX=IKLBOR(IELEB,2)
!
!   BOUNDARY CONDITIONS ON H
!
      IF(LIHBOR%I(IPTFR).EQ.KENT ) THEN
        LIMPRP(IELEB,4) = KDDL
        IF(LIHBOR%I(IPTFRX).EQ.KENT) LIMPRP(IELEB,4) = KDIR
        IF(LIHBOR%I(IPTFRX).EQ.KLOG) LIMPRP(IELEB,4) = KNEU
      ELSEIF(LIHBOR%I(IPTFR).EQ.KSORT) THEN
        LIMPRP(IELEB,4) = KDDL
        IF(LIHBOR%I(IPTFRX).EQ.KLOG) LIMPRP(IELEB,4) = KNEU
      ELSEIF(LIHBOR%I(IPTFR).EQ.KLOG ) THEN
        LIMPRP(IELEB,4) = KNEU
      ELSE
        WRITE(LU,102) IPTFR,LIHBOR%I(IPTFR)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!   BOUNDARY CONDITIONS ON U
!  (TAKES THE BOUNDARY CONDITIONS DEFINED ON THE SECOND PLANE
!   AS MEAN 2D VELOCITY)
!
      IPTFR3 = NPTFR + IPTFR
      IPTFRX = NPTFR + IPTFRX
      IF(LIUBOL%I(IPTFR3).EQ.KENT.OR.
     &  LIUBOL%I(IPTFR3).EQ.KENTU.OR.
     &  LIUBOL%I(IPTFR3).EQ.KADH) THEN
        LIMPRP(IELEB,5) = KDDL
        IF(LIUBOL%I(IPTFRX).EQ.KENT.OR.
     &    LIUBOL%I(IPTFRX).EQ.KENTU) LIMPRP(IELEB,5) = KDIR
        IF(LIUBOL%I(IPTFRX).EQ.KADH) LIMPRP(IELEB,5) = KDIR
        IF(LIUBOL%I(IPTFRX).EQ.KLOG) LIMPRP(IELEB,5) = KNEU
      ELSEIF(LIUBOL%I(IPTFR3).EQ.KSORT) THEN
        LIMPRP(IELEB,5) = KDDL
        IF(LIUBOL%I(IPTFRX).EQ.KLOG) LIMPRP(IELEB,5) = KNEU
      ELSEIF(LIUBOL%I(IPTFR3).EQ.KLOG ) THEN
        LIMPRP(IELEB,5) = KNEU
      ELSE
        WRITE(LU,202) IPTFR3,LIUBOL%I(IPTFR3)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!   BOUNDARY CONDITIONS ON V
!  (TAKES THE BOUNDARY CONDITIONS DEFINED ON THE SECOND PLANE
!   AS MEAN 2D VELOCITY)
!
      IF(LIVBOL%I(IPTFR3).EQ.KENT.OR.
     &  LIVBOL%I(IPTFR3).EQ.KENTU.OR.
     &  LIVBOL%I(IPTFR3).EQ.KADH) THEN
        LIMPRP(IELEB,6) = KDDL
        IF(LIVBOL%I(IPTFRX).EQ.KENT.OR.
     &     LIVBOL%I(IPTFRX).EQ.KENTU) LIMPRP(IELEB,6) = KDIR
        IF(LIVBOL%I(IPTFRX).EQ.KADH) LIMPRP(IELEB,6) = KDIR
        IF(LIVBOL%I(IPTFRX).EQ.KLOG) LIMPRP(IELEB,6) = KNEU
      ELSEIF(LIVBOL%I(IPTFR3).EQ.KSORT) THEN
        LIMPRP(IELEB,6) = KDDL
        IF(LIVBOL%I(IPTFRX).EQ.KLOG) LIMPRP(IELEB,6) = KNEU
      ELSEIF(LIVBOL%I(IPTFR3).EQ.KLOG ) THEN
        LIMPRP(IELEB,6) = KNEU
      ELSE
        WRITE(LU,302) IPTFR3,LIVBOL%I(IPTFR3)
        CALL PLANTE(1)
        STOP
      ENDIF
!
      ENDDO
!
!=======================================================================
! FILLS MASK
!=======================================================================
!
      CALL OS('X=0     ',X=MASK)
!
      IF (.NOT. MSK) THEN
!
        DO IELEB = 1,NELEB2
          IF(LIMPRP(IELEB,5).EQ.KDIR) MASK%ADR(1)%P%R(IELEB) = 1.D0
          IF(LIMPRP(IELEB,6).EQ.KDIR) MASK%ADR(2)%P%R(IELEB) = 1.D0
          IF(LIMPRP(IELEB,5).EQ.KDDL) MASK%ADR(3)%P%R(IELEB) = 1.D0
          IF(LIMPRP(IELEB,6).EQ.KDDL) MASK%ADR(4)%P%R(IELEB) = 1.D0
          IF(LIMPRP(IELEB,5).EQ.KNEU) MASK%ADR(5)%P%R(IELEB) = 1.D0
          IF(LIMPRP(IELEB,6).EQ.KNEU) MASK%ADR(6)%P%R(IELEB) = 1.D0
          MASK%ADR(7)%P%R(IELEB) = 0.D0
          MASK%ADR(8)%P%R(IELEB) = 1.D0 - MASK%ADR(5)%P%R(IELEB)
        ENDDO
!
      ELSE
!
        DO IELEB = 1,NELEB2
          IPTFR=IKLBOR(IELEB,1)
!         MASKEL, BD ELEMENT FROM THE *FIRST* ETAGE
          C = MASKEL%R(MESH2D%NELBOR%I(IPTFR))
          IF(LIMPRP(IELEB,5).EQ.KDIR) MASK%ADR(1)%P%R(IELEB) = C
          IF(LIMPRP(IELEB,6).EQ.KDIR) MASK%ADR(2)%P%R(IELEB) = C
          IF(LIMPRP(IELEB,5).EQ.KDDL) MASK%ADR(3)%P%R(IELEB) = C
          IF(LIMPRP(IELEB,6).EQ.KDDL) MASK%ADR(4)%P%R(IELEB) = C
          IF(LIMPRP(IELEB,5).EQ.KNEU) MASK%ADR(5)%P%R(IELEB) = C
          IF(LIMPRP(IELEB,6).EQ.KNEU) MASK%ADR(6)%P%R(IELEB) = C
          MASK%ADR(7)%P%R(IELEB) = 0.D0
          MASK%ADR(8)%P%R(IELEB) = (1.D0 - MASK%ADR(5)%P%R(IELEB)) * C
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     EXTENDING TO 3D
!
      DO I=1,9
        CALL EXTMSK(MASK_3D%ADR(I)%P,MASK%ADR(I)%P%R,NPLAN-1,NELEB2)
      ENDDO
!
!-----------------------------------------------------------------------
!
102   FORMAT(' LICHEK: BOUNDARY NODE',I5,'LIHBOR = ',1I6)
202   FORMAT(' LICHEK: BOUNDARY NODE',I5,'LIUBOL = ',1I6)
302   FORMAT(' LICHEK: BOUNDARY NODE',I5,'LIVBOL = ',1I6)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
