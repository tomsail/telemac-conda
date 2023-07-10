!                   **********************
                    SUBROUTINE SOURCE_TRAC
!                   **********************
!
!***********************************************************************
! TELEMAC3D   V8P4
!***********************************************************************
!
!brief    PREPARES SOURCE TERMS FOR DIFFUSION OF TRACERS.
!
!history  CDG/SOGREAH
!+        **/06/2001
!+
!+   TRACER SOURCES
!
!history  J-M HERVOUET (LNHE)
!+        21/10/2004
!+        V5P5
!+
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
!history  A. GINEAU, N. DURAND, N. LORRAIN, C.-T. PHAM (LNHE)
!+        09/07/2014
!+        V7P0
!+   Adding an example of the penetration of the solar radiation
!+   for exchange with atmosphere
!
!history  A. LEROY (LNHE)
!+        25/11/2015
!+        V7P1
!+   Remove the call to INTERPMETEO: all the meteo variables are now
!+   read in meteo.f and stored in variables of waqtel
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| LT             |-->| ITERATION NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC,ONLY : COUPLING
      USE DECLARATIONS_TELEMAC3D
      USE METEO_TELEMAC !, ONLY: PATMOS
      USE INTERFACE_WAQTEL
      USE INTERFACE_KHIONE
      USE INTERFACE_TELEMAC3D,EX_SOURCE_TRAC =>SOURCE_TRAC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE

!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!----------------------------------------------------------------------
!
      INTEGER ITRAC
!
!----------------------------------------------------------------------
!
!     SETS SOURCE TERMS TO ZERO
!
      IF(NTRAC.GE.1) THEN
!
        CALL OS( 'X=0     ' , X=S0TA )
        CALL OS( 'X=0     ' , X=S1TA )
!
!       SOURCE TERMS SIMPLY MARKED
!
!       BEWARE, PUT Q INSTEAD OF 0 IN TYPR IF NOT NIL
!
        DO ITRAC=1,NTRAC
          S0TA%ADR(ITRAC)%P%TYPR='0'
          S1TA%ADR(ITRAC)%P%TYPR='0'
        ENDDO
!
!       EXAMPLE OF RADIOACTIVE DECAY E**(-KT) ON FIRST TRACER, HERE C=K
!
!       S1TA%ADR(1)%P%TYPR='Q'
!       CALL OS('X=C     ',X=S1TA%ADR(1)%P,C=1.D0)
!
      ENDIF
!***********************************************************************
!     WATER QUALITY COUPLING
!***********************************************************************
      IF(INCLUS(COUPLING,'WAQTEL'))THEN
!
!       ACTIVATE IMPLICIT SOURCE TERMS
!        IF(LT.EQ.1) CALL YASMI_WAQ(YASMI)
!
!       MAIN ROUTINE FOR WATER QUALITY
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF SOURCE_WAQ'
!                     TEXP,TIMP,TN
        CALL SOURCE_WAQ
     &  (NPOIN3,NPOIN2,S0TA,S1TA,TA,HPROP,U,V,CF,
     &   T3_01,T3_02,T3_03,T3_04,T3_05,T3_07,T3_08,
     &   T3_09,T3_10,T3_11,T3_12,T3_13,
     &   T2_01,T2_02,T2_03,
     &   PATMOS,3,NPLAN,
     &   LATIT,LONGIT,AT,MARDAT,MARTIM,ZPROP,DT,RHO)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM SOURCE_WAQ'
!
      ENDIF
!***********************************************************************
!     KHIONE COUPLING
!***********************************************************************
      IF(INCLUS(COUPLING,'KHIONE'))THEN
!
!       MAIN ROUTINE FOR FRAZIL
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF SOURCE_FRAZIL'
!
        CALL SOURCE_FRAZIL
!                  TEXP,TIMP,TN
     &    ( NPOIN2,S0TA,S1TA,TRN,HPROP,U,V,
     &      DT, CF, AK, EP, ITURBH,LT,NPLAN)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM SOURCE_FRAZIL'
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF SOURCE_RAD'
        CALL SOURCE_RAD(NPOIN3,NPOIN2,NPLAN,ZPROP%R,RHO%R,S0TA)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM OF SOURCE_RAD'
!
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
