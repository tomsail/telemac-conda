!                   *****************
                    SUBROUTINE USTAR1
!                   *****************
!
     &( USTAR , Z0    , TAUWAV,  NPOIN2)
!
!***********************************************************************
! TOMAWAC   V6P1                                   29/06/2011
!***********************************************************************
!
!brief    COMPUTES THE FRICTION VELOCITY AND ROUGHNESS LENGTH
!+                FOR ALL THE NODES IN THE 2D MESH.
!+                BASED ON JANSSEN (1989, 1991).
!
!note     COMPUTES TAUT FROM UVENT AND TAUW IN SUBROUTINE 'TAUTOT'.
!
!reference  JANSSEN P.A.E.M (1989) :
!+                     "WIND-INDUCED STRESS AND THE DRAG OF AIR
!+                      FLOW OVER SEA WAVES". JPO, VOL 19, PP 745-754.
!reference JANSSEN P.A.E.M (1991) :
!+                     "QUASI-LINEAR THEORY OF WIND-WAVE GENERATION
!+                      APPLIED TO WAVE FORECASTING". JPO, VOL 21, PP 1631-1642.
!
!history  M. BENOIT (EDF/DER/LNH)
!+        25/04/95
!+        V1P0
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
!history  G.MATTAROLO (EDF - LNHE)
!+        29/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| TAUWAV         |-->| STRESS DUE TO WAVES
!| USTAR          |<--| FRICTION VELOCITY
!| Z0             |<--| SURFACE ROUGHNESS LENGTH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TOMAWAC, EX_USTAR1 => USTAR1
      USE DECLARATIONS_TOMAWAC, ONLY : GRAVIT, ALPHA, UV, VV
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN)    ::  NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: TAUWAV(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: USTAR(NPOIN2), Z0(NPOIN2)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  ITRMIN, ITRMAX, ITR   , IP
      DOUBLE PRECISION TAUT  , UVENT , TAUW  , USMIN , SEUIL , X
      DOUBLE PRECISION USTEMP
!
!
      USMIN =1.D-6
      SEUIL =1.D-7
      ITRMIN=1
      ITRMAX=15
!
!.....MAIN LOOP ON THE NODES OF THE 2D MESH
!     """""""""""""""""""""""""""""""""""""""""""""""""""""
      DO IP=1,NPOIN2
!
!.......COMPUTES THE TOTAL STRESS
!     """""""""""""""""""""""""""""""
        UVENT=SQRT(UV(IP)**2+VV(IP)**2)
        TAUW =TAUWAV(IP)
        CALL TAUTOT
     &( TAUT  , UVENT , TAUW  , SEUIL ,
     &  ITR   , ITRMIN, ITRMAX)
!
!.......COMPUTES THE FRICTION VELOCITY
!       """""""""""""""""""""""""""""""""""
        USTAR(IP)=SQRT(TAUT)
!
!.......COMPUTES TEH ROUGHNESS LENGTH
!       """"""""""""""""""""""""""""""""""
        USTEMP=MAX(USTAR(IP),USMIN)
        X     =MIN(TAUWAV(IP)/USTEMP**2,0.999D0)
        Z0(IP)=ALPHA*USTEMP**2/(GRAVIT*SQRT(1.D0-X))
!
      ENDDO
!
      RETURN
      END
