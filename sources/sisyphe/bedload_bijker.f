!                   *************************
                    SUBROUTINE BEDLOAD_BIJKER
!                   *************************
!
     &  (TOBW,TOB,MU,KSP,KSR,HN,NPOIN,DM,DENS,XMVE,GRAV,XWC,
     &   KARMAN,ZERO,T4,T7,T8,T9,QSC,QSS,BIJK,HOULE)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    BIJKER BEDLOAD TRANSPORT FORMULATION.
!
!history  C. MACHET; T. BOULET; E. BEN SLAMA
!+        26/11/2001
!+        V5P1
!+
!
!history  C. VILLARET
!+        10/03/2004
!+        V5P4
!+
!
!history  F. HUVELIN
!+        **/12/2004
!+        V5P6
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BIJK           |-->| COEFFICIENT OF THE BIJKER FORMULA
!| DENS           |-->| RELATIVE DENSITY
!| DM             |-->| SEDIMENT GRAIN DIAMETER
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| HN             |-->| WATER DEPTH
!| HOULE          |-->| LOGICAL, FOR WAVE EFFECTS
!| KARMAN         |-->| VON KARMAN CONSTANT
!| KSP            |-->| BED SKIN ROUGHNESS
!| KSR            |-->| RIPPLE BED ROUGHNESS
!| MU             |<->| CORRECTION FACTOR FOR BED ROUGHNESS
!| NPOIN          |-->| NUMBER OF POINTS
!| QSC            |<->| BED LOAD TRANSPORT
!| QSS            |<->| SUSPENDED LOAD TRANSPORT
!| T4             |<->| WORK BIEF_OBJ STRUCTURE
!| T7             |<->| WORK BIEF_OBJ STRUCTURE
!| T8             |<->| WORK BIEF_OBJ STRUCTURE
!| T9             |<->| WORK BIEF_OBJ STRUCTURE
!| TOB            |<->| BED SHEAR STRESS (TOTAL FRICTION)
!| TOBW           |-->| WAVE INDUCED SHEAR STRESS
!| XMVE           |-->| FLUID DENSITY
!| XWC            |-->| SETTLING VELOCITY
!| ZERO           |-->| ZERO
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,EX_BEDLOAD_BIJKER => BEDLOAD_BIJKER
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOBW, TOB, KSR,KSP, HN,MU
      INTEGER,          INTENT(IN)    :: NPOIN
      LOGICAL,          INTENT(IN)    :: HOULE
      DOUBLE PRECISION, INTENT(IN)    :: DM, DENS, XMVE, GRAV, XWC
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN, ZERO
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T4
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T7, T8, T9
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: QSC, QSS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                      :: I
      DOUBLE PRECISION             :: C1, C2, UCF
      DOUBLE PRECISION, INTENT(IN) :: BIJK
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
      ! ***************************************************** !
      ! I - STRESS UNDER THE COMBINED ACTION OF WAVES AND CURRENTS !
      ! ***************************************************** !
      IF(HOULE) THEN
        CALL OS('X=CY    ', X=T4, Y=TOBW, C= 0.5D0)
        CALL OS('X=X+Y   ', X=T4, Y=TOB)
      ELSE
        CALL OS('X=Y     ', X=T4, Y=TOB)
      ENDIF
      ! ******************************************************* !
      ! II - CORRECTION TO TAKE BED FORMS INTO ACCOUNT          !
      ! ******************************************************* !
!      CALL OS('X=Y/Z   ', X=MU, Y=CFP, Z=CF)
!      CALL OS('X=Y**C  ', X=MU, Y=MU , C=0.75D0)
      ! ***************************** !
      ! III - BEDLOAD TRANSPORT       !
      ! ***************************** !
      C1 = BIJK*DM
      C2 = DENS*DM*XMVE*GRAV
      DO I = 1, NPOIN
        IF (T4%R(I)*MU%R(I)> ZERO) THEN
          QSC%R(I) = C1*SQRT(TOB%R(I)/XMVE )
     &             * EXP(-0.27D0*(C2/(T4%R(I)*MU%R(I))))
        ELSE
          QSC%R(I) = 0.D0
        ENDIF
      ENDDO
      ! *********************************************************** !
      ! IV- ROUSE NUMBER AND LOWER BOUND OF EINSTEIN INTEGRAL       !
      ! *********************************************************** !
      DO I = 1, NPOIN
        IF (T4%R(I) > 0.D0) THEN
          UCF     = SQRT( T4%R(I) / XMVE)
          T7%R(I) = XWC / ( KARMAN * UCF )
!         AUX     = 1.D0 + KARMAN*SQRT(2.D0/MAX(CF%R(I),ZERO))
!         T8%R(I) = 30.D0*EXP(-AUX)
          T8%R(I) = MAX(KSR%R(I),KSP%R(I))/MAX(HN%R(I),ZERO)
        ELSE
          T7%R(I)= 100001.D0
          T8%R(I)= 100001.D0
        ENDIF
      ENDDO
      ! ************************************ !
      ! V - EINSTEIN INTEGRAL                !
      ! ************************************ !
      CALL INTEG(T7%R, T8%R, T9%R, NPOIN)
      ! ************************************** !
      ! VI - TRANSPORT BY SUSPENSION           !
      ! ************************************** !
      CALL OS('X=YZ    ', X=QSS, Y=T9, Z=QSC)
!======================================================================!
!======================================================================!
      RETURN
      END SUBROUTINE BEDLOAD_BIJKER
