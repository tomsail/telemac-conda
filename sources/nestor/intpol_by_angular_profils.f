!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  Intpol_by_angular_Profils      !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &( F, xA,yA,zA,  xB,yB,zB,  xC,yC,zC,  xD,yD,zD
     &   , ip, P1km, P2km, angleABCD
     &   , set_refZ, set_km     )
!
      USE m_TypeDefs_Nestor
      USE m_Nestor , ONLY : ipid
!
#ifndef NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  Intersection
     &                               , ErrMsgAndStop
#endif
!
!
!
!
      IMPLICIT NONE
      TYPE(t_Field) ,INTENT(INOUT) :: F
!
      REAL (KIND=R8),INTENT(IN)::  xA,yA,zA, xB,yB,zB      ! points:  A,B  ( profile1 )
     &                            ,xC,yC,zC, xD,yD,zD      !    "     C,D  ( profile2 )
      INTEGER,INTENT(IN)       ::  ip                      ! index of profile1
      REAL (KIND=R8),INTENT(IN)::  P1km, P2km              ! measure of river length: profile1, profile2
      REAL (KIND=R8),INTENT(IN)::  angleABCD               ! angle between profile1 and profile2
      LOGICAL,INTENT(IN)       ::  set_refZ, set_km
!
#ifndef NESTOR_INTERFACES
      !--------------------- local variables ---------------
      INTEGER         :: i
!
      REAL (KIND=R8)  :: xAB,yAB , xCD,yCD                 ! vetkors: AB, CD
      REAL (KIND=R8)  :: skaSASN                           ! skalar_produkt( SA, SN )
      REAL (KIND=R8)  :: xS,yS,  xN,yN                     ! points:  intersection S, field node N
      REAL (KIND=R8)  :: xSN,ySN                           ! vetkor: SN (between intersection S and field node N)
      REAL (KIND=R8)  :: xSA,ySA,  xSC,ySC                 ! vetkors: SA, SC
      REAL (KIND=R8)  :: rL1,rR1,  rL2,rR2,   rN           ! distances (radii): L1,R1,L2,R2,N to S
      REAL (KIND=R8)  :: zR1,zL1,  zL2,zR2,   zN           ! z values ( L1,R1,... see sketch below)
      REAL (KIND=R8)  :: DetSASN, DetSCSN                  ! determinants of Vektors:  SA SN  and  SC SN
      REAL (KIND=R8)  :: z1, z2                            ! interpolated z-values between zR1,zL1 and zL2,zR2
      REAL (KIND=R8)  :: phi                               ! angle between SA and SN
      REAL (KIND=R8)  :: rLphi, rRphi                      ! angle, radii as function of phi
      REAL (KIND=R8)  :: intpolFaktRadial, intpolFaktAngle ! interpolation factors
!
      REAL (KIND=R8), DIMENSION(4) ::    DistanceToS
      REAL (KIND=R8)               :: maxDistanceToS, minDistanceToS
      TYPE(t_String_Length) :: SRname                      ! name of current Subroutine
!
!
      !---------------------------------------------------------------
      !   angle between two vectors:
      !          skalarProd(AB,CD) = |AB|*|CD|*cos(angleABCD)
      !---------------------------------------------------------------
      !
      !
      !                     D (L2)
      !                     *
      !                      \                 vector AB:  xAB = xB - xA  ,   yAB = yB - yA
      !                       \                vector CD:  xCD = xD - xC  ,   yCD = yD - yC
      !                        \
      !                         \                   naming of points:
      !                          \                    L1 or B :  left  point of profile 1
      !              N            \                   R1 or A :  right point of profile 1
      !               *            \ C (R2)           L2 or D :  left  point of profile 2
      !                             *                 R2 or C :  right point of profile 2
      !                                               N       :  the Field node to interpolate a z-value
      !                                               S       :  intersection of line AB and CD
      !         *-------------------*  * S
      !         B                   A
      !        (L1)                (R1)
      !
      !
!      dbug WRITE(6,*)'?>-------  SR Intpol_by_angular_Profils ----'
      SRname%s = "Intpol_by_angular_Profils"                     ! subroutine name
!
      zL1 = zB            ! only a change of name to keep code easier readable
      zR1 = zA            !            "
      zL2 = zD            !            "
      zR2 = zC            !            "
!
      CALL Intersection( xA,yA, xB,yB, xC,yC, xD,yD,  xS,yS )
!      _______________________________________________________________
      !  check for intersection of profiles __________________________|
      IF(       ( xS <= MAX(xA,xB) .AND. xS > MIN(xA,xB))
     &     .OR. ( yS <= MAX(yA,yB) .AND. yS > MIN(yA,yB))
     &  )  Call ErrMsgAndStop(
     &  " ","reason: Profiles must not intersect or touch !"
     & ," ","        Fit profile  ", ip,  SRname, ipid       )
!
!
!
!
      xAB = xB - xA             ! vector AB: x component
      yAB = yB - yA             !     "      y component
      xCD = xD - xC             ! vector CD: x component
      yCD = yD - yC             !     "      y component
!
      xSA = xA - xS             ! vector SA: x component
      ySA = yA - yS             !     "      y component
      xSC = xC - xS             ! vector SC: x component
      ySC = yC - yS             !     "      y component
!
      !  calc radii ( = distance to intersection)
      rR1 = SQRT(  (xA-xS)**2 + (yA-yS)**2  )   ! Distance SA
      rL1 = SQRT(  (xB-xS)**2 + (yB-yS)**2  )   !    "     SB
      rR2 = SQRT(  (xC-xS)**2 + (yC-yS)**2  )   !    "     SC
      rL2 = SQRT(  (xD-xS)**2 + (yD-yS)**2  )   !    "     SD
!
      DistanceToS(1) = rR1
      DistanceToS(2) = rL1
      DistanceToS(3) = rR2
      DistanceToS(4) = rL2
      maxDistanceToS = MAXVAL( DistanceToS )
      minDistanceToS = MINVAL( DistanceToS )
!
!      ________________________________________________________________
      ! 1) do some tests if work on node(i) is required                |
      ! 2) calc. angle phi between SA,SN                               |
      ! 3) calc. left and right radius as a function of phi            |
      ! 4) calc. interpolation factor relating to the angles           |
      ! 5) calc. radial interpolation factor relating to the radii     |
      ! 6) interpolate z-value                  -> F%refZ(i)           |
      ! 7) interpolate measure of river length  -> F%km(i)             |
      DO i = 1 , F%nNodes !____________________________________________|
!
!        ______________________________________________________________
        !  Test if node was already successful processed ______________|
        IF( set_refZ ) THEN
          IF( F%refZ(i) > -1000.0D0 ) CYCLE
        ELSEIF ( set_km )THEN 
          IF( F%km(i)   > -1000.0D0 ) CYCLE
        ENDIF
!
        xN = F%X(i)   ! only a change of name to keep code readable
        yN = F%Y(i)   !            "
!
        xSN     = xN - xS                         ! vector S->N: x component
        ySN     = yN - yS                         ! vector S->N: y component
        DetSASN = xSA*ySN - ySA*xSN               ! determinate of vectors SA and SN
        DetSCSN = xSC*ySN - ySC*xSN               ! determinate of vectors SC and SN
!
!        ______________________________________________________________
        !  Before we do further calculations                           |
        !  we test if node is between the two profiles                 |
        !          if the sign of DetSASN and DetSCSN is the same      |
        !          Point N is not between the line-SA and line-SC _____|
        IF(     (DetSASN < 0.0D0 .AND. DetSCSN < 0.0D0)
     &      .OR.(DetSASN > 0.0D0 .AND. DetSCSN > 0.0D0)  )  CYCLE    ! node is not between !WRITE(6,'("Node not between lines ",2I3,g15.6)') ip, ip+1         ! debug
!
!
        rN = SQRT( xSN*xSN + ySN*ySN )            ! distance SN
!        ______________________________________________________________
        !  Before we do further calculations                           |
        !  test if node is between max or min radii ___________________|
        IF(     (  rN  > maxDistanceToS )
     &      .OR.(  rN  < minDistanceToS )   )  CYCLE  ! node is not between   !WRITE(6,'("Node not inside max or min radii ",2I3,g15.6)')      ! debug
!
        skaSASN = xSA*xSN + ySA*ySN               ! skalarProduct(AB,SN)
        phi     = ACOS( skaSASN / (rR1 * rN) )    ! angle between SA,SN
!
        intpolFaktAngle = phi/angleABCD               ! interpolation factor relating to the angles
!
        rLphi = rL1 + intpolFaktAngle * ( rL2 - rL1 ) ! left  radius as a function of phi     !WRITE(6,*)'rLPhi = ',rLPhi  ! debug
        rRphi = rR1 + intpolFaktAngle * ( rR2 - rR1 ) ! right radius as a function of phi     !WRITE(6,*)'rRPhi = ',rRPhi  ! debug
!
!        ______________________________________________________________
        !  Before we do further calculations                           |
        !  test if node is between valid radii(phi) ___________________|
        IF(     (  rN  > MAX(rLphi, rRphi) )
     &      .OR.(  rN  < MIN(rLphi, rRphi) )     )  CYCLE ! node is not between radii   !WRITE(6,'("Node not inside max or min radii ",2I3,g15.6)')      ! debug
!
!        ______________________________________________________________
        !  Check if phi is between the two profils  ___________________|
        IF( .NOT. (phi >= 0.0D0 .AND. phi <= angleABCD) )  CYCLE  ! node is not between angle !WRITE(6,'("Node not between the angle ",2I3,g15.6)')      ! debug
!
        intpolFaktRadial =  (rN - rRphi) / (rLphi- rRphi)     ! radial interpolation factor relating
!
        z1        = zR1 + intpolFaktRadial * ( zL1 - zR1 )  ! radial          z-interpolation between L1 and R1   ( profile 1 )
        z2        = zR2 + intpolFaktRadial * ( zL2 - zR2 )  ! radial          z-interpolation between L2 and R2   ( profile 2 )
        zN        = z1  + intpolFaktAngle  * ( z2  - z1  )  ! circumferential z-interpolation between z1 and z2
!
        IF( set_refZ ) F%refZ(i) = zN
!
!        ______________________________________________________________
        ! Interpolate measure of river length _________________________|
        IF( set_km ) F%km(i) = P1km + intpolFaktAngle*( P2km - P1km )
!
      ENDDO    !  i=1 , F%nNodes
!
!      dbug WRITE(6,*)'?>-------  SR Intpol_by_angular_Profils END-'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE Intpol_by_angular_Profils   !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
