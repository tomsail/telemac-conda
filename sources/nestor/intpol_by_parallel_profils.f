!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  Intpol_by_parallel_Profils     !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &(  F, xA,yA,zA,  xB,yB,zB,  xC,yC,zC,  xD,yD,zD,  ip, P1km,P2km
     &  ,set_refZ, set_km     )
!
      USE m_TypeDefs_Nestor
      USE m_Nestor , ONLY : ipid
!
#ifndef NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  ErrMsgAndStop
#endif
!
      IMPLICIT NONE
!
      TYPE(t_Field) ,INTENT(INOUT) :: F
!
      REAL (KIND=R8),INTENT(IN) :: xA,yA,zA,xB,yB,zB,xC,yC,zC,xD,yD,zD ! points:  A,B,C,D
      INTEGER,INTENT(IN)        :: ip          ! index of profile1
      REAL (KIND=R8),INTENT(IN) :: P1km, P2km  ! measure of river length: profile1, profile2
      LOGICAL,INTENT(IN)        :: set_refZ, set_km
!
#ifndef NESTOR_INTERFACES
      !--------------------- local variables ---------------
!
      INTEGER         :: i
      REAL (KIND=R8)  :: distNP, distPP                ! distance: profile-1 - point N, profile-1 - profile-2
      REAL (KIND=R8)  :: xN,yN, xSa,ySa, xSb,ySb       ! point N, Sa, Sb
      REAL (KIND=R8)  :: xAB,yAB, xAC,yAC, xAN,yAN     ! vetkors: AB, AC, AN
      REAL (KIND=R8)  :: xBD,yBD, xBN,yBN              ! vetkors: BD, BN
      REAL (KIND=R8)  :: xCD,yCD ,xCN,yCN              ! vetkors: CD, CN
      REAL (KIND=R8)  :: absAB, faktor1, faktor2
      REAL (KIND=R8)  :: distSaN, distSaSb
      REAL (KIND=R8)  :: z1, z2                        ! interpolated z-values between: zA zB , zC zD
      REAL (KIND=R8)  :: zN                            ! interpolated z-value of Node between: z1 z2
      REAL (KIND=R8)  :: DetABAN, DetCDCN, DetACAN, DetBDBN
      TYPE(t_String_Length) :: SRname ! name of current Subroutine
!
      !
      !
      !
      !                 D         z2                            C
      !    profile2     *=========^=============================*--------------------
      !                  \                                     /:               :
      !                   \                                   / :               :
      !                    \ Sb      N                    Sa /  :              distPP
      !                     +-------*-----------------------+   :  ---          :
      !                     :\      :                      /:   :   :           :
      !                     : \     :                     / :   :  distNP       :
      !                     :  \    : z1                 /  :   :   :           :
      !    profile1      ---:---*=====^=================*---:---:---------------------
      !                     :   B   :                   A   :
      !                     :       |--------distSaN--------|
      !                     |------------distSaSb-----------|
      !
      !
      !
!      dbug WRITE(6,*)'?>-------  SR Intpol_Z_parallel_Profils ----'
      SRname%s = "Intpol_Z_parallel_Profils"                     ! subroutine name
!
      xAB = xB - xA             !> vector AB: x component (Profile1)
      yAB = yB - yA             !>     "      y component      "
      xCD = xD - xC             !> vector CD: x component (Profile2)
      yCD = yD - yC             !>     "      y component      "
!
      xAC = xC - xA             !> vector AC: x component
      yAC = yC - yA             !>     "      y component
!
      xBD = xD - xB             !> vector BD: x component
      yBD = yD - yB             !>     "      y component
!
      absAB  = SQRT( xAB**2 + yAB**2 )
      distPP = abs( xAC*yAB - yAC*xAB ) / absAB   !> Distance profile1 - profile2
                                                  !  keywords: point,line distance, 2D
      IF(distPP < 1.D-8   ) Call ErrMsgAndStop(
     &  " ","reason: Profiles are super narrow !"
     & ," ","        Fit profile  ", ip,  SRname, ipid )
!
!
      DO i = 1 , F%nNodes
!        ______________________________________________________________
        !  Test if node was already successful processed ______________|
        IF( set_refZ ) THEN
          IF( F%refZ(i) > -1000.0D0 ) CYCLE
        ELSEIF( set_km ) THEN 
          IF( F%km(i)   > -1000.0D0 ) CYCLE
        ENDIF
!
!
        xN = F%X(i)    !> get the coordinate of node which receives the "refz value"
        yN = F%Y(i)
!
        xAN     = xN - xA            !> vector AN: x component
        yAN     = yN - yA            !>     "      y component
        xCN     = xN - xC            !> vector CN: x component
        yCN     = yN - yC            !>     "      y component
        DetABAN = xAB*yAN - yAB*xAN  !> determinate of vectors AB and AN
        DetCDCN = xCD*yCN - yCD*xCN  !> determinate of vectors CD and CN
!
!        ______________________________________________________________
        !> Before we do further calculations we                        |
        !  test if node N is inside the quadrangle which is given      |
        !  by the two parallel Profiles                                |
        !                  keywords: triangle, area, determinant, sign |
        !  step 1) if the sign of DetABAN and DetCDCN is the same      |
        !          Point N is not between the line-AB and line-CD      |
        !               ==> next loop                                  |
        !  step 2) if the sign of DetACAN and DetBDBN is the same      |
        !          Point N is not between the line-AB and line-CD      |
        !               ==> next loop                  ________________|
        IF(     (DetABAN < 0.0D0 .AND. DetCDCN < 0.0D0)            ! test step 1
     &      .OR.(DetABAN > 0.0D0 .AND. DetCDCN > 0.0D0)  )  CYCLE  ! node is not between the lines
!
        xBN     = xN - xB              !> vector BN: x component
        yBN     = yN - yB              !>     "      y component
        DetACAN = xAC*yAN - yAC*xAN    !> determinate of vectors AC and AN
        DetBDBN = xBD*yBN - yBD*xBN    !> determinate of vectors BD and BN
!
        IF(     (DetACAN < 0.0D0 .AND. DetBDBN < 0.0D0)            ! test step 2
     &      .OR.(DetACAN > 0.0D0 .AND. DetBDBN > 0.0D0)  )  CYCLE  !> node is not between the lines
!
        !> node is inside the quadrangle
!
        distNP = abs( xAN*yAB - yAN*xAB ) / absAB   !> Distance node - profile1
                                                    !  keywords: point, line, distance, 2D
        faktor1 = distNP / distPP
        xSa    = xA  +   faktor1 * xAC
        ySa    = yA  +   faktor1 * yAC
        xSb    = xB  +   faktor1 * xBD
        ySb    = yB  +   yBD
!
        distSaN  =  (xN  - xSa)**2 + (yN  - ySa)**2   !> We do the square root 2 lines below
        distSaSb =  (xSb - xSa)**2 + (ySb - ySa)**2   !> We do the square root 1 line below
        faktor2  = SQRT(  distSaN  / distSaSb   )     !> Here we do it
!
        z1     = zA  +   faktor2 * (zB-zA)   !> interpolate z1 between zA and zB
        z2     = zC  +   faktor2 * (zD-zC)   !> interpolate z2 between zC and zD
        zN     = z1 + faktor1 * (z2-z1)      !> interpolate zN between z1 and z2
!
        IF( set_refZ ) F%refZ(i) = zN 
!
        IF( set_km ) F%km(i) = P1km + faktor1 * (P2km - P1km)  !> interpolate measure of river length
!
      ENDDO ! (i = 1 , F%nNodes)
!
!      dbug WRITE(6,*)'?>-------  SR Intpol_by_parallel_Profils END'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE Intpol_by_parallel_Profils  !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
