!                   *******************************
                    SUBROUTINE T3D_WAC_CPL_UPDATE(NIT_ORI)
!                   *******************************
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    Update data exhanged with tomawac
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        USE DECLARATIONS_TOMAWAC, ONLY : CPL_WAC_DATA
        USE DECLARATIONS_TELEMAC3D, ONLY : PERCOU_WAC,
     &      U2D, V2D, H, DIRMOY, HM0, TPR5, ORBVEL, FXH, FYH, 
     &      DT, AT, NPLAN, MESH3D, WIPDX, WIPDY,
     &      USTOKES, VSTOKES, WST1, WIP, FDX, FDY, FBX, FBY, CFWC, FDK,
     &      FWX,FWY
        USE DECLARATIONS_TELEMAC, ONLY : COUPLING
        USE METEO_TELEMAC, ONLY : WINDX, WINDY
        USE BIEF
        IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
        INTEGER, INTENT(IN) :: NIT_ORI
!
!-----------------------------------------------------------------------
!
        CPL_WAC_DATA%NIT_TEL = NIT_ORI
        CPL_WAC_DATA%PERCOU_WAC = PERCOU_WAC
        CPL_WAC_DATA%U_TEL => U2D
        CPL_WAC_DATA%V_TEL => V2D
        CPL_WAC_DATA%H_TEL => H
        CPL_WAC_DATA%DIRMOY_TEL => DIRMOY
        CPL_WAC_DATA%HM0_TEL => HM0
        CPL_WAC_DATA%TPR5_TEL => TPR5
        CPL_WAC_DATA%ORBVEL_TEL => ORBVEL
        CPL_WAC_DATA%FX_WAC => FXH
        CPL_WAC_DATA%FY_WAC => FYH
        CPL_WAC_DATA%UV_TEL => WINDX
        CPL_WAC_DATA%VV_TEL => WINDY
        CPL_WAC_DATA%DT_TEL = DT
        IF(NIT_ORI.EQ.0) THEN
          CPL_WAC_DATA%AT_TEL = AT
        ELSE
          CPL_WAC_DATA%AT_TEL = AT-DT
        ENDIF

        IF(INCLUS(COUPLING,'TOMAWACT3D')) THEN
          CPL_WAC_DATA%COUPL3D = .TRUE.
          CPL_WAC_DATA%WIPDXW => WIPDX
          CPL_WAC_DATA%WIPDYW => WIPDY
          CPL_WAC_DATA%USTW => USTOKES
          CPL_WAC_DATA%VSTW => VSTOKES
          CPL_WAC_DATA%WSTW => WST1
          CPL_WAC_DATA%ZTELW => MESH3D%Z
          CPL_WAC_DATA%NZW = NPLAN
          CPL_WAC_DATA%WIPW => WIP
          CPL_WAC_DATA%FDXW => FDX
          CPL_WAC_DATA%FDYW => FDY
          CPL_WAC_DATA%FBXW => FBX
          CPL_WAC_DATA%FBYW => FBY
          CPL_WAC_DATA%CFWCW => CFWC
          CPL_WAC_DATA%FDKW => FDK
          CPL_WAC_DATA%FWX => FWX
          CPL_WAC_DATA%FWY => FWY
        ELSE
          CPL_WAC_DATA%COUPL3D = .FALSE.
        ENDIF
!
      END SUBROUTINE
