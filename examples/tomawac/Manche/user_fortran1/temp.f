!                       ***************
                        SUBROUTINE TEMP
!                       ***************
!
     &(TV ,DAT,DDC)
!
!***********************************************************************
!  TOMAWAC VERSION 1.0    01/02/95        F.MARCOS     (LNH) 30 87 72 66
!***********************************************************************
!
!   FONCTION : CE SOUS-PROGRAMME CALCULE LE TEMPS EN SECONDE
!              ENTRE LES DATES DAT ET DDC
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! !      NOM       !MODE!                   ROLE                       !
! !________________!____!______________________________________________!
! !    TV          !<-- !  ECART DE TEMPS EN SECONDES                  !
! !    DAT         ! -->!  DATE D'UN ENREGISTREMENT DES VENTS          !
! !    DDC         ! -->!  DATE DU DEBUT DU CALCUL                     !
! !________________!____!______________________________________________!
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
! APPELE PAR : LECVEN
!
! SOUS-PROGRAMME APPELE : AUCUN
!
!***********************************************************************
!
      IMPLICIT NONE
!
      INTEGER ADC,MDC,JDC,HDC,MNDC,ADT,MDT,JDT,HDT,MNDT
      INTEGER NJDM(0:12)
      DOUBLE PRECISION TV,DDC,DAT
!
!-----------------------------------------------------------------------
!
      DATA NJDM /0,0,31,59,90,120,151,181,212,243,273,304,334/
!       ON NE TRAITE PAS LES ANNEES BISSEXTILES !!
!
!  DECODAGE DE LA DATE DU DEBUT DU CALCUL
!
      ADC=INT(DDC*1.D-8)
      MDC=INT(DDC*1.D-6)
      JDC=INT(DDC*1.D-4)
      HDC=INT(DDC*1.D-2)
      MNDC=INT(DDC-100.D0*HDC)
      HDC =HDC-100*JDC
      JDC =JDC-100*MDC
      MDC =MDC-100*ADC
!
!  DECODAGE DE LA DATE DE L'ENREGISTREMENT DU VENT
!
      ADT=INT(DAT*1.D-8)
      MDT=INT(DAT*1.D-6)
      JDT=INT(DAT*1.D-4)
      HDT=INT(DAT*1.D-2)
      MNDT=INT(DAT-100.D0*HDT)
      HDT =HDT-100*JDT
      JDT =JDT-100*MDT
      MDT =MDT-100*ADT
!
      TV=((((ADT-ADC)*365+(JDT+NJDM(MDT)-JDC-NJDM(MDC)))*24 +
     &     HDT-HDC)*60 + MNDT-MNDC)*60
!
      RETURN
      END
