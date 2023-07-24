!                   **********************
                    SUBROUTINE USER_ANAMAR
!                   **********************
!***********************************************************************
! TOMAWAC
!***********************************************************************
!
!brief    USER SPECIFIES AN ANALYTICAL TIDE :
!+                WATER LEVEL AND CURRENT SPEED ARE VARIABLE IN TIME.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| COMPUTATION TIME
!| DDC            |-->| DATE OF COMPUTATION BEGINNING
!| DZHDT          |<--| VARIATION TEMPORELLE DE LA HAUTEUR DE MAREE
!| LT             |<--| NUMBER OF THE TIME STEP CURRENTLY SOLVED
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| UC             |-->| CURRENT VELOCITY ALONG X AT THE MESH POINTS
!| VC             |-->| CURRENT VELOCITY ALONG Y AT THE MESH POINTS
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| ZM             |<--| DEPTH AT TIME AT, AT THE MESH POINTS
!| ZM1            |-->| DEPTH AT TIME TM1, AT THE MESH POINTS
!| ZM2            |-->| DEPTH AT TIME TM2, AT THE MESH POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TOMAWAC, EX_USER_ANAMAR => USER_ANAMAR
!     USE DECLARATIONS_TOMAWAC, ONLY : UC  , VC  , DEPTH  , ZM1 , ZM2 ,
!    &  DZHDT , X  , Y  , NPOIN2 ,  AT  , DDC , LT
      IMPLICIT NONE
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!     COMMENT THE 5 LINES

      WRITE(LU,*) 'CALL TO USER_ANAMAR'
      WRITE(LU,*) 'IF YOU WANT AN ANALYTICAL TIDAL MODIFY ANAMAR'
      WRITE(LU,*) 'OR MAY BE IT MEANS YOU DID NOT SPECIFY YOUR FILE'
      WRITE(LU,*)'BINARY TIDAL WATER LEVEL FILE OR BINARY CURRENTS FILE'
      CALL PLANTE(1)
!-----------------------------------------------------------------------
!     EXAMPLE 1
!-----------------------------------------------------------------------
!
!      UCONST=0.D0
!      VCONST=0.D0
!
!      DO IP=1,NPOIN2
!        UC(IP)   = UCONST
!        VC(IP)   = VCONST
!        ZM(IP)   = 3.D0
!        DEPTH(IP) = 3.D0
!        DZHDT(IP)= 0.D0
!      ENDDO ! IP
!
!-----------------------------------------------------------------------
!
      RETURN
      END
