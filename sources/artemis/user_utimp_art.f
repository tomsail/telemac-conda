!                   *************************
                    SUBROUTINE USER_UTIMP_ART
!                   *************************
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    ALMOST ALL THE COMPUTATION VARIABLES ARE AVAILABLE
!+             HERE TO WRITE OUT SPECIFIC OUTPUT, COMPUTE ANALYTICAL
!+             SOLUTIONS...
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_ARTEMIS, EX_USER_UTIMP_ART=> USER_UTIMP_ART
      USE DECLARATIONS_ARTEMIS
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!------------------------------------------------------------------
!
      INTEGER I
      TYPE(BIEF_OBJ) :: BID
!
!------------------------------------------------------------------
!
      IF(.FALSE.) THEN
!     EXAMPLE : U1 AND V1
!              (HORIZONTAL VELOCITIES AT T/4)
!               ARE TRANSFERRED TO PRIVE(I,1) AND PRIVE(I,2)
        CALL VECTOR(T2, '=' , 'GRADF          X' , IELM ,
     &              1.D0 , PHII , BID , BID , BID , BID , BID ,
     &              MESH , MSK , MASKEL )

        CALL VECTOR(T3 , '=' , 'GRADF          Y' , IELM ,
     &              1.D0 , PHII , BID , BID , BID , BID , BID ,
     &              MESH , MSK , MASKEL )

        CALL VECTOR(T1 , '=' , 'MASBAS          ' , IELM ,
     &              1.D0 , BID , BID , BID , BID , BID , BID ,
     &              MESH , MSK , MASKEL )

        CALL OS('X=Y/Z   ', X=T2, Y=T2, Z=T1)
        CALL OS('X=Y/Z   ', X=T3, Y=T3, Z=T1)

        ! TODO: Correct the computation below they are wrong
        DO I = 1,NPOIN
          PRIVE%ADR(1)%P%R(I) = OMEGAM%R(I)*T2%R(I)
          PRIVE%ADR(2)%P%R(I) = 2D0*3.1415D0/PERPIC*T3%R(I)
        ENDDO
      ENDIF
!
      RETURN
      END
