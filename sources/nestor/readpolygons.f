!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  ReadPolygons                   !********************************************
!**                                               ********************************************
!**                                               ********************************************
     &()
!
!
      USE m_TypeDefs_Nestor
!
      USE m_Nestor, ONLY : Poly, nPolys, ipid, ParallelComputing, LuPolF
!
!
!
!
#ifndef NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  ErrMsgAndStop
#endif
!
!
!
#ifndef NESTOR_INTERFACES
      !--------------------- local variables ---------------
      INTEGER  :: j, n, countPt, stat, status, lineCount
      LOGICAL  :: ThreeDigitsNumeral   ! function
      EXTERNAL :: ThreeDigitsNumeral   ! function
      CHARACTER (128) :: zeile, chtmp
!
!
!
      TYPE(t_String_Length) :: SRname     ! subroutine where the error occured
!
!
!
!      dbug WRITE(6,*)'?>-------  SR ReadPolygon ------------------'
      SRname%s = "ReadPolygon"
!
      REWIND LuPolF   ! LuPolF = Logical Unit POLygon File
!
      nPolys    = 0
      lineCount = 0
      DO   !  loopt to read the dig polygons file to detect the number of polygons
        READ( LuPolF, '(A)', IOSTAT = stat, END=600 ) zeile
        IF( stat /= 0 .OR. zeile(1:7)  == 'ENDFILE'
     &                .OR. zeile(1:10) == 'ENDE DATEI') EXIT
        lineCount = lineCount + 1
        zeile = ADJUSTL(zeile)
        IF( zeile(1:1) == ''      ) CYCLE           ! empty line
        IF( zeile(1:1) == '#'     ) CYCLE           ! comment line
        IF( zeile(1:1) == '/'     ) CYCLE           ! comment line
        IF( zeile(1:5) == 'INSEL' ) CYCLE           ! ignore BAW insel-dat format
        IF( zeile(1:4) == 'DAMM'  ) CYCLE           ! ignore BAW insel-dat format
        IF( zeile(1:4) == 'NAME'  ) nPolys = nPolys + 1
      ENDDO
!
      ALLOCATE( Poly( nPolys ), stat=status)
!
      REWIND LuPolF
      lineCount = 0
      countPt   = 0
      n         = 0
      DO   ! loop to read the dig polygons file to detect the number of points of each polygon
        READ( LuPolF, '(A)', IOSTAT = stat ) zeile
        IF( stat /= 0 .OR. zeile(1:1) == 'E') EXIT  ! end of file
        lineCount = lineCount + 1
        zeile = ADJUSTL(zeile)
        IF( zeile(1:1) == ''      ) CYCLE           ! empty line
        IF( zeile(1:1) == '#'     ) CYCLE           ! comment line
        IF( zeile(1:1) == '/'     ) CYCLE           ! comment line
        IF( zeile(1:5) == 'INSEL' ) CYCLE           ! ignore BAW insel-dat format
        IF( zeile(1:4) == 'DAMM'  ) CYCLE           ! ignore BAW insel-dat format
        IF( zeile(1:4) == 'NAME' ) THEN
          n = n + 1
          READ(zeile,'(5X,A)') chtmp
          Poly(n)%name = ADJUSTL(chtmp)
!
          IF( .NOT. ThreeDigitsNumeral(Poly(n)%name(1:3))) THEN       !> check if polygon name conforms
            Call ErrMsgAndStop( "while read the Polygon file    "     !  to the demand format
     &      ,"         reason: polygon-name must have at posi-  "
     &      ,"                 tion 1-3 numerals like: 123_aName"
     &      ,"occured in line: ", lineCount, SRname, ipid      )
          ENDIF
!
          IF( n > 1) Poly(n-1)%nPoints = countPt    ! name occured not the first time
          countPt = 0
        ELSE
          countPt = countPt + 1
        ENDIF
      ENDDO
      Poly(n)%nPoints = countPt
      DO n=1, nPolys
        ALLOCATE( Poly(n)%Pt( Poly(n)%nPoints ) , stat=status)
      ENDDO
!
      REWIND LuPolF
      lineCount = 0
      n         = 0
      DO   !  loopt to read the points of each polygon
        READ( LuPolF, '(A)', IOSTAT = stat ) zeile
        IF( stat /= 0 .OR. zeile(1:1) == 'E') EXIT  ! end of file
        lineCount = lineCount + 1
        zeile = ADJUSTL(zeile)
        IF( zeile(1:1) == ''      ) CYCLE           ! empty line
        IF( zeile(1:1) == '#'     ) CYCLE           ! comment line
        IF( zeile(1:1) == '/'     ) CYCLE           ! comment line
        IF( zeile(1:5) == 'INSEL' ) CYCLE           ! ignore BAW insel-dat format
        IF( zeile(1:4) == 'DAMM'  ) CYCLE           ! ignore BAW insel-dat format
        IF( zeile(1:4) == 'NAME' ) THEN
          n = n + 1
          j = 0
        ELSE     ! read x-, y-koortinats of point
          j = j + 1
          READ(zeile,*,IOSTAT = stat) Poly(n)%Pt(j)%x, Poly(n)%Pt(j)%y
          IF(stat /= 0) THEN
            CALL ErrMsgAndStop( "while read the Polygon file "
     &      ,"reason: 2 real values are expected      "," "
     &      ,"        occured in line: ", linecount, SRname, ipid  )
          ENDIF
        ENDIF
      ENDDO
      !DO n=1, nPolys
      !  WRITE(6,'("NAME:",A)')Poly(n)%name
      !  WRITE(6,*)'numberPt = ',Poly(n)%nPoints
      !  DO j=1, Poly(n)%nPoints
      !    WRITE(6,*)Poly(n)%Pt(j)%x, Poly(n)%Pt(j)%y
      !  ENDDO
      !ENDDO
      IF( ParallelComputing ) CALL P_SYNC()
!      dbug WRITE(6,*)'?>-------  SR ReadPolygon End --------------'
      RETURN
!
600   CONTINUE
      CALL ErrMsgAndStop( "while read the Polygon file "
     &      ,"reason: unexpected end of file           "
     &      ,"        missing ENDFILE  (no blankes !)  "
     &      ,"        occured in line: ", linecount, SRname, ipid  )
      RETURN
!
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE ReadPolygons                !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
