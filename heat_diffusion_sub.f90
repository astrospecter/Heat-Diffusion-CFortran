! instructions here
SUBROUTINE diffuse(xw,yw,temp)
  ! declarations
  IMPLICIT NONE
  
  INTEGER, PARAMETER :: REAL8=SELECTED_REAL_KIND(15,300)
  INTEGER, SAVE :: nstp, npr, step, sourcex, sourcey
  REAL(REAL8), SAVE :: diff_const, source_temp
  REAL(REAL8), DIMENSION(:,:), ALLOCATABLE, SAVE :: f, fa, fg
  INTEGER :: xw, yw, x, y, xw2, yw2
  INTEGER, DIMENSION(160000) :: temp
  REAL(REAL8) :: amb_temp
  LOGICAL, SAVE :: initialized=.FALSE.

  xw = 200
  yw = 200

  
  xw2 = 2*xw
  yw2 = 2*yw

  ! init
  IF(.NOT.initialized) THEN
     ! allocate space in memory for f and fa(?)
     ALLOCATE(f(0:xw-1,0:yw-1),fa(0:xw-1,0:yw-1),fg(0:xw2-1,0:yw2-1))
     initialized=.TRUE.
  END IF

  IF(step==1) THEN
     PRINT *
     WRITE(*, '("Enter the x and y coordinates of the heat source and the temperture")')
     READ *,sourcex,sourcey,source_temp
     
     WRITE(*, '("Enter the diffusion constant and the ambient temperature")')
     READ *,diff_const,amb_temp

     DO x=0,xw-1
        DO y=0,yw-1
           f(x,y) = amb_temp
        END DO
     END DO

     f(sourcex,sourcey) = source_temp
     IF(sourcex < xw-1) THEN
        f(sourcex + 1, sourcey) = source_temp
        f(sourcex + 2, sourcey) = source_temp
     END IF
     IF (sourcex > 1) THEN
        f(sourcex - 1, sourcey) = source_temp
        f(sourcex - 2, sourcey) = source_temp
     END IF
     IF (sourcey < yw-1) THEN
        f(sourcex, sourcey+1) = source_temp
        f(sourcex, sourcey+2) = source_temp
     END IF
     IF (sourcey > 1) THEN
        f(sourcex, sourcey-1) = source_temp
        f(sourcex, sourcey-2) = source_temp
     END IF
     
     WRITE(*, '("Enter the total number of time steps")') 
     READ *,nstp 
     PRINT *

     ! TODO
  END IF

  IF(step>0) THEN
     ! TODO
     fa = (CSHIFT(f,1,1)+CSHIFT(f,-1,1)+CSHIFT(f,1,2)+CSHIFT(f,-1,2))/4
     f = (1-diff_const) * f + diff_const * fa

     IF(step<nstp) THEN
         f(sourcex,sourcey) = source_temp
         IF(sourcex < xw-1) THEN
            f(sourcex + 1, sourcey) = source_temp
            f(sourcex + 2, sourcey) = source_temp
         END IF
         IF (sourcex > 1) THEN
            f(sourcex - 1, sourcey) = source_temp
            f(sourcex - 2, sourcey) = source_temp
         END IF
         IF (sourcey < yw-1) THEN
            f(sourcex, sourcey+1) = source_temp
            f(sourcex, sourcey+2) = source_temp
         END IF
         IF (sourcey > 1) THEN
            f(sourcex, sourcey-1) = source_temp
            f(sourcex, sourcey-2) = source_temp
         END IF
     END IF
  END IF

  DO y=0,yw2-1
     DO x=0, xw2-1
         fg(x,y) = f(x/2,y/2)
     END DO
  END DO

  CALL field2pxa(temp, fg, xw, yw)

  IF (step==nstp) THEN
     PRINT *
     PRINT '("Close the window to stop the program.")'
     PRINT *
  END IF

  step = step + 1

  xw = xw2
  yw = yw2

END SUBROUTINE diffuse

SUBROUTINE field2pxa(temp, fg, xw, yw)

  IMPLICIT NONE

  INTEGER, PARAMETER :: REAL8=SELECTED_REAL_KIND(15,300)
  INTEGER :: xw, yw, x, y, xw2, yw2
  REAL(REAL8), DIMENSION(0:2*xw-1, 0:2*yw-1) :: fg
  INTEGER, DIMENSION(160000) :: temp
  REAL(REAL8) :: tempmax, tempmin
  INTEGER :: r,g,b

  xw2 = xw*2
  yw2 = yw*2

  DO y=0,yw2-1
     DO x=0,xw2-1
        IF (fg(x,y) > 150 .AND. fg(x,y) <= 200) THEN
           r = 255
           g = (200 - fg(x,y)) * (255 / 50)
           b = 0
        ELSE IF (fg(x,y) > 100 .AND. fg(x,y) <= 150) THEN
           r = (fg(x,y) - 100) * (255/50)
           g = 255
           b = 0
        ELSE IF (fg(x,y) > 50 .AND. fg(x,y) <= 100) THEN
           r = 0
           g = 255
           b = (100 - fg(x,y)) * (255/50)
        ELSE IF (fg(x,y) >= 0 .AND. fg(x,y) <= 50) THEN
           r = 0
           g = fg(x,y) * (255/50)
           b = 255
        ELSE IF (fg(x,y) > 200) THEN
           r = 255
           g = 255
           b = 255
        ELSE
           r = 0
           g = 0
           b = 0
        END IF
        temp(x+xw2*(yw2-y-1))= r + ISHFT(g,8) + ISHFT(b,16)
     END DO
  END DO
END SUBROUTINE field2pxa
