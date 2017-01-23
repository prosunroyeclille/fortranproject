ProjectFORTRANTurbulence

PROGRAM : Numerical Integration

Author P.Roy. 

Objective: Analysis and calculation of a function using Simpson, Trapezoidal, Mid-point and Rectangular methods. 


IMPLICIT NONE
declaration of variable when they appear !
INTEGER :: m, j, i
REAl :: a, b, h, x, y, l, c, r, right-side-area, left-side-area, total-area
a = 2, b = 4
right-side-area= 8, left-side-area= 4, total-area= 12
y= 0, c= 0, m= 0

WRITE (*,*) "Do Integration using Numerical Methods"
WRITE (*,*) "Entering the lower bound: a"
WRITE (*,*) "Entering the upper bound: b"
WRITE (*,*) "Number of divisions: m"

READ (*,*) m
    where h = b-a/m

IF (e==1) 
THEN  !integration using rectangular method
  
  DO  j = 0, m-1
  l = a + h*j
  y = y + (2+sin(2*sqrt(l)))
  END DO
  right-side-area = y*h

  DO  i = 1, m
  c = a + h*i
  r = r + (2+sin(2*sqrt(c)))
  END DO
  left-side-area = r*h

WRITE (*,*) ' Calculate the area of rectangular method= right, left :', right_area, left_area

ELSE IF(e==2) 
THEN !integration using midpoint method
  DO  j = 0, m-1
  l = a + h*(k+0.5)
  y = y + (2+sin(2*sqrt(l)))
  END DO
  area = y*h
WRITE (*,*) ' Calculate the area of midpoint method=', area

ELSE IF (e==3) 
THEN !integration using trapezoidal method
  DO  j = 1, m-1
  l = a + h*i
  y = y + (2*(2+sin(2*sqrt(l)))) !involves multiplication by 2
  END DO
area =h/2* (y + (2+sin(2*sqrt(a))) + (2+sin(2*sqrt(b))))
WRITE (*,*) ' Calculate the area of trapezoidal method=', area 

ELSE IF (e==4) 
THEN !integration by Simpson's one third method
 DO  j = 0, m-2, 2
  l = (a+h) + (h*i)
  y = y + 4*(2+sin(2*sqrt(l)))
  END DO
  
  DO  j = 0, m-4, 2
  c = (a+2*h) + (h*j)
  r = r + 2*(2+sin(2*sqrt(c)))
  END DO
  
area =h/3* (y + r + (2+sin(2*sqrt(a))) + (2+sin(2*sqrt(b))))
WRITE (*,*) ' Calculate the area of Simpson one third method=', area 

ELSE
WRITE (*,*) 'enter 1, 2, 3 or 4'
END IF
END PROGRAM Iumerical Integration 
