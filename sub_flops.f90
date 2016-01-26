subroutine sub_add(a,b,c)
include 'parameters.inc' 
real*8 a(n), b(n), c(n)
do i=1,n
c(i)=a(i)+b(i)
end do
return
end

subroutine sub_mult(a,b,c)
include 'parameters.inc'
real*8 a(n), b(n), c(n)
do i=1,n
c(i)=a(i)*b(i)
end do
return
end

subroutine sub_fma(a,b,c)
include 'parameters.inc'
real*8 a(n), b(n), c(n)
real*8 d
d=real(n)
do i=1,n
c(i)=a(i)+b(i)*d
c(i)=a(i)*c(i) + b(i)*c(i)
c(i)=sqrt(c(i)**2.0 + 1.0)
end do
return
end

subroutine sub_recip(a,b,c)
include 'parameters.inc'
real*8 a(n), b(n), c(n)
do i=1,n
c(i)=1.0/a(i)
end do
return
end

subroutine sub_divide(a,b,c)
include 'parameters.inc'
real*8 a(n), b(n), c(n)
do i=1,n
c(i)=b(i)/a(i)
end do
return
end


subroutine sub_null(a,b,c)
include 'parameters.inc'
real*8 a(n), b(n), c(n)

return
end

