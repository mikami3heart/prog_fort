subroutine sub_fcount(a,b,c,n)
integer :: n
real*8 a(n), b(n), c(n)

a123=a(1)+a(2)+a(3)
b123=b(1)+b(2)+b(3)

do i=1,n
c(i)=0.1*(a(i)+b(i))
do j=i,n
c(j)= c(j) + (a(j)*a(i)+b(j)*b(i))
end do
end do

c(n)=c(n-1)+a123+b123
return
end


