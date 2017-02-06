real*8 function  ufcount(a,b,c,n)
integer :: n
real*8 a(n), b(n), c(n)
real*8 a123,b123,c123

a123=a(1)+a(2)+a(3)
b123=b(1)+b(2)+b(3)
c123=c(1)+c(2)+c(3)

ufcount = a123*b123*c123
return
end
