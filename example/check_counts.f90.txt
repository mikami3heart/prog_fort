
subroutine sub_fcount(a,b,c,n)
integer :: n
real*8 a(n), b(n), c(n)
do i=1,n
c(i)=0.1/(a(i)+b(i))
end do
do i=1,n
c(i)=c(i)+a(i)*b(i)
end do
return
end


subroutine sub_ext(a,b,c,n)
integer :: n
real*8 a(n), b(n), c(n)
do i=1,n
c(i)=log(exp( a(i)*b(i) ))
end do
return
end


subroutine sub_icount(ia,ib,ic,n)
integer :: n
integer ia(n), ib(n), ic(n)
do i=1,n
ic(i)=ia(i)+ib(i)
end do
return
end


program main
integer,parameter :: n=16384
integer :: loops
real(kind=8), allocatable :: a(:), b(:), c(:)
integer, allocatable :: ia(:), ib(:), ic(:)
integer is_ok, i, j

loops =  1000
write(6,'(a,i10)') "<main> n= ", n
write(6,'(a,i10)') "<main> loops= ", loops
allocate (a(n), b(n), c(n), stat=is_ok)
allocate (ia(n), ib(n), ic(n), stat=is_ok)

do i=1,n
a(i)=real(i)/real(n)
b(i)=1.0-a(i)
c(i)=0.0
end do

do j=1,loops
call sub_fcount(a,b,c,n)
call sub_icount(ia,ib,ic,n)
call sub_ext(a,b,c,n)
end do
write(6,'(a,3f8.3)') "<sub_fcount> flop    add:2n, mult:n, div:n each per loop"
write(6,'(a,3f8.3)') "<sub_icount> integer add:n  each per loop"

deallocate (a)
deallocate (b)
deallocate (c)

stop
end

