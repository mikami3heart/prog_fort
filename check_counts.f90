program main
integer,parameter :: n=16384
integer :: loops
real(kind=8), allocatable :: a(:), b(:), c(:)
!cx	real(kind=8) :: t0,t1,t2
!cx	real(kind=8) :: timer_rdtsc
!cx	real :: r0,r1,r2, flop
integer is_ok, i, j

loops =  1000
write(6,'(a,i10)') "<main> n= ", n
write(6,'(a,i10)') "<main> loops= ", loops
allocate (a(n), stat=is_ok)
allocate (b(n), stat=is_ok)
allocate (c(n), stat=is_ok)

do i=1,n
a(i)=real(i)/real(n)
b(i)=1.0-a(i)
c(i)=0.0
end do

!cx	t0 = timer_rdtsc()
do j=1,loops
call sub_count(a,b,c,n)
end do
!cx	t1 = timer_rdtsc()

!cx	r1 = (t1-t0)
!cx	flop = real(n)*real(loops)
!cx	write(6,'(a)') "Type: Time  GFlops"
!cx	write(6,'(a,f8.3,x,f8.3)') "add: ",   r1, flop/r1*1.0e-9
write(6,'(a,3f8.3)') "<sub_count> executes add:2n, mult:n, div:n each per loop"
write(6,'(a,3f8.3)') "a(1), a(2), a(3): ",   a(1), a(2), a(3)
write(6,'(a,3f8.3)') "b(1), b(2), b(3): ",   b(1), b(2), b(3)
write(6,'(a,3f8.3)') "c(1), c(2), c(3): ",   c(1), c(2), c(3)

deallocate (a)
deallocate (b)
deallocate (c)

stop
end


subroutine sub_count(a,b,c,n)
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

