program main
implicit none
include 'parameters.inc'
real(kind=8), allocatable :: a(:), b(:), c(:)
real(kind=8) :: t0,t1,t2,t3,t4,t5,tnull
real(kind=8) :: timer_rdtsc
real :: r0,r1,r2,r3,r4,r5
integer is_ok, i, j, k
real :: flop

allocate (a(n), stat=is_ok)
allocate (b(n), stat=is_ok)
allocate (c(n), stat=is_ok)

!cx loops=1000000
!cx nexp = nint(log(real(n))/log(2.0))  !because log2(x) == log10(x)/log10(2)
!cx if (nexp.gt.10) loops=1000000/(2**(nexp-10))

loops =  (max_int / n)
write(6,'(a,i9,x,a,i9)') "<main> n=", n
write(6,'(a,i10,x,a,i9)') " loops= ", loops

t0 = timer_rdtsc()
r0=0.0; r1=0.0; r2=0.0; r3=0.0; r4=0.0; r5=0.0
do i=1,n
a(i)=real(i)/real(n)
b(i)=1.0-a(i)
c(i)=0.0
end do

t0 = timer_rdtsc()
do j=1,loops
call sub_null(a,b,c)
end do
t1 = timer_rdtsc()
tnull = t1-t0


t0 = timer_rdtsc()
do j=1,loops
call sub_add(a,b,c)
end do
t1 = timer_rdtsc()
do j=1,loops
call sub_mult(a,b,c)
end do
t2 = timer_rdtsc()
do j=1,loops
call sub_fma(a,b,c)
end do
t3 = timer_rdtsc()
do j=1,loops
call sub_recip(a,b,c)
end do
t4 = timer_rdtsc()
do j=1,loops
call sub_divide(a,b,c)
end do
t5 = timer_rdtsc()

r1 = (t1-t0) - tnull
r2 = (t2-t1) - tnull
r3 = (t3-t2) - tnull
r4 = (t4-t3) - tnull
r5 = (t5-t4) - tnull

flop = real(n)*real(loops)
write(6,'(a)') "Type: Time  GFlops"
write(6,'(a,f8.3,x,f8.3)') "add: ",   r1, flop/r1*1.0e-9
write(6,'(a,f8.3,x,f8.3)') "mult:",   r2, flop/r2*1.0e-9
write(6,'(a,f8.3,x,f8.3)') "fma: ",   r3, flop*2.0/r3*1.0e-9
write(6,'(a,f8.3,x,f8.3)') "reci:",   r4, flop/r4*1.0e-9
write(6,'(a,f8.3,x,f8.3)') "divi:",   r5, flop/r5*1.0e-9
write(6,'(a,f9.3)') "calibr. ovhd(%):", (tnull/(t1-t0))*100.0

deallocate (a)
deallocate (b)
deallocate (c)

stop
end

