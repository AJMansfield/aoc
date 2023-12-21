program springs
  use iso_c_binding
  implicit none

#if defined PERF_TIME
  real t1, t2, t3, t4
  call cpu_time(t1)
#endif

main: block
  integer, parameter ::max_h = 1000, max_w = 32, max_n = 5
  integer :: h
  integer, dimension(max_h) :: w, n
  integer, dimension(max_n, max_h) :: s
  
  character(max_w), dimension(max_h), target :: buf
  character(1), dimension(:, :), pointer :: arr

  call c_f_pointer(c_loc(buf), arr, [max_w, max_h])

  w = -1
  n = -1
  s = -1


input: block
  integer :: ios
  integer :: i
  
  do i = 1, max_h
    read(*, '(A)', iostat=ios) buf(i)
    if(ios /= 0) exit
  end do

  h = i - 1
  w = index(buf, " ")
  n = sum(merge(1,0,arr==","), dim=1) + 1

  do i = 1, max_h
    read(buf(w(i):), *, iostat=ios) s(:,i)
  end do
  

end block input
#if defined PERF_TIME
  call cpu_time(t2)
#endif
work: block

! read input

end block work
#if defined PERF_TIME
  call cpu_time(t3)
#endif
output: block

  write(*, '("Part 1: " I0)') 0
  write(*, '("Part 2: " I0)') 0


  write(0, '("h: " I0)') h
  write(0, '("w: " *(I2 " "))') w(:h)
  write(0, '("n: " *(I2 " "))') n(:h)
  write(0, '("s: " *(5(I2 " "):/"   "))') s(:,:h)
  write(0, '("a: " *(32(A1):/"   ") )') arr(:,:h)

end block output
end block main
#if defined PERF_TIME
  call cpu_time(t4)

  write(0,'("read : " F10.6)') t2 - t1
  write(0,'("work : " F10.6)') t3 - t2
  write(0,'("write: " F10.6)') t4 - t3
  write(0,'("total: " F10.6)') t4 - t1
#endif

contains

end program springs