program reflection
  use iso_c_binding
  implicit none

#if defined PERF_TIME
  real :: t1, t2, t3, t4, t_temp, t_input_begin, t_input_end, d_input
  call cpu_time(t1)
  d_input = 0
  t_input_begin = 0
  t_input_end = 0
  call cpu_time(t2)
#endif

block
  integer, dimension(2) :: res

  integer :: ios
  character(4096), target :: buf
  character, dimension(:,:), pointer :: arr

  integer, dimension(2) :: r

  ios = 0
  do while (ios == 0)

#if defined PERF_TIME
    call cpu_time(t_temp) ! accumulate the time spent working on reading the input so it can be subtracted later
    d_input = d_input + (t_input_end - t_input_begin)
    t_input_begin = t_temp
#endif

    call read_input_block(buf, arr, iostat=ios)

#if defined PERF_TIME
    call cpu_time(t_input_end)
#endif

    r = 0
    call scan_for_hmirror(arr, 100, r)
    call scan_for_hmirror(transpose(arr), 1, r)
    res = res + r
  end do


#if defined PERF_TIME
  call cpu_time(t3)
#endif

  write(*, '("Part 1: " I0)') res(1)
  write(*, '("Part 2: " I0)') res(2)

end block

#if defined PERF_TIME
  call cpu_time(t4)
  d_input = d_input + (t_input_end - t_input_begin)

  write(0,'("read : " F10.6)') d_input
  write(0,'("work : " F10.6)') t3 - t2 - d_input
  write(0,'("write: " F10.6)') t4 - t3
  write(0,'("total: " F10.6)') t4 - t1
#endif

contains

subroutine scan_for_hmirror(arr, mul, res)
  character, dimension(:,:), intent(in) :: arr
  integer, intent(in) ::  mul
  integer, dimension(2), intent(inout) ::  res
  
  integer :: n, i, a, b, c, d

  ! iterating in this pattern (but skipping every other)
  ! i=[-4  -3  -2  -1   0   1   2   3   4]
  ! 1  ab  ab  a   a   a                  
  ! 2  cd       b   b      a              
  ! 3      cd  c        b   b  a          
  ! 4           d  c   c        b  ab     
  ! 5               d      c   c       ab 
  ! 6                   d   d   d  cd  cd 

  n = size(arr,2)
  do i = -n+2, n-2, 2
    if (all(res /= 0)) exit

    a = 1 + max(i, 0)
    d = n + min(i, 0)
    b = ceil_div(a+d, 2)-1
    c = ((a + d) / 2) + 1

    select case (count(arr(:,a:b) /= hflip(arr(:,c:d))))
    case (0)
      res(1) = mul*b
    case (1)
      res(2) = mul*b
    end select
  end do
end subroutine scan_for_hmirror

subroutine read_input_block(buf, arr, iostat)
  character(*), target, intent(out) :: buf
  character, dimension(:,:), pointer, intent(out) :: arr
  integer, intent(out), optional :: iostat

  integer :: w, h

  read(*, '(A)', advance='no', size=w, iostat=iostat) buf

  h = 1
  do
    read(*, '(A)', iostat=iostat) buf(h*w+1:(h+1)*w)
    if (iostat /= 0) exit
    if (len_trim(buf(h*w+1:(h+1)*w)) /= w) exit
    h = h + 1
  end do

  call c_f_pointer(c_loc(buf), arr, [w,h])
end subroutine read_input_block

subroutine print_char_mat(linefmt, mat)
  character(*), intent(in) :: linefmt
  character, dimension(:,:), intent(in) :: mat

  integer :: i
  do i = lbound(mat, dim=2), ubound(mat, dim=2)
    write(0,linefmt) size(mat(:,i)), i, mat(:,i)
  end do
end subroutine print_char_mat

pure function hflip(mat)
  character, dimension(:,:), intent(in) :: mat
  character, dimension(lbound(mat,1):ubound(mat,1),lbound(mat,2):ubound(mat,2)) :: hflip
  hflip = mat(:,ubound(mat,2):lbound(mat,2):-1)
end function

pure elemental function ceil_div(x, y) result(q)
  integer, intent(in) :: x, y
  integer :: q
  q = (x + y - 1) / y
end function

end program reflection