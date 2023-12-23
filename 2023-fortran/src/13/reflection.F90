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
  integer :: res1, res2

  integer :: ios
  character(4096), target :: buf
  character, dimension(:,:), pointer :: arr

  integer :: r1, r2

  res1 = 0
  res2 = 0

  ios = 0
  do while (ios == 0)
    r1 = 0
    r2 = 0

#if defined PERF_TIME
  call cpu_time(t_temp) ! accumulate the time spent working on reading the input so it can be subtracted later
  d_input = d_input + (t_input_end - t_input_begin)
  t_input_begin = t_temp
#endif
    call read_input_block(buf, arr, iostat=ios)
#if defined PERF_TIME
  call cpu_time(t_input_end)
#endif


    call scan_for_hmirror(arr, r1, r2)

    res1 = res1 + r1 * 100
    res2 = res2 + r2 * 100

    ! if we found both the standard and the smudged, skip searching the transpose
    if (r1 == 0 .or. r2 == 0) call scan_for_hmirror(transpose(arr), res1, res2)
    ! if (r1 == 0 .or. r2 == 0) call scan_for_vmirror(arr, res1, res2)
  end do


#if defined PERF_TIME
  call cpu_time(t3)
#endif

  write(*, '("Part 1: " I0)') res1
  write(*, '("Part 2: " I0)') res2

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

subroutine scan_for_hmirror(arr, res1, res2)
  character, dimension(:,:), intent(in) :: arr
  integer, intent(inout) ::  res1, res2
  
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
    a = 1 + max(i, 0)
    d = n + min(i, 0)
    b = ceil_div(a+d, 2)-1
    c = ((a + d) / 2) + 1
    
    if (all(arr(:,a:b) == hflip(arr(:,c:d)))) then
      res1 = res1 + b
    else if (count(arr(:,a:b) /= hflip(arr(:,c:d))) == 1) then
      res2 = res2 + b
    end if
  end do
end subroutine scan_for_hmirror


subroutine scan_for_vmirror(arr, res1, res2)
  ! turns out, scanning the array in this order is actually slower than just transposing the array once
  ! use scan_for_hmirror(transpose(arr), res1, res2) instead
  character, dimension(:,:), intent(in) :: arr
  integer, intent(inout) ::  res1, res2
  
  integer :: n, i, a, b, c, d

  n = size(arr,1)
  do i = -n+2, n-2, 2
    a = 1 + max(i, 0)
    d = n + min(i, 0)
    b = ceil_div(a+d, 2)-1
    c = ((a + d) / 2) + 1

    if (all(arr(a:b,:) == vflip(arr(c:d,:)))) then
      res1 = res1 + b
    else if (count(arr(a:b,:) /= vflip(arr(c:d,:))) == 1) then
      res2 = res2 + b
    end if
  end do
end subroutine

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

pure function vflip(mat)
  character, dimension(:,:), intent(in) :: mat
  character, dimension(lbound(mat,1):ubound(mat,1),lbound(mat,2):ubound(mat,2)) :: vflip
  vflip = mat(ubound(mat,1):lbound(mat,1):-1,:)
end function
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