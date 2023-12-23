program reflection
  use iso_c_binding
  implicit none
  
#if defined PERF_TIME
  real :: t1, t2, t3, t4, t_temp, t_input_begin, t_input_end, d_input
#endif

  integer :: res1, res2
  
#if defined PERF_TIME
  call cpu_time(t1)
  d_input = 0
  t_input_begin = 0
  t_input_end = 0
  call cpu_time(t2)
#endif

  res1 = 0
  res2 = 0

block
  integer :: ios
  character(4096), target :: buf
  character, dimension(:,:), pointer :: arr

  integer :: n, i, a, b, c, d

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
      
      block
        logical, dimension(size(arr,1),a:b) :: match
        match(:,a:b) = arr(:,a:b) == hflip(arr(:,c:d))

        if (all(match)) then
          res1 = res1 + 100*b
        else if (count(.not. match) == 1) then
          res2 = res2 + 100*b
        end if
      end block
    end do

    n = size(arr,1)
    do i = -n+2, n-2, 2
      a = 1 + max(i, 0)
      d = n + min(i, 0)
      b = ceil_div(a+d, 2)-1
      c = ((a + d) / 2) + 1

      block
        logical, dimension(a:b, size(arr,2)) :: match
        match(a:b,:) = arr(a:b,:) == vflip(arr(c:d,:))

        if (all(match)) then
          res1 = res1 + b
        else if (count(.not. match) == 1) then
          res2 = res2 + b
        end if
      end block
    end do
  end do

end block

#if defined PERF_TIME
  call cpu_time(t3)
#endif

  write(*, '("Part 1: " I0)') res1
  write(*, '("Part 2: " I0)') res2

#if defined PERF_TIME
  call cpu_time(t4)
  d_input = d_input + (t_input_end - t_input_begin)

  write(0,'("read : " F10.6)') d_input
  write(0,'("work : " F10.6)') t3 - t2 - d_input
  write(0,'("write: " F10.6)') t4 - t3
  write(0,'("total: " F10.6)') t4 - t1
#endif

contains

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