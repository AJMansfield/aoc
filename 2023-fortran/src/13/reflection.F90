program reflection
  use iso_c_binding
  implicit none
  
  integer :: res1, res2
  res1 = 0
  res2 = 0

block
  integer :: ios
  logical :: last_record
  character(4096), target :: buf
  character, dimension(:,:), pointer :: arr

  integer :: n, i, a, b, c, d
  integer :: rec_num
  rec_num = 0

  last_record = .false.

  do while (.not. last_record)
    call read_input_block(buf, arr, iostat=ios)
    if (ios /= 0) last_record = .true.
    rec_num = rec_num + 1
    write(0, '("Record " I0)') rec_num

    ! call print_char_mat('("A[:" I2 "," I2 "]=" *(A1))', arr)

    ! iterating in this pattern:
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
      
      ! write(0, '("i: " I2 ", regions " I0 ":" I0, ", " I0 ":" I0)') i,a,b,c,d
      ! call print_char_mat('("At[:" I2 "," I2 "]=" *(A1))', arr(:,a:b))
      ! call print_char_mat('("Af[:" I2 "," I2 "]=" *(A1))', hflip(arr(:,c:d)))

      if (all(arr(:,a:b) == hflip(arr(:,c:d)))) then
        write(0, '("hmatch! " SP I0)') 100*b
        res1 = res1 + 100*b
      end if
    end do

    n = size(arr,1)
    do i = -n+2, n-2, 2
      a = 1 + max(i, 0)
      d = n + min(i, 0)
      b = ceil_div(a+d, 2)-1
      c = ((a + d) / 2) + 1
      
      ! write(0, '("i: " I2 ", regions " I0 ":" I0, ", " I0 ":" I0)') i,a,b,c,d
      ! call print_char_mat('("At[:" I2 "," I2 "]=" *(A1))', arr(a:b,:))
      ! call print_char_mat('("Af[:" I2 "," I2 "]=" *(A1))', vflip(arr(c:d,:)))

      if (all(arr(a:b,:) == vflip(arr(c:d,:)))) then
        write(0, '("vmatch! " SP I0)') b
        res1 = res1 + b
      end if
    end do

  end do



  write(*, '("Part 1: " I0)') res1
  write(*, '("Part 2: " I0)') res2
end block

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