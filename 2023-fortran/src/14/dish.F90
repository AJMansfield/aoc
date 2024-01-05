program reflection
  use iso_c_binding
  implicit none
  
block
  integer :: ios
  character(16384), target :: buf
  character, dimension(:,:), pointer :: arr
  integer, dimension(2) :: res

  call read_input_block(buf, arr, iostat=ios)
  call main(arr, res)

  write(*, '("Part 1: " I0)') res(1)
  write(*, '("Part 2: " I0)') res(2)
end block

contains

subroutine main(arr, res)
  character, dimension(:,:), intent(inout) :: arr
  integer, dimension(2), intent(out) :: res
  integer :: n, m
  integer :: i, j
  n = size(arr, 1)
  m = size(arr, 2)

  call print_char_mat('("A(:" I0 "," I3 ")=" *(A))', arr)

  !! Slide East:
  ! do j = 1,m
  !   call slide_left(arr(n:1:-1,j))
  ! end do

  !! Slide North:
  do i = 1,n
    call slide_left(arr(i,:))
  end do

  call print_char_mat('("B(:" I0 "," I3 ")=" *(A))', arr)

  res(1) = sum( spread((/(j, j=m,1,-1)/), 1, n), mask=(arr=='O'))

end subroutine main

pure subroutine slide_left(row)
  character, dimension(:), intent(inout) :: row
  integer :: pos_start, pos_end

  pos_end = 1
  do pos_start = 1,size(row,1)
    select case (row(pos_start))
    case ('O')
      row(pos_start) = '.'
      row(pos_end) = 'O'
      pos_end = pos_end + 1
    case ('#')
      pos_end = pos_start + 1
    case ('.')
      ! no-op
    case default
      ! $ ASSUME FALSE
      row(pos_start) = '!'
    end select
  end do
end subroutine slide_left

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

end program reflection