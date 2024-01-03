program reflection
  use iso_c_binding
  implicit none
  
  integer, dimension(2) :: res

block

  integer :: ios
  character(4096), target :: buf
  character, dimension(:,:), pointer :: arr
  integer :: i,j

  call read_input_block(buf, arr, iostat=ios)
  call print_char_mat('("A(:" I0 "," I3 ")=" *(A))', arr)
  do j = lbound(arr,2),ubound(arr,2)
    arr(::-1,j) = slide_right(arr(::-1,j))
  end do
  call print_char_mat('("B(:" I0 "," I3 ")=" *(A))', arr)

  ! sum( (/(i, i=)/))
end block

contains

pure function slide_right(initial) result(row)
  character, dimension(:), intent(in) :: initial
  character, dimension(lbound(initial,1):ubound(initial,1)) :: row
  integer :: pos_start, pos_end

  row = initial
  pos_end = lbound(row,1)
  do pos_start = lbound(row,1),ubound(row,1)
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
      !$ ASSUME FALSE
    end select
  end do
end function slide_right

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