program reflection
  use iso_c_binding
  implicit none
  
  integer :: res1, res2

block
  character(4096), target :: buf
  character, dimension(:,:), pointer :: arr

  call read_input_block(buf, arr)
  call print_char_mat('("A[:" I5 "," I5 "]=" *(A1))', arr)
  call print_char_mat('("H[:" I5 "," I5 "]=" *(A1))', hflip(arr))
  call print_char_mat('("V[:" I5 "," I5 "]=" *(A1))', vflip(arr))

  write(*, '("Part 1: " I0)') res1
  write(*, '("Part 2: " I0)') res2
end block

contains

subroutine read_input_block(buf, arr)
  character(*), target, intent(out) :: buf
  character, dimension(:,:), pointer, intent(out) :: arr

  integer :: ios, w, h
  read(*, '(A)', advance='no', size=w, iostat=ios) buf

  h = 1
  do
    read(*, '(A)', iostat=ios) buf(h*w+1:(h+1)*w)
    if (ios /= 0) exit
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

function vflip(mat)
  character, dimension(:,:), intent(in) :: mat
  character, dimension(lbound(mat,1):ubound(mat,1),lbound(mat,2):ubound(mat,2)) :: vflip
  vflip = mat(ubound(mat,1):lbound(mat,1):-1,:)
end function
function hflip(mat)
  character, dimension(:,:), intent(in) :: mat
  character, dimension(lbound(mat,1):ubound(mat,1),lbound(mat,2):ubound(mat,2)) :: hflip
  hflip = mat(:,ubound(mat,2):lbound(mat,2):-1)
end function

end program reflection