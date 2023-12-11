program mirage
  implicit none

  integer ios
  integer, dimension(100) :: arr
  integer :: n
  integer :: result

  result = 0
  do
    call read_row(arr, n, ios)
    if (ios /= 0) exit
    result = result + extrapolate(arr(:n))
  end do

  write(*, '("Part 1: " I0)') result

contains
  subroutine read_row(array, count, iostat)
    integer, dimension(:), intent(out) :: array
    integer, intent(out) :: count
    integer, intent(out) :: iostat

    integer, parameter :: nan = -huge(nan)
    character(len=256) :: buf
    integer :: iostat_dontcare
    
    array = nan
    read(*, '(A)', iostat=iostat) buf
    if(iostat == 0) then
      read(buf, *, iostat=iostat_dontcare) arr
      count = findloc(arr, nan, 1) - 1
    endif
  end subroutine

  pure recursive function extrapolate(array) result(output)
    integer, dimension(:), intent(in) :: array
    integer :: output

    integer, dimension(size(array)-1) :: delta
    
    output = array(size(array))
    delta = array(2:) - array(:size(array)-1)
    if (.not. all(delta == 0)) then
      output = output + extrapolate(delta)
    end if

    ! write(0, '(I3 ": " *(I3 :", "))') output, delta
  end function

end program mirage