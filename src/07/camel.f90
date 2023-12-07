program camel
  implicit none

  integer :: ios
  character(len=1), dimension(5) :: hand
  integer :: bid
  do
    read(*, '(5A1 X I3)', iostat=ios) hand, bid
    if (ios /= 0) exit

    write(0,*) hand, bid
  end do
end program camel