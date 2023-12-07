program race
  use, intrinsic :: ieee_arithmetic
  implicit none

  integer :: ios
  character(len=512) :: buf

  real :: nan

  integer, parameter :: max_n = 100
  integer :: n
  real, dimension(max_n) :: time, dist
  real, dimension(2,max_n) :: roots
  integer, dimension(max_n) :: width
  
  nan = ieee_value( nan, ieee_quiet_nan )

  time(:) = nan
  dist(:) = nan
  read(*,'(T10 A)') buf
  read(buf,*,iostat=ios) time
  read(*,'(T10 A)') buf
  read(buf,*,iostat=ios) dist
  n = findloc(isnan(time),.true.,dim=1) - 1

  roots = qroots(-time, dist)
  width(:) = ceiling(roots(2,:)) - floor(roots(1,:)) - 1
  
  ! write(0,'(  *( "(" I12 "            " ")" : " " )  )') width(:n)

  write(*,'( "Part 1: " I0 )') product(width(:n))

contains
  
  function qroots(b, c)
    implicit none
    real, dimension(:), intent(in) :: b, c
    real, dimension(2,size(b)) :: qroots

    qroots(1,:) = -1
    qroots(2,:) = 1
    qroots = qroots * spread(sqrt(b*b - 4*c), 1,2)
    qroots = qroots - spread(b, 1,2)
    qroots = qroots / 2

  end function

    
end program race