program race
  use, intrinsic :: ieee_arithmetic
  implicit none

  integer :: ios
  character(len=128) :: buf

  real :: nan

  integer, parameter :: max_n = 16
  integer :: n
  integer(kind=8), dimension(max_n) :: time, dist
  real(kind=8), dimension(2,max_n) :: roots
  integer(kind=8), dimension(max_n) :: width
  
  nan = ieee_value( nan, ieee_quiet_nan )

  time(:) = -1
  dist(:) = -1

  read(*,'(T10 A)') buf
  read(buf,*,iostat=ios) time
  n = findloc(time,-1,dim=1) - 1
  read(buf,'(I128)') time(n+1)

  read(*,'(T10 A)') buf
  read(buf,*,iostat=ios) dist
  read(buf,'(I128)') dist(n+1)

  write(0,'(  *( "(" I15 "               " ")" : " " )  )') time(:n+1)
  ! write(0,'(  *( "(" I15 ")" : " " )  )') isqrt(time(:n+1))
  ! write(0,*) isqrt(time(:n+1))
  write(0,'(  *( "(" "               " I15 ")" : " " )  )') dist(:n+1)

  roots = qroots(real(-time,kind=8), real(dist,kind=8))
  width(:) = ceiling(roots(2,:)) - floor(roots(1,:)) - 1
  
  ! write(0,'(  *( "(" G15.6 G15.6 ")" : " " )  )') roots(:,:n+1)
  ! write(0,'(  *( "(" I15 "               " ")" : " " )  )') width(:n+1)

  write(*,'( "Part 1: " I0 )') product(width(:n))
  write(*,'( "Part 2: " I0 )') width(n+1)

contains
  
  pure function qroots(b, c)
    implicit none
    real(kind=8), dimension(:), intent(in) :: b, c
    real(kind=8), dimension(2,size(b)) :: qroots

    qroots(1,:) = -1
    qroots(2,:) = 1
    qroots = qroots * spread(sqrt(b*b - 4*c), 1,2)
    qroots = qroots - spread(b, 1,2)
    qroots = qroots / 2

  end function

  function isqrt(x)
    implicit none
    integer(kind=8), dimension(:), intent(in) :: x
    integer(kind=8), dimension(size(x)) :: isqrt
    integer(kind=8), dimension(size(x)) :: next_isqrt
    logical, dimension(size(x)) :: done

    isqrt = abs(x)

    block ! initial guesses for bounds based on bit-shifting out half of the bits
      integer(kind=8), dimension(size(x)):: num_bits
      num_bits = bit_size(x) - leadz(x)
      isqrt = x / 2**(num_bits/2-1)
    end block
    
    done = .false.

    write(0,'( "g:" *( I16 : " " )  )') isqrt/2
    write(0,'( "a:" *( I16 : " " )  )') (isqrt*isqrt)/4
    write(0,'( "x:" *( I16 : " " )  )') x
    write(0,'( "d:" *( L16 : " " )  )') done
    write(0,*)


    do while (.not. all(done))
      ! using isqrt = 2*sqrt(x)

      next_isqrt = (isqrt + 4*x/isqrt)/2
      ! next_isqrt = 1 + ((isqrt + 4*x/isqrt - 1) / 2) ! ceiling division instead of floor

      isqrt = merge( isqrt, next_isqrt, done)
      done = done .or. ((isqrt*isqrt) <= x*4 .and. ((isqrt+1)*(isqrt+1)) > x*4)

      write(0,'( "g:" *( I16 : " " )  )') isqrt/2
      write(0,'( "a:" *( I16 : " " )  )') (isqrt*isqrt)/4
      write(0,'( "x:" *( I16 : " " )  )') x
      write(0,'( "d:" *( L16 : " " )  )') done
      write(0,*)
    end do

    ! isqrt = isqrt / 2
    isqrt = 1 + ((isqrt - 1) / 2)

  end function

    
end program race