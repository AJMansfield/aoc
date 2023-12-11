program pipes
  use iso_c_binding
  implicit none

  character(32768), target :: buf
  integer, dimension(2) :: bounds
  character, dimension(:,:), pointer :: array
  
  integer, dimension(2), parameter :: N=[0,-1], S=[0,1], E=[1,0], W=[-1,0]

  integer, dimension(2) :: pos

  integer :: area, perimeter

  block
    integer :: ios
    read(*,'(A)', iostat=ios) buf
    bounds(2) = 1
    bounds(1) = len_trim(buf)
    
    do
      read(*,'(A)', iostat=ios) buf(product(bounds)+1:)
      if (ios /= 0) exit
      bounds(2) = bounds(2) + 1
    end do

    call c_f_pointer(c_loc(buf), array, bounds)
  end block


  pos = findloc(array, "S")

  block
    logical :: closed

    call walk_pipe(pos, pos+E, area, perimeter, closed)
    if (closed) goto 1
    call walk_pipe(pos, pos+W, area, perimeter, closed)
    if (closed) goto 1
    call walk_pipe(pos, pos+S, area, perimeter, closed)
    1 continue
  end block

  write(*,'("Part 1: " I0)') perimeter / 2
  write(*,'("Part 2: " I0)') area

contains

  subroutine walk_pipe(last_pos, this_pos, area, perimeter, closed)
    integer, dimension(2), intent(in) :: last_pos, this_pos
    integer, intent(out) :: area, perimeter
    logical, intent(out) :: closed

    integer, dimension(2) :: q, p
    logical :: noerr

    q = last_pos
    p = this_pos
    area = 0
    perimeter = 0
    noerr = .true.
    closed = .false.

    do
      if (any(p <= 0) .or. any(p > bounds)) closed = .false.
      if (closed .or. .not. noerr) exit

      perimeter = perimeter + 1
      area = area + p(1) * (p(2) - q(2)) ! Using greenes theorem to compute the area

      select case (array(p(1),p(2)))
      case ("|")
        call try_walks(q, p, N, S, noerr)
      case ("-")
        call try_walks(q, p, E, W, noerr)
      case ("L")
        call try_walks(q, p, N, E, noerr)
      case ("J")
        call try_walks(q, p, N, W, noerr)
      case ("7")
        call try_walks(q, p, S, W, noerr)
      case ("F")
        call try_walks(q, p, S, E, noerr)
      case (".")
        noerr = .false.
      case ("S")
        closed = .true.
      case default
        write(0,*) "unknown case in walk"
      end select
    end do

    area = abs(area) - perimeter/2 + 1 ! subtract out the line width

  end subroutine

  subroutine try_walks(q, p, dp1, dp2, success)
    integer, dimension(2), intent(inout) :: q, p
    integer, dimension(2), intent(in) :: dp1, dp2
    logical, intent(out), optional :: success
    integer, dimension(2) :: oldp

    oldp = p

    if (all(p + dp1 == q)) then
      p = p + dp2
      success = .true.
    else if (all(p + dp2 == q)) then
      p = p + dp1
      success = .true.
    else
      success = .false.
    end if

    if (success) q = oldp
  end subroutine

end program pipes