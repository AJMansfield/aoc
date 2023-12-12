program pipes
  use iso_c_binding
  implicit none

  character(32768), target :: buf
  integer, dimension(2) :: bounds
  character, dimension(:,:), pointer :: array
  
  integer, parameter :: N=1, E=2, S=3, W=4

  integer :: res1, res2

  real t1, t2, t3, t4

  call cpu_time(t1)

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

  call cpu_time(t2)

  block
    integer, dimension(2) :: pos
    integer :: area, perimeter
    logical :: closed

    pos = findloc(array, "S")

    call walk_pipe(pos, E, area, perimeter, closed)
    if (closed) goto 1
    call walk_pipe(pos, W, area, perimeter, closed)
    if (closed) goto 1
    call walk_pipe(pos, S, area, perimeter, closed)
    1 continue

    res1 = perimeter / 2
    res2 = abs(area) - perimeter/2 + 1 ! subtract out the line width
  end block

  call cpu_time(t3)

  write(*,'("Part 1: " I0)') res1
  write(*,'("Part 2: " I0)') res2

  call cpu_time(t4)

  ! write(0,'("read : " F10.6)') t2 - t1
  ! write(0,'("work : " F10.6)') t3 - t2
  ! write(0,'("write: " F10.6)') t4 - t3
  ! write(0,'("total: " F10.6)') t4 - t1
contains

  subroutine walk_pipe(in_pos, in_dir, area, perimeter, closed)
    integer, dimension(2), intent(in) :: in_pos
    integer, intent(in) :: in_dir
    integer, intent(out) :: area, perimeter
    logical, intent(out) :: closed

    integer, dimension(2) :: p
    integer :: d

    p = in_pos
    d = in_dir
    area = 0
    perimeter = 0
    closed = .false.

    do
      perimeter = perimeter + 1
      select case (d)
      case (N)
        area = area - p(1) ! greene's theorem
        p(2) = p(2) - 1
        if (p(2) < 1) exit
        select case (array(p(1),p(2)))
        case ("|")
          d = N
        case ("7")
          d = W
        case ("F")
          d = E
        case ("S")
          closed = .true.
          exit
        case default
          exit
        end select
      case (S)
        area = area + p(1) ! greene's theorem
        p(2) = p(2) + 1
        if (p(2) > bounds(2)) exit
        select case (array(p(1),p(2)))
        case ("|")
          d = S
        case ("J")
          d = W
        case ("L")
          d = E
        case ("S")
          closed = .true.
          exit
        case default
          exit
        end select
      case (E)
        p(1) = p(1) + 1
        if (p(1) > bounds(1)) exit
        select case (array(p(1),p(2)))
        case ("-")
          d = E
        case ("J")
          d = N
        case ("7")
          d = S
        case ("S")
          closed = .true.
          exit
        case default
          exit
        end select
      case (W)
        p(1) = p(1) - 1
        if (p(1) < 1) exit
        select case (array(p(1),p(2)))
        case ("-")
          d = W
        case ("L")
          d = N
        case ("F")
          d = S
        case ("S")
          closed = .true.
          exit
        case default
          exit
        end select
      end select
    end do
  end subroutine


end program pipes