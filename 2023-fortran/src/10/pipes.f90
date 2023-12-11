program pipes
  use iso_c_binding
  implicit none

  character(32768), target :: buf
  integer, dimension(2) :: bounds
  character, dimension(:,:), pointer :: array
  
  integer, dimension(2), parameter :: N=[0,-1], S=[0,1], E=[1,0], W=[-1,0]

  integer, dimension(2) :: pos
  integer, dimension(140,140,2) :: steps
  logical :: success

  integer :: res1

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
  steps = huge(steps)
  steps(pos(1),pos(2),1) = 0

  call walk_pipe(pos, pos+N, steps(:,:,2), success)
  if (success) steps(:,:,1) = minval(steps, 3)

  call walk_pipe(pos, pos+E, steps(:,:,2), success)
  if (success) steps(:,:,1) = minval(steps, 3)

  call walk_pipe(pos, pos+S, steps(:,:,2), success)
  if (success) steps(:,:,1) = minval(steps, 3)

  call walk_pipe(pos, pos+W, steps(:,:,2), success)
  if (success) steps(:,:,1) = minval(steps, 3)

  steps(:,:,2) = 0
  where (steps == huge(steps)) steps = 0
  
  res1 = maxval(steps)

  write(*,'("Part 1: " I0)') res1

contains

  subroutine walk_pipe(last_pos, this_pos, step_overlay, success)
    integer, dimension(2), intent(in) :: last_pos, this_pos
    integer, dimension(:,:), intent(out) :: step_overlay
    logical, intent(out), optional :: success

    integer, dimension(2) :: q, p
    integer :: step_num

    q = last_pos
    p = this_pos
    step_num = 1
    step_overlay = huge(step_overlay)

    if (any(p <= 0) .or. any(p > bounds)) then
      success = .false.
      return
    end if


    do
      step_overlay(p(1),p(2)) = min(step_overlay(p(1),p(2)), step_num)
      step_num = step_num + 1

      select case (array(p(1),p(2)))
      case ("|")
        call try_walks(q, p, N, S, success)
      case ("-")
        call try_walks(q, p, E, W, success)
      case ("L")
        call try_walks(q, p, N, E, success)
      case ("J")
        call try_walks(q, p, N, W, success)
      case ("7")
        call try_walks(q, p, S, W, success)
      case ("F")
        call try_walks(q, p, S, E, success)
      case (".")
        success = .false.
      case ("S")
        success = .true.
        return
      end select

      if (any(p <= 0) .or. any(p > bounds)) success = .false.
      if (.not. success) return
    end do


  end subroutine

  subroutine try_walks(q, p, dp1, dp2, success)
    integer, dimension(2), intent(inout) :: q, p
    integer, dimension(2), intent(in) :: dp1, dp2
    logical, intent(out) :: success
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