program pipes
  use iso_c_binding
  implicit none

  character(32768), target :: buf
  integer, dimension(2) :: bounds
  character, dimension(:,:), pointer :: array
  
  integer, dimension(2), parameter :: N=[0,-1], S=[0,1], E=[1,0], W=[-1,0]

  integer, dimension(2) :: pos
  integer, dimension(140,140,2) :: steps
  logical :: sn,ss,se,sw

  integer :: res1, res2

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

  call walk_pipe(pos, pos+N, steps(:,:,2), sn)
  if (sn) steps(:,:,1) = minval(steps, 3)

  call walk_pipe(pos, pos+E, steps(:,:,2), se)
  if (se) steps(:,:,1) = minval(steps, 3)

  call walk_pipe(pos, pos+S, steps(:,:,2), ss)
  if (ss) steps(:,:,1) = minval(steps, 3)

  call walk_pipe(pos, pos+W, steps(:,:,2), sw)
  if (sw) steps(:,:,1) = minval(steps, 3)

  if (sn .and. ss) then
    array(pos(1),pos(2)) = "|"
  else if (se .and. sw) then
    array(pos(1),pos(2)) = "-"
  else if (sn .and. se) then
    array(pos(1),pos(2)) = "L"
  else if (se .and. ss) then
    array(pos(1),pos(2)) = "F"
  else if (ss .and. sw) then
    array(pos(1),pos(2)) = "7"
  else if (sw .and. sn) then
    array(pos(1),pos(2)) = "J"
  else
    write(0,*) "unknown case in patch"
  end if 
  
  ! block
  !   integer j
  !   do j = 1, bounds(2)
  !     write(0,'(*(A3)/)') array(:bounds(1),j)
  !     write(0,'(*(I3)/)') steps(:bounds(1),j,1)
  !   end do
  ! end block

  steps(:,:,2) = 0
  block
    integer i, j
    do concurrent (j = 1:bounds(2))
      do concurrent (i = 1:bounds(1))
        if (steps(i,j,1) == huge(steps(i,j,1))) then
          array(i,j) = "."
          steps(i,j,1) = 0
        end if
      end do
    end do
  end block
  
  res1 = maxval(steps(:bounds(1),:bounds(2),1))

  block
    integer j
    res2 = 0
    do j = 1, bounds(2)
      res2 = res2 + row_area(array(:bounds(1),j))
    end do
  end block


  write(*,'("Part 1: " I0)') res1
  write(*,'("Part 2: " I0)') res2

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
      case default
        write(0,*) "unknown case in walk"
      end select

      if (any(p <= 0) .or. any(p > bounds)) success = .false.
      if (.not. success) return
    end do


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

  function row_area(row) result(area)
    character, dimension(:), intent(in) :: row
    integer :: area
    integer :: i

    logical is_inside, is_roof
    is_inside = .false.
    is_roof = .false.
    area = 0

    do i = lbound(row,1), ubound(row,1)
      select case (row(i))
      case ("|")
        is_inside = .not. is_inside
      case ("-")
        continue
      case ("F")
        is_roof = .true.
      case ("L")
        is_roof = .false.
      case ("J")
        is_inside = is_inside .neqv. is_roof
      case ("7")
        is_inside = is_inside .eqv. is_roof
      case (".")
        area = area + merge(1,0,is_inside)
      case default
        ! write(0,*) "unknown case in area"
      end select
    end do
    
    ! write(0,'(I4 ":" *(A1))') area, row
  end function



end program pipes