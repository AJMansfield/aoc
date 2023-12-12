program gear
  use iso_c_binding
  implicit none

  integer :: ios
  character(32768), target :: buf
  integer :: w, h
  character, dimension(:,:), pointer :: array

  logical, dimension(:,:), allocatable :: is_digit, is_special, is_target, is_gear

  read(*,'(A)', iostat=ios) buf
  h = 1
  w = len_trim(buf)
  
  do
    read(*,'(A)', iostat=ios) buf(w*h+1:)
    if (ios /= 0) exit
    h = h + 1
  end do

  call c_f_pointer(c_loc(buf), array, [w,h])

  block
    integer :: i, j, k
    integer :: num
    logical :: targeting
    integer, dimension(:), allocatable :: sums
    integer, dimension(:,:), allocatable :: gear_totals

    allocate(is_digit(0:w+1,0:h+1))
    allocate(is_special(0:w+1,0:h+1))
    allocate(is_target(0:w+1,0:h+1))
    allocate(is_gear(0:w+1,0:h+1))
    allocate(sums(h))
    allocate(gear_totals(w,h))
    is_digit(:,:) = .false.
    is_special(:,:) = .false.
    is_target(:,:) = .false.
    is_gear(:,:) = .false.
    sums(:) = 0
    gear_totals(:,:) = 0

    do concurrent (j = 1:h)
      do concurrent (i = 1:w)
        if (verify(array(i,j), "0123456789") == 0) is_digit(i,j) = .true.
        if (.not. is_digit(i,j) .and. array(i,j) /= ".") is_special(i,j) = .true.
        if (array(i,j) == "*") is_gear(i,j) = .true.
      end do
    end do

    ! write(*,*) "Digits:"
    ! do j = 1, h
    !   write(*,*) is_digit(1:w,j)
    ! end do
    ! write(*,*) "Specials:"
    ! do j = 1, h
    !   write(*,*) is_special(1:w,j)
    ! end do

    do concurrent (j = 1:h)
      do concurrent (i = 1:w)
        if ( is_digit(i,j) .and. ( &
            is_special(i-1,j-1) .or. is_special(i,j-1) .or. is_special(i+1,j-1) .or. &
            is_special(i-1,j  ) .or. is_special(i,j  ) .or. is_special(i+1,j  ) .or. &
            is_special(i-1,j+1) .or. is_special(i,j+1) .or. is_special(i+1,j+1) ) ) then
          is_target(i,j) = .true.
        end if
      end do
    end do

    ! write(*,*) "Targets:"
    ! do j = 1, h
    !   write(*,*) is_target(1:w,j)
    ! end do

    do concurrent (j = 1:h)
      k = -1
      do i = w, 0, -1
        if (.not. is_digit(i,j)) k = -1
        if (k < 0 .and. is_digit(i,j)) then
          k = i
        end if

        if  (is_target(i,j)) then
          targeting = .true.
        end if

        if (targeting .and. .not. is_digit(i-1,j)) then
          read(buf((j-1)*w+i:(j-1)*w+k), *, iostat=ios) num
          sums(j) = sums(j) + num
          targeting = .false.
        end if

      end do
    end do

    write(*,*) "Part 1:", sum(sums)

    do j = 1, h
      do i = 1, w
        gear_totals(i,j) = gear_total(i,j)
      end do
    end do

    
    write(*,*) "Part 2:", sum(gear_totals)
  end block

contains
  function gear_total(i,j)
    implicit none
    integer, intent(in) :: i, j
    integer :: gear_total
    integer :: count, dj

    gear_total = 0    
    count = 0

    if (.not. is_gear(i,j)) then
      return
    end if

    gear_total = 1

    if (is_digit(i-1,j)) then 
      count = count + 1
      gear_total = gear_total * parse_at(i-1,j)
    end if

    if (is_digit(i+1,j)) then 
      count = count + 1
      gear_total = gear_total * parse_at(i+1,j)
    end if

    do dj = -1, 1, 2
      if (is_digit(i-1,j+dj)) then
        count = count + 1
        gear_total = gear_total * parse_at(i-1,j+dj)

        if (is_digit(i+1, j+dj) .and. .not. is_digit(i, j+dj) ) then
          count = count + 1
          gear_total = gear_total * parse_at(i+1,j+dj)
        end if
      
      else if (is_digit(i,j+dj)) then
        count = count + 1
        gear_total = gear_total * parse_at(i,j+dj)
      else if (is_digit(i+1,j+dj)) then
        count = count + 1
        gear_total = gear_total * parse_at(i+1,j+dj)
      end if
    end do

    if (count /= 2) gear_total = 0

    return
  end function

  function parse_at(i,j)
    integer, intent(in) :: i, j
    integer :: parse_at
    integer :: a, b, c, d
    a = (j-1)*w+1
    b = (j)*w
    c = (j-1)*w+i

    a = a + verify(buf(a:c), "0123456789", .true.)

    d = verify(buf(c:b), "0123456789", .false.)
    if(d /= 0) b = c + d - 2

    read(buf(a:b), *) parse_at
  end function
  
end program gear