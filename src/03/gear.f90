program gear
  use iso_c_binding
  implicit none

  integer :: ios, idx
  character(32768), target :: buf
  integer :: w, h, i, j, k
  character, dimension(:,:), pointer :: array

  logical, dimension(:,:), allocatable :: is_digit, is_special, is_target
  integer :: num
  logical :: targeting
  integer, dimension(:), allocatable :: sums


  read(*,'(A)', iostat=ios) buf
  h = 1
  w = len_trim(buf)
  
  do
    read(*,'(A)', iostat=ios) buf(w*h+1:)
    if (ios /= 0) exit
    h = h + 1
  end do

  call c_f_pointer(c_loc(buf), array, [w,h])

  allocate(is_digit(0:w+1,0:h+1))
  allocate(is_special(0:w+1,0:h+1))
  allocate(is_target(0:w+1,0:h+1))
  allocate(sums(0:h+1))
  is_digit(:,:) = .false.
  is_special(:,:) = .false.
  is_target(:,:) = .false.
  sums(:) = 0

  do concurrent (j = 1:h)
    do concurrent (i = 1:w)
      if (verify(array(i,j), "0123456789") == 0) is_digit(i,j) = .true.
      if (.not. is_digit(i,j) .and. array(i,j) /= ".") is_special(i,j) = .true.
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
        read(buf((j-1)*w+i:(j-1)*w+k), *) num
        ! write(*,*) "found digits",  i, j, k, buf((j-1)*w+i:(j-1)*w+k)
        sums(j) = sums(j) + num
        targeting = .false.
      end if

    end do
  end do

  write(*,*) "Part 1:", sum(sums)
  
end program gear