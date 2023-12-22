program springs
  use iso_c_binding
  implicit none
  
  integer, parameter :: max_h = 1000, max_w_line = 48, max_w = 32, max_n = 7
  integer, parameter :: Kstat = 1, Kseg = 2

  integer(Kstat), parameter :: BROKE = 5, MAYBE = 6, FIXED = 7

#if defined PERF_TIME
  real t1, t2, t3, t4
  call cpu_time(t1)
#endif

main: block
  integer :: h
  integer, dimension(max_h) :: w, n
  integer(Kseg), dimension(max_n, max_h) :: segments
  integer(Kstat), dimension(max_w, max_h) :: status
  integer, dimension(max_h) :: result
  
  h = -1
  w = -1 ! debug sentinels -- should never see a negative anywhere else
  n = -1
  segments = -1
  status = -1

input: block
  integer :: ios
  integer :: i

  character(max_w_line), dimension(max_h), target :: buf
  character(1), dimension(:, :), pointer :: arr

  call c_f_pointer(c_loc(buf), arr, [max_w_line, max_h])
  
  do i = 1, max_h
    read(*, '(A)', iostat=ios) buf(i)
    if(ios /= 0) exit
  end do

  h = i - 1
  w = index(buf, " ") - 1
  n = count(arr==",", dim=1) + 1

  do i = 1, h
    ! write(0, '("seg[" I4 "]*" I1 "=" A)') i, n(i), buf(i)(w(i)+1:)
    read(buf(i)(w(i)+1:), *, iostat=ios) segments(:,i)
  end do

  where(arr(:max_w,:) == "#") status(:,:) = BROKE
  where(arr(:max_w,:) == "?") status(:,:) = MAYBE
  where(arr(:max_w,:) == ".") status(:,:) = FIXED

end block input
#if defined PERF_TIME
  call cpu_time(t2)
#endif
work: block
  integer :: i

  ! integer(Kseg), dimension(max_w, max_h) :: allowed_length
  ! integer(Kseg), dimension(max_w, max_h) :: forced_length
  ! do i=1,max_h
  !   call calc_fwd_lengths(status(:,i), allowed_length(:,i), forced_length(:,i))
  ! end do

  write(0, '("h: " I0)') h
  write(0, '("w: " *(I2 " "))') w(:h)
  write(0, '("n: " *(I2 " "))') n(:h)
  write(0, '("s: " *(7(I3 " "):/"   "))') segments(:,:h)
  write(0, '("a: " *(32(I2 " "):/"   ") )') status(:,:h)
  write(0, '("A: " *(32(I2 " "):/"   ") )') allowed_length(:,:h)
  write(0, '("F: " *(32(I2 " "):/"   ") )') forced_length(:,:h)

  do i=1,h
    write(0, '(/"Row " I0)') i
    result(i) = number_of_matches(status(:w(i),i), segments(:n(i),i)),  allowed_length(:w(i),i), forced_length(:w(i),i))
  end do

  ! write(0, '("r: " *(I2:/"   ") )') result(:h)

end block work
#if defined PERF_TIME
  call cpu_time(t3)
#endif
output: block

  write(*, '("Part 1: " I0)') sum(result(:h))
  write(*, '("Part 2: " I0)') 0

end block output
end block main
#if defined PERF_TIME
  call cpu_time(t4)

  write(0,'("read : " F10.6)') t2 - t1
  write(0,'("work : " F10.6)') t3 - t2
  write(0,'("write: " F10.6)') t4 - t3
  write(0,'("total: " F10.6)') t4 - t1
#endif

contains

pure subroutine calc_fwd_lengths(status, allowed_length, forced_length)
  integer(Kstat), dimension(:), intent(in) :: status
  integer(Kseg), dimension(size(status)), intent(out) :: allowed_length
  integer(Kseg), dimension(size(status)), intent(out) :: forced_length

  integer(Kseg) :: i, aL, fL
  aL = 0_Kseg
  fL = 0_Kseg
  do i = size(status,kind=Kseg), 1_Kseg, -1_Kseg
    select case (status(i))
    case (BROKE)
      aL = aL + 1_Kseg
      fL = fL + 1_Kseg
      allowed_length(i) = aL
      forced_length(i) = fL
    case (MAYBE)
      aL = aL + 1_Kseg
      fL = fL + 1_Kseg
      allowed_length(i) = aL
      forced_length(i) = fL
      fL = 0_Kseg
    case (FIXED)
      aL = 0_Kseg
      fL = 0_Kseg
      allowed_length(i) = aL
      forced_length(i) = fL
    case default
      aL = 0_Kseg
      fL = 0_Kseg
      allowed_length(i) = aL
      forced_length(i) = fL
    end select
  end do

  ! also, the forced length of a MAYBE directly before a BROKE is +1
end subroutine

recursive function number_of_matches(status, segments) result(num)!, allowed_length, forced_length) result(num)
  integer(Kstat), dimension(:), intent(in) :: status
  integer(Kseg), dimension(:), intent(in) :: segments
  ! integer(Kseg), dimension(size(status)), intent(in) :: allowed_length, forced_length
  integer :: num

  integer :: i, ub, n
  
  write(0, '("begin number_of_matches()")') 
  write(0, '("S= " *(I3))') status
  write(0, '("s= " *(I3))') segments
  ! write(0, '("A= " *(I3))') allowed_length
  ! write(0, '("F= " *(I3))') forced_length

  if (size(segments) == 0) then
    if (all(status >= MAYBE)) then
      num = 1
      return ! no more segments to lay out, and all of the spaces after this could be FIXED
    else
      num = 0
      return ! no more segments to lay out, but there is an obligate BROKE after this point
    end if
  end if
  
  ub = size(status) - (sum(segments) + size(segments) - 2) ! stop searching once we pass the minimum compact length

  write(0, '("i= " I0 ", " I0)') 1, ub
  num = 0
  do i = 1, ub
    n = number_of_matches_here(status(i:), segments)!, allowed_length(i:), forced_length(i:))
    write(0, '("n[" I1 "]= " I0)') i, n
    num = num + n
    if (status(i) <= BROKE) exit
  end do

  write(0, '("end number_of_matches() -> " I0)') num
  
end function number_of_matches

recursive function number_of_matches_here(status, segments) result(num)!, allowed_length, forced_length) result(num)
  integer(Kstat), dimension(:), intent(in) :: status
  integer(Kseg), dimension(:), intent(in) :: segments
  ! integer(Kseg), dimension(size(status)), intent(in) :: allowed_length, forced_length
  integer :: num

  integer :: seg
  integer :: lb

  write(0, '("@s= " I0)') size(segments)

  seg = segments(1) ! try to lay out this next segment at the current point

  if (status(seg+1) == BROKE) then
    num = 0
    return ! there'd be an extra contiguous BROKE
  end if

  if (.not. all(status(:seg) <= MAYBE)) then
    num = 0
    return ! there aren't enough contiguous BROKE and FIXED
  end if

  ! at this point, we have successfully laid out the currently-pending segment
  ! therefore, we just need to truncate and search again

  lb = seg + 2
  num = number_of_matches(status(lb:), segments(2:), allowed_length(lb:), forced_length(lb:))

  ! write(0, '("n: (rec)" I0)') num
end function number_of_matches_here
  

end program springs