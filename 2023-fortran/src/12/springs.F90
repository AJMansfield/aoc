program springs
  use iso_c_binding
  implicit none
  
  integer, parameter :: max_h = 1000, max_n = 6, max_w_line = 32, max_w = 21
  integer, parameter :: unfold_by = 5
  integer, parameter :: Kstat = 1, Kseg = 1, Kres=8

  integer(Kstat), parameter :: BROKE = 5, MAYBE = 6, FIXED = 7

  integer(Kres) :: res1, res2

#if defined PERF_TIME
  real t1, t2, t3, t4
  call cpu_time(t1)
#endif

main: block
  integer :: h
  integer, dimension(max_h) :: n, w
  integer(Kseg), dimension(max_n, max_h) :: segments
  integer(Kstat), dimension(max_w, max_h) :: status
  
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
part1: block
  integer :: i

  ! write(0, '("h: " I0)') h
  ! write(0, '("n: " *(I3))') n(:h)
  ! write(0, '("w: " *(I3))') w(:h)
  ! write(0, '("s: " *(7I3:/"   "))') segments(:,:h)
  ! write(0, '("a: " *(21I3:/"   ") )') status(:,:h)

  res1 = 0
  do i=1,h
    ! write(0, '(/"Row " I0)') i
    res1 = res1 + number_of_matches(status(:w(i),i), segments(:n(i),i))
  end do

  ! write(0, '("r: " *(I2:/"   ") )') result(:h)

end block part1
part2: block
  integer(Kseg), dimension(max_n*unfold_by) :: big_segment
  integer(Kstat), dimension(max_w*unfold_by) :: big_status

  integer :: i, j, lb, ub, big_w, big_n

  res2 = 0
  do i=1,h
    write(0, '("i/r: " I4 " " I0)') i, res2

    do concurrent (j = 1:n(i)*unfold_by:n(i))
      lb = j
      ub = j + n(i)
      big_segment(lb:ub) = segments(:n(i),i)
    end do
    do concurrent (j = 1:w(i)*unfold_by:w(i))
      lb = j
      ub = j + w(i)
      big_status(lb:ub) = status(:w(i),i)
    end do
    big_n = n(i)*unfold_by
    big_w = w(i)*unfold_by

    write(0, '("s: " *(I2))') big_segment(:big_n)
    write(0, '("a: " *(I1))') big_status(:big_w)
    
    res2 = res2 + number_of_matches(big_status(:big_w), big_segment(:big_n))
  end do

  ! write(0, '("h: " I0)') h
  ! write(0, '("n: " *(I6))') big_n(:h)
  ! write(0, '("w: " *(I6))') big_w(:h)
  ! write(0, '("s: " *(7I3:/"   "))') big_segments(:,:h)
  ! write(0, '("a: " *(21I3:/"   ") )') big_status(:,:h)

end block part2

#if defined PERF_TIME
  call cpu_time(t3)
#endif
output: block

  write(*, '("Part 1: " I0)') res1
  write(*, '("Part 2: " I0)') res2

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

recursive function number_of_matches(status, segments) result(num)
  integer(Kstat), dimension(:), intent(in) :: status
  integer(Kseg), dimension(:), intent(in) :: segments
  integer(Kres) :: num
  integer(Kres) :: n

  integer :: i, ub
  
  ! write(0, '("begin number_of_matches()")') 
  ! write(0, '("S= " *(I3))') status
  ! write(0, '("s= " *(I3))') segments

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

  ! write(0, '("i= " I0 ", " I0)') 1, ub
  num = 0
  do i = 1, ub
    n = number_of_matches_here(status(i:), segments)
    ! write(0, '("n[" I1 "]= " I0)') i, n
    num = num + n
    if (status(i) <= BROKE) exit
  end do

  ! write(0, '("end number_of_matches() -> " I0)') num
  
end function number_of_matches

recursive function number_of_matches_here(status, segments) result(num)
  integer(Kstat), dimension(:), intent(in) :: status
  integer(Kseg), dimension(:), intent(in) :: segments
  integer(Kres) :: num

  integer :: seg
  integer :: lb

  ! write(0, '("@s= " I0)') size(segments)

  seg = segments(1) ! try to lay out this next segment at the current point

  if (status(seg+1) == BROKE) then
    num = 0_Kres
    return ! there'd be an extra contiguous BROKE
  end if

  if (.not. all(status(:seg) <= MAYBE)) then
    num = 0_Kres
    return ! there aren't enough contiguous BROKE and FIXED
  end if

  ! at this point, we have successfully laid out the currently-pending segment
  ! therefore, we just need to truncate and search again

  lb = seg + 2
  num = number_of_matches(status(lb:), segments(2:))
  ! write(0, '("n: (rec)" I0)') num
end function number_of_matches_here
  

end program springs