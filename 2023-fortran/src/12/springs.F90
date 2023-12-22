program springs
  use iso_c_binding
  implicit none
  
  integer, parameter :: max_h = 1000, max_n = 6, max_w_line = 32, max_w = 21
  integer, parameter :: unfold_by = 5
  integer, parameter :: Kstat = 1, Kseg = 1, Kres=8

  integer(Kres), parameter :: CACHE_MISS = -1_Kres

  integer(Kstat), parameter :: BROKE = 5, MAYBE = 6, FIXED = 7

  integer(Kres) :: res1, res2

#if defined PERF_TIME
  real :: t1, t2, t3, t4
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

  res1 = 0
  do i=1,h
    res1 = res1 + number_of_matches(status(:w(i),i), segments(:n(i),i))
  end do

end block part1
part2: block
  integer(Kseg), dimension(max_n*unfold_by) :: big_segment
  integer(Kstat), dimension((max_w+1)*unfold_by-1) :: big_status

  integer :: i, j, lb, ub, big_w, big_n

  res2 = 0
  do i=1,h
    big_n = n(i)*unfold_by
    big_w = (w(i)+1)*unfold_by-1

    do concurrent (j = 1:big_n:n(i))
      lb = j
      ub = j + n(i)
      big_segment(lb:ub) = segments(:n(i),i)
    end do
    big_status = MAYBE
    do concurrent (j = 1:big_w:w(i)+1)
      lb = j
      ub = j + w(i)
      big_status(lb:ub) = status(:w(i),i)
    end do

    res2 = res2 + number_of_matches(big_status(:big_w), big_segment(:big_n))
  end do
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

function number_of_matches(status, segments) result(num)
  integer(Kstat), dimension(:), intent(in) :: status
  integer(Kseg), dimension(:), intent(in) :: segments
  
  integer(Kres) :: num

  integer(Kres), dimension(size(status),size(segments)), target :: cache_mem
  integer(Kres), dimension(:,:), pointer :: cache
  cache => cache_mem

  call clear_cache(cache)

  num = number_of_matches_anywhere(status, segments, cache)

end function number_of_matches

recursive function number_of_matches_anywhere(status, segments, cache) result(num)
  integer(Kstat), dimension(:), intent(in) :: status
  integer(Kseg), dimension(:), intent(in) :: segments
  integer(Kres), dimension(:,:), intent(inout), pointer :: cache
  integer(Kres) :: num

  integer :: i, ub

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

  num = 0
  do i = 1, ub
    num = num + number_of_matches_here(status(i:), segments, cache)
    if (status(i) <= BROKE) exit
  end do

end function number_of_matches_anywhere

recursive function number_of_matches_here(status, segments, cache) result(num)
  integer(Kstat), dimension(:), intent(in) :: status
  integer(Kseg), dimension(:), intent(in) :: segments
  integer(Kres), dimension(:,:), intent(inout), pointer :: cache
  integer(Kres) :: num

  integer :: seg
  
  call read_cache(status, segments, cache, num)
  if (num /= CACHE_MISS) return ! caching makes this O(good) instead of O(terrible)


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

  num = number_of_matches_anywhere(status(seg+2:), segments(2:), cache)
  
  call write_cache(status, segments, cache, num)

end function number_of_matches_here

pure subroutine read_cache(status, segments, cache, val)
  integer(Kstat), dimension(:), intent(in) :: status
  integer(Kseg), dimension(:), intent(in) :: segments
  integer(Kres), dimension(:,:), intent(inout), pointer :: cache
  integer(Kres), intent(out) :: val

  if (associated(cache)) then
    val = cache(size(status),size(segments))
  else
    val = CACHE_MISS
  end if
end subroutine

pure subroutine write_cache(status, segments, cache, val)
  integer(Kstat), dimension(:), intent(in) :: status
  integer(Kseg), dimension(:), intent(in) :: segments
  integer(Kres), dimension(:,:), intent(inout), pointer :: cache
  integer(Kres), intent(in) :: val

  if (associated(cache)) then
    cache(size(status),size(segments)) = val
  end if
end subroutine

pure subroutine clear_cache(cache)
  integer(Kres), dimension(:,:), intent(inout), pointer :: cache

  if (associated(cache)) then
    cache(:,:) = CACHE_MISS
  end if
end subroutine
  
end program springs