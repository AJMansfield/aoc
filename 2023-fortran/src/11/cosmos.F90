program cosmos
  use iso_c_binding
  use iso_fortran_env
  implicit none
  
  integer, parameter :: max_stars = 500
  integer, parameter :: expansion_rate_1 = 2, expansion_rate_2 = 1000000, expansion_rate_2e = 100
  integer(kind=8) :: result_1, result_2, result_2e

#if defined PERF_TIME
  real t1, t2, t3, t4
  call cpu_time(t1)
#endif

main: block
  character(32768), target :: buf
  integer :: w,h
  character, dimension(:,:), pointer :: array

  block
    integer :: ios
    read(*, '(A)', size=w, advance='no', iostat=ios) buf
    
    h = 1
    do
      read(*,'(A)', iostat=ios) buf(h*w+1:)
      if (ios /= 0) exit
      h = h + 1
    end do

    call c_f_pointer(c_loc(buf), array, [w,h])
  end block
  
#if defined PERF_TIME
  call cpu_time(t2)
#endif

  call do_everything(array)
  
#if defined PERF_TIME
  call cpu_time(t3)
#endif

  write(*, '("Part 1: " I0)') result_1
  write(*, '("Part 2: " I0)') result_2
  write(*, '("Part 2e: " I0)') result_2e

end block main

#if defined PERF_TIME
  call cpu_time(t4)

  write(0,'("read : " F10.6)') t2 - t1
  write(0,'("work : " F10.6)') t3 - t2
  write(0,'("write: " F10.6)') t4 - t3
  write(0,'("total: " F10.6)') t4 - t1
#endif

contains

  subroutine do_everything(array)
    character, dimension(:,:), intent(in) :: array

    integer :: n
    integer, dimension(max_stars) :: is, js
    
    integer(kind=1), dimension(size(array, dim=1)) :: ei
    integer(kind=1), dimension(size(array, dim=2)) :: ej

    call collate_stars(array, is, js, n)
    call make_expansion_units(array, ei, ej)

    result_1 = calc_result_for_expansion(expansion_rate_1, is(:n), js(:n), ei, ej)
    result_2 = calc_result_for_expansion(expansion_rate_2, is(:n), js(:n), ei, ej)
    result_2e = calc_result_for_expansion(expansion_rate_2e, is(:n), js(:n), ei, ej)
  end subroutine

  pure subroutine collate_stars(array, is, js, n)
    character, dimension(:,:), intent(in) :: array
    integer, dimension(:), intent(out) :: is, js
    integer, intent(out) :: n
    integer :: i, j

    n = 0
    do j = lbound(array,2), ubound(array,2)
      do i = lbound(array,1), ubound(array,1)
        if (array(i,j) == '#') then
          n = n+1
          is(n) = i
          js(n) = j
        end if
      end do
    end do
  end subroutine

  pure subroutine make_expansion_units(array, ei, ej)
    character, dimension(:,:), intent(in) :: array
    integer(kind=1), dimension(size(array, dim=1)), intent(out) :: ei
    integer(kind=1), dimension(size(array, dim=2)), intent(out) :: ej
    integer :: i, j

    ei = 0
    ej = 0
    where (all(array=='.', dim=2)) ei = 1
    where (all(array=='.', dim=1)) ej = 1
    ! ei = merge(1,0,all(array=='.', dim=2)) ! for some reason merge doesn't work quite right at -O3
    ! ej = merge(1,0,all(array=='.', dim=1))

    ei = [(sum(ei(1:i)), i=1,size(ei))]
    ej = [(sum(ej(1:j)), j=1,size(ej))]
  end subroutine
  
  pure function calc_result_for_expansion(expansion_rate, is, js, ei, ej) result(result)
    integer, intent(in) :: expansion_rate
    integer, dimension(:), intent(in) :: is, js
    integer(kind=1), dimension(:), intent(in) :: ei
    integer(kind=1), dimension(:), intent(in) :: ej
    integer(kind=8) :: result

    integer, dimension(size(is)) :: xs, ys
    integer :: z
    integer :: a

    z = expansion_rate - 1

    xs = [(ei(is(a)) * z + is(a), a=1,size(is))]
    ys = [(ej(js(a)) * z + js(a), a=1,size(js))]

    ! TODO replace dist(x1+z*e1,x2+z*e2) with dist(x1,x2)+z*dist(e1,e2)

    result = sum_distance_pairs(xs, ys)
  end function

  pure function sum_distance_pairs(xs, ys) result(total)
    integer, dimension(:), intent(in) :: xs, ys
    integer(kind=8) :: total
    integer :: a, b
    total = 0
    do a = lbound(xs,1), ubound(xs,1)-1
      do b = a+1, ubound(xs,1)
        total = total + star_distance( xs(a),ys(a), xs(b),ys(b) )
      end do
    end do
  end function
  
  pure function star_distance(x1,y1,x2,y2) result(distance)
    integer, intent(in) :: x1,y1,x2,y2
    integer :: distance
    distance = abs(x2-x1) + abs(y2-y1)
  end function

end program cosmos