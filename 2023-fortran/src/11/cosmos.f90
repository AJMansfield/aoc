program cosmos
  use iso_c_binding
  implicit none
  
  integer, parameter :: max_stars = 500
  integer, parameter :: expansion_rate_1 = 2, expansion_rate_2 = 1000000, expansion_rate_2e = 100
  integer(kind=8) :: result_1, result_2, result_2e

main: block
  character(32768), target :: buf
  integer :: w,h
  character, dimension(:,:), pointer :: array

  block
    integer :: ios
    read(*, '(A)', size=w, advance='no', iostat=ios) buf
    
    do h = 1, huge(h) ! unbounded on purpose
      read(*,'(A)', iostat=ios) buf(h*w+1:)
      if (ios /= 0) exit
    end do

    call c_f_pointer(c_loc(buf), array, [w,h])
  end block


  call do_everything(array)

  
  write(*, '("Part 1: " I0)') result_1
  write(*, '("Part 2: " I0)') result_2
  write(*, '("Part 2e: " I0)') result_2e

end block main

contains

  subroutine do_everything(array)
    character, dimension(:,:), intent(in) :: array

    integer :: n
    integer, dimension(max_stars) :: is, js

    call collate_stars(array, is, js, n)

    result_1 = calc_result_for_expansion(array, expansion_rate_1, is(:n), js(:n))
    result_2 = calc_result_for_expansion(array, expansion_rate_2, is(:n), js(:n))
    result_2e = calc_result_for_expansion(array, expansion_rate_2e, is(:n), js(:n))

  end subroutine

  pure subroutine collate_stars(array, is, js, n)
    character, dimension(:,:), intent(in) :: array
    integer, dimension(:), intent(out) :: is, js
    integer, intent(out) :: n
    integer :: i, j

    n = 0
    do i = lbound(array,1), ubound(array,1)
      do j = lbound(array,2), ubound(array,2)
        if (array(i,j) == '#') then
          n = n+1
          is(n) = i
          js(n) = j
        end if
      end do
    end do
  end subroutine
  
  pure function calc_result_for_expansion(array, expansion_rate, is, js) result(result)
    character, dimension(:,:), intent(in) :: array
    integer, intent(in) :: expansion_rate
    integer, dimension(:), intent(in) :: is, js
    integer(kind=8) :: result

    integer, dimension(size(is)) :: xs, ys
    integer, dimension(size(array, dim=1)) :: i2x
    integer, dimension(size(array, dim=2)) :: j2y
    integer :: a
    
    call expansion(array, expansion_rate, i2x, j2y)
    xs = [(i2x(is(a)), a=1,size(is))]
    ys = [(j2y(js(a)), a=1,size(js))]
    result = sum_distance_pairs(xs, ys)
  end function

  pure subroutine expansion(array, expansion_rate, i2x, j2y)
    character, dimension(:,:), intent(in) :: array
    integer, intent(in) :: expansion_rate
    integer, dimension(size(array, dim=1)), intent(out) :: i2x
    integer, dimension(size(array, dim=2)), intent(out) :: j2y
    integer :: i, j

    i2x = 1
    j2y = 1

    where (all(array=='.', dim=2)) i2x = expansion_rate
    where (all(array=='.', dim=1)) j2y = expansion_rate

    i2x = [(sum(i2x(1:i)), i=1,size(i2x))]
    j2y = [(sum(j2y(1:j)), j=1,size(j2y))]
  end subroutine

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