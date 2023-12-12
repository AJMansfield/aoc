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
    
    integer, dimension(size(array, dim=1)) :: ei
    integer, dimension(size(array, dim=2)) :: ej

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

  subroutine make_expansion_units(array, ei, ej)
    character, dimension(:,:), intent(in) :: array
    integer, dimension(size(array, dim=1)), intent(out) :: ei
    integer, dimension(size(array, dim=2)), intent(out) :: ej
    integer :: i, j

    ei = merge(1,0,all(array=='.', dim=2))
    ej = merge(1,0,all(array=='.', dim=1))

    ei = [(sum(ei(1:i)), i=1,size(ei))]
    ej = [(sum(ej(1:j)), j=1,size(ej))]
  end subroutine
  
  function calc_result_for_expansion(expansion_rate, is, js, ei, ej) result(result)
    integer, intent(in) :: expansion_rate
    integer, dimension(:), intent(in) :: is, js
    integer, dimension(:), intent(in) :: ei
    integer, dimension(:), intent(in) :: ej
    integer(kind=8) :: result

    integer, dimension(size(is)) :: xs, ys
    integer :: a

    xs = [(ei(is(a)) * (expansion_rate-1) + is(a), a=1,size(is))]
    ys = [(ej(js(a)) * (expansion_rate-1) + js(a), a=1,size(js))]
    
    ! write(0, '("xs:  " *(I4))') xs
    ! write(0, '("ys:  " *(I4))') ys

    result = sum_distance_pairs(xs, ys)
  end function

  function sum_distance_pairs(xs, ys) result(total)
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
  
  function star_distance(x1,y1,x2,y2) result(distance)
    integer, intent(in) :: x1,y1,x2,y2
    integer :: distance
    distance = abs(x2-x1) + abs(y2-y1)
    
    ! write(0, '("(" I4 "," I4 ")(" I4 "," I4 "):" I4)') x1,y1,x2,y2,distance

  end function

end program cosmos