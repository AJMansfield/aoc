program reflection
  use iso_c_binding
  implicit none
  
  integer, parameter :: number_of_cycles = 1000000000
  integer, parameter :: hi_comp = 5040

block
  integer :: ios
  character(16384), target :: buf
  character, dimension(:,:), pointer :: arr
  integer, dimension(2) :: res

  call read_input_block(buf, arr, iostat=ios)
  call main(arr, res)

  write(*, '("Part 1: " I0)') res(1)
  write(*, '("Part 2: " I0)') res(2)
end block

contains

subroutine main(arr, res)
  character, dimension(:,:), intent(inout) :: arr
  integer, dimension(2), intent(out) :: res
  integer :: iter, cycle_len

  integer(kind=8), dimension(32) :: cache
  integer(kind=8) :: curr_hash
  logical :: finish_mode

  cache = arr_hash(arr)
  finish_mode = .false.

  ! call print_char_mat('("A(:" I0 "," I3 ")=" *(A))', arr)
  ! prev_arr = arr

  call slide_north(arr)

  res(1) = north_load(arr)

  iter = 1
  
  do 
    if (mod(iter, 1000) == 0) then
      write(0,'("iter " I0)') iter
    end if

    call slide_north(arr)
    call slide_west(arr)
    call slide_south(arr)
    call slide_east(arr)

    if (.not. finish_mode) then
      curr_hash = arr_hash(arr)
      cycle_len = findloc(cache, curr_hash, 1)
      if (cycle_len > 0) then
        ! write(0,*) "found repeat!"
        finish_mode = .true.

        write(0,'("iter " I0)') iter
        ! write(0,'("pd " I0)') cycle_len
        ! increment iter by the largest multiple of the periodicity less than the remaining number of cycles
        iter = iter + (number_of_cycles - iter) / cycle_len * cycle_len
        write(0,'("iter " I0)') iter
      end if

      ! push back into periodicity detection array
      cache = [curr_hash, cache(:size(cache,1)-1)]
    end if

    if (iter >= number_of_cycles) exit
    iter = iter + 1
  end do

  res(2) = north_load(arr)

end subroutine main

pure integer(kind=8) function arr_hash(arr)
  character, dimension(:,:), intent(in) :: arr
  integer i, j
  logical mask
  arr_hash = 0
  do j = 1,size(arr,2)
    do i = 1,size(arr,1)
      mask = arr(i,j)=='O'
      arr_hash = arr_hash*311 + merge(1,0,mask)
    end do
  end do
end function

pure integer function north_load(arr)
  character, dimension(:,:), intent(in) :: arr
  integer :: j
  north_load = sum( spread((/(j, j=size(arr,2),1,-1)/), 1, size(arr,1)), mask=(arr=='O'))
end function north_load

pure subroutine slide_north(arr)
  character, dimension(:,:), intent(inout) :: arr
  integer :: i
  do concurrent (i = 1:size(arr,1))
    call slide_left(arr(i,:))
  end do
end subroutine slide_north

pure subroutine slide_south(arr)
  character, dimension(:,:), intent(inout) :: arr
  integer :: i
  do concurrent (i = 1:size(arr,1))
    call slide_left(arr(i,size(arr,2):1:-1))
  end do
end subroutine slide_south

pure subroutine slide_west(arr)
  character, dimension(:,:), intent(inout) :: arr
  integer :: j
  do concurrent (j = 1:size(arr,2))
    call slide_left(arr(:,j))
  end do
end subroutine slide_west

pure subroutine slide_east(arr)
  character, dimension(:,:), intent(inout) :: arr
  integer :: j
  do concurrent (j = 1:size(arr,2))
    call slide_left(arr(size(arr,1):1:-1,j))
  end do
end subroutine slide_east


pure subroutine slide_left(row)
  character, dimension(:), intent(inout) :: row
  integer :: pos_start, pos_end

  pos_end = 1
  do pos_start = 1,size(row,1)
    select case (row(pos_start))
    case ('O')
      row(pos_start) = '.'
      row(pos_end) = 'O'
      pos_end = pos_end + 1
    case ('#')
      pos_end = pos_start + 1
    case ('.')
      ! no-op
    case default
      ! $ ASSUME FALSE
      row(pos_start) = '!'
    end select
  end do
end subroutine slide_left

subroutine read_input_block(buf, arr, iostat)
  character(*), target, intent(out) :: buf
  character, dimension(:,:), pointer, intent(out) :: arr
  integer, intent(out), optional :: iostat

  integer :: w, h

  read(*, '(A)', advance='no', size=w, iostat=iostat) buf

  h = 1
  do
    read(*, '(A)', iostat=iostat) buf(h*w+1:(h+1)*w)
    if (iostat /= 0) exit
    if (len_trim(buf(h*w+1:(h+1)*w)) /= w) exit
    h = h + 1
  end do

  call c_f_pointer(c_loc(buf), arr, [w,h])
end subroutine read_input_block

subroutine print_char_mat(linefmt, mat)
  character(*), intent(in) :: linefmt
  character, dimension(:,:), intent(in) :: mat

  integer :: i
  do i = lbound(mat, dim=2), ubound(mat, dim=2)
    write(0,linefmt) size(mat(:,i)), i, mat(:,i)
  end do
end subroutine print_char_mat

end program reflection