program network
  implicit none

  integer :: ios

  integer, parameter :: n_char = 3
  character(len=*), parameter :: record_format = '(*(A3 XXXX A3 XX A3 X/))'
  integer, parameter :: record_size = (len(' = (, )') + 3*n_char)

  character(len=n_char), parameter :: node_begin = 'AAA', node_end = 'ZZZ'
  character(len=n_char) :: node, next_node
  integer :: n_steps, n_cycles

  integer :: n_dirs
  character(len=1), dimension(64) :: dir_chars
  integer, dimension(64) :: dirs

  integer :: n_map
  character(len=n_char), dimension(3,16) :: map

  integer :: i, j, limit
  limit = 9

  read(*, '(*(A1))', size=n_dirs, eor=1, advance='no') dir_chars
1 dirs = merge(2, 3, dir_chars == 'L') ! direction index in map table
  read(*,*,iostat=ios) ! skip blank line
  read(*, record_format, size=n_map, end=2, advance='no') map
2 n_map = n_map / record_size ! record length

  ! write(0, '("dirs: ''" *(A1) "''")') dir_chars(:n_dirs)
  ! write(0, '("entries: " I0/*(A3 "=" A3 "," A3:/))') n_map, map(:,:n_map)

  node = node_begin
  n_cycles = 0
  each_cycle: do n_cycles = 0, 999
    do i = 1,n_dirs
      j = findloc(map(:,1), node, dim=1)
      next_node = map(dirs(i), j)
      
      ! write(0, '(A3 " " A1 " " A3)') node, dir_chars(i:i), next_node
      node = next_node
      if (node == node_end) then
        n_steps = (n_cycles * n_dirs) + i
        exit each_cycle
      end if

      limit = limit - 1
      if (limit <= 0) call exit(1)
    end do
  end do each_cycle

  ! write(0, *) i, n_cycles, n_dirs, n_steps

  write(*,'("Part 1: " I0)') n_steps
  write(*,'("Part 2: " I0)') 0

end program network