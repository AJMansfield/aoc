program network
  implicit none

  integer :: ios

  integer, parameter :: n_char = 3
  character(len=*), parameter :: record_format = '(*(A3 XXXX A3 XX A3 X/))'
  character(len=n_char), parameter :: node_begin = 'AAA', node_end = 'ZZZ'
  integer, parameter :: record_size = (len(' = (, )') + 3*n_char)
  integer, parameter :: max_n_dirs = 512
  integer, parameter :: max_n_map = 1024
  integer, parameter :: max_n_cycles = 999

  character(len=n_char) :: node
  integer :: n_steps, n_cycles

  integer :: n_dirs
  character(len=1), dimension(max_n_dirs) :: dir_chars
  integer, dimension(max_n_dirs) :: dirs

  integer :: n_map
  character(len=n_char), dimension(3,max_n_map) :: map

  integer :: i, j

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
      j = findloc(map(1,:n_map), node, dim=1)
      node = map(dirs(i), j)
      if (node == node_end) then
        n_steps = (n_cycles * n_dirs) + i
        exit each_cycle
      end if
    end do
  end do each_cycle

  ! write(0, *) i, n_cycles, n_dirs, n_steps

  write(*,'("Part 1: " I0)') n_steps
  write(*,'("Part 2: " I0)') 0

end program network