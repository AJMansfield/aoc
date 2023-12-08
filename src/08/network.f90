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

  integer :: n_dirs
  character(len=1), dimension(max_n_dirs) :: dir_chars
  integer, dimension(max_n_dirs) :: dirs

  integer :: n_map
  character(len=n_char), dimension(3,max_n_map) :: map

  integer :: n_steps

  read(*, '(*(A1))', size=n_dirs, eor=1, advance='no') dir_chars
1 dirs = merge(2, 3, dir_chars == 'L') ! direction index in map table
  read(*,*,iostat=ios) ! skip blank line
  read(*, record_format, size=n_map, end=2, advance='no') map
2 n_map = n_map / record_size ! record length

  ! write(0, '("dirs: ''" *(A1) "''")') dir_chars(:n_dirs)
  ! write(0, '("entries: " I0/*(A3 "=" A3 "," A3:/))') n_map, map(:,:n_map)

  call trace_path(node_begin, node_end, map(:,:n_map), dirs(:n_dirs), n_steps)

  ! write(0, *) i, n_cycles, n_dirs, n_steps

  write(*,'("Part 1: " I0)') n_steps
  write(*,'("Part 2: " I0)') 0

contains
  subroutine trace_path(trace_begin, trace_end, trace_map, trace_dirs, trace_steps)
    character(len=n_char), intent(in) :: trace_begin, trace_end
    integer, dimension(:), intent(in) :: trace_dirs
    character(len=n_char), dimension(:,:), intent(in) :: trace_map
    integer, intent(out) :: trace_steps

    character(len=n_char) :: node
    integer :: cycles
    integer :: dir_idx, map_idx

    node = trace_begin
    each_cycle: do cycles = 0, max_n_cycles
      do dir_idx = lbound(trace_dirs,1), ubound(trace_dirs,1)
        map_idx = findloc(map(1,:), node, dim=1)
        node = trace_map(dirs(dir_idx), map_idx)
        if (node == trace_end) then
          trace_steps = (cycles * size(trace_dirs,1)) + dir_idx
          exit each_cycle
        end if
      end do
    end do each_cycle

  end subroutine trace_path
end program network