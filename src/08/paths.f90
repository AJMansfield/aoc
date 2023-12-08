program paths
  implicit none

  integer :: ios

  integer, parameter :: n_char = 3
  character(len=*), parameter :: record_format = '(*(A3 XXXX A3 XX A3 X/))'
  character(len=n_char), parameter :: node_begin = 'AAA', node_end = 'ZZZ'
  integer, parameter :: record_size = (len(' = (, )') + 3*n_char)
  integer, parameter :: max_n_dirs = 512
  integer, parameter :: max_n_map = 1024
  integer, parameter :: max_n_cycles = 200

  integer :: n_dirs
  character(len=1), dimension(max_n_dirs) :: dir_chars
  integer, dimension(max_n_dirs) :: dirs

  integer :: n_map
  character(len=n_char), dimension(3,max_n_map) :: map

  integer :: n_ghosts
  character(len=n_char), dimension(max_n_map) :: ghosts

  integer :: i
  integer :: n_steps
  integer(kind=8) :: n_ghost_steps

  read(*, '(*(A1))', size=n_dirs, eor=1, advance='no') dir_chars
1 dirs = merge(2, 3, dir_chars == 'L') ! direction index in map table
  read(*,*,iostat=ios) ! skip blank line
  read(*, record_format, size=n_map, end=2, advance='no') map
2 n_map = n_map / record_size ! record length

  ! write(0, '("dirs: ''" *(A1) "''")') dir_chars(:n_dirs)
  ! write(0, '("entries: " I0/*(A3 "=" A3 "," A3:/))') n_map, map(:,:n_map)

  call trace_path(node_begin, node_end, map(:,:n_map), dirs(:n_dirs), n_steps)
  
  n_ghosts = 0
  do i = 1, n_map
    if (ends_with(map(1,i), 'A')) then
      n_ghosts = n_ghosts + 1
      ghosts(n_ghosts) = map(1,i)
    end if
  end do

  ! write(0, '("ghosts: " *(A3 : " "))') ghosts(:n_ghosts)

  call trace_ghost_path(ghosts(:n_ghosts), map(:,:n_map), dirs(:n_dirs), n_ghost_steps)

  write(*,'("Part 1: " I0)') n_steps
  write(*,'("Part 2: " I0)') n_ghost_steps

contains
  subroutine trace_path(trace_begin, trace_end, trace_map, trace_dirs, trace_steps)
    character(len=n_char), intent(in) :: trace_begin, trace_end
    integer, dimension(:), intent(in) :: trace_dirs
    character(len=n_char), dimension(:,:), intent(in) :: trace_map
    integer, intent(out) :: trace_steps

    character(len=n_char) :: node
    integer :: cycles
    integer :: dir_idx, map_idx

    trace_steps = -1
    
    node = trace_begin
    each_cycle: do cycles = 0, max_n_cycles
      do dir_idx = lbound(trace_dirs,1), ubound(trace_dirs,1)
        map_idx = findloc(map(1,:), node, dim=1)
        if(map_idx == 0) then
          trace_steps = -2
          return
        end if

        node = trace_map(dirs(dir_idx), map_idx)

        if (node == trace_end) then
          trace_steps = (cycles * size(trace_dirs,1)) + dir_idx
          exit each_cycle
        end if
      end do
    end do each_cycle

  end subroutine trace_path

  
  subroutine trace_ghost_path(trace_begin, trace_map, trace_dirs, trace_steps)
    character(len=n_char), dimension(:), intent(in) :: trace_begin
    integer, dimension(:), intent(in) :: trace_dirs
    character(len=n_char), dimension(:,:), intent(in) :: trace_map
    integer(kind=8), intent(out) :: trace_steps

    character(len=n_char), dimension(size(trace_begin)) :: node
    integer :: cycles
    integer :: dir_idx, node_idx
    integer, dimension(size(trace_begin)) :: map_idx
    logical, dimension(size(trace_begin)) :: is_end

    integer(kind=8) :: steps
    integer(kind=8), dimension(size(trace_begin)) :: last_end, period
    integer(kind=8) :: real_end, real_period


    last_end = -1
    period = -1

    trace_steps = -1
    steps = 0

    node = trace_begin
    each_cycle: do cycles = 0, max_n_cycles
      do dir_idx = lbound(trace_dirs,1), ubound(trace_dirs,1)
        
        do concurrent (node_idx = 1:size(node))
          map_idx(node_idx) = findloc(map(1,:), node(node_idx), dim=1)
        end do

        node = trace_map(dirs(dir_idx), map_idx)
        steps = steps + 1

        do concurrent (node_idx = 1:size(node))
          is_end(node_idx) = ends_with(node(node_idx), 'Z')
        end do

        if (all(is_end)) then
          trace_steps = steps
          return ! we got lucky, they all lined up perfectly
        end if

        where(last_end /= -1 .and. is_end) period = steps - last_end
        where(is_end) last_end = steps

        if (all(period /= -1)) exit each_cycle ! not so lucky, but we can calculate the full period using lcm functions LCM
      end do
    end do each_cycle

    ! write(0,'(*(A20 : " "))') node
    ! write(0,'(*(I20 : " "))') period
    ! write(0,'(*(G20.5 : " "))') real(period) / real(n_dirs)
    ! write(0,'(*(I20 : " "))') last_end

    ! no following the graph, we can just calculate how long until coincidence with just numbers

    block ! combine by folding in half, since combining a big thing and a small thing is slow
      integer :: i, j

      j = size(node)
      do
        if(mod(j,2) == 1) then
          call add_period(last_end(1), period(1), last_end(j), period(j))
        end if
        j = j/2
        do i = 1, j
          call add_period(last_end(i), period(i), last_end(i+j), period(i+j))
        end do
        if (j <= 1) exit
      end do

      trace_steps = last_end(1)
    end block

  end subroutine trace_ghost_path

  subroutine add_period(offset, period, addl_offset, addl_period)
    integer(kind=8), intent(inout) :: offset, period
    integer(kind=8), intent(in) :: addl_offset, addl_period

    integer(kind=8) :: off2, gcd1, gcd2
    off2 = addl_offset
    gcd1 = period
    gcd2 = addl_period

    do ! find the next coincidence between the pair
      if (offset < off2) then
        offset = offset + period
      else if (off2 < offset) then
        off2 = off2 + addl_period
      else
        exit
      end if
    end do

    do ! compute the GCD of their periods
      if (gcd1 > gcd2) then
        gcd1 = gcd1 - gcd2
      else if (gcd2 > gcd1) then
        gcd2 = gcd2 - gcd1
      else
        exit
      end if
    end do

    period = period * addl_period / gcd1

  end subroutine

  pure function ends_with(string, suffix)
    character(len=n_char), intent(in) :: string
    character(len=1), intent(in) :: suffix
    logical :: ends_with

    ends_with = string(n_char:n_char) == suffix
  end function ends_with

  ! pure function gcd()
end program paths