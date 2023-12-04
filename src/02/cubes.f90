program cubes
  implicit none
  

  integer ios
  character(len=4096) :: line
  integer :: game, i, j, c, n, acc1, acc2

  character(len=3), parameter :: name_colors = "rgb"
  integer, dimension(3), parameter :: max_colors = [12, 13, 14]
  logical :: over_max_flag
  integer, dimension(3) :: min_colors
  
  acc1 = 0
  acc2 = 0
  each_game: do
    read(*, '(A)', iostat=ios) line
    if ( ios /= 0 ) exit

    i = 6 ! skip past 'Game '
    j = i + scan(line(i:), ":") - 1 ! find colon after number
    read(line(i:j-1), *) game ! extract number from before colon

    ! write(*,*) "game number:", g

    over_max_flag = .false.
    min_colors = [0,0,0]

    each_draw: do
      i = i + scan(line(i:), ":;,") ! advance past delimiter
      read(line(i:), *, iostat=ios) n
      if ( ios /= 0 ) then 
        if ( .not. over_max_flag ) acc1 = acc1 + game
        exit each_draw
      end if

      i = i + scan(line(i:), name_colors) - 1 ! advance to color letter
      c = index(name_colors, line(i:i))

      ! write(*,*) "pull:", n, c

      min_colors(c) = max(min_colors(c), n)

      if (n > max_colors(c)) then
        over_max_flag = .true.
      end if
    end do each_draw

    acc2 = acc2 + (min_colors(1) * min_colors(2) * min_colors(3))

  end do each_game

  write(*,*) "Part 1:", acc1
  write(*,*) "Part 2:", acc2

end program cubes