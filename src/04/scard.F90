program scard
  implicit none

  integer :: ios
  integer :: card
  integer(kind=1), dimension(:), allocatable :: winning, drawn
  character(len=32) :: ifmt, ofmt

  integer :: total_points, total_cards, num_wins, num_points, card_multiplier
  integer, dimension(:), allocatable :: cards_won

  call set_up_formats(card, winning, drawn, ifmt, ofmt)
  
  allocate(cards_won(min(size(winning), size(drawn))))

  cards_won(:) = 1

  do while (ios == 0)
    ! write(0,ofmt) card, winning, drawn

    num_wins = count(spread(winning(:),1,size(drawn)) == spread(drawn(:),2,size(winning)))
    num_points = merge(2**(num_wins-1), 0, num_wins > 0)

    total_points = total_points + num_points
    
    card_multiplier = cards_won(1)
    cards_won(:size(cards_won)-1) = cards_won(2:)
    cards_won(size(cards_won)) = 1
    cards_won(:num_wins) = cards_won(:num_wins) + card_multiplier

    total_cards = total_cards + card_multiplier

    read(*, ifmt, iostat=ios) card, winning, drawn
  end do

  write(*,*) "Part 1:", total_points
  write(*,*) "Part 2:", total_cards

contains

  subroutine set_up_formats(card, winning, drawn, ifmt, ofmt)
    implicit none
    integer, intent(out) :: card
    integer(kind=1), dimension(:), allocatable, intent(out) :: winning, drawn
    character(len=32), intent(out) :: ifmt, ofmt

    character(len=256) :: firstline
    integer :: wi, di, ei, clen, wdim, ddim

    read(*, '(A)') firstline
    wi = index(firstline, ':')
    di = index(firstline, '|')
    ei = len_trim(firstline)
    clen = wi - 6
    wdim = (di - wi - 2) / 3
    ddim = (ei - di) / 3

    write(ifmt, '("(T6 I", I0, " X ", I0, "I3 XX ", I0, "I3)")') clen, wdim, ddim
    write(ofmt, '("(''Card '' I", I0, " '':'' ", I0, "I3 '' |'' ", I0, "I3)")') clen, wdim, ddim
    allocate(winning(wdim))
    allocate(drawn(ddim))

    read(firstline, ifmt, iostat=ios) card, winning, drawn
  end subroutine set_up_formats

end program scard