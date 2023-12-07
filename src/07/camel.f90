program camel
  implicit none

  integer :: ios

  character(len=*), parameter :: rank_chars = " 23456789TJQKA"
  integer, parameter :: n = 5
  integer, parameter :: max_l = 1000
  integer :: i, l

  character(len=1), dimension(n) :: hand
  integer :: bid

  integer(kind=8) :: rate, pack
  integer(kind=8), dimension(max_l) :: packs
  integer total_winnings

  do i = 1, max_l
    read(*, '(5A1 X I3)', iostat=ios) hand, bid
    if (ios /= 0) exit

    rate = hand_rating(hand)
    pack = pack_rate_bid(rate, bid)

    ! write(0,'(I3 " " Z24.24 " " I30))') bid, pack, pack

    ! if (bid /= unpack_bid(pack)) then
    !   write(0,'(I3 " " Z15.14 " " I30)') bid, pack, pack
    ! end if

    packs(i) = pack
  end do
  l = i - 1 ! length of the array

  ! write(0,'(I0)') l


  call sort(packs(:l))

  total_winnings = 0
  
  do i = 1, l-1
    bid = unpack_bid(packs(i))
    rate = unpack_rate(packs(i))

    if (rate == unpack_rate(packs(i+1))) then
      ! write(0,'("duplicated hands: " Z7.7 " = " Z7.7)') rate, unpack_rate(packs(i+1))
    end if
  end do

  do i = 1, l
    bid = unpack_bid(packs(i))
    hand = unpack_hand(packs(i))

    ! write(0,'(Z11.11 " " 5A1 " " I0)') packs(i), hand, bid
    ! write(0,'("win: " I4 " * " I4 " -> " I9 " (" Z11.11 ")" )') bid, i, total_winnings, packs(i)
    
    total_winnings = total_winnings + bid * i

    write(0,'(5A1 " " I4 " * " I4 " -> " I9)') hand, bid, i, total_winnings
    
  end do


  ! write(0,'(4Z15.14)') packs(:l)

  write(*,'("Part 1: " I0)') total_winnings

  ! do i = lbound(1, max_l


contains
  pure function hand_rating(hand_chars)
    character(len=1), dimension(n), intent(in) :: hand_chars
    integer(kind=8) :: hand_rating
    
    integer, dimension(n) :: hand_ranks
    integer, dimension(len(rank_chars)) :: rank_counts
    integer :: rank, count, i
    integer :: type_rate, rank_rate

    hand_ranks = index(rank_chars, hand_chars)
    rank_counts = 0
    rank_counts(hand_ranks) = rank_counts(hand_ranks) + 1

    rank_rate = 0
    do i = 1,5
      rank_rate = ior(lshift(rank_rate, 4), hand_ranks(i))
    end do

    type_rate = 0
    do i = 1,2
      rank = maxloc(rank_counts, dim=1, back=.true.)
      count = rank_counts(rank)
      if (count == 0) rank = 0
      rank_counts(rank) = 0
      ! two bits per count(lead could technically have a third bit)
      type_rate = ior(lshift(type_rate, 4), count)

      ! ! four bits per hand card
      ! rank_rate = ior(lshift(rank_rate, 4), rank)
    end do

    hand_rating = ior(lshift(int(type_rate,kind=8), 4*5), iand(int(rank_rate,kind=8), maskr(4*5,kind=8)))
  end function

  pure function pack_rate_bid(hand_rate, hand_bid)
    integer(kind=8), intent(in) :: hand_rate
    integer, intent(in) :: hand_bid
    integer(kind=8) :: pack_rate_bid

    pack_rate_bid = ior(lshift(hand_rate, 16), iand(int(hand_bid, kind=8), maskr(16, kind=8)))
  end function

  pure function unpack_bid(packed_rate_and_bid)
    integer(kind=8), intent(in) :: packed_rate_and_bid
    integer unpack_bid

    unpack_bid = int(iand(packed_rate_and_bid, maskr(16, kind=8)))
  end function

  pure function unpack_rate(packed_rate_and_bid)
    integer(kind=8), intent(in) :: packed_rate_and_bid
    integer(kind=8) unpack_rate

    unpack_rate = rshift(packed_rate_and_bid,16)
  end function

  pure function unpack_hand(packed_rate_and_bid)
    integer(kind=8), intent(in) :: packed_rate_and_bid
    character(len=1), dimension(n) :: unpack_hand
    integer :: i, j

    do i = 1, n
      j = iand(int(rshift(packed_rate_and_bid, 16 + 4*(n-i))), maskr(4))
      unpack_hand(i) = rank_chars(j:j)
    end do
  end function

  subroutine sort(a)
    implicit none
    integer(kind=8), dimension(:), intent(inout) :: a
    integer :: i, j
    integer(kind=8) :: x
    
    do i = 2, size(a)
        x = a(i)
        j = i - 1
        do while (j >= 1)
            if (a(j) <= x) exit
            a(j + 1) = a(j)
            j = j - 1
        end do
        a(j + 1) = x
    end do
end subroutine

end program camel