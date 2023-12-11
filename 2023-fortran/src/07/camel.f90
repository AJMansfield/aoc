program camel
  implicit none

  integer :: ios

  character(len=*), parameter :: norm_rank_chars = "23456789TJQKA"
  character(len=*), parameter :: wild_rank_chars = "J23456789TQKA"
  integer, parameter :: n = 5
  integer, parameter :: max_l = 1000
  integer :: i, l

  integer, parameter :: bid_bits = 16
  integer, parameter :: card_bits = 4
  integer, parameter :: type_bits = 4


  character(len=1), dimension(n) :: hand
  integer :: bid

  integer(kind=8) :: rate, pack
  integer(kind=8), dimension(max_l,2) :: packs
  integer total_winnings, wild_winnings

  do i = 1, max_l
    read(*, '(5A1 X I4)', iostat=ios) hand, bid
    if (ios /= 0) exit

    rate = hand_rating(hand, norm_rank_chars, .false.)
    pack = pack_rate_bid(rate, bid)

    packs(i,1) = pack

    rate = hand_rating(hand, wild_rank_chars, .true.)
    pack = pack_rate_bid(rate, bid)

    packs(i,2) = pack
  end do
  l = i - 1 ! length of the array


  call sort(packs(:l,1))
  call sort(packs(:l,2))

  total_winnings = 0
  wild_winnings = 0

  do i = 1, l
    bid = unpack_bid(packs(i,1))
    hand = unpack_hand(packs(i,1), norm_rank_chars)

    total_winnings = total_winnings + bid * i

    ! write(0,'(5A1 " " I4 " * " I4 " -> " I9)') hand, bid, i, total_winnings

    bid = unpack_bid(packs(i,2))
    hand = unpack_hand(packs(i,2), wild_rank_chars)

    wild_winnings = wild_winnings + bid * i

  end do

  write(*,'("Part 1: " I0)') total_winnings
  write(*,'("Part 2: " I0)') wild_winnings

contains
  pure function hand_rating(hand_chars, rank_chars, j_is_wild )
    character(len=1), dimension(n), intent(in) :: hand_chars
    character(len=*), intent(in) :: rank_chars
    logical, intent(in) :: j_is_wild
    integer(kind=8) :: hand_rating
    
    integer, dimension(n) :: hand_ranks
    integer, dimension(len(rank_chars)) :: rank_counts
    integer :: rank, count, i
    integer :: type_rate, rank_rate

    hand_ranks = index(rank_chars, hand_chars)
    rank_counts = 0
    rank_counts(hand_ranks) = rank_counts(hand_ranks) + 1

    if (j_is_wild) then
      rank = maxloc(rank_counts(2:), dim=1, back=.true.) + 1
      count = rank_counts(1) ! done in this order to avoid "if (rank /= 1)"
      rank_counts(rank) = rank_counts(rank) + count
      rank_counts(1) = rank_counts(1) - count
    end if

    rank_rate = 0
    do i = 1,5
      rank_rate = ior(lshift(rank_rate, card_bits), hand_ranks(i))
    end do

    type_rate = 0
    do i = 1,2
      rank = maxloc(rank_counts, dim=1, back=.true.)
      count = rank_counts(rank)
      if (count == 0) rank = 0
      rank_counts(rank) = 0
      type_rate = ior(lshift(type_rate, type_bits), count)
    end do

    hand_rating = ior(lshift(int(type_rate,kind=8), card_bits*n), iand(int(rank_rate,kind=8), maskr(card_bits*n,kind=8)))
  end function

  pure function pack_rate_bid(hand_rate, hand_bid)
    integer(kind=8), intent(in) :: hand_rate
    integer, intent(in) :: hand_bid
    integer(kind=8) :: pack_rate_bid

    pack_rate_bid = ior(lshift(hand_rate, bid_bits), iand(int(hand_bid, kind=8), maskr(bid_bits, kind=8)))
  end function

  pure function unpack_bid(packed_rate_and_bid)
    integer(kind=8), intent(in) :: packed_rate_and_bid
    integer unpack_bid

    unpack_bid = int(iand(packed_rate_and_bid, maskr(bid_bits, kind=8)))
  end function

  pure function unpack_rate(packed_rate_and_bid)
    integer(kind=8), intent(in) :: packed_rate_and_bid
    integer(kind=8) unpack_rate

    unpack_rate = rshift(packed_rate_and_bid,bid_bits)
  end function

  pure function unpack_hand(packed_rate_and_bid, rank_chars)
    integer(kind=8), intent(in) :: packed_rate_and_bid
    character(len=*), intent(in) :: rank_chars
    character(len=1), dimension(n) :: unpack_hand
    integer :: i, j

    do i = 1, n
      j = iand(int(rshift(packed_rate_and_bid, bid_bits + card_bits*(n-i))), maskr(card_bits))
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