program treb
  implicit none

  integer :: ios
  character(len=300) :: line

  integer :: pos, val, linetotal, acc1, acc2
  acc1 = 0
  acc2 = 0

  do
    read(*, '(A)', iostat=ios) line
    if ( ios /= 0 ) exit
    
    
    ! Part 1:

    pos = scan(line, '0123456789')
    read (line (pos:pos), *) val
    linetotal = 10*val

    pos = scan(line, '0123456789', back=.true.)
    read (line (pos:pos), *) val
    linetotal = linetotal + val

    acc1 = acc1 + linetotal

    ! write(*,*), 'part 1 line: ', linetotal


    ! Part 2:

    linetotal = 10*scan_for_spelled_digit(line, back=.false.) + scan_for_spelled_digit(line, back=.true.)
    acc2 = acc2 + linetotal

    ! write(*,*) 'part 2 line: ', linetotal

  end do

  write(*,*) 'Part 1:', acc1
  write(*,*) 'Part 2:', acc2

contains

  function scan_for_spelled_digit(line, back) result(num)
    implicit none
    character(len=*), intent(in) :: line
    logical, intent(in), optional :: back
    integer :: num

    character(len=*), dimension(0:9), parameter :: digits = &
      [character(len=5) :: "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

    integer :: dig, idx, best_dig, best_idx
    best_dig = -1
    if (back) then 
      best_idx = -1
    else
      best_idx = huge(best_idx)
    end if

    ! scan for a numeral character
    idx = scan(line, '0123456789', back=back)
    if (idx /= 0) then
      best_idx = idx
      read (line (idx:idx), *) best_dig
      ! write(*,*) "numeral ", best_dig, " at position ", idx
    end if
    
    ! scan for a number word
    do dig = 0, 9
      idx = index(line, trim(digits(dig)), back)
      if (idx /= 0) then
        ! write(*,*) "spelled ", dig, " @ ", idx
        if (merge(idx > best_idx, idx < best_idx, back)) then
          best_idx = idx
          best_dig = dig
        end if
      end if
    end do

    num = best_dig

  end function scan_for_spelled_digit
!     implicit none
!     character(len=*), intent(in) :: line

!     integer :: tens, ones, num
!     integer :: i, ios

!     do i = 1, len(line)
!       read


!     end do

!   end function line_cal_value

end program treb