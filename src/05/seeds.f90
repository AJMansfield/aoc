program seeds
  implicit none

  integer ios
  character(len=256) :: buf

  integer(kind=8), dimension(:), allocatable :: keys, vals

  block
    integer :: key_count, buf_len
    integer :: i
    read(*,'(T7 A)') buf ! skip "seeds:"

    buf_len = len_trim(buf)
    key_count = count([(buf(i:i) == ' ', i = 1,buf_len)]) ! number of spaces is number of values

    ! write(0,'("key count: " I2)') key_count

    allocate(keys(key_count))
    allocate(vals(key_count))

    
    ! write(0,'("keys: " *(I12))') keys
    ! write(0,'("vals: " *(I12))') vals
    ! write(0,'("buf: ''" A, "''")') buf(:buf_len)

    read(buf,*) vals

    read(*,'(A)',iostat=ios) buf ! skip blank line
    ! write(0,'("blank: ''" A, "''")') trim(buf)
    ! write(0,'("stat : " I0)') ios

  end block



  each_table: do

    read(*,'(A)',iostat=ios) buf ! skip table header
    ! write(0,'("head : ''" A, "''")') trim(buf)
    ! write(0,'("stat : " I0)') ios
    if (ios /= 0) exit

    keys = vals
    ! write(0,'("value: " *(I0," "))') keys

    each_line: do
    block
      integer(kind=8) :: v, k, l
      integer :: i

      read(*,'(A)',iostat=ios) buf
      ! write(0,'("line : ''" A, "''")') trim(buf)
      ! write(0,'("stat : " I0)') ios

      if (ios /= 0) exit each_table
      if (len_trim(buf) == 0) exit each_line

      read(buf,*) v,k,l

      ! do concurrent (i = lbound(keys,1):ubound(keys,1))
      do i = lbound(keys,1), ubound(keys,1)
        if (k <= keys(i) .and. keys(i) < k+l) then
          vals(i) = v + (keys(i) - k)
          ! write(0,'("map  : " I0, " -> " I0)') keys(i), vals(i)
        end if
      end do

    end block
    end do each_line
    
  end do each_table

  ! write(0,'("final: " *(I0," "))') vals
  write(*,'("Part 1: " I0)') minval(vals)

end program seeds