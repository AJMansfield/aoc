program seeds
  implicit none

  integer ios
  character(len=256) :: buf

  integer(kind=8), dimension(:), allocatable :: keys, vals
  integer(kind=8), dimension(:), allocatable :: rkeys, rklen, rvals, rvlen
  
  ! write(0,*) "===================================="
  block
    integer :: key_count, buf_len
    integer :: i
    read(*,'(T7 A)') buf ! skip "seeds:"

    buf_len = len_trim(buf)
    key_count = count([(buf(i:i) == ' ', i = 1,buf_len)]) ! number of spaces is number of values

    ! write(0,'("key count: " I2)') key_count

    allocate(keys(key_count))
    allocate(vals(key_count))

    allocate(rkeys(key_count/2))
    allocate(rklen(key_count/2))
    allocate(rvals(key_count/2))
    allocate(rvlen(key_count/2))
    
    ! write(0,'("keys: " *(I12))') keys
    ! write(0,'("vals: " *(I12))') vals
    ! write(0,'("buf: ''" A, "''")') buf(:buf_len)

    read(buf,*) vals

    rvals(:) = vals(1::2)
    rvlen(:) = vals(2::2)

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

    rkeys = rvals
    rklen = rvlen
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

      do i = lbound(keys,1), ubound(keys,1)
        ! simple point transform
        if (k <= keys(i) .and. keys(i) < k+l) then
          vals(i) = v + (keys(i) - k)
          ! write(0,'("map  : " I0, " -> " I0)') keys(i), vals(i)
        end if
      end do

      do i = lbound(rkeys,1), ubound(rkeys,1)
        ! range transform
        if (rkeys(i) + rklen(i) <= k) then
          ! ---
          !     ---
          ! no intersection

        else if (rkeys(i) >= k+l) then
          !     ---
          ! ---
          ! no intersection

        else if (rkeys(i) >= k .and. rkeys(i) + rklen(i) <= k+l) then
          ! write(0,*) "case: interior"
          !   ---
          ! -------
          ! fully contained, no need to split or change length
          rvals(i) = v + rkeys(i) - k

        else if (rkeys(i) < k .and. rkeys(i) + rklen(i) > k+l) then
          ! write(0,*) "case: exterior"
          ! -------
          !   ---
          ! need to carve into three pieces

          ! insert tail piece into keys
          rkeys = [rkeys, k + l]
          rklen = [rklen, rkeys(i) + rklen(i) - k - l]
          ! insert transformed center piece and default tail piece into vals
          rvals = [rvals, v, k + l]
          rvlen = [rvlen, l, rkeys(i) + rklen(i) - k - l]
          ! trim length of current piece to end before excised piece
          rklen(i) = k - rkeys(i)
          rvlen(i) = k - rkeys(i)

        else if (rkeys(i) < k) then
          ! write(0,*) "case: left"
          ! -----
          !   -----
          ! need to carve into two pieces
          ! insert transformed right piece into vals
          rvals = [rvals, v]
          rvlen = [rvlen, rklen(i) - k + rkeys(i) ]
          ! trim length of current piece to end before excised piece
          rklen(i) = k - rkeys(i)
          rvlen(i) = k - rkeys(i)

        else if (rkeys(i) + rklen(i) > k + l) then
          ! write(0,*) "case: right"
          !   -----
          ! -----
          ! need to carve into two pieces
          ! insert transformed middle segment
          rvals = [rvals, v + (rkeys(i) - k)]
          rvlen = [rvlen, l - (rkeys(i) - k)]
          ! move head of current piece and truncate length
          rvlen(i) = (rkeys(i) + rklen(i)) - (k + l)
          rvals(i) = k + l
          rklen(i) = (rkeys(i) + rklen(i)) - (k + l)
          rkeys(i) = k + l
        else
          write(0,*) "unknown case!"
          call exit(1)
        end if

      end do

    end block
    end do each_line
    
  end do each_table

  ! write(0,'("final: " *(I0," "))') vals
  write(*,'("Part 1: " I0)') minval(vals)
  write(*,'("Part 2: " I0)') minval(rvals)

end program seeds