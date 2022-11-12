program check
   use iso_linalg_mod
   real(8)::a(3,3)
   call random_number(a)
   if(linalg%inv(a))then
      call linalg%print(a,"a")
   end if
end program check
