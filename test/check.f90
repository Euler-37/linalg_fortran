program check
   use linalg_fortran
   real(8)::a(3,3)
   real(4)::b(3,3)
   call random_number(a)
   b=a
   if(linalg%inv(a))then
      call linalg%print(a,"a")
   end if
   if(linalg%inv(b))then
      call linalg%print(b,"b")
   end if
end program check
