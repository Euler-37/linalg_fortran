program check
   use linalg_fortran
   real(8)::a(3,3),e(3),a8(3,3)
   real(4)::b(3,3),w(3),b4(3,3)
   call random_number(a)
   b=a
   if(linalg%inv(a))then
      call linalg%print(a,"a")
   end if
   if(linalg%inv(b))then
      call linalg%print(b,"b")
   end if
   call linalg%gemm(a, a, a8, 'T', 'N', 1.d0, 0.d0)
   call linalg%gemm(b, b, b4, 'T', 'N', 1.0 ,  0.0)
   call linalg%eigh(a8,e)
   call linalg%eigh(b4,w)
   call linalg%print(e, "eigenvalue")
   call linalg%print(a8, "eigenvactor")
   call linalg%print(w, "eigenvalue")
   call linalg%print(b4, "eigenvactor")

end program check
