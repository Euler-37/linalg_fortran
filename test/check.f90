program main
   use linalg_fortran
   implicit none
   real(8)::a(3,3),c(3,3),d(3,3)
   real(8)::e(3)
   complex(8)::b(3)
   call random_number(a)
   b=cmplx(a(:,1),a(:,2),8)
   call linalg%print(a,"a")
   call linalg%print(b,"b")
   a=1.d0
   c=2.d0
   write(*,*)linalg%equal(a,c)
   call linalg%eye(a)
   call linalg%print(a,"a")
   call random_number(a)
   call linalg%print(a,"a")
   if(linalg%inv(a))then
      call linalg%print(a,"Inv a")
   end if
   call linalg%print(linalg%det(a),"Det (a)")
   call random_number(a)
   call random_number(c)
   !call linalg%gemm(a,c,d,"N","N",1.d0,0.d0)
   call linalg%gemm(a,c,d)
   write(*,*)linalg%equal(d,matmul(a,c))

   call linalg%print(matmul(matmul(transpose(a),c),a),"A^T C A")
   call linalg%geut(a,c)
   call linalg%print(c,"A^T C A")
   call random_number(a)
   a=transpose(a)+a
   c=a
   call linalg%eigh(a,e)
   call linalg%print(e,"EIG")
   call linalg%geut(a,c)
   call linalg%print(c,"B")
end program main
