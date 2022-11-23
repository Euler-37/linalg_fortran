module linalg_fortran
   use iso_fortran_env,only:r8=>real64,r4=>real32
   implicit none
   private
   type::linalg_t
   contains
      generic::inv   => inv_c8  ,inv_r8  , inv_c4  ,inv_r4       
      generic::det   => det_c8  ,det_r8  , det_c4  ,det_r4
      generic::eigh  => eigh_c8 ,eigh_r8 , eigh_c4 ,eigh_r4
      generic::gemm  => gemm_c8 ,gemm_r8 , gemm_c4 ,gemm_r4
      generic::eye   => eye_c8  ,eye_r8  , eye_c4  ,eye_r4
      generic::print => print_c8,print_r8, print_c4,print_r4
      generic::geut  => geut_c8 ,geot_r8 , geut_c4 ,geot_r4
      generic::equal => equal_c8,equal_r8, equal_c4,equal_r4
      procedure,private,nopass::inv_c8
      procedure,private,nopass::inv_r8
      procedure,private,nopass::inv_c4
      procedure,private,nopass::inv_r4
      procedure,private,nopass::det_c8
      procedure,private,nopass::det_r8
      procedure,private,nopass::det_c4
      procedure,private,nopass::det_r4
      procedure,private,nopass::eigh_c8
      procedure,private,nopass::eigh_r8
      procedure,private,nopass::eigh_c4
      procedure,private,nopass::eigh_r4
      procedure,private,nopass::gemm_c8
      procedure,private,nopass::gemm_r8
      procedure,private,nopass::gemm_c4
      procedure,private,nopass::gemm_r4
      procedure,private,nopass::eye_c8
      procedure,private,nopass::eye_r8
      procedure,private,nopass::eye_c4
      procedure,private,nopass::eye_r4
      procedure,private,nopass::print_c8
      procedure,private,nopass::print_r8
      procedure,private,nopass::print_c4
      procedure,private,nopass::print_r4
      procedure,private,nopass::geut_c8
      procedure,private,nopass::geot_r8
      procedure,private,nopass::geut_c4
      procedure,private,nopass::geot_r4
      procedure,private,nopass::equal_c8
      procedure,private,nopass::equal_r8
      procedure,private,nopass::equal_c4
      procedure,private,nopass::equal_r4
   end type linalg_t
   type(linalg_t),public::linalg
   complex(r8),parameter::one_c8 =cmplx(1.0_r8,0.0_r8,r8)
   complex(r8),parameter::zero_c8=cmplx(0.0_r8,0.0_r8,r8)
   complex(r4),parameter::one_c4 =cmplx(1.0_r4,0.0_r4,r4)
   complex(r4),parameter::zero_c4=cmplx(0.0_r4,0.0_r4,r4)
   real(r8)   ,parameter::one_r8 =1.0_r8
   real(r8)   ,parameter::zero_r8=0.0_r8
   real(r4)   ,parameter::one_r4 =1.0_r4
   real(r4)   ,parameter::zero_r4=0.0_r4
   real(r8)   ,parameter::eps_r8 =1.0e-12_r8
   real(r4)   ,parameter::eps_r4 =1.0e-6_r4
contains
   logical function equal_c8(a,b)result(res)
      !! Matrix A == B
      complex(r8),intent(in)::a(:,:)
      complex(r8),intent(in)::b(:,:)
      res=all(abs(a-b)<eps_r8)
   end function equal_c8
   logical function equal_r8(a,b)result(res)
      !! Matrix A == B
      real(r8),intent(in)::a(:,:)
      real(r8),intent(in)::b(:,:)
      res=all(abs(a-b)<eps_r4)
   end function equal_r8

   subroutine print_c8(a,name)
      !! Print Matrix A
      complex(r8),intent(in) :: a(..)
      character(len=*),intent(in)::name
      integer::n,i
      select rank(a)
      rank(2)
      write(*,"(3A)")"matrix ",name,"="
      n=size(a,1)
      do i=1,n
         write(*,"(*(F10.4,'+',F10.4,'i'))")a(i,:)
      end do
      rank(1)
         write(*,"(3A)")"vector ",name,"="
         write(*,"(*(F10.4,'+',F10.4,'i'))")a
      rank(0)
         write(*,"(3A)")"num ",name,"="
         write(*,"(*(F10.4,'+',F10.4,'i'))")a
      end select
   end subroutine print_c8

   subroutine print_r8(a,name)
      !! Print Matrix A
      real(r8),intent(in) :: a(..)
      character(len=*),intent(in)::name
      integer::n,i
      select rank(a)
      rank(2)
         write(*,"(3A)")"matrix ",name,"="
         n=size(a,1)
         do i=1,n
            write(*,"(*(F10.4))")a(i,:)
         end do
      rank(1)
         write(*,"(3A)")"vector ",name,"="
         write(*,"(F10.4)")a
      rank(0)
         write(*,"(3A)")"num ",name,"="
         write(*,"(F10.4)")a
      end select
   end subroutine print_r8


   subroutine eye_c8(a)
      !! Eye Matrix
      complex(r8),intent(inout) :: a(:,:)
      integer::n,i
      n=size(a,1)
      a=zero_c8
      do i=1,n
         a(i,i)=one_c8
      end do
   end subroutine eye_c8
   subroutine eye_r8(a)
      !! Eye Matrix
      real(r8),intent(inout) :: a(:,:)
      integer::n,i
      n=size(a,1)
      a=zero_r8
      do i=1,n
         a(i,i)=one_r8
      end do
   end subroutine eye_r8

   logical function inv_c8(a)result(res)
      !! inverse of matrix a
      complex(r8),intent(inout) :: a(:,:)
      integer                  :: info
      integer,allocatable      :: ipiv(:)
      complex(r8),allocatable   :: work(:)
      integer::n,i
      n=size(a,1)
      allocate(ipiv(n))
      call zgetrf(n,n,a,n,ipiv,info)
      res=(info==0)
      if(.not.res)return
      do i=1,n
         if(abs(a(i,i))<eps_r8)then
            res=.false.
            return
         end if
      end do
      allocate(work(n))
      call zgetri(n,a,n,ipiv,work,n,info)
      res=(info==0)
      deallocate(work)
      deallocate(ipiv)
   end function inv_c8
   logical function inv_r8(a)result(res)
      !! inverse of matrix a
      real(r8),intent(inout) :: a(:,:)
      integer               :: info
      integer,allocatable   :: ipiv(:)
      real(r8),allocatable   :: work(:)
      integer::n,i
      n=size(a,1)
      allocate(ipiv(n))
      call dgetrf(n,n,a,n,ipiv,info)
      res=(info==0)
      if(.not.res)return
      do i=1,n
         if(abs(a(i,i))<eps_r8)then
            res=.false.
            return
         end if
      end do
      allocate(work(n))
      call dgetri(n,a,n,ipiv,work,n,info)
      res=(info==0)
      deallocate(work)
      deallocate(ipiv)
   end function inv_r8

   complex(r8) function det_c8(a)result(res)
      !! Determinant of matrix a
      complex(r8),intent(in)    :: a(:,:)
      integer                  :: info
      integer,allocatable      :: ipiv(:)
      integer::n,i
      complex(r8),allocatable:: tmpa(:,:)
      n=size(a,1)
      allocate(tmpa,source=a)
      allocate(ipiv(n))
      call zgetrf(n,n,tmpa,n,ipiv,info)
      if(info/=0)then
         res=zero_c8
      else
         res=one_c8
         do i=1,n
            res=res*tmpa(i,i)*merge(1,-1,ipiv(i)==i)
         end do
      end if
      deallocate(ipiv)
      deallocate(tmpa)
   end function det_c8
   complex(r8) function det_r8(a)result(res)
      !! Determinant of matrix a
      real(r8),intent(in) :: a(:,:)
      integer               :: info
      integer,allocatable   :: ipiv(:)
      integer::n,i
      real(r8),allocatable::tmpa(:,:)
      n=size(a,1)
      allocate(tmpa,source=a)
      allocate(ipiv(n))
      call dgetrf(n,n,tmpa,n,ipiv,info)
      if(info/=0)then
         res=zero_r8
      else
         res=one_r8
         do i=1,n
            res=res*tmpa(i,i)*merge(1,-1,ipiv(i)==i)
         end do
      end if
      deallocate(ipiv)
      deallocate(tmpa)
   end function det_r8

   subroutine eigh_c8(a,e)
      !! call zheev "V","U"
      complex(r8),intent(inout) :: a(:,:)
      real(r8),intent(out)      :: e(:)
      complex(r8),allocatable   :: work(:)
      real(r8),allocatable      :: rwork(:)
      integer                  :: iflag
      integer::n,nb
      integer,external::ilaenv
      n=size(a,1)
      nb = ilaenv( 1, "zhetrd", "U", n, -1, -1, -1 )
      allocate(work(nb*n))
      allocate(rwork(3*n-2))
      call zheev("V","U",n,a,n,e,work,nb*n,rwork,iflag)
      deallocate(work)
      deallocate(rwork)
   end subroutine eigh_c8
   subroutine eigh_r8(a,e)
      !! call dsyev  "V","U"
      real(r8),intent(inout) :: a(:,:)
      real(r8),intent(out)   :: e(:)
      real(r8),allocatable   :: work(:)
      integer               :: iflag
      integer::n,nb
      integer,external::ilaenv
      n=size(a,1)
      nb = ilaenv( 1, "dsytrd", "U", n, -1, -1, -1 )
      allocate(work(n*nb))
      call dsyev("V","U",n,a,n,e,work,nb*n,iflag)
      deallocate(work)
   end subroutine eigh_r8


   subroutine gemm_c8(a,b,c,transa,transb,alpha,beta)
      !! fortran77 call:
      !! zgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
      !! transa='n','c','t'
      !! transb='n','c','t'
      implicit none
      character(len=1), intent(in):: transa
      character(len=1), intent(in):: transb
      complex(r8), intent(in) :: alpha
      complex(r8), intent(in) :: beta
      complex(r8), intent(in) :: a(:,:)
      complex(r8), intent(in) :: b(:,:)
      complex(r8), intent(inout) :: c(:,:)
      integer :: m,n,k,lda,ldb,ldc
      if((transa=='n'.or.transa=='N')) then
         k = size(a,2)
      else
         k = size(a,1)
      endif
      lda = max(1,size(a,1))
      ldb = max(1,size(b,1))
      ldc = max(1,size(c,1))
      m = size(c,1)
      n = size(c,2)
      ! <<< call blas77 routine >>>
      call zgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
   end subroutine gemm_c8
   subroutine gemm_r8(a,b,c,transa,transb,alpha,beta)
      !! fortran77 call:
      !! zgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
      !! transa='n','c','t'
      !! transb='n','c','t'
      implicit none
      character(len=1), intent(in):: transa
      character(len=1), intent(in):: transb
      real(r8), intent(in) :: beta
      real(r8), intent(in) :: alpha
      real(r8), intent(in) :: a(:,:)
      real(r8), intent(in) :: b(:,:)
      real(r8), intent(inout) :: c(:,:)
      integer :: m,n,k,lda,ldb,ldc
      if((transa=='n'.or.transa=='N')) then
         k = size(a,2)
      else
         k = size(a,1)
      endif
      lda = max(1,size(a,1))
      ldb = max(1,size(b,1))
      ldc = max(1,size(c,1))
      m = size(c,1)
      n = size(c,2)
      ! <<< call blas77 routine >>>
      call dgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
   end subroutine gemm_r8
   
   subroutine geut_c8(itype,a,b)
      !! itypes 1 : B = A^{+} B A
      !! itypes 2 : B = A  B A^{+}
      integer,intent(in)::itype
      complex(r8),intent(in)::a(:,:)
      complex(r8),intent(inout)::b(:,:)
      complex(r8),allocatable::c(:,:)
      allocate(c,mold=a)
      if(itype==1)then
         call linalg%gemm(a, b, c, 'C', 'N', one_c8, zero_c8)
         call linalg%gemm(c, a, b, 'N', 'N', one_c8, zero_c8)
      elseif(itype==2)then
         call linalg%gemm(a, b, c, 'N', 'N', one_c8, zero_c8)
         call linalg%gemm(c, a, b, 'N', 'C', one_c8, zero_c8)
      else
         error stop "[Linalg Error]:Illegal itype, itype must be 1 or 2"
      end if
   end subroutine geut_c8

   subroutine geot_r8(itype,a,b)
      !! itypes 1 : B = A^{+} B A
      !! itypes 2 : B = A  B A^{+}
      integer,intent(in)::itype
      real(r8),intent(in)::a(:,:)
      real(r8),intent(inout)::b(:,:)
      real(r8),allocatable::c(:,:)
      allocate(c,mold=a)
      if(itype==1)then
         call linalg%gemm(a, b, c, 'T', 'N', one_r8, zero_r8)
         call linalg%gemm(c, a, b, 'N', 'N', one_r8, zero_r8)
      elseif(itype==2)then
         call linalg%gemm(a, b, c, 'N', 'N', one_r8, zero_r8)
         call linalg%gemm(c, a, b, 'N', 'T', one_r8, zero_r8)
      else
         error stop "[Linalg Error]:Illegal itype, itype must be 1 or 2"
      end if
   end subroutine geot_r8

   logical function equal_c4(a,b)result(res)
      !! Matrix A == B
      complex(r4),intent(in)::a(:,:)
      complex(r4),intent(in)::b(:,:)
      res=all(abs(a-b)<eps_r4)
   end function equal_c4
   logical function equal_r4(a,b)result(res)
      !! Matrix A == B
      real(r4),intent(in)::a(:,:)
      real(r4),intent(in)::b(:,:)
      res=all(abs(a-b)<eps_r4)
   end function equal_r4

   subroutine print_c4(a,name)
      !! Print Matrix A
      complex(r4),intent(in) :: a(..)
      character(len=*),intent(in)::name
      integer::n,i
      select rank(a)
      rank(2)
      write(*,"(3A)")"matrix ",name,"="
      n=size(a,1)
      do i=1,n
         write(*,"(*(F10.4,'+',F10.4,'i'))")a(i,:)
      end do
      rank(1)
         write(*,"(3A)")"vector ",name,"="
         write(*,"(*(F10.4,'+',F10.4,'i'))")a
      rank(0)
         write(*,"(3A)")"num ",name,"="
         write(*,"(*(F10.4,'+',F10.4,'i'))")a
      end select
   end subroutine print_c4

   subroutine print_r4(a,name)
      !! Print Matrix A
      real(r4),intent(in) :: a(..)
      character(len=*),intent(in)::name
      integer::n,i
      select rank(a)
      rank(2)
         write(*,"(3A)")"matrix ",name,"="
         n=size(a,1)
         do i=1,n
            write(*,"(*(F10.4))")a(i,:)
         end do
      rank(1)
         write(*,"(3A)")"vector ",name,"="
         write(*,"(F10.4)")a
      rank(0)
         write(*,"(3A)")"num ",name,"="
         write(*,"(F10.4)")a
      end select
   end subroutine print_r4

   subroutine eye_c4(a)
      !! Eye Matrix
      complex(r4),intent(inout) :: a(:,:)
      integer::n,i
      n=size(a,1)
      a=zero_c4
      do i=1,n
         a(i,i)=one_c4
      end do
   end subroutine eye_c4
   subroutine eye_r4(a)
      !! Eye Matrix
      real(r4),intent(inout) :: a(:,:)
      integer::n,i
      n=size(a,1)
      a=zero_r4
      do i=1,n
         a(i,i)=one_r4
      end do
   end subroutine eye_r4

   logical function inv_c4(a)result(res)
      !! inverse of matrix a
      complex(r4),intent(inout) :: a(:,:)
      integer                  :: info
      integer,allocatable      :: ipiv(:)
      complex(r4),allocatable   :: work(:)
      integer::n,i
      n=size(a,1)
      allocate(ipiv(n))
      call cgetrf(n,n,a,n,ipiv,info)
      res=(info==0)
      if(.not.res)return
      do i=1,n
         if(abs(a(i,i))<eps_r4)then
            res=.false.
            return
         end if
      end do
      allocate(work(n))
      call cgetri(n,a,n,ipiv,work,n,info)
      res=(info==0)
      deallocate(work)
      deallocate(ipiv)
   end function inv_c4
   logical function inv_r4(a)result(res)
      !! inverse of matrix a
      real(r4),intent(inout) :: a(:,:)
      integer               :: info
      integer,allocatable   :: ipiv(:)
      real(r4),allocatable   :: work(:)
      integer::n,i
      n=size(a,1)
      allocate(ipiv(n))
      call sgetrf(n,n,a,n,ipiv,info)
      res=(info==0)
      if(.not.res)return
      do i=1,n
         if(abs(a(i,i))<eps_r4)then
            res=.false.
            return
         end if
      end do
      allocate(work(n))
      call sgetri(n,a,n,ipiv,work,n,info)
      res=(info==0)
      deallocate(work)
      deallocate(ipiv)
   end function inv_r4

   complex(r4) function det_c4(a)result(res)
      !! Determinant of matrix a
      complex(r4),intent(in)    :: a(:,:)
      integer                  :: info
      integer,allocatable      :: ipiv(:)
      integer::n,i
      complex(r4),allocatable:: tmpa(:,:)
      n=size(a,1)
      allocate(tmpa,source=a)
      allocate(ipiv(n))
      call cgetrf(n,n,tmpa,n,ipiv,info)
      if(info/=0)then
         res=zero_c4
      else
         res=one_c4
         do i=1,n
            res=res*tmpa(i,i)*merge(1,-1,ipiv(i)==i)
         end do
      end if
      deallocate(ipiv)
      deallocate(tmpa)
   end function det_c4
   complex(r4) function det_r4(a)result(res)
      !! Determinant of matrix a
      real(r4),intent(in) :: a(:,:)
      integer               :: info
      integer,allocatable   :: ipiv(:)
      integer::n,i
      real(r4),allocatable::tmpa(:,:)
      n=size(a,1)
      allocate(tmpa,source=a)
      allocate(ipiv(n))
      call sgetrf(n,n,tmpa,n,ipiv,info)
      if(info/=0)then
         res=zero_r4
      else
         res=one_r4
         do i=1,n
            res=res*tmpa(i,i)*merge(1,-1,ipiv(i)==i)
         end do
      end if
      deallocate(ipiv)
      deallocate(tmpa)
   end function det_r4

   subroutine eigh_c4(a,e)
      !! call zheev "V","U"
      complex(r4),intent(inout) :: a(:,:)
      real(r4),intent(out)      :: e(:)
      complex(r4),allocatable   :: work(:)
      real(r4),allocatable      :: rwork(:)
      integer                  :: iflag
      integer::n,nb
      integer,external::ilaenv
      n=size(a,1)
      nb = ilaenv( 1, "chetrd", "U", n, -1, -1, -1 )
      allocate(work(nb*n))
      allocate(rwork(3*n-2))
      call cheev("V","U",n,a,n,e,work,nb*n,rwork,iflag)
      deallocate(work)
      deallocate(rwork)
   end subroutine eigh_c4
   subroutine eigh_r4(a,e)
      !! call dsyev  "V","U"
      real(r4),intent(inout) :: a(:,:)
      real(r4),intent(out)   :: e(:)
      real(r4),allocatable   :: work(:)
      integer               :: iflag
      integer::n,nb
      integer,external::ilaenv
      n=size(a,1)
      nb = ilaenv( 1, "ssytrd", "U", n, -1, -1, -1 )
      allocate(work(nb*n))
      call ssyev("V","U",n,a,n,e,work,nb*n,iflag)
      deallocate(work)
   end subroutine eigh_r4


   subroutine gemm_c4(a,b,c,transa,transb,alpha,beta)
      !! fortran77 call:
      !! cgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
      !! transa='n','c','t'
      !! transb='n','c','t'
      implicit none
      character(len=1), intent(in):: transa
      character(len=1), intent(in):: transb
      complex(r4), intent(in) :: alpha
      complex(r4), intent(in) :: beta
      complex(r4), intent(in) :: a(:,:)
      complex(r4), intent(in) :: b(:,:)
      complex(r4), intent(inout) :: c(:,:)
      integer :: m,n,k,lda,ldb,ldc
      if((transa=='n'.or.transa=='N')) then
         k = size(a,2)
      else
         k = size(a,1)
      endif
      lda = max(1,size(a,1))
      ldb = max(1,size(b,1))
      ldc = max(1,size(c,1))
      m = size(c,1)
      n = size(c,2)
      ! <<< call blas77 routine >>>
      call cgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
   end subroutine gemm_c4
   subroutine gemm_r4(a,b,c,transa,transb,alpha,beta)
      !! fortran77 call:
      !! sgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
      !! transa='n','c','t'
      !! transb='n','c','t'
      implicit none
      character(len=1), intent(in):: transa
      character(len=1), intent(in):: transb
      real(r4), intent(in) :: beta
      real(r4), intent(in) :: alpha
      real(r4), intent(in) :: a(:,:)
      real(r4), intent(in) :: b(:,:)
      real(r4), intent(inout) :: c(:,:)
      integer :: m,n,k,lda,ldb,ldc
      if((transa=='n'.or.transa=='N')) then
         k = size(a,2)
      else
         k = size(a,1)
      endif
      lda = max(1,size(a,1))
      ldb = max(1,size(b,1))
      ldc = max(1,size(c,1))
      m = size(c,1)
      n = size(c,2)
      ! <<< call blas77 routine >>>
      call sgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
   end subroutine gemm_r4
   
   subroutine geut_c4(itype,a,b)
      !! itypes 1 : B = A^{+} B A
      !! itypes 2 : B = A  B A^{+}
      integer,intent(in)::itype
      complex(r4),intent(in)::a(:,:)
      complex(r4),intent(inout)::b(:,:)
      complex(r4),allocatable::c(:,:)
      allocate(c,mold=a)
      if(itype==1)then
         call linalg%gemm(a, b, c, 'C', 'N', one_c4, zero_c4)
         call linalg%gemm(c, a, b, 'N', 'N', one_c4, zero_c4)
      elseif(itype==2)then
         call linalg%gemm(a, b, c, 'N', 'N', one_c4, zero_c4)
         call linalg%gemm(c, a, b, 'N', 'C', one_c4, zero_c4)
      else
         error stop "[Linalg Error]:Illegal itype, itype must be 1 or 2"
      end if
   end subroutine geut_c4

   subroutine geot_r4(itype,a,b)
      !! itypes 1 : B = A^{+} B A
      !! itypes 2 : B = A  B A^{+}
      integer,intent(in)::itype
      real(r4),intent(in)::a(:,:)
      real(r4),intent(inout)::b(:,:)
      real(r4),allocatable::c(:,:)
      allocate(c,mold=a)
      if(itype==1)then
         call linalg%gemm(a, b, c, 'T', 'N', one_r4, zero_r4)
         call linalg%gemm(c, a, b, 'N', 'N', one_r4, zero_r4)
      elseif(itype==2)then
         call linalg%gemm(a, b, c, 'N', 'N', one_r4, zero_r4)
         call linalg%gemm(c, a, b, 'N', 'T', one_r4, zero_r4)
      else
         error stop "[Linalg Error]:Illegal itype, itype must be 1 or 2"
      end if
   end subroutine geot_r4

end module linalg_fortran
