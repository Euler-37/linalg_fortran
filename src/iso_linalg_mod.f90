module iso_linalg_mod
   implicit none
   private
   type::linalg_t
   contains
      generic::inv   => linalg_zinv  ,linalg_dinv
      generic::det   => linalg_zdet  ,linalg_ddet
      generic::eigh  => linalg_zeigh ,linalg_deigh
      generic::gemm  => linalg_zgemm ,linalg_dgemm
      generic::eye   => linalg_zeye  ,linalg_deye
      generic::print => linalg_zprint,linalg_dprint
      generic::geut  => linalg_zgeut ,linalg_dgeot
      generic::equal => linalg_zequal,linalg_dequal
      procedure,private,nopass::linalg_zinv
      procedure,private,nopass::linalg_dinv
      procedure,private,nopass::linalg_zdet
      procedure,private,nopass::linalg_ddet
      procedure,private,nopass::linalg_zeigh
      procedure,private,nopass::linalg_deigh
      procedure,private,nopass::linalg_zgemm
      procedure,private,nopass::linalg_dgemm
      procedure,private,nopass::linalg_zeye
      procedure,private,nopass::linalg_deye
      procedure,private,nopass::linalg_zprint
      procedure,private,nopass::linalg_dprint
      procedure,private,nopass::linalg_zgeut
      procedure,private,nopass::linalg_dgeot
      procedure,private,nopass::linalg_zequal
      procedure,private,nopass::linalg_dequal
   end type linalg_t
   type(linalg_t),public::linalg
   complex(8),parameter::cone =cmplx(1.d0,0.d0,8)
   complex(8),parameter::czero=cmplx(0.d0,0.d0,8)
   real(8)   ,parameter::eps=1.0d-12
contains
   logical function linalg_zequal(a,b)result(res)
      !! Matrix A == B
      complex(8),intent(in)::a(:,:)
      complex(8),intent(in)::b(:,:)
      res=all(abs(a-b)<eps)
   end function linalg_zequal
   logical function linalg_dequal(a,b)result(res)
      !! Matrix A == B
      real(8),intent(in)::a(:,:)
      real(8),intent(in)::b(:,:)
      res=all(abs(a-b)<eps)
   end function linalg_dequal

   subroutine linalg_zprint(a,name)
      !! Print Matrix A
      complex(8),intent(in) :: a(:,:)
      character(len=*),intent(in)::name
      integer::n,i
      write(*,"(3A)")"matrix ",name,"="
      n=size(a,1)
      do i=1,n
         write(*,"(*(F10.4,'+',F10.4,'i'))")a(i,:)
      end do
   end subroutine linalg_zprint

   subroutine linalg_dprint(a,name)
      !! Print Matrix A
      real(8),intent(in) :: a(:,:)
      character(len=*),intent(in)::name
      integer::n,i
      write(*,"(3A)")"matrix ",name,"="
      n=size(a,1)
      do i=1,n
         write(*,"(*(F10.4))")a(i,:)
      end do
   end subroutine linalg_dprint

   subroutine linalg_zeye(a)
      !! Eye Matrix
      complex(8),intent(inout) :: a(:,:)
      integer::n,i
      n=size(a,1)
      do i=1,n
         a(i,i)=1.d0
      end do
   end subroutine linalg_zeye
   subroutine linalg_deye(a)
      !! Eye Matrix
      real(8),intent(inout) :: a(:,:)
      integer::n,i
      n=size(a,1)
      do i=1,n
         a(i,i)=1.d0
      end do
   end subroutine linalg_deye

   logical function linalg_zinv(a)result(res)
      !! inverse of matrix a
      complex(8),intent(inout) :: a(:,:)
      integer                  :: info
      integer,allocatable      :: ipiv(:)
      complex(8),allocatable   :: work(:)
      integer::n,i
      n=size(a,1)
      allocate(ipiv(n))
      call zgetrf(n,n,a,n,ipiv,info)
      res=(info==0)
      if(.not.res)return
      do i=1,n
         if(abs(a(i,i))<eps)then
            res=.false.
            return
         end if
      end do
      allocate(work(n))
      call zgetri(n,a,n,ipiv,work,n,info)
      res=(info==0)
      deallocate(work)
      deallocate(ipiv)
   end function linalg_zinv
   logical function linalg_dinv(a)result(res)
      !! inverse of matrix a
      real(8),intent(inout) :: a(:,:)
      integer               :: info
      integer,allocatable   :: ipiv(:)
      real(8),allocatable   :: work(:)
      integer::n,i
      n=size(a,1)
      allocate(ipiv(n))
      call dgetrf(n,n,a,n,ipiv,info)
      res=(info==0)
      if(.not.res)return
      do i=1,n
         if(abs(a(i,i))<eps)then
            res=.false.
            return
         end if
      end do
      allocate(work(n))
      call dgetri(n,a,n,ipiv,work,n,info)
      res=(info==0)
      deallocate(work)
      deallocate(ipiv)
   end function linalg_dinv

   complex(8) function linalg_zdet(a)result(res)
      !! Determinant of matrix a
      complex(8),intent(in)    :: a(:,:)
      integer                  :: info
      integer,allocatable      :: ipiv(:)
      integer::n,i
      complex(8),allocatable:: tmpa(:,:)
      n=size(a,1)
      allocate(tmpa,source=a)
      allocate(ipiv(n))
      call zgetrf(n,n,tmpa,n,ipiv,info)
      if(info/=0)then
         res=0.d0
      else
         res=1.d0
         do i=1,n
            res=res*tmpa(i,i)*merge(1,-1,ipiv(i)==i)
         end do
      end if
      deallocate(ipiv)
      deallocate(tmpa)
   end function linalg_zdet
   complex(8) function linalg_ddet(a)result(res)
      !! Determinant of matrix a
      real(8),intent(in) :: a(:,:)
      integer               :: info
      integer,allocatable   :: ipiv(:)
      integer::n,i
      real(8),allocatable::tmpa(:,:)
      n=size(a,1)
      allocate(tmpa,source=a)
      allocate(ipiv(n))
      call dgetrf(n,n,tmpa,n,ipiv,info)
      if(info/=0)then
         res=0.d0
      else
         res=1.d0
         do i=1,n
            res=res*tmpa(i,i)*merge(1,-1,ipiv(i)==i)
         end do
      end if
      deallocate(ipiv)
      deallocate(tmpa)
   end function linalg_ddet

   subroutine linalg_zeigh(a,e)
      !! call zheev "V","U"
      complex(8),intent(inout) :: a(:,:)
      real(8),intent(out)      :: e(:)
      complex(8),allocatable   :: work(:)
      real(8),allocatable      :: rwork(:)
      integer                  :: iflag
      integer::n
      n=size(a,1)
      allocate(work(5*n*n))
      allocate(rwork(3*n-2))
      call zheev("V","U",n,a,n,e,work,5*n*n,rwork,iflag)
      deallocate(work)
      deallocate(rwork)
   end subroutine linalg_zeigh
   subroutine linalg_deigh(a,e)
      !! call dsyev  "V","U"
      real(8),intent(inout) :: a(:,:)
      real(8),intent(out)   :: e(:)
      real(8),allocatable   :: work(:)
      integer               :: iflag
      integer::n
      n=size(a,1)
      allocate(work(5*n*n))
      call dsyev("V","U",n,a,n,e,work,5*n*n,iflag)
      deallocate(work)
   end subroutine linalg_deigh


   subroutine linalg_zgemm(a,b,c,transa,transb,alpha,beta)
      !! fortran77 call:
      !! zgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
      !! transa='n','c','t'
      !! transb='n','c','t'
      implicit none
      character(len=1), intent(in):: transa
      character(len=1), intent(in):: transb
      complex(8), intent(in) :: alpha
      complex(8), intent(in) :: beta
      complex(8), intent(in) :: a(:,:)
      complex(8), intent(in) :: b(:,:)
      complex(8), intent(inout) :: c(:,:)
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
   end subroutine linalg_zgemm
   subroutine linalg_dgemm(a,b,c,transa,transb,alpha,beta)
      !! fortran77 call:
      !! zgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
      !! transa='n','c','t'
      !! transb='n','c','t'
      implicit none
      character(len=1), intent(in):: transa
      character(len=1), intent(in):: transb
      real(8), intent(in) :: beta
      real(8), intent(in) :: alpha
      real(8), intent(in) :: a(:,:)
      real(8), intent(in) :: b(:,:)
      real(8), intent(inout) :: c(:,:)
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
   end subroutine linalg_dgemm
   
   subroutine linalg_zgeut(itype,a,b)
      !! itypes 1 : B = A^{+} B A
      !! itypes 2 : B = A  B A^{+}
      integer,intent(in)::itype
      complex(8),intent(in)::a(:,:)
      complex(8),intent(inout)::b(:,:)
      complex(8),allocatable::c(:,:)
      allocate(c,mold=a)
      if(itype==1)then
         call linalg%gemm(a, b, c, 'C', 'N', cone, czero)
         call linalg%gemm(c, a, b, 'N', 'N', cone, czero)
      elseif(itype==2)then
         call linalg%gemm(a, b, c, 'N', 'N', cone, czero)
         call linalg%gemm(c, a, b, 'N', 'C', cone, czero)
      else
         error stop "[Linalg Error]:Illegal itype, itype must be 1 or 2"
      end if
   end subroutine linalg_zgeut

   subroutine linalg_dgeot(itype,a,b)
      !! itypes 1 : B = A^{+} B A
      !! itypes 2 : B = A  B A^{+}
      integer,intent(in)::itype
      real(8),intent(in)::a(:,:)
      real(8),intent(inout)::b(:,:)
      real(8),allocatable::c(:,:)
      allocate(c,mold=a)
      if(itype==1)then
         call linalg%gemm(a, b, c, 'T', 'N', 1.0d0, 0.0d0)
         call linalg%gemm(c, a, b, 'N', 'N', 1.0d0, 0.0d0)
      elseif(itype==2)then
         call linalg%gemm(a, b, c, 'N', 'N', 1.0d0, 0.0d0)
         call linalg%gemm(c, a, b, 'N', 'T', 1.0d0, 0.0d0)
      else
         error stop "[Linalg Error]:Illegal itype, itype must be 1 or 2"
      end if
   end subroutine linalg_dgeot

end module iso_linalg_mod
