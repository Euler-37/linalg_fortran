module linalg_fortran
   use iso_fortran_env,only:r8=>real64,r4=>real32
   implicit none
   private
   type::linalg_t
   contains
      generic::print=>print_r4,print_r8,print_c4,print_c8,&
                     &print_vec_r4,print_vec_r8,print_vec_c4,print_vec_c8,&
                     &print_num_r4,print_num_r8,print_num_c4,print_num_c8
      generic::equal=>equal_r4,equal_r8,equal_c4,equal_c8
      generic::eye=>eye_r4,eye_r8,eye_c4,eye_c8
      generic::inv=>inv_r4,inv_r8,inv_c4,inv_c8
      generic::det=>det_r4,det_r8,det_c4,det_c8
      generic::gemm=>gemm_r4,gemm_r8,gemm_c4,gemm_c8
      generic::gemv=>gemv_r4,gemv_r8,gemv_c4,gemv_c8
      generic::geut=>geut_r4,geut_r8,geut_c4,geut_c8
      generic::eigh=>eigh_r4,eigh_r8,eigh_c4,eigh_c8
      procedure,private,nopass::print_r4
      procedure,private,nopass::print_vec_r4
      procedure,private,nopass::print_num_r4
      procedure,private,nopass::equal_r4
      procedure,private,nopass::eye_r4
      procedure,private,nopass::inv_r4
      procedure,private,nopass::det_r4
      procedure,private,nopass::gemm_r4
      procedure,private,nopass::gemv_r4
      procedure,private,nopass::geut_r4
      procedure,private,nopass::eigh_r4
      procedure,private,nopass::print_r8
      procedure,private,nopass::print_vec_r8
      procedure,private,nopass::print_num_r8
      procedure,private,nopass::equal_r8
      procedure,private,nopass::eye_r8
      procedure,private,nopass::inv_r8
      procedure,private,nopass::det_r8
      procedure,private,nopass::gemm_r8
      procedure,private,nopass::gemv_r8
      procedure,private,nopass::geut_r8
      procedure,private,nopass::eigh_r8
      procedure,private,nopass::print_c4
      procedure,private,nopass::print_vec_c4
      procedure,private,nopass::print_num_c4
      procedure,private,nopass::equal_c4
      procedure,private,nopass::eye_c4
      procedure,private,nopass::inv_c4
      procedure,private,nopass::det_c4
      procedure,private,nopass::gemm_c4
      procedure,private,nopass::gemv_c4
      procedure,private,nopass::geut_c4
      procedure,private,nopass::eigh_c4
      procedure,private,nopass::print_c8
      procedure,private,nopass::print_vec_c8
      procedure,private,nopass::print_num_c8
      procedure,private,nopass::equal_c8
      procedure,private,nopass::eye_c8
      procedure,private,nopass::inv_c8
      procedure,private,nopass::det_c8
      procedure,private,nopass::gemm_c8
      procedure,private,nopass::gemv_c8
      procedure,private,nopass::geut_c8
      procedure,private,nopass::eigh_c8
   end type linalg_t
   character(len=30)::format_c8="(*(F10.4,'+',F10.4,'i',:,','))"
   character(len=30)::format_c4="(*(F10.4,'+',F10.4,'i',:,','))"
   character(len=30)::format_r8="(*(F10.4,:,','))"
   character(len=30)::format_r4="(*(F10.4,:,','))"
   type(linalg_t),public::linalg
   complex(r8),parameter::one_c8 =cmplx(1.0_r8,0.0_r8,r8)
   complex(r4),parameter::one_c4 =cmplx(1.0_r4,0.0_r4,r4)
   real(r8)   ,parameter::one_r8 =1.0_r8
   real(r4)   ,parameter::one_r4 =1.0_r4
   complex(r8),parameter::zero_c8=cmplx(0.0_r8,0.0_r8,r8)
   complex(r4),parameter::zero_c4=cmplx(0.0_r4,0.0_r4,r4)
   real(r8)   ,parameter::zero_r8=0.0_r8
   real(r4)   ,parameter::zero_r4=0.0_r4
   real(r8)   ,parameter::eps_r8 =1.0e-12_r8
   real(r4)   ,parameter::eps_r4 =1.0e-6_r4
   real(r8)   ,parameter::eps_c8 =1.0e-12_r8
   real(r4)   ,parameter::eps_c4 =1.0e-6_r4
   interface optval
      module procedure  optval_r4
      module procedure  optval_r8
      module procedure  optval_c4
      module procedure  optval_c8
      module procedure  optval_char
      module procedure  optval_int
   end interface
contains
   real(r4) function optval_r4(a,default)result(res)
      real(r4),intent(in),optional::a
      real(r4),intent(in)::default
      if(present(a))then
         res=a
      else
         res=default
      end if
   end function optval_r4

   real(r8) function optval_r8(a,default)result(res)
      real(r8),intent(in),optional::a
      real(r8),intent(in)::default
      if(present(a))then
         res=a
      else
         res=default
      end if
   end function optval_r8

   complex(r4) function optval_c4(a,default)result(res)
      complex(r4),intent(in),optional::a
      complex(r4),intent(in)::default
      if(present(a))then
         res=a
      else
         res=default
      end if
   end function optval_c4

   complex(r8) function optval_c8(a,default)result(res)
      complex(r8),intent(in),optional::a
      complex(r8),intent(in)::default
      if(present(a))then
         res=a
      else
         res=default
      end if
   end function optval_c8

   character function optval_char(a,default)result(res)
      character,intent(in),optional::a
      character,intent(in)::default
      if(present(a))then
         res=a
      else
         res=default
      end if
   end function optval_char

   integer function optval_int(a,default)result(res)
      integer,intent(in),optional::a
      integer,intent(in)::default
      if(present(a))then
         res=a
      else
         res=default
      end if
   end function optval_int
   
   subroutine print_r4(a,name)
      !> Print vector/Matrix 
      real(r4),intent(in)::a(:,:)
      character(len=*),intent(in)::name
      integer::n,i
      write(*,"(3A)")"matrix ",name,"="
      n=size(a,1)
      do i=1,n
         write(*,format_r4)a(i,:)
      end do
   end subroutine print_r4

   subroutine print_vec_r4(a,name)
      !> Print vector/Matrix 
      real(r4),intent(in)::a(:)
      character(len=*),intent(in)::name
      write(*,"(3A)")"vector ",name,"="
      write(*,format_r4)a
   end subroutine print_vec_r4

   subroutine print_num_r4(a,name)
      !> Print vector/Matrix 
      real(r4),intent(in)::a
      character(len=*),intent(in)::name
      write(*,"(3A)")"num ",name,"="
      write(*,format_r4)a
   end subroutine print_num_r4

   logical function equal_r4(a,b)result(res)
      !> If Matrix A = B
      real(r4),intent(in)::a(:,:)
      real(r4),intent(in)::b(:,:)
      res=all(abs(a-b)<eps_r4)
   end function equal_r4

   subroutine eye_r4(a)
      !> Eye Matrix
      real(r4),intent(inout) :: a(:,:)
      integer::n,i
      n=size(a,1)
      a=zero_r4
      do i=1,n
         a(i,i)=one_r4
      end do
   end subroutine eye_r4

   logical function inv_r4(a)result(res)
      !! inverse of matrix a
      real(r4),intent(inout) :: a(:,:)
      integer                  :: info
      integer,allocatable      :: ipiv(:)
      real(r4),allocatable       :: work(:)
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

   real(r4) function det_r4(a)result(res)
      !! Determinant of matrix a
      real(r4),intent(in)    :: a(:,:)
      integer                  :: info
      integer,allocatable      :: ipiv(:)
      integer::n,i
      real(r4),allocatable:: tmpa(:,:)
      n=size(a,1)
      select case(n)
      case(2)
         res=a(1,1)*a(2,2)-a(1,2)*a(2,1)
      case(3)
         res=a(1,1)*(a(2,2)*a(3,3)-a(2,3)*a(3,2))+&
            &a(1,2)*(a(2,3)*a(3,1)-a(2,1)*a(3,3))+&
            &a(1,3)*(a(2,1)*a(3,2)-a(2,2)*a(3,1))
      case(4)
         block
            real(r4)::b(6)
            b(1)=a(3,1)*a(4,2)-a(3,2)*a(4,1)
            b(2)=a(3,1)*a(4,3)-a(3,3)*a(4,1)
            b(3)=a(3,1)*a(4,4)-a(3,4)*a(4,1)
            b(4)=a(3,2)*a(4,3)-a(3,3)*a(4,2)
            b(5)=a(3,2)*a(4,4)-a(3,4)*a(4,2)
            b(6)=a(3,3)*a(4,4)-a(3,4)*a(4,3)
            res= a(1,1)*( a(2,2)*b(6)-a(2,3)*b(5)+a(2,4)*b(4))+&
               &a(1,2)*(-a(2,1)*b(6)+a(2,3)*b(3)-a(2,4)*b(2))+&
               &a(1,3)*( a(2,1)*b(5)-a(2,2)*b(3)+a(2,4)*b(1))+&
               &a(1,4)*(-a(2,1)*b(4)+a(2,2)*b(2)-a(2,3)*b(1))
         end block
      case default
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
      end select
   end function det_r4

   subroutine gemm_r4(a,b,c,transa,transb,alpha,beta)
      !! fortran77 call:
      !! zgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
      !! transa='n','c','t'
      !! transb='n','c','t'
      implicit none
      character(len=1), intent(in),optional:: transa
      character(len=1), intent(in),optional:: transb
      real(r4), intent(in),optional:: alpha
      real(r4), intent(in),optional:: beta
      real(r4), intent(in) :: a(:,:)
      real(r4), intent(in) :: b(:,:)
      real(r4), intent(inout) :: c(:,:)
      integer :: m,n,k,lda,ldb,ldc
      character(len=1):: transa_
      character(len=1):: transb_
      real(r4):: alpha_
      real(r4):: beta_
      transa_=optval(transa,"N")
      transb_=optval(transb,"N")
      alpha_ =optval(alpha,one_r4)
      beta_  =optval(beta ,zero_r4)
      if(transa_=='n'.or.transa_=='N') then
         k = size(a,2)
      else
         k = size(a,1)
      endif
      lda = size(a,1)
      ldb = size(b,1)
      ldc = size(c,1)
      m   = size(c,1)
      n   = size(c,2)
      ! <<< call blas77 routine >>>
      call sgemm(transa_,transb_,m,n,k,alpha_,a,lda,b,ldb,beta_,c,ldc)
   end subroutine gemm_r4

   subroutine gemv_r4(a,x,y,trans,alpha,beta)
      ! fortran77 call:
      ! zgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
      ! trans='n','c','t'; default: 'n'
      ! default alpha=1
      ! default beta=0
      ! <<< scalar arguments >>>
      real(r4), intent(in),optional:: alpha
      real(r4), intent(in),optional:: beta
      character(len=1), intent(in),optional:: trans
      ! <<< array arguments >>>
      real(r4), intent(in) :: a(:,:)
      real(r4), intent(in) :: x(:)
      real(r4), intent(inout) :: y(:)
      integer :: incx
      integer :: incy
      integer :: m
      integer :: n
      integer :: lda
      character(len=1):: trans_
      real(r4):: alpha_
      real(r4):: beta_
      ! <<< intrinsic functions >>>
      trans_ =optval(trans,"N")
      alpha_ =optval(alpha,one_r4)
      beta_  =optval(beta ,zero_r4)

      incx = 1
      incy = 1
      lda = size(a,1)
      m = size(a,1)
      n = size(a,2)
      call sgemv(trans_,m,n,alpha_,a,lda,x,incx,beta_,y,incy)
   end subroutine gemv_r4

   subroutine geut_r4(a,b,itype)
      !! itypes 1 : B = A^{+} B A
      !! itypes 2 : B = A  B A^{+}
      integer,intent(in),optional::itype
      real(r4),intent(in)::a(:,:)
      real(r4),intent(inout)::b(:,:)
      real(r4),allocatable::c(:,:)
      integer::itype_
      allocate(c,mold=a)
      itype_=optval(itype,1)
      if(itype_==1)then
         call linalg%gemm(a, b, c, transa='C')
         call linalg%gemm(c, a, b)
      elseif(itype_==2)then
         call linalg%gemm(a, b, c)
         call linalg%gemm(c, a, b, transb="C")
      else
         error stop "[Linalg Error]:Illegal itype, itype must be 1 or 2"
      end if
   end subroutine geut_r4

   subroutine eigh_r4(a,e,uplo)
      !! call zheev "V","U"
      character,intent(in),optional::uplo
      real(r4),intent(inout) :: a(:,:)
      real(r4),allocatable   :: work(:)
      real(r4),intent(out)      :: e(:)
      integer                  :: iflag
      integer::n,nb
      integer,external::ilaenv
      character::uplo_
      n=size(a,1)
      uplo_=optval(uplo,"U")
      nb = ilaenv( 1, "ssytrd", uplo_, n, -1, -1, -1 )
      allocate(work(nb*n))
      call ssyev("V",uplo_,n,a,n,e,work,nb*n,iflag)
      deallocate(work)
   end subroutine eigh_r4

   subroutine print_r8(a,name)
      !> Print vector/Matrix 
      real(r8),intent(in)::a(:,:)
      character(len=*),intent(in)::name
      integer::n,i
      write(*,"(3A)")"matrix ",name,"="
      n=size(a,1)
      do i=1,n
         write(*,format_r8)a(i,:)
      end do
   end subroutine print_r8

   subroutine print_vec_r8(a,name)
      !> Print vector/Matrix 
      real(r8),intent(in)::a(:)
      character(len=*),intent(in)::name
      write(*,"(3A)")"vector ",name,"="
      write(*,format_r8)a
   end subroutine print_vec_r8

   subroutine print_num_r8(a,name)
      !> Print vector/Matrix 
      real(r8),intent(in)::a
      character(len=*),intent(in)::name
      write(*,"(3A)")"num ",name,"="
      write(*,format_r8)a
   end subroutine print_num_r8

   logical function equal_r8(a,b)result(res)
      !> If Matrix A = B
      real(r8),intent(in)::a(:,:)
      real(r8),intent(in)::b(:,:)
      res=all(abs(a-b)<eps_r8)
   end function equal_r8

   subroutine eye_r8(a)
      !> Eye Matrix
      real(r8),intent(inout) :: a(:,:)
      integer::n,i
      n=size(a,1)
      a=zero_r8
      do i=1,n
         a(i,i)=one_r8
      end do
   end subroutine eye_r8

   logical function inv_r8(a)result(res)
      !! inverse of matrix a
      real(r8),intent(inout) :: a(:,:)
      integer                  :: info
      integer,allocatable      :: ipiv(:)
      real(r8),allocatable       :: work(:)
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

   real(r8) function det_r8(a)result(res)
      !! Determinant of matrix a
      real(r8),intent(in)    :: a(:,:)
      integer                  :: info
      integer,allocatable      :: ipiv(:)
      integer::n,i
      real(r8),allocatable:: tmpa(:,:)
      n=size(a,1)
      select case(n)
      case(2)
         res=a(1,1)*a(2,2)-a(1,2)*a(2,1)
      case(3)
         res=a(1,1)*(a(2,2)*a(3,3)-a(2,3)*a(3,2))+&
            &a(1,2)*(a(2,3)*a(3,1)-a(2,1)*a(3,3))+&
            &a(1,3)*(a(2,1)*a(3,2)-a(2,2)*a(3,1))
      case(4)
         block
            real(r8)::b(6)
            b(1)=a(3,1)*a(4,2)-a(3,2)*a(4,1)
            b(2)=a(3,1)*a(4,3)-a(3,3)*a(4,1)
            b(3)=a(3,1)*a(4,4)-a(3,4)*a(4,1)
            b(4)=a(3,2)*a(4,3)-a(3,3)*a(4,2)
            b(5)=a(3,2)*a(4,4)-a(3,4)*a(4,2)
            b(6)=a(3,3)*a(4,4)-a(3,4)*a(4,3)
            res= a(1,1)*( a(2,2)*b(6)-a(2,3)*b(5)+a(2,4)*b(4))+&
               &a(1,2)*(-a(2,1)*b(6)+a(2,3)*b(3)-a(2,4)*b(2))+&
               &a(1,3)*( a(2,1)*b(5)-a(2,2)*b(3)+a(2,4)*b(1))+&
               &a(1,4)*(-a(2,1)*b(4)+a(2,2)*b(2)-a(2,3)*b(1))
         end block
      case default
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
      end select
   end function det_r8

   subroutine gemm_r8(a,b,c,transa,transb,alpha,beta)
      !! fortran77 call:
      !! zgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
      !! transa='n','c','t'
      !! transb='n','c','t'
      implicit none
      character(len=1), intent(in),optional:: transa
      character(len=1), intent(in),optional:: transb
      real(r8), intent(in),optional:: alpha
      real(r8), intent(in),optional:: beta
      real(r8), intent(in) :: a(:,:)
      real(r8), intent(in) :: b(:,:)
      real(r8), intent(inout) :: c(:,:)
      integer :: m,n,k,lda,ldb,ldc
      character(len=1):: transa_
      character(len=1):: transb_
      real(r8):: alpha_
      real(r8):: beta_
      transa_=optval(transa,"N")
      transb_=optval(transb,"N")
      alpha_ =optval(alpha,one_r8)
      beta_  =optval(beta ,zero_r8)
      if(transa_=='n'.or.transa_=='N') then
         k = size(a,2)
      else
         k = size(a,1)
      endif
      lda = size(a,1)
      ldb = size(b,1)
      ldc = size(c,1)
      m   = size(c,1)
      n   = size(c,2)
      ! <<< call blas77 routine >>>
      call dgemm(transa_,transb_,m,n,k,alpha_,a,lda,b,ldb,beta_,c,ldc)
   end subroutine gemm_r8

   subroutine gemv_r8(a,x,y,trans,alpha,beta)
      ! fortran77 call:
      ! zgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
      ! trans='n','c','t'; default: 'n'
      ! default alpha=1
      ! default beta=0
      ! <<< scalar arguments >>>
      real(r8), intent(in),optional:: alpha
      real(r8), intent(in),optional:: beta
      character(len=1), intent(in),optional:: trans
      ! <<< array arguments >>>
      real(r8), intent(in) :: a(:,:)
      real(r8), intent(in) :: x(:)
      real(r8), intent(inout) :: y(:)
      integer :: incx
      integer :: incy
      integer :: m
      integer :: n
      integer :: lda
      character(len=1):: trans_
      real(r8):: alpha_
      real(r8):: beta_
      ! <<< intrinsic functions >>>
      trans_ =optval(trans,"N")
      alpha_ =optval(alpha,one_r8)
      beta_  =optval(beta ,zero_r8)

      incx = 1
      incy = 1
      lda = size(a,1)
      m = size(a,1)
      n = size(a,2)
      call dgemv(trans_,m,n,alpha_,a,lda,x,incx,beta_,y,incy)
   end subroutine gemv_r8

   subroutine geut_r8(a,b,itype)
      !! itypes 1 : B = A^{+} B A
      !! itypes 2 : B = A  B A^{+}
      integer,intent(in),optional::itype
      real(r8),intent(in)::a(:,:)
      real(r8),intent(inout)::b(:,:)
      real(r8),allocatable::c(:,:)
      integer::itype_
      allocate(c,mold=a)
      itype_=optval(itype,1)
      if(itype_==1)then
         call linalg%gemm(a, b, c, transa='C')
         call linalg%gemm(c, a, b)
      elseif(itype_==2)then
         call linalg%gemm(a, b, c)
         call linalg%gemm(c, a, b, transb="C")
      else
         error stop "[Linalg Error]:Illegal itype, itype must be 1 or 2"
      end if
   end subroutine geut_r8

   subroutine eigh_r8(a,e,uplo)
      !! call zheev "V","U"
      character,intent(in),optional::uplo
      real(r8),intent(inout) :: a(:,:)
      real(r8),allocatable   :: work(:)
      real(r8),intent(out)      :: e(:)
      integer                  :: iflag
      integer::n,nb
      integer,external::ilaenv
      character::uplo_
      n=size(a,1)
      uplo_=optval(uplo,"U")
      nb = ilaenv( 1, "dsytrd", uplo_, n, -1, -1, -1 )
      allocate(work(nb*n))
      call dsyev("V",uplo_,n,a,n,e,work,nb*n,iflag)
      deallocate(work)
   end subroutine eigh_r8

   subroutine print_c4(a,name)
      !> Print vector/Matrix 
      complex(r4),intent(in)::a(:,:)
      character(len=*),intent(in)::name
      integer::n,i
      write(*,"(3A)")"matrix ",name,"="
      n=size(a,1)
      do i=1,n
         write(*,format_c4)a(i,:)
      end do
   end subroutine print_c4

   subroutine print_vec_c4(a,name)
      !> Print vector/Matrix 
      complex(r4),intent(in)::a(:)
      character(len=*),intent(in)::name
      write(*,"(3A)")"vector ",name,"="
      write(*,format_c4)a
   end subroutine print_vec_c4

   subroutine print_num_c4(a,name)
      !> Print vector/Matrix 
      complex(r4),intent(in)::a
      character(len=*),intent(in)::name
      write(*,"(3A)")"num ",name,"="
      write(*,format_c4)a
   end subroutine print_num_c4

   logical function equal_c4(a,b)result(res)
      !> If Matrix A = B
      complex(r4),intent(in)::a(:,:)
      complex(r4),intent(in)::b(:,:)
      res=all(abs(a-b)<eps_c4)
   end function equal_c4

   subroutine eye_c4(a)
      !> Eye Matrix
      complex(r4),intent(inout) :: a(:,:)
      integer::n,i
      n=size(a,1)
      a=zero_c4
      do i=1,n
         a(i,i)=one_c4
      end do
   end subroutine eye_c4

   logical function inv_c4(a)result(res)
      !! inverse of matrix a
      complex(r4),intent(inout) :: a(:,:)
      integer                  :: info
      integer,allocatable      :: ipiv(:)
      complex(r4),allocatable       :: work(:)
      integer::n,i
      n=size(a,1)
      allocate(ipiv(n))
      call cgetrf(n,n,a,n,ipiv,info)
      res=(info==0)
      if(.not.res)return
      do i=1,n
         if(abs(a(i,i))<eps_c4)then
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

   complex(r4) function det_c4(a)result(res)
      !! Determinant of matrix a
      complex(r4),intent(in)    :: a(:,:)
      integer                  :: info
      integer,allocatable      :: ipiv(:)
      integer::n,i
      complex(r4),allocatable:: tmpa(:,:)
      n=size(a,1)
      select case(n)
      case(2)
         res=a(1,1)*a(2,2)-a(1,2)*a(2,1)
      case(3)
         res=a(1,1)*(a(2,2)*a(3,3)-a(2,3)*a(3,2))+&
            &a(1,2)*(a(2,3)*a(3,1)-a(2,1)*a(3,3))+&
            &a(1,3)*(a(2,1)*a(3,2)-a(2,2)*a(3,1))
      case(4)
         block
            complex(r4)::b(6)
            b(1)=a(3,1)*a(4,2)-a(3,2)*a(4,1)
            b(2)=a(3,1)*a(4,3)-a(3,3)*a(4,1)
            b(3)=a(3,1)*a(4,4)-a(3,4)*a(4,1)
            b(4)=a(3,2)*a(4,3)-a(3,3)*a(4,2)
            b(5)=a(3,2)*a(4,4)-a(3,4)*a(4,2)
            b(6)=a(3,3)*a(4,4)-a(3,4)*a(4,3)
            res= a(1,1)*( a(2,2)*b(6)-a(2,3)*b(5)+a(2,4)*b(4))+&
               &a(1,2)*(-a(2,1)*b(6)+a(2,3)*b(3)-a(2,4)*b(2))+&
               &a(1,3)*( a(2,1)*b(5)-a(2,2)*b(3)+a(2,4)*b(1))+&
               &a(1,4)*(-a(2,1)*b(4)+a(2,2)*b(2)-a(2,3)*b(1))
         end block
      case default
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
      end select
   end function det_c4

   subroutine gemm_c4(a,b,c,transa,transb,alpha,beta)
      !! fortran77 call:
      !! zgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
      !! transa='n','c','t'
      !! transb='n','c','t'
      implicit none
      character(len=1), intent(in),optional:: transa
      character(len=1), intent(in),optional:: transb
      complex(r4), intent(in),optional:: alpha
      complex(r4), intent(in),optional:: beta
      complex(r4), intent(in) :: a(:,:)
      complex(r4), intent(in) :: b(:,:)
      complex(r4), intent(inout) :: c(:,:)
      integer :: m,n,k,lda,ldb,ldc
      character(len=1):: transa_
      character(len=1):: transb_
      complex(r4):: alpha_
      complex(r4):: beta_
      transa_=optval(transa,"N")
      transb_=optval(transb,"N")
      alpha_ =optval(alpha,one_c4)
      beta_  =optval(beta ,zero_c4)
      if(transa_=='n'.or.transa_=='N') then
         k = size(a,2)
      else
         k = size(a,1)
      endif
      lda = size(a,1)
      ldb = size(b,1)
      ldc = size(c,1)
      m   = size(c,1)
      n   = size(c,2)
      ! <<< call blas77 routine >>>
      call cgemm(transa_,transb_,m,n,k,alpha_,a,lda,b,ldb,beta_,c,ldc)
   end subroutine gemm_c4

   subroutine gemv_c4(a,x,y,trans,alpha,beta)
      ! fortran77 call:
      ! zgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
      ! trans='n','c','t'; default: 'n'
      ! default alpha=1
      ! default beta=0
      ! <<< scalar arguments >>>
      complex(r4), intent(in),optional:: alpha
      complex(r4), intent(in),optional:: beta
      character(len=1), intent(in),optional:: trans
      ! <<< array arguments >>>
      complex(r4), intent(in) :: a(:,:)
      complex(r4), intent(in) :: x(:)
      complex(r4), intent(inout) :: y(:)
      integer :: incx
      integer :: incy
      integer :: m
      integer :: n
      integer :: lda
      character(len=1):: trans_
      complex(r4):: alpha_
      complex(r4):: beta_
      ! <<< intrinsic functions >>>
      trans_ =optval(trans,"N")
      alpha_ =optval(alpha,one_c4)
      beta_  =optval(beta ,zero_c4)

      incx = 1
      incy = 1
      lda = size(a,1)
      m = size(a,1)
      n = size(a,2)
      call cgemv(trans_,m,n,alpha_,a,lda,x,incx,beta_,y,incy)
   end subroutine gemv_c4

   subroutine geut_c4(a,b,itype)
      !! itypes 1 : B = A^{+} B A
      !! itypes 2 : B = A  B A^{+}
      integer,intent(in),optional::itype
      complex(r4),intent(in)::a(:,:)
      complex(r4),intent(inout)::b(:,:)
      complex(r4),allocatable::c(:,:)
      integer::itype_
      allocate(c,mold=a)
      itype_=optval(itype,1)
      if(itype_==1)then
         call linalg%gemm(a, b, c, transa='C')
         call linalg%gemm(c, a, b)
      elseif(itype_==2)then
         call linalg%gemm(a, b, c)
         call linalg%gemm(c, a, b, transb="C")
      else
         error stop "[Linalg Error]:Illegal itype, itype must be 1 or 2"
      end if
   end subroutine geut_c4

   subroutine eigh_c4(a,e,uplo)
      !! call zheev "V","U"
      character,intent(in),optional::uplo
      complex(r4),intent(inout) :: a(:,:)
      complex(r4),allocatable   :: work(:)
      real(r4),intent(out)      :: e(:)
      real(r4),allocatable      :: rwork(:)
      integer                  :: iflag
      integer::n,nb
      integer,external::ilaenv
      character::uplo_
      n=size(a,1)
      uplo_=optval(uplo,"U")
      nb = ilaenv( 1, "chetrd", uplo_, n, -1, -1, -1 )
      allocate(work(nb*n))
      allocate(rwork(3*n-2))
      call cheev("V",uplo_,n,a,n,e,work,nb*n,rwork,iflag)
      deallocate(rwork)
      deallocate(work)
   end subroutine eigh_c4

   subroutine print_c8(a,name)
      !> Print vector/Matrix 
      complex(r8),intent(in)::a(:,:)
      character(len=*),intent(in)::name
      integer::n,i
      write(*,"(3A)")"matrix ",name,"="
      n=size(a,1)
      do i=1,n
         write(*,format_c8)a(i,:)
      end do
   end subroutine print_c8

   subroutine print_vec_c8(a,name)
      !> Print vector/Matrix 
      complex(r8),intent(in)::a(:)
      character(len=*),intent(in)::name
      write(*,"(3A)")"vector ",name,"="
      write(*,format_c8)a
   end subroutine print_vec_c8

   subroutine print_num_c8(a,name)
      !> Print vector/Matrix 
      complex(r8),intent(in)::a
      character(len=*),intent(in)::name
      write(*,"(3A)")"num ",name,"="
      write(*,format_c8)a
   end subroutine print_num_c8

   logical function equal_c8(a,b)result(res)
      !> If Matrix A = B
      complex(r8),intent(in)::a(:,:)
      complex(r8),intent(in)::b(:,:)
      res=all(abs(a-b)<eps_c8)
   end function equal_c8

   subroutine eye_c8(a)
      !> Eye Matrix
      complex(r8),intent(inout) :: a(:,:)
      integer::n,i
      n=size(a,1)
      a=zero_c8
      do i=1,n
         a(i,i)=one_c8
      end do
   end subroutine eye_c8

   logical function inv_c8(a)result(res)
      !! inverse of matrix a
      complex(r8),intent(inout) :: a(:,:)
      integer                  :: info
      integer,allocatable      :: ipiv(:)
      complex(r8),allocatable       :: work(:)
      integer::n,i
      n=size(a,1)
      allocate(ipiv(n))
      call zgetrf(n,n,a,n,ipiv,info)
      res=(info==0)
      if(.not.res)return
      do i=1,n
         if(abs(a(i,i))<eps_c8)then
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

   complex(r8) function det_c8(a)result(res)
      !! Determinant of matrix a
      complex(r8),intent(in)    :: a(:,:)
      integer                  :: info
      integer,allocatable      :: ipiv(:)
      integer::n,i
      complex(r8),allocatable:: tmpa(:,:)
      n=size(a,1)
      select case(n)
      case(2)
         res=a(1,1)*a(2,2)-a(1,2)*a(2,1)
      case(3)
         res=a(1,1)*(a(2,2)*a(3,3)-a(2,3)*a(3,2))+&
            &a(1,2)*(a(2,3)*a(3,1)-a(2,1)*a(3,3))+&
            &a(1,3)*(a(2,1)*a(3,2)-a(2,2)*a(3,1))
      case(4)
         block
            complex(r8)::b(6)
            b(1)=a(3,1)*a(4,2)-a(3,2)*a(4,1)
            b(2)=a(3,1)*a(4,3)-a(3,3)*a(4,1)
            b(3)=a(3,1)*a(4,4)-a(3,4)*a(4,1)
            b(4)=a(3,2)*a(4,3)-a(3,3)*a(4,2)
            b(5)=a(3,2)*a(4,4)-a(3,4)*a(4,2)
            b(6)=a(3,3)*a(4,4)-a(3,4)*a(4,3)
            res= a(1,1)*( a(2,2)*b(6)-a(2,3)*b(5)+a(2,4)*b(4))+&
               &a(1,2)*(-a(2,1)*b(6)+a(2,3)*b(3)-a(2,4)*b(2))+&
               &a(1,3)*( a(2,1)*b(5)-a(2,2)*b(3)+a(2,4)*b(1))+&
               &a(1,4)*(-a(2,1)*b(4)+a(2,2)*b(2)-a(2,3)*b(1))
         end block
      case default
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
      end select
   end function det_c8

   subroutine gemm_c8(a,b,c,transa,transb,alpha,beta)
      !! fortran77 call:
      !! zgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
      !! transa='n','c','t'
      !! transb='n','c','t'
      implicit none
      character(len=1), intent(in),optional:: transa
      character(len=1), intent(in),optional:: transb
      complex(r8), intent(in),optional:: alpha
      complex(r8), intent(in),optional:: beta
      complex(r8), intent(in) :: a(:,:)
      complex(r8), intent(in) :: b(:,:)
      complex(r8), intent(inout) :: c(:,:)
      integer :: m,n,k,lda,ldb,ldc
      character(len=1):: transa_
      character(len=1):: transb_
      complex(r8):: alpha_
      complex(r8):: beta_
      transa_=optval(transa,"N")
      transb_=optval(transb,"N")
      alpha_ =optval(alpha,one_c8)
      beta_  =optval(beta ,zero_c8)
      if(transa_=='n'.or.transa_=='N') then
         k = size(a,2)
      else
         k = size(a,1)
      endif
      lda = size(a,1)
      ldb = size(b,1)
      ldc = size(c,1)
      m   = size(c,1)
      n   = size(c,2)
      ! <<< call blas77 routine >>>
      call zgemm(transa_,transb_,m,n,k,alpha_,a,lda,b,ldb,beta_,c,ldc)
   end subroutine gemm_c8

   subroutine gemv_c8(a,x,y,trans,alpha,beta)
      ! fortran77 call:
      ! zgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
      ! trans='n','c','t'; default: 'n'
      ! default alpha=1
      ! default beta=0
      ! <<< scalar arguments >>>
      complex(r8), intent(in),optional:: alpha
      complex(r8), intent(in),optional:: beta
      character(len=1), intent(in),optional:: trans
      ! <<< array arguments >>>
      complex(r8), intent(in) :: a(:,:)
      complex(r8), intent(in) :: x(:)
      complex(r8), intent(inout) :: y(:)
      integer :: incx
      integer :: incy
      integer :: m
      integer :: n
      integer :: lda
      character(len=1):: trans_
      complex(r8):: alpha_
      complex(r8):: beta_
      ! <<< intrinsic functions >>>
      trans_ =optval(trans,"N")
      alpha_ =optval(alpha,one_c8)
      beta_  =optval(beta ,zero_c8)

      incx = 1
      incy = 1
      lda = size(a,1)
      m = size(a,1)
      n = size(a,2)
      call zgemv(trans_,m,n,alpha_,a,lda,x,incx,beta_,y,incy)
   end subroutine gemv_c8

   subroutine geut_c8(a,b,itype)
      !! itypes 1 : B = A^{+} B A
      !! itypes 2 : B = A  B A^{+}
      integer,intent(in),optional::itype
      complex(r8),intent(in)::a(:,:)
      complex(r8),intent(inout)::b(:,:)
      complex(r8),allocatable::c(:,:)
      integer::itype_
      allocate(c,mold=a)
      itype_=optval(itype,1)
      if(itype_==1)then
         call linalg%gemm(a, b, c, transa='C')
         call linalg%gemm(c, a, b)
      elseif(itype_==2)then
         call linalg%gemm(a, b, c)
         call linalg%gemm(c, a, b, transb="C")
      else
         error stop "[Linalg Error]:Illegal itype, itype must be 1 or 2"
      end if
   end subroutine geut_c8

   subroutine eigh_c8(a,e,uplo)
      !! call zheev "V","U"
      character,intent(in),optional::uplo
      complex(r8),intent(inout) :: a(:,:)
      complex(r8),allocatable   :: work(:)
      real(r8),intent(out)      :: e(:)
      real(r8),allocatable      :: rwork(:)
      integer                  :: iflag
      integer::n,nb
      integer,external::ilaenv
      character::uplo_
      n=size(a,1)
      uplo_=optval(uplo,"U")
      nb = ilaenv( 1, "zhetrd", uplo_, n, -1, -1, -1 )
      allocate(work(nb*n))
      allocate(rwork(3*n-2))
      call zheev("V",uplo_,n,a,n,e,work,nb*n,rwork,iflag)
      deallocate(rwork)
      deallocate(work)
   end subroutine eigh_c8

end module linalg_fortran
