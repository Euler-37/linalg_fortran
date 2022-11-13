# linalg
linalg :Fortran lapack interface 

- type:`complex(8)` , `real(8)`,` complex(8)` , `real(4)`

| name    | Class        | Description                                                |
|   :-:   |   :-:        |:-:                                                         |
| `inv`   | `function`   | inverse of Matrix,return **.true.** if not singular        |
| `det`   | `function`   | Determinant of Matrix                                      |
| `eigh`  | `subroutine` | eigenvalues and eigenvectors for Symmetry/Hermitian Matrix |
| `gemm`  | `subroutine` | gemm                                                       |
| `eye`   | `subroutine` | eye                                                        |
| `print` | `subroutine` | print matrix                                               |
| `geut`  | `subroutine` | $U^{\dagger}AU$ or $UAU^{\dagger}$                         |
| `equal` | `function`   | equal                                                      |

```fortran
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


```
