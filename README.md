# iso_linalg_mod
linalg :Fortran lapack interface 

- type:`complex(8)` and `real(8)`

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
   use iso_linalg_mod
   real(8)::a(3,3)
   call random_number(a)
   if(linalg%inv(a))then
      call linalg%print(a,"a")
   end if
end program check

```
