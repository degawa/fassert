# fassert
An extensible assertion library oriented to work with unit testing frameworks for Fortran

## Motivations
There are some assertion libraries and unit test frameworks for Fortran:

- [Assert](https://github.com/sourceryinstitute/assert)
- [assert-fortran](https://github.com/alecksandr26/assert-fortran)
- [naturalFRUIT](https://cibinjoseph.github.io/naturalFRUIT/index.html)
- [pFUnit](https://github.com/Goddard-Fortran-Ecosystem/pFUnit)
- [test-drive](https://github.com/fortran-lang/test-drive)
- [TOAST](https://github.com/thomasms/toast)
- [veggies](https://gitlab.com/everythingfunctional/veggies) and
[garden](https://gitlab.com/everythingfunctional/garden)

They are designed to complete on their own. Therefore, extending features, such as supporting user-defined types and changing output messages containing expected and actual values, takes a lot of work.

This repository aims to provide an extensible assertion library, fassert, that can collaborate with unit testing frameworks.

## Overview
The current version of fassert provides 9 assertion procedures.

|       procedure       |                               functionality                               |
| :-------------------- | :------------------------------------------------------------------------ |
| `assert_equal`        | checks that an actual value is equal to the expected one                  |
| `expect_equal`        | checks that an actual value is equal to the expected one                  |
| `assert_true`         | checks that an actual value is true                                       |
| `expect_true`         | checks that an actual value is true                                       |
| `assert_false`        | checks that an actual value is false                                      |
| `expect_false`        | checks that an actual value is false                                      |
| `expect_same_shape`   | checks that the actual and expected values have the same shape            |
| `assert_approx_equal` | checks that an actual value is equal to the expected one within tolerance |
| `expect_approx_equal` | checks that an actual value is equal to the expected one within tolerance |

The procedures are classified  into two types.
1. procedures beginning with `assert` immediately error stops the program when the assertion fails.
1. procedures beginning with `expect` do not stop the program even if the assertion fails and return the assertion status and message through `intent(out)` variables. Those are callable inside `pure` procedures.

Supported types and ranks currently are listed below:

|                    procdure                     |       types        |  rank   |                                        note                                        |
| :---------------------------------------------- | :----------------- | :------ | :--------------------------------------------------------------------------------- |
| `assert_equal`,<br>`expect_equal`               | `integer(int8)`    | 0,1,2,3 | 0 means a scalar variable                                                          |
|                                                 | `integer(int16)`   | 0,1,2,3 |                                                                                    |
|                                                 | `integer(int32)`   | 0,1,2,3 |                                                                                    |
|                                                 | `integer(int64)`   | 0,1,2,3 |                                                                                    |
|                                                 | `real(real32)`     | 0,1,2,3 | The equality of floating-point numbers is evaluated based on the integers converted from them to have the same bitset. |
|                                                 | `real(real64)`     | 0,1,2,3 | The default tolerance is 2, which can be specified by means of the preprocessor macro `FASSERT_ULP` at compile time. |
|                                                 | `real(real128)`    | 0,1,2,3 | If the `real128` is not binary128, for example double-double in NAG Fortran, the equality is evaluated based on the relative epsilon.  |
|                                                 | `complex(real32)`  | 0,1,2,3 | The equality is evaluated whether both the real and imaginary parts converted to integers are within the tolerance. |
|                                                 | `complex(real64)`  | 0,1,2,3 |                                                                                    |
|                                                 | `complex(real128)` | 0,1,2,3 |                                                                                    |
|                                                 | `logical`          | 0,1,2,3 |                                                                                    |
|                                                 | `character(*)`     | 0,1,2,3 |                                                                                    |
|                                                 | user-defined type  | 0,1,2,3 | need to write comparator and message output procedures according to the interfaces |
| `assert_true`,<br>`expect_true`                 | `logical`          | 0,1,2,3 |                                                                                    |
| `assert_false`,<br>`expect_false`               | `logical`          | 0,1,2,3 |                                                                                    |
| `assert_approx_equal`,<br>`expect_approx_equal` | `real(real32)`     | 0,1,2,3 |                                                                                    |
|                                                 | `real(real64)`     | 0,1,2,3 |                                                                                    |
|                                                 | `real(real128)`    | 0,1,2,3 |                                                                                    |
|                                                 | `complex(real32)`  | 0,1,2,3 | The equality is evaluated whether the l2 norm of the real and imaginary parts is within the tolerance. |
|                                                 | `complex(real64)`  | 0,1,2,3 |                                                                                    |
|                                                 | `complex(real128)` | 0,1,2,3 |                                                                                    |
| `expect_same_shape`                             | any                | 1,2,3   | Any types, including user-defined types, are acceptable.<br>The intended use is to compare array shapes in `assert_equal`/`expect_equal` before comparing those values. |

The procedures have several optional arguments. `assert_*` procedures have 3 optional logical arguments, `expected_failure`, `verbose`, and `quiet`. Setting `expected_failure` to `.true.`, the procedures assert that the test will fail as expected. `verbose` and `quiet` control the output of the message from the procedures. `expect_*` procedures have the same arguments, excepting `expect_same_shape` which does not have `expected_failure`. In addition, the procedures have `output_message` argument specifying a string to store the output message instead of outputting it to a unit.

### Examples
- A simple assertion

```Fortran
call assert_equal(0 - 1, 1, "result should be 1 when input 0+1")
! FAILED: result should be 1 when input 0+1
!     Expected: 1
!     Actual  : -1
! ERROR STOP
```

- Retrieving an assertion result and message for collaborating with test-drive
```Fortran
use :: testdrive, only:error_type, check
logical :: stat
character(:), allocatable :: msg
type(error_type), allocatable :: error

call expect_equal(doublify(10), 20, "it should return 20 when input 10", stat, output_message=msg)
! doublify() is the procedure under test.
! stat = .false.
! msg = FAILED: it should return 20 when input 10
!           Expected: 20
!           Actual  : 0

call check(error, stat, msg)
! The error will be caught. test-drive will display output messages below:
! # Testing: collaboration with testdrive
!   Starting doublify() should return 20 when input 10 ... (1/1)
!        ... doublify() should return 20 when input 10 [FAILED]
!   Message: FAILED: it should return 20 when input 10
!     Expected: 20
!     Actual  : 0
! 1 test(s) failed!
! ERROR STOP
```

The complete example can be found in `example/collab/testdrive.f90`

- An assertion for user-defined types
```Fortran
type(vector2d_type) :: x, y

x = vector2d_type(1d0, 2d0)
y = vector2d_type(2d0, 1d0)

call assert_equal(x, y, "vector x should equal to y", is_equal_vec2d, output_on_failure_vec2d)
! FAILED: vector x should equal to y
!     Expected: [2.00000000,1.00000000]
!     Actual  : [1.00000000,2.00000000]
!     Difference:[1.00000000,-1.00000000]
! ERROR STOP
```

The complete example can be found in `example/userDefined/vector2d/vector2d.f90`

## Getting started
### Requirements
fassert has been tested only on Windows 10 but may also work on Linux/Mac OS.
The compilers and versions listed below have been used to develop fassert.

- A Fortran 2008 compiler
    - gfortran 11.2 bundled with [quickstart Fortran on Windows](https://github.com/LKedward/quickstart-fortran)
    - Intel Fortran Classic 2021.5.0 Build 20211109_000000
    - NAG Fortran 7.1 Build 7117
- [Fortran Package Manager](https://github.com/fortran-lang/fpm) (fpm) 0.7.0 alpha
    - fassert is created as an fpm project.
- [test-drive](https://github.com/fortran-lang/test-drive) 0.4.0
    - fassert provides an example of a collaboration with test-drive.
- [FORD](https://github.com/Fortran-FOSS-Programmers/ford) (optional)

### Get the code
To get the code, execute the following commnad:

```console
git clone https://github.com/degawa/fassert.git
cd fassert
```

### Build with fpm
To build the library using fpm, execute the following command:

```console
fpm build
```

Specify the preprocessor macro `FASSERT_ULP` to set the tolerance for evaluating the equality of floating-point numbers as follows:

```console
fpm build --flag "-DFASSERT_ULP=4"
```

Then, install the library using:

```console
fpm install --prefix path/to/your/libdir
```

### Running the tests
To run the tests for the assert using fpm, execute the following command:

```console
fpm test --flag "-DWITH_QP"
```

`WITH_QP` is a preprocessor macro for enabling `real128` types in test-drive, the testing library.

### Reference from your project
Add the following `use` statement to modules or procedures calling fassert.

```Fortran
use :: fassert
```

Use the `fassert_kit` module when writing a comparator and message output procedure for supporting user-defined types or extending behavior for supported types.
```Fortran
use :: fassert_kit
```

### Reference as a fpm project's dependency
To use fassert in your fpm project, add the following to the fpm.toml.

```TOML
[dependencies]
fassert = {git = "https://github.com/degawa/fassert.git"}
```

## Todo
- [ ] To translate docstrings from Japanese into English.
- [x] To support `complex` types.
- [ ] To support High-ranked arrays.
- [ ] To add unit tests.
- [x] To generate codes using fypp.
- [ ] To add other assertion procedures.
    - [x] `assert_approx_equal`
    - [x] `assert_char_equal`
