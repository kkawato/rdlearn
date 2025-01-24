# Second Submission
## Changes Made
### Addressing Example and Vignette Runtime
We have reduced the runtime of examples in the package by:
- Using smaller simulation data for demonstrations in the example.
- Changing hyperparameters in the vignette.
- Precomputing results for the most computationally intensive parts of examples in the vignette.

# First Submission
## R CMD Check Results 

### Testing Environments 
#### macOS
- **R Version**: R 4.4.2 (2024-10-31)
- **Platform**: aarch64-apple-darwin20
- **Compiler**:
  - Apple clang version 14.0.0 (clang-1400.0.29.202)
  - GNU Fortran (GCC) 12.2.0
- **Operating System**: macOS Sonoma 14.4.1

#### Windows (win-builder)
- **R Version**: R Under development (unstable) (2025-01-20 r87609 ucrt)
- **Platform**: x86_64-w64-mingw32
- **Compiler**:
  - GCC: gcc.exe (GCC) 13.3.0
  - GNU Fortran: GNU Fortran (GCC) 13.3.0
- **Operating System**: Windows Server 2022 x64 (build 20348)

### Results

#### macOS 0 errors | 0 warnings | 1 note
- **Notes**:
  - `checking for future file timestamps ... NOTE`: Unable to verify current time.

#### Windows (win-builder) 0 errors | 0 warnings | 2 notes
- **Notes**:
  - `checking CRAN incoming feasibility ... NOTE`:
    - New Submission
  - `checking examples ... NOTE`:
    - Several examples exceeded 10 seconds of elapsed time:
      - `package_rdlearn`: 13.34 seconds
      - `rdlearn`: 12.81 seconds
      - `summary`: 12.49 seconds
      - `plot`: 12.40 seconds
      - `sens`: 11.85 seconds
    - These examples are essential to demonstrate the package functionality and require this amount of computational time. 
