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
- **R Version**: R Under development (unstable) (2025-01-15 r87581 ucrt)
- **Platform**: x86_64-w64-mingw32 (64-bit Windows)
- **Compiler**:
  - GCC: gcc.exe (GCC) 13.3.0
  - GNU Fortran: GNU Fortran (GCC) 13.3.0
- **Operating System**: Windows Server 2022 x64 (build 20348)


### Results

#### macOS 0 errors | 0 warnings | 2 notes
- **Notes**:
  - `checking for future file timestamps ... NOTE`: Unable to verify current time.
  
#### Windows (win-builder) 0 errors | 0 warnings | 2 notes
- **Notes**:
  - `checking examples ... NOTE`:
    - Several examples exceeded 10 seconds of elapsed time:
      - `package_rdlearn`: 13.88 seconds
      - `plot.rdlearn`: 13.01 seconds
      - `summary.rdlearn`: 12.95 seconds
      - `rdlearn`: 12.72 seconds
      - `sens`: 12.75 seconds
    - These examples are essential to demonstrate the package functionality and require this amount of computational time. Efforts to optimize the runtime will be considered in future updates.
