# sdfast
SD/FAST open source code

## WORK IN PROGRESS

SD/FAST is a symbolic multibody dynamics code developed by Dan Rosenthal and Michael Sherman (Sherm)
at Symbolic Dynamics Inc. in the late 1980s and early 1990s. 

SD/FAST was acquired by PTC (formerly Parametric Technology Corp.) and sold by PTC until 2025. 
PTC has generously contributed it to open source in 2026, in exchange for Sherm's promise to
create the open source organization on GitHub and get the code working again with no need for
security keys or paying money! The license is Apache 2.0 which is the most permissive open
source license (even better than BSD or MIT in some ways).

Please use the GitHub Discussion feature for questions, or file Issues if you have requests or
find problems. Pull requests would be greatly appreciated if you fix problems or want to make
updates.

### Note from Sherm 20260119

Version 2.0 of open SD/FAST is posted now and still has a lot of ancient cruft. I added
a CMakeLists.txt to get the code to build, and successfully built it on Linux. It should build
on Windows also but I haven't tried it. The old examples and a pdf of the manual are there,
unchanged from circa 1998.

Other than removing the security code and a few minor formatting fixups, this is SD/FAST
B.2.9 from 1998 which is what PTC has been selling for the last 20+ years.

Please let me know what problems you run into. I think there may be some modernizing
needed of the generated code, at least to avoid warnings from modern C compilers.
