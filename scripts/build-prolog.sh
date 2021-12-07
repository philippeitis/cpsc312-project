cd swipl
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=$HOME -G Ninja ..
ninja
ctest -j 8
ninja install