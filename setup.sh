git clone https://github.com/verilator/verilator
cd verilator
git checkout v4.226
autoconf
./configure --prefix=`pwd`/../verilator-install
make -j8
make install #DESTDIR=`pwd`/../verilator-install
cd ..

wget https://github.com/llvm/circt/releases/download/firtool-1.43.0/firrtl-bin-ubuntu-20.04.tar.gz
tar xf firrtl-bin-ubuntu*

export PATH=`pwd`/firtool-1.43.0/bin:`pwd`/verilator-install/bin:"$PATH"
