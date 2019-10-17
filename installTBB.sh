git clone https://github.com/01org/tbb.git
cd tbb
make -j8
PWD="$( pwd )"
TBB_LIB="$( ls $PWD/build/ | grep release )"
TBB_LIB="-L ${PWD}/build/${TBB_LIB} -ltbb"
TBB_INC="-I ${PWD}/include"


echo "****************************************"
echo "add these two lines to your .bashrc (located at $HOME/.bashrc):"
echo "________________________________________"
echo "export TBB_INC=\"${TBB_INC}\""
echo "export TBB_SHLIB=\"${TBB_LIB}\""
echo "________________________________________"
echo "and hit"
echo "________________________________________"
echo "source ${HOME}/.bashrc"
echo "****************************************"
