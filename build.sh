echo Building Spite Lang;
cd ./Build;
make;
echo Running Spite Lang;
./Spite_Lang -file ../Test/Test.sp -dir ../Test -entry Init -os linux;
cd ./Build;
echo Linking;
ld -o output a.o -L /lib/gcc/x86_64-linux-gnu /lib/x86_64-linux-gnu/crti.o /lib/x86_64-linux-gnu/crtn.o /lib/x86_64-linux-gnu/crt1.o -dynamic-linker /lib64/ld-linux-x86-64.so.2 -lc -lm;
./output;