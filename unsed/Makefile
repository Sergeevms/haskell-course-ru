comp=gcc
obj=readlines-main.o readlines.o

readlines-main : $(obj)
	$(comp) -o readlines-main $(obj)

readlines.o : readlines.c readlines.h
	$(comp) -c readlines.c

readlines-main.o : readlines-main.c
	$(comp) -c readlines-main.c

clean:
	rm $(obj)
