#include<execinfo.h>
#include<stdio.h>
#include<stdlib.h>
#include<unistd.h>

void myfunc3(void)
{
	int j, nptrs;
	void *buffer[100];
	char **strings;

	nptrs = backtrace(buffer, 100);
	printf("backtrace() returned %d addresses\n", nptrs);
	strings = backtrace_symbols(buffer, nptrs);

	if (strings == NULL) {
		perror("backtrace_symbols");
		exit(EXIT_FAILURE);
	}
	for (j = 0; j < nptrs; j++)
		printf("%s\n", strings[j]);
	free(strings);
}

void myfunc2(void)
{
	myfunc3();
}

void myfunc1(void)
{
	myfunc2();
}

int main(int argc,char *argv[])
{
	myfunc1();
	exit(0);
}
