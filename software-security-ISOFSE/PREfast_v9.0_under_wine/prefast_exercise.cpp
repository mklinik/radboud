#include "stdafx.h"
#include "stdio.h"
#undef __analysis_assume
#include <CodeAnalysis\SourceAnnotations.h>

#define BUF_SIZE 100
#define STR_SIZE 200

void zeroing();

_Ret_opt_cap_(size) char *my_alloc(_In_ size_t size) {
	char *ch  = (char *)malloc(size);
    // FIXED: check that ch is not NULL and that nulling the first and last char is permissible.
    if( ch && size > 0 )
    {
        *ch = NULL;
        // FIXED: the last char is size-1 not size.
        ch[size-1] = NULL;  // null terminate here too, to be safe
    }
	return ch;
}

// FIXED: add len parameter and use gets_s instead of gets
HRESULT input([SA_Post(Tainted=SA_Yes)] _Out_cap_(len) char *buf, _In_ size_t len) {
	return (gets_s(buf, len) != NULL)?SEVERITY_SUCCESS:SEVERITY_ERROR;
}

[returnvalue:SA_Post(Tainted=SA_Yes)] _Ret_opt_cap_c_(STR_SIZE) char *do_read() {
	char *buf = my_alloc(STR_SIZE);
    // FIXED: to print pointers, use %p in a format string instead of %x
	printf("Allocated a string at %p", buf);
    // FIXED: use FAILED macro to correctly evaluate HRESULT value
    // FIXED: check that buff is not null before passing it to input
	if (buf && FAILED(input(buf, STR_SIZE))) {
		printf("error!");
		exit(-1);
	}
    // FIXED: comparison is == not =
    // FIXED: check that buf is not NULL before dereferencing it
	if (buf == NULL || *buf == NULL)
		printf("empty string");
	return buf;
}

// TODO: is there a way to specify that taintedness propagates from buf1 to buf2?
// If buf1 is tainted so is buf2, but if buf1 is validated so is buf2.
void copy_data(_In_opt_count_c_(STR_SIZE) char *buf1,
               _Out_cap_c_(STR_SIZE) char *buf2) {
	memcpy(buf2,buf1,STR_SIZE);
	buf2[STR_SIZE-1] = NULL; // null terminate, just in case
}

int execute([SA_Pre(Tainted=SA_No)] char *buf) {
	return system(buf); // pass buf as command to be executed by the OS
}

void validate([SA_Pre(Tainted=SA_Yes)][SA_Post(Tainted=SA_No)] char *buf) {

    // This is a magical validation method, which turns tainted data
    // into untainted data, for which the code not shown.
    //
    // A real implementation might for example use a whitelist to filter
    // the string.

}

_Check_return_ int test_ready() {
	// code not shown
	return 1;
}

int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow) {
	char *buf1 = do_read();
    // FIXED:  do_read allocates STR_SIZE, so buf2 must also be STR_SIZE
	char *buf2 = my_alloc(STR_SIZE);
	if (buf2 == NULL)
		exit(-1);
	zeroing();
    // FIXED: we have to use the return value of test_ready.
	if( test_ready() )
    {
        validate(buf1);
        execute(buf1);

        // TODO: copy_data should propagate taintedness; see comment for copy_data
        char* buf3 = do_read();
        copy_data(buf3, buf2);
        execute(buf2);

        char *buf4 = do_read();
        validate(buf4);
        execute(buf4);
    }
}

// *****************************************************************

void zero(_Out_cap_(len) int *buf, _In_ int len)
{
    int i;
    // FIXED: use < instead of <= to prevent buffer overflow
    for(i = 0; i < len; i++)
        buf[i] = 0;
}

void zeroboth(_Out_cap_(len) int *buf, _In_ int len,
              _Out_cap_(len3) int *buf3, _In_ int len3)
{
    int *buf2 = buf;
    int len2 = len;
    zero(buf2, len2);
    zero(buf3, len3);
}

void zeroboth2(_Out_cap_(len) int *buf, _In_ int len,
               _Out_cap_(len3) int *buf3, _In_ int len3)
{
	zeroboth(buf, len3, buf3, len);
}

void zeroing()
{
    int elements[200];
    int oelements[100];
    zeroboth2(elements, 200, oelements, 100);
}
