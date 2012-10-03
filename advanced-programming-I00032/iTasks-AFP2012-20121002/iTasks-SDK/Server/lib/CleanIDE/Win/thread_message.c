
#include <stdio.h>
#include <windows.h>
#include "Clean.h"
#include "thread_message.h"

static int CleanCompiler_message_nunber;

int get_message_number (void)
{
	return RegisterWindowMessage ("CleanCompiler");
}

int get_current_thread_id (void)
{
	return GetCurrentThreadId();
}

int compiler_result_handler_installed=0;

extern void (*dispatch_null_message_hook) (MSG*);

#define MAX_N_COMPILERS 32

int compiler_finished[MAX_N_COMPILERS];
int compiler_exit_codes[MAX_N_COMPILERS];

void compiler_result_handler (MSG *msg)
{	
	if (msg->message==CleanCompiler_message_nunber){
		unsigned int compiler_n;

		compiler_n=msg->wParam;

		if (compiler_n<MAX_N_COMPILERS){
			compiler_exit_codes[compiler_n]=msg->lParam;
			compiler_finished[compiler_n]=1;
		}
	}
}

void install_compiler_result_handler (void)
{
	CleanCompiler_message_nunber=get_message_number();

	dispatch_null_message_hook = &compiler_result_handler;
}

int start_compiler_process (CleanString compiler_path,CleanString compiler_directory,CleanString command,
							int *compiler_thread_id_p,size_t *compiler_thread_handle_p,size_t *compiler_process_handle_p)
{
	PSTR application_name,command_line,env,dir;
	STARTUPINFO si;
	PROCESS_INFORMATION pi;
	int r;

	if (!compiler_result_handler_installed){
		install_compiler_result_handler();
		compiler_result_handler_installed=1;
	}

	application_name=CleanStringCharacters (compiler_path);
	dir=CleanStringCharacters (compiler_directory);
	command_line=CleanStringCharacters (command);
	env=NULL;
	
	si.cb = sizeof (STARTUPINFO);
	si.lpReserved = NULL;
	si.lpReserved2 = NULL;
	si.cbReserved2 = 0;
	si.lpDesktop = NULL;
	si.lpTitle = NULL;
	si.dwFlags = 0;

	r=CreateProcess (application_name,command_line,NULL,NULL,TRUE,DETACHED_PROCESS/*0*/,env,dir,&si,&pi);

	if (r!=0){
		*compiler_thread_id_p=pi.dwThreadId;
		*compiler_thread_handle_p=(size_t)pi.hThread;
		*compiler_process_handle_p=(size_t)pi.hProcess;
	} else {
		*compiler_thread_id_p=0;
		*compiler_thread_handle_p=0;
		*compiler_process_handle_p=0;
	}
	
	return r;
}

int get_integers_from_message (int wm_number,int *i1_p,int *i2_p)
{
	MSG message;
	int r;

	r=GetMessage (&message,NULL,wm_number,wm_number);

	if (r!=0){
		*i1_p=message.wParam;
		*i2_p=message.lParam;
	} else {
		*i1_p=0;
		*i2_p=0;
	}
	
	return r;
}

#define PM_QS_POSTMESSAGE   ((QS_POSTMESSAGE | QS_HOTKEY | QS_TIMER) << 16)

int get_integers_from_thread_message (int wm_number,size_t thread_handle,int *i1_p,int *i2_p)
{
	MSG message;
	int r;

	r=PeekMessage (&message,INVALID_HANDLE_VALUE,wm_number,wm_number,PM_NOREMOVE | (QS_POSTMESSAGE<<16));
	if (r==0){
		r=MsgWaitForMultipleObjects (1,(HANDLE*)&thread_handle,0,INFINITE,QS_POSTMESSAGE);

		if (r==-1 || r==WAIT_OBJECT_0 || r==WAIT_ABANDONED_0){
			*i1_p=0;
			*i2_p=0;
			return 0;
		}

		do {
			r=PeekMessage (&message,INVALID_HANDLE_VALUE,wm_number,wm_number,PM_NOREMOVE | (QS_POSTMESSAGE<<16));
		} while (r==0);
	}

	r=PeekMessage (&message,INVALID_HANDLE_VALUE,wm_number,wm_number,PM_REMOVE | (QS_POSTMESSAGE<<16));
/*	r=GetMessage (&message,INVALID_HANDLE_VALUE,wm_number,wm_number); */

	if (r!=0){
		*i1_p=message.wParam;
		*i2_p=message.lParam;
	} else {
		*i1_p=0;
		*i2_p=0;
	}
	
	return r;
}

int get_string_from_file_map_and_delete_map (size_t file_map,CleanString s)
{
	int l,i;
	char *chars,*p;
	
	chars=CleanStringCharacters (s);
	l=CleanStringLength (s);
	
	p=MapViewOfFile ((HANDLE)file_map,FILE_MAP_ALL_ACCESS,0,0,l);
	if (p==NULL)
		return 0;

	for (i=0; i<l; ++i)
		chars[i]=p[i];
				
	UnmapViewOfFile (p);

	CloseHandle ((HANDLE)file_map);
	
	return 1;
}

int send_string_to_thread (int thread_id,size_t process_handle,int wm_number,CleanString s)
{
	HANDLE file_map,file_map2;
	char *chars,*p1;
	int r,l;
	/*
	int a;
	*/

	chars=CleanStringCharacters (s);
	l=CleanStringLength (s);
	
	if (l==0 || chars[l-1]!='\0')
		return 0;

	file_map=CreateFileMapping (INVALID_HANDLE_VALUE,NULL,PAGE_READWRITE,0,l,NULL);
	if (file_map==NULL)
		return 0;

	p1=MapViewOfFile (file_map,FILE_MAP_ALL_ACCESS,0,0,l);
	if (p1==NULL)
		return 0;
		
	{
		char *s_p,*d_p,c;
		
		s_p=chars;
		d_p=p1;
		do {
			c=*s_p++;
			*d_p++=c;
		} while (c!='\0');
	}
	
	UnmapViewOfFile (p1);

	r=DuplicateHandle (GetCurrentProcess(),file_map,(HANDLE)process_handle,&file_map2,0,0,DUPLICATE_CLOSE_SOURCE | DUPLICATE_SAME_ACCESS);

	if (r==0)
		return 0;

	do {
		/*
		r=PostThreadMessage (thread_id,wm_number,a,l);
		*/
		r=PostThreadMessage (thread_id,wm_number,l,(int)file_map2);
	} while (r==0);
	
	return r;
}

int send_integers_to_thread (int thread_id,int wm_number,int i1,int i2)
{
	int r;
		
	do {
		r=PostThreadMessage (thread_id,wm_number,i1,i2);
	} while (r==0);
	
	return r;
}

int compiler_id=-1;

int set_compiler_id (int compiler_id_p)
{
	compiler_id=compiler_id_p;
	return compiler_id_p;
}

int get_compiler_id (void)
{
	return compiler_id;
}

int get_finished_compiler_id_and_exit_code (int *exit_code_p)
{
	int compiler_n;
	
	for (compiler_n=0; compiler_n<MAX_N_COMPILERS; ++compiler_n)
		if (compiler_finished[compiler_n]){
			*exit_code_p=compiler_exit_codes[compiler_n];
			compiler_finished[compiler_n]=0;
			return compiler_n;
		}
	
	*exit_code_p=0;
	return -1;
}

void win_create_process (PSTR commandline,PSTR dir,int *success,int *exitcode)
{
	SECURITY_ATTRIBUTES sa;
	STARTUPINFO si;
	PROCESS_INFORMATION pi;
	BOOL fsuccess;

	*success = FALSE;
	*exitcode = -1;

	sa.nLength = sizeof (SECURITY_ATTRIBUTES);
	sa.bInheritHandle = TRUE;
	sa.lpSecurityDescriptor = NULL;

	si.cb = sizeof (STARTUPINFO);
	si.lpReserved = NULL;
	si.lpReserved2 = NULL;
	si.cbReserved2 = 0;
	si.lpDesktop = NULL;
	si.lpTitle = NULL;
	si.dwFlags = 0;
	si.hStdInput = NULL;
	si.hStdOutput = NULL;
	si.hStdError = NULL;

	fsuccess =
		CreateProcess (NULL,				/* pointer to name of executable module		*/
					   commandline,			/* pointer to command line string			*/
					   NULL,				/* pointer to process security attributes	*/
					   NULL,				/* pointer to thread security attributes	*/
					   TRUE,				/* handle inheritance flag					*/
					   DETACHED_PROCESS,	/* creation flags							*/
					   NULL,				/* pointer to new environment block			*/
					   dir,					/* pointer to current directory name		*/
					   &si,					/* pointer to STARTUPINFO					*/
					   &pi					/* pointer to PROCESS_INFORMATION			*/
		);

	if (fsuccess) {
		WaitForSingleObject (pi.hProcess, INFINITE);
		GetExitCodeProcess (pi.hProcess, (unsigned long *) exitcode);
		*success = TRUE;
	} else {
		*success = FALSE;
		*exitcode = -1;
	}
}
