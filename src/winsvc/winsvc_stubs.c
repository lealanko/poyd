#define WIN32_LEAN_AND_MEAN		// Exclude rarely-used stuff from Windows headers
#include <stdio.h>
#include <tchar.h>
#include <windows.h>
#include <assert.h>

#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/threads.h>
#include <stdbool.h>

// Windows 7 preshutdown controls. Not yet in mingw headers.
#ifndef SERVICE_ACCEPT_PRESHUTDOWN
# define SERVICE_ACCEPT_PRESHUTDOWN 0x00000100
#endif

#ifndef SERVICE_CONTROL_PRESHUTDOWN
# define SERVICE_CONTROL_PRESHUTDOWN 0x0000000F
#endif

#include <unistd.h>

#ifdef UNICODE
# error "Must be compiled in single-byte character mode"
#endif

static void msg(const char* s)
{
	write(2, s, strlen(s));
	char end = '\n';
	write(2, &end, 1);
}

static const DWORD control_codes[] = {
	SERVICE_CONTROL_STOP,
	SERVICE_CONTROL_PAUSE,
	SERVICE_CONTROL_CONTINUE,
	SERVICE_CONTROL_INTERROGATE,
	SERVICE_CONTROL_SHUTDOWN,
	SERVICE_CONTROL_PRESHUTDOWN,
	SERVICE_CONTROL_PARAMCHANGE,
	0
};

static const DWORD state_codes[] = {
	SERVICE_STOPPED,
	SERVICE_START_PENDING,
	SERVICE_STOP_PENDING,
	SERVICE_RUNNING,
	SERVICE_CONTINUE_PENDING,
	SERVICE_PAUSE_PENDING,
	SERVICE_PAUSED,
	0
};

static const DWORD accept_codes[] = {
	SERVICE_ACCEPT_STOP,
	SERVICE_ACCEPT_PAUSE_CONTINUE,
	SERVICE_ACCEPT_SHUTDOWN,
	SERVICE_ACCEPT_PRESHUTDOWN,
	0
};

static value
code_to_value(DWORD code, const DWORD* codes)
{
	size_t i = 0;
	while (codes[i] && codes[i] != code) {
		i++;
	}
	return Val_int(i);
}
	

static DWORD
value_to_code(value v_val, const DWORD* codes)
{
	int val = Int_val(v_val);
	size_t i = 0;
	while (codes[i] && i != val) {
		i++;
	}
	return codes[i];
}

static DWORD
value_to_flags(value v_flags, const DWORD* codes)
{
	size_t n_flags = Wosize_val(v_flags);
	DWORD flags = 0;
	for (size_t i = 0; i < n_flags; i++) {
		flags |= value_to_code(Field(v_flags, i), codes);
	}
	return flags;
}
	

static void WINAPI
caml_winsvc_service_main(DWORD argc, LPSTR* argv)
{
	CAMLlocal1(v_args);
	msg("caml_winsvc_service_main start!");
	if (argv) {
		msg("args:");
		for (size_t i = 0; i < argc; i++) {
			msg(argv[i]);
		}
		msg(argv[argc] ? "not null-terminated!"
		    : "is null-terminated");
	} else {
		msg("no args");
	}
	bool is_new = caml_c_thread_register();
	msg("acquire caml runtime...");
	caml_acquire_runtime_system();
	msg("Now got caml runtime!");
		
	value* caml_svc_main = caml_named_value("WinSvc.service_main");
	msg("Looked up WinSvc.service_main, checking result...");
	if (!caml_svc_main) {
		// eeeekkk!
		msg("couldn't find WinSvc.service_main!");
		return;
	}
	
	msg("Have service_main, copying args array");
	const char** args = malloc(sizeof(const char*) * (argc + 1));
	for (size_t i = 0; i < argc; i++) {
		args[i] = argv[i];
	}
	args[argc] = NULL;
	v_args = caml_copy_string_array(args);
	free(args);
	msg("now calling WinSvc.service_main");
	caml_callback_exn(*caml_svc_main, v_args);
	caml_release_runtime_system();
	if (is_new) {
		caml_c_thread_unregister();
	}
}

CAMLprim value
caml_winsvc_start_dispatcher(value name)
{
	CAMLparam1(name);
	char* sname = strdup(String_val(name));
	SERVICE_TABLE_ENTRY dispatch_table[] = {
		{ sname, caml_winsvc_service_main },
		{ NULL, NULL}
	};
	caml_release_runtime_system();
	msg("before StartServiceCtrlDispatcher");
	BOOL result = StartServiceCtrlDispatcher(dispatch_table);
	msg("after StartServiceCtrlDispatcher");
	caml_acquire_runtime_system();
	free(sname);
	CAMLreturn(Val_bool(result));
}

static DWORD WINAPI
caml_winsvc_ctrl_handler(DWORD ctrl, DWORD etype, LPVOID edata, LPVOID ctx)
{
	bool is_new = caml_c_thread_register();
	caml_acquire_runtime_system();
	value* caml_ctrl_handler = caml_named_value("WinSvc.ctrl_handler");
	if (!caml_ctrl_handler) {
		return ERROR_CALL_NOT_IMPLEMENTED;
	}
	value ret = caml_callback_exn(*caml_ctrl_handler,
					code_to_value(ctrl, control_codes));
	bool succ = !Is_exception_result(ret);
	caml_release_runtime_system();
	if (is_new) {
		caml_c_thread_unregister();
	}
	return succ ? NO_ERROR : ERROR_CALL_NOT_IMPLEMENTED;
}

CAMLprim value
caml_winsvc_register_handler(value name)
{
	CAMLparam1(name);
	char* sname = strdup(String_val(name));
	caml_release_runtime_system();
	SERVICE_STATUS_HANDLE handle =
		RegisterServiceCtrlHandlerEx(sname,
					     caml_winsvc_ctrl_handler,
					     NULL);
	caml_acquire_runtime_system();
	free(sname);
	if (!handle) {
		caml_failwith("RegisterServiceCtrlHandler");
	}
	CAMLreturn((value) handle);
}

#define status_state(s) Field(s, 0)
#define status_accept(s) Field(s, 1)
#define status_exitcode(s) Field(s, 2)


CAMLprim value
caml_winsvc_set_status(value v_handle, value v_status)
{
	CAMLparam2(v_handle, v_status);
	SERVICE_STATUS status = {
		.dwServiceType = SERVICE_WIN32_OWN_PROCESS,
		.dwCurrentState =
		value_to_code(status_state(v_status), state_codes),
		.dwControlsAccepted =
		value_to_flags(status_accept(v_status), accept_codes),
		.dwWin32ExitCode = Int_val(status_exitcode(v_status)),
		.dwServiceSpecificExitCode = 0,
		.dwCheckPoint = 0,
		.dwWaitHint = 0
	};
	SERVICE_STATUS_HANDLE handle = (SERVICE_STATUS_HANDLE) v_handle;
	caml_release_runtime_system();
	BOOL ret = SetServiceStatus(handle, &status);
	caml_acquire_runtime_system();
	CAMLreturn(Val_bool(ret));
}


CAMLprim value
caml_winevent_register_event_source(value v_name)
{
	CAMLparam1(v_name);
	HANDLE handle = RegisterEventSource(NULL, String_val(v_name));
	if (!handle) {
		caml_failwith("RegisterEventSource");
	}
	CAMLreturn((value) handle);
}

CAMLprim value
caml_winevent_deregister_event_source(value v_handle)
{
	CAMLparam1(v_handle);
	BOOL succ = DeregisterEventSource((HANDLE) v_handle);
	if (!succ) {
		caml_failwith("DeregisterEventSource");
	}
	CAMLreturn(Val_unit);
}

static const DWORD etype_codes[] = {
	EVENTLOG_ERROR_TYPE,
	EVENTLOG_WARNING_TYPE,
	EVENTLOG_INFORMATION_TYPE,
	0
};

static const DWORD event_ids[] = {
	0xc0000001,
	0x80000001,
	0x40000001,
	0
};


CAMLprim value
caml_winevent_report_event(value v_handle, value v_etype,
			   value v_section, value v_msg)
{
	CAMLparam4(v_handle, v_etype, v_section, v_msg);
	DWORD etype = value_to_code(v_etype, etype_codes);
	DWORD event_id = value_to_code(v_etype, event_ids);
	HANDLE handle = (HANDLE) v_handle;
	LPCTSTR msgs[2] = {
		String_val(v_section),
		String_val(v_msg)
	};
	BOOL succ = ReportEvent(handle, etype, 0, event_id, NULL,
				2, 0, msgs, NULL);
	if (!succ) {
		caml_failwith("ReportEvent");
	}
	CAMLreturn(Val_unit);
}

CAMLprim value
caml_win32_get_version(value v_unit)
{
	CAMLparam1(v_unit);
	CAMLlocal1(ret);
	OSVERSIONINFO version;
	version.dwOSVersionInfoSize = sizeof(version);
	BOOL succ = GetVersionEx(&version);
	if (!succ) {
		caml_failwith("GetVersionEx");
	}
	ret = caml_alloc_tuple(4);
	Store_field(ret, 0, Val_int(version.dwMajorVersion));
	Store_field(ret, 1, Val_int(version.dwMinorVersion));
	Store_field(ret, 2, Val_int(version.dwBuildNumber));
	Store_field(ret, 3, caml_copy_string(version.szCSDVersion));
	CAMLreturn(ret);
}
