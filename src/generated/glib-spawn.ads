------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------


pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;            use GNAT.Strings;
with Glib.Error;              use Glib.Error;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Gtkada.Types;            use Gtkada.Types;

package Glib.Spawn is

   type GSpawn_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, GSpawn_Flags);
   --  Flags passed to g_spawn_sync, g_spawn_async and
   --  g_spawn_async_with_pipes.

   G_Spawn_Default : constant GSpawn_Flags := 0;
   G_Spawn_Leave_Descriptors_Open : constant GSpawn_Flags := 1;
   G_Spawn_Do_Not_Reap_Child : constant GSpawn_Flags := 2;
   G_Spawn_Search_Path : constant GSpawn_Flags := 4;
   G_Spawn_Stdout_To_Dev_Null : constant GSpawn_Flags := 8;
   G_Spawn_Stderr_To_Dev_Null : constant GSpawn_Flags := 16;
   G_Spawn_Child_Inherits_Stdin : constant GSpawn_Flags := 32;
   G_Spawn_File_And_Argv_Zero : constant GSpawn_Flags := 64;
   G_Spawn_Search_Path_From_Envp : constant GSpawn_Flags := 128;
   G_Spawn_Cloexec_Pipes : constant GSpawn_Flags := 256;

   type GSpawn_Error is (
      G_Spawn_Error_Fork,
      G_Spawn_Error_Read,
      G_Spawn_Error_Chdir,
      G_Spawn_Error_Acces,
      G_Spawn_Error_Perm,
      G_Spawn_Error_Too_Big,
      G_Spawn_Error_Noexec,
      G_Spawn_Error_Nametoolong,
      G_Spawn_Error_Noent,
      G_Spawn_Error_Nomem,
      G_Spawn_Error_Notdir,
      G_Spawn_Error_Loop,
      G_Spawn_Error_Txtbusy,
      G_Spawn_Error_Io,
      G_Spawn_Error_Nfile,
      G_Spawn_Error_Mfile,
      G_Spawn_Error_Inval,
      G_Spawn_Error_Isdir,
      G_Spawn_Error_Libbad,
      G_Spawn_Error_Failed);
   pragma Convention (C, GSpawn_Error);
   --  Error codes returned by spawning processes.

   type GPid is new Interfaces.C.ptrdiff_t;

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package GSpawn_Flags_Properties is
      new Generic_Internal_Discrete_Property (GSpawn_Flags);
   type Property_GSpawn_Flags is new GSpawn_Flags_Properties.Property;

   package GSpawn_Error_Properties is
      new Generic_Internal_Discrete_Property (GSpawn_Error);
   type Property_GSpawn_Error is new GSpawn_Error_Properties.Property;

   ----------------------
   -- GtkAda additions --
   ----------------------

   generic
   type User_Data is limited private;
   function Generic_Spawn_Async
     (Working_Directory : Gtkada.Types.Chars_Ptr := Gtkada.Types.Null_Ptr;
      Argv              : access Gtkada.Types.Chars_Ptr_Array;
      Envp              : access Gtkada.Types.Chars_Ptr_Array;
      Flags             : GSpawn_Flags;
      Child_Setup       : access procedure
        (Data : access User_Data);
      Data              : access User_Data;
      Child_Pid         : access GPid;
      Error             : access Glib.Error.GError)
   return Glib.Gboolean;
   pragma Import (C, Generic_Spawn_Async, "gnat_spawn_async");
   --  See Glib.Spawn.Spawn_Async_With_Pipes for a full description; this
   --  function simply calls the Glib.Spawn.Spawn_Async_With_Pipes without any
   --  pipes.
   --  You should call Glib.Spawn.Spawn_Close_Pid on the returned child
   --  process reference when you don't need it any more.
   --  If you are writing a GTK+ application, and the program you are spawning
   --  is a graphical application, too, then you may want to use
   --  gdk_spawn_on_screen instead to ensure that the spawned program opens its
   --  windows on the right screen.
   --  Note that the returned Child_Pid on Windows is a handle to the child
   --  process and not its identifier. Process handles and process identifiers
   --  are different concepts on Windows.
   --  "working_directory": child's current working directory, or null to
   --  inherit parent's
   --  "argv": child's argument vector
   --  "envp": child's environment, or null to inherit parent's
   --  "flags": flags from Glib.Spawn.GSpawn_Flags
   --  "child_setup": function to run in the child just before exec
   --  "child_pid": return location for child process reference, or null

   generic
   type User_Data is limited private;
   function Generic_Spawn_Async_With_Pipes
     (Working_Directory : Gtkada.Types.Chars_Ptr := Gtkada.Types.Null_Ptr;
      Argv              : access Gtkada.Types.Chars_Ptr_Array;
      Envp              : access Gtkada.Types.Chars_Ptr_Array;
      Flags             : GSpawn_Flags;
      Child_Setup       : access procedure
        (Data : access User_Data);
      Data              : access User_Data;
      Child_Pid         : access GPid;
      Standard_Input    : access Glib.Gint;
      Standard_Output   : access Glib.Gint;
      Standard_Error    : access Glib.Gint;
      Error             : access Glib.Error.GError)
   return Glib.Gboolean;
   pragma Import
     (C, Generic_Spawn_Async_With_Pipes, "gnat_spawn_async_with_pipes");
   --  Executes a child program asynchronously (your program will not block
   --  waiting for the child to exit). The child program is specified by the
   --  only argument that must be provided, Argv. Argv should be a
   --  null-terminated array of strings, to be passed as the argument vector
   --  for the child. The first string in Argv is of course the name of the
   --  program to execute. By default, the name of the program must be a full
   --  path. If Flags contains the Glib.Spawn.G_Spawn_Search_Path flag, the
   --  `PATH` environment variable is used to search for the executable. If
   --  Flags contains the Glib.Spawn.G_Spawn_Search_Path_From_Envp flag, the
   --  `PATH` variable from Envp is used to search for the executable. If both
   --  the Glib.Spawn.G_Spawn_Search_Path and
   --  Glib.Spawn.G_Spawn_Search_Path_From_Envp flags are set, the `PATH`
   --  variable from Envp takes precedence over the environment variable.
   --  If the program name is not a full path and
   --  Glib.Spawn.G_Spawn_Search_Path flag is not used, then the program will
   --  be run from the current directory (or Working_Directory, if specified);
   --  this might be unexpected or even dangerous in some cases when the
   --  current directory is world-writable.
   --  On Windows, note that all the string or string vector arguments to this
   --  function and the other g_spawn* functions are in UTF-8, the GLib file
   --  name encoding. Unicode characters that are not part of the system
   --  codepage passed in these arguments will be correctly available in the
   --  spawned program only if it uses wide character API to retrieve its
   --  command line. For C programs built with Microsoft's tools it is enough
   --  to make the program have a wmain instead of main. wmain has a wide
   --  character argument vector as parameter.
   --  At least currently, mingw doesn't support wmain, so if you use mingw to
   --  develop the spawned program, it will have to call the undocumented
   --  function __wgetmainargs to get the wide character argument vector and
   --  environment. See gspawn-win32-helper.c in the GLib sources or init.c in
   --  the mingw runtime sources for a prototype for that function.
   --  Alternatively, you can retrieve the Win32 system level wide character
   --  command line passed to the spawned program using the GetCommandLineW
   --  function.
   --  On Windows the low-level child process creation API CreateProcess
   --  doesn't use argument vectors, but a command line. The C runtime
   --  library's spawn* family of functions (which
   --  Glib.Spawn.Spawn_Async_With_Pipes eventually calls) paste the argument
   --  vector elements together into a command line, and the C runtime startup
   --  code does a corresponding reconstruction of an argument vector from the
   --  command line, to be passed to main. Complications arise when you have
   --  argument vector elements that contain spaces of double quotes. The
   --  spawn* functions don't do any quoting or escaping, but on the other hand
   --  the startup code does do unquoting and unescaping in order to enable
   --  receiving arguments with embedded spaces or double quotes. To work
   --  around this asymmetry, Glib.Spawn.Spawn_Async_With_Pipes will do quoting
   --  and escaping on argument vector elements that need it before calling the
   --  C runtime spawn function.
   --  The returned Child_Pid on Windows is a handle to the child process, not
   --  its identifier. Process handles and process identifiers are different
   --  concepts on Windows.
   --  Envp is a null-terminated array of strings, where each string has the
   --  form `KEY=VALUE`. This will become the child's environment. If Envp is
   --  null, the child inherits its parent's environment.
   --  Flags should be the bitwise OR of any flags you want to affect the
   --  function's behaviour. The Glib.Spawn.G_Spawn_Do_Not_Reap_Child means
   --  that the child will not automatically be reaped; you must use a child
   --  watch to be notified about the death of the child process. Eventually
   --  you must call Glib.Spawn.Spawn_Close_Pid on the Child_Pid, in order to
   --  free resources which may be associated with the child process. (On Unix,
   --  using a child watch is equivalent to calling waitpid or handling the
   --  SIGCHLD signal manually. On Windows, calling Glib.Spawn.Spawn_Close_Pid
   --  is equivalent to calling CloseHandle on the process handle returned in
   --  Child_Pid). See g_child_watch_add.
   --  Glib.Spawn.G_Spawn_Leave_Descriptors_Open means that the parent's open
   --  file descriptors will be inherited by the child; otherwise all
   --  descriptors except stdin/stdout/stderr will be closed before calling
   --  exec in the child. Glib.Spawn.G_Spawn_Search_Path means that Argv[0]
   --  need not be an absolute path, it will be looked for in the `PATH`
   --  environment variable. Glib.Spawn.G_Spawn_Search_Path_From_Envp means
   --  need not be an absolute path, it will be looked for in the `PATH`
   --  variable from Envp. If both Glib.Spawn.G_Spawn_Search_Path and
   --  Glib.Spawn.G_Spawn_Search_Path_From_Envp are used, the value from Envp
   --  takes precedence over the environment.
   --  Glib.Spawn.G_Spawn_Stdout_To_Dev_Null means that the child's standard
   --  output will be discarded, instead of going to the same location as the
   --  parent's standard output. If you use this flag, Standard_Output must be
   --  null. Glib.Spawn.G_Spawn_Stderr_To_Dev_Null means that the child's
   --  standard error will be discarded, instead of going to the same location
   --  as the parent's standard error. If you use this flag, Standard_Error
   --  must be null. Glib.Spawn.G_Spawn_Child_Inherits_Stdin means that the
   --  child will inherit the parent's standard input (by default, the child's
   --  standard input is attached to /dev/null). If you use this flag,
   --  Standard_Input must be null. Glib.Spawn.G_Spawn_File_And_Argv_Zero means
   --  that the first element of Argv is the file to execute, while the
   --  remaining elements are the actual argument vector to pass to the file.
   --  Normally Glib.Spawn.Spawn_Async_With_Pipes uses Argv[0] as the file to
   --  execute, and passes all of Argv to the child.
   --  Child_Setup and User_Data are a function and user data. On POSIX
   --  platforms, the function is called in the child after GLib has performed
   --  all the setup it plans to perform (including creating pipes, closing
   --  file descriptors, etc.) but before calling exec. That is, Child_Setup is
   --  called just before calling exec in the child. Obviously actions taken in
   --  this function will only affect the child, not the parent.
   --  On Windows, there is no separate fork and exec functionality. Child
   --  processes are created and run with a single API call, CreateProcess.
   --  There is no sensible thing Child_Setup could be used for on Windows so
   --  it is ignored and not called.
   --  If non-null, Child_Pid will on Unix be filled with the child's process
   --  ID. You can use the process ID to send signals to the child, or to use
   --  g_child_watch_add (or waitpid) if you specified the
   --  Glib.Spawn.G_Spawn_Do_Not_Reap_Child flag. On Windows, Child_Pid will be
   --  filled with a handle to the child process only if you specified the
   --  Glib.Spawn.G_Spawn_Do_Not_Reap_Child flag. You can then access the child
   --  process using the Win32 API, for example wait for its termination with
   --  the WaitFor* functions, or examine its exit code with
   --  GetExitCodeProcess. You should close the handle with CloseHandle or
   --  Glib.Spawn.Spawn_Close_Pid when you no longer need it.
   --  If non-null, the Standard_Input, Standard_Output, Standard_Error
   --  locations will be filled with file descriptors for writing to the
   --  child's standard input or reading from its standard output or standard
   --  error. The caller of Glib.Spawn.Spawn_Async_With_Pipes must close these
   --  file descriptors when they are no longer in use. If these parameters are
   --  null, the corresponding pipe won't be created.
   --  If Standard_Input is NULL, the child's standard input is attached to
   --  /dev/null unless Glib.Spawn.G_Spawn_Child_Inherits_Stdin is set.
   --  If Standard_Error is NULL, the child's standard error goes to the same
   --  location as the parent's standard error unless
   --  Glib.Spawn.G_Spawn_Stderr_To_Dev_Null is set.
   --  If Standard_Output is NULL, the child's standard output goes to the
   --  same location as the parent's standard output unless
   --  Glib.Spawn.G_Spawn_Stdout_To_Dev_Null is set.
   --  Error can be null to ignore errors, or non-null to report errors. If an
   --  error is set, the function returns False. Errors are reported even if
   --  they occur in the child (for example if the executable in Argv[0] is not
   --  found). Typically the `message` field of returned errors should be
   --  displayed to users. Possible errors are those from the G_SPAWN_ERROR
   --  domain.
   --  If an error occurs, Child_Pid, Standard_Input, Standard_Output, and
   --  Standard_Error will not be filled with valid values.
   --  If Child_Pid is not null and an error does not occur then the returned
   --  process reference must be closed using Glib.Spawn.Spawn_Close_Pid.
   --  If you are writing a GTK+ application, and the program you are spawning
   --  is a graphical application, too, then you may want to use
   --  gdk_spawn_on_screen_with_pipes instead to ensure that the spawned
   --  program opens its windows on the right screen.
   --  "working_directory": child's current working directory, or null to
   --  inherit parent's, in the GLib file name encoding
   --  "argv": child's argument vector, in the GLib file name encoding
   --  "envp": child's environment, or null to inherit parent's, in the GLib
   --  file name encoding
   --  "flags": flags from Glib.Spawn.GSpawn_Flags
   --  "child_setup": function to run in the child just before exec
   --  "child_pid": return location for child process ID, or null
   --  "standard_input": return location for file descriptor to write to
   --  child's stdin, or null
   --  "standard_output": return location for file descriptor to read child's
   --  stdout, or null
   --  "standard_error": return location for file descriptor to read child's
   --  stderr, or null

   generic
   type User_Data is limited private;
   function Generic_Spawn_Async_With_Fds
     (Working_Directory : Gtkada.Types.Chars_Ptr := Gtkada.Types.Null_Ptr;
      Argv              : access Gtkada.Types.Chars_Ptr_Array;
      Envp              : access Gtkada.Types.Chars_Ptr_Array;
      Flags             : GSpawn_Flags;
      Child_Setup       : access procedure
        (Data : access User_Data);
      Data              : access User_Data;
      Child_Pid         : access GPid;
      Stdin_Fd          : Glib.Gint;
      Stdout_Fd         : Glib.Gint;
      Stderr_Fd         : Glib.Gint;
      Error             : access Glib.Error.GError)
   return Glib.Gboolean;
   pragma Import
     (C, Generic_Spawn_Async_With_Fds, "gnat_spawn_async_with_fds");
   --  Identical to g_spawn_async_with_pipes() but instead of creating pipes
   --  for the stdin/stdout/stderr, you can pass existing file descriptors
   --  into this function through the stdin_fd , stdout_fd and
   --  stderr_fd parameters. The following flags also have their behaviour
   --  slightly tweaked as a result: G_SPAWN_STDOUT_TO_DEV_NULL means that
   --  the child's standard output will be discarded, instead of going to the
   --  same location as the parent's standard output. If you use this flag,
   --  standard_output must be -1. G_SPAWN_STDERR_TO_DEV_NULL means that the
   --  child's standard error will be discarded, instead of going to the same
   --  location as the parent's standard error. If you use this flag,
   --  standard_error must be -1. G_SPAWN_CHILD_INHERITS_STDIN means that the
   --  child will inherit the parent's standard input (by default, the child's
   --  standard input is attached to /dev/null). If you use this flag,
   --  standard_input must be -1.
   --  It is valid to pass the same fd in multiple parameters (e.g. you can
   --  pass a single fd for both stdout and stderr).
   --  Parameters
   --  "working_directory" child's current working directory, or NULL to
   --  inherit parent's, in the GLib file name encoding.
   --  "argv" child's argument vector, in the GLib file name encoding.
   --  "envp"  child's environment, or NULL to inherit parent's, in the GLib
   --  file name encoding.
   --  "flags" flags from GSpawnFlags
   --  "child_setup" function to run in the child just before exec().
   --  "user_data" user data for child_setup
   --  "child_pid" return location for child process ID, or NULL.
   --  "stdin_fd" file descriptor to use for child's stdin, or -1
   --  "stdout_fd" file descriptor to use for child's stdout, or -1
   --  "stderr_fd" file descriptor to use for child's stderr, or -1
   --  "error" return location for error
   --  Returns
   --  TRUE on success, FALSE if an error was set
   --  Since: 2.58

   generic
   type User_Data is limited private;
   function Generic_Spawn_Sync
     (Working_Directory : Gtkada.Types.Chars_Ptr := Gtkada.Types.Null_Ptr;
      Argv              : access Gtkada.Types.Chars_Ptr_Array;
      Envp              : access Gtkada.Types.Chars_Ptr_Array;
      Flags             : GSpawn_Flags;
      Child_Setup       : access procedure
        (Data : access User_Data);
      Data              : access User_Data;
      Child_Pid         : access GPid;
      Standard_Output   : access Gtkada.Types.Chars_Ptr_Array;
      Standard_Error    : access Gtkada.Types.Chars_Ptr_Array;
      Exit_Status       : access Glib.Gint;
      Error             : access Glib.Error.GError)
   return Glib.Gboolean;
   pragma Import (C, Generic_Spawn_Sync, "gnat_spawn_sync");
   --  Executes a child synchronously (waits for the child to exit before
   --  returning). All output from the child is stored in Standard_Output and
   --  Standard_Error, if those parameters are non-null. Note that you must set
   --  the Glib.Spawn.G_Spawn_Stdout_To_Dev_Null and
   --  Glib.Spawn.G_Spawn_Stderr_To_Dev_Null flags when passing null for
   --  Standard_Output and Standard_Error.
   --  If Exit_Status is non-null, the platform-specific exit status of the
   --  child is stored there; see the documentation of
   --  Glib.Spawn.Spawn_Check_Exit_Status for how to use and interpret this.
   --  Note that it is invalid to pass Glib.Spawn.G_Spawn_Do_Not_Reap_Child in
   --  Flags.
   --  If an error occurs, no data is returned in Standard_Output,
   --  Standard_Error, or Exit_Status.
   --  This function calls Glib.Spawn.Spawn_Async_With_Pipes internally; see
   --  that function for full details on the other parameters and details on
   --  how these functions work on Windows.
   --  "working_directory": child's current working directory, or null to
   --  inherit parent's
   --  "argv": child's argument vector
   --  "envp": child's environment, or null to inherit parent's
   --  "flags": flags from Glib.Spawn.GSpawn_Flags
   --  "child_setup": function to run in the child just before exec
   --  "standard_output": return location for child output, or null
   --  "standard_error": return location for child error messages, or null
   --  "exit_status": return location for child exit status, as returned by
   --  waitpid, or null

   generic
   type User_Data is limited private;
   function Generic_Spawn_Command_Line_Sync
     (Command_Line      : Gtkada.Types.Chars_Ptr;
      Standard_Output   : access Gtkada.Types.Chars_Ptr_Array;
      Standard_Error    : access Gtkada.Types.Chars_Ptr_Array;
      Exit_Status       : access Glib.Gint;
      Error             : access Glib.Error.GError)
   return Glib.Gboolean;
   pragma Import
     (C, Generic_Spawn_Command_Line_Sync, "gnat_spawn_command_line_sync");
   --  A simple version of g_spawn_sync() with little-used parameters removed,
   --  taking a command line instead of an argument vector. See g_spawn_sync()
   --  for full details. command_line will be parsed by g_shell_parse_argv().
   --  Unlike g_spawn_sync(), the G_SPAWN_SEARCH_PATH flag is enabled.
   --  Note that G_SPAWN_SEARCH_PATH can have security implications, so
   --  consider using g_spawn_sync() directly if appropriate. Possible errors
   --  are those from g_spawn_sync() and those from g_shell_parse_argv().
   --  If exit_status is non-NULL, the platform-specific exit status of the
   --  child is stored there; see the documentation of
   --  g_spawn_check_exit_status() for how to use and interpret this.
   --  On Windows, please note the implications of g_shell_parse_argv()
   --  parsing command_line . Parsing is done according to Unix shell rules,
   --  not Windows command interpreter rules. Space is a separator,
   --  and backslashes are special. Thus you cannot simply pass a
   --  command_line containing canonical Windows paths, like
   --  "c:\program files\app\app.exe", as the backslashes will be eaten,
   --  and the space will act as a separator. You need to enclose such paths
   --  with single quotes, like "'c:\program files\app\app.exe'
   --  'e:\folder\argument.txt'".
   --  Parameters
   --  "command_line" a command line.
   --  "standard_output" return location for child output.
   --  "standard_error" return location for child errors.
   --  "exit_status" return location for child exit status, as returned
   --  by waitpid().
   --  "error" return location for errors
   --  Returns
   --  TRUE on success, FALSE if an error was set

   function Spawn_Command_Line_Async
     (Command_Line : Gtkada.Types.Chars_Ptr;
      Error        : access Glib.Error.GError)
   return Boolean;
   --  A simple version of Glib.Spawn.Spawn_Async that parses a command line
   --  with g_shell_parse_argv and passes it to Glib.Spawn.Spawn_Async. Runs a
   --  command line in the background. Unlike Glib.Spawn.Spawn_Async, the
   --  Glib.Spawn.G_Spawn_Search_Path flag is enabled, other flags are not.
   --  Note that Glib.Spawn.G_Spawn_Search_Path can have security implications,
   --  so consider using Glib.Spawn.Spawn_Async directly if appropriate.
   --  Possible errors are those from g_shell_parse_argv and
   --  Glib.Spawn.Spawn_Async.
   --  The same concerns on Windows apply as for g_spawn_command_line_sync.
   --  "command_line": a command line

   function Spawn_Check_Exit_Status
     (Exit_Status : Glib.Gint;
      Error       : access Glib.Error.GError)
   return Boolean;
   --  Set Error if Exit_Status indicates the child exited abnormally (e.g.
   --  with a nonzero exit code, or via a fatal signal).
   --  The Glib.Spawn.Spawn_Sync and g_child_watch_add family of APIs return
   --  an exit status for subprocesses encoded in a platform-specific way. On
   --  Unix, this is guaranteed to be in the same format waitpid returns, and
   --  on Windows it is guaranteed to be the result of GetExitCodeProcess.
   --  Prior to the introduction of this function in GLib 2.34, interpreting
   --  Exit_Status required use of platform-specific APIs, which is problematic
   --  for software using GLib as a cross-platform layer.
   --  Additionally, many programs simply want to determine whether or not the
   --  child exited successfully, and either propagate a Gerror.Gerror or print
   --  a message to standard error. In that common case, this function can be
   --  used. Note that the error message in Error will contain human-readable
   --  information about the exit status.
   --  The Domain and Code of Error have special semantics in the case where
   --  the process has an "exit code", as opposed to being killed by a signal.
   --  On Unix, this happens if WIFEXITED would be true of Exit_Status. On
   --  Windows, it is always the case.
   --  The special semantics are that the actual exit code will be the code
   --  set in Error, and the domain will be G_SPAWN_EXIT_ERROR. This allows you
   --  to differentiate between different exit codes.
   --  If the process was terminated by some means other than an exit status,
   --  the domain will be G_SPAWN_ERROR, and the code will be
   --  Glib.Spawn.G_Spawn_Error_Failed.
   --  This function just offers convenience; you can of course also check the
   --  available platform via a macro such as G_OS_UNIX, and use WIFEXITED and
   --  WEXITSTATUS on Exit_Status directly. Do not attempt to scan or parse the
   --  error message string; it may be translated and/or change in future
   --  versions of GLib.
   --  Since: gtk+ 2.34
   --  "exit_status": An exit code as returned from Glib.Spawn.Spawn_Sync

   ---------------
   -- Functions --
   ---------------

   function Get_Environ return GNAT.Strings.String_List;
   --  Gets the list of environment variables for the current process.
   --  The list is null terminated and each item in the list is of the form
   --  'NAME=VALUE'.
   --  This is equivalent to direct access to the 'environ' global variable,
   --  except portable.
   --  The return value is freshly allocated and it should be freed with
   --  g_strfreev when it is no longer needed.
   --  Since: gtk+ 2.28

   procedure Spawn_Close_Pid (Pid : GPid);
   --  On some platforms, notably Windows, the GPid type represents a resource
   --  which must be closed to prevent resource leaking.
   --  Glib.Spawn.Spawn_Close_Pid is provided for this purpose. It should be
   --  used on all platforms, even though it doesn't do anything under UNIX.
   --  "pid": The process reference to close

end Glib.Spawn;
