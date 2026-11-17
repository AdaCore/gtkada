------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  A Glib.GUnixFDList.Gunix_Fdlist contains a list of file descriptors. It
--  owns the file descriptors that it contains, closing them when finalized.
--
--  It may be wrapped in a Gunix.Fdmessage.Gunix_Fdmessage and sent over a
--  Gsocket.Gsocket in the G_SOCKET_FAMILY_UNIX family by using
--  g_socket_send_message and received using g_socket_receive_message.
--
--  Before 2.74, `<gio/gunixfdlist.h>` belonged to the UNIX-specific GIO
--  interfaces, thus you had to use the `gio-unix-2.0.pc` pkg-config file when
--  using it.
--
--  Since 2.74, the API is available for Windows.

pragma Warnings (Off, "*is already use-visible*");
with Glib.Error;  use Glib.Error;
with Glib.Object; use Glib.Object;

package Glib.GUnixFDList is

   type Gunix_Fdlist_Record is new GObject_Record with null record;
   type Gunix_Fdlist is access all Gunix_Fdlist_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure G_New (Self : out Gunix_Fdlist);
   --  Creates a new Glib.GUnixFDList.Gunix_Fdlist containing no file
   --  descriptors.
   --  Since: gtk+ 2.24

   procedure Initialize (Self : not null access Gunix_Fdlist_Record'Class);
   --  Creates a new Glib.GUnixFDList.Gunix_Fdlist containing no file
   --  descriptors.
   --  Since: gtk+ 2.24
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gunix_Fdlist_New return Gunix_Fdlist;
   --  Creates a new Glib.GUnixFDList.Gunix_Fdlist containing no file
   --  descriptors.
   --  Since: gtk+ 2.24

   procedure G_New_From_Array
      (Self  : out Gunix_Fdlist;
       Fds   : Gint_Array;
       N_Fds : Glib.Gint);
   --  Creates a new Glib.GUnixFDList.Gunix_Fdlist containing the file
   --  descriptors given in Fds. The file descriptors become the property of
   --  the new list and may no longer be used by the caller. The array itself
   --  is owned by the caller.
   --  Each file descriptor in the array should be set to close-on-exec.
   --  If N_Fds is -1 then Fds must be terminated with -1.
   --  Since: gtk+ 2.24
   --  @param Fds the initial list of file descriptors
   --  @param N_Fds the length of fds, or -1

   procedure Initialize_From_Array
      (Self  : not null access Gunix_Fdlist_Record'Class;
       Fds   : Gint_Array;
       N_Fds : Glib.Gint);
   --  Creates a new Glib.GUnixFDList.Gunix_Fdlist containing the file
   --  descriptors given in Fds. The file descriptors become the property of
   --  the new list and may no longer be used by the caller. The array itself
   --  is owned by the caller.
   --  Each file descriptor in the array should be set to close-on-exec.
   --  If N_Fds is -1 then Fds must be terminated with -1.
   --  Since: gtk+ 2.24
   --  Initialize_From_Array does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Fds the initial list of file descriptors
   --  @param N_Fds the length of fds, or -1

   function Gunix_Fdlist_New_From_Array
      (Fds   : Gint_Array;
       N_Fds : Glib.Gint) return Gunix_Fdlist;
   --  Creates a new Glib.GUnixFDList.Gunix_Fdlist containing the file
   --  descriptors given in Fds. The file descriptors become the property of
   --  the new list and may no longer be used by the caller. The array itself
   --  is owned by the caller.
   --  Each file descriptor in the array should be set to close-on-exec.
   --  If N_Fds is -1 then Fds must be terminated with -1.
   --  Since: gtk+ 2.24
   --  @param Fds the initial list of file descriptors
   --  @param N_Fds the length of fds, or -1

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_unix_fd_list_get_type");

   -------------
   -- Methods --
   -------------

   function Append
      (Self  : not null access Gunix_Fdlist_Record;
       Fd    : Glib.Gint;
       Error : out Glib.Error.GError) return Glib.Gint;
   --  Adds a file descriptor to List.
   --  The file descriptor is duplicated using dup. You keep your copy of the
   --  descriptor and the copy contained in List will be closed when List is
   --  finalized.
   --  A possible cause of failure is exceeding the per-process or system-wide
   --  file descriptor limit.
   --  The index of the file descriptor in the list is returned. If you use
   --  this index with Glib.GUnixFDList.Get then you will receive back a
   --  duplicated copy of the same file descriptor.
   --  Since: gtk+ 2.24
   --  @param Fd a valid open file descriptor
   --  @param Error the return location for a recoverable error
   --  @return the index of the appended fd in case of success, else -1 (and
   --  Error is set)

   function Get
      (Self  : not null access Gunix_Fdlist_Record;
       Index : Glib.Gint;
       Error : out Glib.Error.GError) return Glib.Gint;
   --  Gets a file descriptor out of List.
   --  Index_ specifies the index of the file descriptor to get. It is a
   --  programmer error for Index_ to be out of range; see
   --  Glib.GUnixFDList.Get_Length.
   --  The file descriptor is duplicated using dup and set as close-on-exec
   --  before being returned. You must call close on it when you are done.
   --  A possible cause of failure is exceeding the per-process or system-wide
   --  file descriptor limit.
   --  Since: gtk+ 2.24
   --  @param Index the index into the list
   --  @param Error the return location for a recoverable error
   --  @return the file descriptor, or -1 in case of error

   function Get_Length
      (Self : not null access Gunix_Fdlist_Record) return Glib.Gint;
   --  Gets the length of List (ie: the number of file descriptors contained
   --  within).
   --  Since: gtk+ 2.24
   --  @return the length of List

   function Peek_Fds
      (Self : not null access Gunix_Fdlist_Record) return Gint_Array;
   --  Returns the array of file descriptors that is contained in this object.
   --  After this call, the descriptors remain the property of List. The caller
   --  must not close them. The array is valid only until List is changed in
   --  any way. In case there are no file descriptors contained in List, an
   --  empty array is returned.
   --  Since: gtk+ 2.24
   --  @return an array of file descriptors

   function Steal_Fds
      (Self : not null access Gunix_Fdlist_Record) return Gint_Array;
   --  Returns the array of file descriptors that is contained in this object.
   --  After this call, the descriptors are no longer contained in List.
   --  Further calls will return an empty list (unless more descriptors have
   --  been added). The caller is also responsible for closing all of the file
   --  descriptors. The file descriptors in the array are set to close-on-exec.
   --  If Length is non-null then it is set to the number of file descriptors
   --  in the returned array. The returned array is also terminated with -1.
   --  This function never returns null. In case there are no file descriptors
   --  contained in List, an empty array is returned.
   --  Since: gtk+ 2.24
   --  @return an array of file descriptors

end Glib.GUnixFDList;
