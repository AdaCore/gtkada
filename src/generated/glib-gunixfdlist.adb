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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Error;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Interfaces.C.Pointers;

use type Glib.Error.GError;

package body Glib.GUnixFDList is

   use Interfaces.C;
   package P is new Interfaces.C.Pointers
     (Natural, Gint, Gint_Array, -1);

   package Type_Conversion_Gunix_Fdlist is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gunix_Fdlist_Record);
   pragma Unreferenced (Type_Conversion_Gunix_Fdlist);

   -----------
   -- G_New --
   -----------

   procedure G_New (Self : out Gunix_Fdlist) is
   begin
      Self := new Gunix_Fdlist_Record;
      Glib.GUnixFDList.Initialize (Self);
   end G_New;

   ----------------------
   -- G_New_From_Array --
   ----------------------

   procedure G_New_From_Array
      (Self  : out Gunix_Fdlist;
       Fds   : Gint_Array;
       N_Fds : Glib.Gint)
   is
   begin
      Self := new Gunix_Fdlist_Record;
      Glib.GUnixFDList.Initialize_From_Array (Self, Fds, N_Fds);
   end G_New_From_Array;

   ----------------------
   -- Gunix_Fdlist_New --
   ----------------------

   function Gunix_Fdlist_New return Gunix_Fdlist is
      Self : constant Gunix_Fdlist := new Gunix_Fdlist_Record;
   begin
      Glib.GUnixFDList.Initialize (Self);
      return Self;
   end Gunix_Fdlist_New;

   ---------------------------------
   -- Gunix_Fdlist_New_From_Array --
   ---------------------------------

   function Gunix_Fdlist_New_From_Array
      (Fds   : Gint_Array;
       N_Fds : Glib.Gint) return Gunix_Fdlist
   is
      Self : constant Gunix_Fdlist := new Gunix_Fdlist_Record;
   begin
      Glib.GUnixFDList.Initialize_From_Array (Self, Fds, N_Fds);
      return Self;
   end Gunix_Fdlist_New_From_Array;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gunix_Fdlist_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "g_unix_fd_list_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ---------------------------
   -- Initialize_From_Array --
   ---------------------------

   procedure Initialize_From_Array
      (Self  : not null access Gunix_Fdlist_Record'Class;
       Fds   : Gint_Array;
       N_Fds : Glib.Gint)
   is
      function Internal
         (Fds   : System.Address;
          N_Fds : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "g_unix_fd_list_new_from_array");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Fds'Address, N_Fds));
      end if;
   end Initialize_From_Array;

   ------------
   -- Append --
   ------------

   function Append
      (Self  : not null access Gunix_Fdlist_Record;
       Fd    : Glib.Gint;
       Error : out Glib.Error.GError) return Glib.Gint
   is
      function Internal
         (Self      : System.Address;
          Fd        : Glib.Gint;
          Acc_Error : access Glib.Error.GError) return Glib.Gint;
      pragma Import (C, Internal, "g_unix_fd_list_append");
      Acc_Error  : aliased Glib.Error.GError;
      Return_Obj : Glib.Gint := -1;
      Tmp_Return : Glib.Gint;
   begin
      Tmp_Return := Internal (Get_Object (Self), Fd, Acc_Error'Access);
      Error := Acc_Error;
      if Error = null then
         Return_Obj := Tmp_Return;
      end if;
      return Return_Obj;
   end Append;

   ---------
   -- Get --
   ---------

   function Get
      (Self  : not null access Gunix_Fdlist_Record;
       Index : Glib.Gint;
       Error : out Glib.Error.GError) return Glib.Gint
   is
      function Internal
         (Self      : System.Address;
          Index     : Glib.Gint;
          Acc_Error : access Glib.Error.GError) return Glib.Gint;
      pragma Import (C, Internal, "g_unix_fd_list_get");
      Acc_Error  : aliased Glib.Error.GError;
      Return_Obj : Glib.Gint := -1;
      Tmp_Return : Glib.Gint;
   begin
      Tmp_Return := Internal (Get_Object (Self), Index, Acc_Error'Access);
      Error := Acc_Error;
      if Error = null then
         Return_Obj := Tmp_Return;
      end if;
      return Return_Obj;
   end Get;

   ----------------
   -- Get_Length --
   ----------------

   function Get_Length
      (Self : not null access Gunix_Fdlist_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "g_unix_fd_list_get_length");
   begin
      return Internal (Get_Object (Self));
   end Get_Length;

   --------------
   -- Peek_Fds --
   --------------

   function Peek_Fds
      (Self : not null access Gunix_Fdlist_Record) return Gint_Array
   is
      function Internal
         (Self   : System.Address;
          Length : access Glib.Gint) return P.Pointer;
      pragma Import (C, Internal, "g_unix_fd_list_peek_fds");
      Acc_Length : aliased Glib.Gint;
      Acc_Result : P.Pointer := null;
   begin
      Acc_Result := Internal (Get_Object (Self), Acc_Length'Access);
      declare
         Result : constant GLib.Gint_Array :=
           P.Value (Acc_Result, Interfaces.C.ptrdiff_t (Acc_Length));
      begin
         return Result;
      end;
   end Peek_Fds;

   ---------------
   -- Steal_Fds --
   ---------------

   function Steal_Fds
      (Self : not null access Gunix_Fdlist_Record) return Gint_Array
   is
      function Internal
         (Self   : System.Address;
          Length : access Glib.Gint) return P.Pointer;
      pragma Import (C, Internal, "g_unix_fd_list_steal_fds");
      Acc_Length : aliased Glib.Gint;
      Acc_Result : P.Pointer := null;
   begin
      Acc_Result := Internal (Get_Object (Self), Acc_Length'Access);
      declare
         Result : constant GLib.Gint_Array :=
           P.Value (Acc_Result, Interfaces.C.ptrdiff_t (Acc_Length));
      begin
         Glib.g_free (Acc_Result.all'Address); --  transfer-ownership="full"
         return Result;
      end;
   end Steal_Fds;

end Glib.GUnixFDList;
