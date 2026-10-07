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

package body Gtk.Bitset_Iter is

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Self : access Gtk_Bitset_Iter) return Boolean is
      function Internal (Self : access Gtk_Bitset_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_bitset_iter_is_valid");
   begin
      return Internal (Self) /= 0;
   end Is_Valid;

   ----------
   -- Next --
   ----------

   function Next
      (Self  : access Gtk_Bitset_Iter;
       Value : access Guint := null) return Boolean
   is
      function Internal
         (Self  : access Gtk_Bitset_Iter;
          Value : access Guint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_bitset_iter_next");
   begin
      return Internal (Self, Value) /= 0;
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous
      (Self  : access Gtk_Bitset_Iter;
       Value : access Guint := null) return Boolean
   is
      function Internal
         (Self  : access Gtk_Bitset_Iter;
          Value : access Guint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_bitset_iter_previous");
   begin
      return Internal (Self, Value) /= 0;
   end Previous;

   ----------------------
   -- From_Object_Free --
   ----------------------

   function From_Object_Free
     (B : not null access Gtk_Bitset_Iter) return Gtk_Bitset_Iter
   is
      Result : constant Gtk_Bitset_Iter := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   -------------
   -- Init_At --
   -------------

   function Init_At
      (Iter   : out Gtk_Bitset_Iter;
       Set    : Gtk.Bitset.Gtk_Bitset;
       Target : Guint;
       Value  : access Guint := null) return Boolean
   is
      function Internal
         (Acc_Iter : access Gtk_Bitset_Iter;
          Set      : System.Address;
          Target   : Guint;
          Value    : access Guint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_bitset_iter_init_at");
      Acc_Iter     : aliased Gtk_Bitset_Iter;
      Tmp_Acc_Iter : aliased Gtk_Bitset_Iter;
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Tmp_Acc_Iter'Access, Get_Object (Set), Target, Value);
      Acc_Iter := Tmp_Acc_Iter;
      Iter := Acc_Iter;
      return Tmp_Return /= 0;
   end Init_At;

   ----------------
   -- Init_First --
   ----------------

   function Init_First
      (Iter  : out Gtk_Bitset_Iter;
       Set   : Gtk.Bitset.Gtk_Bitset;
       Value : access Guint := null) return Boolean
   is
      function Internal
         (Acc_Iter : access Gtk_Bitset_Iter;
          Set      : System.Address;
          Value    : access Guint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_bitset_iter_init_first");
      Acc_Iter     : aliased Gtk_Bitset_Iter;
      Tmp_Acc_Iter : aliased Gtk_Bitset_Iter;
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Tmp_Acc_Iter'Access, Get_Object (Set), Value);
      Acc_Iter := Tmp_Acc_Iter;
      Iter := Acc_Iter;
      return Tmp_Return /= 0;
   end Init_First;

   ---------------
   -- Init_Last --
   ---------------

   function Init_Last
      (Iter  : out Gtk_Bitset_Iter;
       Set   : Gtk.Bitset.Gtk_Bitset;
       Value : access Guint := null) return Boolean
   is
      function Internal
         (Acc_Iter : access Gtk_Bitset_Iter;
          Set      : System.Address;
          Value    : access Guint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_bitset_iter_init_last");
      Acc_Iter     : aliased Gtk_Bitset_Iter;
      Tmp_Acc_Iter : aliased Gtk_Bitset_Iter;
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Tmp_Acc_Iter'Access, Get_Object (Set), Value);
      Acc_Iter := Tmp_Acc_Iter;
      Iter := Acc_Iter;
      return Tmp_Return /= 0;
   end Init_Last;

end Gtk.Bitset_Iter;
