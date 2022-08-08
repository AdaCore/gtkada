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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");

package body Gtk.Target_List is

   function From_Object_Free
     (B : access Gtk_Target_List'Class) return Gtk_Target_List
   is
      Result : constant Gtk_Target_List := Gtk_Target_List (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gtk_Target_List is
      S : Gtk_Target_List;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (List   : out Gtk_Target_List;
      Targets : Target_Entry_Array)
   is
      function Internal
        (Targets  : System.Address;
         Ntargets : Guint) return System.Address;
      pragma Import (C, Internal, "gtk_target_list_new");
   begin
      List.Set_Object (Internal (Targets'Address, Targets'Length));
   end Gtk_New;

   ---------------
   -- Add_Table --
   ---------------

   procedure Add_Table
     (List    : Gtk_Target_List;
      Targets : Target_Entry_Array)
   is
      procedure Internal
        (List     : System.Address;
         Targets  : System.Address;
         Ntargets : Guint);
      pragma Import (C, Internal, "gtk_target_list_add_table");
   begin
      Internal (Get_Object (List), Targets'Address, Targets'Length);
   end Add_Table;

   ---------
   -- Add --
   ---------

   procedure Add
      (List   : Gtk_Target_List;
       Target : Gdk.Types.Gdk_Atom;
       Flags  : Guint;
       Info   : Guint)
   is
      procedure Internal
         (List   : System.Address;
          Target : Gdk.Types.Gdk_Atom;
          Flags  : Guint;
          Info   : Guint);
      pragma Import (C, Internal, "gtk_target_list_add");
   begin
      Internal (Get_Object (List), Target, Flags, Info);
   end Add;

   -----------------------
   -- Add_Image_Targets --
   -----------------------

   procedure Add_Image_Targets
      (List     : Gtk_Target_List;
       Info     : Guint;
       Writable : Boolean)
   is
      procedure Internal
         (List     : System.Address;
          Info     : Guint;
          Writable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_target_list_add_image_targets");
   begin
      Internal (Get_Object (List), Info, Boolean'Pos (Writable));
   end Add_Image_Targets;

   ----------------------
   -- Add_Text_Targets --
   ----------------------

   procedure Add_Text_Targets (List : Gtk_Target_List; Info : Guint) is
      procedure Internal (List : System.Address; Info : Guint);
      pragma Import (C, Internal, "gtk_target_list_add_text_targets");
   begin
      Internal (Get_Object (List), Info);
   end Add_Text_Targets;

   ---------------------
   -- Add_Uri_Targets --
   ---------------------

   procedure Add_Uri_Targets (List : Gtk_Target_List; Info : Guint) is
      procedure Internal (List : System.Address; Info : Guint);
      pragma Import (C, Internal, "gtk_target_list_add_uri_targets");
   begin
      Internal (Get_Object (List), Info);
   end Add_Uri_Targets;

   ----------
   -- Find --
   ----------

   procedure Find
      (List   : Gtk_Target_List;
       Target : Gdk.Types.Gdk_Atom;
       Info   : out Guint;
       Found  : out Boolean)
   is
      function Internal
         (List     : System.Address;
          Target   : Gdk.Types.Gdk_Atom;
          Acc_Info : access Guint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_target_list_find");
      Acc_Info   : aliased Guint;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (List), Target, Acc_Info'Access);
      Info := Acc_Info;
      Found := Tmp_Return /= 0;
   end Find;

   ---------
   -- Ref --
   ---------

   function Ref (List : Gtk_Target_List) return Gtk_Target_List is
      function Internal (List : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_target_list_ref");
   begin
      return From_Object (Internal (Get_Object (List)));
   end Ref;

   ------------
   -- Remove --
   ------------

   procedure Remove (List : Gtk_Target_List; Target : Gdk.Types.Gdk_Atom) is
      procedure Internal
         (List   : System.Address;
          Target : Gdk.Types.Gdk_Atom);
      pragma Import (C, Internal, "gtk_target_list_remove");
   begin
      Internal (Get_Object (List), Target);
   end Remove;

   -----------
   -- Unref --
   -----------

   procedure Unref (List : Gtk_Target_List) is
      procedure Internal (List : System.Address);
      pragma Import (C, Internal, "gtk_target_list_unref");
   begin
      Internal (Get_Object (List));
   end Unref;

end Gtk.Target_List;
