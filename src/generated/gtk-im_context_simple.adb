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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.IM_Context_Simple is

   package Type_Conversion_Gtk_IM_Context_Simple is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_IM_Context_Simple_Record);
   pragma Unreferenced (Type_Conversion_Gtk_IM_Context_Simple);

   -------------------------------
   -- Gtk_IM_Context_Simple_New --
   -------------------------------

   function Gtk_IM_Context_Simple_New return Gtk_IM_Context_Simple is
      Self : constant Gtk_IM_Context_Simple := new Gtk_IM_Context_Simple_Record;
   begin
      Gtk.IM_Context_Simple.Initialize (Self);
      return Self;
   end Gtk_IM_Context_Simple_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_IM_Context_Simple) is
   begin
      Self := new Gtk_IM_Context_Simple_Record;
      Gtk.IM_Context_Simple.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_IM_Context_Simple_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_im_context_simple_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ----------------------
   -- Add_Compose_File --
   ----------------------

   procedure Add_Compose_File
      (Self         : not null access Gtk_IM_Context_Simple_Record;
       Compose_File : UTF8_String)
   is
      procedure Internal
         (Self         : System.Address;
          Compose_File : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_im_context_simple_add_compose_file");
      Tmp_Compose_File : Gtkada.Types.Chars_Ptr := New_String (Compose_File);
   begin
      Internal (Get_Object (Self), Tmp_Compose_File);
      Free (Tmp_Compose_File);
   end Add_Compose_File;

   ---------------
   -- Add_Table --
   ---------------

   procedure Add_Table
      (Self        : not null access Gtk_IM_Context_Simple_Record;
       Data        : Gint_Array;
       Max_Seq_Len : Glib.Gint;
       N_Seqs      : Glib.Gint)
   is
      procedure Internal
         (Self        : System.Address;
          Data        : System.Address;
          Max_Seq_Len : Glib.Gint;
          N_Seqs      : Glib.Gint);
      pragma Import (C, Internal, "gtk_im_context_simple_add_table");
   begin
      Internal (Get_Object (Self), Data (Data'First)'Address, Max_Seq_Len, N_Seqs);
   end Add_Table;

end Gtk.IM_Context_Simple;
