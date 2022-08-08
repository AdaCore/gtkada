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
with Ada.Unchecked_Conversion;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Gtkada.Bindings;            use Gtkada.Bindings;

package body Gtk.File_Filter is

   function From_Object_Free (B : access Gtk_File_Filter_Info) return Gtk_File_Filter_Info is
      Result : constant Gtk_File_Filter_Info := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   procedure C_Gtk_File_Filter_Add_Custom
      (Self   : System.Address;
       Needed : Gtk_File_Filter_Flags;
       Func   : System.Address;
       Data   : System.Address;
       Notify : System.Address);
   pragma Import (C, C_Gtk_File_Filter_Add_Custom, "gtk_file_filter_add_custom");
   --  Adds rule to a filter that allows files based on a custom callback
   --  function. The bitfield Needed which is passed in provides information
   --  about what sorts of information that the filter function needs; this
   --  allows GTK+ to avoid retrieving expensive information when it isn't
   --  needed by the filter.
   --  Since: gtk+ 2.4
   --  "needed": bitfield of flags indicating the information that the custom
   --  filter function needs.
   --  "func": callback function; if the function returns True, then the file
   --  will be displayed.
   --  "data": data to pass to Func
   --  "notify": function to call to free Data when it is no longer needed.

   function To_Gtk_File_Filter_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_File_Filter_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_File_Filter_Func, System.Address);

   function Internal_Gtk_File_Filter_Func
      (Filter_Info : access Gtk.File_Filter.Gtk_File_Filter_Info;
       Data        : System.Address) return Glib.Gboolean;
   pragma Convention (C, Internal_Gtk_File_Filter_Func);
   --  "filter_info": a Gtk.File_Filter.Gtk_File_Filter_Info that is filled
   --  according to the Needed flags passed to Gtk.File_Filter.Add_Custom
   --  "data": user data passed to Gtk.File_Filter.Add_Custom

   -----------------------------------
   -- Internal_Gtk_File_Filter_Func --
   -----------------------------------

   function Internal_Gtk_File_Filter_Func
      (Filter_Info : access Gtk.File_Filter.Gtk_File_Filter_Info;
       Data        : System.Address) return Glib.Gboolean
   is
      Func : constant Gtk_File_Filter_Func := To_Gtk_File_Filter_Func (Data);
   begin
      return Boolean'Pos (Func (Filter_Info.all));
   end Internal_Gtk_File_Filter_Func;

   package Type_Conversion_Gtk_File_Filter is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_File_Filter_Record);
   pragma Unreferenced (Type_Conversion_Gtk_File_Filter);

   -------------------------
   -- Gtk_File_Filter_New --
   -------------------------

   function Gtk_File_Filter_New return Gtk_File_Filter is
      Self : constant Gtk_File_Filter := new Gtk_File_Filter_Record;
   begin
      Gtk.File_Filter.Initialize (Self);
      return Self;
   end Gtk_File_Filter_New;

   ---------------------------------------
   -- Gtk_File_Filter_New_From_Gvariant --
   ---------------------------------------

   function Gtk_File_Filter_New_From_Gvariant
      (Variant : Glib.Variant.Gvariant) return Gtk_File_Filter
   is
      Self : constant Gtk_File_Filter := new Gtk_File_Filter_Record;
   begin
      Gtk.File_Filter.Initialize_From_Gvariant (Self, Variant);
      return Self;
   end Gtk_File_Filter_New_From_Gvariant;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_File_Filter) is
   begin
      Self := new Gtk_File_Filter_Record;
      Gtk.File_Filter.Initialize (Self);
   end Gtk_New;

   ---------------------------
   -- Gtk_New_From_Gvariant --
   ---------------------------

   procedure Gtk_New_From_Gvariant
      (Self    : out Gtk_File_Filter;
       Variant : Glib.Variant.Gvariant)
   is
   begin
      Self := new Gtk_File_Filter_Record;
      Gtk.File_Filter.Initialize_From_Gvariant (Self, Variant);
   end Gtk_New_From_Gvariant;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_File_Filter_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_file_filter_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ------------------------------
   -- Initialize_From_Gvariant --
   ------------------------------

   procedure Initialize_From_Gvariant
      (Self    : not null access Gtk_File_Filter_Record'Class;
       Variant : Glib.Variant.Gvariant)
   is
      function Internal (Variant : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_file_filter_new_from_gvariant");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Variant)));
      end if;
   end Initialize_From_Gvariant;

   ----------------
   -- Add_Custom --
   ----------------

   procedure Add_Custom
      (Self   : not null access Gtk_File_Filter_Record;
       Needed : Gtk_File_Filter_Flags;
       Func   : Gtk_File_Filter_Func)
   is
   begin
      if Func = null then
         C_Gtk_File_Filter_Add_Custom (Get_Object (Self), Needed, System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_File_Filter_Add_Custom (Get_Object (Self), Needed, Internal_Gtk_File_Filter_Func'Address, To_Address (Func), System.Null_Address);
      end if;
   end Add_Custom;

   package body Add_Custom_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_File_Filter_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_File_Filter_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_File_Filter_Func, System.Address);

      function Internal_Cb
         (Filter_Info : access Gtk.File_Filter.Gtk_File_Filter_Info;
          Data        : System.Address) return Glib.Gboolean;
      pragma Convention (C, Internal_Cb);
      --  The type of function that is used with custom filters, see
      --  Gtk.File_Filter.Add_Custom.
      --  "filter_info": a Gtk.File_Filter.Gtk_File_Filter_Info that is filled
      --  according to the Needed flags passed to Gtk.File_Filter.Add_Custom
      --  "data": user data passed to Gtk.File_Filter.Add_Custom

      ----------------
      -- Add_Custom --
      ----------------

      procedure Add_Custom
         (Self   : not null access Gtk.File_Filter.Gtk_File_Filter_Record'Class;
          Needed : Gtk.File_Filter.Gtk_File_Filter_Flags;
          Func   : Gtk_File_Filter_Func;
          Data   : User_Data_Type)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_File_Filter_Add_Custom (Get_Object (Self), Needed, System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Func), Data);
            C_Gtk_File_Filter_Add_Custom (Get_Object (Self), Needed, Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Add_Custom;

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Filter_Info : access Gtk.File_Filter.Gtk_File_Filter_Info;
          Data        : System.Address) return Glib.Gboolean
      is
         D : constant Users.Internal_Data_Access := Users.Convert (Data);
      begin
         return Boolean'Pos (To_Gtk_File_Filter_Func (D.Func) (Filter_Info.all, D.Data.all));
      end Internal_Cb;

   end Add_Custom_User_Data;

   -------------------
   -- Add_Mime_Type --
   -------------------

   procedure Add_Mime_Type
      (Self      : not null access Gtk_File_Filter_Record;
       Mime_Type : UTF8_String)
   is
      procedure Internal
         (Self      : System.Address;
          Mime_Type : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_file_filter_add_mime_type");
      Tmp_Mime_Type : Gtkada.Types.Chars_Ptr := New_String (Mime_Type);
   begin
      Internal (Get_Object (Self), Tmp_Mime_Type);
      Free (Tmp_Mime_Type);
   end Add_Mime_Type;

   -----------------
   -- Add_Pattern --
   -----------------

   procedure Add_Pattern
      (Self    : not null access Gtk_File_Filter_Record;
       Pattern : UTF8_String)
   is
      procedure Internal
         (Self    : System.Address;
          Pattern : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_file_filter_add_pattern");
      Tmp_Pattern : Gtkada.Types.Chars_Ptr := New_String (Pattern);
   begin
      Internal (Get_Object (Self), Tmp_Pattern);
      Free (Tmp_Pattern);
   end Add_Pattern;

   ------------------------
   -- Add_Pixbuf_Formats --
   ------------------------

   procedure Add_Pixbuf_Formats
      (Self : not null access Gtk_File_Filter_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_file_filter_add_pixbuf_formats");
   begin
      Internal (Get_Object (Self));
   end Add_Pixbuf_Formats;

   ------------
   -- Filter --
   ------------

   function Filter
      (Self        : not null access Gtk_File_Filter_Record;
       Filter_Info : Gtk_File_Filter_Info) return Boolean
   is
      function Internal
         (Self        : System.Address;
          Filter_Info : Gtk_File_Filter_Info) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_file_filter_filter");
   begin
      return Internal (Get_Object (Self), Filter_Info) /= 0;
   end Filter;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
      (Self : not null access Gtk_File_Filter_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_file_filter_get_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Name;

   ----------------
   -- Get_Needed --
   ----------------

   function Get_Needed
      (Self : not null access Gtk_File_Filter_Record)
       return Gtk_File_Filter_Flags
   is
      function Internal (Self : System.Address) return Gtk_File_Filter_Flags;
      pragma Import (C, Internal, "gtk_file_filter_get_needed");
   begin
      return Internal (Get_Object (Self));
   end Get_Needed;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
      (Self : not null access Gtk_File_Filter_Record;
       Name : UTF8_String := "")
   is
      procedure Internal
         (Self : System.Address;
          Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_file_filter_set_name");
      Tmp_Name : Gtkada.Types.Chars_Ptr;
   begin
      if Name = "" then
         Tmp_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Name := New_String (Name);
      end if;
      Internal (Get_Object (Self), Tmp_Name);
      Free (Tmp_Name);
   end Set_Name;

   -----------------
   -- To_Gvariant --
   -----------------

   function To_Gvariant
      (Self : not null access Gtk_File_Filter_Record)
       return Glib.Variant.Gvariant
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_file_filter_to_gvariant");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end To_Gvariant;

end Gtk.File_Filter;
