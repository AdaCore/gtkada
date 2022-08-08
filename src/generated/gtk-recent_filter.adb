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

package body Gtk.Recent_Filter is

   function From_Object_Free (B : access Gtk_Recent_Filter_Info) return Gtk_Recent_Filter_Info is
      Result : constant Gtk_Recent_Filter_Info := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function Convert (R : Gtk.Recent_Filter.Gtk_Recent_Filter) return System.Address is
   begin
      return Get_Object (R);
   end Convert;

   function Convert (R : System.Address) return Gtk.Recent_Filter.Gtk_Recent_Filter is
      Stub : Gtk.Recent_Filter.Gtk_Recent_Filter_Record;begin
         return Gtk.Recent_Filter.Gtk_Recent_Filter (Glib.Object.Get_User_Data (R, Stub));
      end Convert;

   procedure C_Gtk_Recent_Filter_Add_Custom
      (Filter       : System.Address;
       Needed       : Gtk_Recent_Filter_Flags;
       Func         : System.Address;
       Data         : System.Address;
       Data_Destroy : Glib.G_Destroy_Notify_Address);
   pragma Import (C, C_Gtk_Recent_Filter_Add_Custom, "gtk_recent_filter_add_custom");
   --  Adds a rule to a filter that allows resources based on a custom
   --  callback function. The bitfield Needed which is passed in provides
   --  information about what sorts of information that the filter function
   --  needs; this allows GTK+ to avoid retrieving expensive information when
   --  it isn't needed by the filter.
   --  Since: gtk+ 2.10
   --  "needed": bitfield of flags indicating the information that the custom
   --  filter function needs.
   --  "func": callback function; if the function returns True, then the file
   --  will be displayed.
   --  "data": data to pass to Func
   --  "data_destroy": function to call to free Data when it is no longer
   --  needed.

   function To_Gtk_Recent_Filter_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Recent_Filter_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Recent_Filter_Func, System.Address);

   function Internal_Gtk_Recent_Filter_Func
      (Filter_Info : access Gtk.Recent_Filter.Gtk_Recent_Filter_Info;
       User_Data   : System.Address) return Glib.Gboolean;
   pragma Convention (C, Internal_Gtk_Recent_Filter_Func);
   --  "filter_info": a Gtk.Recent_Filter.Gtk_Recent_Filter_Info that is
   --  filled according to the Needed flags passed to
   --  Gtk.Recent_Filter.Add_Custom
   --  "user_data": user data passed to Gtk.Recent_Filter.Add_Custom

   -------------------------------------
   -- Internal_Gtk_Recent_Filter_Func --
   -------------------------------------

   function Internal_Gtk_Recent_Filter_Func
      (Filter_Info : access Gtk.Recent_Filter.Gtk_Recent_Filter_Info;
       User_Data   : System.Address) return Glib.Gboolean
   is
      Func : constant Gtk_Recent_Filter_Func := To_Gtk_Recent_Filter_Func (User_Data);
   begin
      return Boolean'Pos (Func (Filter_Info.all));
   end Internal_Gtk_Recent_Filter_Func;

   package Type_Conversion_Gtk_Recent_Filter is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Recent_Filter_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Recent_Filter);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Filter : out Gtk_Recent_Filter) is
   begin
      Filter := new Gtk_Recent_Filter_Record;
      Gtk.Recent_Filter.Initialize (Filter);
   end Gtk_New;

   ---------------------------
   -- Gtk_Recent_Filter_New --
   ---------------------------

   function Gtk_Recent_Filter_New return Gtk_Recent_Filter is
      Filter : constant Gtk_Recent_Filter := new Gtk_Recent_Filter_Record;
   begin
      Gtk.Recent_Filter.Initialize (Filter);
      return Filter;
   end Gtk_Recent_Filter_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Filter : not null access Gtk_Recent_Filter_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_recent_filter_new");
   begin
      if not Filter.Is_Created then
         Set_Object (Filter, Internal);
      end if;
   end Initialize;

   -------------
   -- Add_Age --
   -------------

   procedure Add_Age
      (Filter : not null access Gtk_Recent_Filter_Record;
       Days   : Glib.Gint)
   is
      procedure Internal (Filter : System.Address; Days : Glib.Gint);
      pragma Import (C, Internal, "gtk_recent_filter_add_age");
   begin
      Internal (Get_Object (Filter), Days);
   end Add_Age;

   ---------------------
   -- Add_Application --
   ---------------------

   procedure Add_Application
      (Filter      : not null access Gtk_Recent_Filter_Record;
       Application : UTF8_String)
   is
      procedure Internal
         (Filter      : System.Address;
          Application : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_recent_filter_add_application");
      Tmp_Application : Gtkada.Types.Chars_Ptr := New_String (Application);
   begin
      Internal (Get_Object (Filter), Tmp_Application);
      Free (Tmp_Application);
   end Add_Application;

   ----------------
   -- Add_Custom --
   ----------------

   procedure Add_Custom
      (Filter       : not null access Gtk_Recent_Filter_Record;
       Needed       : Gtk_Recent_Filter_Flags;
       Func         : Gtk_Recent_Filter_Func;
       Data_Destroy : Glib.G_Destroy_Notify_Address)
   is
   begin
      if Func = null then
         C_Gtk_Recent_Filter_Add_Custom (Get_Object (Filter), Needed, System.Null_Address, System.Null_Address, Data_Destroy);
      else
         C_Gtk_Recent_Filter_Add_Custom (Get_Object (Filter), Needed, Internal_Gtk_Recent_Filter_Func'Address, To_Address (Func), Data_Destroy);
      end if;
   end Add_Custom;

   package body Add_Custom_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Recent_Filter_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Recent_Filter_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Recent_Filter_Func, System.Address);

      function Internal_Cb
         (Filter_Info : access Gtk.Recent_Filter.Gtk_Recent_Filter_Info;
          User_Data   : System.Address) return Glib.Gboolean;
      pragma Convention (C, Internal_Cb);
      --  The type of function that is used with custom filters, see
      --  Gtk.Recent_Filter.Add_Custom.
      --  "filter_info": a Gtk.Recent_Filter.Gtk_Recent_Filter_Info that is
      --  filled according to the Needed flags passed to
      --  Gtk.Recent_Filter.Add_Custom
      --  "user_data": user data passed to Gtk.Recent_Filter.Add_Custom

      ----------------
      -- Add_Custom --
      ----------------

      procedure Add_Custom
         (Filter       : not null access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class;
          Needed       : Gtk.Recent_Filter.Gtk_Recent_Filter_Flags;
          Func         : Gtk_Recent_Filter_Func;
          Data         : User_Data_Type;
          Data_Destroy : Glib.G_Destroy_Notify_Address)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_Recent_Filter_Add_Custom (Get_Object (Filter), Needed, System.Null_Address, System.Null_Address, Data_Destroy);
         else
            D := Users.Build (To_Address (Func), Data);
            C_Gtk_Recent_Filter_Add_Custom (Get_Object (Filter), Needed, Internal_Cb'Address, D, Data_Destroy);
         end if;
      end Add_Custom;

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Filter_Info : access Gtk.Recent_Filter.Gtk_Recent_Filter_Info;
          User_Data   : System.Address) return Glib.Gboolean
      is
         D : constant Users.Internal_Data_Access := Users.Convert (User_Data);
      begin
         return Boolean'Pos (To_Gtk_Recent_Filter_Func (D.Func) (Filter_Info.all, D.Data.all));
      end Internal_Cb;

   end Add_Custom_User_Data;

   ---------------
   -- Add_Group --
   ---------------

   procedure Add_Group
      (Filter : not null access Gtk_Recent_Filter_Record;
       Group  : UTF8_String)
   is
      procedure Internal
         (Filter : System.Address;
          Group  : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_recent_filter_add_group");
      Tmp_Group : Gtkada.Types.Chars_Ptr := New_String (Group);
   begin
      Internal (Get_Object (Filter), Tmp_Group);
      Free (Tmp_Group);
   end Add_Group;

   -------------------
   -- Add_Mime_Type --
   -------------------

   procedure Add_Mime_Type
      (Filter    : not null access Gtk_Recent_Filter_Record;
       Mime_Type : UTF8_String)
   is
      procedure Internal
         (Filter    : System.Address;
          Mime_Type : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_recent_filter_add_mime_type");
      Tmp_Mime_Type : Gtkada.Types.Chars_Ptr := New_String (Mime_Type);
   begin
      Internal (Get_Object (Filter), Tmp_Mime_Type);
      Free (Tmp_Mime_Type);
   end Add_Mime_Type;

   -----------------
   -- Add_Pattern --
   -----------------

   procedure Add_Pattern
      (Filter  : not null access Gtk_Recent_Filter_Record;
       Pattern : UTF8_String)
   is
      procedure Internal
         (Filter  : System.Address;
          Pattern : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_recent_filter_add_pattern");
      Tmp_Pattern : Gtkada.Types.Chars_Ptr := New_String (Pattern);
   begin
      Internal (Get_Object (Filter), Tmp_Pattern);
      Free (Tmp_Pattern);
   end Add_Pattern;

   ------------------------
   -- Add_Pixbuf_Formats --
   ------------------------

   procedure Add_Pixbuf_Formats
      (Filter : not null access Gtk_Recent_Filter_Record)
   is
      procedure Internal (Filter : System.Address);
      pragma Import (C, Internal, "gtk_recent_filter_add_pixbuf_formats");
   begin
      Internal (Get_Object (Filter));
   end Add_Pixbuf_Formats;

   ------------
   -- Filter --
   ------------

   function Filter
      (Filter      : not null access Gtk_Recent_Filter_Record;
       Filter_Info : Gtk_Recent_Filter_Info) return Boolean
   is
      function Internal
         (Filter      : System.Address;
          Filter_Info : Gtk_Recent_Filter_Info) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_recent_filter_filter");
   begin
      return Internal (Get_Object (Filter), Filter_Info) /= 0;
   end Filter;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
      (Filter : not null access Gtk_Recent_Filter_Record) return UTF8_String
   is
      function Internal
         (Filter : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_recent_filter_get_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Filter)));
   end Get_Name;

   ----------------
   -- Get_Needed --
   ----------------

   function Get_Needed
      (Filter : not null access Gtk_Recent_Filter_Record)
       return Gtk_Recent_Filter_Flags
   is
      function Internal
         (Filter : System.Address) return Gtk_Recent_Filter_Flags;
      pragma Import (C, Internal, "gtk_recent_filter_get_needed");
   begin
      return Internal (Get_Object (Filter));
   end Get_Needed;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
      (Filter : not null access Gtk_Recent_Filter_Record;
       Name   : UTF8_String)
   is
      procedure Internal
         (Filter : System.Address;
          Name   : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_recent_filter_set_name");
      Tmp_Name : Gtkada.Types.Chars_Ptr := New_String (Name);
   begin
      Internal (Get_Object (Filter), Tmp_Name);
      Free (Tmp_Name);
   end Set_Name;

end Gtk.Recent_Filter;
