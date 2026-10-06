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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Constraint_Guide is

   package Type_Conversion_Gtk_Constraint_Guide is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Constraint_Guide_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Constraint_Guide);

   ------------------------------
   -- Gtk_Constraint_Guide_New --
   ------------------------------

   function Gtk_Constraint_Guide_New return Gtk_Constraint_Guide is
      Self : constant Gtk_Constraint_Guide := new Gtk_Constraint_Guide_Record;
   begin
      Gtk.Constraint_Guide.Initialize (Self);
      return Self;
   end Gtk_Constraint_Guide_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Constraint_Guide) is
   begin
      Self := new Gtk_Constraint_Guide_Record;
      Gtk.Constraint_Guide.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Constraint_Guide_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_constraint_guide_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ------------------
   -- Get_Max_Size --
   ------------------

   procedure Get_Max_Size
      (Self   : not null access Gtk_Constraint_Guide_Record;
       Width  : out Glib.Gint;
       Height : out Glib.Gint)
   is
      procedure Internal
         (Self   : System.Address;
          Width  : out Glib.Gint;
          Height : out Glib.Gint);
      pragma Import (C, Internal, "gtk_constraint_guide_get_max_size");
   begin
      Internal (Get_Object (Self), Width, Height);
   end Get_Max_Size;

   ------------------
   -- Get_Min_Size --
   ------------------

   procedure Get_Min_Size
      (Self   : not null access Gtk_Constraint_Guide_Record;
       Width  : out Glib.Gint;
       Height : out Glib.Gint)
   is
      procedure Internal
         (Self   : System.Address;
          Width  : out Glib.Gint;
          Height : out Glib.Gint);
      pragma Import (C, Internal, "gtk_constraint_guide_get_min_size");
   begin
      Internal (Get_Object (Self), Width, Height);
   end Get_Min_Size;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
      (Self : not null access Gtk_Constraint_Guide_Record)
       return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_constraint_guide_get_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Name;

   ------------------
   -- Get_Nat_Size --
   ------------------

   procedure Get_Nat_Size
      (Self   : not null access Gtk_Constraint_Guide_Record;
       Width  : out Glib.Gint;
       Height : out Glib.Gint)
   is
      procedure Internal
         (Self   : System.Address;
          Width  : out Glib.Gint;
          Height : out Glib.Gint);
      pragma Import (C, Internal, "gtk_constraint_guide_get_nat_size");
   begin
      Internal (Get_Object (Self), Width, Height);
   end Get_Nat_Size;

   ------------------
   -- Get_Strength --
   ------------------

   function Get_Strength
      (Self : not null access Gtk_Constraint_Guide_Record)
       return Gtk.Enums.Gtk_Constraint_Strength
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Constraint_Strength;
      pragma Import (C, Internal, "gtk_constraint_guide_get_strength");
   begin
      return Internal (Get_Object (Self));
   end Get_Strength;

   ------------------
   -- Set_Max_Size --
   ------------------

   procedure Set_Max_Size
      (Self   : not null access Gtk_Constraint_Guide_Record;
       Width  : Glib.Gint;
       Height : Glib.Gint)
   is
      procedure Internal
         (Self   : System.Address;
          Width  : Glib.Gint;
          Height : Glib.Gint);
      pragma Import (C, Internal, "gtk_constraint_guide_set_max_size");
   begin
      Internal (Get_Object (Self), Width, Height);
   end Set_Max_Size;

   ------------------
   -- Set_Min_Size --
   ------------------

   procedure Set_Min_Size
      (Self   : not null access Gtk_Constraint_Guide_Record;
       Width  : Glib.Gint;
       Height : Glib.Gint)
   is
      procedure Internal
         (Self   : System.Address;
          Width  : Glib.Gint;
          Height : Glib.Gint);
      pragma Import (C, Internal, "gtk_constraint_guide_set_min_size");
   begin
      Internal (Get_Object (Self), Width, Height);
   end Set_Min_Size;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
      (Self : not null access Gtk_Constraint_Guide_Record;
       Name : UTF8_String := "")
   is
      procedure Internal
         (Self : System.Address;
          Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_constraint_guide_set_name");
      Tmp_Name : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Name :=
        (if Name = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Name));
      Internal (Get_Object (Self), Tmp_Name);
      Free (Tmp_Name);
   end Set_Name;

   ------------------
   -- Set_Nat_Size --
   ------------------

   procedure Set_Nat_Size
      (Self   : not null access Gtk_Constraint_Guide_Record;
       Width  : Glib.Gint;
       Height : Glib.Gint)
   is
      procedure Internal
         (Self   : System.Address;
          Width  : Glib.Gint;
          Height : Glib.Gint);
      pragma Import (C, Internal, "gtk_constraint_guide_set_nat_size");
   begin
      Internal (Get_Object (Self), Width, Height);
   end Set_Nat_Size;

   ------------------
   -- Set_Strength --
   ------------------

   procedure Set_Strength
      (Self     : not null access Gtk_Constraint_Guide_Record;
       Strength : Gtk.Enums.Gtk_Constraint_Strength)
   is
      procedure Internal
         (Self     : System.Address;
          Strength : Gtk.Enums.Gtk_Constraint_Strength);
      pragma Import (C, Internal, "gtk_constraint_guide_set_strength");
   begin
      Internal (Get_Object (Self), Strength);
   end Set_Strength;

end Gtk.Constraint_Guide;
