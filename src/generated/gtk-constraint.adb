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

package body Gtk.Constraint is

   package Type_Conversion_Gtk_Constraint is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Constraint_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Constraint);

   ------------------------
   -- Gtk_Constraint_New --
   ------------------------

   function Gtk_Constraint_New
      (Target           : System.Address;
       Target_Attribute : Gtk.Enums.Gtk_Constraint_Attribute;
       Relation         : Gtk.Enums.Gtk_Constraint_Relation;
       Source           : System.Address;
       Source_Attribute : Gtk.Enums.Gtk_Constraint_Attribute;
       Multiplier       : Gdouble;
       The_Constant     : Gdouble;
       Strength         : Glib.Gint) return Gtk_Constraint
   is
      Self : constant Gtk_Constraint := new Gtk_Constraint_Record;
   begin
      Gtk.Constraint.Initialize (Self, Target, Target_Attribute, Relation, Source, Source_Attribute, Multiplier, The_Constant, Strength);
      return Self;
   end Gtk_Constraint_New;

   ---------------------------------
   -- Gtk_Constraint_New_Constant --
   ---------------------------------

   function Gtk_Constraint_New_Constant
      (Target           : System.Address;
       Target_Attribute : Gtk.Enums.Gtk_Constraint_Attribute;
       Relation         : Gtk.Enums.Gtk_Constraint_Relation;
       The_Constant     : Gdouble;
       Strength         : Glib.Gint) return Gtk_Constraint
   is
      Self : constant Gtk_Constraint := new Gtk_Constraint_Record;
   begin
      Gtk.Constraint.Initialize_Constant (Self, Target, Target_Attribute, Relation, The_Constant, Strength);
      return Self;
   end Gtk_Constraint_New_Constant;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self             : out Gtk_Constraint;
       Target           : System.Address;
       Target_Attribute : Gtk.Enums.Gtk_Constraint_Attribute;
       Relation         : Gtk.Enums.Gtk_Constraint_Relation;
       Source           : System.Address;
       Source_Attribute : Gtk.Enums.Gtk_Constraint_Attribute;
       Multiplier       : Gdouble;
       The_Constant     : Gdouble;
       Strength         : Glib.Gint)
   is
   begin
      Self := new Gtk_Constraint_Record;
      Gtk.Constraint.Initialize (Self, Target, Target_Attribute, Relation, Source, Source_Attribute, Multiplier, The_Constant, Strength);
   end Gtk_New;

   ----------------------
   -- Gtk_New_Constant --
   ----------------------

   procedure Gtk_New_Constant
      (Self             : out Gtk_Constraint;
       Target           : System.Address;
       Target_Attribute : Gtk.Enums.Gtk_Constraint_Attribute;
       Relation         : Gtk.Enums.Gtk_Constraint_Relation;
       The_Constant     : Gdouble;
       Strength         : Glib.Gint)
   is
   begin
      Self := new Gtk_Constraint_Record;
      Gtk.Constraint.Initialize_Constant (Self, Target, Target_Attribute, Relation, The_Constant, Strength);
   end Gtk_New_Constant;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self             : not null access Gtk_Constraint_Record'Class;
       Target           : System.Address;
       Target_Attribute : Gtk.Enums.Gtk_Constraint_Attribute;
       Relation         : Gtk.Enums.Gtk_Constraint_Relation;
       Source           : System.Address;
       Source_Attribute : Gtk.Enums.Gtk_Constraint_Attribute;
       Multiplier       : Gdouble;
       The_Constant     : Gdouble;
       Strength         : Glib.Gint)
   is
      function Internal
         (Target           : System.Address;
          Target_Attribute : Gtk.Enums.Gtk_Constraint_Attribute;
          Relation         : Gtk.Enums.Gtk_Constraint_Relation;
          Source           : System.Address;
          Source_Attribute : Gtk.Enums.Gtk_Constraint_Attribute;
          Multiplier       : Gdouble;
          The_Constant     : Gdouble;
          Strength         : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gtk_constraint_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Target, Target_Attribute, Relation, Source, Source_Attribute, Multiplier, The_Constant, Strength));
      end if;
   end Initialize;

   -------------------------
   -- Initialize_Constant --
   -------------------------

   procedure Initialize_Constant
      (Self             : not null access Gtk_Constraint_Record'Class;
       Target           : System.Address;
       Target_Attribute : Gtk.Enums.Gtk_Constraint_Attribute;
       Relation         : Gtk.Enums.Gtk_Constraint_Relation;
       The_Constant     : Gdouble;
       Strength         : Glib.Gint)
   is
      function Internal
         (Target           : System.Address;
          Target_Attribute : Gtk.Enums.Gtk_Constraint_Attribute;
          Relation         : Gtk.Enums.Gtk_Constraint_Relation;
          The_Constant     : Gdouble;
          Strength         : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gtk_constraint_new_constant");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Target, Target_Attribute, Relation, The_Constant, Strength));
      end if;
   end Initialize_Constant;

   ------------------
   -- Get_Constant --
   ------------------

   function Get_Constant
      (Self : not null access Gtk_Constraint_Record) return Gdouble
   is
      function Internal (Self : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_constraint_get_constant");
   begin
      return Internal (Get_Object (Self));
   end Get_Constant;

   --------------------
   -- Get_Multiplier --
   --------------------

   function Get_Multiplier
      (Self : not null access Gtk_Constraint_Record) return Gdouble
   is
      function Internal (Self : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_constraint_get_multiplier");
   begin
      return Internal (Get_Object (Self));
   end Get_Multiplier;

   ------------------
   -- Get_Relation --
   ------------------

   function Get_Relation
      (Self : not null access Gtk_Constraint_Record)
       return Gtk.Enums.Gtk_Constraint_Relation
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Constraint_Relation;
      pragma Import (C, Internal, "gtk_constraint_get_relation");
   begin
      return Internal (Get_Object (Self));
   end Get_Relation;

   ----------------
   -- Get_Source --
   ----------------

   function Get_Source
      (Self : not null access Gtk_Constraint_Record)
       return Gtk.Constraint_Target.Gtk_Constraint_Target
   is
      function Internal
         (Self : System.Address)
          return Gtk.Constraint_Target.Gtk_Constraint_Target;
      pragma Import (C, Internal, "gtk_constraint_get_source");
   begin
      return Internal (Get_Object (Self));
   end Get_Source;

   --------------------------
   -- Get_Source_Attribute --
   --------------------------

   function Get_Source_Attribute
      (Self : not null access Gtk_Constraint_Record)
       return Gtk.Enums.Gtk_Constraint_Attribute
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Constraint_Attribute;
      pragma Import (C, Internal, "gtk_constraint_get_source_attribute");
   begin
      return Internal (Get_Object (Self));
   end Get_Source_Attribute;

   ------------------
   -- Get_Strength --
   ------------------

   function Get_Strength
      (Self : not null access Gtk_Constraint_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_constraint_get_strength");
   begin
      return Internal (Get_Object (Self));
   end Get_Strength;

   ----------------
   -- Get_Target --
   ----------------

   function Get_Target
      (Self : not null access Gtk_Constraint_Record)
       return Gtk.Constraint_Target.Gtk_Constraint_Target
   is
      function Internal
         (Self : System.Address)
          return Gtk.Constraint_Target.Gtk_Constraint_Target;
      pragma Import (C, Internal, "gtk_constraint_get_target");
   begin
      return Internal (Get_Object (Self));
   end Get_Target;

   --------------------------
   -- Get_Target_Attribute --
   --------------------------

   function Get_Target_Attribute
      (Self : not null access Gtk_Constraint_Record)
       return Gtk.Enums.Gtk_Constraint_Attribute
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Constraint_Attribute;
      pragma Import (C, Internal, "gtk_constraint_get_target_attribute");
   begin
      return Internal (Get_Object (Self));
   end Get_Target_Attribute;

   -----------------
   -- Is_Attached --
   -----------------

   function Is_Attached
      (Self : not null access Gtk_Constraint_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_constraint_is_attached");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Attached;

   -----------------
   -- Is_Constant --
   -----------------

   function Is_Constant
      (Self : not null access Gtk_Constraint_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_constraint_is_constant");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Constant;

   -----------------
   -- Is_Required --
   -----------------

   function Is_Required
      (Self : not null access Gtk_Constraint_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_constraint_is_required");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Required;

end Gtk.Constraint;
