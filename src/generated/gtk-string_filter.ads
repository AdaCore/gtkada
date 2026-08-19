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

--  Determines whether to include items by comparing strings to a fixed search
--  term.
--
--  The strings are obtained from the items by evaluating an expression set
--  with [methodGtk.StringFilter.set_expression], and they are compared against
--  a search term set with [methodGtk.StringFilter.set_search].
--
--  `GtkStringFilter` has several different modes of comparison - it can match
--  the whole string, just a prefix, or any substring. Use
--  [methodGtk.StringFilter.set_match_mode] choose a mode.
--
--  It is also possible to make case-insensitive comparisons, with
--  [methodGtk.StringFilter.set_ignore_case].

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Properties;         use Glib.Properties;
with Gtk.Expression;          use Gtk.Expression;
with Gtk.Filter;              use Gtk.Filter;

package Gtk.String_Filter is

   type Gtk_String_Filter_Record is new Gtk_Filter_Record with null record;
   type Gtk_String_Filter is access all Gtk_String_Filter_Record'Class;

   type Gtk_String_Filter_Match_Mode is (
      String_Filter_Match_Mode_Exact,
      String_Filter_Match_Mode_Substring,
      String_Filter_Match_Mode_Prefix);
   pragma Convention (C, Gtk_String_Filter_Match_Mode);
   --  Specifies how search strings are matched inside text.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_String_Filter_Match_Mode_Properties is
      new Generic_Internal_Discrete_Property (Gtk_String_Filter_Match_Mode);
   type Property_Gtk_String_Filter_Match_Mode is new Gtk_String_Filter_Match_Mode_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self       : out Gtk_String_Filter;
       Expression : Gtk.Expression.Gtk_Expression);
   procedure Initialize
      (Self       : not null access Gtk_String_Filter_Record'Class;
       Expression : Gtk.Expression.Gtk_Expression);
   --  Creates a new string filter.
   --  You will want to set up the filter by providing a string to search for
   --  and by providing a property to look up on the item.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Expression the expression to evaluate

   function Gtk_String_Filter_New
      (Expression : Gtk.Expression.Gtk_Expression) return Gtk_String_Filter;
   --  Creates a new string filter.
   --  You will want to set up the filter by providing a string to search for
   --  and by providing a property to look up on the item.
   --  @param Expression the expression to evaluate

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_string_filter_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Expression
      (Self : not null access Gtk_String_Filter_Record)
       return Gtk.Expression.Gtk_Expression;
   --  Gets the expression that the string filter uses to obtain strings from
   --  items.
   --  @return the expression
   --  Return has transfer-ownership='none'

   procedure Set_Expression
      (Self       : not null access Gtk_String_Filter_Record;
       Expression : Gtk.Expression.Gtk_Expression);
   --  Sets the expression that the string filter uses to obtain strings from
   --  items.
   --  The expression must have a value type of `G_TYPE_STRING`.
   --  @param Expression the expression

   function Get_Ignore_Case
      (Self : not null access Gtk_String_Filter_Record) return Boolean;
   --  Returns whether the filter ignores case differences.
   --  @return true if the filter ignores case

   procedure Set_Ignore_Case
      (Self        : not null access Gtk_String_Filter_Record;
       Ignore_Case : Boolean);
   --  Sets whether the filter ignores case differences.
   --  @param Ignore_Case true to ignore case

   function Get_Match_Mode
      (Self : not null access Gtk_String_Filter_Record)
       return Gtk_String_Filter_Match_Mode;
   --  Returns the match mode that the filter is using.
   --  @return the match mode of the filter

   procedure Set_Match_Mode
      (Self : not null access Gtk_String_Filter_Record;
       Mode : Gtk_String_Filter_Match_Mode);
   --  Sets the match mode for the filter.
   --  @param Mode the new match mode

   function Get_Search
      (Self : not null access Gtk_String_Filter_Record) return UTF8_String;
   --  Gets the search term.
   --  @return the search term

   procedure Set_Search
      (Self   : not null access Gtk_String_Filter_Record;
       Search : UTF8_String := "");
   --  Sets the string to search for.
   --  @param Search the string to search for

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Ignore_Case_Property : constant Glib.Properties.Property_Boolean;
   --  If matching is case sensitive.

   Match_Mode_Property : constant Gtk.String_Filter.Property_Gtk_String_Filter_Match_Mode;
   --  Type: Gtk_String_Filter_Match_Mode
   --  If exact matches are necessary or if substrings are allowed.

   Search_Property : constant Glib.Properties.Property_String;
   --  The search term.

private
   Search_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("search");
   Match_Mode_Property : constant Gtk.String_Filter.Property_Gtk_String_Filter_Match_Mode :=
     Gtk.String_Filter.Build ("match-mode");
   Ignore_Case_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("ignore-case");
end Gtk.String_Filter;
