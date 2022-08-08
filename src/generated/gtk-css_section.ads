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

--  <description>
--  Defines a part of a CSS document. Because sections are nested into one
--  another, you can use Gtk.Css_Section.Get_Parent to get the containing
--  region.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;

package Gtk.Css_Section is

   type Gtk_Css_Section is new Glib.C_Boxed with null record;
   Null_Gtk_Css_Section : constant Gtk_Css_Section;

   function From_Object (Object : System.Address) return Gtk_Css_Section;
   function From_Object_Free (B : access Gtk_Css_Section'Class) return Gtk_Css_Section;
   pragma Inline (From_Object_Free, From_Object);

   type Gtk_Css_Section_Type is (
      Css_Section_Document,
      Css_Section_Import,
      Css_Section_Color_Definition,
      Css_Section_Binding_Set,
      Css_Section_Ruleset,
      Css_Section_Selector,
      Css_Section_Declaration,
      Css_Section_Value,
      Css_Section_Keyframes);
   pragma Convention (C, Gtk_Css_Section_Type);
   --  The different types of sections indicate parts of a CSS document as
   --  parsed by GTK's CSS parser. They are oriented towards the [CSS
   --  Grammar](http://www.w3.org/TR/CSS21/grammar.html), but may contain
   --  extensions.
   --
   --  More types might be added in the future as the parser incorporates more
   --  features.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Css_Section_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Css_Section_Type);
   type Property_Gtk_Css_Section_Type is new Gtk_Css_Section_Type_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_css_section_get_type");

   -------------
   -- Methods --
   -------------

   function Get_End_Line (Self : Gtk_Css_Section) return Guint;
   --  Returns the line in the CSS document where this section end. The line
   --  number is 0-indexed, so the first line of the document will return 0.
   --  This value may change in future invocations of this function if Section
   --  is not yet parsed completely. This will for example happen in the
   --  GtkCssProvider::parsing-error signal. The end position and line may be
   --  identical to the start position and line for sections which failed to
   --  parse anything successfully.
   --  Since: gtk+ 3.2

   function Get_End_Position (Self : Gtk_Css_Section) return Guint;
   --  Returns the offset in bytes from the start of the current line returned
   --  via Gtk.Css_Section.Get_End_Line. This value may change in future
   --  invocations of this function if Section is not yet parsed completely.
   --  This will for example happen in the GtkCssProvider::parsing-error
   --  signal. The end position and line may be identical to the start position
   --  and line for sections which failed to parse anything successfully.
   --  Since: gtk+ 3.2

   function Get_Parent (Self : Gtk_Css_Section) return Gtk_Css_Section;
   --  Gets the parent section for the given Section. The parent section is
   --  the section that contains this Section. A special case are sections of
   --  type GTK_CSS_SECTION_DOCUMENT. Their parent will either be null if they
   --  are the original CSS document that was loaded by
   --  gtk_css_provider_load_from_file or a section of type
   --  GTK_CSS_SECTION_IMPORT if it was loaded with an import rule from a
   --  different file.
   --  Since: gtk+ 3.2

   function Get_Section_Type
      (Self : Gtk_Css_Section) return Gtk_Css_Section_Type;
   --  Gets the type of information that Section describes.
   --  Since: gtk+ 3.2

   function Get_Start_Line (Self : Gtk_Css_Section) return Guint;
   --  Returns the line in the CSS document where this section starts. The
   --  line number is 0-indexed, so the first line of the document will return
   --  0.
   --  Since: gtk+ 3.2

   function Get_Start_Position (Self : Gtk_Css_Section) return Guint;
   --  Returns the offset in bytes from the start of the current line returned
   --  via Gtk.Css_Section.Get_Start_Line.
   --  Since: gtk+ 3.2

   function Ref (Self : Gtk_Css_Section) return Gtk_Css_Section;
   --  Increments the reference count on Section.
   --  Since: gtk+ 3.2

   procedure Unref (Self : Gtk_Css_Section);
   --  Decrements the reference count on Section, freeing the structure if the
   --  reference count reaches 0.
   --  Since: gtk+ 3.2

private

   Null_Gtk_Css_Section : constant Gtk_Css_Section := (Glib.C_Boxed with null record);

end Gtk.Css_Section;
