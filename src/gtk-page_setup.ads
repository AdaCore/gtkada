------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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
--  A Gtk_Page_Setup object stores the page size, orientation and margins. The
--  idea is that you can get one of these from the page setup dialog and then
--  pass it to the Gtk_Print_Operation when printing. The benefit of splitting
--  this out of the Gtk_Print_Settings is that these affect the actual layout
--  of the page, and thus need to be set long before user prints.
--
--  The margins specified in this object are the "print margins", i.e. the
--  parts of the page that the printer cannot print on. These are different
--  from the layout margins that a word processor uses; they are typically
--  used to determine the minimal size for the layout margins.
--
--  To obtain a Gtk_Page_Setup use Gtk_New to get the defaults, or use
--  Gtk_Print_Run_Page_Setup_Dialog to show the page setup dialog and receive
--  the resulting page setup.
--  </description>
--  <c_version>2.16.6</c_version>

with Glib.Error;
with Glib.Key_File;
with Glib.Object;
with Gtk.Enums;
with Gtk.Paper_Size;

package Gtk.Page_Setup is

   type Gtk_Page_Setup_Record is new Glib.Object.GObject_Record with private;
   type Gtk_Page_Setup is access all Gtk_Page_Setup_Record'Class;

   function Get_Type return GType;

   procedure Gtk_New (Widget : out Gtk_Page_Setup);
   procedure Initialize (Widget : access Gtk_Page_Setup_Record'Class);
   --  Creates a new Gtk_Page_Setup.

   procedure Gtk_New_From_File
     (Widget    : out Gtk_Page_Setup;
      File_Name : String;
      Error     : Glib.Error.GError := null);
   procedure Initialize_From_File
     (Widget    : access Gtk_Page_Setup_Record'Class;
      File_Name : String;
      Error     : Glib.Error.GError := null);
   --  Reads the page setup from the file File_Name. Returns a
   --  new Gtk_Page_Setup object with the restored page setup,
   --  or null if an error occurred. See To_File.

   procedure Gtk_New_From_Key_File
     (Widget     : out Gtk_Page_Setup;
      Key_File   : Glib.Key_File.G_Key_File;
      Group_Name : String := "";
      Error      : Glib.Error.GError := null);
   procedure Initialize_From_Key_File
     (Widget     : access Gtk_Page_Setup_Record'Class;
      Key_File   : Glib.Key_File.G_Key_File;
      Group_Name : String := "";
      Error      : Glib.Error.GError := null);
   --  Reads the page setup from the group @group_name in the key file
   --  Key_File. Returns a new Gtk_Page_Setup object with the restored
   --  page setup, or null if an error occurred.
   --
   --  Return value: the restored Gtk_Page_Setup

   ------------------------------
   -- Loading, Saving, Copying --
   ------------------------------

   function Load_File
     (Setup     : access Gtk_Page_Setup_Record;
      File_Name : String;
      Error     : Glib.Error.GError := null)
      return Boolean;
   --  Reads the page setup from the file File_name.
   --  See To_File.  Returns True on success.

   function Load_Key_File
     (Setup      : access Gtk_Page_Setup_Record;
      Key_File   : Glib.Key_File.G_Key_File;
      Group_Name : String := "";
      Error      : Glib.Error.GError := null)
      return Boolean;
   --  Reads the page setup from the group Group_Name (by default,
   --  "Page Setup") in the key file Key_File.  Returns True on success.

   function To_File
     (Setup     : access Gtk_Page_Setup_Record;
      File_Name : String;
      Error     : Glib.Error.GError := null)
      return Boolean;
   --  This function saves the information from Setup to File_Name.
   --  Returns True on success.

   procedure To_Key_File
     (Setup      : access Gtk_Page_Setup_Record;
      Key_File   : Glib.Key_File.G_Key_File;
      Group_Name : String := "");
   --  This function adds the page setup from Setup to Key_File.

   function Copy (Other : access Gtk_Page_Setup_Record) return Gtk_Page_Setup;
   --  Copies a Gtk_Page_Setup.

   -------------
   -- Margins --
   -------------

   function Get_Bottom_Margin
     (Setup : access Gtk_Page_Setup_Record;
      Unit  : Gtk.Enums.Gtk_Unit)
      return Gdouble;
   function Get_Left_Margin
     (Setup : access Gtk_Page_Setup_Record;
      Unit  : Gtk.Enums.Gtk_Unit)
      return Gdouble;
   function Get_Right_Margin
     (Setup : access Gtk_Page_Setup_Record;
      Unit  : Gtk.Enums.Gtk_Unit)
      return Gdouble;
   function Get_Top_Margin
     (Setup : access Gtk_Page_Setup_Record;
      Unit  : Gtk.Enums.Gtk_Unit)
      return Gdouble;
   --  Gets the specified margin in units of Unit.

   procedure Set_Bottom_Margin
     (Setup  : access Gtk_Page_Setup_Record;
      Margin : Gdouble;
      Unit   : Gtk.Enums.Gtk_Unit);
   procedure Set_Left_Margin
     (Setup  : access Gtk_Page_Setup_Record;
      Margin : Gdouble;
      Unit   : Gtk.Enums.Gtk_Unit);
   procedure Set_Right_Margin
     (Setup  : access Gtk_Page_Setup_Record;
      Margin : Gdouble;
      Unit   : Gtk.Enums.Gtk_Unit);
   procedure Set_Top_Margin
     (Setup  : access Gtk_Page_Setup_Record;
      Margin : Gdouble;
      Unit   : Gtk.Enums.Gtk_Unit);
   --  Sets the margins of the Gtk_Page_Setup.

   ---------------
   -- Page Size --
   ---------------

   function Get_Page_Height
     (Setup : access Gtk_Page_Setup_Record;
      Unit  : Gtk.Enums.Gtk_Unit)
      return Gdouble;
   function Get_Page_Width
     (Setup : access Gtk_Page_Setup_Record;
      Unit  : Gtk.Enums.Gtk_Unit)
      return Gdouble;
   --  Returns the page height/width in units of Unit.
   --
   --  Note that this function takes orientation and
   --  margins into consideration.  See Get_Paper_Height.

   ------------------------------
   -- Paper Size / Orientation --
   ------------------------------

   function Get_Paper_Size
     (Setup : access Gtk_Page_Setup_Record)
      return Gtk.Paper_Size.Gtk_Paper_Size;
   procedure Set_Paper_Size
     (Setup : access Gtk_Page_Setup_Record;
      Size  : Gtk.Paper_Size.Gtk_Paper_Size);
   --  Gets/Sets the paper size of the Gtk_Page_Setup without
   --  changing the margins. See Set_Paper_Size_And_Default_Margins.

   procedure Set_Paper_Size_And_Default_Margins
     (Setup : access Gtk_Page_Setup_Record;
      Size  : Gtk.Paper_Size.Gtk_Paper_Size);
   --  Sets the paper size of the Gtk_Page_Setup and modifies
   --  the margins according to the new paper size.

   function Get_Paper_Height
     (Setup : access Gtk_Page_Setup_Record;
      Unit  : Gtk.Enums.Gtk_Unit)
      return Gdouble;
   function Get_Paper_Width
     (Setup : access Gtk_Page_Setup_Record;
      Unit  : Gtk.Enums.Gtk_Unit)
      return Gdouble;
   --  Returns the paper height in units of Unit.
   --
   --  Note that this function takes orientation, but
   --  not margins into consideration.  See Get_Page_Height.

   function Get_Orientation
     (Setup : access Gtk_Page_Setup_Record)
      return Gtk.Enums.Gtk_Page_Orientation;
   procedure Set_Orientation
     (Setup       : access Gtk_Page_Setup_Record;
      Orientation : Gtk.Enums.Gtk_Page_Orientation);
   --  The page orientation of the Gtk_Page_Setup.

private

   type Gtk_Page_Setup_Record is
      new Glib.Object.GObject_Record with null record;

   pragma Import (C, Get_Type, "gtk_page_setup_get_type");

end Gtk.Page_Setup;
