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
--  Gtk.Volume_Button.Gtk_Volume_Button is a subclass of
--  Gtk.Scale_Button.Gtk_Scale_Button that has been tailored for use as a
--  volume control widget with suitable icons, tooltips and accessible labels.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
with Glib.Properties;  use Glib.Properties;
with Glib.Types;       use Glib.Types;
with Glib.Variant;     use Glib.Variant;
with Gtk.Action;       use Gtk.Action;
with Gtk.Actionable;   use Gtk.Actionable;
with Gtk.Activatable;  use Gtk.Activatable;
with Gtk.Buildable;    use Gtk.Buildable;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Orientable;   use Gtk.Orientable;
with Gtk.Scale_Button; use Gtk.Scale_Button;

package Gtk.Volume_Button is

   type Gtk_Volume_Button_Record is new Gtk_Scale_Button_Record with null record;
   type Gtk_Volume_Button is access all Gtk_Volume_Button_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Widget : out Gtk_Volume_Button);
   procedure Initialize
      (Widget : not null access Gtk_Volume_Button_Record'Class);
   --  Creates a Gtk.Volume_Button.Gtk_Volume_Button, with a range between 0.0
   --  and 1.0, with a stepping of 0.02. Volume values can be obtained and
   --  modified using the functions from Gtk.Scale_Button.Gtk_Scale_Button.
   --  Since: gtk+ 2.12
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Volume_Button_New return Gtk_Volume_Button;
   --  Creates a Gtk.Volume_Button.Gtk_Volume_Button, with a range between 0.0
   --  and 1.0, with a stepping of 0.02. Volume values can be obtained and
   --  modified using the functions from Gtk.Scale_Button.Gtk_Scale_Button.
   --  Since: gtk+ 2.12

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_volume_button_get_type");

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Action_Name
      (Self : not null access Gtk_Volume_Button_Record) return UTF8_String;

   procedure Set_Action_Name
      (Self        : not null access Gtk_Volume_Button_Record;
       Action_Name : UTF8_String := "");

   function Get_Action_Target_Value
      (Self : not null access Gtk_Volume_Button_Record)
       return Glib.Variant.Gvariant;

   procedure Set_Action_Target_Value
      (Self         : not null access Gtk_Volume_Button_Record;
       Target_Value : Glib.Variant.Gvariant);

   procedure Set_Detailed_Action_Name
      (Self                 : not null access Gtk_Volume_Button_Record;
       Detailed_Action_Name : UTF8_String);

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Volume_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Do_Set_Related_Action);

   function Get_Related_Action
      (Self : not null access Gtk_Volume_Button_Record)
       return Gtk.Action.Gtk_Action;
   pragma Obsolescent (Get_Related_Action);

   procedure Set_Related_Action
      (Self   : not null access Gtk_Volume_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Set_Related_Action);

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Volume_Button_Record) return Boolean;
   pragma Obsolescent (Get_Use_Action_Appearance);

   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Volume_Button_Record;
       Use_Appearance : Boolean);
   pragma Obsolescent (Set_Use_Action_Appearance);

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Volume_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Sync_Action_Properties);

   function Get_Orientation
      (Self : not null access Gtk_Volume_Button_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Volume_Button_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Use_Symbolic_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to use symbolic icons as the icons. Note that if the symbolic
   --  icons are not available in your installed theme, then the normal
   --  (potentially colorful) icons will be used.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Actionable"
   --
   --  - "Activatable"
   --
   --  - "Buildable"
   --
   --  - "Orientable"

   package Implements_Gtk_Actionable is new Glib.Types.Implements
     (Gtk.Actionable.Gtk_Actionable, Gtk_Volume_Button_Record, Gtk_Volume_Button);
   function "+"
     (Widget : access Gtk_Volume_Button_Record'Class)
   return Gtk.Actionable.Gtk_Actionable
   renames Implements_Gtk_Actionable.To_Interface;
   function "-"
     (Interf : Gtk.Actionable.Gtk_Actionable)
   return Gtk_Volume_Button
   renames Implements_Gtk_Actionable.To_Object;

   package Implements_Gtk_Activatable is new Glib.Types.Implements
     (Gtk.Activatable.Gtk_Activatable, Gtk_Volume_Button_Record, Gtk_Volume_Button);
   function "+"
     (Widget : access Gtk_Volume_Button_Record'Class)
   return Gtk.Activatable.Gtk_Activatable
   renames Implements_Gtk_Activatable.To_Interface;
   function "-"
     (Interf : Gtk.Activatable.Gtk_Activatable)
   return Gtk_Volume_Button
   renames Implements_Gtk_Activatable.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Volume_Button_Record, Gtk_Volume_Button);
   function "+"
     (Widget : access Gtk_Volume_Button_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Volume_Button
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Volume_Button_Record, Gtk_Volume_Button);
   function "+"
     (Widget : access Gtk_Volume_Button_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Volume_Button
   renames Implements_Gtk_Orientable.To_Object;

private
   Use_Symbolic_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-symbolic");
end Gtk.Volume_Button;
