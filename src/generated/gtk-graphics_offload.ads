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

--  Bypasses gsk rendering by passing the content of its child directly to the
--  compositor.
--
--  Graphics offload is an optimization to reduce overhead and battery use
--  that is most useful for video content. It only works on some platforms and
--  in certain situations. GTK will automatically fall back to normal rendering
--  if it doesn't.
--
--  Graphics offload is most efficient if there are no controls drawn on top
--  of the video content.
--
--  You should consider using graphics offload for your main widget if it
--  shows frequently changing content (such as a video, or a VM display) and
--  you provide the content in the form of dmabuf textures (see
--  [classGdk.DmabufTextureBuilder]), in particular if it may be fullscreen.
--
--  Numerous factors can prohibit graphics offload:
--
--  - Unsupported platforms. Currently, graphics offload only works on Linux
--  with Wayland.
--
--  - Clipping, such as rounded corners that cause the video content to not be
--  rectangular
--
--  - Unsupported dmabuf formats (see [methodGdk.Display.get_dmabuf_formats])
--
--  - Translucent video content (content with an alpha channel, even if it
--  isn't used)
--
--  - Transforms that are more complex than translations and scales
--
--  - Filters such as opacity, grayscale or similar
--
--  To investigate problems related graphics offload, GTK offers debug flags
--  to print out information about graphics offload and dmabuf use:
--
--  GDK_DEBUG=offload GDK_DEBUG=dmabuf
--
--  The GTK inspector provides a visual debugging tool for graphics offload.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Accessible;          use Gtk.Accessible;
with Gtk.Atcontext;           use Gtk.Atcontext;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Constraint_Target;   use Gtk.Constraint_Target;
with Gtk.Widget;              use Gtk.Widget;

package Gtk.Graphics_Offload is

   type Gtk_Graphics_Offload_Record is new Gtk_Widget_Record with null record;
   type Gtk_Graphics_Offload is access all Gtk_Graphics_Offload_Record'Class;

   type Gtk_Graphics_Offload_Enabled is (
      Enabled,
      Disabled);
   pragma Convention (C, Gtk_Graphics_Offload_Enabled);
   --  Represents the state of graphics offloading.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Graphics_Offload_Enabled_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Graphics_Offload_Enabled);
   type Property_Gtk_Graphics_Offload_Enabled is new Gtk_Graphics_Offload_Enabled_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self  : out Gtk_Graphics_Offload;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   procedure Initialize
      (Self  : not null access Gtk_Graphics_Offload_Record'Class;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Creates a new GtkGraphicsOffload widget.
   --  Since: gtk+ 4.14
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Child the child widget

   function Gtk_Graphics_Offload_New
      (Child : access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk_Graphics_Offload;
   --  Creates a new GtkGraphicsOffload widget.
   --  Since: gtk+ 4.14
   --  @param Child the child widget

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_graphics_offload_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Black_Background
      (Self : not null access Gtk_Graphics_Offload_Record) return Boolean;
   --  Returns whether the widget draws a black background.
   --  See [methodGtk.GraphicsOffload.set_black_background].
   --  Since: gtk+ 4.16
   --  @return `TRUE` if black background is drawn

   procedure Set_Black_Background
      (Self  : not null access Gtk_Graphics_Offload_Record;
       Value : Boolean);
   --  Sets whether this GtkGraphicsOffload widget will draw a black
   --  background.
   --  A main use case for this is **_letterboxing_** where black bars are
   --  visible next to the content if the aspect ratio of the content does not
   --  match the dimensions of the monitor.
   --  Using this property for letterboxing instead of CSS allows compositors
   --  to show content with maximum efficiency, using direct scanout to avoid
   --  extra copies in the compositor.
   --  On Wayland, this is implemented using the [single-pixel
   --  buffer](https://wayland.app/protocols/single-pixel-buffer-v1) protocol.
   --  Since: gtk+ 4.16
   --  @param Value whether to draw a black background behind the content

   function Get_Child
      (Self : not null access Gtk_Graphics_Offload_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the child of Self.
   --  Since: gtk+ 4.14
   --  @return the child widget. Has transfer-ownership='none'.

   procedure Set_Child
      (Self  : not null access Gtk_Graphics_Offload_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the child of Self.
   --  Since: gtk+ 4.14
   --  @param Child the child widget

   function Get_Enabled
      (Self : not null access Gtk_Graphics_Offload_Record)
       return Gtk_Graphics_Offload_Enabled;
   --  Returns whether offload is enabled for Self.
   --  Since: gtk+ 4.14
   --  @return whether offload is enabled

   procedure Set_Enabled
      (Self    : not null access Gtk_Graphics_Offload_Record;
       Enabled : Gtk_Graphics_Offload_Enabled);
   --  Sets whether this GtkGraphicsOffload widget will attempt to offload the
   --  content of its child widget.
   --  Since: gtk+ 4.14
   --  @param Enabled whether to enable offload

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Graphics_Offload_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Graphics_Offload_Record)
       return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Graphics_Offload_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Graphics_Offload_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Graphics_Offload_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Graphics_Offload_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Graphics_Offload_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Graphics_Offload_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Graphics_Offload_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Graphics_Offload_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Graphics_Offload_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Graphics_Offload_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Graphics_Offload_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Graphics_Offload_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Graphics_Offload_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Black_Background_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to draw a black background.

   Child_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The child widget.

   Enabled_Property : constant Gtk.Graphics_Offload.Property_Gtk_Graphics_Offload_Enabled;
   --  Type: Gtk_Graphics_Offload_Enabled
   --  Whether graphics offload is enabled.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.ConstraintTarget"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Graphics_Offload_Record, Gtk_Graphics_Offload);
   function "+"
     (Widget : access Gtk_Graphics_Offload_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Graphics_Offload
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Graphics_Offload_Record, Gtk_Graphics_Offload);
   function "+"
     (Widget : access Gtk_Graphics_Offload_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Graphics_Offload
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Graphics_Offload_Record, Gtk_Graphics_Offload);
   function "+"
     (Widget : access Gtk_Graphics_Offload_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Graphics_Offload
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Enabled_Property : constant Gtk.Graphics_Offload.Property_Gtk_Graphics_Offload_Enabled :=
     Gtk.Graphics_Offload.Build ("enabled");
   Child_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("child");
   Black_Background_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("black-background");
end Gtk.Graphics_Offload;
