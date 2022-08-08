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
--  Actions are organised into groups. An action group is essentially a map
--  from names to Gtk.Action.Gtk_Action objects.
--
--  All actions that would make sense to use in a particular context should be
--  in a single group. Multiple action groups may be used for a particular user
--  interface. In fact, it is expected that most nontrivial applications will
--  make use of multiple groups. For example, in an application that can edit
--  multiple documents, one group holding global actions (e.g. quit, about,
--  new), and one group per document holding actions that act on that document
--  (eg. save, cut/copy/paste, etc). Each window's menus would be constructed
--  from a combination of two action groups.
--
--  ## Accelerators ## {Gtk.Action.Gtk_Action-Accel}
--
--  Accelerators are handled by the GTK+ accelerator map. All actions are
--  assigned an accelerator path (which normally has the form
--  `<Actions>/group-name/action-name`) and a shortcut is associated with this
--  accelerator path. All menuitems and toolitems take on this accelerator
--  path. The GTK+ accelerator map code makes sure that the correct shortcut is
--  displayed next to the menu item.
--
--  # GtkActionGroup as GtkBuildable #
--  {Gtk.Action_Group.Gtk_Action_Group-BUILDER-UI}
--
--  The Gtk.Action_Group.Gtk_Action_Group implementation of the
--  Gtk.Buildable.Gtk_Buildable interface accepts Gtk.Action.Gtk_Action objects
--  as <child> elements in UI definitions.
--
--  Note that it is probably more common to define actions and action groups
--  in the code, since they are directly related to what the code can do.
--
--  The GtkActionGroup implementation of the GtkBuildable interface supports a
--  custom <accelerator> element, which has attributes named "key" and
--  "modifiers" and allows to specify accelerators. This is similar to the
--  <accelerator> element of Gtk.Widget.Gtk_Widget, the main difference is that
--  it doesn't allow you to specify a signal.
--
--  ## A Gtk.Dialog.Gtk_Dialog UI definition fragment. ## |[ <object
--  class="GtkActionGroup" id="actiongroup"> <child> <object class="GtkAction"
--  id="About"> <property name="name">About</property> <property
--  name="stock_id">gtk-about</property> <signal handler="about_activate"
--  name="activate"/> </object> <accelerator key="F1"
--  modifiers="GDK_CONTROL_MASK | GDK_SHIFT_MASK"/> </child> </object> ]|
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Glist;      use Glib.Glist;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Action;      use Gtk.Action;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Widget;      use Gtk.Widget;
with Gtkada.Types;    use Gtkada.Types;

package Gtk.Action_Group is

   type Gtk_Action_Group_Record is new GObject_Record with null record;
   type Gtk_Action_Group is access all Gtk_Action_Group_Record'Class;

   function Convert (R : Gtk.Action.Gtk_Action) return System.Address;
   function Convert (R : System.Address) return Gtk.Action.Gtk_Action;
   package Action_List is new Generic_List (Gtk.Action.Gtk_Action);

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Translate_Func is access function (Path : UTF8_String) return UTF8_String;
   --  The function used to translate messages in e.g.
   --  Gtk.Icon_Factory.Gtk_Icon_Factory and Gtk.Action_Group.Gtk_Action_Group.
   --  "path": The id of the message. In Gtk.Action_Group.Gtk_Action_Group
   --  this will be a label or tooltip from a Gtk_Action_Entry.

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Action_Group : out Gtk_Action_Group;
       Name         : UTF8_String);
   procedure Initialize
      (Action_Group : not null access Gtk_Action_Group_Record'Class;
       Name         : UTF8_String);
   --  Creates a new Gtk.Action_Group.Gtk_Action_Group object. The name of the
   --  action group is used when associating [keybindings][Action-Accel] with
   --  the actions.
   --  Since: gtk+ 2.4
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "name": the name of the action group.

   function Gtk_Action_Group_New
      (Name : UTF8_String) return Gtk_Action_Group;
   --  Creates a new Gtk.Action_Group.Gtk_Action_Group object. The name of the
   --  action group is used when associating [keybindings][Action-Accel] with
   --  the actions.
   --  Since: gtk+ 2.4
   --  "name": the name of the action group.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_action_group_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Action
      (Action_Group : not null access Gtk_Action_Group_Record;
       Action       : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Add_Action);
   --  Adds an action object to the action group. Note that this function does
   --  not set up the accel path of the action, which can lead to problems if a
   --  user tries to modify the accelerator of a menuitem associated with the
   --  action. Therefore you must either set the accel path yourself with
   --  Gtk.Action.Set_Accel_Path, or use
   --  `gtk_action_group_add_action_with_accel (..., NULL)`.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1
   --  "action": an action

   procedure Add_Action_With_Accel
      (Action_Group : not null access Gtk_Action_Group_Record;
       Action       : not null access Gtk.Action.Gtk_Action_Record'Class;
       Accelerator  : UTF8_String := "");
   pragma Obsolescent (Add_Action_With_Accel);
   --  Adds an action object to the action group and sets up the accelerator.
   --  If Accelerator is null, attempts to use the accelerator associated with
   --  the stock_id of the action.
   --  Accel paths are set to `<Actions>/group-name/action-name`.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1
   --  "action": the action to add
   --  "accelerator": the accelerator for the action, in the format understood
   --  by Gtk.Accel_Group.Accelerator_Parse, or "" for no accelerator, or null
   --  to use the stock accelerator

   function Get_Accel_Group
      (Action_Group : not null access Gtk_Action_Group_Record)
       return Gtk.Accel_Group.Gtk_Accel_Group;
   pragma Obsolescent (Get_Accel_Group);
   --  Gets the accelerator group.
   --  Since: gtk+ 3.6
   --  Deprecated since 3.10, 1

   procedure Set_Accel_Group
      (Action_Group : not null access Gtk_Action_Group_Record;
       Accel_Group  : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class);
   pragma Obsolescent (Set_Accel_Group);
   --  Sets the accelerator group to be used by every action in this group.
   --  Since: gtk+ 3.6
   --  Deprecated since 3.10, 1
   --  "accel_group": a Gtk.Accel_Group.Gtk_Accel_Group to set or null

   function Get_Action
      (Action_Group : not null access Gtk_Action_Group_Record;
       Action_Name  : UTF8_String) return Gtk.Action.Gtk_Action;
   pragma Obsolescent (Get_Action);
   --  Looks up an action in the action group by name.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1
   --  "action_name": the name of the action

   function Get_Name
      (Action_Group : not null access Gtk_Action_Group_Record)
       return UTF8_String;
   pragma Obsolescent (Get_Name);
   --  Gets the name of the action group.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1

   function Get_Sensitive
      (Action_Group : not null access Gtk_Action_Group_Record)
       return Boolean;
   pragma Obsolescent (Get_Sensitive);
   --  Returns True if the group is sensitive. The constituent actions can
   --  only be logically sensitive (see Gtk.Action.Is_Sensitive) if they are
   --  sensitive (see Gtk.Action.Get_Sensitive) and their group is sensitive.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1

   procedure Set_Sensitive
      (Action_Group : not null access Gtk_Action_Group_Record;
       Sensitive    : Boolean);
   pragma Obsolescent (Set_Sensitive);
   --  Changes the sensitivity of Action_Group
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1
   --  "sensitive": new sensitivity

   function Get_Visible
      (Action_Group : not null access Gtk_Action_Group_Record)
       return Boolean;
   pragma Obsolescent (Get_Visible);
   --  Returns True if the group is visible. The constituent actions can only
   --  be logically visible (see Gtk.Action.Is_Visible) if they are visible
   --  (see Gtk.Action.Get_Visible) and their group is visible.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1

   procedure Set_Visible
      (Action_Group : not null access Gtk_Action_Group_Record;
       Visible      : Boolean);
   pragma Obsolescent (Set_Visible);
   --  Changes the visible of Action_Group.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1
   --  "visible": new visiblity

   function List_Actions
      (Action_Group : not null access Gtk_Action_Group_Record)
       return Action_List.Glist;
   pragma Obsolescent (List_Actions);
   --  Lists the actions in the action group.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1

   procedure Remove_Action
      (Action_Group : not null access Gtk_Action_Group_Record;
       Action       : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Remove_Action);
   --  Removes an action object from the action group.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1
   --  "action": an action

   procedure Set_Translate_Func
      (Action_Group : not null access Gtk_Action_Group_Record;
       Func         : Gtk_Translate_Func);
   pragma Obsolescent (Set_Translate_Func);
   --  Sets a function to be used for translating the Label and Tooltip of
   --  Gtk_Action_Entrys added by gtk_action_group_add_actions.
   --  If you're using gettext, it is enough to set the translation domain
   --  with Gtk.Action_Group.Set_Translation_Domain.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1
   --  "func": a Gtk_Translate_Func

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Translate_Func_User_Data is

      type Gtk_Translate_Func is access function
        (Path      : UTF8_String;
         Func_Data : User_Data_Type) return UTF8_String;
      --  The function used to translate messages in e.g.
      --  Gtk.Icon_Factory.Gtk_Icon_Factory and Gtk.Action_Group.Gtk_Action_Group.
      --  "path": The id of the message. In Gtk.Action_Group.Gtk_Action_Group
      --  this will be a label or tooltip from a Gtk_Action_Entry.
      --  "func_data": user data passed in when registering the function

      procedure Set_Translate_Func
         (Action_Group : not null access Gtk.Action_Group.Gtk_Action_Group_Record'Class;
          Func         : Gtk_Translate_Func;
          Data         : User_Data_Type);
      pragma Obsolescent (Set_Translate_Func);
      --  Sets a function to be used for translating the Label and Tooltip of
      --  Gtk_Action_Entrys added by gtk_action_group_add_actions.
      --  If you're using gettext, it is enough to set the translation domain
      --  with Gtk.Action_Group.Set_Translation_Domain.
      --  Since: gtk+ 2.4
      --  Deprecated since 3.10, 1
      --  "func": a Gtk_Translate_Func
      --  "data": data to be passed to Func and Notify

   end Set_Translate_Func_User_Data;

   procedure Set_Translation_Domain
      (Action_Group : not null access Gtk_Action_Group_Record;
       Domain       : UTF8_String := "");
   pragma Obsolescent (Set_Translation_Domain);
   --  Sets the translation domain and uses g_dgettext for translating the
   --  Label and Tooltip of Gtk_Action_Entrys added by
   --  gtk_action_group_add_actions.
   --  If you're not using gettext for localization, see
   --  Gtk.Action_Group.Set_Translate_Func.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1
   --  "domain": the translation domain to use for g_dgettext calls, or null
   --  to use the domain set with textdomain

   function Translate_String
      (Action_Group : not null access Gtk_Action_Group_Record;
       String       : UTF8_String) return UTF8_String;
   pragma Obsolescent (Translate_String);
   --  Translates a string using the function set with
   --  Gtk.Action_Group.Set_Translate_Func. This is mainly intended for
   --  language bindings.
   --  Since: gtk+ 2.6
   --  Deprecated since 3.10, 1
   --  "string": a string

   ----------------------
   -- GtkAda additions --
   ----------------------

   type Action_Callback is access procedure
     (Action : System.Address; User_Data : System.Address);
   pragma Convention (C, Action_Callback);
   --  Profile of callbacks when an action is activated. You must convert
   --  Action to a Gtk_Action through:
   --      Act : constant Gtk_Action := Convert (Action);

   type Action_Entry is record
      Name         : Gtkada.Types.Chars_Ptr;
      Stock_Id     : Gtkada.Types.Chars_Ptr;
      Label        : Gtkada.Types.Chars_Ptr;
      Accelerator  : Gtkada.Types.Chars_Ptr;
      Tooltip      : Gtkada.Types.Chars_Ptr;
      Callback     : Action_Callback;
   end record;
   pragma Convention (C, Action_Entry);

   type Radio_Action_Entry is record
      Name         : Gtkada.Types.Chars_Ptr;
      Stock_Id     : Gtkada.Types.Chars_Ptr;
      Label        : Gtkada.Types.Chars_Ptr;
      Accelerator  : Gtkada.Types.Chars_Ptr;
      Tooltip      : Gtkada.Types.Chars_Ptr;
      Value        : Glib.Gint;
   end record;
   pragma Convention (C, Radio_Action_Entry);

   type Toggle_Action_Entry is record
      Name         : Gtkada.Types.Chars_Ptr;
      Stock_Id     : Gtkada.Types.Chars_Ptr;
      Label        : Gtkada.Types.Chars_Ptr;
      Accelerator  : Gtkada.Types.Chars_Ptr;
      Tooltip      : Gtkada.Types.Chars_Ptr;
      Callback     : Action_Callback;
      Is_Active    : Glib.Gboolean;
   end record;
   pragma Convention (C, Toggle_Action_Entry);
   --  An opaque structure describing an action entry

   type Action_Entry_Array is array (Natural range <>) of Action_Entry;
   type Radio_Action_Entry_Array
   is array (Natural range <>) of Radio_Action_Entry;
   type Toggle_Action_Entry_Array
   is array (Natural range <>) of Toggle_Action_Entry;

   type Radio_Action_Callback is access procedure
     (Group     : access Gtk.Action.Gtk_Action_Record'Class;
      Current   : access Gtk.Action.Gtk_Action_Record'Class;
      User_Data : System.Address);
   --   Called when an element of the Gtk_Radio_Action group is selected

   function Create
     (Name        : String;
      Label       : String := "";
      Stock_Id    : String := "";
      Accelerator : String := "";
      Tooltip     : String := "";
      Callback    : Action_Callback := null) return Action_Entry;
   --  Create a new Action_Entry. The returned value must be freed by the
   --  caller.

   function Create
     (Name        : String;
      Label       : String := "";
      Stock_Id    : String := "";
      Accelerator : String := "";
      Tooltip     : String := "";
      Callback    : Action_Callback := null;
      Is_Active   : Boolean := True) return Toggle_Action_Entry;
   --  Create a new Action_Entry. The returned value must be freed by the
   --  caller. Is_Active is the initial state of the button.

   function Create
     (Name        : String;
      Label       : String;
      Stock_Id    : String := "";
      Accelerator : String := "";
      Tooltip     : String := "";
      Value       : Glib.Gint) return Radio_Action_Entry;
   --  Create a new Radio_Action_Entry. Value is the value set on the radio
   --  action (see Gtk.Radio_Action.Get_Current_Value)

   procedure Free (Action  : in out Action_Entry);
   procedure Free (Actions : in out Action_Entry_Array);
   procedure Free (Action  : in out Radio_Action_Entry);
   procedure Free (Actions : in out Radio_Action_Entry_Array);
   procedure Free (Action  : in out Toggle_Action_Entry);
   procedure Free (Actions : in out Toggle_Action_Entry_Array);
   --  Free Action and Actions

   procedure Add_Actions
     (Action_Group : access Gtk_Action_Group_Record;
      Entries      : Action_Entry_Array;
      User_Data    : System.Address := System.Null_Address;
      Destroy      : Glib.G_Destroy_Notify_Address := null);
   --  This is a convenience function to create a number of actions and add
   --  them to the action group.
   --  Destroy is called when User_Data is no longer needed.
   --
   --  The "activate" signals of the actions are connected to the callbacks in
   --  Entries, and their accel paths are set to
   --  <Actions>/group-name/action-name.

   procedure Add_Radio_Actions
     (Action_Group : access Gtk_Action_Group_Record;
      Entries      : Radio_Action_Entry_Array;
      Value        : Glib.Gint;
      On_Change    : Radio_Action_Callback;
      User_Data    : System.Address := System.Null_Address;
      Destroy      : Glib.G_Destroy_Notify_Address := null);
   --  This is a convenience routine to create a group of radio actions and
   --  add them to the action group.
   --
   --  The "changed" signal of the first radio action is connected to the
   --  On_Change callback and the accel paths of the actions are set to
   --    <Actions>/group-name/action-name
   --
   --  Value is the value of the action to activate initially, or -1 if no
   --  action should be activated.
   --  Destroy is called when User_Data is no longer necessary.

   procedure Add_Toggle_Actions
     (Action_Group : access Gtk_Action_Group_Record;
      Entries      : Toggle_Action_Entry_Array;
      User_Data    : System.Address := System.Null_Address;
      Destroy      : Glib.G_Destroy_Notify_Address := null);
   --  This is a convenience function to create a number of toggle actions and
   --  add them to the action group.
   --  The "activate" signals of the actions are connected to the callbacks and
   --  their accel paths are set to <Actions>/group-name/action-name.
   --  Destroy is called when User_Data is no longer necessary.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Accel_Group_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Accel_Group.Gtk_Accel_Group
   --  The accelerator group the actions of this group should use.

   Name_Property : constant Glib.Properties.Property_String;
   --  A name for the action.

   Sensitive_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the action group is enabled.

   Visible_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the action group is visible.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Action_Group_Gtk_Action_Gtk_Widget_Void is not null access procedure
     (Self   : access Gtk_Action_Group_Record'Class;
      Action : not null access Gtk.Action.Gtk_Action_Record'Class;
      Proxy  : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   type Cb_GObject_Gtk_Action_Gtk_Widget_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Action : not null access Gtk.Action.Gtk_Action_Record'Class;
      Proxy  : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   Signal_Connect_Proxy : constant Glib.Signal_Name := "connect-proxy";
   procedure On_Connect_Proxy
      (Self  : not null access Gtk_Action_Group_Record;
       Call  : Cb_Gtk_Action_Group_Gtk_Action_Gtk_Widget_Void;
       After : Boolean := False);
   procedure On_Connect_Proxy
      (Self  : not null access Gtk_Action_Group_Record;
       Call  : Cb_GObject_Gtk_Action_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::connect-proxy signal is emitted after connecting a proxy to an
   --  action in the group. Note that the proxy may have been connected to a
   --  different action before.
   --
   --  This is intended for simple customizations for which a custom action
   --  class would be too clumsy, e.g. showing tooltips for menuitems in the
   --  statusbar.
   --
   --  Gtk.UI_Manager.Gtk_UI_Manager proxies the signal and provides global
   --  notification just before any action is connected to a proxy, which is
   --  probably more convenient to use.
   -- 
   --  Callback parameters:
   --    --  "action": the action
   --    --  "proxy": the proxy

   Signal_Disconnect_Proxy : constant Glib.Signal_Name := "disconnect-proxy";
   procedure On_Disconnect_Proxy
      (Self  : not null access Gtk_Action_Group_Record;
       Call  : Cb_Gtk_Action_Group_Gtk_Action_Gtk_Widget_Void;
       After : Boolean := False);
   procedure On_Disconnect_Proxy
      (Self  : not null access Gtk_Action_Group_Record;
       Call  : Cb_GObject_Gtk_Action_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::disconnect-proxy signal is emitted after disconnecting a proxy
   --  from an action in the group.
   --
   --  Gtk.UI_Manager.Gtk_UI_Manager proxies the signal and provides global
   --  notification just before any action is connected to a proxy, which is
   --  probably more convenient to use.
   -- 
   --  Callback parameters:
   --    --  "action": the action
   --    --  "proxy": the proxy

   type Cb_Gtk_Action_Group_Gtk_Action_Void is not null access procedure
     (Self   : access Gtk_Action_Group_Record'Class;
      Action : not null access Gtk.Action.Gtk_Action_Record'Class);

   type Cb_GObject_Gtk_Action_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Action : not null access Gtk.Action.Gtk_Action_Record'Class);

   Signal_Post_Activate : constant Glib.Signal_Name := "post-activate";
   procedure On_Post_Activate
      (Self  : not null access Gtk_Action_Group_Record;
       Call  : Cb_Gtk_Action_Group_Gtk_Action_Void;
       After : Boolean := False);
   procedure On_Post_Activate
      (Self  : not null access Gtk_Action_Group_Record;
       Call  : Cb_GObject_Gtk_Action_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::post-activate signal is emitted just after the Action in the
   --  Action_Group is activated
   --
   --  This is intended for Gtk.UI_Manager.Gtk_UI_Manager to proxy the signal
   --  and provide global notification just after any action is activated.

   Signal_Pre_Activate : constant Glib.Signal_Name := "pre-activate";
   procedure On_Pre_Activate
      (Self  : not null access Gtk_Action_Group_Record;
       Call  : Cb_Gtk_Action_Group_Gtk_Action_Void;
       After : Boolean := False);
   procedure On_Pre_Activate
      (Self  : not null access Gtk_Action_Group_Record;
       Call  : Cb_GObject_Gtk_Action_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::pre-activate signal is emitted just before the Action in the
   --  Action_Group is activated
   --
   --  This is intended for Gtk.UI_Manager.Gtk_UI_Manager to proxy the signal
   --  and provide global notification just before any action is activated.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Action_Group_Record, Gtk_Action_Group);
   function "+"
     (Widget : access Gtk_Action_Group_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Action_Group
   renames Implements_Gtk_Buildable.To_Object;

private
   Visible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible");
   Sensitive_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("sensitive");
   Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("name");
   Accel_Group_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("accel-group");
end Gtk.Action_Group;
