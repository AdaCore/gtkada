------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2022, AdaCore                     --
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
--
--  This package provides a minimal binding to the GObject type in Glib.
--  See Glib.Properties for information on how to manipulate properties
--
--  </description>
--  <group>Glib, the general-purpose library</group>

with Interfaces.C.Strings;
with Glib.GSlist;
with Glib.Glist;
pragma Elaborate_All (Glib.GSlist);
pragma Elaborate_All (Glib.Glist);

package Glib.Object is

   type GObject_Record is tagged private;
   type GObject is access all GObject_Record'Class;
   pragma No_Strict_Aliasing (GObject);
   --  The base type for Glib/Gdk/Gtk objects. It basically gives access
   --  to an underlying C object. This is not a controlled type for
   --  efficiency reasons and because glib takes care of the memory
   --  management on its own.

   function Is_Created (Object : GObject_Record'Class) return Boolean;
   --  Return True if the associated C object has been created, False if
   --  no C object is associated with Object.
   --  This is not the same as testing whether an access type (for instance
   --  any of the widgets) is "null", since this relates to the underlying
   --  C object.

   function Get_Type (Object : access GObject_Record) return GType;
   --  Return the type of Object.
   --  This function is mostly used internally, since in Ada you can simply
   --  test whether an object belong to a class with a statement like:
   --
   --     if Object in Gtk_Button_Record'Class then ...
   --
   --  which is easier.

   ----------------
   -- Life cycle --
   ----------------

   procedure Initialize (Object : access GObject_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Ref (Object : access GObject_Record);
   --  Increment the reference counter for Object. See Unref below.
   --  Since an object is not deleted while its reference count is not null,
   --  this is a way to keep an object in memory, in particular when you
   --  want to temporarily remove a widget from its parent.

   procedure Unref (Object : access GObject_Record);
   --  Decrement the reference counter for Object. When this reaches 0, the
   --  object is effectively destroy, all the callbacks associated with it are
   --  disconnected.

   type Weak_Notify is access procedure
     (Data                 : System.Address;
      Where_The_Object_Was : System.Address);
   pragma Convention (C, Weak_Notify);
   --  Called when Where_The_Object_Was is destroyed (although you can still
   --  use this to reset it). Data is the argument passed to Weak_Ref.
   --  You should destroy and free the memory occupied by Data

   procedure Weak_Ref
     (Object : access GObject_Record'Class;
      Notify : Weak_Notify;
      Data   : System.Address := System.Null_Address);
   --  This kind of reference doesn't increment the object's reference
   --  counting. However, it can and should be used to monitor the object's
   --  life cycle, in particular to detect is destruction.
   --  When Object is destroyed, calls Notify

   procedure Weak_Unref
     (Object : access GObject_Record'Class;
      Notify : Weak_Notify;
      Data   : System.Address := System.Null_Address);
   --  Cancels the settings of Weak_Ref.

   procedure Deallocate (Object : access GObject_Record);
   --  This operation is used to deallocate Object.
   --  The default implementation assumes that the value passed in is an
   --  access value created by an allocator of the default pool, i.e. it
   --  will assume that an instance of
   --  Ada.Unchecked_Deallocation (GObject_Record'Class, GObject)
   --  can be used to deallocate the designated object.
   --  Types derived of GObject_Record can override this operation in order
   --  to cope with objects allocated on other pools or even objects allocated
   --  on the stack.
   --  This design is limited to support only one allocation strategy for each
   --  class, as the class tag is used to identify the applicable strategy.

   procedure Ref_Sink (Object : access GObject_Record);
   --  Increase the reference count of Object, and possibly remove the
   --  floating reference, if Object has a floating reference.
   --  In other words, if the object is floating, then this call "assumes
   --  ownership" of the floating reference, converting it to a normal
   --  reference by clearing the floating flag while leaving the reference
   --  count unchanged.  If the object is not floating, then this call
   --  adds a new normal reference increasing the reference count by one.

   ------------------------
   -- Interfacing with C --
   ------------------------
   --  The following functions are made public so that one can easily create
   --  new objects outside the Glib or Gtk package hierarchy.
   --  Only experienced users should make use of these functions.

   function Get_Object
     (Object : access GObject_Record'Class) return System.Address;
   --  Access the underlying C pointer.

   function Get_Object_Or_Null (Object : GObject) return System.Address;
   --  Same as above, but passing "null" is valid.

   procedure Set_Object
     (Object : access GObject_Record'Class;
      Value  : System.Address);
   --  Modify the underlying C pointer.

   function Get_User_Data
     (Obj  : System.Address;
      Stub : GObject_Record'Class) return GObject;
   --  Return the Ada object matching the C object Obj. If Obj was created
   --  explicitely from GtkAda, this will be the exact same widget. If Obj was
   --  created implicitely by gtk+ (buttons in complex windows,...), a new Ada
   --  object of type Stub will be created.

   function Get_User_Data_Or_Null (Obj : System.Address) return GObject;
   --  Return the Ada object matching the Obj, if Obj was created explicitly
   --  from Ada, or null otherwise.

   function Get_User_Data_Fast
     (Obj  : System.Address;
      Stub : GObject_Record'Class) return GObject;
   --  Same as Get_User_Data, but does not try to guess the type of Obj,
   --  always default to Stub if Obj is unknown to GtkAda.

   function Unchecked_Cast
     (Obj  : access GObject_Record'Class;
      Stub : GObject_Record'Class) return GObject;
   --  Cast Obj in an object of tag Stub'Class.
   --  Return the resulting object and free the memory pointed by Obj.

   -------------
   -- Signals --
   -------------
   --  Any child of GObject can be associated with any number of signals. The
   --  mechanism for signals is fully generic, and any number of arguments can
   --  be associated with signals.
   --  See the function Initialize_Class_Record for more information on how
   --  to create new signals for your own new widgets.
   --  The subprograms below are provided for introspection: they make it
   --  possible to query the list of signals defined for a specific widget,
   --  as well as their parameters and return types.

   type Signal_Id_Array is array (Guint range <>) of Glib.Signal_Id;

   type Signal_Query is private;

   function Lookup
     (Object : Glib.GType; Signal : Signal_Name) return Glib.Signal_Id;
   --  Returns the signal Id associated with a specific Object/Signal pair.
   --  Null_Signal_Id is returned if no such signal exists for Object.
   --  You can then use the Query procedure to get more information on the
   --  signal.

   function List_Ids (Typ : Glib.GType) return Signal_Id_Array;
   --  Return the list of signals defined for Typ. You can get more information
   --  on each of this signals by using the Query function below.
   --  See also the function Get_Type above to convert from an object instance
   --  to its type. Using a GType as the parameter makes it easier to find the
   --  signals for a widget and its ancestors (using Glib.Parent).

   procedure Query (Id : Glib.Signal_Id; Result : out Signal_Query);
   --  Return the description associated with the signal Id. You can get the
   --  various fields from Query with one of the functions below.
   --  Result is undefined if Id is Invalid_Signal_Id or Null_Signal_Id

   function Id (Q : Signal_Query) return Glib.Signal_Id;
   --  Return the signal Id. Each Id is specific to a widget/signal name pair.
   --  These Ids can then be used to temporarily block a signal for instance,
   --  through the subprograms in Gtk.Handlers.

   function Signal_Name (Q : Signal_Query) return Glib.Signal_Name;
   --  Return the name of the signal, as should be used in a call to Connect.

   function Return_Type (Q : Signal_Query) return Glib.GType;
   --  Return the type of object returned by the handlers for this signal.

   function Params (Q : Signal_Query) return GType_Array;
   --  Return the list of parameters for the handlers for this signal

   --------------------------
   -- Creating new widgets --
   --------------------------
   --  These types and functions are used only when creating new widget types
   --  directly in Ada. These functions initialize the classes so that they are
   --  correctly recognized by gtk+ itself
   --  See the GtkAda user's guide for more information on how to create your
   --  own widget types in Ada.

   type Interface_Vtable is private;
   --  The virtual table of an interface (see Glib.Types). This is only useful
   --  when doing introspection.

   type GObject_Class is new GType_Class;
   Null_GObject_Class : constant GObject_Class;
   --  This type encloses all the informations related to a specific type of
   --  object or widget. All instances of such an object have a pointer to this
   --  structure, that includes the definition of all the signals that exist
   --  for a given object, all its properties,...
   --
   --  A GObject_Class can be retrieved from a GType by calling
   --  Glib.Types.Class_Ref. This will however initialize the class record, so
   --  is too late to add interfaces or properties to the class afterwards.

   type Ada_GObject_Class_Record;
   type Ada_GObject_Class is access all Ada_GObject_Class_Record;
   pragma Convention (C, Ada_GObject_Class);
   Uninitialized_Class : constant Ada_GObject_Class;
   --  This type plays a role similar to GObject_Class, but encapsulates
   --  information needed on the Ada side to implement proxies to Ada code
   --  (since we need to convert from C type to Ada types).
   --  This structure should be treated as opaque.

   type Ada_Class_Init is access procedure (Self : GObject_Class);
   pragma Convention (C, Ada_Class_Init);
   --  Called the first time the class is initialized.
   --  This procedure is meant to add new properties to the class, or
   --  override the default handlers.

   type Ada_GObject_Class_Record is record
      The_Type     : GType := 0;

      Class_Init      : Ada_Class_Init := null;
      Property_Setter : System.Address := System.Null_Address;
      Property_Getter : System.Address := System.Null_Address;
      --  Do not call this function directly (or set it directly, these are
      --  set through various other subprograms).
   end record;

   type Signal_Parameter_Types is
     array (Natural range <>, Natural range <>) of GType;
   --  The description of the parameters for each event. These are the
   --  parameters that the application must provide when emitting the
   --  signal. The user can of course add his own parameters when connecting
   --  the signal in his application, through the use of
   --  Gtk.Handlers.User_Callback.
   --
   --  Each event defined with Initialize_Class_Record below should have an
   --  entry in this table. If Gtk_Type_None is found in the table, it is
   --  ignored. For instance, a Signal_Parameter_Type like:
   --    (1 => (1 => Gdk_Type_Gdk_Event, 2 => GType_None),
   --     2 => (1 => GType_Int,          2 => GType_Int));
   --  defines two signals, the first with a single Gdk_Event parameter, the
   --  second with two ints parameters.

   type Signal_Return_Types is array (Natural range <>) of GType;
   No_Return_Types : constant Signal_Return_Types := (1 .. 0 => GType_None);
   --  The expected return types for signal callbacks.
   --  In general, callbacks are not expected to return any value. One special
   --  case is callbacks returning a boolean (GType_Boolean). In such a case,
   --  GtkAda assumes the return value is used to stop signal propagation when
   --  it is true (indicating the signal has been handled).

   No_Signals : constant Interfaces.C.Strings.chars_ptr_array :=
      (1 .. 0 => Interfaces.C.Strings.Null_Ptr);
   Null_Parameter_Types : constant Signal_Parameter_Types (1 .. 0, 1 .. 0) :=
     (others => (others => GType_None));
   --  An empty array, used as a default parameter in Initialize_Class_Record.

   procedure Initialize_Class_Record
     (Ancestor     : GType;
      Class_Record : in out Ada_GObject_Class;
      Type_Name    : String;
      Signals      : Interfaces.C.Strings.chars_ptr_array := No_Signals;
      Parameters   : Signal_Parameter_Types := Null_Parameter_Types;
      Returns      : Signal_Return_Types := No_Return_Types;
      Class_Init   : Ada_Class_Init := null);
   function Initialize_Class_Record
     (Ancestor     : GType;
      Class_Record : not null access Ada_GObject_Class;
      Type_Name    : String;
      Signals      : Interfaces.C.Strings.chars_ptr_array := No_Signals;
      Parameters   : Signal_Parameter_Types := Null_Parameter_Types;
      Returns      : Signal_Return_Types := No_Return_Types;
      Class_Init   : Ada_Class_Init := null)
   return Boolean;
   --  Create the class record for a new object type.
   --  It is associated with Signals new signals. A pointer to the
   --  newly created structure is also returned in Class_Record.
   --  If Class_Record /= Uninitialized_Class, no memory allocation is
   --  performed, we just reuse it. As a result, each instantiation of an
   --  object will share the same GObject_Class, exactly as is done for gtk+.
   --
   --  As a special case, if Parameters has its default value, all signals are
   --  created with no argument. This is done for backward compatibility
   --  mainly, and you should instead give it an explicit value.
   --  Type_Name should be a unique name identifying the name of the new type.
   --
   --  Only the signals with no parameter can be connected from C code.
   --  However, any signal can be connected from Ada. This is due to the way
   --  we define default marshallers for the signals.
   --
   --  The function returns True if the class record was just created (i.e.
   --  only the first time). This can be used to do further initialization
   --  at that point, like adding interfaces that the type implement. It used
   --  to be where one would override the default signal handlers for draw,
   --  for instance, but this now needs to be done in the Class_Init
   --  callback (see below).
   --
   --  The function Class_Init can be used to add custom properties
   --  to the class, or override default signal handlers. Such settings need
   --  to be done in a separate callback (as opposed to in the function that
   --  calls Initialize_Class_Record) because they need access to the gtk+
   --  class record, which is only created when it is actually needed.

   function Type_From_Class (Class_Record : GObject_Class) return GType;
   --  Return the internal gtk+ type that describes the newly created
   --  Class_Record.
   --  See the function Glib.Types.Class_Peek for the opposite function
   --  converting from a GType to a GObject_Class.

   procedure G_New
      (Object : not null access GObject_Record'Class;
       Typ    : GType);
   procedure G_New
      (Object : not null access GObject_Record'Class;
       Typ    : Ada_GObject_Class);
   --  Create a new instance of Typ (at the C level). This has no effect if
   --  the C object has already been created (so that G_New can be called
   --  from Initialize (and you can call the parent's Initialize).
   --
   --  See a variant in Glib.Values that allow you to set properties at
   --  creation time (which is needed for some properties that cannot be set
   --  after creating the widget).
   --
   --  Object must have been allocated first, but you should not have called
   --  any of the Gtk_New procedures yet.
   --  This procedure is meant to be used when you create your own object
   --  types with own signals, properties,... The code would thus be
   --
   --   with Glib.Properties.Creation;  use Glib.Properties.Creation;
   --   with Gtk.Scrollable;
   --   with Gtk.Widget;                use Gtk.Widget;
   --
   --   Klass : aliased Ada_GObject_Class := Uninitialized_Class;
   --
   --   PROP_H_ADJ : constant Property_Id := 1;
   --   PROP_V_ADJ : constant Property_Id := 2;
   --   --  internal identifier for our widget properties
   --
   --   procedure Class_Init (Self : GObject_Class);
   --   pragma Convention (C, Class_Init);
   --
   --   procedure Class_Init (Self : GObject_Class) is
   --   begin
   --      --  Set properties handler
   --      Set_Properties_Handlers (Self, Prop_Set'Access, Prop_Get'Access);
   --
   --      --  Override inherited properties
   --      Override_Property  (Self, PROP_H_ADJ, "hadjustment");
   --      Override_Property  (Self, PROP_V_ADJ, "vadjustment");
   --
   --      --  Install some custom style properties
   --      Install_Style_Property (Self, Gnew_Int (...));
   --
   --      --  Override some the inherited methods
   --      Set_Default_Draw_Handler (Self, On_Draw'Access);
   --   end Class_Init;
   --
   --   function Get_Type return GType is
   --      Info : GInterface_Info_Access;
   --   begin
   --      if Initialize_Class_Record
   --         (Ancestor     => Gtk.Button.Get_Type,
   --          Class_Record => Klass'Access,
   --          Type_Name    => "My_Widget",
   --          Class_Init   => Class_Init'Access)
   --      then
   --         Info := new GInterface_Info'(null, null, System.Null_Address);
   --         Add_Interface (Klass, Gtk.Scrollable.Get_Type, Info);
   --      end if;
   --      return Klass.The_Type;
   --   end Get_Type;
   --
   --   procedure Gtk_New (Self : out My_Widget) is
   --   begin
   --      Self := new My_Widget_Record;  --  create the Ada wrapper
   --      Initialize (Self);
   --   end Gtk_New;
   --
   --   procedure Initialize (Self : not null access My_Widget_Record'Class) is
   --   begin
   --      G_New (Self, Get_Type); --  allocate the C widget, unless done
   --
   --      --  Initialize parent fields
   --
   --      My_Widget_Parent.Initialize (Self);
   --
   --      --  Initialization of the Ada types
   --      ...
   --   end Initialize;

   ----------------
   -- Interfaces --
   ----------------

   type Interface_Description is new System.Address;

   type GInterfaceInitFunc is access procedure
      (Iface : Interface_Description;
       Data  : System.Address);
   pragma Convention (C, GInterfaceInitFunc);
   --  The interface initialization function. This function should initialize
   --  all internal data and allocate any resources required by the interface.
   --  Iface: the interface structure to initialize. It is allocated
   --    specifically for each class instance that implements the interface.
   --    The exact type depends on the interface that is implemented. This is
   --    in general a record that contains a number of access to procedures
   --    for the interface methods. See for instance Gtk.Tree_Model.Interfaces
   --  Data: the data supplied to the GInterface_Info.

   type GInterfaceFinalizeFunc is access procedure
      (Iface : Interface_Description;
       Data  : System.Address);
   pragma Convention (C, GInterfaceFinalizeFunc);
   --  The interface finalization function.
   --  This function should destroy any internal data and release any resources
   --  allocated by the corresponding init func.

   type GInterface_Info is record
      Interface_Init     : GInterfaceInitFunc := null;
      Interface_Finalize : GInterfaceFinalizeFunc := null;
      Interface_Data     : System.Address := System.Null_Address;
   end record;
   pragma Convention (C, GInterface_Info);
   --  A structure that provides information to the type system which is used
   --  specifically for managing interface types.

   type GInterface_Info_Access is access all GInterface_Info;

   procedure Add_Interface
      (Klass : Ada_GObject_Class;
       Iface : GType;
       Info  : not null GInterface_Info_Access);
   --  State that Klass implements the given interface. It will need to
   --  override the inherited methods. This is low-level handling.
   --  Info should be allocated in this call, and is never freed in the
   --  lifetime of the application.

   -------------
   -- Signals --
   -------------
   --  ??? This section is incomplete.

   --  <signals>
   --  The following new signals are defined for this object:
   --
   --  - "notify"
   --    procedure Handler
   --      (Object : access GObject_Record'Class; Name : String);
   --
   --    Emitted when the property Name has been modified
   --  </signals>

   procedure Notify
     (Object        : access GObject_Record;
      Property_Name : String);
   --  Emits the "notify" signal, to signal every listener that the property
   --  has been changed.

   --------------
   -- Bindings --
   --------------

   type G_Binding_Record is new GObject_Record with private;
   type G_Binding is access all G_Binding_Record'Class;
   --  A binding is the representation of a binding between a property on a
   --  source object and another property on a target project. Whenever the
   --  source property changes, the same value is applied to the target
   --  property.
   --  It is possible to create a bidirectional binding between two properties
   --  so that if either property change, the other is updated as well (see
   --  the use of Binding_Bidirectional).
   --  It is also possible to set a custom transformation function (in both
   --  directions in case of a bidirectional binding) to apply a custom
   --  transformation from the source value to the target value before
   --  applying it.

   type Binding_Flags is mod Integer'Last;
   Binding_Default        : constant Binding_Flags := 0;
   Binding_Bidirectional  : constant Binding_Flags := 1;
   Binding_Sync_Create    : constant Binding_Flags := 2;
   Binding_Invert_Boolean : constant Binding_Flags := 4;
   --  Binding_Default is the default binding: if the source property changes,
   --     the target property is updated with its value.
   --  Binding_Bidirectional: if either property changes, the other property
   --     is updated.
   --  Binding_Sync_Create: synchronizes the values of the source and target
   --     properties when creating the binding; the direction of the
   --     synchronization is always from the source to the target.
   --  Binding_Invert_Boolean: if the two properties being bound are booleans
   --     setting one to True will result in the other being set to False and
   --     vice versa. The flag cannot be used when passing custom
   --     transformation functions.

   procedure Bind_Property
      (Source          : not null access GObject_Record'Class;
       Source_Property : String;
       Target          : not null access GObject_Record'Class;
       Target_Property : String;
       Flags           : Binding_Flags := Binding_Default);
   function Bind_Property
      (Source          : not null access GObject_Record'Class;
       Source_Property : String;
       Target          : not null access GObject_Record'Class;
       Target_Property : String;
       Flags           : Binding_Flags := Binding_Default)
       return G_Binding;
   --  Creates a binding between the source property and the target
   --  property on the two objects. Whenever the source property is changed,
   --  the target property is updated using the same value.
   --  For instance:
   --      Bind_Property (Action, "active", Widget, "sensitive");
   --  will result in the "sensitive" property of the widget instance to be
   --  updated with the same value of the "active" property of the action.
   --
   --  The binding is automatically removed when either the source or the
   --  target instances are finalized.
   --
   --  To remove the binding without affecting the source and target, you
   --  can just call Unref on the returned Binding.

   procedure Unbind (Self : not null access G_Binding_Record'Class);
   --  Explicitly releases the binding between the source and the target
   --  property.
   --  This function will release the reference that is being helf on the
   --  binding instance; if you want to hold on to the instance, you will
   --  need to hold a reference to it.

   ------------------------------
   -- Properties introspection --
   ------------------------------
   --  See glib.ads for more information on properties

   function Interface_List_Properties
     (Vtable : Interface_Vtable) return Glib.Param_Spec_Array;
   --  Return the list of properties of an interface (see also Glib.Properties)
   --  from a Vtable from Default_Interface_Peek).
   --  See also Class_List_Properties for a similar function for objects.

   function Class_List_Properties
     (Class : GObject_Class) return Glib.Param_Spec_Array;
   --  Return the list of all properties of the class.

   ---------------
   -- User_Data --
   ---------------
   --  This package allow you to associate your own Data to the C widgets. No
   --  type verification is made to check if you are using the correct
   --  matching Get function. This is your own responsability.
   --
   --  We recommend using this package only if you want your data to be
   --  available from your own C code. If you just want to access it from Ada,
   --  you should consider creating a new tagged type instead, that extends
   --  either GObject_Record or the specific widget type you need.

   --  <doc_ignore>

   generic
      type Data_Type (<>) is private;
   package User_Data is
      type On_Destroyed_Callback is access procedure (Data : Data_Type);
      --  On_Destroyed is called when the data is overridden in the object, by
      --  an other object with the same ID, or when the object itself is
      --  destroyed

      function Is_Set
        (Object : access GObject_Record'Class;
         Id     : String := "user_data") return Boolean;
      --  Whether the given user data was set on the object.

      function Get
        (Object : access GObject_Record'Class;
         Id     : String := "user_data") return Data_Type;
      --  Get the information associated with the key ID.
      --  Raise Gtkada.Types.Data_Error if there is none.

      function Get
        (Object  : access GObject_Record'Class;
         Id      : String := "user_data";
         Default : Data_Type) return Data_Type;
      --  Get the information associated with the key ID.
      --  Return Default instead of raising an exception if there is no such
      --  user data

      procedure Set
        (Object : access GObject_Record'Class;
         Data   : Data_Type;
         Id     : String := "user_data";
         On_Destroyed : On_Destroyed_Callback := null);
      --  Associate some new user data with the object.
      --  The strings starting with "gtkada_" are reserved for GtkAda's
      --  internal use, please avoid using them.

      procedure Remove
        (Object : access GObject_Record'Class; Id : String := "user_data");
      --  Remove some data from the object

      function Get
        (Object : access GObject_Record'Class;
         Id     : Glib.GQuark) return Data_Type;
      function Get
        (Object  : access GObject_Record'Class;
         Id      : Glib.GQuark;
         Default : Data_Type) return Data_Type;
      --  Same function as Get above, but uses directly the Quark associated
      --  with the string, which speeds up the access time significantly.

      procedure Set
        (Object : access GObject_Record'Class;
         Data   : Data_Type;
         Id     : Glib.GQuark;
         On_Destroyed : On_Destroyed_Callback := null);
      --  Same function as Set above, but uses directly the Quark associated
      --  with the string, which speeds up the access time significantly.

      procedure Remove
        (Object : access GObject_Record'Class; Id : Glib.GQuark);
      --  Same function as Remove above, but uses directly the Quark associated
      --  with the string, which speeds up the access time significantly.

   private
      --  <doc_ignore>
      procedure Free_Data (Data : System.Address);
      --  Internal procedure used to free user data in the package body
      pragma Convention (C, Free_Data);
      --  </doc_ignore>
   end User_Data;

   --  </doc_ignore>

   -----------
   -- Lists --
   -----------

   function Convert (W : GObject) return System.Address;
   function Convert (W : System.Address) return GObject;

   package Object_List is new Glib.GSlist.Generic_SList (GObject);
   package Object_Simple_List is new Glib.Glist.Generic_List (GObject);

   --  <doc_ignore>

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type);
   package User_Data_Closure is
      --  This package is meant for internal use in GtkAda application.
      --  It provides a convenient wrapper around user-provided data, to
      --  be passed to callbacks.

      type Data_Access is access all User_Data_Type;

      type Internal_Data is record
         Func       : System.Address;  --  The actual user callback
         Data       : Data_Access;
      end record;
      type Internal_Data_Access is access all Internal_Data;

      function Convert is new Ada.Unchecked_Conversion
         (System.Address, Internal_Data_Access);

      function Build
         (Func : System.Address; Data : User_Data_Type)
         return System.Address;
      pragma Inline (Build);
      --  Allocate a new user data.
      --  It returns an access to Internal_Data_Access, but in a form easier
      --  to pass to a C function.

      procedure Free_Data (Data : System.Address);
      pragma Convention (C, Free_Data);
      --  Callback suitable for calling from C, to free user data

   end User_Data_Closure;

   --  </doc_ignore>

private

   type GObject_Record is tagged record
      Ptr : System.Address := System.Null_Address;
   end record;

   type G_Binding_Record is new GObject_Record with null record;

   type Interface_Vtable is new Glib.C_Proxy;

   Null_GObject_Class : constant GObject_Class :=
      GObject_Class (System.Null_Address);
   Uninitialized_Class : constant Ada_GObject_Class := null;

   type Signal_Query is record
      Signal_Id    : Guint;
      Signal_Name  : System.Address;  --  const gchar*
      IType        : GType;
      Signal_Flags : Gint;            --  enum GSignalFlags
      Return_Type  : GType;
      N_Params     : Guint;
      Param_Types  : System.Address;  --  const gtype*
   end record;
   pragma Convention (C, Signal_Query);

   --  <doc_ignore>

   --  Note: the following functions and types should only be used
   --  for internal usage, not in the user's applications.
   --  If you use type inheritance for new widgets, you should not need
   --  these functions.

   GtkAda_String : constant String := "_GtkAda" & ASCII.NUL;
   GtkAda_String_Quark : Glib.GQuark := Glib.Unknown_Quark;
   --  The name for the user data that we set in the objects.
   --  The Quark version is to speed up the string lookup (this is done
   --  only once).

   --  </doc_ignore>

   pragma Inline (Get_Object);
   pragma Inline (Set_Object);
   pragma Import (C, Type_From_Class, "ada_type_from_class");
   pragma Import (C, Query, "g_signal_query");
   pragma Import (C, Id, "ada_gsignal_query_id");
   pragma Import (C, Return_Type, "ada_gsignal_query_return_type");
end Glib.Object;
