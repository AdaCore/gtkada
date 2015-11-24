.. _Object-oriented_features:

************************
Object-oriented features
************************

GtkAda has been designed from the beginning to provide a full object
oriented layer over gtk+. This means that features such as type
extension and dynamic dispatching are made available through the
standard Ada language.

This section will describe how things work, how you can extend existing
widgets, and even how to create your own widgets.

.. _General_description_of_the_tagged_types:

General description of the tagged types
=======================================


Why should I use object-oriented programming ?
----------------------------------------------

Every widget in the `Gtk.*` packages in GtkAda is a tagged type with a number
of primitive subprograms that are inherited by all of its children.  Tagged
types in Ada make it possible to perform safe, automatic type conversions
without using explicit casts (such as is necessary when coding in C). It is
also possible for the compiler to verify whether or not these type conversions
are valid. Most errors are found at compile time, which leads to a safer and
more robust application.

As a further example, imagine a table that has been populated by some widgets.
It is possible to query for this table's children and operate on these widgets
without knowing details about their type, their creator, and so on--the tagged
objects that are returned contain all the information necessary.  It becomes
possible to use dynamic dispatching without ever having to cast to a known
type.

Modifying a standard widget to draw itself differently or display different
data is easy using tagged types.  Simply create a new type that extends the
current one (see the section :ref:`Using_tagged_types_to_extend_Gtk_widgets`
below.

Creating a new reusable widget from scratch is also possible.  Create a new
tagged type and specify properties of the widget--such as how it is to draw
itself and how it should react to events.  See the section
:ref:`Creating_new_widgets_in_Ada` below.

Object oriented programming through the use of Ada tagged types makes GtkAda a
very powerful, flexible, and safe tool for designing graphical interfaces.

Type conversions from C to Ada widgets
--------------------------------------

There are three kinds of widgets that you can use with GtkAda:

* *Ada widgets*:
  These are widgets that are written directly in Ada, using the object
  oriented features of GtkAda

* *Standard widgets*:
  These are the widgets that are part of the standard gtk+ and GtkAda
  distributions. This include all the basic widgets you need to build
  advanced interfaces.

* *third party C widgets*
  These are widgets that were created in C, and for which you (or someone else)
  created an Ada binding. This is most probably the kind of widgets you will
  have if you want to use third party widgets.

GtkAda will always be able to find and/or create a valid tagged type in the
first two cases, no matter if you explicitly created the widget or if it was
created automatically by gtk+. For instance, if you created a widget in Ada,
put it in a table, and later on extracted it from the table, then you will
still have the same widget.

In the third case (third party C widgets), GtkAda is not, by default, able to
create the corresponding Ada type.

The case of third party C widgets is a little bit trickier. Since GtkAda does
not know anything about them when it is built, it can't magically convert the C
widgets to Ada widgets. This is your job to teach GtkAda how to do the
conversion.

We thus provide a 'hook' function which you need to modify. This function is
defined in the package **Glib.Type_Conversion**. This function takes a string
with the name of the C widget (ex/ "GtkButton"), and should return a newly
allocated pointer. If you don't know this type either, simply return **null**.

.. _Using_tagged_types_to_extend_Gtk_widgets:

Using tagged types to extend Gtk widgets
========================================

.. highlight:: ada

With this toolkit, it's possible to associate your own data with existing
widgets simply by creating new types. This section will show you a simple
example, but you should rather read the source code in the :file:`testgtk/`
directory where we used this feature instead of using `user_data` as is used in
the C version:::

  type My_Button_Record is new Gtk_Button_Record with record
      --  whatever data you want to associate with your button
  end record;
  type My_Button is access all My_Button_Record'Class;

With the above statements, your new type is defined. Every function
available for `Gtk_Button` is also available for `My_Button`.
Of course, as with every tagged type in Ada, you can create your own
primitive functions with the following prototype::

  procedure My_Primitive_Func (Myb : access My_Button_Record);

To instanciate an object of type `My_Button` in your application, do
the following::

  declare
     Myb : My_Button;
  begin
     Myb := new My_Button_Record;
     Initialize (Myb);   --  from Gtk.Button
  end;

The first line creates the Ada type, whereas the `Initialize` call
actually creates the C widget and associates it with the Ada type.

.. _Creating_new_widgets_in_Ada:

Creating new widgets in Ada
===========================

With GtkAda, you can create widgets directly in Ada. These new
widgets can be used directly, as if they were part of gtk itself.

Creating new widgets is a way to create reuseable components. You can apply to
them the same functions as you would for any other widget, such as `Show`,
`Hide`, and so on.

This section will explain how to create two types of widgets: composite widgets
and widgets created from scratch. Two examples are provided with GtkAda, in the
directories :file:`examples/composite_widget` and :file:`examples/base_widget`.
Please also refer to the gtk+ tutorial, which describes the basic mechanisms
that you need to know to create a widget.

.. _Creating_composite_widgets:

Creating composite widgets
--------------------------

A composite widget is a widget that does not do much by itself. Rather, this is
a collection of subwidgets grouped into a more general entity.  For instance,
among the standard widgets, `Gtk_File_Selection` and `Gtk_Font_Selection`
belong to this category.

The good news is that there is nothing special to know. Just create a new
tagged type, extending one of the standard widgets (or even another of your own
widgets), provide a `Gtk_New` function that allocates memory for this widget,
and call the `Initialize` function that does the actual creation of the widget
and the subwidgets.  There is only one thing to do: `Initialize` should call
the parent class's `Initialize` function, to create the underlying C widget.

The example directory :file:`examples/composite_widget` reimplements the
`Gtk_Dialog` widget as written in C by the creators of gtk+.

.. _Creating_widgets_from_scratch:

Creating widgets from scratch
-----------------------------

Creating a working widget from scratch requires a certain level of familiary
with the GtkAda signal mechanism and entails much work with low level signals.
This is therefore not an activity recommended for novice GtkAda programmers.

Creating a widget from scratch is what you want to do if your widget should be
drawn in a special way, should create and emit new signals, or otherwise
perform differently than pre-existing widgets.  The example we give in
:file:`examples/base_widget` is a small target on which the user can click, and
that sends one of two signals: "bullseye" or "missed", depending on where the
user has clicked.

See also the example in :file:`examples/tutorial/gtkdial` for a more complex
widget, that implements a gauge where the user can move the arrow to select
a new value.

Since we are creating a totally new widget from scratch, with potentially
its own signals, we need to do slightly more work. In particular, we need to
provide a function ``Get_Type`` similar to what all the predefined widgets
provide::

   with Glib.Properties.Creation;   use Glib.Properties.Creation;
   with Glib.Objects;               use Glib.Objects;
   with Gtk.Scrollable;
   with System;

   package body My_Widgets is
      type My_Widget_Record is new Gtk_Button_Record with record
          ...
      end record;
      type My_Widget is access all My_Widget_Record'Class;

      Klass : aliased Ada_GObject_Class := Uninitialized_Class;

      PROP_H_ADJ : constant Property_Id := 1;
      PROP_V_ADJ : constant Property_Id := 2;
      --  internal identifier for our widget properties

      procedure Class_Init (Self : GObject_Class);
      pragma Convention (C, Class_Init);

      procedure Class_Init (Self : GObject_Class) is
      begin
         --  Set properties handler
         Set_Properties_Handlers (Self, Prop_Set'Access, Prop_Get'Access);
   
         --  Override inherited properties
         Override_Property  (Self, PROP_H_ADJ, "hadjustment");
         Override_Property  (Self, PROP_V_ADJ, "vadjustment");
   
         --  Install some custom style properties
         Install_Style_Property (Self, Gnew_Int (...));
   
         --  Override some the inherited methods (how to draw the widget)
         Set_Default_Draw_Handler (Self, On_Draw'Access);

         --  Override the primitives to compute the widget size
         Set_Default_Get_Preferred_Width (Self, ...);
      end Class_Init;
      
      function Get_Type return GType is
         Info : access GInterface_Info;
      begin
         if Initialize_Class_Record
            (Ancestor     => Gtk.Button.Get_Type,
             Class_Record => Klass'Access,
             Type_Name    => "My_Widget",
             Class_Init   => Class_Init)
         begin
            --  Add interfaces if needed
            Info := new GInterface_Info'(null, null, System.Address);
            Add_Interface (Klass, Gtk.Scrollable.Get_Type, Info);
         end if;
         return Klass.The_Type;
      end Get_Type;
   end My_Widgets;

You should also create the usual functions ``Gtk_New`` and
``Initialize``::

   procedure Gtk_New (Self : out My_Widget) is
   begin
      Self := new My_Widget_Record;  --  create the Ada wrapper
      Initialize (Self);
   end Gtk_New;
   
   procedure Initialize (Self : not null access My_Widget_Record'Class) is
   begin
      G_New (Self, Get_Type); --  allocate the C widget, unless done
   
      --  Initialize parent fields.

      Gtk.Button.Initialize (Self);
   
      --  Initialization of the Ada types

      Self.Field1 := ...;
   end Initialize;

  
In the above example, the new part is the ``Get_Type`` subprogram. It takes
three or four arguments:

* `Ancestor`
  This is the GType for the ancestor that is being extended.

* `Signals`
  This is an array of string access containing the name of the signals
  you want to create. For instance, you could create Signals with::

    Signals : Gtkada.Types.Chars_Ptr_Array := "bullseye" + "missed";
    
  This will create two signals, named "bullseye" and "missed", whose callbacks'
  arguments can be specified with the fourth parameter.

* `Class_Record`
  Every widget in C is associated with three records:

  - An instance of `GType`, which is a unique identifier (integer) for all
    the class of widgets defined in the framework. This description also
    contains the name of the class, its parent type, the list of
    interfaces it inherits, and all the signals is defines.

    These `GType` are often created early on when an application is launched,
    and provide the basic introspection capabilities in a gtk+ application.

    In Ada, this type is created by the function `Get_Type` in the example
    above (which is why we need to add the interface in that function).

  - An instance of `GObject_Class`, which contains implementation details
    for the class, defines the default signal handlers (how to draw a
    widget of the class, how to handle size negociation,...), and defines
    any number of properties that can be configured on the widget
    (properties are a generic interface to access the components of a
    composite widget, as well as some of its behavior -- they can be
    modified through introspection for instance in a GUI builder).

    Such a type is created automatically by gtk+ just before it creates
    the first instance of that widget type. It will then immediately call
    the `Class_Init` function that might have been passed to
    `Glib.Object.Initialize_Class_Record`. At that point, you can add your
    own new properties, or override the default signal handlers to redirect
    them to your own implementation.

   - A class instance record; there is one such record for each widget of
     that type.
     In GtkAda, the 'instance record' is simply your tagged type and its
     fields. It is created when you call any of the `Gtk_New` functions.

* `Parameters`
  This fourth argument is in fact optional, and is used to specify which
  kind of parameters each new signal is expecting.
  By default (ie if you don't give any value for this parameter), all the
  signals won't expect any argument, except of course a possible user_data.
  However, you can decide for instance that the first signal ("bullseye") should
  in fact take a second argument (say a Gint), and that "missed" will take
  two parameters (two Gints).

  `Parameters` should thus contain a value of::

    (1 => (1 => Gtk_Type_Int, 2 => Gtk_Type_None),
     2 => (1 => Gtk_Type_Int, 2 => Gtk_Type_Int));

  Due to the way arrays are handled in Ada, each component must have the same
  number of signals. However, if you specify a type of `Gtk_Type_None`, this
  will in fact be considered as no argument. Thus, the first signal above has
  only one parameter.

  Note also that to be able to emit a signal such a the second one, ie with
  multiple arguments, you will have to extend the packages defined in
  Gtk.Handlers. By default, the provided packages can only emit up to one
  argument (and only for a few specific types). Creating your own
  `Emit_By_Name` subprograms should not be hard if you look at what is done
  in :file:`gtk-marshallers.adb`. Basically, something like::

    procedure Emit_With_Two_Ints
      (Object : access Widget_Type'Class;
       Name   : String;
       Arg1   : Gint;
       Arg2   : Gint);
    pragma Import (C, Emit_With_Two_Ints,
        "gtk_signal_emit_by_name");

    Emit_With_Two_Ints (Gtk.Get_Object (Your_Widget),
        "missed" & ASCII.NUL, 1, 2);

  will emit the "missed" signal with the two parameters 1 and 2.
