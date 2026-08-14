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

--  Provides a way to describe references to values.
--
--  An important aspect of expressions is that the value can be obtained from
--  a source that is several steps away. For example, an expression may
--  describe ‘the value of property A of `object1`, which is itself the value
--  of a property of `object2`'. And `object1` may not even exist yet at the
--  time that the expression is created. This is contrast to `GObject` property
--  bindings, which can only create direct connections between the properties
--  of two objects that must both exist for the duration of the binding.
--
--  An expression needs to be "evaluated" to obtain the value that it
--  currently refers to. An evaluation always happens in the context of a
--  current object called `this` (it mirrors the behavior of object-oriented
--  languages), which may or may not influence the result of the evaluation.
--  Use [methodGtk.Expression.evaluate] for evaluating an expression.
--
--  Various methods for defining expressions exist, from simple constants via
--  [ctorGtk.ConstantExpression.new] to looking up properties in a `GObject`
--  (even recursively) via [ctorGtk.PropertyExpression.new] or providing custom
--  functions to transform and combine expressions via
--  [ctorGtk.ClosureExpression.new].
--
--  Here is an example of a complex expression:
--
--  ```c color_expr = gtk_property_expression_new (GTK_TYPE_LIST_ITEM, NULL,
--  "item"); expression = gtk_property_expression_new (GTK_TYPE_COLOR,
--  color_expr, "name"); ```
--
--  when evaluated with `this` being a `GtkListItem`, it will obtain the
--  "item" property from the `GtkListItem`, and then obtain the "name" property
--  from the resulting object (which is assumed to be of type
--  `GTK_TYPE_COLOR`).
--
--  A more concise way to describe this would be
--
--  ``` this->item->name ```
--
--  The most likely place where you will encounter expressions is in the
--  context of list models and list widgets using them. For example,
--  `GtkDropDown` is evaluating a `GtkExpression` to obtain strings from the
--  items in its model that it can then use to match against the contents of
--  its search entry. `GtkStringFilter` is using a `GtkExpression` for similar
--  reasons.
--
--  By default, expressions are not paying attention to changes and evaluation
--  is just a snapshot of the current state at a given time. To get informed
--  about changes, an expression needs to be "watched" via a
--  [structGtk.ExpressionWatch], which will cause a callback to be called
--  whenever the value of the expression may have changed;
--  [methodGtk.Expression.watch] starts watching an expression, and
--  [methodGtk.ExpressionWatch.unwatch] stops.
--
--  Watches can be created for automatically updating the property of an
--  object, similar to GObject's `GBinding` mechanism, by using
--  [methodGtk.Expression.bind].
--
--  ## GtkExpression in GObject properties
--
--  In order to use a `GtkExpression` as a `GObject` property, you must use
--  the [funcGtk.param_spec_expression] when creating a `GParamSpec` to install
--  in the `GObject` class being defined; for instance:
--
--  ```c obj_props[PROP_EXPRESSION] = gtk_param_spec_expression ("expression",
--  "Expression", "The expression used by the widget", G_PARAM_READWRITE |
--  G_PARAM_STATIC_STRINGS | G_PARAM_EXPLICIT_NOTIFY); ```
--
--  When implementing the `GObjectClass.set_property` and
--  `GObjectClass.get_property` virtual functions, you must use
--  [funcGtk.value_get_expression], to retrieve the stored `GtkExpression` from
--  the `GValue` container, and [funcGtk.value_set_expression], to store the
--  `GtkExpression` into the `GValue`; for instance:
--
--  ```c // in set_property... case PROP_EXPRESSION: foo_widget_set_expression
--  (foo, gtk_value_get_expression (value)); break;
--
--  // in get_property... case PROP_EXPRESSION: gtk_value_set_expression
--  (value, foo->expression); break; ```
--
--  ## GtkExpression in .ui files
--
--  `GtkBuilder` has support for creating expressions. The syntax here can be
--  used where a `GtkExpression` object is needed like in a `<property>` tag
--  for an expression property, or in a `<binding name="property">` tag to bind
--  a property to an expression.
--
--  To create a property expression, use the `<lookup>` element. It can have a
--  `type` attribute to specify the object type, and a `name` attribute to
--  specify the property to look up. The content of `<lookup>` can either be a
--  string that specifies the name of the object to use, an element specifying
--  an expression to provide an object, or empty to use the `this` object.
--
--  Example:
--
--  ```xml <lookup name='search'>string_filter</lookup> ```
--
--  Since the `<lookup>` element creates an expression and its element content
--  can itself be an expression, this means that `<lookup>` tags can also be
--  nested. This is a common idiom when dealing with `GtkListItem`s. See
--  [classGtk.BuilderListItemFactory] for an example of this technique.
--
--  To create a constant expression, use the `<constant>` element. If the type
--  attribute is specified, the element content is interpreted as a value of
--  that type, and the initial attribute can be specified to get the initial
--  value for that type. Otherwise, it is assumed to be an object. For
--  instance:
--
--  ```xml <constant>string_filter</constant> <constant
--  type='gchararray'>Hello, world</constant> <constant type='gchararray'
--  initial='true' /> <!-- NULL --> ```
--
--  String (`type='gchararray'`) constants can be marked for translation with
--  the `translatable=` attribute, and will then be looked up in the
--  [propertyGtk.Builder:translation-domain] when the expression is
--  constructed.
--
--  ```xml <constant type='gchararray' translatable='yes'>I'm
--  translatable!</constant> ```
--
--  As with other translatable strings in [typeGtk.Builder], constants can
--  also have a context and/or translation comment:
--
--  ```xml <constant type='gchararray' translatable='yes' context='example'
--  comments='A sample string'>I'm translatable!</constant> ```
--
--  To create a closure expression, use the `<closure>` element. The
--  `function` attribute specifies what function to use for the closure, and
--  the `type` attribute specifies its return type. The content of the element
--  contains the expressions for the parameters. For instance:
--
--  ```xml <closure type='gchararray' function='combine_args_somehow'>
--  <constant type='gchararray'>File size:</constant> <lookup type='GFile'
--  name='size'>myfile</lookup> </closure> ```
--
--  If an expression can fail, a `<try>` element can be used to provide
--  fallbacks. The expressions are tried from top to bottom until one of them
--  succeeds. If none of the expressions succeed, the expression fails as
--  normal:
--
--  ```xml <try> <lookup type='GtkWindow' name='title'> <lookup
--  type='GtkLabel' name='root'></lookup> </lookup> <constant
--  type='gchararray'>Hello World</constant> </try> ```
--
--  To create a property binding, use the `<binding>` element in place of
--  where a `<property>` tag would ordinarily be used. The `name` and `object`
--  attributes are supported. The `name` attribute is required, and pertains to
--  the applicable property name. The `object` attribute is optional. If
--  provided, it will use the specified object as the `this` object when the
--  expression is evaluated. Here is an example in which the `label` property
--  of a `GtkLabel` is bound to the `string` property of another arbitrary
--  object:
--
--  ```xml <object class='GtkLabel'> <binding name='label'> <lookup
--  name='string'>some_other_object</lookup> </binding> </object> ```

pragma Warnings (Off, "*is already use-visible*");
with Ada.Finalization;     use Ada.Finalization;
with Glib;                 use Glib;
with Glib.Values;          use Glib.Values;
with Gtk.Expression_Watch; use Gtk.Expression_Watch;
with System;               use System;

package Gtk.Expression is

   type Gtk_Expression_Record is abstract new Ada.Finalization.Controlled with private;
   type Gtk_Expression is access all Gtk_Expression_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Expression_Notify is access procedure;
   --  Callback called by Gtk.Expression.Watch when the expression value
   --  changes.

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_expression_get_type");

   -------------
   -- Methods --
   -------------

   function Bind
      (Self     : Gtk_Expression;
       Target   : System.Address;
       Property : UTF8_String;
       This     : System.Address)
       return Gtk.Expression_Watch.Gtk_Expression_Watch;
   --  Bind `target`'s property named `property` to `self`. The value that
   --  `self` evaluates to is set via `g_object_set` on `target`. This is
   --  repeated whenever `self` changes to ensure that the object's property
   --  stays synchronized with `self`. If `self`'s evaluation fails, `target`'s
   --  `property` is not updated. Use a [classGtk.TryExpression] to provide a
   --  fallback for this case.
   --  @param Target the target object to bind to
   --  @param Property name of the property on `target` to bind to
   --  @param This the this argument for the evaluation of `self`
   --  @return a `GtkExpressionWatch`

   function Evaluate
      (Self  : Gtk_Expression;
       This  : System.Address;
       Value : in out Glib.Values.GValue) return Boolean;
   --  Evaluates the given expression and on success stores the result in
   --  Value.
   --  The `GType` of `value` will be the type given by
   --  [methodGtk.Expression.get_value_type].
   --  It is possible that expressions cannot be evaluated - for example when
   --  the expression references objects that have been destroyed or set to
   --  `NULL`. In that case `value` will remain empty and `FALSE` will be
   --  returned.
   --  @param This the this argument for the evaluation
   --  @param Value an empty `GValue`
   --  @return `TRUE` if the expression could be evaluated

   function Get_Value_Type (Self : Gtk_Expression) return GType;
   --  Gets the `GType` that this expression evaluates to.
   --  This type is constant and will not change over the lifetime of this
   --  expression.
   --  @return The type returned from [methodGtk.Expression.evaluate]

   function Is_Static (Self : Gtk_Expression) return Boolean;
   --  Checks if the expression is static.
   --  A static expression will never change its result when
   --  [methodGtk.Expression.evaluate] is called on it with the same arguments.
   --  That means a call to [methodGtk.Expression.watch] is not necessary
   --  because it will never trigger a notify.
   --  @return `TRUE` if the expression is static

   function Watch
      (Self   : Gtk_Expression;
       This   : System.Address;
       Notify : Gtk_Expression_Notify)
       return Gtk.Expression_Watch.Gtk_Expression_Watch;
   --  Watch the given `expression` for changes.
   --  The Notify function will be called whenever the evaluation of `self`
   --  may have changed.
   --  GTK cannot guarantee that the evaluation did indeed change when the
   --  Notify gets invoked, but it guarantees the opposite: When it did in fact
   --  change, the Notify will be invoked.
   --  @param This the `this` argument to watch
   --  @param Notify callback to invoke when the expression changes
   --  @return The newly installed watch. Note that the only reference held to
   --  the watch will be released when the watch is unwatched which can happen
   --  automatically, and not just via [methodGtk.ExpressionWatch.unwatch]. You
   --  should call [methodGtk.ExpressionWatch.ref] if you want to keep the
   --  watch around.

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Watch_User_Data is

      type Gtk_Expression_Notify is access procedure (User_Data : User_Data_Type);
      --  Callback called by Gtk.Expression.Watch when the expression value
      --  changes.
      --  @param User_Data data passed to Gtk.Expression.Watch

      function Watch
         (Self      : Gtk.Expression.Gtk_Expression;
          This      : System.Address;
          Notify    : Gtk_Expression_Notify;
          User_Data : User_Data_Type)
          return Gtk.Expression_Watch.Gtk_Expression_Watch;
      --  Watch the given `expression` for changes.
      --  The Notify function will be called whenever the evaluation of `self`
      --  may have changed.
      --  GTK cannot guarantee that the evaluation did indeed change when the
      --  Notify gets invoked, but it guarantees the opposite: When it did in
      --  fact change, the Notify will be invoked.
      --  @param This the `this` argument to watch
      --  @param Notify callback to invoke when the expression changes
      --  @param User_Data user data to pass to the `notify` callback
      --  @return The newly installed watch. Note that the only reference held
      --  to the watch will be released when the watch is unwatched which can
      --  happen automatically, and not just via
      --  [methodGtk.ExpressionWatch.unwatch]. You should call
      --  [methodGtk.ExpressionWatch.ref] if you want to keep the watch around.

   end Watch_User_Data;

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Create (Ptr: not null access System.Address) return Gtk_Expression_Record is abstract;

   type Dummy_Gtk_Expression_Record is new Gtk_Expression_Record with private;
   function Create (Ptr: not null access System.Address) return Dummy_Gtk_Expression_Record;

   type Gtk_Expression_Array is array (Natural range <>) of Gtk_Expression;

   type C_GtkExpressionArray is array (Natural range <>) of System.Address;
   pragma Convention (C, C_GtkExpressionArray);

   function To_C (Value : Gtk_Expression_Array) return C_GtkExpressionArray;
   --  Convert the array of expressions to C array with C expressions

   function Is_Created
      (Self : not null access Gtk_Expression_Record'Class) return Boolean;
   --  Retrn True if Self is initialized with Gtk object

   function Get_Object
      (Self : access Gtk_Expression_Record'Class) return System.Address;
   --  For internal usage. Do not call manualy.

   procedure Set_Object
      (Self   : not null access Gtk_Expression_Record'Class;
       Object : System.Address);
   --  For internal usage. Do not call manualy.

   function From_Object_Full_Ownership
      (Object : System.Address) return Gtk_Expression;
   --  For internal usage. Do not call manualy.

   function From_Object_None_Ownership
      (Object : System.Address) return Gtk_Expression;
   --  For internal usage. Do not call manualy.

private

   type Gtk_Expression_Record is abstract new Ada.Finalization.Controlled with record
      Ptr : System.Address := System.Null_Address;
   end record;

   overriding procedure Adjust (Object : in out Gtk_Expression_Record);
   overriding procedure Finalize (Object : in out Gtk_Expression_Record);

   type Dummy_Gtk_Expression_Record is new Gtk_Expression_Record with null record;

end Gtk.Expression;
