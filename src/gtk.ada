
with Gtk.Container;

package Gtk.Bin is

   type Gtk_Bin is new Gtk.Container.Gtk_Container with private;

private

   type Gtk_Bin is new Gtk.Container.Gtk_Container with null record;

end Gtk.Bin;

package body Gtk.Box is

   procedure Gtk_Pack_Start (Box     : in Gtk_Box'Class;
                             Widget  : in Gtk.Widget.Gtk_Widget'Class;
                             Expand  : in Boolean;
                             Fill    : in Boolean;
                             Padding : in GInt) is
      procedure Internal (Box     : System.Address;
                          Widget  : System.Address;
                          Expand  : GInt;
                          Fill    : GInt;
                          Padding : GInt);
      pragma Import (C, Internal, "gtk_box_pack_start");
   begin
      Internal (Get_Object (Box), Get_Object (Widget),
                Boolean'Pos (Expand), Boolean'Pos (Fill), Padding);
   end Gtk_Pack_Start;

end Gtk.Box;

with Gtk.Container;
with Gtk.Widget;

package Gtk.Box is

   type Gtk_Box is new Gtk.Container.Gtk_Container with private;

   procedure Gtk_Pack_Start (Box     : in Gtk_Box'Class;
                             Widget  : in Gtk.Widget.Gtk_Widget'Class;
                             Expand  : in Boolean;
                             Fill    : in Boolean;
                             Padding : in GInt);

   --  mapping: Gtk_Pack_Start gtkbox.h gtk_box_pack_start

private

   type Gtk_Box is new Gtk.Container.Gtk_Container with null record;

end Gtk.Box;

package body Gtk.Button is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Button) is
      function Gtk_New_Internal return System.Address;
      pragma Import (C, Gtk_New_Internal, "gtk_button_new");
   begin
      Set_Object (Widget, Gtk_New_Internal);
   end Gtk_New;

   --------------
   --  Gtk_New --
   --------------

   procedure Gtk_New (Widget : out Gtk_Button'Class;
                      Label  : in String) is
      function Gtk_New_Internal (S : String) return System.Address;
      pragma Import (C, Gtk_New_Internal, "gtk_button_new_with_label");
   begin
      Set_Object (Widget, Gtk_New_Internal (Label & Ascii.NUL));
   end Gtk_New;

end Gtk.Button;








with Gtk.Container;

package Gtk.Button is

   type Gtk_Button is new Gtk.Container.Gtk_Container with private;

   procedure Gtk_New (Widget : out Gtk_Button);
   procedure Gtk_New (Widget : out Gtk_Button'Class;
                      Label  : in String);

   --  mapping: Gtk_New gtkbutton.h gtk_button_new
   --  mapping: Gtk_New gtkbutton.h gtk_button_new_with_label

private

   type Gtk_Button is new Gtk.Container.Gtk_Container with null record;

end Gtk.Button;
with Unchecked_Conversion;

package body Gtk.Callbacks is

   package body Callback is

      ------------------------
      -- Gtk_Signal_Connect --
      ------------------------

      function Gtk_Signal_Connect
        (Object      : in Gtk_Object'Class;
         Signal_Name : in String;
         Func        : in Gtk_Callback;
         Data        : in Data_Type)
         return GUint
      is
         function Internal (Object : System.Address;
                            Name   : String;
                            Func   : Gtk_Callback;
                            Data   : System.Address) return GUint;
         pragma Import (C, Internal, "gtk_signal_connect");
         type Data_Type_Access is access all Data_Type;
         D : Data_Type_Access;

         function Convert is new Unchecked_Conversion (Data_Type_Access,
                                                       System.Address);
      begin
         D := new Data_Type'(Data);
         return Internal (Get_Object (Object), Signal_Name & Ascii.NUL,
                          Func, Convert (D));
      end Gtk_Signal_Connect;
   end Callback;

   ------------------------
   -- Gtk_Signal_Connect --
   ------------------------

   function Gtk_Signal_Connect
     (Object      : in Gtk_Object'Class;
      Signal_Name : in String;
      Func        : in Gtk_Void_Callback)
      return GUint
   is
      function Internal (Object : System.Address;
                         Name   : String;
                         Func   : Gtk_Void_Callback;
                         Data   : System.Address) return GUint;
      pragma Import (C, Internal, "gtk_signal_connect");
   begin
      return Internal (Get_Object (Object), Signal_Name & Ascii.NUL,
                       Func, System.Null_Address);
   end Gtk_Signal_Connect;

   -------------------------------
   -- Gtk_Signal_Connect_Object --
   -------------------------------

   function Gtk_Signal_Connect_Object
     (Object      : in Gtk_Object'Class;
      Signal_Name : in String;
      Func        : in Gtk_Signal_Func;
      Slot_Object : access Gtk.Widget.Gtk_Widget'Class)
      return GUint
   is
      function Internal (Object : System.Address;
                         Name   : String;
                         Func   : Gtk_Signal_Func;
                         Slot   : System.Address) return GUint;
      pragma Import (C, Internal, "gtk_signal_connect_object");
   begin
      return Internal (Get_Object (Object), Signal_Name & Ascii.NUL,
                       Func, Slot_Object.all'Address);
   end Gtk_Signal_Connect_Object;

end Gtk.Callbacks;
with Gtk; use Gtk;
with Gtk.Widget;

package Gtk.Callbacks is

   --  The following package is for callbacks requiring a data to
   --  be passed to the callback function

   generic
      type Data_Type is private;
      --  The type of the data for the callback

   package Callback is

      type Gtk_Callback is access procedure
        (Widget : in Gtk.Widget.Gtk_Widget'Class;
         Data   : access Data_Type);
      --  Callback function for Gtk_Signal_Connect below

      function Gtk_Signal_Connect
        (Object      : in Gtk_Object'Class;
         Signal_Name : in String;
         Func        : in Gtk_Callback;
         Data        : in Data_Type)
         return GUint;
      --  Connects a signal to a callback

      --  mapping: Gtk_Signal_Connect gtksignal.h gtk_signal_connect
   end Callback;


   --  The following functions are for callbacks requiring no data to be
   --  passed to the callback (except the Object itself)

   type Gtk_Void_Callback is access procedure
     (Widget : in Gtk.Widget.Gtk_Widget'Class);

   function Gtk_Signal_Connect
     (Object      : in Gtk_Object'Class;
      Signal_Name : in String;
      Func        : in Gtk_Void_Callback)
      return GUint;

   --  The following functions are for callbacks send to another
   --  object

   type Gtk_Signal_Func is access procedure
     (Object : in Gtk.Widget.Gtk_Widget'Class);

   function Gtk_Signal_Connect_Object
     (Object      : in Gtk_Object'Class;
      Signal_Name : in String;
      Func        : in Gtk_Signal_Func;
      Slot_Object : access Gtk.Widget.Gtk_Widget'Class)
      return GUint;
   --  mapping: Gtk_Signal_Connect_Object gtksignal.h gtk_signal_connect_object

end Gtk.Callbacks;


package body Gtk.Container is

   -------------
   -- Gtk_Add --
   -------------

   procedure Gtk_Add (Container : in Gtk_Container'Class;
                      Widget    : in Gtk.Widget.Gtk_Widget'Class) is
      procedure Internal (Container : System.Address;
                          Widget    : System.Address);
      pragma Import (C, Internal, "gtk_container_add");
   begin
      Internal (Get_Object (Container), Get_Object (Widget));
   end Gtk_Add;

   ----------------------
   -- Gtk_Border_Width --
   ----------------------

   procedure Gtk_Border_Width (Container    : in Gtk_Container'Class;
                               Border_Width : in GInt) is
      procedure Internal (Container     : System.Address;
                          Border_Widget : GInt);
      pragma Import (C, Internal, "gtk_container_border_width");
   begin
      Internal (Get_Object (Container), Border_Width);
   end Gtk_Border_Width;

end Gtk.Container;

with Gtk.Widget;

package Gtk.Container is

   type Gtk_Container is new Gtk.Widget.Gtk_Widget with private;

   procedure Gtk_Add (Container : in Gtk_Container'Class;
                      Widget    : in Gtk.Widget.Gtk_Widget'Class);

   procedure Gtk_Border_Width (Container    : in Gtk_Container'Class;
                               Border_Width : in GInt);

   --  mapping: Gtk_Add  gtkcontainer.h gtk_container_add
   --  mapping: Gtk_Border_Width gtkcontainer.h gtk_container_border_width

private

   type Gtk_Container is new Gtk.Widget.Gtk_Widget with null record;

end Gtk.Container;

package body Gtk.Hbox is

   procedure Gtk_New (Widget      : out Gtk_Hbox;
                      Homogeneous : in Boolean;
                      Spacing     : in GInt)
   is
      function Internal (Homogeneous : GInt;
                         Spacing     : GInt) return System.Address;
      pragma Import (C, Internal, "gtk_hbox_new");
   begin
      Set_Object (Widget, Internal (Boolean'Pos (Homogeneous),
                                    Spacing));
   end Gtk_New;
end Gtk.Hbox;

with Gtk.Box;

package Gtk.Hbox is

   type Gtk_Hbox is new Gtk.Box.Gtk_Box with private;

   procedure Gtk_New (Widget      : out Gtk_Hbox;
                      Homogeneous : in Boolean;
                      Spacing     : in GInt);
   --  mapping: Gtk_New gtkhbox.h gtk_hbox_new


private

   type Gtk_Hbox is new Gtk.Box.Gtk_Box with null record;

end Gtk.Hbox;
package body Gtk.Widget is

   --------------
   -- Gtk_Show --
   --------------

   procedure Gtk_Show (Widget : in Gtk_Widget'Class) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_show");
   begin
      Internal (Get_Object (Widget));
   end Gtk_Show;

   ------------------
   --  Gtk_Destroy --
   ------------------

   procedure Gtk_Destroy (Widget : in Gtk_Widget'Class) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_destroy");
   begin
      Internal (Get_Object (Widget));
   end Gtk_Destroy;
end Gtk.Widget;

--  The widget is the base of the tree for displayable objects.
--  (A displayable object is one which takes up some amount
--  of screen real estate). It provides a common base and interface
--  which actual widgets must adhere to.

package Gtk.Widget is

   --  Flags used by Gtk_Widget on top of Gtk_Object
   Gtk_TopLevel         : constant := 2 ** 4;
   Gtk_No_Window        : constant := 2 ** 5;
   Gtk_Realized         : constant := 2 ** 6;
   Gtk_Mapped           : constant := 2 ** 7;
   Gtk_Visible          : constant := 2 ** 8;
   Gtk_Sensitive        : constant := 2 ** 9;
   Gtk_Parent_Sensitive : constant := 2 ** 10;
   Gtk_Can_Focus        : constant := 2 ** 11;
   Gtk_Has_Focus        : constant := 2 ** 12;
   Gtk_Can_Default      : constant := 2 ** 13;
   Gtk_Has_Default      : constant := 2 ** 14;
   Gtk_Has_Grab         : constant := 2 ** 15;
   Gtk_Basic            : constant := 2 ** 16;
   Gtk_Reserved_3       : constant := 2 ** 17;
   Gtk_Rc_Style         : constant := 2 ** 18;

   type Gtk_Widget is new Gtk_Object with private;

   procedure Gtk_Show (Widget : in Gtk_Widget'Class);

   procedure Gtk_Destroy (Widget : in Gtk_Widget'Class);

private

   type Gtk_Widget is new Gtk_Object with null record;

   --  mapping: Gtk_TopLevel gtkwidget.h GTK_TOPLEVEL
   --  mapping: Gtk_No_Window gtkwidget.h GTK_NOWINDOW
   --  mapping: Gtk_Realized gtkwidget.h GTK_REALIZED
   --  mapping: Gtk_Mapped gtkwidget.h GTK_MAPPED
   --  mapping: Gtk_Visible gtkwidget.h GTK_VISIBLE
   --  mapping: Gtk_Sensitive gtkwidget.h GTK_SENSITIVE
   --  mapping: Gtk_Parent_Sensitive gtkwidget.h GTK_PARENT_SENSITIVE
   --  mapping: Gtk_Can_Focus gtkwidget.h GTK_CAN_FOCUS
   --  mapping: Gtk_Has_Focus gtkwidget.h GTK_HAS_FOCUS
   --  mapping: Gtk_Can_Default gtkwidget.h GTK_CAN_DEFAULT
   --  mapping: Gtk_Has_Default gtkwidget.h GTK_HAS_DEFAULT
   --  mapping: Gtk_Has_Grab gtkwidget.h GTK_HAS_GRAB
   --  mapping: Gtk_Basic gtkwidget.h GTK_BASIC
   --  mapping: Gtk_Reserved_3 gtkwidget.h GTK_RESERVED_3
   --  mapping: Gtk_Rc_Style gtkwidget.h GTK_RC_STYLE
   --  mapping: Gtk_Show gtkwidget.h gtk_widget_show
   --  mapping: Gtk_Destroy gtkwidget.h gtk_widget_destroy

end Gtk.Widget;
package body Gtk.Window is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget   : out Gtk_Window;
                      The_Type : in  Gtk_Window_Type) is
      function Internal (T : in Integer) return System.Address;
      pragma Import (C, Internal, "gtk_window_new");
   begin
      Set_Object (Widget, Internal (Gtk_Window_Type'Pos (The_Type)));
   end Gtk_New;

   -------------------
   -- Gtk_Set_Title --
   -------------------

   procedure Gtk_Set_Title (Window : in Gtk_Window;
                            Title  : in String) is
      procedure Internal (W : in System.Address;
                          T : in String);
      pragma Import (C, Internal, "gtk_window_set_title");
   begin
      Internal (Get_Object (Window), Title & Ascii.NUL);
   end Gtk_Set_Title;


end Gtk.Window;

with Gtk.Bin;

package Gtk.Window is

   type Gtk_Window_Type is (Gtk_Window_Toplevel,
                            Gtk_Window_Dialog,
                            Gtk_Window_Popup);

   type Gtk_Window is new Gtk.Bin.Gtk_Bin with private;

   procedure Gtk_New (Widget   : out Gtk_Window;
                      The_Type : in  Gtk_Window_Type);

   procedure Gtk_Set_Title (Window : in Gtk_Window;
                            Title  : in String);

   --  mapping: Gtk_Window_Type gtkenums.h GtkWindowType
   --  mapping: Gtk_New gtkwindow.h gtk_window_new
   --  mapping: Gtk_Set_Title gtkwindow.h gtk_window_set_title

private

   type Gtk_Window is new Gtk.Bin.Gtk_Bin with null record;

end Gtk.Window;

with System;

package body Gtk is

   type Gtk_Object_Internal is new Integer;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object (Object : in Gtk_Object'Class)
                        return System.Address is
   begin
      return Object.Ptr;
   end Get_Object;

   --------------
   -- Gtk_Init --
   --------------

   procedure Gtk_Init is
      procedure Internal (Arg  : System.Address;
                          Argv : System.Address);
      pragma Import (C, Internal, "gtk_init");
      Argc : Integer := 0;
   begin
      Internal (Argc'Address, System.Null_Address);
   end Gtk_Init;

   --------------
   -- Gtk_Main --
   --------------

   procedure Gtk_Main is
      procedure Internal;
      pragma Import (C, Internal, "gtk_main");
   begin
      Internal;
   end Gtk_Main;

   -------------------
   -- Gtk_Main_Quit --
   -------------------

   procedure Gtk_Main_Quit is
      procedure Internal;
      pragma Import (C, Internal, "gtk_main_quit");
   begin
      Internal;
   end Gtk_Main_Quit;

   ----------------
   -- Set_Object --
   ----------------

   procedure Set_Object (Object : in out Gtk_Object'Class;
                         Value  : in     System.Address) is
      use type System.Address;
   begin
      Object.Ptr := Value;
   end Set_Object;

end Gtk;

with System;

package Gtk is

   subtype GInt  is Integer;
   subtype GUint is Positive;
   subtype GInt32 is Integer range -(2 ** 16) .. (2 ** 16 - 1);
   --  Same for all basic types

   type Gtk_Object is tagged private;

   procedure Gtk_Init;
   --  How can we pass the command line arguments ?

   procedure Gtk_Main;

   procedure Gtk_Main_Quit;

   --  mapping: Gtk_Init gtkmain.h gtk_init
   --  mapping: Gtk_Main gtkmain.h gtk_main
   --  mapping: Gtk_Main_Quit gtkmain.h gtk_main_quit

private

   type Gtk_Object is tagged
     record
        Ptr : System.Address := System.Null_Address;
     end record;

   function Get_Object (Object : in Gtk_Object'Class)
                        return System.Address;
   pragma Inline (Get_Object);

   procedure Set_Object (Object : in out Gtk_Object'Class;
                         Value  : in     System.Address);
   pragma Inline (Set_Object);

end Gtk;

with Test;

procedure Main is
begin
   Test.Main;
end Main;
with Gtk; use Gtk;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk.Container; use Gtk.Container;
with Gtk.Callbacks; use Gtk.Callbacks;
with Gtk.Button; use Gtk.Button;
with Gtk.Box;    use Gtk.Box;
with Gtk.Hbox;   use Gtk.Hbox;
with Ada.Text_IO;

package body Test is

   subtype String7 is String (1 .. 7);

   procedure Hello (Widget : in Gtk.Widget.Gtk_Widget'Class;
                    S      : access String7);
   procedure Destroy (Object : in Gtk.Widget.Gtk_Widget'Class);

   -----------
   -- Hello --
   -----------

   procedure Hello (Widget : in Gtk.Widget.Gtk_Widget'Class;
                    S      : access String7) is
   begin
      Ada.Text_IO.Put_Line ("Hello World  => String was=" & S.all);
   end Hello;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Object : in Gtk.Widget.Gtk_Widget'Class) is
   begin
      Ada.Text_IO.Put_Line ("Destroy");
      Gtk_Main_Quit;
   end Destroy;

   ----------
   -- Main --
   ----------

   procedure Main is

      package String_Cb is new Callback (String7);

      A_Window : aliased Gtk.Window.Gtk_Window;
      A_Button : Gtk.Button.Gtk_Button;
      A_Box    : Gtk.Hbox.Gtk_Hbox;
      Cb_Id    : GInt;
   begin
      Gtk_Init;
      --  Initialize the library (how can we pass the command line arguments ?)

      --  Create the window  (window = gtk_window_new (GTK_WINDOW_TOPLEVEL))
      Gtk_New (A_Window, Gtk_Window_Toplevel);
      Gtk_Set_Title (A_Window, "Hello buttons");
      Gtk_Border_Width (A_Window, 10);
      Cb_Id := Gtk_Signal_Connect (A_Window, "destroy", Destroy'Access);

      --  Create the box to store the buttons
      Gtk_New (A_Box, False, 0);
      Gtk_Add (A_Window, A_Box);

      --  Create the first button
      Gtk_New (A_Button, Label => "Button1");
      Cb_Id := String_Cb.Gtk_Signal_Connect (A_Button, "clicked",
                                             Hello'Access, "Button1");
      Gtk_Pack_Start (A_Box, A_Button, True, True, 0);
      Gtk_Show (A_Button);

      --  Create the second button
      Gtk_New (A_Button, Label => "Button2");
      Cb_Id := String_Cb.Gtk_Signal_Connect (A_Button, "clicked",
                                             Hello'Access, "Button2");
      Gtk_Pack_Start (A_Box, A_Button, True, True, 0);
      Gtk_Show (A_Button);

      --  Show the box
      Gtk_Show (A_Box);

      Gtk_Show (A_Window);

      Gtk_Main;
      --  Call the main loop   (gtk_main)
   end Main;

end Test;

package Test is

   procedure Main;

end Test;
