
with Ada.Text_IO;    use Ada.Text_IO;
with Unchecked_Deallocation;
with Interfaces.C.Strings;
with Gdk.Color;      use Gdk.Color;
with Gdk.Event;      use Gdk.Event;
with Gdk.Types;      use Gdk.Types;
with Gdk.Window;     use Gdk.Window;
with System;         use System;
with Gtk.Container;  use Gtk.Container;
with Gtk.Enums;      use Gtk.Enums;
with Gtk.Widget;     use Gtk.Widget;
with Gtk.Window;     use Gtk.Window;
with Gtk.Box;        use Gtk.Box;
with Gtk.Button;     use Gtk.Button;
with Gtk.Handlers;   use Gtk.Handlers;
with Gtk.Main;       use Gtk.Main;
with Gtk.Label;      use Gtk.Label;
with Gtk.List;       use Gtk.List;
with Gtk.List_Item;  use Gtk.List_Item;
with Gtk.Pixmap;     use Gtk.Pixmap;
with Gdk.Pixmap;     use Gdk.Pixmap;
with Gdk.Bitmap;     use Gdk.Bitmap;
with Gtk.Toolbar;    use Gtk.Toolbar;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.File_Selection; use Gtk.File_Selection;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;

with Gtk.Type_Conversion;
pragma Warnings (Off, Gtk.Type_Conversion);

package body Gtk.Macro is

   package ICS renames Interfaces.C.Strings;
   Load_Xpm : ICS.chars_ptr_array :=
     (ICS.New_String ("24 15 15 1"),
      ICS.New_String (".      c #020204"),
      ICS.New_String ("+      c #0D77CC"),
      ICS.New_String ("@      c #0C0C0E"),
      ICS.New_String ("#      c #284C6C"),
      ICS.New_String ("$      c #949596"),
      ICS.New_String ("%      c #2A2C2C"),
      ICS.New_String ("&      c #4E5151"),
      ICS.New_String ("*      c #1A4264"),
      ICS.New_String ("=      c #7C7D7E"),
      ICS.New_String ("-      c #353B41"),
      ICS.New_String (";      c #5B5D5D"),
      ICS.New_String (">      c #37444E"),
      ICS.New_String (",      c #2F3639"),
      ICS.New_String ("'      c #078BEF"),
      ICS.New_String (")      c #1A72B4"),
      ICS.New_String ("........................"),
      ICS.New_String (".&&&&&;&;&;&;;&;&;&;;;,."),
      ICS.New_String (".;;;;;;;;;;;;;;;;;;;&&&."),
      ICS.New_String (".&,-,------%,,,%%,%%%%%."),
      ICS.New_String (".;&&&&&&>>>--,--,-,,%%@."),
      ICS.New_String (".=&&&&&&&->#)*-,,,,,%%%."),
      ICS.New_String (".=&&&&&>>>+'''+-,,,%%%@."),
      ICS.New_String (".=&&&&&>)+'+++++),%%%%%."),
      ICS.New_String (".$&&&&&-##******%%%%%%@."),
      ICS.New_String (".$&&&&>>)+)+)+)+)%%%%%%."),
      ICS.New_String (".$&&&-&--*,,%*%%%%%@%%@."),
      ICS.New_String (".$;-&>---,%%%%%%%%%%%%%."),
      ICS.New_String (".$;&&>>-%,%%%%%@%%%%%%%."),
      ICS.New_String (".@@@.@.................."),
      ICS.New_String ("........@..............."));
   Save_Xpm : ICS.chars_ptr_array :=
     (ICS.New_String ("24 15 15 1"),
      ICS.New_String (".      c #020204"),
      ICS.New_String ("+      c #0D77CC"),
      ICS.New_String ("@      c #0C0C0E"),
      ICS.New_String ("#      c #284C6C"),
      ICS.New_String ("$      c #949596"),
      ICS.New_String ("%      c #2A2C2C"),
      ICS.New_String ("&      c #4E5151"),
      ICS.New_String ("*      c #1A4264"),
      ICS.New_String ("=      c #7C7D7E"),
      ICS.New_String ("-      c #353B41"),
      ICS.New_String (";      c #5B5D5D"),
      ICS.New_String (">      c #37444E"),
      ICS.New_String (",      c #2F3639"),
      ICS.New_String ("'      c #078BEF"),
      ICS.New_String (")      c #1A72B4"),
      ICS.New_String ("........@..............."),
      ICS.New_String (".@@@.@.................."),
      ICS.New_String (".$;&&>>-%,%%%%%@%%%%%%%."),
      ICS.New_String (".$;-&>---,%%%%%%%%%%%%%."),
      ICS.New_String (".$&&&-&--*,,%*%%%%%@%%@."),
      ICS.New_String (".$&&&&>>)+)+)+)+)%%%%%%."),
      ICS.New_String (".$&&&&&-##******%%%%%%@."),
      ICS.New_String (".=&&&&&>)+'+++++),%%%%%."),
      ICS.New_String (".=&&&&&>>>+'''+-,,,%%%@."),
      ICS.New_String (".=&&&&&&&->#)*-,,,,,%%%."),
      ICS.New_String (".;&&&&&&>>>--,--,-,,%%@."),
      ICS.New_String (".&,-,------%,,,%%,%%%%%."),
      ICS.New_String (".;;;;;;;;;;;;;;;;;;;&&&."),
      ICS.New_String (".&&&&&;&;&;&;;&;&;&;;;,."),
      ICS.New_String ("........................"));
   Play_Xpm : ICS.chars_ptr_array :=
     (ICS.New_String ("24 15 15 1"),
      ICS.New_String (".      c #020204"),
      ICS.New_String ("+      c #077DE2"),
      ICS.New_String ("@      c #020604"),
      ICS.New_String ("#      c #1279C8"),
      ICS.New_String ("$      c #0A0A0C"),
      ICS.New_String ("%      c #939495"),
      ICS.New_String ("&      c #2A2C2C"),
      ICS.New_String ("*      c #264E74"),
      ICS.New_String ("=      c #263D51"),
      ICS.New_String ("-      c #4A4A4C"),
      ICS.New_String (";      c #666768"),
      ICS.New_String (">      c #545656"),
      ICS.New_String (",      c #373939"),
      ICS.New_String ("'      c #404447"),
      ICS.New_String (")      c #202322"),
      ICS.New_String ("........................"),
      ICS.New_String (".>>>>>>>>>>>>>>>>>>>>>&."),
      ICS.New_String (".>>>>>;;;;;;;;;;>;>;>>>."),
      ICS.New_String (".>,,,,,&,,&,&,&&,&&&&))."),
      ICS.New_String (".;>-------'',',,',,,&&)."),
      ICS.New_String ("@;->>>>>-''',,',,,,,&))."),
      ICS.New_String (".%>->---''#+*=,&,,&&&&)."),
      ICS.New_String (".%>>>>>-'=++++==,&,&&))."),
      ICS.New_String (".%-->---,=++++##&&&&&))."),
      ICS.New_String (".%>>>-'''*++##=&&&&&)&)."),
      ICS.New_String (".%>---'',=##*&))&)&&)))."),
      ICS.New_String (".%;'-''',=&=)&&&&)&)&&)."),
      ICS.New_String (".%;>-',,&&&&)&)&&)&&&,)."),
      ICS.New_String (".@@@@$@@@@...@.........."),
      ICS.New_String (".@...@...@.............."));
   Record_Xpm : ICS.chars_ptr_array :=
     (ICS.New_String ("24 15 15 1"),
      ICS.New_String (".      c #020204"),
      ICS.New_String ("+      c #F6020C"),
      ICS.New_String ("@      c #2A2A2C"),
      ICS.New_String ("#      c #5F6161"),
      ICS.New_String ("$      c #AE0A1C"),
      ICS.New_String ("%      c #535555"),
      ICS.New_String ("&      c #F65A6C"),
      ICS.New_String ("*      c #3E464C"),
      ICS.New_String ("=      c #959697"),
      ICS.New_String ("-      c #222624"),
      ICS.New_String (";      c #223346"),
      ICS.New_String (">      c #222224"),
      ICS.New_String (",      c #868688"),
      ICS.New_String ("'      c #2F3231"),
      ICS.New_String (")      c #373B3F"),
      ICS.New_String ("........................"),
      ICS.New_String (".%%%%%%%%%%%%%%%%%%%#%'."),
      ICS.New_String (".%%%%##############%%#%."),
      ICS.New_String (".%))))))))))'''''''@@>>."),
      ICS.New_String (".#%*%*%**)))))))'))''->."),
      ICS.New_String (".,*%%*%%***)))))'))''->."),
      ICS.New_String (".,%%%%%)**&&&&*;''''@->."),
      ICS.New_String (".,%%%%**)&++++$;''''@->."),
      ICS.New_String (".,%%%%**)&++++$;@@@@@>>."),
      ICS.New_String (".=%%%***;&++++$@-@@@@->."),
      ICS.New_String (".=*%*%)*)*$$$$*;-@@--->."),
      ICS.New_String (".=#***));;;;;;;@@@-@@->."),
      ICS.New_String (".=#%%*))''-------@-@@)-."),
      ICS.New_String ("........................"),
      ICS.New_String ("........................"));
   Step_Xpm : ICS.chars_ptr_array :=
     (ICS.New_String ("24 15 15 1"),
      ICS.New_String (".      c #020204"),
      ICS.New_String ("+      c #077DE2"),
      ICS.New_String ("@      c #0A0A0C"),
      ICS.New_String ("#      c #1277C1"),
      ICS.New_String ("$      c #212423"),
      ICS.New_String ("%      c #225684"),
      ICS.New_String ("&      c #20496E"),
      ICS.New_String ("*      c #959697"),
      ICS.New_String ("=      c #283D50"),
      ICS.New_String ("-      c #868688"),
      ICS.New_String (";      c #646566"),
      ICS.New_String (">      c #434A4D"),
      ICS.New_String (",      c #2C2F31"),
      ICS.New_String ("'      c #3B3E40"),
      ICS.New_String (")      c #565958"),
      ICS.New_String ("........................"),
      ICS.New_String (".>))>))))))))))))))))),."),
      ICS.New_String (".))))));););;);;))));))."),
      ICS.New_String (".),'''',',',',,,,,,,$$$."),
      ICS.New_String (".;>>>>>'>''''''''',,,$$."),
      ICS.New_String (".-))>)>>>>'''''','',,$$."),
      ICS.New_String (".->))>>>#+%&',=#,,,',$$."),
      ICS.New_String (".-)>))'&+++#&,=#=,,,$,$."),
      ICS.New_String (".*)>)>''++++##=#=$,,$$$."),
      ICS.New_String (".-))>>>%+++#==,#,,$$,$$."),
      ICS.New_String (".*>)>>''+#&=$$=#$$,$$$$."),
      ICS.New_String (".*)>>'''==$$,$$,,,$,$,$."),
      ICS.New_String (".*;)>>',,,,,$,,$$$$,,,$."),
      ICS.New_String (".@@@.@@...@............."),
      ICS.New_String (".......@...............@"));
   Stop_Xpm : ICS.chars_ptr_array :=
     (ICS.New_String ("24 15 15 1"),
      ICS.New_String (".      c #020204"),
      ICS.New_String ("+      c #077DE2"),
      ICS.New_String ("@      c #0A0A0C"),
      ICS.New_String ("#      c #525554"),
      ICS.New_String ("$      c #282A2A"),
      ICS.New_String ("%      c #959697"),
      ICS.New_String ("&      c #2A4254"),
      ICS.New_String ("*      c #127ECC"),
      ICS.New_String ("=      c #373B3F"),
      ICS.New_String ("-      c #868688"),
      ICS.New_String (";      c #646566"),
      ICS.New_String (">      c #2D3439"),
      ICS.New_String (",      c #45494A"),
      ICS.New_String ("'      c #0583EF"),
      ICS.New_String (")      c #5A5C5C"),
      ICS.New_String ("........................"),
      ICS.New_String (".################)#)))>."),
      ICS.New_String (".)#)#);;);;);;);))))#)#."),
      ICS.New_String (".#=>=====>==>>>>>>$$$$$."),
      ICS.New_String (".;#,,,,,,,====>====>$$@."),
      ICS.New_String (".-#####,,=&=>====>=>>$$."),
      ICS.New_String (".-,###,,,*''''*>>>>$$$$."),
      ICS.New_String (".-,###,,&'''''+>$>$>$$$."),
      ICS.New_String (".%####,=='''+++>$$$$$$$."),
      ICS.New_String (".%#,#,,,&''+''+$$$$$$$$."),
      ICS.New_String (".%##,,,=&*+++++$$$$$$@$."),
      ICS.New_String (".%),,,==>&>$$$$$$$$$$$$."),
      ICS.New_String (".%;#,,=>>$$$$$$$$$$$$>$."),
      ICS.New_String (".@.@@@.@@@@............."),
      ICS.New_String (".@......................"));


   type Recorder_Record is new Gtk_Toolbar_Record with
      record
         Record_Button : Gtk_Toggle_Button;
         List          : Gtk_List;
      end record;
   type Recorder is access all Recorder_Record'Class;
   procedure Initialize (Rec : access Recorder_Record'Class;
                         Win : Gdk_Window);

   Global_Rec : Recorder;

   function Get_Widget_From_Name (Name : String;
                                  List : Widget_List.Glist)
                                 return Gtk_Widget;
   --  Finds the widget whose name is NAME in the application

   function Child_From_Coordinates (Widget    : access Gtk_Widget_Record'Class;
                                    Max_Depth : Natural;
                                    X, Y      : Gint)
                                   return Gtk_Widget;
   --  Returns the child (or grand-* child) which is at the coordinates (X, Y)
   --  in WIDGET, or WIDGET itself if there is none.
   --  The search is no deeper than MAX_DEPTH

   procedure Find_Named_Parent (Widget : access Gtk_Widget_Record'Class;
                                Parent : out Gtk_Widget;
                                X, Y   : in out Gint;
                                Depth  : out Natural);
   --  Returns the first widget in WIDGET's hierarchy that has a name. It might
   --  be WIDGET itself.
   --  (X, Y) are updated so that they end up being relative to PARENT instead
   --  of WIDGET.
   --  DEPTH is the number of level we had to go up to find this PARENT.
   --  PARENT is null if there was no named parent.

   type Macro is
      record
         Current_Read : Macro_Item_Access := null;
         Item_List    : Macro_Item_Access := null;
         Last_Item    : Macro_Item_Access := null;
         Macro_Size   : Natural := 0;
         Current_Item : Natural := 1;
      end record;

   Record_Macro : Boolean := True;
   --  True if we should record the events in the current macro

   Current_Macro : Macro;

   procedure Free is new Unchecked_Deallocation
     (Macro_Item'Class, Macro_Item_Access);

   package Void_Cb is new Gtk.Handlers.Callback (Gtk_Button_Record);
   package Boolean_Cb is new Gtk.Handlers.Return_Callback
     (Widget_Type => Gtk_Widget_Record, Return_Type => Boolean);
   package Event_Cb is new Gtk.Handlers.Return_Callback
     (Widget_Type => Gtk_Widget_Record, Return_Type => Gint);
   package List_Cb is new Gtk.Handlers.Callback
     (Widget_Type => Gtk_List_Record);
   package Rec_Cb is new Gtk.Handlers.Callback
     (Widget_Type => Recorder_Record);
   package File_Cb is new Gtk.Handlers.Callback
     (Widget_Type => Gtk_File_Selection_Record);
   package Widget_Cb is new Gtk.Handlers.Callback
     (Widget_Type => Gtk_Widget_Record);

   function Get_Real_Name (Widget : System.Address) return System.Address;
   pragma Import (C, Get_Real_Name, "ada_gtk_get_real_name");
   --  Returns the real widget name (as opposed to gtk_widget_get_name,
   --  this one returns NULL instead of the class name if no name was
   --  set.

   procedure Initialize_Macro;
   --  Creates the small graphical interface for macros

   procedure Add_Item (M : in out Macro; Item : Macro_Item_Access);
   --  Add a new item to the macro list

   procedure Save_Button_Event (Widget : access Gtk_Widget_Record'Class;
                                  Event  : Gdk_Event_Button);
   procedure Save_Key_Event (Widget : access Gtk_Widget_Record'Class;
                               Event  : Gdk_Event_Key);
   procedure Save_Motion_Event (Widget : access Gtk_Widget_Record'Class;
                                  Event  : Gdk_Event_Motion);
   procedure Save_Crossing_Event (Widget : access Gtk_Widget_Record'Class;
                                    Event  : Gdk_Event_Crossing);
   procedure Save_Configure_Event (Widget : access Gtk_Widget_Record'Class;
                                     Event  : Gdk_Event_Configure);

   function Record_Event
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event)
     return Boolean;
   --  Records a new event in the current macro.
   --  This function is not thread safe. It also requires a macro to be
   --  started.

   procedure Record_Macro_Cb (Button : access Gtk_Button_Record'Class);
   --  Starts recording a new macro, with the given name.
   --  This becomes the current macro, and any previously open macro is
   --  simply deleted.

   procedure Play_Macro_Cb (Button : access Gtk_Button_Record'Class);
   --  Plays the whole macro, starting from the beginning

   procedure Real_Save_Cb (File : access Gtk_File_Selection_Record'Class);
   procedure Save_Cb (Button : access Gtk_Button_Record'Class);
   --  Saves the current macro in the file whose name was given when the
   --  macro started.

   procedure Load_Cb (List : access Gtk_List_Record'Class);
   --  Loads the macro selected in the list.
   --  This macro becomes the current one, and any previously open macro is
   --  deleted.

   procedure Fill_List (List : access Gtk_List_Record'Class);
   --  Inserts in the list the name of all the macros found in the current
   --  directory.

   procedure Next_Step (Num_Steps : Natural := Natural'Last;
                        Verbose   : Boolean := False);
   --  Plays the next NUM_STEPS in the macro.
   --  You can restart the macro with Reset
   --  If NUM_STEPS is Natural'Last, the macro is played till the end

   procedure Step_Macro_Cb (Button : access Gtk_Button_Record'Class);
   --  Plays one more step in the macro

   procedure Stop_Macro_Cb (Rec : access Recorder_Record'Class);
   --  Stop recording the macro (if we were), and reinitialize it at the
   --  beginning so that Step_Macro_Cb starts from there.

   procedure Create_Object (Obj : access Root_Type'Class);
   --  Called every time an object is created

   function Event_Name_From_Type (Event_Type : Gdk.Types.Gdk_Event_Type)
                             return String;
   --  Returns the name of the signal corresponding to an event.

   procedure Save_To_Disk (The_Macro : Macro; Name : String);
   function Load_From_Disk (Name : String) return Macro;
   --  Saves or loads the macro to the file NAME.gtkada_macro

   function Load_Line (File : File_Type; Name : String) return String;
   --  Reads the next line in the file, check that the item name is NAME,
   --  and return the vlaue (i.e after ":=" ). Raises Invalid_Line if
   --  the item is incorrect.

   Invalid_Line : exception;

   ---------------------
   -- Record_Macro_Cb --
   ---------------------

   procedure Record_Macro_Cb (Button : access Gtk_Button_Record'Class) is
      pragma Warnings (Off, Button);
   begin
      if Is_Active (Gtk_Toggle_Button (Button)) then
         --  Starts recording the macro
         Current_Macro.Item_List    := null;
         Current_Macro.Last_Item    := null;
         Current_Macro.Current_Read := null;
         Current_Macro.Current_Item := 1;
         Record_Macro := True;
      else
         --  Stop recording the macro and reinitialize the player
         Current_Macro.Current_Read := Current_Macro.Item_List;
         Record_Macro   := False;
         Current_Macro.Current_Item := 1;
      end if;
   end Record_Macro_Cb;

   ------------------
   -- Real_Save_Cb --
   ------------------

   procedure Real_Save_Cb (File : access Gtk_File_Selection_Record'Class) is
   begin
      Save_To_Disk (Current_Macro, Get_Filename (File));
      Destroy (File);
      Fill_List (Global_Rec.List);
   end Real_Save_Cb;

   -------------
   -- Save_Cb --
   -------------

   procedure Save_Cb (Button : access Gtk_Button_Record'Class) is
      File : Gtk_File_Selection;
   begin
      Gtk_New (File, "Select file name");
      Hide_Fileop_Buttons (File);
      --  Unmap (Get_Help_Button (File));
      Show (File);
      File_Cb.Object_Connect (Get_Ok_Button (File),
                              "clicked",
                              File_Cb.To_Marshaller (Real_Save_Cb'Access),
                              Slot_Object => File);
      Widget_Cb.Object_Connect
        (Get_Cancel_Button (File),
         "clicked",
         Widget_Cb.To_Marshaller (Gtk.Widget.Destroy_Cb'Access),
         Slot_Object => File);
   end Save_Cb;

   -------------
   -- Load_Cb --
   -------------

   procedure Load_Cb (List : access Gtk_List_Record'Class) is
      use type Widget_List.Glist;
      Select_List : Widget_List.Glist := Get_Selection (List);
      Selection   : Gtk_List_Item;
      Label       : Gtk_Label;
   begin
      if Select_List /= Widget_List.Null_List then
         Selection := Gtk_List_Item (Widget_List.Get_Data (Select_List));
         Label     := Gtk_Label (Widget_List.Get_Data (Children (Selection)));
         declare
            Name : constant String := Get (Label);
         begin
            Current_Macro := Load_From_Disk (Name);
            Put_Line ("Macro """ & Name & """ loaded.");
         end;
      end if;
   end Load_Cb;

   -------------------
   -- Stop_Macro_Cb --
   -------------------

   procedure Stop_Macro_Cb (Rec : access Recorder_Record'Class) is
   begin
      Set_Active (Rec.Record_Button, False);
      Put_Line ("Current Step in the macro is: "
                & Natural'Image (Current_Macro.Current_Item)
                & " / "
                & Natural'Image (Current_Macro.Macro_Size));
   end Stop_Macro_Cb;

   -------------------
   -- Play_Macro_Cb --
   -------------------

   procedure Play_Macro_Cb (Button : access Gtk_Button_Record'Class) is
      pragma Warnings (Off, Button);
   begin
      Current_Macro.Current_Read := Current_Macro.Item_List;
      Current_Macro.Current_Item := 1;
      Next_Step (Natural'Last);
   end Play_Macro_Cb;

   -------------------
   -- Step_Macro_Cb --
   -------------------

   procedure Step_Macro_Cb (Button : access Gtk_Button_Record'Class) is
      pragma Warnings (Off, Button);
   begin
      Put_Line ("Executing step "
                & Natural'Image (Current_Macro.Current_Item)
                & " / "
                & Natural'Image (Current_Macro.Macro_Size));
      Next_Step (1, True);
   end Step_Macro_Cb;

   ---------------
   -- Next_Step --
   ---------------

   procedure Next_Step (Num_Steps : Natural := Natural'Last;
                        Verbose   : Boolean := False) is
      N : Natural := Num_Steps;
      E : Gdk_Event;
      W : Gtk_Widget;
      B : Boolean;
   begin
      while N /= 0
        and then Current_Macro.Current_Read /= null
      loop
         --  Get the widget to which we want to send the event: first use
         --  the name, and then if required the relevant child.

         W := Get_Widget_From_Name (Current_Macro.Current_Read.Widget_Name,
                                    Get_Toplevels);
         if Current_Macro.Current_Read.Widget_Depth /= 0
           and then W /= null
         then
            W := Child_From_Coordinates
              (W,
               Current_Macro.Current_Read.Widget_Depth,
               Current_Macro.Current_Read.X,
               Current_Macro.Current_Read.Y);
         end if;

         --  Send an event to the widget

         if W = null then
            Put_Line (Current_Macro.Current_Read.Widget_Name & " not found");
         else
            E := Create_Event (Current_Macro.Current_Read.all, W);
            if Verbose then
               Put_Line ("Event_Type: " &
                         Gdk_Event_Type'Image (Get_Event_Type (E))
                         & " (on "
                         & Current_Macro.Current_Read.Widget_Name
                         & ")");
            end if;
            B := Boolean_Cb.Emit_By_Name
              (W, Event_Name_From_Type (Get_Event_Type (E)), E);
            Free (E);
         end if;
         Current_Macro.Current_Read := Current_Macro.Current_Read.Next;
         Current_Macro.Current_Item := Current_Macro.Current_Item + 1;
         N := N - 1;
      end loop;
   end Next_Step;

   --------------------------
   -- Event_Name_From_Type --
   --------------------------

   function Event_Name_From_Type (Event_Type : Gdk.Types.Gdk_Event_Type)
                                 return String
   is
   begin
      case Event_Type is
         when Enter_Notify   => return "enter_notify_event";
         when Leave_Notify   => return "leave_notify_event";
         when Button_Press   => return "button_press_event";
         when Button_Release => return "button_release_event";
         when others         =>
            Put_Line ("Event_Name_From_Type: unknown type: " &
                      Gdk_Event_Type'Image (Event_Type));
            return "event";
      end case;
   end Event_Name_From_Type;

   -------------------
   -- Create_Object --
   -------------------

   procedure Create_Object (Obj : access Root_Type'Class) is
   begin
      if Obj.all in Gtk_Widget_Record'Class then
         Boolean_Cb.Connect
           (Gtk_Widget (Obj), "event",
            Boolean_Cb.To_Marshaller (Record_Event'Access));
      end if;
   end Create_Object;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Rec  : access Recorder_Record'Class;
                         Win  : Gdk_Window)
   is
      Pix    : Gdk_Pixmap;
      Mask   : Gdk_Bitmap;
      Pixmap : Gtk_Pixmap;
   begin

      Gtk.Toolbar.Initialize (Rec, Orientation_Horizontal, Toolbar_Both);
      Gtk_New (Rec.List);

      --  Record button
      Create_From_Xpm_D (Pix, Win, Mask, Null_Color, Record_Xpm);
      Gtk_New (Pixmap, Pix, Mask);
      Gtk_New (Rec.Record_Button);
      Add (Rec.Record_Button, Pixmap);
      Append_Widget (Rec, Rec.Record_Button, "Record a new macro", "");
      Void_Cb.Connect (Rec.Record_Button,
                       "toggled",
                       Void_Cb.To_Marshaller (Record_Macro_Cb'Access));

      --  Play button
      Create_From_Xpm_D (Pix, Win, Mask, Null_Color, Play_Xpm);
      Gtk_New (Pixmap, Pix, Mask);
      Void_Cb.Connect (Append_Item
                       (Rec,
                        Text         => "Play",
                        Tooltip_Text => "Play current macro entirely",
                        Tooltip_Private_Text => "",
                        Icon         => Pixmap),
                       "clicked",
                       Void_Cb.To_Marshaller (Play_Macro_Cb'Access));

      --  Step button
      Create_From_Xpm_D (Pix, Win, Mask, Null_Color, Step_Xpm);
      Gtk_New (Pixmap, Pix, Mask);
      Void_Cb.Connect
        (Append_Item (Rec,
                      Text         => "Step",
                      Tooltip_Text => "Play next event in the macro",
                      Tooltip_Private_Text => "",
                      Icon         => Pixmap),
         "clicked",
         Void_Cb.To_Marshaller (Step_Macro_Cb'Access));

      --  Stop button
      Create_From_Xpm_D (Pix, Win, Mask, Null_Color, Stop_Xpm);
      Gtk_New (Pixmap, Pix, Mask);
      Rec_Cb.Object_Connect
        (Append_Item
         (Rec,
          Text         => "Stop",
          Tooltip_Text => "Stop recording macro, or reset reading",
          Tooltip_Private_Text => "",
          Icon         => Pixmap),
         "clicked",
         Rec_Cb.To_Marshaller (Stop_Macro_Cb'Access),
         Slot_Object => Rec);

      --  Space
      Append_Space (Rec);

      --  Load button
      Create_From_Xpm_D (Pix, Win, Mask, Null_Color, Load_Xpm);
      Gtk_New (Pixmap, Pix, Mask);
      List_Cb.Object_Connect
        (Append_Item (Rec,
                      Text         => "Load",
                      Tooltip_Text => "Load selected macro",
                      Tooltip_Private_Text => "",
                      Icon         => Pixmap),
         "clicked",
         List_Cb.To_Marshaller (Load_Cb'Access),
         Slot_Object => Rec.List);

      --  Save button
      Create_From_Xpm_D (Pix, Win, Mask, Null_Color, Save_Xpm);
      Gtk_New (Pixmap, Pix, Mask);
      Void_Cb.Connect
        (Append_Item (Rec,
                      Text         => "Save",
                      Tooltip_Text => "Save current macro",
                      Tooltip_Private_Text => "",
                      Icon         => Pixmap),
         "clicked",
         Void_Cb.To_Marshaller (Save_Cb'Access));

      Set_Space_Size (Rec, 10);
      Set_Button_Relief (Rec, Relief_None);
   end Initialize;

   ---------------
   -- Fill_List --
   ---------------

   procedure Fill_List (List : access Gtk_List_Record'Class) is
      Item     : Gtk_List_Item;
      Dir      : Dir_Type;
      File     : String (1 .. 256);
      Last     : Natural := 1;
   begin
      Clear_Items (List, 0, Gint'Last);

      Open (Dir, Get_Current_Dir);
      loop
         Read (Dir, File, Last);
         exit when Last = 0;

         if Last > 13
           and then File (Last - 12 .. Last) = ".gtkada_macro"
         then
            Gtk_New (Item, File (File'First .. Last - 13));
            Add (List, Item);
            Show (Item);
         end if;
      end loop;
      Close (Dir);
   end Fill_List;

   ----------------------
   -- Initialize_Macro --
   ----------------------

   procedure Initialize_Macro is
      Win      : Gtk_Window;
      Vbox     : Gtk_Box;
   begin
      Gtk_New (Win, Window_Toplevel);

      Gtk_New_Vbox (Vbox, Homogeneous => False, Spacing => 0);
      Add (Win, Vbox);

      Realize (Win);

      Global_Rec := new Recorder_Record;
      Initialize (Global_Rec, Get_Window (Win));

      Pack_Start (Vbox, Global_Rec,
                  Expand => False,
                  Fill => False,
                  Padding => 0);
      Pack_Start (Vbox, Global_Rec.List);

      Fill_List (Global_Rec.List);

      Show_All (Win);
      Set_Style (Global_Rec, Toolbar_Icons);
   end Initialize_Macro;

   ------------------
   -- Save_To_Disk --
   ------------------

   procedure Save_To_Disk (The_Macro : Macro; Name : String) is
      Item : Macro_Item_Access := The_Macro.Item_List;
      File : Ada.Text_IO.File_Type;
   begin
      if Name (Name'Length - 12 .. Name'Length) = ".gtkada_macro" then
         Create (File, Name => Name);
      else
         Create (File, Name => Name & ".gtkada_macro");
      end if;
      while Item /= null loop
         Save_To_Disk (File, Item.all);
         New_Line (File);
         Item := Item.Next;
      end loop;
      Close (File);
   end Save_To_Disk;

   ------------------
   -- Save_To_Disk --
   ------------------

   procedure Save_To_Disk (File : Ada.Text_IO.File_Type;
                           Item : Macro_Item)
   is
   begin
      Put_Line (File, "Name:=" & Item.Widget_Name);
      Put_Line (File, "Type:="
                & Integer'Image (Gdk_Event_Type'Pos (Item.Event_Type)));
      Put_Line (File, "Depth:=" & Natural'Image (Item.Widget_Depth));
      Put_Line (File, "X:=" & Gint'Image (Item.X));
      Put_Line (File, "Y:=" & Gint'Image (Item.Y));
   end Save_To_Disk;

   ------------------
   -- Save_To_Disk --
   ------------------

   procedure Save_To_Disk (File : Ada.Text_IO.File_Type;
                           Item : Macro_Item_Mouse_Press)
   is
   begin
      Save_To_Disk (File, Macro_Item (Item));
      Put_Line (File, "Button:=" & Guint'Image (Item.Button));
      Put_Line (File, "State:=" & Gdk_Modifier_Type'Image (Item.State));
   end Save_To_Disk;

   ------------------
   -- Save_To_Disk --
   ------------------

   procedure Save_To_Disk (File : Ada.Text_IO.File_Type;
                           Item : Macro_Item_Crossing)
   is
   begin
      Save_To_Disk (File, Macro_Item (Item));
      Put_Line (File, "Mode:=" & Gdk_Crossing_Mode'Image (Item.Mode));
   end Save_To_Disk;

   ---------------
   -- Load_Line --
   ---------------

   function Load_Line (File : File_Type; Name : String) return String is
      Line  : String (1 .. 256);
      Last  : Natural;
      First : Natural;
   begin
      Get_Line (File, Line, Last);
      if Line (1 .. Name'Length) /= Name then
         Put_Line ("Invalid line read: expecting (" & Name
                   & ")   found  ("
                   & Line (1 .. Name'Length) & ")");
         raise Invalid_Line;
      end if;

      First := Name'Length + 3;
      while First <= Last and then Line (First) = ' ' loop
         First := First + 1;
      end loop;

      return Line (First .. Last);
   end Load_Line;

   --------------------
   -- Load_From_Disk --
   --------------------

   function Load_From_Disk (Name : String) return Macro
   is
      M    : Macro := (Current_Read => null,
                       Item_List    => null,
                       Last_Item    => null,
                       Macro_Size   => 0,
                       Current_Item => 1);
      Item : Macro_Item_Access;
      File : Ada.Text_IO.File_Type;
   begin
      Open (File, Mode => In_File, Name => Name & ".gtkada_macro");
      while not End_Of_File (File) loop
         declare
            Name : String  := Load_Line (File, "Name");
            Typ  : Integer := Integer'Value (Load_Line (File, "Type"));
         begin
            case Typ is
               when Gdk_Event_Type'Pos (Enter_Notify)
                 |  Gdk_Event_Type'Pos (Leave_Notify)   =>

                  Item := Macro_Item_Access'
                    (new Macro_Item_Crossing (Name'Length));

               when Gdk_Event_Type'Pos (Button_Press)
                 |  Gdk_Event_Type'Pos (Button_Release) =>

                  Item := Macro_Item_Access'
                    (new Macro_Item_Mouse_Press (Name'Length));

               when others =>
                  Put_Line ("Unknown event type: "
                            & Gdk_Event_Type'Image (Gdk_Event_Type'Val (Typ)));
            end case;

            Item.Widget_Name := Name;
            Item.Event_Type  := Gdk_Event_Type'Val (Typ);
            Load_From_Disk (File, Item.all);
            Add_Item (M, Item);
            M.Macro_Size := M.Macro_Size + 1;
            Skip_Line (File);
         end;
      end loop;
      Close (File);
      return M;

   exception
      when Ada.Text_IO.Name_Error =>
         Put_Line ("File not found : " & Name & ".gtkada_macro");
         return (null, null, null, 0, 1);
   end Load_From_Disk;

   --------------------
   -- Load_From_Disk --
   --------------------

   procedure Load_From_Disk (File : Ada.Text_IO.File_Type;
                             Item : out Macro_Item)  is
   begin
      Item.Widget_Depth := Natural'Value (Load_Line (File, "Depth"));
      Item.X := Gint'Value (Load_Line (File, "X"));
      Item.Y := Gint'Value (Load_Line (File, "Y"));
   end Load_From_Disk;

   --------------------
   -- Load_From_Disk --
   --------------------

   procedure Load_From_Disk (File : Ada.Text_IO.File_Type;
                             Item : out Macro_Item_Mouse_Press) is
   begin
      Load_From_Disk (File, Macro_Item (Item));
      Item.Button := Guint'Value (Load_Line (File, "Button"));
      Item.State  := Gdk_Modifier_Type'Value (Load_Line (File, "State"));
   end Load_From_Disk;

   --------------------
   -- Load_From_Disk --
   --------------------

   procedure Load_From_Disk (File : Ada.Text_IO.File_Type;
                             Item : out Macro_Item_Crossing) is
   begin
      Load_From_Disk (File, Macro_Item (Item));
      Item.Mode := Gdk_Crossing_Mode'Value (Load_Line (File, "Mode"));
   end Load_From_Disk;

   --------------------------
   -- Get_Widget_From_Name --
   --------------------------

   function Get_Widget_From_Name (Name : String;
                                  List : Widget_List.Glist)
                                 return Gtk_Widget
   is
      Current : Gtk.Widget.Widget_List.Glist := Widget_List.First (List);
      W       : Gtk_Widget;
      L       : Widget_List.Glist;
      use type Gtk.Widget.Widget_List.Glist;
   begin
      loop
         W := Widget_List.Get_Data (Current);

         if Get_Name (W) = Name then
            return W;
         elsif W.all in Gtk_Container_Record'Class then
            L := Children (Gtk_Container (W));
            if L /= Widget_List.Null_List then
               W := Get_Widget_From_Name (Name, L);
               if W /= null then
                  return W;
               end if;
            end if;
         end if;

         exit when Current = Widget_List.Last (List);

         Current := Widget_List.Next (Current);
      end loop;
      return null;
   end Get_Widget_From_Name;

   ----------------------------
   -- Child_From_Coordinates --
   ----------------------------

   function Child_From_Coordinates (Widget    : access Gtk_Widget_Record'Class;
                                    Max_Depth : Natural;
                                    X, Y      : Gint)
                                   return Gtk_Widget
   is
      use type Widget_List.Glist;
      List    : Widget_List.Glist;
      Current : Widget_List.Glist;
   begin
      if Max_Depth = 0 then
         return Gtk_Widget (Widget);
      end if;

      if Widget.all in Gtk_Container_Record'Class then
         List := Children (Gtk_Container (Widget));
         Current := Widget_List.First (List);
      else
         return Gtk_Widget (Widget);
      end if;

      loop
         declare
            W     : Gtk_Widget := Widget_List.Get_Data (Current);
            Rec_X : Gint := Get_Allocation_X (W);
            Rec_Y : Gint := Get_Allocation_Y (W);
            Rec_W : Gint := Gint (Get_Allocation_Width (W));
            Rec_H : Gint := Gint (Get_Allocation_Height (W));
         begin
            if X >= Rec_X
              and then X <= Rec_X + Rec_W
              and then Y >= Rec_Y
              and then Y <= Rec_Y + Rec_H
            then
               return Child_From_Coordinates (W,
                                              Max_Depth - 1,
                                              X - Rec_X,
                                              Y - Rec_Y);
            end if;
         end;
         exit when Current = Widget_List.Last (List);
         Current := Widget_List.Next (Current);
      end loop;
      return Gtk_Widget (Widget);
   end Child_From_Coordinates;

   -----------------------
   -- Find_Named_Parent --
   -----------------------

   procedure Find_Named_Parent (Widget : access Gtk_Widget_Record'Class;
                                Parent : out Gtk_Widget;
                                X, Y   : in out Gint;
                                Depth  : out Natural)
   is
   begin
      Parent := Gtk_Widget (Widget);
      Depth  := 0;
      while Parent /= null
        and then Get_Real_Name (Get_Object (Parent)) = System.Null_Address
      loop
         X      := X + Get_Allocation_X (Parent);
         Y      := Y + Get_Allocation_Y (Parent);
         Parent := Get_Parent (Parent);
         Depth  := Depth + 1;
      end loop;
   end Find_Named_Parent;

   ------------------
   -- Create_Event --
   ------------------

   function Create_Event (Item : Macro_Item;
                          Widget : access Gtk_Widget_Record'Class)
                         return Gdk.Event.Gdk_Event
   is
      E : Gdk.Event.Gdk_Event;
   begin
      return E;
   end Create_Event;

   ------------------
   -- Create_Event --
   ------------------

   function Create_Event (Item : Macro_Item_Mouse_Press;
                          Widget : access Gtk_Widget_Record'Class)
                         return Gdk_Event is
      E : Gdk_Event;
   begin
      Allocate (Event      => E,
                Event_Type => Item.Event_Type,
                Window     => Get_Window (Widget));
      Set_X (E, Gdouble (Item.X));
      Set_Y (E, Gdouble (Item.Y));
      Set_Button (E, Item.Button);
      Set_State (E, Item.State);
      return E;
   end Create_Event;

   ------------------
   -- Create_Event --
   ------------------

   function Create_Event (Item : Macro_Item_Crossing;
                          Widget : access Gtk_Widget_Record'Class)
                         return Gdk_Event is
      E   : Gdk_Event;
   begin
      Allocate (Event      => E,
                Event_Type => Item.Event_Type,
                Window     => Get_Window (Widget));
      Set_Mode (E, Item.Mode);
      Set_Subwindow (E, Get_Window (Widget));
      Gdk.Window.Ref (Get_Window (Widget));
      return E;
   end Create_Event;

   --------------
   -- Add_Item --
   --------------

   procedure Add_Item (M : in out Macro; Item : Macro_Item_Access) is
   begin
      Item.Next        := null;

      if M.Last_Item = null then
         M.Item_List    := Item;
      else
         M.Last_Item.Next := Item;
      end if;

      M.Last_Item    := Item;
      M.Current_Read := M.Item_List;
      M.Current_Item := 1;
   end Add_Item;

   -----------------------
   -- Save_Button_Event --
   -----------------------

   procedure Save_Button_Event (Widget : access Gtk_Widget_Record'Class;
                                Event  : Gdk_Event_Button)
   is
      W : Gtk_Widget;
      X : Gint := Gint (Get_X (Event));
      Y : Gint := Gint (Get_Y (Event));
      In_W : Natural;
   begin
      Find_Named_Parent (Widget => Widget,
                         Parent => W,
                         X      => X,
                         Y      => Y,
                         Depth  => In_W);

      if W /= null then
         declare
            Item : Macro_Item_Mouse_Press_Access;
            Name : constant String := Get_Name (W);
         begin
            Item := new Macro_Item_Mouse_Press
              (Widget_Name_Length => Name'Length);
            Item.Widget_Name   := Name;
            Item.Event_Type    := Get_Event_Type (Event);
            Item.Widget_Depth  := In_W;
            Item.X             := X;
            Item.Y             := Y;
            Item.Button        := Get_Button (Event);
            Item.State         := Get_State (Event);
            Add_Item (Current_Macro, Macro_Item_Access (Item));
         end;
      end if;
   end Save_Button_Event;

   --------------------
   -- Save_Key_Event --
   --------------------

   procedure Save_Key_Event (Widget : access Gtk_Widget_Record'Class;
                               Event  : Gdk_Event_Key)
   is
   begin
      null;
   end Save_Key_Event;

   -----------------------
   -- Save_Motion_Event --
   -----------------------

   procedure Save_Motion_Event (Widget : access Gtk_Widget_Record'Class;
                                  Event  : Gdk_Event_Motion)
   is
   begin
      null;
   end Save_Motion_Event;

   -------------------------
   -- Save_Crossing_Event --
   -------------------------

   procedure Save_Crossing_Event (Widget : access Gtk_Widget_Record'Class;
                                  Event  : Gdk_Event_Crossing)
   is
      W    : Gtk_Widget;
      X    : Gint := Gint (Get_X (Event));
      Y    : Gint := Gint (Get_Y (Event));
      In_W : Natural;
   begin
      Find_Named_Parent (Widget => Widget,
                         Parent => W,
                         X      => X,
                         Y      => Y,
                         Depth  => In_W);

      if W /= null then
         declare
            Item : Macro_Item_Crossing_Access;
            Name : constant String := Get_Name (W);
         begin
            Item := new Macro_Item_Crossing
              (Widget_Name_Length => Name'Length);
            Item.Widget_Name   := Name;
            Item.Widget_Depth  := In_W;
            Item.Event_Type    := Get_Event_Type (Event);
            Item.Mode          := Get_Mode (Event);
            Item.X             := X;
            Item.Y             := Y;
            Add_Item (Current_Macro, Macro_Item_Access (Item));
         end;
      end if;
   end Save_Crossing_Event;

   --------------------------
   -- Save_Configure_Event --
   --------------------------

   procedure Save_Configure_Event (Widget : access Gtk_Widget_Record'Class;
                                     Event  : Gdk_Event_Configure)
   is
   begin
      null;
   end Save_Configure_Event;

   ------------------
   -- Record_Event --
   ------------------

   function Record_Event
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event)
     return Boolean
   is
   begin
      if not Record_Macro then
         --  The event should not be considered as Saved
         return False;
      end if;

      case Get_Event_Type (Event) is

         when Button_Press
           |  Gdk_2button_Press
           |  Gdk_3button_Press
           |  Button_Release     =>

            Save_Button_Event (Widget, Gdk_Event_Button (Event));

         when Key_Press
           |  Key_Release        =>

            Save_Key_Event (Widget, Gdk_Event_Key (Event));

         when Motion_Notify      =>

            Save_Motion_Event (Widget, Gdk_Event_Motion (Event));

         when Enter_Notify
           |  Leave_Notify       =>

            Save_Crossing_Event (Widget, Gdk_Event_Crossing (Event));

         when Configure          =>

            Save_Configure_Event (Widget, Gdk_Event_Configure (Event));

         when others             =>

            null;

      end case;

      --  The event should not be considered as Handled
      return False;
   end Record_Event;

   function Parse_Cmd_Line (Switch : String) return Gint;
   pragma Import (C, Parse_Cmd_Line, "ada_gtk_parse_cmd_line");
begin
   if Parse_Cmd_Line ("--gtkada_macro" & ASCII.NUL) /= 0 then
      Gtk.Main.Main_Hook            := Initialize_Macro'Access;
      Gtk.Initialize_User_Data_Hook := Create_Object'Access;
   end if;
end Gtk.Macro;
