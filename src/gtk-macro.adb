with Gdk.Pixmap;
with Gdk.Window;

with Gdk.Color;          use Gdk.Color;
with Gdk.Event;          use Gdk.Event;
with Gdk.Types;          use Gdk.Types;
with Gdk.Window;

with Gtk.Adjustment;     use Gtk.Adjustment;
with Gtk.Container;      use Gtk.Container;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Widget;         use Gtk.Widget;
with Gtk.Window;         use Gtk.Window;
with Gtk.Bin;            use Gtk.Bin;
with Gtk.Box;            use Gtk.Box;
with Gtk.Button;         use Gtk.Button;
with Gtk.Handlers;       use Gtk.Handlers;
with Gtk.Main;           use Gtk.Main;
with Gtk.Menu_Item;      use Gtk.Menu_Item;
with Gtk.Menu_Shell;     use Gtk.Menu_Shell;
with Gtk.Label;          use Gtk.Label;
with Gtk.List;           use Gtk.List;
with Gtk.List_Item;      use Gtk.List_Item;
with Gtk.Pixmap;         use Gtk.Pixmap;
with Gtk.Scale;          use Gtk.Scale;
with Gtk.Toolbar;        use Gtk.Toolbar;
with Gtk.Toggle_Button;  use Gtk.Toggle_Button;
with Gtk.Hbutton_Box;    use Gtk.Hbutton_Box;
with Gtk.Vbutton_Box;    use Gtk.Vbutton_Box;
with Gtkada.File_Selection; use Gtkada.File_Selection;

with Gtk.Type_Conversion;
pragma Warnings (Off, Gtk.Type_Conversion);

with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with Ada.Text_IO;                use Ada.Text_IO;
with System;                     use System;
with Unchecked_Deallocation;
with Unchecked_Conversion;
with Interfaces.C.Strings;

package body Gtk.Macro is

   --  ??? Known problem: handling of grabs is still incorrect
   --  when replaying macros, if a BUTTON_PRESS event is generated for a
   --  Gtk_Button, then the latter does a grab on the mouse. We have tried to
   --  counter-balance that in Step_Macro_Cb, by forcing a grab on the
   --  macro interface, but if the user presses and releases the mouse twice
   --  anywhere on the interface, then the second release is sent to the user
   --  application as if a BUTTON_RELEASE had been generated.
   --    * It would be nice if the gtk application could be started with
   --      grabs disabled.
   --    * Or we could try to prevent any event directly on the user's
   --      application.

   --  ??? Implement special events that either print a message to the
   --  customer or do an automatic screenshot and comparison

   --  ??? Recorder_Record should be the window itself

   --  ??? Should support identifiers (widget name,...) with ASCII.LF in them.

   --  ??? Have an option in gate to force a name for all widgets.

   --  ??? How to test resizing

   --  ??? Do we need a function to wait for a specific event: for instance,
   --  when pressing a button does some background work, and then displays
   --  a dialog to report, it would be nice if we were able to wait for the
   --  dialog (for instance, wait until a specific widget is available)

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

   ---------------
   -- Constants --
   ---------------

   Macro_Window_Title : constant String := "@@gtkada_macro@@";

   Delay_In_Play      : Gfloat := 0.1;
   --  Delay used when replaying a macro between each event

   Verbose            : constant Boolean := True;
   --  Whether playing the macro should also print some information on
   --  standard_output

   Event_Compression  : constant Boolean := True;
   --  Whether we should do event compression to try and keep the size of
   --  macros small

   ---------------------------
   -- Types and subprograms --
   ---------------------------

   type Event_Boolean_Array is array (Gdk_Event_Type'Range) of Boolean;

   type Macro is record
      Current_Read : Macro_Item_Access := null;
      Item_List    : Macro_Item_Access := null;
      Last_Item    : Macro_Item_Access := null;
      Macro_Size   : Natural := 0;
      Current_Item : Natural := 1;

      Last_Widget  : Gtk_Widget := null;
      --  Last widget to which an event was applied. This is used for event
      --  compression.
   end record;

   type Recorder_Record is new Gtk_Toolbar_Record with record
      Record_Button : Gtk_Toggle_Button;
      Step_Button   : Gtk_Button;
      List          : Gtk_List;
      Delay_Scale   : Gtk_Scale;

      Event_Filter  : Event_Boolean_Array := (others => True);
      Current_Macro : Macro;

      Record_Macro : Boolean := False;
      --  True if we should record the events in the current macro
   end record;
   type Recorder is access all Recorder_Record'Class;

   function To_System_Address is new Unchecked_Conversion
     (Recorder, System.Address);
   function From_System_Address is new Unchecked_Conversion
     (System.Address, Recorder);

   procedure Initialize
     (Rec : access Recorder_Record'Class;
      Win : Gdk_Window);

   procedure My_Event_Handler (Event : System.Address; Rec : System.Address);

   function Get_Widget_From_Id
     (Id : Identifier; List : Widget_List.Glist) return Gtk_Widget;
   --  Finds the widget whose Id is ID in the application.

   function Child_From_Coordinates
     (Widget    : access Gtk_Widget_Record'Class;
      Max_Depth : Natural;
      X, Y      : Gint) return Gtk_Widget;
   --  Returns the child (or grand-* child) which is at the coordinates (X, Y)
   --  in WIDGET, or WIDGET itself if there is none.
   --  The search is no deeper than MAX_DEPTH

   procedure Find_Named_Parent (Widget : access Gtk_Widget_Record'Class;
                                Parent : out Gtk_Widget;
                                Parent_Id : out Identifier;
                                X, Y   : in out Gint;
                                Depth  : out Natural);
   --  Returns the first widget in WIDGET's hierarchy that has a name. It might
   --  be WIDGET itself.
   --  (X, Y) are updated so that they end up being relative to PARENT instead
   --  of WIDGET.
   --  DEPTH is the number of levels we had to go up to find this PARENT.
   --  PARENT is null if there was no named parent, or if Widget is part of
   --  the macro GUI (and not of the application itself)

   function Get_Id (Widget : access Gtk_Widget_Record'Class) return Identifier;
   --  Return an identifier that can be used for the widget.
   --  return value.Id is left to null if no specific identifier could be
   --  found.

   procedure Free (Item : in out Macro_Item_Access);
   --  Free the memory associated with an item

   procedure Free is new Unchecked_Deallocation (String, String_Access);

   package Widget_Cb is new Gtk.Handlers.Callback (Gtk_Widget_Record);
   package Widget_Boolean_Cb is new Gtk.Handlers.Return_Callback
     (Gtk_Widget_Record, Boolean);
   package Button_Cb is new Gtk.Handlers.User_Callback
     (Gtk_Button_Record, Recorder);
   package Rec_Cb is new Gtk.Handlers.Callback (Recorder_Record);

   function Get_Real_Name (Widget : System.Address) return System.Address;
   pragma Import (C, Get_Real_Name, "ada_gtk_get_real_name");
   --  Returns the real widget name (as opposed to gtk_widget_get_name,
   --  this one returns NULL instead of the class name if no name was
   --  set.

   procedure Initialize_Macro (Data : System.Address);
   --  Creates the small graphical interface for macros

   procedure Add_Item (M : in out Macro; Item : Macro_Item_Access);
   --  Add a new item to the macro list

   procedure Remove_Last_Item (M : in out Macro);
   --  Remove the last item recorded in the macro

   function Last_Item (M : Macro) return Macro_Item_Access;
   --  Return the last item recorded in the macro

   procedure Save_Button_Event
     (Rec    : access Recorder_Record'Class;
      Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Button);
   procedure Save_Key_Event
     (Rec    : access Recorder_Record'Class;
      Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Key);
   procedure Save_Motion_Event
     (Rec    : access Recorder_Record'Class;
      Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Motion);
   procedure Save_Crossing_Event
     (Rec    : access Recorder_Record'Class;
      Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Crossing);
   procedure Save_Configure_Event
     (Rec    : access Recorder_Record'Class;
      Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Configure);

   procedure Record_Macro_Cb
     (Button : access Gtk_Button_Record'Class; Rec : Recorder);
   --  Starts recording a new macro, with the given name.
   --  This becomes the current macro, and any previously open macro is
   --  simply deleted.

   procedure Play_Macro_Cb (Rec : access Recorder_Record'Class);
   --  Plays the whole macro, starting from the beginning

   procedure Save_Cb (Rec : access Recorder_Record'Class);
   --  Saves the current macro in the file whose name was given when the
   --  macro started.

   procedure Load_Cb (Rec : access Recorder_Record'Class);
   --  Loads the macro selected in the list.
   --  This macro becomes the current one, and any previously open macro is
   --  deleted.

   procedure Fill_List (List : access Gtk_List_Record'Class);
   --  Inserts in the list the name of all the macros found in the current
   --  directory.

   procedure Next_Step (Rec       : access Recorder_Record'Class;
                        Num_Steps : Natural := Natural'Last);
   --  Plays the next NUM_STEPS in the macro.
   --  You can restart the macro with Reset
   --  If NUM_STEPS is Natural'Last, the macro is played till the end

   procedure Step_Macro_Cb (Rec : access Recorder_Record'Class);
   --  Plays one more step in the macro

   procedure Stop_Macro_Cb (Rec : access Recorder_Record'Class);
   --  Stop recording the macro (if we were), and reinitialize it at the
   --  beginning so that Step_Macro_Cb starts from there.

   procedure Save_To_Disk (The_Macro : Macro; Name : String);
   function Load_From_Disk (Name : String) return Macro;
   --  Saves or loads the macro to the file NAME.gtkada_macro

   function Load_Line
     (File : access File_Buffer;
      Name : String;
      Optional : Boolean := False) return String;
   --  Reads the next line in the file, check that the item name is NAME,
   --  and return the vlaue (i.e after ":=" ). Raises Invalid_Line if
   --  the item is incorrect.
   --  If Optional is True, then "" is returned if the current line doesn't
   --  match Name.

   Invalid_Line : exception;

   ----------
   -- Free --
   ----------

   procedure Free (Item : in out Macro_Item_Access) is
      procedure Internal is new Unchecked_Deallocation
        (Macro_Item'Class, Macro_Item_Access);
   begin
      Free (Item.Id.Id);
      Internal (Item);
   end Free;

   ---------------------
   -- Record_Macro_Cb --
   ---------------------

   procedure Record_Macro_Cb
     (Button : access Gtk_Button_Record'Class;
      Rec    : Recorder)
   is
      Tmp : Macro_Item_Access;
   begin
      if Is_Active (Gtk_Toggle_Button (Button)) then

         --  Free the memory for the old macro, if any
         while Rec.Current_Macro.Item_List /= null loop
            Tmp := Rec.Current_Macro.Item_List.Next;
            Free (Rec.Current_Macro.Item_List);
            Rec.Current_Macro.Item_List := Tmp;
         end loop;

         --  Starts recording the macro
         Rec.Current_Macro.Item_List    := null;
         Rec.Current_Macro.Last_Item    := null;
         Rec.Current_Macro.Current_Read := null;
         Rec.Current_Macro.Current_Item := 1;
         Rec.Record_Macro := True;
      else
         --  Stop recording the macro and reinitialize the player
         Rec.Current_Macro.Current_Read := Rec.Current_Macro.Item_List;
         Rec.Record_Macro   := False;
         Rec.Current_Macro.Current_Item := 1;
      end if;
   end Record_Macro_Cb;

   -------------
   -- Save_Cb --
   -------------

   procedure Save_Cb (Rec : access Recorder_Record'Class) is
   begin
      --  Stop recording the macro, if we were still doing that
      Set_Active (Rec.Record_Button, False);
      Rec.Record_Macro := False;

      --  Get the name of the macro
      declare
         Name : constant String := File_Selection_Dialog ("Select file name");
      begin
         if Name /= "" then
            Save_To_Disk (Rec.Current_Macro, Name);
            Fill_List (Rec.List);
         end if;
      end;
   end Save_Cb;

   -------------
   -- Load_Cb --
   -------------

   procedure Load_Cb (Rec : access Recorder_Record'Class) is
      use type Widget_List.Glist;
      Select_List : Widget_List.Glist := Get_Selection (Rec.List);
      Selection   : Gtk_List_Item;
      Label       : Gtk_Label;
   begin
      --  Stop recording the macro, if we were still doing that
      Set_Active (Rec.Record_Button, False);
      Rec.Record_Macro := False;

      if Select_List /= Widget_List.Null_List then
         Selection := Gtk_List_Item (Widget_List.Get_Data (Select_List));
         Label     := Gtk_Label (Widget_List.Get_Data (Children (Selection)));
         declare
            Name : constant String := Get (Label);
         begin
            Rec.Current_Macro := Load_From_Disk (Name);
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
      Rec.Current_Macro.Current_Read := Rec.Current_Macro.Item_List;
      Rec.Current_Macro.Current_Item := 1;
      Rec.Record_Macro := False;
      Put_Line ("Current Step in the macro is: "
                & Natural'Image (Rec.Current_Macro.Current_Item)
                & " / "
                & Natural'Image (Rec.Current_Macro.Macro_Size));
   end Stop_Macro_Cb;

   -------------------
   -- Play_Macro_Cb --
   -------------------

   procedure Play_Macro_Cb (Rec : access Recorder_Record'Class) is
   begin
      --  Stop recording the macro, if we were still doing that
      Set_Active (Rec.Record_Button, False);
      Rec.Record_Macro := False;

      --  Start playing from the beginning
      Rec.Current_Macro.Current_Read := Rec.Current_Macro.Item_List;
      Rec.Current_Macro.Current_Item := 1;
      Next_Step (Rec, Natural'Last);
   end Play_Macro_Cb;

   -------------------
   -- Step_Macro_Cb --
   -------------------

   procedure Step_Macro_Cb (Rec : access Recorder_Record'Class) is
   begin
      --  Stop recording the macro, if we were still doing that
      Set_Active (Rec.Record_Button, False);
      Rec.Record_Macro := False;

      Next_Step (Rec, 1);

      --  Note that this Grab is needed for proper handling of events: if we
      --  don't put it, then the next Button_Press or Button_Release event
      --  would be sent to the user application instead of detecting an
      --  event on the macro interface.
      Grab_Add (Rec.Step_Button);
   end Step_Macro_Cb;

   ---------------
   -- Next_Step --
   ---------------

   procedure Next_Step (Rec       : access Recorder_Record'Class;
                        Num_Steps : Natural := Natural'Last)
   is
      N : Natural := Num_Steps;
      E : Gdk_Event;
      W : Gtk_Widget;
      Dead : Boolean;

   begin
      while N /= 0
        and then Rec.Current_Macro.Current_Read /= null
      loop
         W := Get_Widget_From_Id
           (Rec.Current_Macro.Current_Read.Id, Get_Toplevels);

         --  Once we have the top-level widget, we go down as many levels as
         --  required, using the mouse coordinates to find the appropriate
         --  child.

         if Rec.Current_Macro.Current_Read.Widget_Depth /= 0
           and then W /= null
         then
            W := Child_From_Coordinates
              (W,
               Rec.Current_Macro.Current_Read.Widget_Depth,
               Rec.Current_Macro.Current_Read.X,
               Rec.Current_Macro.Current_Read.Y);
         end if;

         --  Send an event to the widget

         if W = null then
            Put_Line (Rec.Current_Macro.Current_Read.Id.Id_Type'Img
                      & "=" & Rec.Current_Macro.Current_Read.Id.Id.all
                      & " not found");

         elsif Get_Window (W) = Gdk.Window.Null_Window then
            Put_Line (Rec.Current_Macro.Current_Read.Id.Id_Type'Img
                      & "=" & Rec.Current_Macro.Current_Read.Id.Id.all
                      & " not mapped");

         else
            E := Create_Event (Rec.Current_Macro.Current_Read.all, W);
            if Verbose then
               Put (Natural'Image (Rec.Current_Macro.Current_Item)
                    & " / "
                    & Natural'Image (Rec.Current_Macro.Macro_Size)
                    & ":   Event_Type: " &
                    Gdk_Event_Type'Image (Get_Event_Type (E))
                    & " (on "
                    & Rec.Current_Macro.Current_Read.Id.Id_Type'Img
                    & "=" & Rec.Current_Macro.Current_Read.Id.Id.all
                    & " depth="
                    & Natural'Image
                    (Rec.Current_Macro.Current_Read.Widget_Depth)
                    & ")");
               begin
                  Put (" at" & Gint'Image (Gint (Get_X (E)))
                       & " x" & Gint'Image (Gint (Get_Y (E))));
               exception
                  when Invalid_Field => null;
               end;
               New_Line;
            end if;

            --  Send the event, as if it originated from the server itself.
            --  We use the Put function to simulate the standard behavior (as
            --  if the event had been emitted by the server itself), rather
            --  than Emit_By_Name. This is required for some widgets that
            --  test the last event emitted (for instance option_menu), and
            --  modify their behavior accordingly.
            --  This is also much simpler since we don't have to find which
            --  widget should receive the event. Gtk+ takes care of the
            --  propagation itself (for instance, key_press events for menu
            --  items should be sent to the menu shell rather than the item)
            Put (E);

            Free (E);
         end if;

         Rec.Current_Macro.Current_Read := Rec.Current_Macro.Current_Read.Next;
         Rec.Current_Macro.Current_Item := Rec.Current_Macro.Current_Item + 1;
         N := N - 1;

         --  If we are supposed to wait between each event, do so, but only
         --  after processing all remaining X events so that the interface
         --  is correctly updated.

         while Gtk.Main.Events_Pending loop
            Dead := Gtk.Main.Main_Iteration;
         end loop;

         if N /= 0 then
            delay Duration (Get_Value (Get_Adjustment (Rec.Delay_Scale)));
         end if;
      end loop;
   end Next_Step;

   ----------------------
   -- My_Event_Handler --
   ----------------------

   procedure My_Event_Handler
     (Event : System.Address; Rec : System.Address)
   is
      E      : Gdk.Event.Gdk_Event := Gdk.Event.From_Address (Event);
      Widget : Gtk.Widget.Gtk_Widget;
      Global_Rec : constant Recorder := From_System_Address (Rec);
      Motion_Mask : constant Gdk_Event_Mask :=
        Button_Motion_Mask or Button1_Motion_Mask or Button2_Motion_Mask or
        Button3_Motion_Mask or Pointer_Motion_Mask or Pointer_Motion_Hint_Mask;
      Parent : Gtk.Widget.Gtk_Widget;
      Mask_Ok : Boolean;

   begin
      --  If we are recording the macro, and we are interested in that
      --  specific event type...

      if Global_Rec.Record_Macro
        and then Global_Rec.Event_Filter (Get_Event_Type (E))
      then
         Widget := Gtk.Main.Get_Event_Widget (E);

         case Get_Event_Type (E) is

            when Button_Press
              |  Gdk_2button_Press
              |  Gdk_3button_Press
              |  Button_Release     =>

               Save_Button_Event (Global_Rec, Widget, Gdk_Event_Button (E));

            when Key_Press
              |  Key_Release        =>

               Save_Key_Event (Global_Rec, Widget, Gdk_Event_Key (E));

            when Motion_Notify      =>

               --  Only save it if Widget has asked for this type of event,
               --  even has default. We also tests whether the parent has
               --  ask for it, both by checking the mask or the default handler
               --  (for instance, a Gtk_Range doesn't set the range but has
               --  a default handler).

               Parent := Widget;
               Mask_Ok := False;
               while Parent /= null loop
                  if (Get_Events (Parent) and Motion_Mask) /= 0
                    or else Has_Default_Motion_Notify_Handler (Parent)
                  then
                     Mask_Ok := True;
                     exit;
                  end if;
                  Parent := Get_Parent (Parent);
               end loop;
               if Mask_Ok then
                  Save_Motion_Event (Global_Rec, Widget, Gdk_Event_Motion (E));
               end if;

            when Enter_Notify
              |  Leave_Notify       =>

               Save_Crossing_Event
                 (Global_Rec, Widget, Gdk_Event_Crossing (E));

            when Configure          =>

               Save_Configure_Event
                 (Global_Rec, Widget, Gdk_Event_Configure (E));

            when others             =>

               null;

         end case;

         if Event_Compression then
            Global_Rec.Current_Macro.Last_Widget := Widget;
         end if;
      end if;

      --  Process the event normally, with the default handler
      Gtk.Main.Do_Event (E);
   end My_Event_Handler;

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
      Gdk.Pixmap.Create_From_Xpm_D (Pix, Win, Mask, Null_Color, Record_Xpm);
      Gtk_New (Pixmap, Pix, Mask);
      Gtk_New (Rec.Record_Button);
      Add (Rec.Record_Button, Pixmap);
      Append_Widget (Rec, Rec.Record_Button, "Record a new macro", "");
      Button_Cb.Connect
        (Rec.Record_Button,
         "toggled",
         Button_Cb.To_Marshaller (Record_Macro_Cb'Access),
         Recorder (Rec));

      --  Play button
      Gdk.Pixmap.Create_From_Xpm_D (Pix, Win, Mask, Null_Color, Play_Xpm);
      Gtk_New (Pixmap, Pix, Mask);
      Rec_Cb.Object_Connect
        (Append_Item
         (Rec,
          Text         => "Play",
          Tooltip_Text => "Play current macro entirely",
          Tooltip_Private_Text => "",
          Icon         => Gtk_Widget (Pixmap)),
         "clicked",
         Rec_Cb.To_Marshaller (Play_Macro_Cb'Access),
         Rec);

      --  Step button
      Gdk.Pixmap.Create_From_Xpm_D (Pix, Win, Mask, Null_Color, Step_Xpm);
      Gtk_New (Pixmap, Pix, Mask);

      Gtk_New (Rec.Step_Button);
      Add (Rec.Step_Button, Pixmap);
      Append_Widget (Rec, Rec.Step_Button, "Play next event in the macro", "");
      Rec_Cb.Object_Connect
        (Rec.Step_Button,
         "clicked",
         Rec_Cb.To_Marshaller (Step_Macro_Cb'Access),
         Rec);

      --  Stop button
      Gdk.Pixmap.Create_From_Xpm_D (Pix, Win, Mask, Null_Color, Stop_Xpm);
      Gtk_New (Pixmap, Pix, Mask);
      Rec_Cb.Object_Connect
        (Append_Item
         (Rec,
          Text         => "Stop",
          Tooltip_Text => "Stop recording macro, or reset reading",
          Tooltip_Private_Text => "",
          Icon         => Gtk_Widget (Pixmap)),
         "clicked",
         Rec_Cb.To_Marshaller (Stop_Macro_Cb'Access),
         Slot_Object => Rec);

      --  Space
      Append_Space (Rec);

      --  Load button
      Gdk.Pixmap.Create_From_Xpm_D (Pix, Win, Mask, Null_Color, Load_Xpm);
      Gtk_New (Pixmap, Pix, Mask);
      Rec_Cb.Object_Connect
        (Append_Item (Rec,
                      Text         => "Load",
                      Tooltip_Text => "Load selected macro",
                      Tooltip_Private_Text => "",
                      Icon         => Gtk_Widget (Pixmap)),
         "clicked",
         Rec_Cb.To_Marshaller (Load_Cb'Access),
         Rec);

      --  Save button
      Gdk.Pixmap.Create_From_Xpm_D (Pix, Win, Mask, Null_Color, Save_Xpm);
      Gtk_New (Pixmap, Pix, Mask);
      Rec_Cb.Object_Connect
        (Append_Item (Rec,
                      Text         => "Save",
                      Tooltip_Text => "Save current macro",
                      Tooltip_Private_Text => "",
                      Icon         => Gtk_Widget (Pixmap)),
         "clicked",
         Rec_Cb.To_Marshaller (Save_Cb'Access),
         Rec);

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

   procedure Initialize_Macro (Data : System.Address) is
      pragma Warnings (Off, Data);
      Win      : Gtk_Window;
      Vbox     : Gtk_Box;
      Global_Rec : Recorder;
      Adj    : Gtk_Adjustment;

   begin
      Gtk_New (Win, Window_Toplevel);
      Set_Name (Win, Macro_Window_Title);

      Gtk_New_Vbox (Vbox, Homogeneous => False, Spacing => 0);
      Add (Win, Vbox);

      Realize (Win);

      Global_Rec := new Recorder_Record;
      Initialize (Global_Rec, Get_Window (Win));

      Pack_Start (Vbox, Global_Rec,
                  Expand => False,
                  Fill => False,
                  Padding => 0);

      Gtk_New (Adj, Delay_In_Play, 0.0, 2.0, 0.1, 0.5, 0.1);
      Gtk_New_Hscale (Global_Rec.Delay_Scale, Adj);
      Set_Digits (Global_Rec.Delay_Scale, 1);
      Set_Draw_Value (Global_Rec.Delay_Scale, True);
      Set_Value_Pos (Global_Rec.Delay_Scale, Pos_Left);

      Pack_Start (Vbox, Global_Rec.Delay_Scale);
      Pack_Start (Vbox, Global_Rec.List);

      Fill_List (Global_Rec.List);

      Show_All (Win);
      Set_Style (Global_Rec, Toolbar_Icons);

      Event_Handler_Set
        (My_Event_Handler'Access,
         To_System_Address (Global_Rec));
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
                           Item : Macro_Item) is
   begin
      Put_Line (File, "Id:=" & Item.Id.Id_Type'Img);
      Put_Line (File, "Name:=" & Item.Id.Id.all);
      Put_Line (File, "Type:="
                & Integer'Image (Gdk_Event_Type'Pos (Item.Event_Type)));
      Put_Line (File, "Depth:=" & Natural'Image (Item.Widget_Depth));
   end Save_To_Disk;

   ------------------
   -- Save_To_Disk --
   ------------------

   procedure Save_To_Disk (File : Ada.Text_IO.File_Type;
                           Item : Macro_Item_Mouse_Press)
   is
   begin
      Save_To_Disk (File, Macro_Item (Item));
      Put_Line (File, "X:=" & Gint'Image (Item.X));
      Put_Line (File, "Y:=" & Gint'Image (Item.Y));
      Put_Line (File, "Button:=" & Guint'Image (Item.Button));
      Put_Line (File, "State:=" & Gdk_Modifier_Type'Image (Item.State));
      Put_Line (File, "Time:=" & Guint32'Image (Item.Time));
   end Save_To_Disk;

   ------------------
   -- Save_To_Disk --
   ------------------

   procedure Save_To_Disk (File : Ada.Text_IO.File_Type;
                           Item : Macro_Item_Crossing) is
   begin
      Save_To_Disk (File, Macro_Item (Item));
      Put_Line (File, "X:=" & Gint'Image (Item.X));
      Put_Line (File, "Y:=" & Gint'Image (Item.Y));
      Put_Line (File, "Mode:=" & Gdk_Crossing_Mode'Image (Item.Mode));
      Put_Line (File, "State:=" & Gdk_Modifier_Type'Image (Item.State));
      Put_Line (File, "Detail:=" & Gdk_Notify_Type'Image (Item.Detail));
      Put_Line (File, "Time:=" & Guint32'Image (Item.Time));
   end Save_To_Disk;

   ------------------
   -- Save_To_Disk --
   ------------------

   procedure Save_To_Disk
     (File : Ada.Text_IO.File_Type;
      Item : Macro_Item_Key) is
   begin
      Save_To_Disk (File, Macro_Item (Item));
      Put_Line (File, "State:=" & Gdk_Modifier_Type'Image (Item.State));
      Put_Line (File, "Keyval:=" & Gdk_Key_Type'Image (Item.Keyval));
      Put_Line (File, "Str:=" & Item.Str.all);
      Put_Line (File, "Time:=" & Guint32'Image (Item.Time));
   end Save_To_Disk;

   ------------------
   -- Save_To_Disk --
   ------------------

   procedure Save_To_Disk
     (File : Ada.Text_IO.File_Type;
      Item : Macro_Item_Motion) is
   begin
      Save_To_Disk (File, Macro_Item (Item));
      Put_Line (File, "X:=" & Gint'Image (Item.X));
      Put_Line (File, "Y:=" & Gint'Image (Item.Y));
      Put_Line (File, "State:=" & Gdk_Modifier_Type'Image (Item.State));
      Put_Line (File, "Time:=" & Guint32'Image (Item.Time));
   end Save_To_Disk;

   ---------------
   -- Load_Line --
   ---------------

   function Load_Line
     (File : access File_Buffer;
      Name : String;
      Optional : Boolean := False) return String
   is
      Last  : Natural;
      First : Natural;
   begin
      Last := File.Index;
      while Last <= File.Buffer'Last
        and then File.Buffer (Last) /= ASCII.LF
      loop
         Last := Last + 1;
      end loop;

      if File.Buffer (File.Index .. File.Index + Name'Length - 1) /= Name then
         if Optional then
            return "";
         else
            Put_Line ("Invalid line read: expecting (" & Name
                      & ")   found  ("
                      & File.Buffer (File.Index .. Last - 1) & "), "
                      & " at index " & File.Index'Img);
            raise Invalid_Line;
         end if;
      end if;

      First := File.Index + Name'Length + 2;
      while First <= Last and then File.Buffer (First) = ' ' loop
         First := First + 1;
      end loop;

      File.Index := Last + 1;
      return File.Buffer (First .. Last - 1);
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
                       Current_Item => 1,
                       Last_Widget  => null);
      Item : Macro_Item_Access;

      File   : aliased File_Buffer;
      F      : File_Descriptor;
      Length : Positive;
      N      : constant String := Name & ".gtkada_macro" & ASCII.NUL;
   begin

      --  Read the size of the file, we first try locally.
      F := Open_Read (N'Address, Binary);
      if F /= Invalid_FD then
         Length := Positive (File_Length (F));

         declare
            S : String (1 .. Length);
         begin
            Length := Read (F, S'Address, Length);
            File.Buffer := new String' (S);
         end;

         Close (F);
      end if;

      File.Index := File.Buffer'First;

      while File.Index <= File.Buffer'Last loop
         declare
            Id   : constant Identifier_Type :=
              Identifier_Type'Value (Load_Line (File'Access, "Id"));
            Name : constant String  := Load_Line (File'Access, "Name");
            Typ  : Integer := Integer'Value (Load_Line (File'Access, "Type"));
         begin
            case Typ is
               when Gdk_Event_Type'Pos (Enter_Notify)
                 |  Gdk_Event_Type'Pos (Leave_Notify)   =>

                  Item := new Macro_Item_Crossing;

               when Gdk_Event_Type'Pos (Button_Press)
                 |  Gdk_Event_Type'Pos (Button_Release) =>

                  Item := new Macro_Item_Mouse_Press;

               when Gdk_Event_Type'Pos (Key_Press)
                 |  Gdk_Event_Type'Pos (Key_Release)    =>

                  Item := new Macro_Item_Key;

               when Gdk_Event_Type'Pos (Motion_Notify) =>

                  Item := new Macro_Item_Motion;

               when others =>
                  Put_Line ("Unknown event type: "
                            & Gdk_Event_Type'Image (Gdk_Event_Type'Val (Typ)));
            end case;

            Item.Id := (Id, new String'(Name));
            Item.Event_Type  := Gdk_Event_Type'Val (Typ);
            Load_From_Disk (File'Access, Item.all);
            Add_Item (M, Item);

            --  Skip the blank line
            File.Index := File.Index + 1;
         end;
      end loop;
      return M;

   exception
      when Ada.Text_IO.Name_Error =>
         Put_Line ("File not found : " & Name & ".gtkada_macro");
         return (null, null, null, 0, 1, null);
   end Load_From_Disk;

   --------------------
   -- Load_From_Disk --
   --------------------

   procedure Load_From_Disk (File : access File_Buffer;
                             Item : out Macro_Item)  is
   begin
      Item.Widget_Depth := Natural'Value (Load_Line (File, "Depth"));
   end Load_From_Disk;

   --------------------
   -- Load_From_Disk --
   --------------------

   procedure Load_From_Disk (File : access File_Buffer;
                             Item : out Macro_Item_Mouse_Press) is
   begin
      Load_From_Disk (File, Macro_Item (Item));
      Item.X := Gint'Value (Load_Line (File, "X"));
      Item.Y := Gint'Value (Load_Line (File, "Y"));
      Item.Button := Guint'Value (Load_Line (File, "Button"));
      Item.State  := Gdk_Modifier_Type'Value (Load_Line (File, "State"));
      Item.Time  := Guint32'Value (Load_Line (File, "Time"));
   end Load_From_Disk;

   --------------------
   -- Load_From_Disk --
   --------------------

   procedure Load_From_Disk
     (File : access File_Buffer;
      Item : out Macro_Item_Key)
   is
   begin
      Load_From_Disk (File, Macro_Item (Item));
      Item.X := 0;
      Item.Y := 0;
      Item.State := Gdk_Modifier_Type'Value (Load_Line (File, "State"));
      Item.Keyval := Gdk_Key_Type'Value (Load_Line (File, "Keyval"));
      Item.Str := new String'(Load_Line (File, "Str"));
      Item.Time  := Guint32'Value (Load_Line (File, "Time"));
   end Load_From_Disk;

   --------------------
   -- Load_From_Disk --
   --------------------

   procedure Load_From_Disk (File : access File_Buffer;
                             Item : out Macro_Item_Crossing) is
   begin
      Load_From_Disk (File, Macro_Item (Item));
      Item.X := Gint'Value (Load_Line (File, "X"));
      Item.Y := Gint'Value (Load_Line (File, "Y"));
      Item.Mode := Gdk_Crossing_Mode'Value (Load_Line (File, "Mode"));
      Item.State := Gdk_Modifier_Type'Value (Load_Line (File, "State"));
      Item.Detail := Gdk_Notify_Type'Value (Load_Line (File, "Detail"));
      Item.Time  := Guint32'Value (Load_Line (File, "Time"));
   end Load_From_Disk;

   --------------------
   -- Load_From_Disk --
   --------------------

   procedure Load_From_Disk
     (File : access File_Buffer;
      Item : out Macro_Item_Motion) is
   begin
      Load_From_Disk (File, Macro_Item (Item));
      Item.X := Gint'Value (Load_Line (File, "X"));
      Item.Y := Gint'Value (Load_Line (File, "Y"));
      Item.State := Gdk_Modifier_Type'Value (Load_Line (File, "State"));
      Item.Time  := Guint32'Value (Load_Line (File, "Time"));
   end Load_From_Disk;

   ------------
   -- Get_Id --
   ------------

   function Get_Id (Widget : access Gtk_Widget_Record'Class)
      return Identifier
   is
   begin
      if Get_Real_Name (Get_Object (Widget)) /= System.Null_Address then
         return (Name, new String'(Get_Name (Widget)));

      elsif Widget.all in Gtk_Window_Record'Class then
         if Get_Title (Gtk_Window (Widget)) /= "" then
            return (Title, new String'(Get_Title (Gtk_Window (Widget))));

         elsif Get_Transient_Parent (Gtk_Window (Widget)) /= null then
            declare
               T : constant String :=
                 Get_Title (Get_Transient_Parent (Gtk_Window (Widget)));
            begin
               if T /= "" then
                  return (Transient, new String'(T));
               end if;
            end;
         end if;

      elsif Widget.all in Gtk_Menu_Item_Record'Class
        or else Widget.all in Gtk_Button_Record'Class
      then
         declare
            C : constant Gtk_Widget := Get_Child (Gtk_Bin (Widget));
         begin
            if C /= null and then C.all in Gtk_Label_Record'Class then
               return (Label, new String'(Get (Gtk_Label (C))));
            end if;
         end;

      end if;

      return (None, null);
   end Get_Id;

   ------------------------
   -- Get_Widget_From_Id --
   ------------------------

   function Get_Widget_From_Id
     (Id : Identifier; List : Widget_List.Glist)
      return Gtk_Widget
   is
      Current : Gtk.Widget.Widget_List.Glist := Widget_List.First (List);
      W       : Gtk_Widget;
      L       : Widget_List.Glist;
      W_Id    : Identifier;
      use type Gtk.Widget.Widget_List.Glist;

   begin
      while Current /= Widget_List.Null_List loop
         W := Widget_List.Get_Data (Current);
         W_Id := Get_Id (W);

         if W_Id.Id_Type = Id.Id_Type
           and then W_Id.Id.all = Id.Id.all
         then
            return W;

         --  Else we examine the children of W (except when we know the title
         --  of a top-level window, in which case there is no need to go down)

         elsif W.all in Gtk_Container_Record'Class
           and then Id.Id_Type /= Title
         then
            L := Children (Gtk_Container (W));
            if L /= Widget_List.Null_List then
               W := Get_Widget_From_Id (Id, L);
               if W /= null then
                  return W;
               end if;
            end if;
         end if;

         Current := Widget_List.Next (Current);
      end loop;
      return null;
   end Get_Widget_From_Id;

   ----------------------------
   -- Child_From_Coordinates --
   ----------------------------

   function Child_From_Coordinates (Widget    : access Gtk_Widget_Record'Class;
                                    Max_Depth : Natural;
                                    X, Y      : Gint)
                                   return Gtk_Widget
   is
      use type Widget_List.Glist;
      Current : Widget_List.Glist := Widget_List.Null_List;
      New_X, New_Y : Gint;
   begin
      if Max_Depth = 0 then
         return Gtk_Widget (Widget);
      end if;

      if Widget.all in Gtk_Container_Record'Class then
         Current := Widget_List.First (Children (Gtk_Container (Widget)));
      end if;

      while Current /= Widget_List.Null_List loop
         declare
            W     : constant Gtk_Widget := Widget_List.Get_Data (Current);
            Rec_X : constant Gint := Get_Allocation_X (W);
            Rec_Y : constant Gint := Get_Allocation_Y (W);
            Rec_W : constant Gint := Gint (Get_Allocation_Width (W));
            Rec_H : constant Gint := Gint (Get_Allocation_Height (W));
         begin
            pragma Debug
              (Put_Line ("  "
                         & (1 .. Max_Depth * 3 => ' ')
                         & "Child_From_Coordinates, Depth=" & Max_Depth'Img
                         & " X=" & X'Img & " Y=" & Y'Img
                         & " Rec_X=" & Rec_X'Img & " Rec_Y=" & Rec_Y'Img
                         & " Rec_W=" & Rec_W'Img & " Rec_H=" & Rec_H'Img));

            if X >= Rec_X
              and then X <= Rec_X + Rec_W
              and then Y >= Rec_Y
              and then Y <= Rec_Y + Rec_H
            then
               --  Note that in Gtk+ the button boxes and boxes set the
               --  coordinates of their children to the parent's, so we need a
               --  special handling here

               if W.all in Gtk_Vbutton_Box_Record'Class
                 or else Get_Type (W) = Get_Vbox_Type
               then
                  New_X := X;
               else
                  New_X := X - Rec_X;
               end if;

               if W.all in Gtk_Hbutton_Box_Record'Class
                 or else Get_Type (W) = Get_Hbox_Type
               then
                  New_Y := Y;
               else
                  New_Y := Y - Rec_Y;
               end if;

               return Child_From_Coordinates (W, Max_Depth - 1, New_X, New_Y);
            end if;
         end;
         Current := Widget_List.Next (Current);
      end loop;
      return Gtk_Widget (Widget);
   end Child_From_Coordinates;

   -----------------------
   -- Find_Named_Parent --
   -----------------------

   procedure Find_Named_Parent
     (Widget    : access Gtk_Widget_Record'Class;
      Parent    : out Gtk_Widget;
      Parent_Id : out Identifier;
      X, Y      : in out Gint;
      Depth     : out Natural)
   is
      Tmp : Gtk_Widget;
   begin

      Parent := Gtk_Widget (Widget);
      Depth  := 0;

      --  Check that the event didn't take place in fact in the macro GUI
      Tmp := Gtk_Widget (Widget);
      while Get_Parent (Tmp) /= null loop
         Tmp := Get_Parent (Tmp);
      end loop;
      if Get_Name (Tmp) = Macro_Window_Title then
         return;
      end if;

      --  Stop either at the top-level widget, or at the first widget that is
      --  associated with an id.

      while Parent /= null loop

         if Get_Window (Parent) /= Gdk.Window.Null_Window then
            Parent_Id := Get_Id (Parent);
            exit when Parent_Id.Id /= null;
         end if;

         --  Change the coordinates of the child to include that of the
         --  parent's, except in the special case of boxes and button boxes
         --  where this is already done by gtk+ itself.

         if not (Parent.all in  Gtk_Vbutton_Box_Record'Class)
           and then Get_Type (Parent) /= Get_Vbox_Type
         then
            X := X + Get_Allocation_X (Parent);
         end if;

         if not (Parent.all in  Gtk_Hbutton_Box_Record'Class)
           and then Get_Type (Parent) /= Get_Hbox_Type
         then
            Y := Y + Get_Allocation_Y (Parent);
         end if;

         Parent := Get_Parent (Parent);
         Depth  := Depth + 1;
      end loop;
   end Find_Named_Parent;

   ------------------
   -- Create_Event --
   ------------------

   function Create_Event
     (Item : Macro_Item_Mouse_Press; Widget : access Gtk_Widget_Record'Class)
      return Gdk_Event
   is
      E : Gdk_Event;
   begin
      Allocate (Event      => E,
                Event_Type => Item.Event_Type,
                Window     => Get_Window (Widget));
      Set_X (E, Gdouble (Item.X));
      Set_Y (E, Gdouble (Item.Y));
      Set_Button (E, Item.Button);
      Set_State (E, Item.State);
      Set_Time (E, Item.Time);
      return E;
   end Create_Event;

   ------------------
   -- Create_Event --
   ------------------

   function Create_Event
     (Item : Macro_Item_Crossing; Widget : access Gtk_Widget_Record'Class)
      return Gdk_Event
   is
      E   : Gdk_Event;
   begin
      Allocate (Event      => E,
                Event_Type => Item.Event_Type,
                Window     => Get_Window (Widget));
      Set_Mode (E, Item.Mode);
      Set_Subwindow (E, Get_Window (Widget));
      Gdk.Window.Ref (Get_Window (Widget));
      Set_X (E, Gdouble (Item.X));
      Set_Y (E, Gdouble (Item.Y));
      Set_State (E, Item.State);
      Set_Focus (E, Item.Focus);
      Set_Detail (E, Item.Detail);
      Set_Time (E, Item.Time);
      return E;
   end Create_Event;

   ------------------
   -- Create_Event --
   ------------------

   function Create_Event
     (Item : Macro_Item_Key; Widget : access Gtk_Widget_Record'Class)
      return Gdk_Event
   is
      E   : Gdk_Event;
   begin
      Allocate (Event      => E,
                Event_Type => Item.Event_Type,
                Window     => Get_Window (Widget));
      Set_State (E, Item.State);
      Set_Time (E, Item.Time);
      Set_Key_Val (E, Item.Keyval);
      Set_String (E, Item.Str.all);
      return E;
   end Create_Event;

   ------------------
   -- Create_Event --
   ------------------

   function Create_Event
     (Item : Macro_Item_Motion;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gdk.Event.Gdk_Event
   is
      E   : Gdk_Event;
   begin
      Allocate (Event      => E,
                Event_Type => Item.Event_Type,
                Window     => Get_Window (Widget));
      Set_State (E, Item.State);
      Set_X (E, Gdouble (Item.X));
      Set_Y (E, Gdouble (Item.Y));
--        Set_X_Root (E, Gdouble (Item.X));
--        Set_Y_Root (E, Gdouble (Item.Y));
      Set_Is_Hint (E, False);
--        Set_Source (E, Source_Mouse);
--        Set_Device_Id (E, Core_Pointer);
      Set_Time (E, Item.Time);
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
      M.Macro_Size   := M.Macro_Size + 1;
   end Add_Item;

   ----------------------
   -- Remove_Last_Item --
   ----------------------

   procedure Remove_Last_Item (M : in out Macro) is
      Item : Macro_Item_Access := M.Item_List;
   begin
      if Item = null then
         return;
      end if;

      --  If there was a single item in the list
      if Item = M.Last_Item then
         Free (Item);
         M.Last_Item := null;
         M.Macro_Size := 0;
         M.Current_Read := null;
         return;
      end if;

      --  Else go to the end of the list and remove it
      while Item.Next /= M.Last_Item loop
         Item := Item.Next;
      end loop;

      Free (Item.Next);
      Item.Next := null;
      M.Last_Item := Item;
      M.Macro_Size := M.Macro_Size - 1;
   end Remove_Last_Item;

   ---------------
   -- Last_Item --
   ---------------

   function Last_Item (M : Macro) return Macro_Item_Access is
   begin
      return M.Last_Item;
   end Last_Item;

   -----------------------
   -- Save_Button_Event --
   -----------------------

   procedure Save_Button_Event
     (Rec    : access Recorder_Record'Class;
      Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Button)
   is
      W : Gtk_Widget;
      X : Gint := Gint (Get_X (Event));
      Y : Gint := Gint (Get_Y (Event));
      In_W : Natural;
      Id : Identifier;
      Item : Macro_Item_Mouse_Press_Access;

   begin
      Find_Named_Parent (Widget, W, Id, X, Y, In_W);

      if W /= null and then Id.Id /= null then
         Put_Line (Get_Event_Type (Event)'Img
                   & " at X=" & Gint (Get_X (Event))'Img
                   & " Y=" & Gint (Get_Y (Event))'Img
                   & " Id=" & Id.Id_Type'Img
                   & " Name=" & Id.Id.all
                   & " Depth=" & In_W'Img
                   & " final_X=" & X'Img & " final_Y=" & Y'Img);

         Item              := new Macro_Item_Mouse_Press;
         Item.Id           := Id;
         Item.Event_Type   := Get_Event_Type (Event);
         Item.Widget_Depth := In_W;
         Item.X            := X;
         Item.Y            := Y;
         Item.Button       := Get_Button (Event);
         Item.State        := Get_State (Event);
         Item.Time         := Get_Time (Event);
         Add_Item (Rec.Current_Macro, Macro_Item_Access (Item));
      end if;
   end Save_Button_Event;

   --------------------
   -- Save_Key_Event --
   --------------------

   procedure Save_Key_Event
     (Rec    : access Recorder_Record'Class;
      Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Key)
   is
      W : Gtk_Widget;
      X : Gint := 0;
      Y : Gint := 0;
      In_W : Natural;
      Id : Identifier;
      Item : Macro_Item_Key_Access;

   begin
      Find_Named_Parent (Widget, W, Id, X, Y, In_W);

      if W /= null and then Id.Id /= null then
         Put_Line (Get_Event_Type (Event)'Img
                   & " Id=" & Id.Id_Type'Img
                   & " Name=" & Id.Id.all
                   & " Depth=" & In_W'Img
                   & " final_X=" & X'Img & " final_Y=" & Y'Img);

         Item              := new Macro_Item_Key;
         Item.Id           := Id;
         Item.Event_Type   := Get_Event_Type (Event);
         Item.Widget_Depth := In_W;
         Item.State        := Get_State (Event);
         Item.Time         := Get_Time (Event);
         Item.Keyval       := Get_Key_Val (Event);
         Item.Str          := new String' (Get_String (Event));
         Add_Item (Rec.Current_Macro, Macro_Item_Access (Item));
      end if;
   end Save_Key_Event;

   -----------------------
   -- Save_Motion_Event --
   -----------------------

   procedure Save_Motion_Event
     (Rec    : access Recorder_Record'Class;
      Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Motion)
   is
      W : Gtk_Widget;
      X : Gint := Gint (Get_X (Event));
      Y : Gint := Gint (Get_Y (Event));
      In_W : Natural;
      Id : Identifier;
      Item : Macro_Item_Motion_Access;

   begin
      Find_Named_Parent (Widget, W, Id, X, Y, In_W);

      if W /= null and then Id.Id /= null then
         Put_Line (Get_Event_Type (Event)'Img
                   & " at X=" & Gint (Get_X (Event))'Img
                   & " Y=" & Gint (Get_Y (Event))'Img
                   & " Id=" & Id.Id_Type'Img
                   & " Name=" & Id.Id.all
                   & " Depth=" & In_W'Img
                   & " final_X=" & X'Img & " final_Y=" & Y'Img);

         Item              := new Macro_Item_Motion;
         Item.Id           := Id;
         Item.Event_Type   := Get_Event_Type (Event);
         Item.Widget_Depth := In_W;
         Item.X            := X;
         Item.Y            := Y;
         Item.State        := Get_State (Event);
         Item.Time         := Get_Time (Event);
         Add_Item (Rec.Current_Macro, Macro_Item_Access (Item));
      end if;
   end Save_Motion_Event;

   -------------------------
   -- Save_Crossing_Event --
   -------------------------

   procedure Save_Crossing_Event
     (Rec    : access Recorder_Record'Class;
      Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Crossing)
   is
      W    : Gtk_Widget;
      X    : Gint := Gint (Get_X (Event));
      Y    : Gint := Gint (Get_Y (Event));
      In_W : Natural;
      Id   : Identifier;
      Item : Macro_Item_Crossing_Access;

   begin

      --  If we are doing an event compression, and we saw two enter/leave
      --  events on the same widget, then no need to record either of them
      --  We can not do that for Menu_Item and Menu_Shell widgets, since
      --  the mere presence of the pointer inside them might trigger the
      --  display of their submenu.

      if Event_Compression
        and then Get_Event_Type (Event) = Leave_Notify
        and then Rec.Current_Macro.Last_Widget = Gtk_Widget (Widget)
        and then not (Widget.all in Gtk_Menu_Item_Record'Class)
        and then not (Widget.all in Gtk_Menu_Shell_Record'Class)
        and then Last_Item (Rec.Current_Macro) /= null
        and then Last_Item (Rec.Current_Macro).Event_Type = Enter_Notify
      then
         Remove_Last_Item (Rec.Current_Macro);
         if Verbose then
            Put_Line ("Event_Compression: Last event was removed");
         end if;
         return;
      end if;

      --  Else simply record the event

      Find_Named_Parent (Widget, W, Id, X, Y, In_W);

      if W /= null and then Id.Id /= null then
         Put_Line (Get_Event_Type (Event)'Img
                   & " at X=" & Gint (Get_X (Event))'Img
                   & " Y=" & Gint (Get_Y (Event))'Img
                   & " Id=" & Id.Id_Type'Img
                   & " Name=" & Id.Id.all
                   & " Depth=" & In_W'Img
                   & " final_X=" & X'Img & " final_Y=" & Y'Img);

         Item              := new Macro_Item_Crossing;
         Item.Id           := Id;
         Item.Widget_Depth := In_W;
         Item.Event_Type   := Get_Event_Type (Event);
         Item.Mode         := Get_Mode (Event);
         Item.X            := X;
         Item.Y            := Y;
         Item.Focus        := Get_Focus (Event);
         Item.Detail       := Get_Detail (Event);
         Item.State        := Get_State (Event);
         Item.Time         := Get_Time (Event);
         Add_Item (Rec.Current_Macro, Macro_Item_Access (Item));
      end if;
   end Save_Crossing_Event;

   --------------------------
   -- Save_Configure_Event --
   --------------------------

   procedure Save_Configure_Event
     (Rec    : access Recorder_Record'Class;
      Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Configure) is
   begin
      null;
   end Save_Configure_Event;

   gnat_argc : Integer;
   pragma Import (C, gnat_argc);

   gnat_argv : System.Address;
   pragma Import (C, gnat_argv);

   function Parse_Cmd_Line
     (Argc, Argv : System.Address; Switch : String) return Gint;
   pragma Import (C, Parse_Cmd_Line, "ada_gtk_parse_cmd_line");

begin
   if Parse_Cmd_Line
     (gnat_argc'Address, gnat_argv, "--gtkada_macro" & ASCII.NUL) /= 0
   then
      Gtk.Main.Init_Add (Initialize_Macro'Access, System.Null_Address);
   end if;
end Gtk.Macro;
