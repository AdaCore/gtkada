with Glib;               use Glib;
with Gtk.Box;            use Gtk.Box;
with Gtk.Button;         use Gtk.Button;
with Gtk.Dialog;         use Gtk.Dialog;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Frame;          use Gtk.Frame;
with Gtk.GEntry;         use Gtk.GEntry;
with Gtk.Grid;           use Gtk.Grid;
with Gtk.Label;          use Gtk.Label;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Widget;         use Gtk.Widget;
with Gtk.Window;         use Gtk.Window;

package body Create_Dialog is

   Count : Natural := 0;
   --  How many times the message dialog has been popped up so far.

   Entry1 : Gtk_Entry;
   Entry2 : Gtk_Entry;
   --  The two entries of the interactive dialog. They outlive the dialog
   --  itself, which is destroyed as soon as it is answered, so the response
   --  handler can still read them.

   Label1 : Gtk_Label;
   Label2 : Gtk_Label;
   --  Where the entries' contents are copied back to on Gtk_Response_OK.

   procedure On_Message_Clicked (Self : access Gtk_Button_Record'Class);
   procedure On_Interactive_Clicked (Self : access Gtk_Button_Record'Class);

   procedure On_Message_Response
     (Self        : access Gtk_Dialog_Record'Class;
      Response_Id : Gtk_Response_Type);
   procedure On_Interactive_Response
     (Self        : access Gtk_Dialog_Record'Class;
      Response_Id : Gtk_Response_Type);

   function Parent_Window
     (Widget : access Gtk_Widget_Record'Class) return Gtk_Window;
   --  The toplevel Widget sits in, to be used as the dialog's transient
   --  parent.

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "A @bGtk_Dialog@B is a transient window that asks the user"
        & " something and reports the answer through its @bresponse@B"
        & " signal. Gtk4 dropped @bRun@B, so a dialog is never waited on:"
        & " it is presented, and the handler runs when the user answers."
        & ASCII.LF
        & "The first button pops up a @bGtk_Message_Dialog@B, the canned"
        & " dialog for a short message and a standard set of buttons --"
        & " here @bMessage_Info@B and @bButtons_Ok@B."
        & ASCII.LF
        & "The second builds its @bGtk_Dialog@B by hand: @bGet_Content_Area@B"
        & " is packed like any other box, and each button is added with"
        & " @bAdd_Button@B paired with the @bGtk_Response_Type@B it emits."
        & " Answering @bOK@B copies the two entries into the labels on the"
        & " right; answering @bCancel@B does not."
        & ASCII.LF
        & "Neither dialog is destroyed by being answered, so both handlers"
        & " call @bDestroy@B themselves.";
   end Help;

   -------------------
   -- Parent_Window --
   -------------------

   function Parent_Window
     (Widget : access Gtk_Widget_Record'Class) return Gtk_Window is
   begin
      return Gtk_Window (Widget.Get_Ancestor (Gtk.Window.Get_Type));
   end Parent_Window;

   -------------------------
   -- On_Message_Response --
   -------------------------

   procedure On_Message_Response
     (Self        : access Gtk_Dialog_Record'Class;
      Response_Id : Gtk_Response_Type)
   is
      pragma Unreferenced (Response_Id);
   begin
      --  A dialog is not destroyed by answering it; that is up to us.
      Self.Destroy;
   end On_Message_Response;

   ------------------------
   -- On_Message_Clicked --
   ------------------------

   procedure On_Message_Clicked (Self : access Gtk_Button_Record'Class) is
      Dialog : Gtk_Message_Dialog;
   begin
      Count := Count + 1;

      declare
         Img : constant String := Natural'Image (Count);
      begin
         --  The message is handed to printf as its format string, so build
         --  the count into it here rather than passing it as an argument: a
         --  stray '%' in a message is a hazard, not a substitution.

         Gtk_New
           (Dialog,
            Parent   => Parent_Window (Self),
            Flags    => Dialog_Modal or Dialog_Destroy_With_Parent,
            The_Type => Message_Info,
            Buttons  => Buttons_Ok,
            Message  =>
              "This message box has been popped up the following"
              & ASCII.LF & "number of times:" & ASCII.LF & ASCII.LF
              & Img (Img'First + 1 .. Img'Last));
         --  'Image puts a blank where a minus sign would go; drop it.
      end;

      Dialog.On_Response (On_Message_Response'Access);
      Dialog.Present;
   end On_Message_Clicked;

   -----------------------------
   -- On_Interactive_Response --
   -----------------------------

   procedure On_Interactive_Response
     (Self        : access Gtk_Dialog_Record'Class;
      Response_Id : Gtk_Response_Type) is
   begin
      if Response_Id = Gtk_Response_OK then
         Label1.Set_Text (Entry1.Get_Text);
         Label2.Set_Text (Entry2.Get_Text);
      end if;

      Self.Destroy;
   end On_Interactive_Response;

   ----------------------------
   -- On_Interactive_Clicked --
   ----------------------------

   procedure On_Interactive_Clicked (Self : access Gtk_Button_Record'Class) is
      Dialog  : Gtk_Dialog;
      Grid    : Gtk_Grid;
      Label   : Gtk_Label;
      Ignored : Gtk_Widget;
   begin
      Gtk_New
        (Dialog,
         Title  => "Interactive Dialog",
         Parent => Parent_Window (Self),
         Flags  => Dialog_Modal or Dialog_Destroy_With_Parent);

      Ignored := Dialog.Add_Button ("_OK", Gtk_Response_OK);
      Ignored := Dialog.Add_Button ("_Cancel", Gtk_Response_Cancel);
      Dialog.Set_Default_Response (Gtk_Response_OK);

      --  The content area is ours to populate, exactly like any other box.

      Gtk_New (Grid);
      Grid.Set_Row_Spacing (6);
      Grid.Set_Column_Spacing (6);
      Grid.Set_Margin_Top (12);
      Grid.Set_Margin_Bottom (12);
      Grid.Set_Margin_Start (12);
      Grid.Set_Margin_End (12);
      Dialog.Get_Content_Area.Append (Grid);

      Gtk_New_With_Mnemonic (Label, "_Entry 1");
      Grid.Attach (Label, 0, 0);
      Gtk_New (Entry1);
      Entry1.Set_Text (Label1.Get_Text);
      Label.Set_Mnemonic_Widget (Entry1);
      Grid.Attach (Entry1, 1, 0);

      Gtk_New_With_Mnemonic (Label, "E_ntry 2");
      Grid.Attach (Label, 0, 1);
      Gtk_New (Entry2);
      Entry2.Set_Text (Label2.Get_Text);
      Label.Set_Mnemonic_Widget (Entry2);
      Grid.Attach (Entry2, 1, 1);

      Dialog.On_Response (On_Interactive_Response'Access);
      Dialog.Present;
   end On_Interactive_Clicked;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box    : Gtk_Box;
      Grid   : Gtk_Grid;
      Button : Gtk_Button;
      Label  : Gtk_Label;
   begin
      Set_Label (Frame, "Dialogs");

      Gtk_New (Box, Orientation_Vertical, 8);
      Box.Set_Margin_Top (8);
      Box.Set_Margin_Bottom (8);
      Box.Set_Margin_Start (8);
      Box.Set_Margin_End (8);
      Frame.Set_Child (Box);

      Button := Gtk_Button_New_With_Mnemonic ("_Message Dialog");
      Button.Set_Halign (Align_Start);
      Button.On_Clicked (On_Message_Clicked'Access);
      Box.Append (Button);

      Gtk_New (Grid);
      Grid.Set_Row_Spacing (6);
      Grid.Set_Column_Spacing (6);
      Box.Append (Grid);

      Button := Gtk_Button_New_With_Mnemonic ("_Interactive Dialog");
      Button.Set_Halign (Align_Start);
      Button.Set_Valign (Align_Center);
      Button.On_Clicked (On_Interactive_Clicked'Access);
      Grid.Attach (Button, 0, 0, Height => 2);

      Gtk_New (Label, "Entry 1");
      Grid.Attach (Label, 1, 0);
      Gtk_New (Label1, "");
      Grid.Attach (Label1, 2, 0);

      Gtk_New (Label, "Entry 2");
      Grid.Attach (Label, 1, 1);
      Gtk_New (Label2, "");
      Grid.Attach (Label2, 2, 1);
   end Run;

end Create_Dialog;
