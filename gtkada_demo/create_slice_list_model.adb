
with GNAT.Strings;

with Glib;                         use Glib;
with Gtk;                          use Gtk;
with Gtk.Box;                      use Gtk.Box;
with Gtk.Button;                   use Gtk.Button;
with Gtk.Enums;                    use Gtk.Enums;
with Gtk.Label;                    use Gtk.Label;
with Gtk.List_Item;                use Gtk.List_Item;
with Gtk.List_View;                use Gtk.List_View;
with Gtk.Scrolled_Window;          use Gtk.Scrolled_Window;
with Gtk.Slice_List_Model;         use Gtk.Slice_List_Model;
with Gtk.Single_Selection;         use Gtk.Single_Selection;
with Gtk.Signal_List_Item_Factory; use Gtk.Signal_List_Item_Factory;
with Gtk.String_List;              use Gtk.String_List;
with Gtk.String_Object;            use Gtk.String_Object;
with Gtk.Frame;                    use Gtk.Frame;

package body Create_Slice_List_Model is

   Offset  : Glib.Guint := 0;
   Page    : Glib.Guint := 5;

   UpBtn   : Gtk_Button;
   DownBtn : Gtk_Button;
   OLabel  : Gtk_Label;
   Slice   : Gtk_Slice_List_Model;

   procedure Setup_Item
      (Self   : access Gtk_Signal_List_Item_Factory_Record'Class;
       Object : not null access Gtk.List_Item.Gtk_List_Item_Record'Class);
   procedure Bind_Item
      (Self   : access Gtk_Signal_List_Item_Factory_Record'Class;
       Object : not null access Gtk.List_Item.Gtk_List_Item_Record'Class);
   procedure Up
      (Self : access Gtk_Button_Record'Class);
   procedure Down
      (Self : access Gtk_Button_Record'Class);

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "Simple demo of Gtk.Slice_List_Model that presents a slice of another model"
         & ASCII.LF
         & "This is useful when implementing paging by setting the size to the number"
         & ASCII.LF
         & "of elements per page and updating the offset whenever a different page is"
         & ASCII.LF
         & "opened."
         & ASCII.LF
         & "Shows the `page` of 5 elements from the list. Up/Down buttons scroll the list";
   end Help;

   ----------------
   -- Setup_Item --
   ----------------

   procedure Setup_Item
      (Self   : access Gtk_Signal_List_Item_Factory_Record'Class;
       Object : not null access Gtk.List_Item.Gtk_List_Item_Record'Class)
   is
      Label : Gtk.Label.Gtk_Label;
   begin
      Gtk_New (Label);
      Object.Set_Child (Label);
   end Setup_Item;

   ---------------
   -- Bind_Item --
   ---------------

   procedure Bind_Item
      (Self   : access Gtk_Signal_List_Item_Factory_Record'Class;
       Object : not null access Gtk.List_Item.Gtk_List_Item_Record'Class)
   is
      Label : Gtk_Label;
      Str   : Gtk_String_Object;
   begin
      Label := Gtk.Label.Gtk_Label (Object.Get_Child);
      Str   := Gtk_String_Object (Object.Get_Item);
      Label.Set_Label (Str.Get_String);
   end Bind_Item;

   --------
   -- Up --
   --------

   procedure Up
      (Self : access Gtk_Button_Record'Class) is
   begin
      if Offset >= 5 then
         Offset := Offset - Page;
      end if;

      OLabel.Set_Label (Offset'Image);
      Slice.Set_Offset (Offset);
   end Up;

   ----------
   -- Down --
   ----------

   procedure Down
      (Self : access Gtk_Button_Record'Class) is
   begin
      if Offset + Page < 100 then
         Offset := Offset + Page;
      end if;

      OLabel.Set_Label (Offset'Image);
      Slice.Set_Offset (Offset);
   end Down;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      VBox     : Gtk.Box.Gtk_Box;
      HBox     : Gtk.Box.Gtk_Box;
      Scrolled : Gtk_Scrolled_Window;
      List     : Gtk_String_List;
      Single   : Gtk_Single_Selection;
      Factory  : Gtk_Signal_List_Item_Factory;
      View     : Gtk_List_View;
      Empty    : GNAT.Strings.String_List (1 .. 0);
   begin
      Set_Label (Frame, "List View with Slice Model");

      Gtk_New (VBox, Orientation_Vertical, 0);
      Frame.Set_Child (VBox);

      --  View

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

      --  Create the list that contains the actual data. This list will
      --  never be modified

      Gtk.String_List.Gtk_New (List, Empty);

      for Index in 1 .. 100 loop
         List.Append (Index'Img);
      end loop;

      --  Create a slice model
      Gtk_New (Slice, +List, Offset, Page);

      --  Create a selection model
      Gtk_New (Single, +Slice);

      --  Create a signal factory
      Gtk_New (Factory);
      Factory.On_Setup (Setup_Item'Access);
      Factory.On_Bind (Bind_Item'Access);

      --  And now a view that displays the model. A single column is displayed
      Gtk.List_View.Gtk_New (View, +Single, Factory);
      Scrolled.Set_Child (View);
      Scrolled.Set_Vexpand (True);

      --  Controls

      Gtk_New (HBox, Orientation_Horizontal, 0);
      UpBtn := Gtk_Button_New_With_Label ("Up");
      UpBtn.On_Clicked (Up'Access);
      HBox.Append (UpBtn);

      DownBtn := Gtk_Button_New_With_Label ("Down");
      DownBtn.On_Clicked (Down'Access);
      HBox.Append (DownBtn);

      Gtk_New (OLabel);
      OLabel.Set_Label (Offset'Image);
      HBox.Append (OLabel);

      VBox.Append (HBox);
      VBox.Append (Scrolled);
   end Run;

end Create_Slice_List_Model;
